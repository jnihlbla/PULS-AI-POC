000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4020200.                                                
000500 AUTHOR.         JAN-ERIK FRANTZEN                                        
000600 DATE-WRITTEN.   DEC   -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET HANTERAR TILLÄGG AV ORDERRADER.                       
001200*        DÄR ORDERN / ORDERDELAR HAR STATUS HÖGRE ÄN R                    
001300*        SKAPAS TPO2:OR.                                                  
001400*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA HÄMTAS FRÅN          
001500*        ARTIKELREGISTRET.                                                
001600*                                                                         
001700*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001800*        ORDERAVSLUT 4203.                                                
001900     EJECT                                                                
002000*                                                                         
002100*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002200*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
002300*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002400*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002500*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
002600*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
002700*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002800*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
002900*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
003000*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
003100*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003200*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
003300*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003400*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
003500*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
003600*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003700*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003800*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
003900*                                                                         
004000*    INDATA.                                                              
004100*        TRANSAKTION: W4T202                                              
004200*        MID:         W4I20201                                            
004300*    UTDATA.                                                              
004400*        MOD:         W4O20201                                            
004500*                                                                         
004600*    E'TRACKER: 5444132 DATED 2007-09-18                                  
004700*    E'TRACKER: 2218613 DATED 2008-03-11 KAMPANJORDER SEPARAT PRC.        
004800*    E'TRACKER: 7450328       2008 HÖST  VOHF                             
004900*    E'TRACKER: 8081720       ORDER STEERING TO VOHF                      
005000*    E'TRACKER: 10254592 2015 DECOMISSION VOHF                            
005100*                                                                         
005200     EJECT                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400                                                                          
005500 DATA DIVISION.                                                           
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900     SKIP3                                                                
006000 77  IDPGM                       PIC X(08)   VALUE 'W4020200'.            
006100 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
006200 77  HOPP-TILL-4203              PIC X(1)   VALUE 'N'.                    
006300 77  YES                         PIC X(1)   VALUE 'Y'.                    
006400 77  NEJ                         PIC X(1)   VALUE 'N'.                    
006500 77  JA                          PIC X(1)   VALUE 'J'.                    
006600 77  SPEC-FORBI                  PIC X(1)   VALUE 'S'.                    
006700 77  SW-KDORDSTA-ALL-E-FLAG      PIC X(1)   VALUE 'J'.                    
006800 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)  VALUE 'J'.                    
006900 77  SW-KDORDSTA-O-STATUS-FLAG   PIC X(1)   VALUE 'J'.                    
007000 77  SW-KDORDSTA-STATUS-FLAG     PIC X(1)   VALUE 'J'.                    
007100                                                                          
007200 01  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
007300*01  -COPY WWDCKONS                                                       
007400                                                                          
007500*01  -COPY WWPRODSL                                                       
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
007900 77  FELTEXT                     PIC  X(64) VALUE SPACE.                  
008000 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
008100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
008200 77  HFAK-TAB-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
008300 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
008400 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008500 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008600 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
008700 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
008900 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009000 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
009100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
009200 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
009300 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
009400 77  WS-KDORDKL                  PIC X(1)    VALUE SPACE.                 
009500 77  WS-IDDC-DDGS                PIC X(2)    VALUE SPACE.                 
009600 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
009700 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009800 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
009900 77  WS-HFAK-REF-X10             PIC X(10)   VALUE SPACE.                 
010000 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
010100 77  WX-KDORDBEK                 PIC S9(2)   VALUE +0.                    
010200 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
010300 77  DAGENS-DATUM                PIC S9(7)   VALUE +0  COMP-3.            
010400 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010500 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010600 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010700 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
010800 77  WS-ARB-KDORDSTA-PRIM        PIC X(2)    VALUE SPACE.                 
010900 77  WS-ARB-KDORDSTA-O-PRIM      PIC X(2)    VALUE SPACE.                 
011000                                                                          
011100 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
011200 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011300     03 WS-TIHHMM                PIC 9(4).                                
011400     03 FILLER                   PIC 9(2).                                
011500 01 DB2-LASNING.                                                          
011600     03 FILLER                   PIC X(16)   VALUE                        
011700                                             'WS-DB2-SEKTION'.            
011800     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
011900 01  DUMMY-PCB                   PIC X(4)   VALUE LOW-VALUE.              
012000                                                                          
012100                                                                          
012200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012300     88  ALLT-OK                             VALUE 'J'.                   
012400                                                                          
012500 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
012600     88  FIRST-TIME                          VALUE 'J'.                   
012700                                                                          
012800 77  TILLK-SW                    PIC X       VALUE 'N'.                   
012900     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
013000     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
013100                                                                          
013200 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
013300     88  KOLLA-ERS                           VALUE 'J'.                   
013400                                                                          
013500 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
013600     88  SVARSBILD                           VALUE 'J'.                   
013700                                                                          
013800 77  OBKR-SW                     PIC X       VALUE 'N'.                   
013900     88  SKRIV-OBKR                          VALUE 'J'.                   
014000     88  OBKR-SKRIVEN                        VALUE 'S'.                   
014100                                                                          
014200 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
014300     88  EGET-CL-RAD                         VALUE 'J'.                   
014400                                                                          
014500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014600     88  EGEN-MID                            VALUE '4202'.                
014700     88  GODK-MID                            VALUE '4201' '4202'          
014800                                                   '4203' '4204'.         
014900     EJECT                                                                
015000 77  TILLAEGG-TPO-SW             PIC X       VALUE 'N'.                   
015100     88  TILLAEGG-TPO                        VALUE 'J'.                   
015200                                                                          
015300 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
015400     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
015500                                                                          
015600 77  VANLIGA-RADER-C1-SW         PIC X       VALUE 'N'.                   
015700     88  VANLIGA-RADER-C1                    VALUE 'J'.                   
015800     88  VANLIGA-RADER-C1-EJ                 VALUE 'N'.                   
015900                                                                          
016000 77  KOLLA-ARBTAB-C1-SW          PIC X       VALUE 'N'.                   
016100     88  KOLLA-ARBTAB-C1                     VALUE 'J'.                   
016200     88  KOLLA-ARBTAB-C1-EJ                  VALUE 'N'.                   
016300                                                                          
016400 77  RAD-GODKAND-SW              PIC X       VALUE 'N'.                   
016500     88  RAD-GODKAND                         VALUE 'J'.                   
016600                                                                          
016700 77  RAD-LO-60-SW                PIC X       VALUE 'N'.                   
016800     88  RAD-LO-60                           VALUE 'J'.                   
016900                                                                          
017000 77  RAD-LO-61-SW                PIC X       VALUE 'N'.                   
017100     88  RAD-LO-61                           VALUE 'J'.                   
017200     EJECT                                                                
017300                                                                          
017400 01  W-GMT-IDDC-CLEAR-GRP.                                                
017500*                                 GRUPP AV IDDC-CLEAR                     
017600     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
017700                                 PIC X(2)    VALUE SPACE.                 
017800 01  WS-ALFA-1.                                                           
017900     03  WS-NUM-1                PIC 9(1).                                
018000 01  WS-ALFA-2.                                                           
018100     03  WS-NUM-2                PIC 9(2).                                
018200 01  WS-ALFA-6.                                                           
018300     03  WS-NUM-6                PIC 9(6).                                
018400 01  WS-ALFA-7.                                                           
018500     03  WS-NUM-7                PIC 9(7).                                
018600 01  WS-ALFA-8.                                                           
018700     03  WS-NUM-8                PIC 9(8).                                
018800     03  FILLER REDEFINES WS-NUM-8.                                       
018900         05  WS-NUM-1--4         PIC 9(4).                                
019000         05  WS-NUM-5--8         PIC 9(4).                                
019100 01  WS-ALFA-12.                                                          
019200     03  WS-NUM-12               PIC 9(12).                               
019300 01  WS-ALFA-2V3.                                                         
019400     03  WS-ALFA-1--2            PIC X(2).                                
019500     03  WS-NUM-PUNKT            PIC X(1).                                
019600     03  WS-ALFA-3--5            PIC X(3).                                
019700 01  WS-NUM-2V3                  PIC 9(2)V9(3).                           
019800 01  FILLER REDEFINES WS-NUM-2V3.                                         
019900     03  WS-NUM-1--2             PIC 9(2).                                
020000     03  WS-NUM-3--5             PIC 9(3).                                
020100     EJECT                                                                
020200                                                                          
020300 01  WS-IDKONTO.                                                          
020400     03  WS-IDKONTO-1--4         PIC 9(4).                                
020500     03  WS-IDKONTO-5            PIC X(1).                                
020600     03  WS-IDKONTO-6--9         PIC 9(4).                                
020700     03  FILLER                  PIC X(1).                                
020800                                                                          
020900 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
021000 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
021100     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
021200     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
021300                                                                          
021400     EJECT                                                                
021500 01 NYCKLAR-TP4TRAN.                                                      
021600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
021700                                                                          
021800     EJECT                                                                
021900 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
022000*01  FILLER -COPY WWDIST07 -RED TEST-IDDISTR.                             
022100     EJECT                                                                
022200*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
022300     EJECT                                                                
022400*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
022500     EJECT                                                                
022600*    ----DIST79-DEALER-PRICE----                                          
022700*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
022800     EJECT                                                                
022900*                                                                         
023000 01 FILLER                       PIC X(8) VALUE 'W411TILK'.               
023100*    -COPY W411TILK                                                       
023200     EJECT                                                                
023300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023400 01  GENERELLA-SUBPROGRAM.                                                
023500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
024100     EJECT                                                                
024200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
024300*01 -COPY WMSGINIT                                                        
024400*                                                                         
024500*                                                                         
024600*                                                                         
024700 01  GEMENSAMMA-SUBPROGRAM.                                               
024800     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
024900*        PRISTILLÄMPNING                                                  
025000     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
025100*        HÄMTA PRISFRÅGENR                                                
025200     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
025300*        DEALER PRISFRÅGABEHANDLING                                       
025400     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
025500*        LÄSNING ARTIKELREGISTER                                          
025600     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
025700*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
025800     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
025900*        KONTROLL DIREKTLEVERANS                                          
026000     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
026100*        KONTROLL DIREKTLEVERANS                                          
026200     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
026300*        KONTROLL TPO4 - KAMPANJ                                          
026400     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
026500*        KONTROLL ERSÄTTNINGAR                                            
026600     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
026700*        KONTROLL KVANTANPASSNING                                         
026800     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
026900*        KONTROLL ENHETSLAST                                              
027000     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
027100*        FORMELLA KONTROLLER AV INDATA                                    
027200     EJECT                                                                
027300     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
027400*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
027500     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
027600*        BERÄKNA RANSONERING                                              
027700     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
027800*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
027900     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
028000*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
028100     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
028200*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
028300     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
028400*        KONTROLL SPÄRRAR                                                 
028500     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
028600*        KONTROLL STORA UTTAG                                             
028700     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
028800*        KONTROLL TPO1                                                    
028900     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
029000*        KONTROLL TPO2                                                    
029100     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
029200*        KONTROLL RELS                                                    
029300     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
029400*        RÄKNA OM VALUTA  DDI                                             
029500     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
029600*        WDB601-SEGMENT FÖR CLARING-DC                                    
029700     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
029800*        WOPS RADBEHANDLING                                               
029900     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
030000*        OMVANDLING AV LAGOMR + PLATS                                     
030100*                                                                         
030200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
030300*   -COPY WMEDAREA                                                        
030400     EJECT                                                                
030500 01  MESSAGE-CODES.                                                       
030600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030700     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
030800     03  ERR-ORDER-EJ-AVSLUT     PIC X(3)    VALUE '053'.                 
030900     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
031000     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
031100     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
031200     03  ERR-ORDER-ANNULL        PIC X(3)    VALUE '052'.                 
031300     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
031400     03  ERR-ORDERLINES-MISSING  PIC X(3)    VALUE '029'.                 
031500     EJECT                                                                
031600                                                                          
031700*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
031800*                                                                         
031900 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
032000*   -COPY W335PRIS                                                        
032100*                                                                         
032200     EJECT                                                                
032300 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
032400*   -COPY W335PRNO                                                        
032500     EJECT                                                                
032600 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
032700*   -COPY W335PRQU                                                        
032800     EJECT                                                                
032900*                                                                         
033000 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
033100*   -COPY W411AREG                                                        
033200*                                                                         
033300     EJECT                                                                
033400*                                                                         
033500 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
033600*   -COPY W411ARTM                                                        
033700*                                                                         
033800     EJECT                                                                
033900*                                                                         
034000 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
034100*   -COPY W411DLEV                                                        
034200*                                                                         
034300     EJECT                                                                
034400*                                                                         
034500 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
034600*   -COPY W411DNOT                                                        
034700*                                                                         
034800     EJECT                                                                
034900*                                                                         
035000 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
035100*   -COPY W411KAMP                                                        
035200*                                                                         
035300     EJECT                                                                
035400*                                                                         
035500 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
035600*   -COPY W411KERS                                                        
035700*                                                                         
035800     EJECT                                                                
035900*                                                                         
036000 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
036100*   -COPY W411KVAN                                                        
036200*                                                                         
036300     EJECT                                                                
036400*                                                                         
036500 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
036600*   -COPY W411LAST                                                        
036700*                                                                         
036800     EJECT                                                                
036900*                                                                         
037000 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
037100*   -COPY W411ORFK                                                        
037200*                                                                         
037300     EJECT                                                                
037400*                                                                         
037500 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
037600*   -COPY W411CDCA                                                        
037700*                                                                         
037800     EJECT                                                                
037900*                                                                         
038000 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
038100*   -COPY W411RANS                                                        
038200*                                                                         
038300     EJECT                                                                
038400*                                                                         
038500 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
038600*   -COPY W411NDCA                                                        
038700*                                                                         
038800     EJECT                                                                
038900 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
039000*   -COPY W411XDCA                                                        
039100*                                                                         
039200     EJECT                                                                
039300 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
039400*   -COPY W411XDK7 -PRE NDCA-                                             
039500*                                                                         
039600     EJECT                                                                
039700*                                                                         
039800 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
039900*   -COPY W411SDCA                                                        
040000*                                                                         
040100     EJECT                                                                
040200*                                                                         
040300 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
040400*   -COPY W411SPAR                                                        
040500*                                                                         
040600     EJECT                                                                
040700*                                                                         
040800 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
040900*   -COPY W411STOR                                                        
041000*                                                                         
041100     EJECT                                                                
041200*                                                                         
041300 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
041400*   -COPY W411TPO1                                                        
041500*                                                                         
041600     EJECT                                                                
041700*                                                                         
041800 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
041900*   -COPY W411TPO2                                                        
042000*                                                                         
042100     EJECT                                                                
042200 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
042300*   -COPY W411RELS                                                        
042400*                                                                         
042500     EJECT                                                                
042600*                                                                         
042700     EJECT                                                                
042800 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
042900*   -COPY W411CLDC                                                        
043000     EJECT                                                                
043100*                                                                         
043200 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
043300*   -COPY W413AVSR                                                        
043400*                                                                         
043500     SKIP2                                                                
043600*                                                                         
043700 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
043800*   -COPY W413ADRS                                                        
043900*                                                                         
044000     EJECT                                                                
044100*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
044200                                                                          
044300*   -COPY W413WHFA                                                        
044400     EJECT                                                                
044500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
044600*                                                                         
044700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
044800     SKIP3                                                                
044900*01  MID -COPY W4I20201                                                   
045000     EJECT                                                                
045100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
045200     SKIP3                                                                
045300*01  -COPY WMSGAREA                                                       
045400     EJECT                                                                
045500*    03  MOD -COPY W4O20201   -RED MSG-AREA.                              
045600     EJECT                                                                
045700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
045800     SKIP3                                                                
045900*01  -COPY WMFSAREA                                                       
046000     EJECT                                                                
046100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046200*                                                                         
046300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046400     SKIP3                                                                
046500 01  NYCKLAR-TILL-DLI.                                                    
046600                                                                          
046700     03  W-IDGMTREF-X.                                                    
046800         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
046900         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
047000         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
047100                                                                          
047200     03  W-WDQ211KY-X.                                                    
047300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
047400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
047500                                                                          
047600     03  W-IDARTNR-X.                                                     
047700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
047800                                                                          
047900     03  W-IDDC-X.                                                        
048000         05  W-IDDC-WDQ212       PIC X(2)    VALUE SPACE.                 
048100                                                                          
048200     03  W-ADLAGOMR-X.                                                    
048300         05  W-ADLAGOMR          PIC S9(3)   VALUE ZERO COMP-3.           
048400                                                                          
048500     03  W-WDQ101KY-MIN-X.                                                
048600         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
048700         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
048800         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
048900         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
049000         05  FILLER              PIC  X(04)  VALUE LOW-VALUE.             
049100                                                                          
049200     03  W-WDQ101KY-MAX-X.                                                
049300         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
049400         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
049500         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
049600         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
049700         05  FILLER              PIC  X(04)  VALUE HIGH-VALUE.            
049800     03  W-IDGMT-X.                                                       
049900         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
050000         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
050100                                                                          
050200     03  W-WDB101KY-X.                                                    
050300       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
050400       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
050500                                                                          
050600     03  W-IDDC-B6-X.                                                     
050700       05 W-IDDC-B6              PIC X(2).                                
050800                                                                          
050900*                                                                         
051000     EJECT                                                                
051100                                                                          
051200*    --- STATUS-KOD FRÅN IMS                                              
051300 01  STATUS-WS                   PIC XX.                                  
051400     88  SEGMENT-FINNS                       VALUE '  '.                  
051500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
051600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
051700     88  BASEN-SLUT                          VALUE 'GB'.                  
051800     SKIP2                                                                
051900 01  GODK-STATUSKODER.                                                    
052000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052100     EJECT                                                                
052200*                            DB2 FUNKTIONSKODER                           
052300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
052400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
052500                                                                          
052600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
052700 01  DB2-WS.                                                              
052800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
052900         88  CURSOR-OK                       VALUE 000.                   
053000         88  RADER-FINNS                     VALUE 000.                   
053100         88  RADER-SAKNAS                    VALUE 100.                   
053200         88  ATKOMST-FEL                     VALUE 904.                   
053300     03  GODK-SQLCODEKODER.                                               
053400         05  GODK-SQLCODE OCCURS 5                                        
053500             INDEXED BY SQLCODE-IX PIC 9(3).                              
053600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
053700     EJECT                                                                
053800                                                                          
053900 01  SSA1                        PIC X(96).                               
054000 01  SSA2                        PIC X(64).                               
054100     EJECT                                                                
054200*    --- IMS FUNKTIONSKODER                                               
054300*01  -COPY W0003                                                          
054400     EJECT                                                                
054500*    ---  DLI INPUT-OUTPUT AREA                                           
054600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
054700     SKIP3                                                                
054800 01  DLI-IO-AREA-OBKR.                                                    
054900     03  WDQ101.                                                          
055000*        05  -COPY WDQ101                                                 
055100     EJECT                                                                
055200 01  DLI-IO-AREA-OHUV.                                                    
055300     03  WDQ201.                                                          
055400*        05  -COPY WDQ201                                                 
055500     EJECT                                                                
055600 01  DLI-IO-AREA-ARB.                                                     
055700     03  WDQ212.                                                          
055800*        05  -COPY WDQ212                                                 
055900     EJECT                                                                
056000 01  DLI-IO-AREA-LOR.                                                     
056100     03  WDQ221.                                                          
056200*        05  -COPY WDQ221                                                 
056300     EJECT                                                                
056400 01  DLI-IO-AREA-DLEV.                                                    
056500     03  WDQ211.                                                          
056600*        05  -COPY WDQ211                                                 
056700     EJECT                                                                
056800 01  DLI-IO-AREA-ORAD.                                                    
056900     03  WDQ401.                                                          
057000*        05  -COPY WDQ401                                                 
057100     EJECT                                                                
057200 01  DLI-IO-AREA-ART.                                                     
057300     03  WLARTM01.                                                        
057400*        05  -COPY WDK901                                                 
057500 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
057600 01  DLI-IO-AREA-WDB201.                                                  
057700*    03  -COPY WDB201                                                     
057800     EJECT                                                                
057900 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
058000 01  DLI-IO-AREA-WDB101.                                                  
058100     03  WLBETC01.                                                        
058200*        05  -COPY WDB101                                                 
058300                                                                          
058400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
058500 01   DLI-IO-AREA-B601.                                                   
058600*     03  -COPY WDB601                                                    
058700     EJECT                                                                
058800 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
058900 01   DLI-IO-AREA-R601.                                                   
059000*     03  -COPY WDR601                                                    
059100*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
059200     EJECT                                                                
059300*                                                                         
059400                                                                          
059500*---MSG-AREA FÖR HOPP TILL 4203-SVARSBILDEN                               
059600                                                                          
059700 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
059800 01  4203-MSG-IO-AREA.                                                    
059900     03  4203-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
060000     03  4203-Z1               PIC X.                                     
060100     03  4203-Z2               PIC X.                                     
060200     03  4203-TRANSKOD         PIC X(8)   VALUE 'W4T203  '.               
060300     03  4203-IDTRANS          PIC X(4)   VALUE '4202'.                   
060400     03  4203-SPRAK            PIC X.                                     
060500     03  4203-IDDISTR-IN       PIC X(4).                                  
060600     03  4203-IDKUNDNR-IN      PIC X(6).                                  
060700     03  4203-IDORDNR-IN       PIC X(5).                                  
060800     03  4203-IDDISTR-UT       PIC X(4).                                  
060900     03  4203-IDKUNDNR-UT      PIC X(6).                                  
061000     03  4203-IDORDNR-UT       PIC X(5).                                  
061100     EJECT                                                                
061200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
061300     SKIP3                                                                
061400 01  -COPY WZ01SEND                                                       
061500     EJECT                                                                
061600 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
061700     SKIP3                                                                
061800 01  SEND-AREA.                                                           
061900*    03  -COPY WZ01REQU  -PRE 3039-                                       
062000*    03  -COPY W30391I1  -PRE 3039-                                       
062100     EJECT                                                                
062200     EJECT                                                                
062300 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
062400                                                                          
062500*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
062600     EJECT                                                                
062700     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
062800     EJECT                                                                
062900 LINKAGE SECTION.                                                         
063000                                                                          
063100*01  -COPY W0009      -PRE MSG-                                           
063200                                                                          
063300*01  -COPY W0009      -PRE 4203-                                          
063400     EJECT                                                                
063500*01  -COPY W0009      -PRE 4203V-                                         
063600     EJECT                                                                
063700 01  AVSR-LIMS-PCB               PIC X.                                   
063800                                                                          
063900 01  2109-PCB                    PIC X.                                   
064000                                                                          
064100 01  PRQRY-PCB                   PIC X.                                   
064200*01  -COPY W0008      -PRE USEA-                                          
064300     05  FILLER                  PIC X.                                   
064400                                                                          
064500*01  -COPY W0008      -PRE WDQ1-                                          
064600     05  FILLER                  PIC X.                                   
064700     SKIP2                                                                
064800*01  -COPY W0008      -PRE WDQ2-                                          
064900     05  FILLER                  PIC X.                                   
065000     EJECT                                                                
065100*01  -COPY W0008      -PRE WDQ4-                                          
065200     05  FILLER                  PIC X.                                   
065300     EJECT                                                                
065400*01  -COPY W0008      -PRE ARTM-                                          
065500     05  FILLER                  PIC X.                                   
065600     EJECT                                                                
065700*01  -COPY W0008      -PRE WDB2-                                          
065800     05  FILLER                  PIC X.                                   
065900     EJECT                                                                
066000*01  -COPY W0008      -PRE WDB1-                                          
066100     05  FILLER                  PIC X.                                   
066200     EJECT                                                                
066300*01  -COPY W0008      -PRE WDB6-                                          
066400     05  FILLER                  PIC X.                                   
066500     EJECT                                                                
066600*01  -COPY W0008      -PRE WDR6-                                          
066700     05  FILLER                  PIC X.                                   
066800     EJECT                                                                
066900 01  PRIS-ARTC-PCB               PIC X.                                   
067000 01  PRIS-WDK7-PCB               PIC X.                                   
067100 01  PRIS-GMTA-PCB               PIC X.                                   
067200 01  PRIS-BETA-PCB               PIC X.                                   
067300 01  PRIS-GPRIA-PCB              PIC X.                                   
067400 01  PRIS-GPRIB-PCB              PIC X.                                   
067500 01  PRIS-COST-WDK6-PCB          PIC X.                                   
067600 01  PRIS-COST-WDK7-PCB          PIC X.                                   
067700 01  PRIS-COST-WDF1-PCB          PIC X.                                   
067800 01  PRIS-COST-9305-PCB          PIC X.                                   
067900 01  PRIS-COST-WDK72-PCB         PIC X.                                   
068000 01  PRIS-COST-WDB6-PCB         PIC X.                                    
068100 01  PRNO-3107-PCB               PIC X.                                   
068200 01  PRQU-WDG2-PCB               PIC X.                                   
068300 01  PRQU-WDC7-PCB               PIC X.                                   
068400 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
068500 01  AREG-WDK6-PCB               PIC X.                                   
068600 01  AREG-WDK7-PCB               PIC X.                                   
068700 01  ARTM-ARTM-PCB               PIC X.                                   
068800 01  DLEV-LEVF-PCB               PIC X.                                   
068900 01  DLEV-LEVG-PCB               PIC X.                                   
069000 01  DLEV-LEVA-PCB               PIC X.                                   
069100 01  DLEV-ARTS-PCB               PIC X.                                   
069200 01  DLEV-WDB6-PCB               PIC X.                                   
069300 01  SPAR-WDF8-PCB               PIC X.                                   
069400 01  SPAR-WDF8A-PCB              PIC X.                                   
069500 01  SPAR-WDK6-PCB               PIC X.                                   
069600 01  DNOT-ORQP-PCB               PIC X.                                   
069700 01  DNOT-ORQP2-PCB              PIC X.                                   
069800 01  DNOT-ORQP3-PCB              PIC X.                                   
069900 01  DNOT-4013-PCB               PIC X.                                   
070000 01  DNOT-BENA-PCB               PIC X.                                   
070100 01  KAMP-ORDP-PCB               PIC X.                                   
070200 01  KAMP-ZZAC-PCB               PIC X.                                   
070300 01  KAMP-WDM2-PCB               PIC X.                                   
070400 01  KERS-ARTC-PCB               PIC X.                                   
070500 01  KERS-ERSA-PCB               PIC X.                                   
070600 01  NDCA-USEA-PCB               PIC X.                                   
070700 01  NDCA-WDK7-PCB               PIC X.                                   
070800 01  NDCA-WDL6-PCB               PIC X.                                   
070900 01  NDCA-WDB6-PCB               PIC X.                                   
071000 01  SDCA-ARTS-PCB               PIC X.                                   
071100 01  SDCA-WDB6-PCB               PIC X.                                   
071200 01  SDCA-WDK9-PCB               PIC X.                                   
071300 01  SDCA-WDR6-PCB               PIC X.                                   
071400 01  SDCA-WDK6-PCB               PIC X.                                   
071500 01  SDCA-WDQ4B-PCB              PIC X.                                   
071600 01  SDCA-WDQ2-PCB               PIC X.                                   
071700 01  SDCA-WDQ4-PCB               PIC X.                                   
071800 01  SDCA-WDB6-2-PCB             PIC X.                                   
071900 01  SDCA-WDK6-2-PCB             PIC X.                                   
072000 01  SDCA-WDK7-2-PCB             PIC X.                                   
072100 01  SDCA-WDK7-3-PCB             PIC X.                                   
072200 01  CDCA-ARTM-PCB               PIC X.                                   
072300 01  CDCA-INLB-PCB               PIC X.                                   
072400 01  CDCA-WDB2-PCB               PIC X.                                   
072500 01  CDCA-WDC1-PCB               PIC X.                                   
072600 01  RANS-XXKM-PCB               PIC X.                                   
072700 01  RANS-ARTM-PCB               PIC X.                                   
072800 01  RANS-ARTS-PCB               PIC X.                                   
072900     EJECT                                                                
073000 01  TPO1-ORDP-PCB               PIC X.                                   
073100 01  TPO1-ARTM-PCB               PIC X.                                   
073200 01  TPO1-ZZAC-PCB               PIC X.                                   
073300 01  TPO2-ORDP-PCB               PIC X.                                   
073400 01  TPO2-XXBU-PCB               PIC X.                                   
073500 01  TPO2-XXBV-PCB               PIC X.                                   
073600 01  TPO2-ARTM-PCB               PIC X.                                   
073700 01  TPO2-FILA-PCB               PIC X.                                   
073800 01  TPO2-XXBX-PCB               PIC X.                                   
073900 01  RELS-ORDP-PCB               PIC X.                                   
074000 01  RELS-FILA-PCB               PIC X.                                   
074100 01  RELS-ARTM-PCB               PIC X.                                   
074200 01  TIME-4437-PCB               PIC X.                                   
074300 01  AVSR-ORQI-PCB               PIC X.                                   
074400 01  AVSR-GMTB-PCB               PIC X.                                   
074500 01  AVSR-GMTC-PCB               PIC X.                                   
074600 01  AVSR-WDB2-PCB               PIC X.                                   
074700 01  AVSR-WDB6-PCB               PIC X.                                   
074800 01  TRAN-XXKB-PCB               PIC X.                                   
074900 01  KVAN-WDB2-PCB               PIC X.                                   
075000 01  KVAN-WDC1-PCB               PIC X.                                   
075100 01  XDCA-USEA-PCB               PIC X.                                   
075200 01  XDCA-WDB6-PCB               PIC X.                                   
075300 01  XDCA-WDK6-PCB               PIC X.                                   
075400 01  XDCA-WDK7-PCB               PIC X.                                   
075500 01  XDCA-WDK9-PCB               PIC X.                                   
075600 01  XDCA-WDL6-PCB               PIC X.                                   
075700 01  XDCA-WDQ4B-PCB              PIC X.                                   
075800 01  XDCA-WDQ2-PCB               PIC X.                                   
075900 01  XDCA-WDQ4-PCB               PIC X.                                   
076000 01  XDCA-WDR6-PCB               PIC X.                                   
076100 01  XDCA-WDB6-2-PCB             PIC X.                                   
076200 01  XDCA-WDK6-2-PCB             PIC X.                                   
076300 01  XDCA-WDK7-2-PCB             PIC X.                                   
076400 01  XDCA-WDK7-3-PCB             PIC X.                                   
076500     EJECT                                                                
076600 PROCEDURE DIVISION  USING                                                
076700        MSG-PCB        4203-PCB       4203V-PCB                           
076800        AVSR-LIMS-PCB  2109-PCB       PRQRY-PCB                           
076900        USEA-PCB                                                          
077000        WDQ1-PCB       WDQ2-PCB       WDQ4-PCB      ARTM-PCB              
077100        WDB2-PCB       WDB1-PCB       WDB6-PCB      WDR6-PCB              
077200        PRIS-ARTC-PCB  PRIS-WDK7-PCB                                      
077300        PRIS-GMTA-PCB  PRIS-BETA-PCB                                      
077400        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
077500        PRIS-COST-WDK6-PCB                                                
077600        PRIS-COST-WDK7-PCB                                                
077700        PRIS-COST-WDF1-PCB                                                
077800        PRIS-COST-9305-PCB                                                
077900        PRIS-COST-WDK72-PCB                                               
078000        PRIS-COST-WDB6-PCB                                                
078100        PRNO-3107-PCB                                                     
078200        PRQU-WDG2-PCB                                                     
078300        PRQU-WDC7-PCB                                                     
078400        PRQU-SJKO-WDK6-PCB                                                
078500        AREG-WDK6-PCB                                                     
078600        AREG-WDK7-PCB                                                     
078700        ARTM-ARTM-PCB                                                     
078800        DLEV-LEVF-PCB                                                     
078900        DLEV-LEVG-PCB                                                     
079000        DLEV-LEVA-PCB                                                     
079100        DLEV-ARTS-PCB                                                     
079200        DLEV-WDB6-PCB                                                     
079300        SPAR-WDF8-PCB                                                     
079400        SPAR-WDF8A-PCB                                                    
079500        SPAR-WDK6-PCB                                                     
079600        DNOT-ORQP-PCB                                                     
079700        DNOT-ORQP2-PCB                                                    
079800        DNOT-ORQP3-PCB                                                    
079900        DNOT-4013-PCB                                                     
080000        DNOT-BENA-PCB                                                     
080100        KAMP-ORDP-PCB  KAMP-ZZAC-PCB  KAMP-WDM2-PCB                       
080200        KERS-ARTC-PCB  KERS-ERSA-PCB                                      
080300        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
080400        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
080500        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
080600        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
080700        SDCA-WDK7-3-PCB                                                   
080800        CDCA-ARTM-PCB  CDCA-INLB-PCB  CDCA-WDB2-PCB  CDCA-WDC1-PCB        
080900        RANS-XXKM-PCB  RANS-ARTM-PCB  RANS-ARTS-PCB                       
081000        TPO1-ORDP-PCB  TPO1-ARTM-PCB  TPO1-ZZAC-PCB                       
081100        TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                       
081200        TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                       
081300        RELS-ORDP-PCB  RELS-FILA-PCB  RELS-ARTM-PCB                       
081400        TIME-4437-PCB                                                     
081500        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
081600        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
081700        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
081800        XDCA-USEA-PCB                                                     
081900        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
082000        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
082100        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
082200        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
082300        XDCA-WDK7-3-PCB.                                                  
082400     EJECT                                                                
082500                                                                          
082600     ENTRY 'DLITCBL' USING                                                
082700        MSG-PCB        4203-PCB       4203V-PCB                           
082800        AVSR-LIMS-PCB  2109-PCB       PRQRY-PCB                           
082900        USEA-PCB                                                          
083000        WDQ1-PCB       WDQ2-PCB       WDQ4-PCB      ARTM-PCB              
083100        WDB2-PCB       WDB1-PCB       WDB6-PCB      WDR6-PCB              
083200        PRIS-ARTC-PCB  PRIS-WDK7-PCB                                      
083300        PRIS-GMTA-PCB  PRIS-BETA-PCB                                      
083400        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
083500        PRIS-COST-WDK6-PCB                                                
083600        PRIS-COST-WDK7-PCB                                                
083700        PRIS-COST-WDF1-PCB                                                
083800        PRIS-COST-9305-PCB                                                
083900        PRIS-COST-WDK72-PCB                                               
084000        PRIS-COST-WDB6-PCB                                                
084100        PRNO-3107-PCB                                                     
084200        PRQU-WDG2-PCB                                                     
084300        PRQU-WDC7-PCB                                                     
084400        PRQU-SJKO-WDK6-PCB                                                
084500        AREG-WDK6-PCB                                                     
084600        AREG-WDK7-PCB                                                     
084700        ARTM-ARTM-PCB                                                     
084800        DLEV-LEVF-PCB                                                     
084900        DLEV-LEVG-PCB                                                     
085000        DLEV-LEVA-PCB                                                     
085100        DLEV-ARTS-PCB                                                     
085200        DLEV-WDB6-PCB                                                     
085300        SPAR-WDF8-PCB                                                     
085400        SPAR-WDF8A-PCB                                                    
085500        SPAR-WDK6-PCB                                                     
085600        DNOT-ORQP-PCB                                                     
085700        DNOT-ORQP2-PCB                                                    
085800        DNOT-ORQP3-PCB                                                    
085900        DNOT-4013-PCB                                                     
086000        DNOT-BENA-PCB                                                     
086100        KAMP-ORDP-PCB  KAMP-ZZAC-PCB  KAMP-WDM2-PCB                       
086200        KERS-ARTC-PCB  KERS-ERSA-PCB                                      
086300        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
086400        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
086500        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
086600        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
086700        SDCA-WDK7-3-PCB                                                   
086800        CDCA-ARTM-PCB  CDCA-INLB-PCB  CDCA-WDB2-PCB  CDCA-WDC1-PCB        
086900        RANS-XXKM-PCB  RANS-ARTM-PCB  RANS-ARTS-PCB                       
087000        TPO1-ORDP-PCB  TPO1-ARTM-PCB  TPO1-ZZAC-PCB                       
087100        TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                       
087200        TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                       
087300        RELS-ORDP-PCB  RELS-FILA-PCB  RELS-ARTM-PCB                       
087400        TIME-4437-PCB                                                     
087500        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
087600        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
087700        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
087800        XDCA-USEA-PCB                                                     
087900        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
088000        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
088100        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
088200        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
088300        XDCA-WDK7-3-PCB.                                                  
088400     EJECT                                                                
088500                                                                          
088600     PERFORM IMS-GET-MSG                                                  
088700     IF SEGMENT-FINNS                                                     
088800        PERFORM A-INIT                                                    
088900        IF ALLT-OK                                                        
089000          PERFORM B-KOLLA-NYCKLAR                                         
089100        END-IF                                                            
089200        IF ALLT-OK                                                        
089300           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
089400           IF ALLT-OK                                                     
089500              PERFORM D-FORMELL-KONTROLL                                  
089600              IF ALLT-OK                                                  
089700                 PERFORM E-BEHANDLA-RADER                                 
089800              END-IF                                                      
089900           END-IF                                                         
090000        END-IF                                                            
090100        IF ALLT-OK                                                        
090200           IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+' OR SVARSBILD        
090300              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
090400                PERFORM I-SKICKA-PRISFRAGA                                
090500              END-IF                                                      
090600              PERFORM F-HOPPA-TILL-SVARSBILD                              
090700           ELSE                                                           
090800              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
090900                PERFORM I-SKICKA-PRISFRAGA                                
091000              END-IF                                                      
091100              PERFORM G-VISA-TOM-SIDA                                     
091200           END-IF                                                         
091300        END-IF                                                            
091400        IF HOPP-TILL-4203 = NEJ                                           
091500           PERFORM Z-FINIT-INSERT-MSG                                     
091600        END-IF                                                            
091700     END-IF                                                               
091800     MOVE +0 TO RETURN-CODE                                               
091900     GOBACK                                                               
092000     .                                                                    
092100     EJECT                                                                
092200 A-INIT SECTION.                                                          
092300                                                                          
092400     MOVE 'STA A-SEC'                     TO   WS-PGM-POSITION            
092500     MOVE JA                   TO ALLT-SW                                 
092600     MOVE SPACE                TO MED-IDMFSFEL                            
092700                                                                          
092800     IF MSG-DUBBLA-TRANSKODER                                             
092900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20201                 
093000       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
093100       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
093200     ELSE                                                                 
093300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I20201                  
093400       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
093500       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
093600     END-IF                                                               
093700                                                                          
093800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
093900     MOVE MSG-IDPFK            TO MFS-IDPFK                               
094000     MOVE MFS-IDTRANS          TO W-IDTRANS                               
094100     IF W-IDTRANS = '4203' AND MSG-KDTRANS-1  = 'W4T202U '                
094200        MOVE JA TO SVARSBILD-SW                                           
094300     END-IF                                                               
094400     MOVE LOW-VALUE            TO MSG-AREA                                
094500     MOVE 'W4O202N1'           TO MFS-IDMOD                               
094600     MOVE '4202'               TO MOD-IDTRANS                             
094700     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
094800                                  MOD-TEMFSINF                            
094900     IF NOT EGEN-MID AND NOT SVARSBILD                                    
095000       MOVE SPACE              TO MFS-KDTRTYP                             
095100       MOVE '7'                TO MFS-IDPFK                               
095200     END-IF                                                               
095300                                                                          
095400     IF ENGLISH-TEXT                                                      
095500       MOVE 'GB '              TO MED-IDSKYLT                             
095600     ELSE                                                                 
095700       MOVE 'S  '              TO MED-IDSKYLT                             
095800     END-IF                                                               
095900                                                                          
096000                                                                          
096100     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O20201 + 4                  
096200     PERFORM AA-NOLLA-WOPS-TABELL                                         
096300     .                                                                    
096400     EJECT                                                                
096500 AA-NOLLA-WOPS-TABELL SECTION.                                            
096600                                                                          
096700     MOVE +1                   TO WS-INDEX-WOPS                           
096800     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
096900        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
097000        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
097100        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
097200        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
097300        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
097400        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
097500        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
097600        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
097700        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
097800        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
097900        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
098000        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
098100        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
098200        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
098300        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
098400        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
098500                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
098600        ADD +1                 TO WS-INDEX-WOPS                           
098700     END-PERFORM                                                          
098800                                                                          
098900     MOVE +1                   TO WS-INDEX-WOPS                           
099000     .                                                                    
099100     EJECT                                                                
099200 B-KOLLA-NYCKLAR SECTION.                                                 
099300                                                                          
099400     MOVE 'STA B-KOLLA'                   TO   WS-PGM-POSITION            
099500                                                                          
099600     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
099700                                   MOD-IDKUNDNR-IN                        
099800                                   MOD-IDORDNR-IN                         
099900                                                                          
100000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
100100     MOVE '001'             TO MSGI-KDCALL                                
100200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
100300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
100400     MOVE '4202'            TO MSGI-IDTRANS                               
100500     IF MFS-IDTRANS = '4202'                                              
100600        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
100700        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
100800                                                                          
100900****** ORDERNR : IDORDNR/IDKUNDRF                                         
101000        IF MID-IDORDNR-IN = ALL '+'                                       
101100          MOVE ALL '+'         TO MSGI-IDKUNDRF                           
101200        ELSE                                                              
101300          MOVE MID-IDORDNR-IN  TO WS-NUM-7                                
101400          MOVE WS-NUM-7        TO MSGI-IDKUNDRF (1:7)                     
101500          MOVE SPACE           TO MSGI-IDKUNDRF (8:3)                     
101600        END-IF                                                            
101700     END-IF                                                               
101800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
101900                                                                          
102000     MOVE MSGI-IDDISTR         TO WS-IDDISTR                              
102100     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
102200                                                                          
102300     MOVE MSGI-IDKUNDNR        TO WS-IDKUNDNR                             
102400     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
102500                                                                          
102600     MOVE MSGI-IDKUNDRF (3:5)  TO WS-IDORDNR                              
102700     INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                   
102800                                                                          
102900     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
103000        MOVE WS-IDDISTR           TO W-IDDISTR                            
103100     ELSE                                                                 
103200        MOVE NEJ                  TO ALLT-SW                              
103300     END-IF                                                               
103400                                                                          
103500     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
103600     IF DIST79-DEALER-PRICE                                               
103700        IF ENGLISH-TEXT                                                   
103800           MOVE 'DEALERPRICE'     TO MOD-TEDDI                            
103900        ELSE                                                              
104000           MOVE '    ÅF PRIS'     TO MOD-TEDDI                            
104100        END-IF                                                            
104200     ELSE                                                                 
104300        MOVE SPACES               TO MOD-TEDDI                            
104400     END-IF                                                               
104500                                                                          
104600     IF WS-IDKUNDNR NUMERIC                                               
104700        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
104800     ELSE                                                                 
104900        MOVE NEJ                  TO ALLT-SW                              
105000     END-IF                                                               
105100                                                                          
105200     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
105300        MOVE WS-IDORDNR           TO WS-NUM-7                             
105400        MOVE WS-NUM-7             TO W-IDKUNDRF                           
105500     ELSE                                                                 
105600        MOVE NEJ                  TO ALLT-SW                              
105700     END-IF                                                               
105800                                                                          
105900     IF MID-IDDISTR-IN NOT = ALL '+'                                      
106000        MOVE SPACE             TO MFS-KDTRTYP                             
106100        MOVE '7'               TO MFS-IDPFK                               
106200     END-IF                                                               
106300                                                                          
106400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
106500        MOVE SPACE             TO MFS-KDTRTYP                             
106600        MOVE '7'               TO MFS-IDPFK                               
106700     END-IF                                                               
106800                                                                          
106900     IF MID-IDORDNR-IN NOT = ALL '+'                                      
107000        MOVE SPACE             TO MFS-KDTRTYP                             
107100        MOVE '7'               TO MFS-IDPFK                               
107200     END-IF                                                               
107300                                                                          
107400     IF NOT ALLT-OK                                                       
107500       IF SVARSBILD                                                       
107600         MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4203'           
107700                                  TO FELTEXT                              
107800         CALL ABEND USING RKOD-ABEND                                      
107900       END-IF                                                             
108000       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
108100     END-IF                                                               
108200     IF GODK-MID OR ALLT-OK                                               
108300       MOVE WS-IDDISTR              TO MOD-IDDISTR-UT                     
108400       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
108500                                                                          
108600       IF WS-IDKUNDNR = ZERO                                              
108700         MOVE '     0'              TO MOD-IDKUNDNR-UT                    
108800       ELSE                                                               
108900         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR-UT                    
109000         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
109100       END-IF                                                             
109200                                                                          
109300       MOVE WS-IDORDNR              TO MOD-IDORDNR-UT                     
109400       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
109500                                                                          
109600       IF NOT ALLT-OK                                                     
109700          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                     
109800                                       MOD-IDKUNDNR-UT                    
109900                                       MOD-IDORDNR-UT                     
110000       END-IF                                                             
110100                                                                          
110200     ELSE                                                                 
110300       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-UT                     
110400                                       MOD-IDKUNDNR-UT                    
110500                                       MOD-IDORDNR-UT                     
110600     END-IF                                                               
110700     .                                                                    
110800     EJECT                                                                
110900 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
111000                                                                          
111100     MOVE 'STA C-KOLLA'                   TO   WS-PGM-POSITION            
111200     PERFORM IMS-01-GU-WDQ2-WDQ201                                        
111300     IF SEGMENT-FINNS                                                     
111400                                                                          
111500       PERFORM CA-KDORDSTA-VALIDATE                                       
111600       PERFORM CB-HAMTA-KUND                                              
111700       PERFORM CC-HAMTA-WDB6-INFO                                         
111800                                                                          
111900       IF (OHUV-KDORDKL = 1 AND                                           
112000           GMT-FLORDTIL-KL1 = JA)                                         
112100       OR                                                                 
112200          (OHUV-KDORDKL = 2 AND                                           
112300           GMT-FLORDTIL-KL2 = JA)                                         
112400       OR                                                                 
112500          (OHUV-KDORDKL = 3 AND                                           
112600           GMT-FLORDTIL-KL3 = JA)                                         
112700       OR                                                                 
112800          (OHUV-KDORDKL = 4 AND                                           
112900           GMT-FLORDTIL-KL4 = JA)                                         
113000           MOVE NEJ             TO ALLT-SW                                
113100           MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                           
113200       END-IF                                                             
113300       IF ALLT-OK                                                         
113400         IF OHUV-FLBORT = NEJ                                             
113500                                                                          
113600           IF OHUV-IDDC-TVS > ZERO                                        
113700             MOVE OHUV-IDDC-TVS     TO WS-IDDC                            
113800           ELSE                                                           
113900             MOVE OHUV-IDDC-PRIM      TO WS-IDDC                          
114000           END-IF                                                         
114100           IF (OHUV-FLKLAR = NEJ AND NOT SVARSBILD AND                    
114200              (OHUV-IDSYSTEM NOT = '4202' AND '4244'))  OR                
114300              (SW-KDORDSTA-ALL-E-FLAG = JA         AND                    
114400               SW-KDORDSTA-O-ALL-SPACE-FLAG = JA )                        
114500             MOVE ERR-ORDER-EJ-AVSLUT TO MED-IDMFSFEL                     
114600             MOVE NEJ                 TO ALLT-SW                          
114700           ELSE                                                           
114800                                                                          
114900******************************************************************        
115000*                                                                         
115100*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
115200*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
115300*                                                                         
115400******************************************************************        
115500                                                                          
115600             MOVE WS-IDDISTR   TO W-TP4TRAN-IDDISTR                       
115700                                                                          
115800             PERFORM DB2-SELECT-TP4TRAN                                   
115900                                                                          
116000             MOVE WS-IDDISTR TO TEST-IDDISTR                              
116100             IF OHUV-FLFORBI = JA OR                                      
116200                OHUV-FLFORBI = SPEC-FORBI OR                              
116300                OHUV-FLORDSPE = JA OR                                     
116400                OHUV-KDTPOTYP = +3 OR OHUV-KDORDKL = +0 OR                
116500                OHUV-FLOVRLEV = JA                      OR                
116600                OHUV-IDSYSTEM = 'LDC '                  OR                
116700                OHUV-IDSYSTEM(1:3) = 'LYN'              OR                
116800                DIST35-REFILL                           OR                
116900                DIST35-REFILL-INOM-NDC                  OR                
117000                DIST35-NA-TRANSFER                      OR                
117100                DIST35-NA-NDC-RETURNS                   OR                
117200                DIST35-PACIFIC-TRANSFER                 OR                
117300                DIST35-REFILL-INOM-JP                   OR                
117400                DIST35-CN-TRANSFER                      OR                
117500                DIST35-NONVCC-REFILL                    OR                
117600                DIST35-NONVCC-VCC-TRANSFER              OR                
117700                DIST35-NONVCC-NONVCC-TRANSFER           OR                
117800                RADER-FINNS                                               
117900                MOVE NEJ              TO ALLT-SW                          
118000                MOVE ERR-EJ-TILLAEGG  TO MED-IDMFSFEL                     
118100             ELSE                                                         
118200                MOVE OHUV-KDORDKL     TO MOD-KDORDKL-UT                   
118300                PERFORM S10-HAMTA-WDB6-INFO                               
118400                IF DCS-CDC                                                
118500                  PERFORM CD-KONTROLLERA-STATUS-CDC                       
118600                ELSE                                                      
118700                  IF DCS-NDC AND NOT DCS-CHINA                            
118800                     PERFORM CE-KONTROLLERA-STATUS-NDC                    
118900                  ELSE                                                    
119000*                  IF OHUV-IDDC-CLEAR(3) NOT = SPACE                      
119100*                     PERFORM CF-KONTROLLERA-STATUS-LDC                   
119200*                  ELSE                                                   
119300*                     PERFORM CB-KONTROLLERA-STATUS-SDC                   
119400*                  END-IF                                                 
119500                   PERFORM CF-KONTROLLERA-STATUS-EJ-CDC                   
119600                  END-IF                                                  
119700                END-IF                                                    
119800                                                                          
119900                PERFORM CG-LAS-ARBETSTABELLER                             
120000             END-IF                                                       
120100           END-IF                                                         
120200         ELSE                                                             
120300           MOVE ERR-ORDER-ANNULL       TO MED-IDMFSFEL                    
120400           MOVE NEJ                    TO ALLT-SW                         
120500         END-IF                                                           
120600       END-IF                                                             
120700     ELSE                                                                 
120800        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
120900        MOVE NEJ                     TO ALLT-SW                           
121000     END-IF                                                               
121100     MOVE SPACE                      TO MOD-KDVALISO                      
121200                                                                          
121300     IF MFS-FIRST AND ALLT-OK                                             
121400        MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-ATTR(1)                  
121500        PERFORM MFS-RENSA-MOD-RADER                                       
121600        MOVE NEJ                    TO ALLT-SW                            
121700     END-IF                                                               
121800     .                                                                    
121900     EJECT                                                                
122000 CA-KDORDSTA-VALIDATE  SECTION.                                           
122100                                                                          
122200     MOVE 'STA CA-KDORDSTA'               TO   WS-PGM-POSITION            
122300                                                                          
122400     MOVE JA TO SW-KDORDSTA-ALL-E-FLAG                                    
122500     MOVE JA TO SW-KDORDSTA-O-ALL-SPACE-FLAG                              
122600     MOVE JA TO SW-KDORDSTA-O-STATUS-FLAG                                 
122700     MOVE JA TO SW-KDORDSTA-STATUS-FLAG                                   
122800                                                                          
122900     PERFORM IMS-GNP-WDQ212                                               
123000                                                                          
123100     PERFORM UNTIL SEGMENT-SAKNAS                                         
123200        IF ARB-IDDC = OHUV-IDDC-PRIM                                      
123300           MOVE ARB-KDORDSTA   TO WS-ARB-KDORDSTA-PRIM                    
123400           MOVE ARB-KDORDSTA-O TO WS-ARB-KDORDSTA-O-PRIM                  
123500        END-IF                                                            
123600                                                                          
123700        IF ARB-KDORDSTA NOT = 'E'                                         
123800           MOVE NEJ TO SW-KDORDSTA-ALL-E-FLAG                             
123900        END-IF                                                            
124000        IF ARB-KDORDSTA-O NOT = SPACE                                     
124100           MOVE NEJ TO SW-KDORDSTA-O-ALL-SPACE-FLAG                       
124200        END-IF                                                            
124300        IF ARB-KDORDSTA-O NOT = ' ' AND                                   
124400           ARB-KDORDSTA-O NOT = 'B' AND                                   
124500           ARB-KDORDSTA-O NOT = 'C' AND                                   
124600           ARB-KDORDSTA-O NOT = 'R'                                       
124700           MOVE NEJ TO SW-KDORDSTA-O-STATUS-FLAG                          
124800        END-IF                                                            
124900        IF ARB-KDORDSTA   NOT = ' ' AND                                   
125000           ARB-KDORDSTA   NOT = 'B' AND                                   
125100           ARB-KDORDSTA   NOT = 'C' AND                                   
125200           ARB-KDORDSTA   NOT = 'R'                                       
125300           MOVE NEJ TO SW-KDORDSTA-STATUS-FLAG                            
125400        END-IF                                                            
125500                                                                          
125600        PERFORM IMS-GNP-WDQ212                                            
125700     END-PERFORM                                                          
125800     .                                                                    
125900     EJECT                                                                
126000                                                                          
126100 CB-HAMTA-KUND      SECTION.                                              
126200                                                                          
126300     MOVE 'STA CB-HAMTA   '         TO WS-PGM-POSITION                    
126400                                                                          
126500     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
126600     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
126700     PERFORM IMS-GU-WDB201                                                
126800                                                                          
126900     IF OHUV-KDORDKL > 1                                                  
127000                                                                          
127100        MOVE +1 TO WS-INDEX                                               
127200        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
127300           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
127400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
127500           ADD +1 TO WS-INDEX                                             
127600        END-PERFORM                                                       
127700                                                                          
127800     ELSE                                                                 
127900       IF OHUV-KDORDKL = 1                                                
128000                                                                          
128100          MOVE +1 TO WS-INDEX                                             
128200          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
128300             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
128400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
128500             ADD +1 TO WS-INDEX                                           
128600          END-PERFORM                                                     
128700                                                                          
128800       ELSE                                                               
128900         IF OHUV-KDORDKL = 0                                              
129000                                                                          
129100            MOVE +1 TO WS-INDEX                                           
129200            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
129300               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
129400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
129500               ADD +1 TO WS-INDEX                                         
129600            END-PERFORM                                                   
129700                                                                          
129800         END-IF                                                           
129900       END-IF                                                             
130000     END-IF                                                               
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 CC-HAMTA-WDB6-INFO SECTION.                                              
130500                                                                          
130600     MOVE SPACE                TO CLDC-W411CLDC                           
130700     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
130800                                                                          
130900     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
131000     MOVE +1 TO WS-INDEX                                                  
131100     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
131200        ADD +1 TO WS-INDEX                                                
131300     END-PERFORM                                                          
131400     .                                                                    
131500     EJECT                                                                
131600*                                                                         
131700 CD-KONTROLLERA-STATUS-CDC SECTION.                                       
131800                                                                          
131900     MOVE 'STA CD-KONTROLL'               TO   WS-PGM-POSITION            
132000     IF WS-ARB-KDORDSTA-O-PRIM NOT = SPACE                                
132100       IF WS-ARB-KDORDSTA-O-PRIM                                          
132200                             = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
132300                          OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
132400                          OR 'S' OR 'S*' OR 'SF'                          
132500          MOVE JA                        TO TILLAEGG-TPO-SW               
132600                                            RAD-GODKAND-SW                
132700       ELSE                                                               
132800          IF (WS-ARB-KDORDSTA-O-PRIM = 'B' OR 'C' OR 'R')                 
132900             MOVE JA                     TO VANLIGA-RADER-C1-SW           
133000                                            RAD-GODKAND-SW                
133100          ELSE                                                            
133200             IF WS-ARB-KDORDSTA-O-PRIM = 'R*'                             
133300                MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
133400                                            RAD-GODKAND-SW                
133500             ELSE                                                         
133600                MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
133700                MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
133800                MOVE NEJ                 TO TILLAEGG-TPO-SW               
133900             END-IF                                                       
134000          END-IF                                                          
134100       END-IF                                                             
134200     ELSE                                                                 
134300       IF WS-ARB-KDORDSTA-PRIM                                            
134400                           = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
134500                      'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
134600                       OR 'S*' OR 'S' OR 'SF'                             
134700          MOVE JA                        TO TILLAEGG-TPO-SW               
134800                                            RAD-GODKAND-SW                
134900       ELSE                                                               
135000          IF WS-ARB-KDORDSTA-PRIM = 'B' OR 'C' OR 'R'                     
135100             MOVE JA                     TO VANLIGA-RADER-C1-SW           
135200                                            RAD-GODKAND-SW                
135300          ELSE                                                            
135400             IF WS-ARB-KDORDSTA-PRIM = 'R*'                               
135500                MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
135600                                            RAD-GODKAND-SW                
135700             ELSE                                                         
135800                MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
135900                MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
136000                MOVE NEJ                 TO TILLAEGG-TPO-SW               
136100             END-IF                                                       
136200          END-IF                                                          
136300       END-IF                                                             
136400     END-IF                                                               
136500     .                                                                    
136600     EJECT                                                                
136700*CB-KONTROLLERA-STATUS-SDC SECTION.                                       
136800*                                                                         
136900*    MOVE 'STA CB-KONTROLL'               TO   WS-PGM-POSITION            
137000*    IF OHUV-KDORDSTA-O(2) NOT = SPACE                                    
137100*      IF OHUV-KDORDSTA-O(1) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
137200*                         OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
137300*                         OR 'S*' OR 'S' OR 'SF'                          
137400*         MOVE JA                        TO TILLAEGG-TPO-SW               
137500*                                           RAD-GODKAND-SW                
137600*      ELSE                                                               
137700*         IF (OHUV-KDORDSTA-O(2) = 'B' OR 'C' OR 'R')                     
137800*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
137900*                                           RAD-GODKAND-SW                
138000*         ELSE                                                            
138100*            IF OHUV-KDORDSTA-O(2) = 'R*'                                 
138200*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
138300*                                           RAD-GODKAND-SW                
138400*            ELSE                                                         
138500*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
138600*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
138700*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
138800*            END-IF                                                       
138900*         END-IF                                                          
139000*      END-IF                                                             
139100*      IF OHUV-KDORDSTA-O(1) = 'B' OR 'C' OR 'R'                          
139200*         MOVE JA                     TO RAD-GODKAND-SW                   
139300*      END-IF                                                             
139400*    ELSE                                                                 
139500*      IF OHUV-KDORDSTA(2) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
139600*                     'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
139700*                      OR 'S*' OR 'S' OR 'SF'                             
139800*         MOVE JA                        TO TILLAEGG-TPO-SW               
139900*                                           RAD-GODKAND-SW                
140000*      ELSE                                                               
140100*         IF (OHUV-KDORDSTA(2) = 'B' OR 'C' OR 'R')                       
140200*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
140300*                                           RAD-GODKAND-SW                
140400*         ELSE                                                            
140500*            IF OHUV-KDORDSTA(2) = 'R*'                                   
140600*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
140700*                                           RAD-GODKAND-SW                
140800*            ELSE                                                         
140900*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
141000*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
141100*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
141200*            END-IF                                                       
141300*         END-IF                                                          
141400*      END-IF                                                             
141500*      IF (OHUV-KDORDSTA(1) = 'B' OR 'C' OR 'R')                          
141600*         MOVE JA                     TO RAD-GODKAND-SW                   
141700*      END-IF                                                             
141800*    END-IF                                                               
141900*    .                                                                    
142000*    EJECT                                                                
142100*                                                                         
142200*CF-KONTROLLERA-STATUS-LDC SECTION.                                       
142300*                                                                         
142400*    MOVE 'STA CF-KONTROLL'               TO   WS-PGM-POSITION            
142500*    IF OHUV-KDORDSTA-O(3) NOT = SPACE                                    
142600*      IF OHUV-KDORDSTA-O(3) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
142700*                         OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
142800*         MOVE JA                        TO TILLAEGG-TPO-SW               
142900*                                           RAD-GODKAND-SW                
143000*      ELSE                                                               
143100*         IF (OHUV-KDORDSTA-O(3) = 'B' OR 'C' OR 'R')                     
143200*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
143300*                                           RAD-GODKAND-SW                
143400*         ELSE                                                            
143500*            IF OHUV-KDORDSTA-O(3) = 'R*'                                 
143600*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
143700*                                           RAD-GODKAND-SW                
143800*            ELSE                                                         
143900*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
144000*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
144100*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
144200*            END-IF                                                       
144300*         END-IF                                                          
144400*      END-IF                                                             
144500*      IF  (OHUV-KDORDSTA-O(1) = 'B' OR 'C' OR 'R')                       
144600*      AND (OHUV-KDORDSTA-O(2) = 'B' OR 'C' OR 'R')                       
144700*         MOVE JA                     TO RAD-GODKAND-SW                   
144800*      END-IF                                                             
144900*    ELSE                                                                 
145000*      IF OHUV-KDORDSTA(3) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
145100*                     'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
145200*         MOVE JA                        TO TILLAEGG-TPO-SW               
145300*                                           RAD-GODKAND-SW                
145400*      ELSE                                                               
145500*         IF (OHUV-KDORDSTA(3) = 'B' OR 'C' OR 'R')                       
145600*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
145700*                                           RAD-GODKAND-SW                
145800*         ELSE                                                            
145900*            IF OHUV-KDORDSTA(3) = 'R*'                                   
146000*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
146100*                                           RAD-GODKAND-SW                
146200*            ELSE                                                         
146300*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
146400*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
146500*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
146600*            END-IF                                                       
146700*         END-IF                                                          
146800*      END-IF                                                             
146900*      IF  (OHUV-KDORDSTA(1) = 'B' OR 'C' OR 'R')                         
147000*      AND (OHUV-KDORDSTA(2) = 'B' OR 'C' OR 'R')                         
147100*         MOVE JA                     TO RAD-GODKAND-SW                   
147200*      END-IF                                                             
147300*    END-IF                                                               
147400*    .                                                                    
147500*    EJECT                                                                
147600 CE-KONTROLLERA-STATUS-NDC SECTION.                                       
147700                                                                          
147800     MOVE 'STA CE-KONTROLL'               TO   WS-PGM-POSITION            
147900     IF WS-ARB-KDORDSTA-O-PRIM NOT = SPACE                                
148000       IF WS-ARB-KDORDSTA-O-PRIM  = 'R'                                   
148100          MOVE JA                        TO RAD-GODKAND-SW                
148200       END-IF                                                             
148300     ELSE                                                                 
148400       IF WS-ARB-KDORDSTA-PRIM  = 'R'                                     
148500          MOVE JA                        TO RAD-GODKAND-SW                
148600       END-IF                                                             
148700     END-IF                                                               
148800     .                                                                    
148900     EJECT                                                                
149000 CF-KONTROLLERA-STATUS-EJ-CDC SECTION.                                    
149100     MOVE 'STA CF-KONTROLL'               TO   WS-PGM-POSITION            
149200                                                                          
149300     IF SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                                
149400                                                                          
149500          IF SW-KDORDSTA-O-STATUS-FLAG = JA                               
149600                                                                          
149700               MOVE JA    TO VANLIGA-RADER-C1-SW                          
149800                             RAD-GODKAND-SW                               
149900          ELSE                                                            
150000               IF NOT DCS-CHINA                                           
150100                  MOVE JA  TO TILLAEGG-TPO-SW                             
150200                              RAD-GODKAND-SW                              
150300               ELSE                                                       
150400                  MOVE NEJ TO TILLAEGG-TPO-SW                             
150500                              RAD-GODKAND-SW                              
150600               END-IF                                                     
150700          END-IF                                                          
150800     ELSE                                                                 
150900          IF SW-KDORDSTA-STATUS-FLAG = JA                                 
151000                                                                          
151100                  MOVE JA TO VANLIGA-RADER-C1-SW                          
151200                             RAD-GODKAND-SW                               
151300          ELSE                                                            
151400               IF NOT DCS-CHINA                                           
151500                  MOVE JA  TO TILLAEGG-TPO-SW                             
151600                              RAD-GODKAND-SW                              
151700               ELSE                                                       
151800                  MOVE NEJ TO TILLAEGG-TPO-SW                             
151900                              RAD-GODKAND-SW                              
152000               END-IF                                                     
152100          END-IF                                                          
152200     END-IF                                                               
152300     .                                                                    
152400     EJECT                                                                
152500                                                                          
152600                                                                          
152700 CG-LAS-ARBETSTABELLER SECTION.                                           
152800                                                                          
152900     MOVE 'STA CG-LAS-ARB '               TO   WS-PGM-POSITION            
153000     MOVE WS-IDDC         TO W-IDDC-WDQ212                                
153100     PERFORM IMS-GNP-WDQ212-FIRST                                         
153200     IF SEGMENT-FINNS                                                     
153300       IF ARB-IDDC = OHUV-IDDC-PRIM                                       
153400          MOVE ARB-KDFRAKT         TO MOD-KDFRAKT-UT                      
153500       END-IF                                                             
153600     ELSE                                                                 
153700       MOVE NEJ                     TO ALLT-SW                            
153800       MOVE ERR-ORDERLINES-MISSING  TO MED-IDMFSFEL                       
153900     END-IF                                                               
154000     .                                                                    
154100     EJECT                                                                
154200                                                                          
154300                                                                          
154400 D-FORMELL-KONTROLL SECTION.                                              
154500     MOVE 'STA D-FORMELL  '          TO WS-PGM-POSITION                   
154600                                                                          
154700     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
154800     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
154900     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
155000     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
155100     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
155200     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
155300     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
155400     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
155500     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
155600     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
155700     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
155800     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
155900     MOVE +1                   TO WS-INDEX-MID                            
156000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
156100        MOVE NEJ               TO ORFK-FLINVEST(WS-INDEX-MID)             
156200        IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                            
156300           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN(WS-INDEX-MID)              
156400        ELSE                                                              
156500           IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                             
156600              MOVE JA          TO MID-FLRESTN(WS-INDEX-MID)               
156700           END-IF                                                         
156800           MOVE MID-FLRESTN(WS-INDEX-MID)                                 
156900                               TO ORFK-FLRESTN(WS-INDEX-MID)              
157000        END-IF                                                            
157100        IF MID-FLSLATT(WS-INDEX-MID) = ALL '+'                            
157200           MOVE JA             TO ORFK-FLSLATT(WS-INDEX-MID)              
157300        ELSE                                                              
157400           IF MID-FLSLATT(WS-INDEX-MID) = 'Y'                             
157500              MOVE JA          TO MID-FLSLATT(WS-INDEX-MID)               
157600           END-IF                                                         
157700           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
157800                               TO ORFK-FLSLATT(WS-INDEX-MID)              
157900        END-IF                                                            
158000                                                                          
158100        MOVE MID-IDARTNR(WS-INDEX-MID)                                    
158200                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
158300                                                                          
158400        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
158500                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
158600                                                                          
158700        IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                           
158800           MOVE OHUV-KDVRINFO  TO ORFK-KDVRINFO(WS-INDEX-MID)             
158900        ELSE                                                              
159000           MOVE MID-KDVRINFO(WS-INDEX-MID)                                
159100                               TO ORFK-KDVRINFO(WS-INDEX-MID)             
159200        END-IF                                                            
159300                                                                          
159400        MOVE MID-KVBEART(WS-INDEX-MID)                                    
159500                               TO ORFK-KVBEART(WS-INDEX-MID)              
159600                                                                          
159700        IF DIST79-DEALER-PRICE                                            
159800           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
159900                               TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
160000           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)             
160100           MOVE ALL '+'        TO                                         
160200                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
160300        ELSE                                                              
160400           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
160500                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
160600           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
160700           MOVE ALL '+'        TO                                         
160800                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
160900        END-IF                                                            
161000                                                                          
161100        MOVE MID-TITPO(WS-INDEX-MID)                                      
161200                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
161300        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
161400        ADD +1                 TO WS-INDEX-MID                            
161500     END-PERFORM                                                          
161600                                                                          
161700     CALL W411ORFK USING ORFK-W411ORFK                                    
161800                         AREG-WDK6-PCB                                    
161900                         AREG-WDK7-PCB                                    
162000                                                                          
162100     MOVE +1                   TO WS-INDEX-MID                            
162200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
162300        PERFORM DA-KOLLA-FEL-FK                                           
162400        ADD +1                 TO WS-INDEX-MID                            
162500     END-PERFORM                                                          
162600                                                                          
162700     IF SVARSBILD AND NOT ALLT-OK                                         
162800        MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4203'           
162900                               TO FELTEXT                                 
163000        CALL ABEND USING RKOD-ABEND                                       
163100     END-IF                                                               
163200     .                                                                    
163300     EJECT                                                                
163400 DA-KOLLA-FEL-FK SECTION.                                                 
163500                                                                          
163600     MOVE 'STA DA-KOLLA-FEL'              TO   WS-PGM-POSITION            
163700     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
163800        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
163900        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)        
164000        MOVE NEJ                TO ALLT-SW                                
164100     END-IF                                                               
164200                                                                          
164300     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
164400        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
164500        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR(WS-INDEX-MID)        
164600        MOVE NEJ                 TO ALLT-SW                               
164700     END-IF                                                               
164800                                                                          
164900     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
165000        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
165100        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSLATT-ATTR(WS-INDEX-MID)        
165200        MOVE NEJ                 TO ALLT-SW                               
165300     END-IF                                                               
165400                                                                          
165500     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
165600        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
165700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
165800        MOVE NEJ                 TO ALLT-SW                               
165900     ELSE                                                                 
166000        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
166100                  AND NOT MFS-UPDATE                                      
166200          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
166300          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
166400          MOVE NEJ               TO ALLT-SW                               
166500        END-IF                                                            
166600     END-IF                                                               
166700                                                                          
166800     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
166900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
167000        MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)         
167100        MOVE NEJ                 TO ALLT-SW                               
167200     END-IF                                                               
167300                                                                          
167400     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
167500        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
167600        MOVE MFS-NUM-FAELT-FEL TO MOD-KDVRINFO-ATTR(WS-INDEX-MID)         
167700        MOVE NEJ                 TO ALLT-SW                               
167800     END-IF                                                               
167900     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
168000        IF MED-IDMFSFEL = SPACE                                           
168100           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC  AND                     
168200                    ORFK-KVBEART(WS-INDEX-MID) > ZERO                     
168300              IF NOT MFS-UPDATE                                           
168400                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
168500                 MOVE MFS-NUM-FAELT-FEL   TO                              
168600                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
168700                 MOVE NEJ             TO ALLT-SW                          
168800              END-IF                                                      
168900           ELSE                                                           
169000            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
169100            MOVE MFS-NUM-FAELT-FEL   TO                                   
169200                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
169300            MOVE NEJ                 TO ALLT-SW                           
169400           END-IF                                                         
169500        ELSE                                                              
169600           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
169700           MOVE MFS-NUM-FAELT-FEL   TO                                    
169800                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
169900           MOVE NEJ                 TO ALLT-SW                            
170000        END-IF                                                            
170100     END-IF                                                               
170200                                                                          
170300     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
170400        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
170500        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
170600        MOVE NEJ               TO ALLT-SW                                 
170700     END-IF                                                               
170800                                                                          
170900     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
171000        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
171100        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
171200        MOVE NEJ                 TO ALLT-SW                               
171300     END-IF                                                               
171400                                                                          
171500     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
171600        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
171700        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
171800        MOVE NEJ                 TO ALLT-SW                               
171900     END-IF                                                               
172000                                                                          
172100     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
172200        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
172300        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR(WS-INDEX-MID)          
172400        MOVE NEJ                 TO ALLT-SW                               
172500     END-IF                                                               
172600     .                                                                    
172700     EJECT                                                                
172800 E-BEHANDLA-RADER SECTION.                                                
172900                                                                          
173000     MOVE 'STA E-BEHANDLA  '              TO   WS-PGM-POSITION            
173100     MOVE +1 TO WS-INDEX-MID                                              
173200     MOVE NEJ                     TO TILLK-SW                             
173300                                     OBKR-SW                              
173400     MOVE +0                      TO WS-IDPRQUES                          
173500     MOVE JA                      TO FIRST-TIME-SW                        
173600     MOVE SPACE                   TO WS-IDDC-DDGS                         
173700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
173800        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
173900           PERFORM S02-RENSA-TILLK-TAB                                    
174000           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
174100                                  TO AREG-W411AREG-001                    
174200           PERFORM EC-BEHANDLA-RAD                                        
174300           MOVE SPACE             TO WS-IDDC-DDGS                         
174400                                                                          
174500           PERFORM S10-HAMTA-WDB6-INFO                                    
174600                                                                          
174700           IF DCS-NDC-NA                                                  
174800              PERFORM S03-DATA-TILL-DEL-NOTE                              
174900           END-IF                                                         
175000           MOVE JA                TO TILLK-SW                             
175100           MOVE +1                TO WS-INDEX-TILLK                       
175200           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
175300              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
175400              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
175500                 PERFORM ED-LAES-TILLK-DATA                               
175600                 PERFORM EC-BEHANDLA-RAD                                  
175700                 MOVE SPACE       TO WS-IDDC-DDGS                         
175800                    IF DCS-NDC-NA                                         
175900                       PERFORM S03-DATA-TILL-DEL-NOTE                     
176000                    END-IF                                                
176100              END-IF                                                      
176200              ADD +1              TO WS-INDEX-TILLK                       
176300           END-PERFORM                                                    
176400        END-IF                                                            
176500        MOVE NEJ                  TO TILLK-SW                             
176600                                     OBKR-SW                              
176700        MOVE SPACE                TO WS-IDDC-DDGS                         
176800        ADD +1 TO WS-INDEX-MID                                            
176900     END-PERFORM                                                          
177000                                                                          
177100     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
177200       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
177300       MOVE +3                      TO PRNO-KDCALL                        
177400                                                                          
177500       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
177600     END-IF                                                               
177700                                                                          
177800     IF AVSR-IDDC(1) NOT = SPACE                                          
177900        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIMS-PCB                   
178000              AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                   
178100              AVSR-WDB2-PCB AVSR-WDB6-PCB                                 
178200              TRAN-XXKB-PCB                                               
178300     END-IF                                                               
178400     MOVE JA                      TO ALLT-SW                              
178500     .                                                                    
178600     EJECT                                                                
178700                                                                          
178800 EC-BEHANDLA-RAD SECTION.                                                 
178900                                                                          
179000     MOVE 'STA EC-BEHANDLA '              TO   WS-PGM-POSITION            
179100     IF FIRST-TIME AND EGEN-MID                                           
179200       PERFORM ECE-OEPNA-ORDERN                                           
179300     END-IF                                                               
179400                                                                          
179500     PERFORM ECA-NOLLSTALL-OBKR                                           
179600     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
179700     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
179800     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
179900     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
180000                                                                          
180100     IF NOT TILLKOMMANDE-RAD                                              
180200        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
180300     END-IF                                                               
180400                                                                          
180500     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
180600     PERFORM ECJ-KOMPLETTERA-PRIS                                         
180700     PERFORM ECM-KOMPLETTERA-TPO1                                         
180800     PERFORM ECN-KOMPLETTERA-TPO2                                         
180900     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
181000     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
181100     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
181200     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
181300     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
181400     PERFORM ECG-PREL-AVBOKNING-XDC                                       
181500*    PERFORM ECX-PREL-AVBOKNING-SDC                                       
181600     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
181700     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
181800**** HAR TAGIT BORT ORAD-IDDC-TVS I NEDANSTÅENDE IF                       
181900     IF TILLAEGG-TPO AND                                                  
182000       ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                               
182100        MOVE NEJ             TO ALLT-SW                                   
182200        MOVE JA              TO OBKR-SW                                   
182300        MOVE NEJ             TO RAD-GODKAND-SW                            
182400     END-IF                                                               
182500     PERFORM ECR-PREL-AVBOKNING-CDC                                       
182600                                                                          
182700     IF NOT TILLKOMMANDE-RAD                                              
182800        IF SKRIV-OBKR                                                     
182900           PERFORM ECS-SKRIV-OBKR                                         
183000           IF NOT OBKR-SKRIVEN OR                                         
183100              EGET-CL-RAD                                                 
183200              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
183300              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
183400           END-IF                                                         
183500        ELSE                                                              
183600           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
183700                               OR KAMP-FLKLAR = JA                        
183800                               OR RELS-FLKLAR = JA                        
183900                               OR TILLAEGG-TPO                            
184000              CONTINUE                                                    
184100           ELSE                                                           
184200              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
184300              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
184400           END-IF                                                         
184500        END-IF                                                            
184600     ELSE                                                                 
184700        IF SKRIV-OBKR                                                     
184800           PERFORM ECS-SKRIV-OBKR                                         
184900        END-IF                                                            
185000     END-IF                                                               
185100     .                                                                    
185200     EJECT                                                                
185300 ECA-NOLLSTALL-OBKR SECTION.                                              
185400                                                                          
185500     MOVE 'STA ECA-NOLLA   '              TO   WS-PGM-POSITION            
185600     MOVE +0                   TO KVAN-KDORDBEK-UT                        
185700     MOVE +0                   TO KVAN-KVQPACK-UT                         
185800     MOVE +0                   TO DLEV-KDORDBEK-UT                        
185900     MOVE +0                   TO KERS-KDERS                              
186000     IF NOT TILLKOMMANDE-RAD                                              
186100        MOVE +0                TO KERS-KDORDBEK                           
186200     ELSE                                                                 
186300        MOVE JA                TO OBKR-SW                                 
186400     END-IF                                                               
186500     MOVE +0                   TO TPO1-KDORDBEK                           
186600     MOVE +0                   TO TPO2-KDORDBEK                           
186700     MOVE +0                   TO RELS-KDORDBEK                           
186800     MOVE +0                   TO KAMP-KDORDBEK                           
186900     MOVE +0                   TO STOR-KDORDBEK                           
187000     MOVE +0                   TO XDCA-KDORDBEK                           
187100*    MOVE +0                   TO NDCA-KDORDBEK                           
187200     MOVE +0                   TO SDCA-KDORDBEK                           
187300     MOVE +0                   TO SDCA-KDORDBEK-FIRST-SDC                 
187400     MOVE +0                   TO SDCA-KDORDBEK-SECOND-SDC                
187500     MOVE +0                   TO CDCA-KDORDBEK-UT                        
187600     MOVE +1                   TO WS-INDEX                                
187700     MOVE ZERO                 TO SPAR-KDORDBEK                           
187800                                                                          
187900     MOVE JA                   TO ALLT-SW                                 
188000     MOVE NEJ                  TO EGET-CL-RAD-SW                          
188100                                                                          
188200     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
188300       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
188400       MOVE +7                          TO W-KDTPOTYP                     
188500     ELSE                                                                 
188600       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
188700         MOVE NEJ               TO ALLT-SW                                
188800                                   KOLLA-ERS-SW                           
188900         MOVE JA                TO OBKR-SW                                
189000       END-IF                                                             
189100     END-IF                                                               
189200     .                                                                    
189300     EJECT                                                                
189400 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
189500                                                                          
189600     MOVE 'STA ECB-BYGG    '              TO   WS-PGM-POSITION            
189700     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
189800     IF OHUV-IDDC-TVS > ZERO                                              
189900        MOVE OHUV-IDDC-TVS     TO ORAD-IDDC                               
190000     ELSE                                                                 
190100       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
190200     END-IF                                                               
190300     IF TILLAEGG-TPO                                                      
190400        MOVE WC-CDC-SE         TO ORAD-IDDC                               
190500     END-IF                                                               
190600     MOVE ORAD-IDDC            TO WS-IDDC                                 
190700     MOVE +0                   TO ORAD-ADLAGOMR                           
190800     MOVE +0                   TO ORAD-ADGANG                             
190900     MOVE +0                   TO ORAD-ADPLATS                            
191000     IF TILLKOMMANDE-RAD                                                  
191100        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
191200     ELSE                                                                 
191300        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
191400                               TO ORAD-IDARTNR                            
191500     END-IF                                                               
191600     MOVE +1                   TO ORAD-IDLOPNR                            
191700                                                                          
191800     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
191900        MOVE SPACE             TO ORAD-BERADREF                           
192000     ELSE                                                                 
192100        MOVE MID-BERADREF(WS-INDEX-MID)                                   
192200                               TO ORAD-BERADREF                           
192300     END-IF                                                               
192400     MOVE OHUV-BEKUNDRF        TO ORAD-BEVOLREF                           
192500     MOVE SPACE                TO ORAD-FLAKPLOC                           
192600     MOVE 'N'                  TO ORAD-FLSDCLEV                           
192700                                                                          
192800     MOVE NEJ                  TO ORAD-FLINVEST                           
192900     MOVE JA                   TO ORAD-FLOBTRAN                           
193000     IF TILLKOMMANDE-RAD                                                  
193100        IF DIST79-DEALER-PRICE                                            
193200          MOVE NEJ             TO ORAD-FLPRTILL                           
193300        ELSE                                                              
193400         IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                            
193500           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
193600                               TO ORAD-FLPRTILL                           
193700         ELSE                                                             
193800           MOVE NEJ            TO ORAD-FLPRTILL                           
193900         END-IF                                                           
194000        END-IF                                                            
194100     ELSE                                                                 
194200        MOVE NEJ               TO ORAD-FLPRTILL                           
194300     END-IF                                                               
194400     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
194500        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
194600     ELSE                                                                 
194700        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
194800                               TO ORAD-FLRESTN                            
194900     END-IF                                                               
195000     IF TILLKOMMANDE-RAD                                                  
195100        MOVE JA                TO ORAD-FLTILLK                            
195200     ELSE                                                                 
195300        MOVE NEJ               TO ORAD-FLTILLK                            
195400     END-IF                                                               
195500     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
195600     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
195700     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
195800     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
195900     IF OHUV-IDKAMPRF > +0                                                
196000        MOVE WC-CDC-SE         TO ORAD-IDDC                               
196100        MOVE ORAD-IDDC         TO WS-IDDC                                 
196200     END-IF                                                               
196300     MOVE SPACE                TO ORAD-IDLEVNR                            
196400     MOVE +0                   TO ORAD-IDLOPNR-RO                         
196500     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
196600     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
196700     MOVE ZERO                 TO ORAD-IDSPECEMB                          
196800     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
196900     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
197000     IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                              
197100        MOVE OHUV-KDVRINFO     TO ORAD-KDDSP                              
197200     ELSE                                                                 
197300        MOVE MID-KDVRINFO(WS-INDEX-MID)                                   
197400                               TO WS-ALFA-1                               
197500        MOVE WS-NUM-1          TO ORAD-KDDSP                              
197600     END-IF                                                               
197700     IF ORAD-KDDSP = +0                                                   
197800        MOVE +1                TO ORAD-KDDSP                              
197900     END-IF                                                               
198000     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
198100     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
198200        MOVE +0                TO ORAD-KDKVBRYT                           
198300     ELSE                                                                 
198400        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
198500                               TO ORAD-KDKVBRYT                           
198600     END-IF                                                               
198700     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
198800     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
198900     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
199000                                  TEST-KDPRODSL                           
199100     IF ORAD-KDORDING = +3                                                
199200       MOVE SPACE              TO ORAD-KDOI                               
199300     ELSE                                                                 
199400       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
199500         MOVE 'CD'             TO ORAD-KDOI                               
199600       ELSE                                                               
199700         MOVE 'DT'             TO ORAD-KDOI                               
199800       END-IF                                                             
199900     END-IF                                                               
200000     MOVE SPACE                TO ORAD-CLEARGROUP                         
200100                                                                          
200200     IF TILLKOMMANDE-RAD                                                  
200300       IF DIST79-DEALER-PRICE                                             
200400         MOVE SPACE            TO ORAD-KDPRTYP                            
200500       ELSE                                                               
200600        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
200700           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
200800                               TO ORAD-KDPRTYP                            
200900        ELSE                                                              
201000           MOVE SPACE          TO ORAD-KDPRTYP                            
201100        END-IF                                                            
201200       END-IF                                                             
201300     ELSE                                                                 
201400        MOVE SPACE             TO ORAD-KDPRTYP                            
201500     END-IF                                                               
201600     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
201700     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
201800        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
201900     ELSE                                                                 
202000        IF OHUV-KDTPOTYP = +0 AND                                         
202100                   MID-TITPO(WS-INDEX-MID) NOT = ALL '+'                  
202200           IF OHUV-IDKAMPRF > +0                                          
202300              MOVE +4          TO ORAD-KDTPOTYP                           
202400           ELSE                                                           
202500              MOVE +2          TO ORAD-KDTPOTYP                           
202600              IF ORAD-KDORDING = +3                                       
202700                 CONTINUE                                                 
202800              ELSE                                                        
202900                 MOVE +2       TO ORAD-KDORDING                           
203000              END-IF                                                      
203100           END-IF                                                         
203200        ELSE                                                              
203300           MOVE +0             TO ORAD-KDTPOTYP                           
203400        END-IF                                                            
203500     END-IF                                                               
203600     MOVE JA                   TO ORAD-FLORDING                           
203700     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
203800                                                                          
203900     IF TILLKOMMANDE-RAD                                                  
204000       MOVE TILK-KVBEART(WS-INDEX-TILLK)                                  
204100                              TO ORAD-KVBEART                             
204200     ELSE                                                                 
204300       MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                       
204400       MOVE WS-NUM-6           TO ORAD-KVBEART                            
204500     END-IF                                                               
204600                                                                          
204700     MOVE +0                   TO ORAD-KVBEART-Q                          
204800     MOVE +0                   TO ORAD-KVPREAVB                           
204900     MOVE +0                   TO ORAD-KVPRERO                            
205000     MOVE +0                   TO ORAD-KVOKS-PREL                         
205100     MOVE +0                   TO ORAD-IDPRQUES                           
205200     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
205300     MOVE +0                   TO ORAD-RERAB                              
205400     MOVE SPACE                TO ORAD-KDVALISO                           
205500     MOVE SPACE                TO ORAD-KDVAT                              
205600     MOVE SPACE                TO ORAD-KDRAB                              
205700     MOVE SPACE                TO ORAD-BEART-VIPS                         
205800                                                                          
205900     IF TILLKOMMANDE-RAD                                                  
206000       IF DIST79-DEALER-PRICE                                             
206100           MOVE +0             TO ORAD-PRARTNTO                           
206200           MOVE TILK-PRARTNTO-LOC(WS-INDEX-TILLK)                         
206300                                  TO ORAD-PRARTNTO-LOC                    
206400           MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                     
206500                                  TO ORAD-PRARTNTO-LOCPREL                
206600       ELSE                                                               
206700        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
206800             MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                           
206900                                 TO ORAD-PRARTNTO                         
207000             MOVE ZERO           TO ORAD-PRARTNTO-LOC                     
207100        ELSE                                                              
207200           MOVE +0             TO ORAD-PRARTNTO                           
207300                                  ORAD-PRARTNTO-LOC                       
207400                                  ORAD-PRARTNTO-LOCPREL                   
207500        END-IF                                                            
207600       END-IF                                                             
207700     ELSE                                                                 
207800        IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                           
207900           MOVE +0             TO ORAD-PRARTNTO                           
208000                                  ORAD-PRARTNTO-LOC                       
208100                                  ORAD-PRARTNTO-LOCPREL                   
208200        ELSE                                                              
208300          IF DIST79-DEALER-PRICE                                          
208400            MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                       
208500                               TO ORAD-PRARTNTO-LOC                       
208600            MOVE +0            TO ORAD-PRARTNTO                           
208700            MOVE +0            TO ORAD-PRARTNTO-LOCPREL                   
208800          ELSE                                                            
208900            MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                           
209000                               TO ORAD-PRARTNTO                           
209100            MOVE +0            TO ORAD-PRARTNTO-LOC                       
209200            MOVE +0            TO ORAD-PRARTNTO-LOCPREL                   
209300          END-IF                                                          
209400        END-IF                                                            
209500        MOVE +0                TO ORAD-PRBPRIS                            
209600     END-IF                                                               
209700     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
209800        MOVE MID-IDARTNR(WS-INDEX-MID) (11:1)                             
209900                               TO ORAD-REKSIFFR                           
210000     ELSE                                                                 
210100        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
210200     END-IF                                                               
210300     MOVE +0                   TO ORAD-RERF-RAD                           
210400     MOVE +0                   TO ORAD-KVSLATT                            
210500                                                                          
210600     IF TILLKOMMANDE-RAD                                                  
210700       IF DIST79-DEALER-PRICE                                             
210800          MOVE +0              TO ORAD-TIPRIS                             
210900       ELSE                                                               
211000        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
211100           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
211200                               TO ORAD-TIPRIS                             
211300        ELSE                                                              
211400           MOVE +0             TO ORAD-TIPRIS                             
211500        END-IF                                                            
211600       END-IF                                                             
211700     ELSE                                                                 
211800        MOVE +0                TO ORAD-TIPRIS                             
211900     END-IF                                                               
212000                                                                          
212100     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
212200     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
212300     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
212400     MOVE +0                   TO ORAD-TIRODAT                            
212500     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
212600        MOVE OHUV-TITPO        TO ORAD-TITPO                              
212700     ELSE                                                                 
212800        MOVE MID-TITPO(WS-INDEX-MID) TO WS-ALFA-6                         
212900        MOVE WS-NUM-6          TO ORAD-TITPO                              
213000     END-IF                                                               
213100     MOVE AREG-VKART           TO ORAD-VKART                              
213200     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
213300     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
213400     MOVE SPACE                TO ORAD-IDBIL                              
213500                                  ORAD-IDKLIENT                           
213600                                  ORAD-IDARBREF                           
213700                                  ORAD-IDVIN                              
213800                                                                          
213900     IF ORAD-KDORDKL = 1 AND                                              
214000        GMT-FLLDCKND = JA                                                 
214100        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
214200     ELSE                                                                 
214300        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
214400     END-IF                                                               
214500     MOVE +0                   TO ORAD-PRAVCOST                           
214600                                                                          
214700     .                                                                    
214800     EJECT                                                                
214900 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
215000                                                                          
215100     MOVE 'STA ECC-LAS-ART '              TO   WS-PGM-POSITION            
215200     IF ALLT-OK                                                           
215300                                                                          
215400     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
215500     PERFORM IMS-10-GU-WLARTM-WDK901                                      
215600                                                                          
215700     IF SEGMENT-SAKNAS                                                    
215800        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
215900        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
216000     END-IF                                                               
216100                                                                          
216200     END-IF                                                               
216300     .                                                                    
216400     EJECT                                                                
216500 ECE-OEPNA-ORDERN SECTION.                                                
216600                                                                          
216700     MOVE 'STA ECE-OEPNA-ORDER'           TO   WS-PGM-POSITION            
216800                                                                          
216900     PERFORM IMS-02-GHU-WDQ2-WDQ201                                       
217000     MOVE NEJ                    TO OHUV-FLKLAR                           
217100     MOVE '4202'                 TO OHUV-IDSYSTEM                         
217200     MOVE NEJ                    TO FIRST-TIME-SW                         
217300     PERFORM IMS-04-REPL-WDQ2-WDQ201                                      
217400                                                                          
217500     IF SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                                
217600        CONTINUE                                                          
217700     ELSE                                                                 
217800        PERFORM IMS-GHNP-WDQ212                                           
217900                                                                          
218000        PERFORM UNTIL SEGMENT-SAKNAS                                      
218100                                                                          
218200           MOVE ARB-KDORDSTA     TO ARB-KDORDSTA-O                        
218300           MOVE 'E '             TO ARB-KDORDSTA                          
218400           PERFORM IMS-REPL-WDQ212                                        
218500                                                                          
218600           PERFORM IMS-GHNP-WDQ212                                        
218700        END-PERFORM                                                       
218800                                                                          
218900     END-IF                                                               
219000*    TO RESTORE ARB-IO-AREA WITH PRIMARY DC                               
219100     MOVE OHUV-IDDC-PRIM    TO W-IDDC-WDQ212                              
219200     PERFORM IMS-GNP-WDQ212-FIRST                                         
219300*                                                                         
219400     .                                                                    
219500     EJECT                                                                
219600 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
219700                                                                          
219800     MOVE 'STA ECF-KVANT      '           TO   WS-PGM-POSITION            
219900     IF ALLT-OK                                                           
220000                                                                          
220100     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
220200     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
220300     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
220400     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
220500     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
220600     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
220700     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
220800     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
220900     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
221000     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
221100     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
221200     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
221300     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
221400     MOVE OHUV-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
221500     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
221600     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
221700     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
221800     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
221900     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
222000                                                                          
222100     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
222200                                                                          
222300     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
222400     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
222500                                                                          
222600     IF KVAN-KDORDBEK-UT > +0                                             
222700        MOVE JA                    TO OBKR-SW                             
222800     END-IF                                                               
222900                                                                          
223000     END-IF                                                               
223100     .                                                                    
223200     EJECT                                                                
223300 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
223400                                                                          
223500     MOVE 'STA ECI-DLEV       '           TO   WS-PGM-POSITION            
223600                                                                          
223700     PERFORM S10-HAMTA-WDB6-INFO                                          
223800                                                                          
223900     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                    
224000                                                                          
224100     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
224200     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
224300     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
224400     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
224500     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
224600     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
224700     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
224800     MOVE OHUV-IDDC-TVS        TO DLEV-IDDC-IN                            
224900     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
225000     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
225100     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
225200     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
225300     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
225400     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
225500     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
225600     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
225700                                                                          
225800                                                                          
225900     MOVE +1 TO WS-INDEX                                                  
226000     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
226100        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
226200                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
226300        ADD +1 TO WS-INDEX                                                
226400     END-PERFORM                                                          
226500                                                                          
226600                                                                          
226700     MOVE ZERO                 TO DLEV-KDCALL                             
226800                                  DLEV-IDKUNDRF-IN                        
226900     MOVE SPACE                TO DLEV-CLEARGROUP                         
227000                                  DLEV-KDOI-UT                            
227100                                                                          
227200     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
227300                                       DLEV-LEVG-PCB                      
227400                                       DLEV-LEVA-PCB                      
227500                                       DLEV-ARTS-PCB                      
227600                                       DLEV-WDB6-PCB                      
227700                                       DUMMY-PCB                          
227800     IF DLEV-FLSDCLEV-UT = JA                                             
227900        MOVE 82                  TO DLEV-KDORDBEK-UT                      
228000     END-IF                                                               
228100     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                                 
228200        MOVE JA                  TO OBKR-SW                               
228300        MOVE NEJ                 TO ALLT-SW                               
228400        MOVE ZERO                TO KVAN-KDORDBEK-UT                      
228500     ELSE                                                                 
228600        IF DLEV-KDORDBEK-UT = 95                                          
228700           MOVE JA               TO OBKR-SW                               
228800        END-IF                                                            
228900        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
229000           IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                             
229100             MOVE DLEV-IDDC-UT      TO ORAD-IDDC                          
229200                                       WS-IDDC                            
229300                                       WS-IDDC-DDGS                       
229400           END-IF                                                         
229500                                                                          
229600           MOVE AREG-ADLAGOMR       TO ORAD-ADLAGOMR                      
229700           MOVE AREG-ADGANG         TO ORAD-ADGANG                        
229800           MOVE AREG-ADPLATS        TO ORAD-ADPLATS                       
229900                                                                          
230000           PERFORM S10-HAMTA-WDB6-INFO                                    
230100                                                                          
230200           IF TILLAEGG-TPO OR DCS-DDC                                     
230300              MOVE 82               TO DLEV-KDORDBEK-UT                   
230400              MOVE NEJ              TO ALLT-SW                            
230500              MOVE JA               TO OBKR-SW                            
230600              MOVE ZERO             TO KVAN-KDORDBEK-UT                   
230700              IF DCS-DDC                                                  
230800                MOVE DLEV-IDDC-IN   TO ORAD-IDDC                          
230900                                       WS-IDDC                            
231000              END-IF                                                      
231100           ELSE                                                           
231200              IF KOLLA-ARBTAB-C1                                          
231300                 PERFORM ECIA-KOLLA-UTSKRIVEN-DLEV                        
231400              END-IF                                                      
231500           END-IF                                                         
231600        END-IF                                                            
231700        MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
231800        MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)        
231900        MOVE DLEV-KVDAGAR-DIFF-UT TO                                      
232000                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
232100        MOVE DLEV-TISKEPPN-DDC-UT TO                                      
232200                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
232300                                                                          
232400        MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                         
232500        MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                        
232600        MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                         
232700     END-IF                                                               
232800     IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                       
232900        AND ORAD-IDDC = WC-CDC-SE                                         
233000        MOVE OHUV-IDDISTR     TO TEST-IDDISTR                             
233100        IF DIST18-SKROT                                                   
233200          CONTINUE                                                        
233300        ELSE                                                              
233400          MOVE 26             TO DLEV-KDORDBEK-UT                         
233500          MOVE JA             TO OBKR-SW                                  
233600        END-IF                                                            
233700     END-IF                                                               
233800                                                                          
233900     END-IF                                                               
234000     .                                                                    
234100     EJECT                                                                
234200 ECIA-KOLLA-UTSKRIVEN-DLEV SECTION.                                       
234300                                                                          
234400     MOVE 'STA ECIA-KOLLA-UTSK'           TO   WS-PGM-POSITION            
234500     MOVE WS-IDDC              TO W-IDDC                                  
234600     MOVE DLEV-IDLEVNR-UT      TO W-IDLEVNR                               
234700     PERFORM IMS-16-GNP-WDQ2-WDQ211                                       
234800     IF SEGMENT-FINNS                                                     
234900        IF DIRL-KVRADER = +0                                              
235000           MOVE 82          TO DLEV-KDORDBEK-UT                           
235100           MOVE NEJ         TO ALLT-SW                                    
235200           MOVE JA          TO OBKR-SW                                    
235300           MOVE ZERO        TO KVAN-KDORDBEK-UT                           
235400        ELSE                                                              
235500           MOVE JA          TO RAD-GODKAND-SW                             
235600        END-IF                                                            
235700     ELSE                                                                 
235800        MOVE JA                TO RAD-GODKAND-SW                          
235900     END-IF                                                               
236000     .                                                                    
236100     EJECT                                                                
236200 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
236300                                                                          
236400     MOVE 'STA ECK-ERS        '           TO   WS-PGM-POSITION            
236500     IF ALLT-OK                                                           
236600                                                                          
236700     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
236800     MOVE ORAD-IDDC            TO KERS-IDDC                               
236900     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
237000     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
237100     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
237200     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
237300     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
237400     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
237500     MOVE AREG-KDERS           TO KERS-KDERS                              
237600     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
237700     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
237800     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
237900     MOVE AREG-KDUART          TO KERS-KDUART                             
238000     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
238100     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
238200     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
238300     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
238400     MOVE ZERO                 TO WX-KDORDBEK                             
238500                                                                          
238600     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
238700                         KERS-ARTC-PCB KERS-ERSA-PCB                      
238800                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
238900                                                                          
239000     IF KERS-KDORDBEK > ZERO   AND                                        
239100        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
239200          KERS-KDERS > 10 )                                               
239300        MOVE JA                  TO OBKR-SW                               
239400        MOVE NEJ                 TO ALLT-SW                               
239500     END-IF                                                               
239600                                                                          
239700     PERFORM S10-HAMTA-WDB6-INFO                                          
239800                                                                          
239900     IF KERS-KDORDBEK > +0                                                
240000        IF DCS-SDC                                                        
240100          IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR 19          
240200                       OR 21 OR 22 OR 24 OR 25 OR 27 OR 28 OR 29          
240300             MOVE JA            TO KOLLA-ERS-SW                           
240400          END-IF                                                          
240500        END-IF                                                            
240600        IF DCS-NDC                                                        
240700          IF AREG-KDERS > 18                                              
240800            MOVE JA                 TO KOLLA-ERS-SW                       
240900          ELSE                                                            
241000            MOVE ZERO               TO KERS-KDORDBEK                      
241100            PERFORM S02-RENSA-TILLK-TAB                                   
241200            MOVE JA                 TO ALLT-SW                            
241300          END-IF                                                          
241400        END-IF                                                            
241500     END-IF                                                               
241600     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
241700        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
241800***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
241900***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
242000***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
242100***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
242200           CONTINUE                                                       
242300        ELSE                                                              
242400           MOVE +0               TO KVAN-KDORDBEK-UT                      
242500           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
242600        END-IF                                                            
242700     END-IF                                                               
242800     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
242900        MOVE +0                  TO DLEV-KDORDBEK-UT                      
243000     END-IF                                                               
243100                                                                          
243200     ELSE                                                                 
243300        MOVE +0                  TO KERS-KDERS                            
243400     END-IF                                                               
243500     .                                                                    
243600     EJECT                                                                
243700 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
243800                                                                          
243900     MOVE 'STA ECL-SPARR      '           TO   WS-PGM-POSITION            
244000     IF ALLT-OK OR KOLLA-ERS                                              
244100                                                                          
244200     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
244300     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
244400     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
244500     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
244600     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
244700     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
244800     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
244900     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
245000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
245100     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
245200     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
245300     MOVE AREG-FLIART          TO SPAR-FLIART                             
245400     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
245500     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
245600     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
245700     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
245800     MOVE ORAD-IDDC            TO SPAR-IDDC                               
245900     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
246000     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
246100     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
246200     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
246300     MOVE +1                   TO SPAR-KDORDBEH                           
246400     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
246500     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
246600     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
246700     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
246800     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
246900     MOVE AREG-KDUART          TO SPAR-KDUART                             
247000     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
247100     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
247200     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
247300     MOVE ORAD-TITPO           TO SPAR-TITPO                              
247400     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
247500     MOVE AREG-KDERS           TO SPAR-KDERS                              
247600     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
247700                                                                          
247800     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
247900                                       SPAR-WDF8A-PCB                     
248000                                       SPAR-WDK6-PCB                      
248100                                                                          
248200     IF SPAR-KDORDBEK        > ZERO                                       
248300       MOVE JA                TO OBKR-SW                                  
248400       MOVE NEJ               TO ALLT-SW                                  
248500       IF SPAR-KDORDBEK = 51 OR 67 OR 58                                  
248600         MOVE NEJ             TO KOLLA-ERS-SW                             
248700         MOVE ZERO            TO KERS-KDORDBEK                            
248800         PERFORM S02-RENSA-TILLK-TAB                                      
248900       END-IF                                                             
248100       MOVE ZERO           TO XDCA-DAPUBL                                 
249000*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411NDCA                         
249100       IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                      
249200           MOVE 99999999      TO XDCA-DAPUBL                              
249300*          MOVE 99999999      TO NDCA-DAPUBL                              
249400       END-IF                                                             
249500                                                                          
249600       IF KVAN-KDORDBEK-UT > +0                                           
249700          MOVE +0             TO KVAN-KDORDBEK-UT                         
249800          MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                           
249900       END-IF                                                             
250000*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
250100*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
250200        IF SPAR-KDORDBEK = 54 AND NOT DCS-DDC                             
250300           MOVE JA             TO ALLT-SW                                 
250400        END-IF                                                            
250500       IF DLEV-KDORDBEK-UT > +0                                           
250600          MOVE +0             TO DLEV-KDORDBEK-UT                         
250700       END-IF                                                             
250800     END-IF                                                               
250900     IF AREG-KDSORT = 'SW'                                                
251000        MOVE 67                     TO SPAR-KDORDBEK                      
251100        MOVE JA                     TO OBKR-SW                            
251200        MOVE NEJ                    TO ALLT-SW                            
251300        IF KOLLA-ERS-SW = JA                                              
251400           MOVE NEJ             TO KOLLA-ERS-SW                           
251500           MOVE ZERO            TO KERS-KDORDBEK                          
251600           PERFORM S02-RENSA-TILLK-TAB                                    
251700        END-IF                                                            
251800     END-IF                                                               
251900                                                                          
252000                                                                          
252100     END-IF                                                               
252200     .                                                                    
252300     EJECT                                                                
252400 ECJ-KOMPLETTERA-PRIS SECTION.                                            
252500                                                                          
252600     MOVE 'STA ECJ-PRIS       '           TO   WS-PGM-POSITION            
252700     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
252800                                                                          
252900     IF DIST79-DEALER-PRICE                                               
253000       IF WS-IDPRQUES                = +0                                 
253100          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
253200          MOVE +1                    TO PRNO-KDCALL                       
253300                                                                          
253400          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
253500                                                                          
253600          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
253700                                        WS-IDPRQUES                       
253800          MOVE +1                    TO PRQU-KDCALL                       
253900       ELSE                                                               
254000          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
254100          MOVE +2                    TO PRNO-KDCALL                       
254200                                                                          
254300          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
254400                                                                          
254500          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
254600                                        WS-IDPRQUES                       
254700          MOVE +2                    TO PRQU-KDCALL                       
254800       END-IF                                                             
254900                                                                          
255000       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
255100       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
255200       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
255300       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
255400       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
255500       MOVE 'N'                      TO PRQU-KDPRSTA                      
255600       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
255700       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
255800                                                                          
255900       MOVE GMT-IDPARTNR              TO W-WDB1-IDPARTNR                  
256000       MOVE GMT-IDFTG                 TO W-WDB1-IDFTG                     
256100       PERFORM IMS-GU-WDB101                                              
256200       MOVE BET-KDVALISO              TO ORAD-KDVALISO                    
256300                                         PRQU-KDVALISO                    
256400                                                                          
256500       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
256600       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
256700       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
256800                                                                          
256900       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
257000                                          PRQU-WDC7-PCB                   
257100                                          PRQU-SJKO-WDK6-PCB              
257200                                                                          
257300       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
257400                                         WS-IDPRQUES                      
257500       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
257600                                                                          
257700       IF ORAD-PRARTNTO-LOC = +0                                          
257800          MOVE PRQU-PRARTNTO-LOCPREL TO ORAD-PRARTNTO-LOCPREL             
257900       END-IF                                                             
258000                                                                          
258100       IF ORAD-PRARTNTO-LOC NOT = +0                                      
258200         IF ORAD-KDPRTYP = SPACE                                          
258300           MOVE 'P'            TO ORAD-KDPRTYP                            
258400           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
258500         END-IF                                                           
258600       END-IF                                                             
258700                                                                          
258800     ELSE                                                                 
258900*        *NOT DIST79-DEALER-PRICE                                         
259000                                                                          
259100         PERFORM S10-HAMTA-WDB6-INFO                                      
259200                                                                          
259300         IF DCS-NDC OR DCS-DDC OR DCS-SDC OR                              
259400           (ORAD-KDTPOTYP = +0 AND AREG-KDUART = SPACE)                   
259500                                                                          
259600           IF ORAD-PRARTNTO NOT = +0                                      
259700*            *FETCH ONLY KDVALISO FROM W335PRIS                           
259800             MOVE 2                TO PRIS-KDCALL                         
259900           ELSE                                                           
260000*            *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS          
260100             MOVE 1                TO PRIS-KDCALL                         
260200           END-IF                                                         
260300         ELSE                                                             
260400             MOVE 2                TO PRIS-KDCALL                         
260500         END-IF                                                           
260600                                                                          
260700         MOVE IDPGM                TO PRIS-IDPGM                          
260800         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
260900         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
261000         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
261100         MOVE ORAD-IDDC            TO PRIS-IDDC                           
261200         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
261300         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
261400         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
261500                                                                          
261600         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
261700                             PRIS-WDK7-PCB                                
261800                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
261900                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
262000                             PRIS-COST-WDK6-PCB                           
262100                             PRIS-COST-WDK7-PCB                           
262200                             PRIS-COST-WDF1-PCB                           
262300                             PRIS-COST-9305-PCB                           
262400                             PRIS-COST-WDK72-PCB                          
262500                             PRIS-COST-WDB6-PCB                           
262600                                                                          
262700         IF PRIS-KDSVAR = '2'                                             
262800           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
262900                                TO FELTEXT                                
263000           CALL ABEND USING RKOD-ABEND                                    
263100         END-IF                                                           
263200                                                                          
263300         IF PRIS-KDCALL = 2                                               
263400           MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                           
263500           IF ORAD-KDPRTYP = SPACE                                        
263600             MOVE 'P'           TO ORAD-KDPRTYP                           
263700             MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                            
263800           END-IF                                                         
263900         ELSE                                                             
264000           MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO                           
264100           MOVE PRIS-FLPRTILL  TO ORAD-FLPRTILL                           
264200           MOVE PRIS-KDPRTYP   TO ORAD-KDPRTYP                            
264300           MOVE PRIS-PRBPRIS   TO ORAD-PRBPRIS                            
264400           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
264500           MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                           
264600           MOVE PRIS-PRAVCOST  TO ORAD-PRAVCOST                           
264700         END-IF                                                           
264800     END-IF                                                               
264900     END-IF                                                               
265000     .                                                                    
265100     EJECT                                                                
265200 ECM-KOMPLETTERA-TPO1 SECTION.                                            
265300                                                                          
265400     MOVE 'STA ECM-TPO1       '           TO   WS-PGM-POSITION            
265500                                                                          
265600     PERFORM S10-HAMTA-WDB6-INFO                                          
265700                                                                          
265800     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                    
265900                                                                          
266000     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
266100     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
266200     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
266300     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
266400     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
266500     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
266600     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
266700     MOVE OHUV-IDKST           TO TPO1-IDKST                              
266800     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
266900     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
267000     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
267100     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
267200     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
267300     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
267400     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
267500     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
267600     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
267700     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
267800     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
267900     MOVE ORAD-TITPO           TO TPO1-TITPO                              
268000     IF ORAD-KDPRTYP = 'P'                                                
268100        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
268200        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
268300        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
268400        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
268500     ELSE                                                                 
268600        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
268700        MOVE ZERO              TO TPO1-PRARTNTO                           
268800        MOVE SPACE             TO TPO1-KDPRTYP                            
268900        MOVE NEJ               TO TPO1-FLPRTILL                           
269000     END-IF                                                               
269100     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
269200     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
269300     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
269400     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
269500     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
269600     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
269700     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
269800     MOVE +1                   TO TPO1-KDORDBEH                           
269900     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
270000     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
270100     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
270200     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
270300     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
270400                                                                          
270500     MOVE +0                   TO TPO1-KDORDBEK                           
270600     MOVE SPACE                TO TPO1-FLKLAR                             
270700                                                                          
270800     MOVE OHUV-KDORDTYP-LDC    TO TPO1-KDORDTYP-LDC                       
270900     MOVE OHUV-TIREPDAT        TO TPO1-TIREPDAT                           
271000     MOVE ORAD-IDKUNDRF-WIP    TO TPO1-IDKUNDRF-WIP                       
271100                                                                          
271200     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
271300                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
271400                                                                          
271500     IF TPO1-KDORDBEK > +0                                                
271600        MOVE JA                TO OBKR-SW                                 
271700        MOVE NEJ               TO ALLT-SW                                 
271800        MOVE WC-CDC-SE         TO ORAD-IDDC                               
271900        MOVE ORAD-IDDC         TO WS-IDDC                                 
272000        IF KVAN-KDORDBEK-UT > +0                                          
272100           MOVE +0             TO KVAN-KDORDBEK-UT                        
272200           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
272300        END-IF                                                            
272400        IF DLEV-KDORDBEK-UT > +0                                          
272500           MOVE +0             TO DLEV-KDORDBEK-UT                        
272600        END-IF                                                            
272700     ELSE                                                                 
272800        IF TPO1-FLKLAR = JA                                               
272900           MOVE NEJ            TO ALLT-SW                                 
273000           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
273100              MOVE ZERO        TO KERS-KDORDBEK                           
273200              PERFORM S02-RENSA-TILLK-TAB                                 
273300              MOVE NEJ         TO TILLK-SW                                
273400           END-IF                                                         
273500        END-IF                                                            
273600     END-IF                                                               
273700                                                                          
273800     END-IF                                                               
273900     .                                                                    
274000     EJECT                                                                
274100 ECN-KOMPLETTERA-TPO2 SECTION.                                            
274200                                                                          
274300     MOVE 'STA ECN-TPO2       '           TO   WS-PGM-POSITION            
274400                                                                          
274500     PERFORM S10-HAMTA-WDB6-INFO                                          
274600                                                                          
274700     IF DCS-SDC             AND                                           
274800        ORAD-KDORDKL  = 1   AND                                           
274900        ORAD-IDKAMPRF = 0   AND                                           
275000       (AREG-KDUART   = 'L' OR  AREG-KDUART = 'P')                        
275100                                                                          
275200        CONTINUE                                                          
275300     ELSE                                                                 
275400       IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                  
275500                                                                          
275600       MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                          
275700       MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                         
275800       MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                         
275900       MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                          
276000       MOVE ORAD-BERADREF        TO TPO2-BERADREF                         
276100       MOVE AREG-IDANSK          TO TPO2-IDANSK                           
276200       MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                          
276300       MOVE OHUV-IDKST           TO TPO2-IDKST                            
276400       MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                         
276500       MOVE ORAD-KDDSP           TO TPO2-KDDSP                            
276600       MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                         
276700       MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                          
276800       MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                         
276900       MOVE ORAD-KDORDING        TO TPO2-KDORDING                         
277000       MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                          
277100       MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                         
277200       MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                         
277300       MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                        
277400       MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                         
277500       MOVE ORAD-TITPO           TO TPO2-TITPO                            
277600       MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                         
277700       MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                     
277800       MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                          
277900       MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                         
278000       MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                         
278100       MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                         
278200       MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                         
278300       MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                         
278400       MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                         
278500       MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                         
278600       MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                          
278700       MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                          
278800       MOVE AREG-KDUART          TO TPO2-KDUART                           
278900       MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                         
279000       MOVE +1                   TO TPO2-KDORDBEH                         
279100       MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                         
279200       MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                         
279300       MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                          
279400       MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                         
279500       MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                         
279600       MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                        
279700       MOVE ORAD-KVBEART         TO TPO2-KVBEART                          
279800                                                                          
279900       MOVE +0                   TO TPO2-KDORDBEK                         
280000       MOVE SPACE                TO TPO2-FLKLAR                           
280100                                                                          
280200       MOVE OHUV-KDORDTYP-LDC    TO TPO2-KDORDTYP-LDC                     
280300       MOVE OHUV-TIREPDAT        TO TPO2-TIREPDAT                         
280400       MOVE ORAD-IDKUNDRF-WIP    TO TPO2-IDKUNDRF-WIP                     
280500                                                                          
280600       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
280700                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
280800                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
280900                         TIME-4437-PCB                                    
281000                                                                          
281100       IF TPO2-KDORDBEK > +0                                              
281200          MOVE JA                TO OBKR-SW                               
281300          MOVE NEJ               TO ALLT-SW                               
281400          MOVE WC-CDC-SE         TO ORAD-IDDC                             
281500          MOVE ORAD-IDDC         TO WS-IDDC                               
281600          MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                         
281700          IF ORAD-KDTPOTYP = 6                                            
281800             IF ORAD-KDPRTYP NOT = 'P'                                    
281900                MOVE ZERO        TO ORAD-PRARTNTO                         
282000                MOVE SPACE       TO ORAD-KDPRTYP                          
282100**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
282200                IF NOT DIST79-DEALER-PRICE                                
282300                  MOVE ZERO        TO ORAD-PRARTNTO-LOC                   
282400                  MOVE NEJ         TO ORAD-FLPRTILL                       
282500                END-IF                                                    
282600*************                                                             
282700             END-IF                                                       
282800          END-IF                                                          
282900       ELSE                                                               
283000          IF TPO2-FLKLAR = JA                                             
283100             MOVE NEJ            TO ALLT-SW                               
283200             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
283300                MOVE ZERO        TO KERS-KDORDBEK                         
283400                PERFORM S02-RENSA-TILLK-TAB                               
283500                MOVE NEJ         TO TILLK-SW                              
283600             END-IF                                                       
283700          END-IF                                                          
283800       END-IF                                                             
283900                                                                          
284000       END-IF                                                             
284100     END-IF                                                               
284200     .                                                                    
284300     EJECT                                                                
284400 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
284500                                                                          
284600     MOVE 'STA ECO-KAMP       '           TO   WS-PGM-POSITION            
284700                                                                          
284800     PERFORM S10-HAMTA-WDB6-INFO                                          
284900                                                                          
285000     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                    
285100                                                                          
285200       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
285300       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
285400       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
285500       MOVE ORAD-IDDC          TO KAMP-IDDC                               
285600       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
285700       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
285800       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
285900       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
286000       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
286100       MOVE OHUV-IDKST         TO KAMP-IDKST                              
286200       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
286300       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
286400       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
286500       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
286600       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
286700       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
286800       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
286900       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
287000       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
287100       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
287200       MOVE ORAD-TITPO         TO KAMP-TITPO                              
287300       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
287400          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
287500          MOVE ORAD-DEAL-PR-LINE                                          
287600                               TO KAMP-DEAL-PR-LINE                       
287700          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
287800          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
287900       ELSE                                                               
288000          MOVE ORAD-DEAL-PR-LINE                                          
288100                               TO KAMP-DEAL-PR-LINE                       
288200          MOVE ZERO            TO KAMP-PRARTNTO                           
288300*                                 KAMP-PRARTNTO-LOC                       
288400*                                 KAMP-PRARTNTO-LOCPREL                   
288500          MOVE SPACE           TO KAMP-KDPRTYP                            
288600          MOVE NEJ             TO KAMP-FLPRTILL                           
288700       END-IF                                                             
288800       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
288900       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
289000       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
289100       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
289200       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
289300       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
289400       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
289500       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
289600       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
289700       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
289800       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
289900                                                                          
290000       MOVE +0                 TO KAMP-KDORDBEK                           
290100       MOVE SPACE              TO KAMP-FLKLAR                             
290200                                                                          
290300       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
290400                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
290500                                                                          
290600       IF KAMP-KDORDBEK > +0                                              
290700          MOVE JA              TO OBKR-SW                                 
290800          MOVE NEJ             TO ALLT-SW                                 
290900          MOVE WC-CDC-SE       TO ORAD-IDDC                               
291000          MOVE ORAD-IDDC       TO WS-IDDC                                 
291100          IF KVAN-KDORDBEK-UT > +0                                        
291200             MOVE +0           TO KVAN-KDORDBEK-UT                        
291300             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
291400          END-IF                                                          
291500          IF DLEV-KDORDBEK-UT > +0                                        
291600             MOVE +0           TO DLEV-KDORDBEK-UT                        
291700          END-IF                                                          
291800       ELSE                                                               
291900          IF KAMP-FLKLAR = JA                                             
292000             MOVE NEJ          TO ALLT-SW                                 
292100             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
292200                MOVE ZERO      TO KERS-KDORDBEK                           
292300                PERFORM S02-RENSA-TILLK-TAB                               
292400                MOVE NEJ       TO TILLK-SW                                
292500             END-IF                                                       
292600          END-IF                                                          
292700       END-IF                                                             
292800                                                                          
292900     END-IF                                                               
293000     .                                                                    
293100     EJECT                                                                
293200 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
293300                                                                          
293400     MOVE 'STA ECT-RELS     '              TO   WS-PGM-POSITION           
293500       IF ALLT-OK AND W-KDORDBEK = 56                                     
293600                                                                          
293700         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
293800         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
293900         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
294000         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
294100         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
294200         MOVE AREG-IDANSK          TO RELS-IDANSK                         
294300         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
294400         MOVE OHUV-IDKST           TO RELS-IDKST                          
294500         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
294600         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
294700         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
294800         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
294900         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
295000         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
295100         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
295200         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
295300         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
295400         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
295500         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
295600         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
295700         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
295800         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
295900         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
296000         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
296100         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
296200         EJECT                                                            
296300         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
296400         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
296500         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
296600         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
296700         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
296800         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
296900         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
297000         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
297100         MOVE AREG-KDUART          TO RELS-KDUART                         
297200         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
297300         MOVE +1                   TO RELS-KDORDBEH                       
297400         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
297500         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
297600         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
297700         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
297800         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
297900         MOVE 0                    TO RELS-KVQPACK-1                      
298000         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
298100         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
298200                                                                          
298300         MOVE SPACE                TO RELS-FLKLAR                         
298400                                                                          
298500         MOVE OHUV-KDORDTYP-LDC    TO RELS-KDORDTYP-LDC                   
298600         MOVE OHUV-TIREPDAT        TO RELS-TIREPDAT                       
298700         MOVE ORAD-IDKUNDRF-WIP    TO RELS-IDKUNDRF-WIP                   
298800                                                                          
298900                                                                          
299000         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
299100                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
299200                                                                          
299300         PERFORM ECTA-ANDRA-WDC711                                        
299400         IF RELS-KDORDBEK > +0                                            
299500            MOVE JA                TO OBKR-SW                             
299600            MOVE NEJ               TO ALLT-SW                             
299700            MOVE WC-CDC-SE         TO ORAD-IDDC                           
299800            MOVE ORAD-IDDC         TO WS-IDDC                             
299900         ELSE                                                             
300000            IF RELS-FLKLAR = JA                                           
300100               MOVE NEJ            TO ALLT-SW                             
300200               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
300300                  MOVE ZERO        TO KERS-KDORDBEK                       
300400                  PERFORM S02-RENSA-TILLK-TAB                             
300500               END-IF                                                     
300600            END-IF                                                        
300700         END-IF                                                           
300800                                                                          
300900       MOVE +0                   TO W-KDORDBEK                            
301000       MOVE SPACE                TO RELS-FLKLAR                           
301100                                                                          
301200       END-IF                                                             
301300     .                                                                    
301400     EJECT                                                                
301500 ECTA-ANDRA-WDC711 SECTION.                                               
301600                                                                          
301700     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
301800       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
301900         INITIALIZE PRQU-W335PRQU                                         
302000         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
302100         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
302200         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
302300         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
302400         MOVE 6                  TO PRQU-KDCALL                           
302500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
302600                                            PRQU-WDC7-PCB                 
302700                                            PRQU-SJKO-WDK6-PCB            
302800         MOVE 'N'                TO ORAD-FLPRTILL                         
302900       END-IF                                                             
303000     END-IF                                                               
303100     .                                                                    
303200     EJECT                                                                
303300 ECG-PREL-AVBOKNING-XDC SECTION.                                          
303400                                                                          
303500     MOVE 'STA ECG-PREL-XDC   '           TO   WS-PGM-POSITION            
303600     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
303700                                                                          
303800       PERFORM S10-HAMTA-WDB6-INFO                                        
303900                                                                          
304000       IF DCS-NDC                                                         
304100         MOVE JA TO ALLT-SW                                               
304200         PERFORM ECGX-PREL-AVBOKNING-XDC                                  
304300                                                                          
304400*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
304500*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
304600*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
304700*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
304800*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
304900*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
305000*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
305100*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
305200*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
305300*        IF DCS-CHINA                                                     
305400*          MOVE OHUV-IDDC-TVS      TO NDCA-IDDC-TVS                       
305500*        ELSE                                                             
305600*          MOVE OHUV-IDDC-CLEAR(1) TO NDCA-IDDC-TVS                       
305700*        END-IF                                                           
305800*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
305900*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
306000*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
306100*        MOVE AREG-KDERS           TO NDCA-KDERS                          
306200*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
306300*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
306400*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
306500*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
306600*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
306700*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
306800*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
306900*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
307000*        MOVE ORAD-VKART           TO NDCA-VKART                          
307100*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
307200*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
307300*        MOVE +2                   TO NDCA-KDCALL                         
307400*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
307500*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
307600*                                     NDCA-XDK7-KVOKS-DAG                 
307700*                                     NDCA-XDK7-KVOKS-BULK                
307800                                                                          
307900*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
308000*                                          NDCA-USEA-PCB                  
308100*                                          NDCA-WDK7-PCB                  
308200*                                          NDCA-WDL6-PCB                  
308300*                                          NDCA-WDB6-PCB                  
308400*                                          NDCA-XDK7-W411XDK7             
308500                                                                          
308600*        PERFORM ECGX-CHECK-DIFF                                          
308700         IF XDCA-KDORDBEK > ZERO                                          
308800           IF SPAR-FLPUBCDC = YES                                         
308900*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE            
309000*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
309100*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
309200              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
309300                 AND XDCA-DAPUBL > ZERO                                   
309400                 MOVE ZERO TO SPAR-KDORDBEK                               
309500              ELSE                                                        
309600                 MOVE ZERO TO XDCA-KDORDBEK                               
309700              END-IF                                                      
309800           END-IF                                                         
309900           IF KOLLA-ERS                                                   
310000              IF XDCA-KVPREAVB > 0                                        
310100                MOVE ZERO            TO KERS-KDORDBEK                     
310200                PERFORM S02-RENSA-TILLK-TAB                               
310300                MOVE ZERO            TO SPAR-KDORDBEK                     
310400              ELSE                                                        
310500                MOVE ZERO            TO XDCA-KDORDBEK                     
310600              END-IF                                                      
310700           ELSE                                                           
310800             IF XDCA-KDORDBEK = 15                                        
310900                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
311000                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
311100                END-IF                                                    
311200                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
311300                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
311400                END-IF                                                    
311500                IF SDCA-KDORDBEK = 15                                     
311600                   MOVE ZERO         TO SDCA-KDORDBEK                     
311700                END-IF                                                    
311800             END-IF                                                       
311900             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
312000                 MOVE ZERO           TO XDCA-KDORDBEK                     
312100             END-IF                                                       
312200           END-IF                                                         
312300           MOVE JA                   TO OBKR-SW                           
312400         ELSE                                                             
312500           IF KOLLA-ERS    OR                                             
312600             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
312700             IF XDCA-KVPREAVB > 0                                         
312800               MOVE ZERO             TO KERS-KDORDBEK                     
312900               PERFORM S02-RENSA-TILLK-TAB                                
313000               MOVE ZERO             TO SPAR-KDORDBEK                     
313100             ELSE                                                         
313200               MOVE JA               TO OBKR-SW                           
313300             END-IF                                                       
313400           ELSE                                                           
313500            IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                  
313600               MOVE ZERO             TO SPAR-KDORDBEK                     
313700            END-IF                                                        
313800           END-IF                                                         
313900           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
314000*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
314100*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
314200              MOVE 0  TO SPAR-KDORDBEK                                    
314300              MOVE JA  TO ALLT-SW                                         
314400              MOVE NEJ TO OBKR-SW                                         
314500           END-IF                                                         
314600         END-IF                                                           
314700         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
314800         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
314900         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
315000         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
315100         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
315200         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
315300         MOVE XDCA-KDOI              TO ORAD-KDOI                         
315400         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
315500         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
315600         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
315700         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
315800         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
315900         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
316000         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
316100         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
316200         MOVE NEJ                    TO ALLT-SW                           
316300       END-IF                                                             
316400                                                                          
316500     END-IF                                                               
316600     .                                                                    
316700     EJECT                                                                
316800 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
316900                                                                          
317000     MOVE 'STA ECGX-XDC      '       TO   WS-PGM-POSITION                 
317100     IF ALLT-OK OR KOLLA-ERS                                              
317200                                                                          
317300       PERFORM S10-HAMTA-WDB6-INFO                                        
317400                                                                          
317500       IF DCS-NDC                                                         
317600                                                                          
317700* XDCA-INPUT                                                              
317800         MOVE +1 TO WS-INDEX                                              
317900         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
318000           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
318100                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
318200           ADD +1 TO WS-INDEX                                             
318300         END-PERFORM                                                      
318400                                                                          
318500         IF DCS-CHINA                                                     
318600            MOVE OHUV-IDDC-TVS      TO XDCA-IDDC-TVS                      
318700         ELSE                                                             
318800            MOVE OHUV-IDDC-PRIM     TO XDCA-IDDC-TVS                      
318900         END-IF                                                           
319000         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
319100         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
319200         MOVE ORAD-IDDC            TO XDCA-IDDC                           
319300         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
319400         MOVE AREG-KDERS           TO XDCA-KDERS                          
319500         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
319600         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
319700         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
319800         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
319900         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
320000         MOVE ORAD-VKART           TO XDCA-VKART                          
320100         MOVE +1                   TO XDCA-KDCALL                         
320200                                                                          
320300* XDCA-OUTPUT                                                             
320400         MOVE SPACE                TO XDCA-IDDC-OUT                       
320500                                      XDCA-IDDC-RO                        
320600                                      XDCA-KDARTURS                       
320700                                      XDCA-KDOI                           
320800                                      XDCA-CLEARGROUP                     
320900         MOVE ZERO                 TO XDCA-ADLAGOMR                       
321000                                      XDCA-ADGANG                         
321100                                      XDCA-ADPLATS                        
321200                                      XDCA-KDORDBEK                       
321300                                      XDCA-KVPREAVB                       
321400                                      XDCA-KVPRERO                        
321500                                      XDCA-TIREGDAT-OUT                   
321600                                      XDCA-TIREGTID-OUT                   
321700                                      XDCA-VKART-OUT                      
321800                                      XDCA-VKART-NTO                      
321900                                      XDCA-VLARTNTO                       
322000         MOVE ZERO                 TO                                     
322100                                      XDCA-KVOKS-DAG                      
322200                                      XDCA-KVOKS-BULK                     
322300                                                                          
322400         IF XDCA-DAPUBL NOT = 99999999                                    
322500            MOVE ZERO              TO XDCA-DAPUBL                         
322600         END-IF                                                           
322700                                                                          
322800         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
322900         XDCA-USEA-PCB                                                    
323000         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
323100         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
323200         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
323300         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
323400         XDCA-WDK7-3-PCB                                                  
323500                                                                          
323600* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
323700* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
323800         IF XDCA-KDARTURS = SPACE                                         
323900           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
324000         END-IF                                                           
324100         IF XDCA-VKART-NTO = ZERO                                         
324200           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
324300         END-IF                                                           
324400         IF XDCA-VLARTNTO = ZERO                                          
324500           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
324600         END-IF                                                           
324700       END-IF                                                             
324800     END-IF                                                               
324900     .                                                                    
325000     EJECT                                                                
325100 ECGX-CHECK-DIFF SECTION.                                                 
325200                                                                          
325300     MOVE 'CHECK-DIFF       '              TO   WS-PGM-POSITION           
325400     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
325500     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
325600     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
325700     AND NDCA-ADGANG     = XDCA-ADGANG                                    
325800     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
325900     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
326000     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
326100     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
326200     AND NDCA-KDOI       = XDCA-KDOI                                      
326300     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
326400     AND NDCA-VKART      = XDCA-VKART-OUT                                 
326500     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
326600     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
326700     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
326800     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
326900     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
327000     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
327100         MOVE NEJ TO DIFF-FLSVAR                                          
327200     ELSE                                                                 
327300        MOVE JA           TO DIFF-FLSVAR                                  
327400     END-IF                                                               
327500                                                                          
327600* ORDER LOG INFO                                                          
327700     IF DIFF-FLSVAR = JA                                                  
327800       MOVE IDPGM         TO FIL-IDPGM                                    
327900       ACCEPT FIL-TIREGDAT FROM DATE                                      
328000       ACCEPT FIL-TIKLOCK FROM TIME                                       
328100       MOVE 1             TO FIL-IDSEKVNR                                 
328200       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
328300       MOVE 'A'           TO FIL-CT-IDVTYP                                
328400       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
328500                                                                          
328600*   ORDER LINE INFO                                                       
328700       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
328800       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
328900       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
329000       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
329100       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
329200       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
329300       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
329400       MOVE '4202'         TO DIFF-IDSYSTEM                               
329500                                                                          
329600*   NDCA INFO                                                             
329700       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
329800       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
329900       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
330000       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
330100       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
330200       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
330300       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
330400       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
330500       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
330600       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
330700       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
330800       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
330900       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
331000       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
331100       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
331200       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
331300                                                                          
331400*   XDCA INFO                                                             
331500       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
331600       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
331700       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
331800       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
331900       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
332000       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
332100       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
332200       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
332300       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
332400       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
332500       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
332600       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
332700       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
332800       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
332900       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
333000       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
333100                                                                          
333200       PERFORM IMS-ISRT-WDR601                                            
333300       IF SEGMENT-FINNS-REDAN                                             
333400          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
333500             ADD 1 TO FIL-IDSEKVNR                                        
333600             PERFORM IMS-ISRT-WDR601                                      
333700          END-PERFORM                                                     
333800       END-IF                                                             
333900     END-IF                                                               
334000     .                                                                    
334100     EJECT                                                                
334200*ECX-PREL-AVBOKNING-SDC  SECTION.                                         
334300*                                                                         
334400*    MOVE 'STA ECX-PREL-SDC   ' TO   WS-PGM-POSITION                      
334500*    IF ALLT-OK OR KOLLA-ERS                                              
334600*                                                                         
334700*      MOVE +1 TO SDCA-IXDCCLEAR                                          
334800*      PERFORM UNTIL SDCA-IXDCCLEAR > 3                                   
334900*                                                                         
335000*        IF WS-IDDC NOT = W-IDDC-B6                                       
335100*          MOVE WS-IDDC TO W-IDDC-B6                                      
335200*          PERFORM IMS-GU-WDB601                                          
335300*        END-IF                                                           
335400*                                                                         
335500*        IF DCS-SDC                                                       
335600*                                                                         
335700*          PERFORM ECXA-CALL-W411SDCA                                     
335800*                                                                         
335900*          IF SDCA-KDORDBEK > ZERO                                        
336000*             PERFORM ECXB-KOLLA-CLEARING                                 
336100*                                                                         
336200*          ELSE                                                           
336300*            MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                      
336400*            MOVE SDCA-ADGANG       TO ORAD-ADGANG                        
336500*            MOVE SDCA-ADPLATS      TO ORAD-ADPLATS                       
336600*            MOVE SDCA-KVBEART-Q    TO ORAD-KVPREAVB                      
336700*            MOVE NEJ               TO ALLT-SW                            
336800*            IF KVAN-KDORDBEK-UT = ZERO                                   
336900*              MOVE JA             TO EGET-CL-RAD-SW                      
337000*            END-IF                                                       
337100*            IF KOLLA-ERS OR                                              
337200*             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)              
337300*              MOVE NEJ            TO KOLLA-ERS-SW                        
337400*              MOVE ZERO           TO KERS-KDORDBEK                       
337500*              PERFORM S02-RENSA-TILLK-TAB                                
337600*              MOVE ZERO           TO SPAR-KDORDBEK                       
337700*            END-IF                                                       
337800*          END-IF                                                         
337900*          MOVE SDCA-KDOI            TO ORAD-KDOI                         
338000*          MOVE SDCA-CLEARGROUP      TO ORAD-CLEARGROUP                   
338100*        END-IF                                                           
338200*                                                                         
338300*        ADD +1 TO SDCA-IXDCCLEAR                                         
338400*      END-PERFORM                                                        
338500*    END-IF                                                               
338600*    .                                                                    
338700*    EJECT                                                                
338800*ECXA-CALL-W411SDCA      SECTION.                                         
338900*    MOVE 'ECXA-CALL-W411SDCA '       TO WS-PGM-POSITION                  
339000*                                                                         
339100*    MOVE OHUV-IDDISTR    TO SDCA-IDDISTR                                 
339200*    MOVE OHUV-FLFORBI    TO SDCA-FLFORBI                                 
339300*    MOVE AREG-FLREFILL   TO SDCA-FLREFILL                                
339400*    MOVE NEJ             TO SDCA-FLORDSPE                                
339500*    MOVE ORAD-IDARTNR    TO SDCA-IDARTNR                                 
339600*    MOVE ORAD-IDDC       TO SDCA-IDDC                                    
339700*    MOVE OHUV-IDDC-TVS   TO SDCA-IDDC-TVS                                
339800*    MOVE ORAD-IDLEVNR    TO SDCA-IDLEVNR                                 
339900*    MOVE ORAD-IDSYSTEM   TO SDCA-IDSYSTEM                                
340000*    MOVE ORAD-KDORDKL    TO SDCA-KDORDKL                                 
340100*    MOVE ORAD-KDORDING   TO SDCA-KDORDING                                
340200*    MOVE AREG-KDPRODSL   TO SDCA-KDPRODSL                                
340300*    MOVE ORAD-KVBEART-Q  TO SDCA-KVBEART-Q                               
340400*    MOVE AREG-REDIRLEV   TO SDCA-REDIRLEV                                
340500*    MOVE ORAD-FLSDCLEV   TO SDCA-FLSDCLEV                                
340600*    MOVE +1              TO SDCA-KDCALL                                  
340700*                                                                         
340800*    CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
340900*                                      SDCA-WDB6-PCB                      
341000*    .                                                                    
341100*    EJECT                                                                
341200*ECXB-KOLLA-CLEARING     SECTION.                                         
341300*    MOVE 'ECXB-KOLLA-CLEARING'       TO WS-PGM-POSITION                  
341400*                                                                         
341500*    IF SDCA-IXDCCLEAR = 1                                                
341600*       MOVE SDCA-KDORDBEK    TO SDCA-KDORDBEK-FIRST-SDC                  
341700*       MOVE ZERO             TO SDCA-KDORDBEK                            
341800*    ELSE                                                                 
341900*       IF SDCA-IXDCCLEAR = 2                                             
342000*          MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                 
342100*          MOVE ZERO       TO SDCA-KDORDBEK                               
342200*       END-IF                                                            
342300*    END-IF                                                               
342400*    IF SDCA-FLSDCLEV = JA AND                                            
342500*     ((SDCA-IXDCCLEAR = 1 AND                                            
342600*       SDCA-KDORDBEK-FIRST-SDC = 80)                                     
342700*    OR                                                                   
342800*      (SDCA-IXDCCLEAR = 2 AND                                            
342900*       SDCA-KDORDBEK-SECOND-SDC = 80)                                    
343000*    OR                                                                   
343100*      (SDCA-IXDCCLEAR = 3 AND                                            
343200*       SDCA-KDORDBEK = 80))                                              
343300*                                                                         
343400*       IF SDCA-IXDCCLEAR = 1                                             
343500*          IF OHUV-IDDC-CLEAR(2) > '19'                                   
343600*             MOVE ZERO  TO SDCA-KDORDBEK-FIRST-SDC                       
343700*             MOVE OHUV-IDDC-CLEAR(2)                                     
343800*                             TO ORAD-IDDC                                
343900*             MOVE ORAD-IDDC  TO WS-IDDC                                  
344000*             IF SDCA-KDOI = 'XX'                                         
344100*                MOVE JA TO  SDCA-FLCLEAR(SDCA-IXDCCLEAR)                 
344200*             END-IF                                                      
344300*          ELSE                                                           
344400*             MOVE JA    TO OBKR-SW                                       
344500*             MOVE NEJ   TO ALLT-SW                                       
344600*          END-IF                                                         
344700*       ELSE                                                              
344800*          IF SDCA-IXDCCLEAR = 2                                          
344900*             IF OHUV-IDDC-CLEAR(3) > '19'                                
345000*                MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC                    
345100*                MOVE OHUV-IDDC-CLEAR(3)                                  
345200*                            TO ORAD-IDDC                                 
345300*                MOVE ORAD-IDDC                                           
345400*                            TO WS-IDDC                                   
345500*                IF SDCA-KDOI = 'XX'                                      
345600*                   MOVE JA TO     SDCA-FLCLEAR(SDCA-IXDCCLEAR)           
345700*                END-IF                                                   
345800*             ELSE                                                        
345900*                MOVE JA    TO OBKR-SW                                    
346000*                MOVE NEJ   TO ALLT-SW                                    
346100*             END-IF                                                      
346200*          ELSE                                                           
346300*             MOVE JA       TO OBKR-SW                                    
346400*             MOVE NEJ      TO ALLT-SW                                    
346500*          END-IF                                                         
346600*       END-IF                                                            
346700*    ELSE                                                                 
346800*       IF ORAD-KDORDKL > 0           AND                                 
346900*          ORAD-IDSYSTEM NOT = 'OREL' AND                                 
347000*         (AREG-KDUART = 'L' OR                                           
347100*          AREG-KDUART = 'P')         AND                                 
347200*          OHUV-FLORDSPE NOT = JA     AND                                 
347300*          OHUV-FLOVRLEV NOT = JA     AND                                 
347400*          OHUV-FLFORBI = NEJ                                             
347500*                                                                         
347600*          IF SDCA-IXDCCLEAR = 1                                          
347700*             MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                     
347800*          ELSE                                                           
347900*             IF SDCA-IXDCCLEAR = 2                                       
348000*                MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC                    
348100*             ELSE                                                        
348200*                MOVE ZERO TO SDCA-KDORDBEK                               
348300*             END-IF                                                      
348400*          END-IF                                                         
348500*          MOVE 70         TO TPO2-KDORDBEK                               
348600*          MOVE JA         TO OBKR-SW                                     
348700*          MOVE NEJ        TO ALLT-SW                                     
348800*          MOVE WC-CDC-SE  TO ORAD-IDDC                                   
348900*          MOVE ORAD-IDDC  TO WS-IDDC                                     
349000*          MOVE 6          TO ORAD-KDTPOTYP                               
349100*          IF ORAD-KDPRTYP NOT = 'P'                                      
349200*             MOVE ZERO    TO ORAD-PRARTNTO                               
349300*             MOVE SPACE   TO ORAD-KDPRTYP                                
349400**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
349500*             IF NOT DIST79-DEALER-PRICE                                  
349600*                MOVE ZERO TO ORAD-PRARTNTO-LOC                           
349700*                MOVE NEJ  TO ORAD-FLPRTILL                               
349800*             END-IF                                                      
349900*************                                                             
350000*          END-IF                                                         
350100*       ELSE                                                              
350200*          PERFORM ECXBA-KOLLA-ERS                                        
350300*       END-IF                                                            
350400*    END-IF                                                               
350500*    .                                                                    
350600*    EJECT                                                                
350700*ECXBA-KOLLA-ERS          SECTION.                                        
350800*    MOVE 'STA ECXBA-KOLLA-ERS  '       TO WS-PGM-POSITION                
350900*                                                                         
351000*    IF KOLLA-ERS                                                         
351100*       IF SDCA-IXDCCLEAR = 1                                             
351200*          MOVE ZERO               TO SDCA-KDORDBEK-FIRST-SDC             
351300*       ELSE                                                              
351400*          IF SDCA-IXDCCLEAR = 2                                          
351500*             MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC            
351600*          ELSE                                                           
351700*             MOVE NEJ             TO KOLLA-ERS-SW                        
351800*             MOVE ZERO            TO SDCA-KDORDBEK                       
351900*          END-IF                                                         
352000*       END-IF                                                            
352100*       IF SPAR-KDORDBEK = ZERO                                           
352200*          IF SDCA-IXDCCLEAR = 1                                          
352300*             MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                        
352400*          ELSE                                                           
352500*             IF SDCA-IXDCCLEAR = 2                                       
352600*                MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                     
352700*             ELSE                                                        
352800*                MOVE WC-CDC-SE          TO ORAD-IDDC                     
352900*             END-IF                                                      
353000*          END-IF                                                         
353100*          MOVE ORAD-IDDC          TO WS-IDDC                             
353200*       ELSE                                                              
353300*          MOVE NEJ                TO KOLLA-ERS-SW                        
353400*       END-IF                                                            
353500*    ELSE                                                                 
353600*       MOVE JA                 TO OBKR-SW                                
353700*       IF SDCA-IXDCCLEAR = 1 AND                                         
353800*          SDCA-KDORDBEK-FIRST-SDC = 15                                   
353900*          MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                           
354000*          MOVE ORAD-IDDC          TO WS-IDDC                             
354100*       ELSE                                                              
354200*          IF SDCA-IXDCCLEAR = 2 AND                                      
354300*             SDCA-KDORDBEK-SECOND-SDC = 15                               
354400*             MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                        
354500*             MOVE ORAD-IDDC          TO WS-IDDC                          
354600*          ELSE                                                           
354700*             IF SDCA-IXDCCLEAR = 3 AND                                   
354800*                SDCA-KDORDBEK = 15                                       
354900*                MOVE WC-CDC-SE          TO ORAD-IDDC                     
355000*                MOVE ORAD-IDDC          TO WS-IDDC                       
355100*                IF SDCA-KDORDBEK-FIRST-SDC = 15                          
355200*                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC          
355300*                END-IF                                                   
355400*                IF SDCA-KDORDBEK-SECOND-SDC = 15                         
355500*                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC         
355600*                END-IF                                                   
355700*             ELSE                                                        
355800*                MOVE NEJ             TO ALLT-SW                          
355900*             END-IF                                                      
356000*          END-IF                                                         
356100*       END-IF                                                            
356200*    END-IF                                                               
356300*    .                                                                    
356400*    EJECT                                                                
356500 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
356600                                                                          
356700     MOVE 'STA ECW-PREL-SDC1  '           TO   WS-PGM-POSITION            
356800                                                                          
356900                                                                          
357000     IF ALLT-OK OR KOLLA-ERS                                              
357100                                                                          
357200       PERFORM S10-HAMTA-WDB6-INFO                                        
357300                                                                          
357400                                                                          
357500       IF DCS-SDC                                                         
357600                                                                          
357700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
357800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
357900         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
358000         MOVE NEJ                  TO SDCA-FLORDSPE                       
358100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
358200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
358300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
358400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
358500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
358600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
358700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
358800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
358900         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
359000         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
359100         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
359200         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
359300         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
359400         MOVE +0                   TO SDCA-TIREPDAT                       
359500         MOVE +0                   TO SDCA-KVOKS-PREL                     
359600         MOVE +1                   TO SDCA-KDCALL                         
359700         MOVE +1                   TO SDCA-IXDCCLEAR                      
359800                                                                          
359900         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
360000                                           SDCA-WDB6-PCB                  
360100                                           SDCA-WDK9-PCB                  
360200                                           SDCA-WDR6-PCB                  
360300                                           SDCA-WDK6-PCB                  
360400                                           SDCA-WDQ4B-PCB                 
360500                                           SDCA-WDQ2-PCB                  
360600                                           SDCA-WDQ4-PCB                  
360700                                           SDCA-WDB6-2-PCB                
360800                                           SDCA-WDK6-2-PCB                
360900                                           SDCA-WDK7-2-PCB                
361000                                           SDCA-WDK7-3-PCB                
361100                                                                          
361200         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
361300         MOVE ZERO          TO SDCA-KDORDBEK                              
361400                                                                          
361500         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
361600           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
361700             IF W-GMT-IDDC-CLEAR(2) > '19'                                
361800               MOVE ZERO                TO SDCA-KDORDBEK-FIRST-SDC        
361900               MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                      
362000               MOVE ORAD-IDDC           TO WS-IDDC                        
362100               IF SDCA-KDOI = 'XX' OR 'CN'                                
362200                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
362300               END-IF                                                     
362400             ELSE                                                         
362500               MOVE JA        TO OBKR-SW                                  
362600               MOVE NEJ       TO ALLT-SW                                  
362700             END-IF                                                       
362800           ELSE                                                           
362900             IF ORAD-KDORDKL > 0                                          
363000             AND ORAD-IDSYSTEM NOT = 'OREL'                               
363100             AND (AREG-KDUART = 'L'                                       
363200             OR AREG-KDUART = 'P')                                        
363300             AND OHUV-FLORDSPE NOT = JA                                   
363400             AND OHUV-FLOVRLEV NOT = JA                                   
363500             AND OHUV-FLFORBI = NEJ                                       
363600                MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                   
363700                MOVE 70      TO TPO2-KDORDBEK                             
363800                MOVE JA      TO OBKR-SW                                   
363900                MOVE NEJ     TO ALLT-SW                                   
364000                MOVE WC-CDC-SE TO ORAD-IDDC                               
364100                MOVE ORAD-IDDC TO WS-IDDC                                 
364200                MOVE 6       TO ORAD-KDTPOTYP                             
364300                IF ORAD-KDPRTYP NOT = 'P'                                 
364400                   MOVE ZERO  TO ORAD-PRARTNTO                            
364500                   MOVE SPACE TO ORAD-KDPRTYP                             
364600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
364700                   IF NOT DIST79-DEALER-PRICE                             
364800                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
364900                     MOVE NEJ         TO ORAD-FLPRTILL                    
365000                   END-IF                                                 
365100*************                                                             
365200                END-IF                                                    
365300             ELSE                                                         
365400               IF KOLLA-ERS                                               
365500                  MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
365600                  IF SPAR-KDORDBEK      = ZERO                            
365700                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
365800                    MOVE ORAD-IDDC     TO WS-IDDC                         
365900                  ELSE                                                    
366000                    MOVE NEJ           TO KOLLA-ERS-SW                    
366100                  END-IF                                                  
366200               ELSE                                                       
366300                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
366400                    MOVE JA            TO OBKR-SW                         
366500                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
366600                    MOVE ORAD-IDDC     TO WS-IDDC                         
366700                 ELSE                                                     
366800                    MOVE JA            TO OBKR-SW                         
366900                    MOVE NEJ           TO ALLT-SW                         
367000                 END-IF                                                   
367100               END-IF                                                     
367200             END-IF                                                       
367300           END-IF                                                         
367400         ELSE                                                             
367500           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
367600           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
367700           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
367800           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
367900           MOVE NEJ                  TO ALLT-SW                           
368000           IF KVAN-KDORDBEK-UT = ZERO                                     
368100             MOVE JA                 TO EGET-CL-RAD-SW                    
368200           END-IF                                                         
368300           IF KOLLA-ERS OR                                                
368400             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
368500              MOVE NEJ               TO KOLLA-ERS-SW                      
368600              MOVE ZERO              TO KERS-KDORDBEK                     
368700              PERFORM S02-RENSA-TILLK-TAB                                 
368800              MOVE ZERO              TO SPAR-KDORDBEK                     
368900           END-IF                                                         
369000         END-IF                                                           
369100         MOVE SDCA-KDOI              TO ORAD-KDOI                         
369200         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
369300       END-IF                                                             
369400                                                                          
369500     END-IF                                                               
369600     .                                                                    
369700     EJECT                                                                
369800 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
369900                                                                          
370000     MOVE 'STA ECH-PREL-SDC2  '           TO   WS-PGM-POSITION            
370100     IF ALLT-OK OR KOLLA-ERS                                              
370200                                                                          
370300       PERFORM S10-HAMTA-WDB6-INFO                                        
370400                                                                          
370500       IF DCS-SDC                                                         
370600                                                                          
370700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
370800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
370900         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
371000         MOVE NEJ                  TO SDCA-FLORDSPE                       
371100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
371200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
371300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
371400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
371500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
371600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
371700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
371800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
371900         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
372000         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
372100         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
372200         MOVE +1                   TO SDCA-KDCALL                         
372300         MOVE +2                   TO SDCA-IXDCCLEAR                      
372400                                                                          
372500         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
372600                                           SDCA-WDB6-PCB                  
372700                                           SDCA-WDK9-PCB                  
372800                                           SDCA-WDR6-PCB                  
372900                                           SDCA-WDK6-PCB                  
373000                                           SDCA-WDQ4B-PCB                 
373100                                           SDCA-WDQ2-PCB                  
373200                                           SDCA-WDQ4-PCB                  
373300                                           SDCA-WDB6-2-PCB                
373400                                           SDCA-WDK6-2-PCB                
373500                                           SDCA-WDK7-2-PCB                
373600                                           SDCA-WDK7-3-PCB                
373700                                                                          
373800         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
373900         MOVE ZERO          TO SDCA-KDORDBEK                              
374000                                                                          
374100         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
374200           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
374300*            IF OHUV-IDDC-CLEAR(3) > '19'                                 
374400*              MOVE ZERO               TO SDCA-KDORDBEK-SECOND-SDC        
374500*              MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                       
374600*              MOVE ORAD-IDDC          TO WS-IDDC                         
374700*              IF SDCA-KDOI = 'XX'                                        
374800*                 MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
374900*              END-IF                                                     
375000*            ELSE                                                         
375100               MOVE JA        TO OBKR-SW                                  
375200               MOVE NEJ       TO ALLT-SW                                  
375300*            END-IF                                                       
375400           ELSE                                                           
375500             IF ORAD-KDORDKL > 0                                          
375600             AND ORAD-IDSYSTEM NOT = 'OREL'                               
375700             AND (AREG-KDUART = 'L'                                       
375800             OR AREG-KDUART = 'P')                                        
375900             AND OHUV-FLORDSPE NOT = JA                                   
376000             AND OHUV-FLOVRLEV NOT = JA                                   
376100             AND OHUV-FLFORBI = NEJ                                       
376200                MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                  
376300                MOVE 70      TO TPO2-KDORDBEK                             
376400                MOVE JA      TO OBKR-SW                                   
376500                MOVE NEJ     TO ALLT-SW                                   
376600                MOVE WC-CDC-SE TO ORAD-IDDC                               
376700                MOVE ORAD-IDDC TO WS-IDDC                                 
376800                MOVE 6       TO ORAD-KDTPOTYP                             
376900                IF ORAD-KDPRTYP NOT = 'P'                                 
377000                   MOVE ZERO  TO ORAD-PRARTNTO                            
377100                   MOVE SPACE TO ORAD-KDPRTYP                             
377200**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
377300                   IF NOT DIST79-DEALER-PRICE                             
377400                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
377500                     MOVE NEJ         TO ORAD-FLPRTILL                    
377600                   END-IF                                                 
377700*************                                                             
377800                END-IF                                                    
377900             ELSE                                                         
378000               IF KOLLA-ERS                                               
378100                  MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
378200                  IF SPAR-KDORDBEK      = ZERO                            
378300                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
378400                    MOVE ORAD-IDDC     TO WS-IDDC                         
378500                  ELSE                                                    
378600                    MOVE NEJ           TO KOLLA-ERS-SW                    
378700                  END-IF                                                  
378800               ELSE                                                       
378900                 IF SDCA-KDORDBEK-SECOND-SDC = 15                         
379000                    IF SDCA-KDORDBEK-FIRST-SDC = 15                       
379100                       MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC               
379200                    END-IF                                                
379300                    MOVE JA            TO OBKR-SW                         
379400                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
379500                    MOVE ORAD-IDDC     TO WS-IDDC                         
379600                 ELSE                                                     
379700                    MOVE JA            TO OBKR-SW                         
379800                    MOVE NEJ           TO ALLT-SW                         
379900                 END-IF                                                   
380000               END-IF                                                     
380100             END-IF                                                       
380200           END-IF                                                         
380300         ELSE                                                             
380400           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
380500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
380600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
380700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
380800           MOVE NEJ                  TO ALLT-SW                           
380900           IF KVAN-KDORDBEK-UT = ZERO                                     
381000           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
381100             MOVE JA                 TO EGET-CL-RAD-SW                    
381200           END-IF                                                         
381300           IF KOLLA-ERS OR                                                
381400             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
381500              MOVE NEJ               TO KOLLA-ERS-SW                      
381600              MOVE ZERO              TO KERS-KDORDBEK                     
381700              PERFORM S02-RENSA-TILLK-TAB                                 
381800              MOVE ZERO              TO SPAR-KDORDBEK                     
381900           END-IF                                                         
382000         END-IF                                                           
382100         MOVE SDCA-KDOI              TO ORAD-KDOI                         
382200         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
382300       END-IF                                                             
382400                                                                          
382500     END-IF                                                               
382600     .                                                                    
382700     EJECT                                                                
382800 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
382900                                                                          
383000     MOVE 'STA ECX-PREL-SDC3  '           TO   WS-PGM-POSITION            
383100     IF ALLT-OK OR KOLLA-ERS                                              
383200                                                                          
383300       PERFORM S10-HAMTA-WDB6-INFO                                        
383400                                                                          
383500       IF DCS-SDC                                                         
383600                                                                          
383700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
383800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
383900         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
384000         MOVE NEJ                  TO SDCA-FLORDSPE                       
384100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
384200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
384300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
384400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
384500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
384600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
384700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
384800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
384900         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
385000         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
385100         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
385200         MOVE +1                   TO SDCA-KDCALL                         
385300         MOVE +3                   TO SDCA-IXDCCLEAR                      
385400                                                                          
385500         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
385600                                           SDCA-WDB6-PCB                  
385700                                           SDCA-WDK9-PCB                  
385800                                           SDCA-WDR6-PCB                  
385900                                           SDCA-WDK6-PCB                  
386000                                           SDCA-WDQ4B-PCB                 
386100                                           SDCA-WDQ2-PCB                  
386200                                           SDCA-WDQ4-PCB                  
386300                                           SDCA-WDB6-2-PCB                
386400                                           SDCA-WDK6-2-PCB                
386500                                           SDCA-WDK7-2-PCB                
386600                                           SDCA-WDK7-3-PCB                
386700                                                                          
386800         IF SDCA-KDORDBEK > ZERO                                          
386900           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
387000              MOVE JA        TO OBKR-SW                                   
387100              MOVE NEJ       TO ALLT-SW                                   
387200           ELSE                                                           
387300             IF ORAD-KDORDKL > 0                                          
387400             AND ORAD-IDSYSTEM NOT = 'OREL'                               
387500             AND (AREG-KDUART = 'L'                                       
387600             OR AREG-KDUART = 'P')                                        
387700             AND OHUV-FLORDSPE NOT = JA                                   
387800             AND OHUV-FLOVRLEV NOT = JA                                   
387900             AND OHUV-FLFORBI = NEJ                                       
388000                MOVE ZERO    TO SDCA-KDORDBEK                             
388100                MOVE 70      TO TPO2-KDORDBEK                             
388200                MOVE JA      TO OBKR-SW                                   
388300                MOVE NEJ     TO ALLT-SW                                   
388400                MOVE WC-CDC-SE TO ORAD-IDDC                               
388500                MOVE ORAD-IDDC TO WS-IDDC                                 
388600                MOVE 6       TO ORAD-KDTPOTYP                             
388700                IF ORAD-KDPRTYP NOT = 'P'                                 
388800                   MOVE ZERO  TO ORAD-PRARTNTO                            
388900                   MOVE SPACE TO ORAD-KDPRTYP                             
389000**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
389100                   IF NOT DIST79-DEALER-PRICE                             
389200                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
389300                     MOVE NEJ         TO ORAD-FLPRTILL                    
389400                   END-IF                                                 
389500*************                                                             
389600                END-IF                                                    
389700             ELSE                                                         
389800               IF KOLLA-ERS                                               
389900                  MOVE NEJ             TO KOLLA-ERS-SW                    
390000                  MOVE ZERO            TO SDCA-KDORDBEK                   
390100                  IF SPAR-KDORDBEK      = ZERO                            
390200                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
390300                    MOVE ORAD-IDDC     TO WS-IDDC                         
390400                  END-IF                                                  
390500               ELSE                                                       
390600                 IF SDCA-KDORDBEK = 15                                    
390700                    IF SDCA-KDORDBEK-SECOND-SDC = 15                      
390800                       MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC              
390900                    END-IF                                                
391000                    MOVE JA            TO OBKR-SW                         
391100                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
391200                    MOVE ORAD-IDDC     TO WS-IDDC                         
391300                 ELSE                                                     
391400                    MOVE JA            TO OBKR-SW                         
391500                    MOVE NEJ           TO ALLT-SW                         
391600                 END-IF                                                   
391700               END-IF                                                     
391800             END-IF                                                       
391900           END-IF                                                         
392000         ELSE                                                             
392100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
392200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
392300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
392400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
392500           MOVE NEJ                  TO ALLT-SW                           
392600           IF KVAN-KDORDBEK-UT   = ZERO                                   
392700           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
392800           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
392900             MOVE JA                 TO EGET-CL-RAD-SW                    
393000           END-IF                                                         
393100           IF KOLLA-ERS OR                                                
393200             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
393300              MOVE NEJ               TO KOLLA-ERS-SW                      
393400              MOVE ZERO              TO KERS-KDORDBEK                     
393500              PERFORM S02-RENSA-TILLK-TAB                                 
393600              MOVE ZERO              TO SPAR-KDORDBEK                     
393700           END-IF                                                         
393800         END-IF                                                           
393900                                                                          
394000         MOVE SDCA-KDOI              TO ORAD-KDOI                         
394100         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
394200       END-IF                                                             
394300                                                                          
394400     END-IF                                                               
394500     .                                                                    
394600     EJECT                                                                
394700 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
394800                                                                          
394900     MOVE 'STA ECP-RANSON     '           TO   WS-PGM-POSITION            
395000                                                                          
395100     IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                    
395200                                                                          
395300       MOVE ORAD-BERADREF      TO RANS-BERADREF                           
395400       MOVE OHUV-FLEMBORD      TO RANS-FLEMBORD                           
395500       MOVE OHUV-FLFORBI       TO RANS-FLFORBI                            
395600       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
395700       MOVE OHUV-FLOVRLEV      TO RANS-FLOVRLEV                           
395800       MOVE ORAD-IDKAMPRF      TO RANS-IDKAMPRF                           
395900       MOVE ORAD-IDARTNR       TO RANS-IDARTNR                            
396000       MOVE ORAD-IDLEVNR       TO RANS-IDLEVNR                            
396100       MOVE OHUV-IDRFTAB       TO RANS-IDRFTAB                            
396200       MOVE ORAD-TIRODAT       TO RANS-TIRODAT                            
396300       MOVE OHUV-KDORDKL       TO RANS-KDORDKL                            
396400       MOVE +1                 TO RANS-KDORDBEH                           
396500       MOVE ORAD-KVBEART-Q     TO RANS-KVBEART-Q                          
396600       MOVE ORAD-KDTPOTYP      TO RANS-KDTPOTYP                           
396700       MOVE AREG-KDERS         TO RANS-KDERS                              
396800       MOVE AREG-KVLS          TO RANS-KVLS                               
396900       MOVE AREG-KVPB-SATS     TO RANS-KVPB-SATS                          
397000       MOVE AREG-KVPB-SEP      TO RANS-KVPB-SEP                           
397100       MOVE AREG-REDIRLEV      TO RANS-REDIRLEV                           
397200       MOVE AREG-KVRESS        TO RANS-KVRESS                             
397300       MOVE AREG-KVSPANT       TO RANS-KVSPANT                            
397400       MOVE AREG-KVUTRS        TO RANS-KVUTRS                             
397500       MOVE AREG-TIDISPIN      TO RANS-TIDISPIN                           
397600                                                                          
397700       MOVE AREG-KDPRODSL      TO TEST-KDPRODSL                           
397800       IF KDPRODSL-BIMA                                                   
397900         MOVE 1                TO ORAD-RERF-RAD                           
398000                                  RANS-RERF-RAD-UT                        
398100         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
398200                                  RANS-SUTPO-EJPB-UT                      
398300                                  RANS-RERF-ART-UT                        
398400       ELSE                                                               
398500         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
398600                             RANS-ARTM-PCB RANS-ARTS-PCB                  
398700                                                                          
398800         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
398900       END-IF                                                             
399000                                                                          
399100     END-IF                                                               
399200     .                                                                    
399300     EJECT                                                                
399400 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
399500                                                                          
399600     MOVE 'STA ECQ-STOR       '           TO   WS-PGM-POSITION            
399700     IF ALLT-OK                                                           
399800                                                                          
399900     MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                           
400000     MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                            
400100     MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                        
400200     MOVE ORAD-BERADREF        TO STOR-BERADREF                           
400300     MOVE OHUV-FLFORBI         TO STOR-FLFORBI                            
400400     MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                           
400500     MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                           
400600     MOVE OHUV-KDORDKL         TO STOR-KDORDKL                            
400700     MOVE AREG-KDERS           TO STOR-KDERS                              
400800     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
400900     MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                          
401000     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
401100     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
401200     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
401300     MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                           
401400     MOVE ORAD-IDDISTR         TO STOR-IDDISTR                            
401500     MOVE SPACE                TO STOR-KDPROTYP                           
401600     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
401700                                                                          
401800     CALL W411STOR USING STOR-W411STOR                                    
401900                                                                          
402000     IF STOR-KDORDBEK > +0                                                
402100        MOVE +6                   TO ORAD-KDTPOTYP                        
402200        IF ORAD-KDPRTYP NOT = 'P'                                         
402300           MOVE +0             TO ORAD-PRARTNTO                           
402400           MOVE SPACE          TO ORAD-KDPRTYP                            
402500**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
402600           IF NOT DIST79-DEALER-PRICE                                     
402700              MOVE ZERO        TO ORAD-PRARTNTO-LOC                       
402800              MOVE NEJ         TO ORAD-FLPRTILL                           
402900           END-IF                                                         
403000*************                                                             
403100        END-IF                                                            
403200        MOVE JA                   TO OBKR-SW                              
403300        MOVE NEJ                  TO ALLT-SW                              
403400     END-IF                                                               
403500                                                                          
403600     END-IF                                                               
403700     .                                                                    
403800     EJECT                                                                
403900 ECR-PREL-AVBOKNING-CDC SECTION.                                          
404000                                                                          
404100     MOVE 'STA ECR-PREL-CDC   '           TO   WS-PGM-POSITION            
404200                                                                          
404300     IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                    
404400                                                                          
404500       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
404600                                 TO CDCA-FLFINLV-IN                       
404700       MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                       
404800       MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                      
404900       MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                      
405000       MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                      
405100       MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                       
405200       MOVE ORFK-FLSLATT(WS-INDEX-MID)                                    
405300                                 TO CDCA-FLSLATT-IN                       
405400       MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                       
405500       MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
405600       MOVE ORAD-IDDC            TO CDCA-IDDC-IN                          
405700       MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                      
405800       MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                   
405900       MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
406000       MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
406100       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
406200       MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
406300       MOVE SPACE                TO CDCA-KDPROTYP-IN                      
406400       MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                       
406500       MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
406600       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
406700       MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
406800       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
406900       MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                       
407000       MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
407100       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
407200       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
407300       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
407400       MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                      
407500       MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                      
407600       MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                       
407700       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
407800       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
407900       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
408000       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
408100       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
408200       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
408300       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
408400       MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
408500       MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                      
408600       MOVE +1                   TO CDCA-KDCALL                           
408700                                                                          
408800       IF KOLLA-ARBTAB-C1                                                 
408900         PERFORM ECRA-KOLLA-UTSKRIVET-LO                                  
409000       END-IF                                                             
409100                                                                          
409200       IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                  
409300         CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                  
409400                                           CDCA-INLB-PCB                  
409500                                           CDCA-WDB2-PCB                  
409600                                           CDCA-WDC1-PCB                  
409700                                                                          
409800         IF KERS-KDERS > 0 AND < 10                                       
409900            IF CDCA-KVPREAVB-UT = +0  AND  CDCA-KVPRERO-UT = +0           
410000               MOVE JA             TO OBKR-SW                             
410100            ELSE                                                          
410200               PERFORM S02-RENSA-TILLK-TAB                                
410300               MOVE ZERO           TO KERS-KDORDBEK                       
410400               MOVE NEJ            TO TILLK-SW                            
410500            END-IF                                                        
410600         ELSE                                                             
410700***FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                        
410800            IF CDCA-KVPREAVB-UT > 0                                       
410900              IF KOLLA-ERS    OR                                          
411000                (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)            
411100                 MOVE NEJ        TO KOLLA-ERS-SW                          
411200                 MOVE ZERO       TO KERS-KDORDBEK                         
411300                 PERFORM S02-RENSA-TILLK-TAB                              
411400                 MOVE ZERO       TO SPAR-KDORDBEK                         
411500              ELSE                                                        
411600                 IF KERS-KDERS = +19 OR +29                               
411700                    MOVE ZERO    TO SPAR-KDORDBEK                         
411800                 END-IF                                                   
411900              END-IF                                                      
412000            ELSE                                                          
412100               IF (CDCA-KVPREAVB-UT <= 0) AND                             
412200                 (CDCA-KDORDBEK-UT = 92 OR 99)                            
412300                   MOVE ZEROES   TO CDCA-KDORDBEK-UT                      
412400               END-IF                                                     
412500               IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                    
412600               OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)            
412700               OR SPAR-KDORDBEK = 54                                      
412800                  PERFORM S07-SPACE-SDCA-KDORDBEK                         
412900               END-IF                                                     
413000            END-IF                                                        
413100         END-IF                                                           
413200************                                                              
413300         IF NOT TILLAEGG-TPO                                              
413400*OM DET ÄR EN TILLÄGGSTPO SÅ SKALL INGEN RAD SKRIVAS. TL 050127           
413500           MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                     
413600                                                                          
413700           IF ORAD-IDLEVNR NOT = SPACE                                    
413800              CONTINUE                                                    
413900           ELSE                                                           
414000              MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                   
414100              MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                 
414200           END-IF                                                         
414300           MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                     
414400           MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                      
414500           MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                      
414600           MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                     
414700           IF CDCA-KDORDBEK-UT > ZERO                                     
414800              MOVE JA                TO OBKR-SW                           
414900           END-IF                                                         
415000           IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND         
415100               CDCA-KDORDBEK-UT  = +0 AND                                 
415200               KVAN-KDORDBEK-UT  = +0 AND                                 
415300               DLEV-KDORDBEK-UT  = +0 AND                                 
415400               KERS-KDORDBEK     = +0 AND                                 
415500               TPO1-KDORDBEK     = +0 AND                                 
415600               TPO2-KDORDBEK     = +0 AND                                 
415700               KAMP-KDORDBEK     = +0 AND                                 
415800               STOR-KDORDBEK     = +0 AND                                 
415900               XDCA-KDORDBEK     = +0 AND                                 
416000               SDCA-KDORDBEK     = +0 AND                                 
416100               SDCA-KDORDBEK-FIRST-SDC = +0 AND                           
416200               SDCA-KDORDBEK-SECOND-SDC = +0 AND                          
416300               SPAR-KDORDBEK     = ZERO                                   
416400             MOVE JA                  TO EGET-CL-RAD-SW                   
416500           END-IF                                                         
416600           MOVE AREG-ADLAGOMR       TO ORAD-ADLAGOMR                      
416700           MOVE AREG-ADGANG         TO ORAD-ADGANG                        
416800           MOVE AREG-ADPLATS        TO ORAD-ADPLATS                       
416900         END-IF                                                           
417000       END-IF                                                             
417100     END-IF                                                               
417200     .                                                                    
417300     EJECT                                                                
417400 ECRA-KOLLA-UTSKRIVET-LO SECTION.                                         
417500                                                                          
417600     MOVE 'STA ECRA-KOLL-LO   '           TO   WS-PGM-POSITION            
417700     MOVE NEJ       TO RAD-GODKAND-SW                                     
417800                                                                          
417900     MOVE ARB-IDDC         TO W-IDDC-WDQ212                               
418000     IF AREG-ADLAGOMR = +0                                                
418100        MOVE +1            TO W-ADLAGOMR                                  
418200     ELSE                                                                 
418300        MOVE AREG-ADLAGOMR TO W-ADLAGOMR                                  
418400     END-IF                                                               
418500                                                                          
418600     PERFORM IMS-GNP-WDQ221                                               
418700     IF SEGMENT-FINNS                                                     
418800        MOVE JA               TO RAD-GODKAND-SW                           
418900     END-IF                                                               
419000                                                                          
419100     IF NOT RAD-GODKAND                                                   
419200        MOVE NEJ              TO ALLT-SW                                  
419300        MOVE JA               TO OBKR-SW                                  
419400     END-IF                                                               
419500     .                                                                    
419600     EJECT                                                                
419700 ECS-SKRIV-OBKR SECTION.                                                  
419800                                                                          
419900     MOVE 'STA ECS-SKRIV-OBKR '           TO   WS-PGM-POSITION            
420000*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
420100*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT LAGER 'SLÄPAR'               
420200*    ISRT AV RADEN.                                                       
420300*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
420400*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
420500*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
420600*---                                                                      
420700     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
420800                                                                          
420900     IF TILLKOMMANDE-RAD                                                  
421000        IF KERS-KDORDBEK = 41                                             
421100           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
421200           MOVE '4202KER1'        TO OBKR-IDPGM                           
421300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
421400                                  TO OBKR-KVBEART-TILLK                   
421500           COMPUTE OBKR-DIERS-KVOT =                                      
421600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
421700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
421800           MOVE 'S'               TO OBKR-SW                              
421900        END-IF                                                            
422000     END-IF                                                               
422100                                                                          
422200     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
422300       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
422400*----(KOD 58, 59, 98)                                                     
422500        IF OBKR-SKRIVEN                                                   
422600           PERFORM IMS-08-ISRT-WDQ101                                     
422700           ADD +1              TO OBKR-IDSEKVNR                           
422800        END-IF                                                            
422900        IF ORFK-KDORDBEK(WS-INDEX-MID) = 98                               
423000          MOVE 82              TO OBKR-KDORDBEK                           
423100          MOVE IDPGM           TO OBKR-IDPGM                              
423200        ELSE                                                              
423300          MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                
423400                               TO OBKR-KDORDBEK                           
423500          MOVE '4202ORFK'      TO OBKR-IDPGM                              
423600        END-IF                                                            
423700        MOVE 'S'               TO OBKR-SW                                 
423800     END-IF                                                               
423900     EJECT                                                                
424000     IF KVAN-KDORDBEK-UT > +0                                             
424100*----(KOD 43, 44)                                                         
424200        IF OBKR-SKRIVEN                                                   
424300           PERFORM IMS-08-ISRT-WDQ101                                     
424400           ADD +1              TO OBKR-IDSEKVNR                           
424500        END-IF                                                            
424600        IF TILLKOMMANDE-RAD                                               
424700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
424800                               TO OBKR-KVBEART-TILLK                      
424900           COMPUTE OBKR-DIERS-KVOT =                                      
425000                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
425100                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
425200        END-IF                                                            
425300        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
425400        MOVE '4202KVAN'        TO OBKR-IDPGM                              
425500        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
425600        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
425700        MOVE 'S'               TO OBKR-SW                                 
425800     END-IF                                                               
425900     EJECT                                                                
426000                                                                          
426100     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
426200*----(KOD 21, 53, 82, 95) , 26                                            
426300        IF OBKR-SKRIVEN                                                   
426400           PERFORM IMS-08-ISRT-WDQ101                                     
426500           ADD +1              TO OBKR-IDSEKVNR                           
426600        END-IF                                                            
426700        IF TILLKOMMANDE-RAD                                               
426800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
426900                               TO OBKR-KVBEART-TILLK                      
427000           COMPUTE OBKR-DIERS-KVOT =                                      
427100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
427200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
427300        END-IF                                                            
427400        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
427500        MOVE '4202DLEV'        TO OBKR-IDPGM                              
427600        MOVE 'S'               TO OBKR-SW                                 
427700     END-IF                                                               
427800     EJECT                                                                
427900     IF KERS-KDORDBEK > +0                                                
428000*----(KOD 41, 61)                                                         
428100                                                                          
428200        IF KERS-KDORDBEK = 61                                             
428300*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
428400*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
428500*----RADBEHANDLINGEN                                                      
428600           IF OBKR-SKRIVEN                                                
428700              PERFORM IMS-08-ISRT-WDQ101                                  
428800              ADD +1           TO OBKR-IDSEKVNR                           
428900           END-IF                                                         
429000           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
429100           MOVE '4202KER2'     TO OBKR-IDPGM                              
429200           MOVE 'S'            TO OBKR-SW                                 
429300           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
429400           PERFORM S02-RENSA-TILLK-TAB                                    
429500        ELSE                                                              
429600           IF NOT TILLKOMMANDE-RAD                                        
429700              IF OBKR-SKRIVEN                                             
429800                 PERFORM IMS-08-ISRT-WDQ101                               
429900                 ADD +1        TO OBKR-IDSEKVNR                           
430000              END-IF                                                      
430100              MOVE 'S'            TO OBKR-SW                              
430200              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
430300              MOVE '4202KER3'  TO OBKR-IDPGM                              
430400           END-IF                                                         
430500        END-IF                                                            
430600     END-IF                                                               
430700     EJECT                                                                
430800     IF SPAR-KDORDBEK > ZERO                                              
430900*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90, 92)                 
431000        IF OBKR-SKRIVEN                                                   
431100           PERFORM IMS-08-ISRT-WDQ101                                     
431200           ADD +1              TO OBKR-IDSEKVNR                           
431300        END-IF                                                            
431400        IF TILLKOMMANDE-RAD                                               
431500           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
431600                               TO OBKR-KVBEART-TILLK                      
431700           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
431800              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
431900             MOVE +0              TO OBKR-DIERS-KVOT                      
432000           ELSE                                                           
432100             COMPUTE OBKR-DIERS-KVOT =                                    
432200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
432300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
432400           END-IF                                                         
432500        END-IF                                                            
432600        MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                           
432700        MOVE '4202SPAR'        TO OBKR-IDPGM                              
432800        MOVE 'S'               TO OBKR-SW                                 
432900     END-IF                                                               
433000     EJECT                                                                
433100                                                                          
433200     IF TPO1-KDORDBEK > +0                                                
433300*----(KOD 72, 73, 74, 85)                                                 
433400        IF OBKR-SKRIVEN                                                   
433500           PERFORM IMS-08-ISRT-WDQ101                                     
433600           ADD +1              TO OBKR-IDSEKVNR                           
433700        END-IF                                                            
433800        IF TILLKOMMANDE-RAD                                               
433900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
434000                               TO OBKR-KVBEART-TILLK                      
434100           COMPUTE OBKR-DIERS-KVOT =                                      
434200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
434300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
434400        END-IF                                                            
434500        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
434600        MOVE '4202TPO1'        TO OBKR-IDPGM                              
434700        IF TPO1-KDORDBEK = 85                                             
434800           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
434900        END-IF                                                            
435000        MOVE 'S'               TO OBKR-SW                                 
435100     END-IF                                                               
435200     EJECT                                                                
435300     IF TPO2-KDORDBEK > +0                                                
435400*----(KOD 70)                                                             
435500        IF OBKR-SKRIVEN                                                   
435600           PERFORM IMS-08-ISRT-WDQ101                                     
435700           ADD +1              TO OBKR-IDSEKVNR                           
435800        END-IF                                                            
435900        IF TILLKOMMANDE-RAD                                               
436000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
436100                               TO OBKR-KVBEART-TILLK                      
436200           COMPUTE OBKR-DIERS-KVOT =                                      
436300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
436400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
436500        END-IF                                                            
436600        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
436700        MOVE '4202TPO2'        TO OBKR-IDPGM                              
436800        MOVE 'S'               TO OBKR-SW                                 
436900     END-IF                                                               
437000     EJECT                                                                
437100                                                                          
437200     IF KAMP-KDORDBEK > +0                                                
437300*----(KOD 72, 75, 76)                                                     
437400        IF OBKR-SKRIVEN                                                   
437500           PERFORM IMS-08-ISRT-WDQ101                                     
437600           ADD +1              TO OBKR-IDSEKVNR                           
437700        END-IF                                                            
437800        IF TILLKOMMANDE-RAD                                               
437900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
438000                               TO OBKR-KVBEART-TILLK                      
438100           COMPUTE OBKR-DIERS-KVOT =                                      
438200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
438300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
438400        END-IF                                                            
438500        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
438600        MOVE '4202KAMP'        TO OBKR-IDPGM                              
438700        MOVE 'S'               TO OBKR-SW                                 
438800     END-IF                                                               
438900     EJECT                                                                
439000                                                                          
439100     IF RELS-KDORDBEK > 0                                                 
439200*----(KOD 56)                                                             
439300          IF OBKR-SKRIVEN                                                 
439400             PERFORM IMS-08-ISRT-WDQ101                                   
439500             ADD +1              TO OBKR-IDSEKVNR                         
439600          END-IF                                                          
439700          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
439800          MOVE '4202ORFK'        TO OBKR-IDPGM                            
439900          MOVE 'S'               TO OBKR-SW                               
440000       END-IF                                                             
440100    EJECT                                                                 
440200     IF XDCA-KDORDBEK > ZERO                                              
440300*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
440400        IF OBKR-SKRIVEN                                                   
440500           PERFORM IMS-08-ISRT-WDQ101                                     
440600           ADD +1              TO OBKR-IDSEKVNR                           
440700        END-IF                                                            
440800        IF TILLKOMMANDE-RAD                                               
440900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
441000                               TO OBKR-KVBEART-TILLK                      
441100           COMPUTE OBKR-DIERS-KVOT =                                      
441200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
441300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
441400        END-IF                                                            
441500                                                                          
441600        IF XDCA-KDORDBEK NOT = 15                                         
441700          IF OHUV-IDDC-TVS = SPACE                                        
441800            IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                     
441900              MOVE 15          TO OBKR-KDORDBEK                           
442000              MOVE IDPGM       TO OBKR-IDPGM                              
442100              MOVE 'S'         TO OBKR-SW                                 
442200                                                                          
442300* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
442400              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
442500                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
442600              END-IF                                                      
442700              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
442800                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
442900              END-IF                                                      
443000              IF SDCA-KDORDBEK = 15                                       
443100                 MOVE ZERO     TO SDCA-KDORDBEK                           
443200              END-IF                                                      
443300            END-IF                                                        
443400            IF OBKR-SKRIVEN                                               
443500              PERFORM IMS-08-ISRT-WDQ101                                  
443600              ADD +1           TO OBKR-IDSEKVNR                           
443700            END-IF                                                        
443800          END-IF                                                          
443900        END-IF                                                            
444000        IF XDCA-KDORDBEK = 80                                             
444100           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
444200        END-IF                                                            
444300        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
444400        MOVE '4202XDCA'        TO OBKR-IDPGM                              
444500        MOVE 'S'               TO OBKR-SW                                 
444600     END-IF                                                               
444700     EJECT                                                                
444800                                                                          
444900     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
445000*----(KOD 15, 53, 80, 92)                                                 
445100        IF OBKR-SKRIVEN                                                   
445200           PERFORM IMS-08-ISRT-WDQ101                                     
445300           ADD +1              TO OBKR-IDSEKVNR                           
445400        END-IF                                                            
445500        IF TILLKOMMANDE-RAD                                               
445600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
445700                               TO OBKR-KVBEART-TILLK                      
445800           COMPUTE OBKR-DIERS-KVOT =                                      
445900                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
446000                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
446100        END-IF                                                            
446200        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
446300           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
446400        END-IF                                                            
446500        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
446600        MOVE '4202SDCA'        TO OBKR-IDPGM                              
446700        MOVE 'S'               TO OBKR-SW                                 
446800     END-IF                                                               
446900     EJECT                                                                
447000                                                                          
447100     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
447200*----(KOD 15, 53, 80, 92)                                                 
447300        IF OBKR-SKRIVEN                                                   
447400           PERFORM IMS-08-ISRT-WDQ101                                     
447500           ADD +1              TO OBKR-IDSEKVNR                           
447600        END-IF                                                            
447700        IF TILLKOMMANDE-RAD                                               
447800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
447900                               TO OBKR-KVBEART-TILLK                      
448000           COMPUTE OBKR-DIERS-KVOT =                                      
448100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
448200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
448300        END-IF                                                            
448400        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
448500           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
448600        END-IF                                                            
448700        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
448800        MOVE '4202SDCA'        TO OBKR-IDPGM                              
448900        MOVE 'S'               TO OBKR-SW                                 
449000     END-IF                                                               
449100     EJECT                                                                
449200                                                                          
449300     IF SDCA-KDORDBEK > ZERO                                              
449400*----(KOD 15, 53, 80, 92)                                                 
449500        IF OBKR-SKRIVEN                                                   
449600           PERFORM IMS-08-ISRT-WDQ101                                     
449700           ADD +1              TO OBKR-IDSEKVNR                           
449800        END-IF                                                            
449900        IF TILLKOMMANDE-RAD                                               
450000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
450100                               TO OBKR-KVBEART-TILLK                      
450200           COMPUTE OBKR-DIERS-KVOT =                                      
450300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
450400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
450500        END-IF                                                            
450600        IF SDCA-KDORDBEK = 80                                             
450700           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
450800        END-IF                                                            
450900        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
451000        MOVE '4202SDCA'        TO OBKR-IDPGM                              
451100        MOVE 'S'               TO OBKR-SW                                 
451200     END-IF                                                               
451300     EJECT                                                                
451400     IF STOR-KDORDBEK > +0                                                
451500*----(KOD 70)                                                             
451600        IF OBKR-SKRIVEN                                                   
451700           PERFORM IMS-08-ISRT-WDQ101                                     
451800           ADD +1              TO OBKR-IDSEKVNR                           
451900        END-IF                                                            
452000        IF TILLKOMMANDE-RAD                                               
452100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
452200                               TO OBKR-KVBEART-TILLK                      
452300           COMPUTE OBKR-DIERS-KVOT =                                      
452400                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
452500                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
452600        END-IF                                                            
452700        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
452800        MOVE '4202STOR'        TO OBKR-IDPGM                              
452900        MOVE 'S'               TO OBKR-SW                                 
453000     END-IF                                                               
453100     EJECT                                                                
453200     IF CDCA-KDORDBEK-UT > +0                                             
453300*----(KOD 80, 92, 99)                                                     
453400        IF OBKR-SKRIVEN                                                   
453500           PERFORM IMS-08-ISRT-WDQ101                                     
453600           ADD +1              TO OBKR-IDSEKVNR                           
453700        END-IF                                                            
453800        IF TILLKOMMANDE-RAD                                               
453900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
454000                               TO OBKR-KVBEART-TILLK                      
454100           COMPUTE OBKR-DIERS-KVOT =                                      
454200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
454300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
454400        END-IF                                                            
454500        IF CDCA-KDORDBEK-UT = +80                                         
454600           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
454700        END-IF                                                            
454800        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
454900        MOVE '4202CDCA'        TO OBKR-IDPGM                              
455000        MOVE 'S'               TO OBKR-SW                                 
455100     END-IF                                                               
455200*                                                                         
455300* PÅ SISTA RADEN FÖR KUNDENS NORMALA LAGER LÄGGS DE AVBOKADE              
455400* ANTALEN!                                                                
455500*                                                                         
455600     IF OBKR-SKRIVEN                                                      
455700         MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                          
455800         MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                           
455900******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
456000         IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                    
456100           PERFORM S05-DELETE-PRICE-Q-LINE                                
456200           INITIALIZE OBKR-DEAL-PR-LINE                                   
456300         END-IF                                                           
456400*************TL 030514                                                    
456500        PERFORM IMS-08-ISRT-WDQ101                                        
456600        ADD +1                 TO OBKR-IDSEKVNR                           
456700     END-IF                                                               
456800     EJECT                                                                
456900                                                                          
457000*** TILLÄGGSTPO SKAPAS                                                    
457100                                                                          
457200     IF NOT OBKR-SKRIVEN OR                                               
457300       (KVAN-KDORDBEK-UT > ZERO AND STOR-KDORDBEK = ZERO)   OR            
457400       (TILLKOMMANDE-RAD AND ORFK-KDORDBEK(WS-INDEX-MID) = ZERO           
457500        AND DLEV-KDORDBEK-UT = ZERO                                       
457600        AND SPAR-KDORDBEK    = ZERO                                       
457700        AND TPO1-KDORDBEK = ZERO AND TPO2-KDORDBEK = ZERO                 
457800        AND KAMP-KDORDBEK = ZERO                                          
457900        AND STOR-KDORDBEK = ZERO AND CDCA-KDORDBEK-UT = ZERO )            
458000        IF NOT RAD-GODKAND                                                
458100           IF TPO1-FLKLAR = NEJ AND TPO2-FLKLAR = NEJ AND                 
458200              KAMP-FLKLAR = NEJ                                           
458300              PERFORM ECSB-SKAPA-TPO2                                     
458400           END-IF                                                         
458500        END-IF                                                            
458600     END-IF                                                               
458700     .                                                                    
458800     EJECT                                                                
458900 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
459000                                                                          
459100     MOVE 'STA ECSA-RED-OBKR  '           TO   WS-PGM-POSITION            
459200     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
459300     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
459400                               TO OBKR-IDARTNR                            
459500     IF NOT TILLKOMMANDE-RAD                                              
459600        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
459700                                  W-IDORDER-Q1-MAX                        
459800        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
459900                                  W-IDARTNR-Q1-MAX                        
460000        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
460100                                  W-IDLOPNR-Q1-MAX                        
460200                                  W-IDSEKVNR-Q1-MIN                       
460300                                  W-IDSEKVNR-Q1-MAX                       
460400        PERFORM IMS-07-GU-WDQ1-WDQ101                                     
460500        PERFORM UNTIL SEGMENT-SAKNAS                                      
460600           ADD +1              TO W-IDLOPNR-Q1-MIN                        
460700                                  W-IDLOPNR-Q1-MAX                        
460800           PERFORM IMS-07-GU-WDQ1-WDQ101                                  
460900        END-PERFORM                                                       
461000        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
461100        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
461200     END-IF                                                               
461300     IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                                
461400        IF OHUV-IDDC-TVS > ZERO                                           
461500           MOVE OHUV-IDDC-TVS       TO OBKR-IDDC                          
461600                                       ORAD-IDDC                          
461700        ELSE                                                              
461800          MOVE OHUV-IDDC-PRIM     TO OBKR-IDDC                            
461900                                     ORAD-IDDC                            
462000        END-IF                                                            
462100     ELSE                                                                 
462200        MOVE ORAD-IDDC         TO OBKR-IDDC                               
462300     END-IF                                                               
462400     IF WS-IDDC-DDGS NOT = SPACE                                          
462500       MOVE WS-IDDC-DDGS       TO OBKR-IDDC                               
462600     END-IF                                                               
462700     MOVE +0                   TO OBKR-KDORDBEK                           
462800     MOVE SPACE                TO OBKR-BEERS                              
462900     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
463000     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
463100     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
463200     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
463300     MOVE +0                   TO OBKR-DIERS-KVOT                         
463400     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
463500     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
463600     MOVE NEJ                  TO OBKR-FLOBOK                             
463700     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
463800     MOVE NEJ                  TO OBKR-FLOBPRT                            
463900     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
464000     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
464100     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
464200                               TO OBKR-FLSLATT                            
464300     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
464400     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
464500        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
464600     ELSE                                                                 
464700        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
464800                               TO OBKR-REKSIFFR                           
464900     END-IF                                                               
465000     IF TILLKOMMANDE-RAD                                                  
465100        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
465200                               TO OBKR-IDARTNR-TILLK                      
465300        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
465400                               TO OBKR-REKSIFFR-TILLK                     
465500     ELSE                                                                 
465600        MOVE +0                TO OBKR-IDARTNR-TILLK                      
465700        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
465800     END-IF                                                               
465900     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
466000     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
466100     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
466200     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
466300     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
466400     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
466500     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
466600     IF NOT TILLKOMMANDE-RAD                                              
466700        MOVE AREG-KDERS        TO OBKR-KDERS                              
466800     END-IF                                                               
466900     MOVE ORAD-KDOI            TO OBKR-KDOI                               
467000     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
467100     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
467200     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
467300     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
467400     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
467500     MOVE +0                   TO OBKR-KVANNANT                           
467600     MOVE +0                   TO OBKR-KVAVBART                           
467700     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
467800     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
467900     MOVE +0                   TO OBKR-KVBEART-TILLK                      
468000     MOVE +0                   TO OBKR-KVPREAVB                           
468100     MOVE +0                   TO OBKR-KVPRERO                            
468200     IF ALLT-OK                                                           
468300       MOVE KVAN-KVQPACK-UT    TO OBKR-KVQPACK                            
468400     ELSE                                                                 
468500       MOVE ZERO               TO OBKR-KVQPACK                            
468600     END-IF                                                               
468700     MOVE +0                   TO OBKR-KVRO                               
468800     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
468900     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
469000     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
469100     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
469200     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
469300     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
469400     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
469500     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
469600     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
469700     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
469800     MOVE +0                   TO OBKR-TIRODAT                            
469900     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
470000     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
470100       MOVE 20                 TO WS-SEKEL-9KOMPL                         
470200     ELSE                                                                 
470300       MOVE 19                 TO WS-SEKEL-9KOMPL                         
470400     END-IF                                                               
470500     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
470600     MOVE ORAD-TITPO           TO OBKR-TITPO                              
470700     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
470800     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
470900     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
471000     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
471100     MOVE SPACE                TO OBKR-IDBIL                              
471200                                                                          
471300     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
471400     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
471500     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
471600     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
471700     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
471800     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
471900*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
472000     IF OBKR-KDVALISO = SPACE                                             
472100        MOVE 'N/A'             TO OBKR-KDVALISO                           
472200     END-IF                                                               
472300                                                                          
472400     .                                                                    
472500     EJECT                                                                
472600 ECSB-SKAPA-TPO2 SECTION.                                                 
472700                                                                          
472800     MOVE 'STA ECSB-SKAPA-TPO2'           TO   WS-PGM-POSITION            
472900     MOVE +2                   TO OBKR-KDTPOTYP                           
473000     MOVE +71                  TO OBKR-KDORDBEK                           
473100     MOVE IDPGM                TO OBKR-IDPGM                              
473200     MOVE ORAD-TITPO           TO OBKR-TITPO                              
473300     IF ORAD-KDORDING = +3                                                
473400       MOVE SPACE              TO OBKR-KDOI                               
473500     ELSE                                                                 
473600       MOVE ORAD-KDPRODSL      TO TEST-KDPRODSL                           
473700       IF KDPRODSL-VOLVO-EMB                                              
473800         MOVE 'CD'             TO OBKR-KDOI                               
473900       ELSE                                                               
474000         MOVE 'DT'             TO OBKR-KDOI                               
474100       END-IF                                                             
474200     END-IF                                                               
474300     MOVE SPACE                TO OBKR-CLEARGROUP                         
474400     MOVE NEJ                  TO ALLT-SW                                 
474500     IF TILLKOMMANDE-RAD                                                  
474600        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
474700                               TO OBKR-KVBEART-TILLK                      
474800        COMPUTE OBKR-DIERS-KVOT =                                         
474900                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
475000                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
475100     END-IF                                                               
475200     MOVE 'S'                  TO OBKR-SW                                 
475300     PERFORM IMS-08-ISRT-WDQ101                                           
475400     ADD +1                    TO OBKR-IDSEKVNR                           
475500     .                                                                    
475600     EJECT                                                                
475700 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
475800                                                                          
475900     MOVE 'STA ECSC-OBKR-TILLK'           TO   WS-PGM-POSITION            
476000     MOVE +1                   TO WS-INDEX-TILLK                          
476100     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
476200                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
476300        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
476400           IF OBKR-SKRIVEN                                                
476500              PERFORM IMS-08-ISRT-WDQ101                                  
476600              ADD +1              TO OBKR-IDSEKVNR                        
476700           END-IF                                                         
476800           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
476900           MOVE '4202KER4'     TO OBKR-IDPGM                              
477000           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
477100                               TO OBKR-IDARTNR-TILLK                      
477200           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
477300                               TO OBKR-REKSIFFR-TILLK                     
477400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
477500                               TO OBKR-KVBEART-TILLK                      
477600           COMPUTE OBKR-DIERS-KVOT =                                      
477700                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
477800                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
477900           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
478000                               TO OBKR-BEERS                              
478100                                                                          
478200           MOVE 'S'            TO OBKR-SW                                 
478300        END-IF                                                            
478400        ADD +1                 TO WS-INDEX-TILLK                          
478500     END-PERFORM                                                          
478600                                                                          
478700     IF WS-INDEX-TILLK = +1                                               
478800        MOVE +0                TO OBKR-KDERS                              
478900     END-IF                                                               
479000     .                                                                    
479100     EJECT                                                                
479200 ECU-KONTROLLERA-ENHETSLAST SECTION.                                      
479300                                                                          
479400     MOVE 'STA ECU-KOLLA-ENHET'           TO   WS-PGM-POSITION            
479500     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
479600     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
479700     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
479800     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
479900     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
480000     MOVE ORAD-IDDC            TO LAST-IDDC                               
480100     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
480200     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
480300     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
480400     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
480500                                                                          
480600     PERFORM S10-HAMTA-WDB6-INFO                                          
480700                                                                          
480800     IF VANLIGA-RADER-C1 AND DCS-CDC                                      
480900       CALL W411LAST USING LAST-W411LAST                                  
481000     ELSE                                                                 
481100       IF KOLLA-ARBTAB-C1 AND DCS-CDC AND                                 
481200          (AREG-KVQPACK-3 > +0 OR AREG-KVQPACK-4 > +0)                    
481300          PERFORM ECUA-KOLLA-LO-60-61                                     
481400          IF RAD-LO-60 AND RAD-LO-61                                      
481500             CALL W411LAST USING LAST-W411LAST                            
481600          ELSE                                                            
481700             MOVE +0            TO LAST-ADLAGOMR-UT                       
481800             MOVE +0            TO LAST-KVANTAL-UT                        
481900             MOVE +0            TO LAST-KVBEART-UT                        
482000          END-IF                                                          
482100       ELSE                                                               
482200          MOVE +0               TO LAST-ADLAGOMR-UT                       
482300          MOVE +0               TO LAST-KVANTAL-UT                        
482400          MOVE +0               TO LAST-KVBEART-UT                        
482500       END-IF                                                             
482600     END-IF                                                               
482700     .                                                                    
482800     EJECT                                                                
482900 ECUA-KOLLA-LO-60-61 SECTION.                                             
483000                                                                          
483100     MOVE 'STA ECUA-LO-60--61 '           TO   WS-PGM-POSITION            
483200     MOVE NEJ      TO RAD-LO-60-SW                                        
483300     MOVE NEJ      TO RAD-LO-61-SW                                        
483400                                                                          
483500     MOVE ARB-IDDC         TO W-IDDC-WDQ212                               
483600     MOVE +60              TO W-ADLAGOMR                                  
483700     PERFORM IMS-GNP-WDQ221                                               
483800     IF SEGMENT-FINNS                                                     
483900        MOVE JA               TO RAD-LO-60-SW                             
484000     END-IF                                                               
484100                                                                          
484200     MOVE +61              TO W-ADLAGOMR                                  
484300     PERFORM IMS-GNP-WDQ221                                               
484400     IF SEGMENT-FINNS                                                     
484500        MOVE JA               TO RAD-LO-61-SW                             
484600     END-IF                                                               
484700     .                                                                    
484800     EJECT                                                                
484900 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
485000     MOVE 'STA ECV-BERAKNA-WOPS'          TO   WS-PGM-POSITION            
485100     IF LAST-ADLAGOMR-UT = +0 AND                                         
485200        LAST-KVANTAL-UT  = +0 AND                                         
485300        LAST-KVBEART-UT  = +0                                             
485400*------------------------------------------------------------*            
485500*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
485600*------------------------------------------------------------*            
485700        PERFORM ECVA-FIXA-LAGEROMR-PLATS                                  
485800        PERFORM ECVB-REDIGERA-WOPS-AREA                                   
485900        PERFORM IMS-09-ISRT-WDQ401                                        
486000        PERFORM UNTIL SEGMENT-FINNS                                       
486100           ADD +1                    TO ORAD-IDLOPNR                      
486200           PERFORM IMS-09-ISRT-WDQ401                                     
486300        END-PERFORM                                                       
486400     ELSE                                                                 
486500*------------------------------------------------------------*            
486600*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
486700*------------------------------------------------------------*            
486800                                                                          
486900        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
487000*------------------------------------------------------------*            
487100*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
487200*------------------------------------------------------------*            
487300                                                                          
487400           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
487500           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
487600                                    ORAD-KVPRERO                          
487700           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
487800           PERFORM ECVC-BERAEKNA-KVSLATT                                  
487900           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
488000           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
488100           MOVE ORAD-ADGANG          TO WS-ADGANG                         
488200           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
488300           PERFORM ECVB-REDIGERA-WOPS-AREA                                
488400           PERFORM IMS-09-ISRT-WDQ401                                     
488500           PERFORM UNTIL SEGMENT-FINNS                                    
488600              ADD +1                 TO ORAD-IDLOPNR                      
488700              PERFORM IMS-09-ISRT-WDQ401                                  
488800           END-PERFORM                                                    
488900*------------------------------------------------------------*            
489000*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
489100*------------------------------------------------------------*            
489200                                                                          
489300           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
489400           MOVE WS-ADGANG            TO ORAD-ADGANG                       
489500           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
489600                                                                          
489700           MOVE +0                   TO ORAD-KVBEART                      
489800           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
489900                                        ORAD-KVPREAVB                     
490000           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
490100           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
490200             CONTINUE                                                     
490300           ELSE                                                           
490400             IF LAST-ADGANG-UT > ZERO                                     
490500               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
490600             END-IF                                                       
490700           END-IF                                                         
490800           MOVE +0                   TO ORAD-KVPRERO                      
490900           MOVE 1.0000               TO ORAD-RERF-RAD                     
491000           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
491100           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
491200           PERFORM ECVB-REDIGERA-WOPS-AREA                                
491300           PERFORM IMS-09-ISRT-WDQ401                                     
491400           PERFORM UNTIL SEGMENT-FINNS                                    
491500              ADD +1                 TO ORAD-IDLOPNR                      
491600              PERFORM IMS-09-ISRT-WDQ401                                  
491700           END-PERFORM                                                    
491800        ELSE                                                              
491900*------------------------------------------------------------*            
492000*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
492100*------------------------------------------------------------*            
492200           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
492300           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
492400             CONTINUE                                                     
492500           ELSE                                                           
492600             IF LAST-ADGANG-UT > ZERO                                     
492700               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
492800             END-IF                                                       
492900           END-IF                                                         
493000           MOVE 1.0000            TO ORAD-RERF-RAD                        
493100           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
493200           PERFORM ECVB-REDIGERA-WOPS-AREA                                
493300           PERFORM IMS-09-ISRT-WDQ401                                     
493400           PERFORM UNTIL SEGMENT-FINNS                                    
493500              ADD +1              TO ORAD-IDLOPNR                         
493600              PERFORM IMS-09-ISRT-WDQ401                                  
493700           END-PERFORM                                                    
493800        END-IF                                                            
493900     END-IF                                                               
494000     .                                                                    
494100     EJECT                                                                
494200 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
494300                                                                          
494400     MOVE 'STA ECVA-LAGEROMR   '          TO   WS-PGM-POSITION            
494500     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
494600     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
494700                                                                          
494800     IF OHUV-BEVARREF = SPACE                                             
494900       MOVE ORAD-BERADREF   TO ADRS-BEVARREF-IN                           
495000     ELSE                                                                 
495100       MOVE OHUV-BEVARREF   TO WS-HFAK-REF-X10                            
495200       PERFORM ECVAA-KOLLA-I-HFAK-TAB                                     
495300       IF BEVARREF-I-HFAK-TAB                                             
495400         MOVE OHUV-BEVARREF TO ADRS-BEVARREF-IN                           
495500       ELSE                                                               
495600         MOVE ORAD-BERADREF TO ADRS-BEVARREF-IN                           
495700       END-IF                                                             
495800     END-IF                                                               
495900                                                                          
496000     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
496100     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
496200     MOVE 1                    TO ADRS-KDCALL-IN                          
496300     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
496400     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
496500     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
496600     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
496700                                                                          
496800     CALL W413ADRS USING ADRS-W413ADRS                                    
496900                                                                          
497000*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
497100     IF OHUV-IDKAMPRF > 0                                                 
497200       MOVE 8                  TO ORAD-ADLAGOMR                           
497300     ELSE                                                                 
497400       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
497500     END-IF                                                               
497600                                                                          
497700*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
497800                                                                          
497900     PERFORM S10-HAMTA-WDB6-INFO                                          
498000                                                                          
498100     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
498200     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
498300                                                                          
498400     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
498500                                                                          
498600     .                                                                    
498700     EJECT                                                                
498800 ECVAA-KOLLA-I-HFAK-TAB   SECTION.                                        
498900                                                                          
499000     MOVE 'STA ECVAA-HFAK-TAB  '          TO   WS-PGM-POSITION            
499100     IF WS-HFAK-REF-X10 NOT = SPACE                                       
499200        MOVE 1 TO HFAK-TAB-IX                                             
499300        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
499400           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
499500           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
499600           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
499700           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
499800              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
499900              MOVE 99 TO HFAK-TAB-IX                                      
500000           END-IF                                                         
500100           ADD 1 TO HFAK-TAB-IX                                           
500200        END-PERFORM                                                       
500300     END-IF                                                               
500400     .                                                                    
500500     EJECT                                                                
500600 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
500700                                                                          
500800     MOVE 'STA ECVB-RED-WOPS   '          TO   WS-PGM-POSITION            
500900     MOVE +1                   TO AVSR-KDCALL                             
501000     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
501100     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
501200     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
501300     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
501400     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
501500     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
501600                                                                          
501700     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
501800     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
501900     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
502000     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
502100     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
502200     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
502300     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
502400     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
502500     MOVE ORAD-DEAL-PR-LINE                                               
502600                               TO AVSR-DEAL-PR-LINE                       
502700                                               (WS-INDEX-WOPS)            
502800     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
502900     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
503000     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
503100     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
503200                                                                          
503300     ADD +1                    TO WS-INDEX-WOPS                           
503400     .                                                                    
503500     EJECT                                                                
503600 ECVC-BERAEKNA-KVSLATT SECTION.                                           
503700                                                                          
503800     MOVE 'STA ECVC-KVSLATT    '          TO   WS-PGM-POSITION            
503900     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
504000                                                                          
504100        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
504200                                                                          
504300        COMPUTE ORAD-KVSLATT ROUNDED =                                    
504400               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
504500     END-IF                                                               
504600     .                                                                    
504700     EJECT                                                                
504800 S07-SPACE-SDCA-KDORDBEK  SECTION.                                        
504900                                                                          
505000     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
505100         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
505200     ELSE                                                                 
505300        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
505400           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
505500        ELSE                                                              
505600           IF SDCA-KDORDBEK > 0                                           
505700              MOVE ZEROES  TO SDCA-KDORDBEK                               
505800           END-IF                                                         
505900        END-IF                                                            
506000     END-IF                                                               
506100     .                                                                    
506200                                                                          
506300     EJECT                                                                
506400 ED-LAES-TILLK-DATA SECTION.                                              
506500                                                                          
506600     MOVE 'STA ED-TILLK-DATA   '          TO   WS-PGM-POSITION            
506700     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
506800                               TO AREG-IDARTNR                            
506900                                                                          
507000     CALL W411AREG USING AREG-W411AREG                                    
507100                         AREG-WDK6-PCB                                    
507200                         AREG-WDK7-PCB                                    
507300     .                                                                    
507400     EJECT                                                                
507500 F-HOPPA-TILL-SVARSBILD SECTION.                                          
507600                                                                          
507700     MOVE 'STA F-HOPPA         '          TO   WS-PGM-POSITION            
507800     MOVE MFS-KDMFSFOR           TO 4203-SPRAK                            
507900     IF MID-KDTRTYP = 'V'                                                 
508000        MOVE 'W4T203V '          TO 4203-TRANSKOD                         
508100        MOVE ALL '+'             TO 4203-IDDISTR-IN                       
508200                                    4203-IDKUNDNR-IN                      
508300                                    4203-IDORDNR-IN                       
508400        MOVE WS-IDDISTR          TO 4203-IDDISTR-UT                       
508500        MOVE WS-IDKUNDNR         TO 4203-IDKUNDNR-UT                      
508600        MOVE WS-IDORDNR          TO 4203-IDORDNR-UT                       
508700        PERFORM IMS-INSERT-4203V-MSG                                      
508800     ELSE                                                                 
508900        MOVE WS-IDDISTR          TO 4203-IDDISTR-IN                       
509000        MOVE WS-IDKUNDNR         TO 4203-IDKUNDNR-IN                      
509100        MOVE WS-IDORDNR          TO 4203-IDORDNR-IN                       
509200        MOVE MFS-RENSA-FAELT     TO 4203-IDDISTR-UT                       
509300                                    4203-IDKUNDNR-UT                      
509400                                    4203-IDORDNR-UT                       
509500        PERFORM IMS-INSERT-4203-MSG                                       
509600     END-IF                                                               
509700                                                                          
509800                                                                          
509900     MOVE JA                     TO HOPP-TILL-4203                        
510000     .                                                                    
510100     EJECT                                                                
510200 G-VISA-TOM-SIDA SECTION.                                                 
510300                                                                          
510400     MOVE 'STA G-TOM-SIDA      '          TO   WS-PGM-POSITION            
510500     MOVE +1 TO WS-INDEX                                                  
510600     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
510700       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
510800                                 MOD-KVBEART(WS-INDEX)                    
510900                                 MOD-PRARTNTO(WS-INDEX)                   
511000                                 MOD-TITPO(WS-INDEX)                      
511100                                 MOD-FLRESTN(WS-INDEX)                    
511200                                 MOD-FLSLATT(WS-INDEX)                    
511300                                 MOD-KDKVBRYT(WS-INDEX)                   
511400                                 MOD-FLINVEST(WS-INDEX)                   
511500                                 MOD-KDVRINFO(WS-INDEX)                   
511600                                 MOD-BERADREF(WS-INDEX)                   
511700       ADD  +1 TO WS-INDEX                                                
511800     END-PERFORM                                                          
511900     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
512000     .                                                                    
512100     EJECT                                                                
512200 I-SKICKA-PRISFRAGA SECTION.                                              
512300                                                                          
512400                                                                          
512500     MOVE 1                      TO 3039-REQU-IDMSGVER                    
512600     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
512700     MOVE 'W4020200'             TO 3039-REQU-IDUSER                      
512800                                                                          
512900     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
513000     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
513100     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
513200     IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'                           
513300       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
513400       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
513500       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
513600     END-IF                                                               
513700*    MOVE ORAD-IDPRQUES          TO 3039-MID-IDPRQUES                     
513800     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
513900                                                                          
514000     PERFORM S04-SKICKA-OPEN                                              
514100     PERFORM S04-SKICKA-MEDDELANDE                                        
514200     PERFORM S04-SKICKA-CLOSE                                             
514300                                                                          
514400     .                                                                    
514500 Z-FINIT-INSERT-MSG SECTION.                                              
514600                                                                          
514700     IF MED-IDMFSFEL NOT = SPACE                                          
514800         CALL WMEDKONV USING MED-WMEDAREA                                 
514900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
515000     END-IF                                                               
515100                                                                          
515200     IF NOT ALLT-OK                                                       
515300        PERFORM MFS-ROER-EJ-BILD                                          
515400     END-IF                                                               
515500                                                                          
515600     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
515700     PERFORM IMS-INSERT-MSG                                               
515800     .                                                                    
515900     EJECT                                                                
516000 S02-RENSA-TILLK-TAB SECTION.                                             
516100                                                                          
516200     MOVE +1              TO WS-INDEX-TILLK                               
516300     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
516400        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
516500        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
516600        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
516700        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
516800        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
516900        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
517000        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
517100        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
517200        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
517300        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
517400        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
517500        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
517600        INITIALIZE        TILK-DEAL-PR-LINE(WS-INDEX-TILLK)               
517700        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
517800        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
517900        ADD +1            TO WS-INDEX-TILLK                               
518000     END-PERFORM                                                          
518100     MOVE +1              TO WS-INDEX-TILLK                               
518200     .                                                                    
518300     EJECT                                                                
518400                                                                          
518500                                                                          
518600 S03-DATA-TILL-DEL-NOTE SECTION.                                          
518700                                                                          
518800     MOVE 'STA S03-DEL-NOTE    '          TO   WS-PGM-POSITION            
518900     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
519000     IF DIST07-USA-RETAILER-DNOTE                                         
519100     OR DIST07-CAN-RETAILER                                               
519200                                                                          
519300        INITIALIZE DNOT-ORDER-INFO                                        
519400                                                                          
519500        MOVE IDPGM                    TO DNOT-IDPGM                       
519600        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
519700        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
519800        MOVE ORAD-IDDC                TO DNOT-IDDC                        
519900        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
520000        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
520100        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
520200        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
520300        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
520400        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
520500        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
520600        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
520700        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
520800        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
520900        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
521000        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
521100        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
521200        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
521300        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
521400        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
521500        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
521600        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
521700        MOVE NEJ                      TO DNOT-FLDIRLEV                    
521800                                                                          
521900                                                                          
522000        IF WS-INDEX-MID > WS-INDEX-MID-MAX                                
522100           MOVE JA              TO DNOT-FL-ORAD-LAST                      
522200        END-IF                                                            
522300                                                                          
522400        CALL W411DNOT USING DNOT-W411DNOT                                 
522500                            DNOT-ORQP-PCB                                 
522600                            DNOT-ORQP2-PCB                                
522700                            DNOT-ORQP3-PCB                                
522800                            DNOT-4013-PCB                                 
522900                            DNOT-BENA-PCB                                 
523000     END-IF                                                               
523100     .                                                                    
523200     EJECT                                                                
523300 S04-SKICKA-OPEN SECTION.                                                 
523400                                                                          
523500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
523600     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
523700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
523800                                                                          
523900     IF SEND-KDRC > 0                                                     
524000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
524100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
524200       DELIMITED BY SIZE INTO FELTEXT                                     
524300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
524400     END-IF                                                               
524500     .                                                                    
524600     SKIP3                                                                
524700 S04-SKICKA-MEDDELANDE SECTION.                                           
524800                                                                          
524900     MOVE 'PUT'                      TO SEND-KDFUNC                       
525000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
525100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
525200                                                                          
525300     IF SEND-KDRC > 0                                                     
525400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
525500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
525600       DELIMITED BY SIZE INTO FELTEXT                                     
525700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
525800     END-IF                                                               
525900     .                                                                    
526000     SKIP3                                                                
526100 S04-SKICKA-CLOSE SECTION.                                                
526200                                                                          
526300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
526400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
526500                                                                          
526600     IF SEND-KDRC > 0                                                     
526700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
526800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
526900       DELIMITED BY SIZE INTO FELTEXT                                     
527000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
527100     END-IF                                                               
527200     .                                                                    
527300     EJECT                                                                
527400 S05-DELETE-PRICE-Q-LINE SECTION.                                         
527500                                                                          
527600     IF DIST79-DEALER-PRICE                                               
527700       IF OBKR-IDPRQUES > ZERO                                            
527800         INITIALIZE PRQU-W335PRQU                                         
527900         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
528000         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
528100         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
528200         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
528300         MOVE 4                  TO PRQU-KDCALL                           
528400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
528500                                            PRQU-WDC7-PCB                 
528600                                            PRQU-SJKO-WDK6-PCB            
528700       END-IF                                                             
528800     END-IF                                                               
528900     .                                                                    
529000     EJECT                                                                
529100 S10-HAMTA-WDB6-INFO      SECTION.                                        
529200                                                                          
529300     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
529400        CONTINUE                                                          
529500     ELSE                                                                 
529600        IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                           
529700                                                                          
529800           MOVE 1 TO WS-CLDC-IX                                           
529900           PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                   
530000                         CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR              
530100                         CLDC-IDDC (WS-CLDC-IX) = SPACE                   
530200              ADD 1 TO WS-CLDC-IX                                         
530300           END-PERFORM                                                    
530400                                                                          
530500        END-IF                                                            
530600     END-IF                                                               
530700     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
530800        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
530900        MOVE WS-IDDC TO W-IDDC-B6                                         
531000        PERFORM IMS-GU-WDB601                                             
531100     ELSE                                                                 
531200        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
531300     END-IF                                                               
531400     .                                                                    
531500     EJECT                                                                
531600 MFS-RENSA-MOD-RADER SECTION.                                             
531700                                                                          
531800     MOVE +1 TO WS-INDEX                                                  
531900     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
532000       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
532100                                 MOD-KVBEART(WS-INDEX)                    
532200                                 MOD-PRARTNTO(WS-INDEX)                   
532300                                 MOD-TITPO(WS-INDEX)                      
532400                                 MOD-FLRESTN(WS-INDEX)                    
532500                                 MOD-FLSLATT(WS-INDEX)                    
532600                                 MOD-KDKVBRYT(WS-INDEX)                   
532700                                 MOD-FLINVEST(WS-INDEX)                   
532800                                 MOD-KDVRINFO(WS-INDEX)                   
532900                                 MOD-BERADREF(WS-INDEX)                   
533000       ADD  +1 TO WS-INDEX                                                
533100     END-PERFORM                                                          
533200     .                                                                    
533300     EJECT                                                                
533400                                                                          
533500 MFS-ROER-EJ-BILD SECTION.                                                
533600                                                                          
533700     MOVE +1 TO WS-INDEX                                                  
533800     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
533900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(WS-INDEX)                    
534000                                 MOD-KVBEART(WS-INDEX)                    
534100                                 MOD-PRARTNTO(WS-INDEX)                   
534200                                 MOD-TITPO(WS-INDEX)                      
534300                                 MOD-FLRESTN(WS-INDEX)                    
534400                                 MOD-FLSLATT(WS-INDEX)                    
534500                                 MOD-KDKVBRYT(WS-INDEX)                   
534600                                 MOD-FLINVEST(WS-INDEX)                   
534700                                 MOD-KDVRINFO(WS-INDEX)                   
534800                                 MOD-BERADREF(WS-INDEX)                   
534900       ADD  +1 TO WS-INDEX                                                
535000     END-PERFORM                                                          
535100     .                                                                    
535200     EJECT                                                                
535300                                                                          
535400* --- IMS SEKTIONER ---                                                   
535500                                                                          
535600 IMS-GET-MSG SECTION.                                                     
535700                                                                          
535800     MOVE '  QC' TO GODK-STATUSKODER                                      
535900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
536000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
536100     PERFORM IMS-STATUSKONTROLL                                           
536200     .                                                                    
536300     SKIP2                                                                
536400 IMS-INSERT-MSG SECTION.                                                  
536500                                                                          
536600     IF NOT ENGLISH-TEXT                                                  
536700       MOVE '0' TO MFS-KDHUVOMR                                           
536800     END-IF                                                               
536900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
537000     MOVE SPACE TO GODK-STATUSKODER                                       
537100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
537200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
537300     PERFORM IMS-STATUSKONTROLL                                           
537400     .                                                                    
537500     SKIP2                                                                
537600 IMS-INSERT-4203-MSG SECTION.                                             
537700                                                                          
537800     IF NOT ENGLISH-TEXT                                                  
537900       MOVE '0' TO MFS-KDHUVOMR                                           
538000     END-IF                                                               
538100     MOVE LOW-VALUE TO 4203-Z1 4203-Z2                                    
538200     MOVE SPACE TO GODK-STATUSKODER                                       
538300     CALL CBLTDLI USING ISRT 4203-PCB 4203-MSG-IO-AREA                    
538400     MOVE 4203-STATUS-CODE TO STATUS-WS                                   
538500     PERFORM IMS-STATUSKONTROLL                                           
538600     .                                                                    
538700     EJECT                                                                
538800 IMS-INSERT-4203V-MSG SECTION.                                            
538900                                                                          
539000     IF NOT ENGLISH-TEXT                                                  
539100       MOVE '0' TO MFS-KDHUVOMR                                           
539200     END-IF                                                               
539300     MOVE LOW-VALUE TO 4203-Z1 4203-Z2                                    
539400     MOVE SPACE TO GODK-STATUSKODER                                       
539500     CALL CBLTDLI USING ISRT 4203V-PCB 4203-MSG-IO-AREA                   
539600     MOVE 4203V-STATUS-CODE TO STATUS-WS                                  
539700     PERFORM IMS-STATUSKONTROLL                                           
539800     .                                                                    
539900     SKIP2                                                                
540000 IMS-01-GU-WDQ2-WDQ201 SECTION.                                           
540100                                                                          
540200     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
540300          DELIMITED BY SIZE INTO SSA1                                     
540400     MOVE '  GE'               TO GODK-STATUSKODER                        
540500     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                 
540600     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
540700     PERFORM IMS-STATUSKONTROLL                                           
540800     .                                                                    
540900     EJECT                                                                
541000 IMS-02-GHU-WDQ2-WDQ201 SECTION.                                          
541100                                                                          
541200     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
541300          DELIMITED BY SIZE INTO SSA1                                     
541400     MOVE '    '               TO GODK-STATUSKODER                        
541500     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                
541600     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
541700     PERFORM IMS-STATUSKONTROLL                                           
541800     .                                                                    
541900     SKIP2                                                                
542000 IMS-GNP-WDQ212         SECTION.                                          
542100                                                                          
542200     MOVE 'WDQ212  '       TO SSA1                                        
542300     MOVE '  GE'           TO GODK-STATUSKODER                            
542400     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
542500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
542600     PERFORM IMS-STATUSKONTROLL                                           
542700     .                                                                    
542800     SKIP3                                                                
542900 IMS-GHNP-WDQ212         SECTION.                                         
543000                                                                          
543100     MOVE 'WDQ212  '       TO SSA1                                        
543200     MOVE '  GE'           TO GODK-STATUSKODER                            
543300     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                
543400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
543500     PERFORM IMS-STATUSKONTROLL                                           
543600     .                                                                    
543700     SKIP3                                                                
543800 IMS-GNP-WDQ212-FIRST   SECTION.                                          
543900                                                                          
544000     STRING 'WDQ212  *F(IDDC     =' W-IDDC-X ')'                          
544100          DELIMITED BY SIZE INTO SSA1                                     
544200     MOVE '  GE' TO GODK-STATUSKODER                                      
544300     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
544400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
544500     PERFORM IMS-STATUSKONTROLL                                           
544600     .                                                                    
544700     SKIP3                                                                
544800 IMS-GNP-WDQ221         SECTION.                                          
544900                                                                          
545000     STRING 'WDQ212  *F(IDDC     =' W-IDDC-X ')'                          
545100          DELIMITED BY SIZE INTO SSA1                                     
545200     STRING 'WDQ221  *F(ADLAGOMR =' W-ADLAGOMR-X ')'                      
545300            DELIMITED BY SIZE INTO SSA2                                   
545400     MOVE '  GE' TO GODK-STATUSKODER                                      
545500     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-LOR SSA1 SSA2            
545600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
545700     PERFORM IMS-STATUSKONTROLL                                           
545800     .                                                                    
545900     SKIP3                                                                
546000 IMS-04-REPL-WDQ2-WDQ201 SECTION.                                         
546100                                                                          
546200     MOVE '    '               TO GODK-STATUSKODER                        
546300     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-OHUV                    
546400     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
546500     PERFORM IMS-STATUSKONTROLL                                           
546600     .                                                                    
546700     EJECT                                                                
546800 IMS-REPL-WDQ212         SECTION.                                         
546900                                                                          
547000     MOVE '    '               TO GODK-STATUSKODER                        
547100     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-ARB                     
547200     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
547300     PERFORM IMS-STATUSKONTROLL                                           
547400     .                                                                    
547500     EJECT                                                                
547600 IMS-07-GU-WDQ1-WDQ101 SECTION.                                           
547700                                                                          
547800     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
547900                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
548000          DELIMITED BY SIZE INTO SSA1                                     
548100     MOVE '  GE'             TO GODK-STATUSKODER                          
548200     CALL CBLTDLI USING GU   WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
548300     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
548400     PERFORM IMS-STATUSKONTROLL                                           
548500     .                                                                    
548600     SKIP2                                                                
548700 IMS-08-ISRT-WDQ101 SECTION.                                              
548800                                                                          
548900     MOVE 'WDQ101   '          TO SSA1                                    
549000     MOVE '    '               TO GODK-STATUSKODER                        
549100     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
549200     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
549300     PERFORM IMS-STATUSKONTROLL                                           
549400     .                                                                    
549500     SKIP2                                                                
549600 IMS-09-ISRT-WDQ401 SECTION.                                              
549700                                                                          
549800     MOVE 'WDQ401   '          TO SSA1                                    
549900     MOVE '  II'               TO GODK-STATUSKODER                        
550000     CALL CBLTDLI USING ISRT WDQ4-PCB DLI-IO-AREA-ORAD SSA1               
550100     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
550200     PERFORM IMS-STATUSKONTROLL                                           
550300     .                                                                    
550400     EJECT                                                                
550500 IMS-10-GU-WLARTM-WDK901 SECTION.                                         
550600                                                                          
550700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
550800          DELIMITED BY SIZE INTO SSA1                                     
550900     MOVE '  GE'               TO GODK-STATUSKODER                        
551000     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
551100     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
551200     PERFORM IMS-STATUSKONTROLL                                           
551300     .                                                                    
551400     SKIP2                                                                
551500 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
551600                                                                          
551700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
551800          DELIMITED BY SIZE INTO SSA1                                     
551900     MOVE '    '               TO GODK-STATUSKODER                        
552000     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
552100     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
552200     PERFORM IMS-STATUSKONTROLL                                           
552300     .                                                                    
552400                                                                          
552500 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
552600                                                                          
552700     MOVE '    '               TO GODK-STATUSKODER                        
552800     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
552900     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
553000     PERFORM IMS-STATUSKONTROLL                                           
553100     .                                                                    
553200     EJECT                                                                
553300 IMS-16-GNP-WDQ2-WDQ211 SECTION.                                          
553400                                                                          
553500     STRING 'WDQ211  *F(WDQ211KY =' W-WDQ211KY-X ')'                      
553600          DELIMITED BY SIZE INTO SSA1                                     
553700     MOVE '  GE'               TO GODK-STATUSKODER                        
553800     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-AREA-DLEV SSA1               
553900     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
554000     PERFORM IMS-STATUSKONTROLL                                           
554100     .                                                                    
554200     SKIP2                                                                
554300 IMS-GU-WDB201       SECTION.                                             
554400                                                                          
554500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
554600          DELIMITED BY SIZE INTO SSA1                                     
554700     MOVE '  '                 TO GODK-STATUSKODER                        
554800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
554900     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
555000     PERFORM IMS-STATUSKONTROLL                                           
555100     .                                                                    
555200     SKIP2                                                                
555300 IMS-GU-WDB101 SECTION.                                                   
555400                                                                          
555500     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
555600          DELIMITED BY SIZE INTO SSA1                                     
555700     MOVE '  '                 TO GODK-STATUSKODER                        
555800     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
555900     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
556000     PERFORM IMS-STATUSKONTROLL                                           
556100     .                                                                    
556200                                                                          
556300 IMS-GU-WDB601    SECTION.                                                
556400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
556500          DELIMITED BY SIZE INTO SSA1                                     
556600     MOVE '  GE' TO GODK-STATUSKODER                                      
556700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
556800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
556900     PERFORM IMS-STATUSKONTROLL                                           
557000     IF SEGMENT-SAKNAS                                                    
557100        MOVE SPACE TO DCS-KDDC                                            
557200     END-IF                                                               
557300     .                                                                    
557400     EJECT                                                                
557500 DB2-SELECT-TP4TRAN     SECTION.                                          
557600     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
557700                                                                          
557800     MOVE 000100 TO GODK-SQLCODEKODER                                     
557900                                                                          
558000     EXEC SQL                                                             
558100           SELECT  DISTINCT                                               
558200                   IDDC_REC                                               
558300                                                                          
558400           INTO   :TP4TRAN-IDDC-REC                                       
558500                                                                          
558600           FROM    TP4TRAN                                                
558700                                                                          
558800           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
558900     END-EXEC                                                             
559000                                                                          
559100     MOVE SQLCODE TO SQLCODE-WS                                           
559200     PERFORM DB2-STATUSKONTROLL                                           
559300     .                                                                    
559400     EJECT                                                                
559500 IMS-ISRT-WDR601 SECTION.                                                 
559600                                                                          
559700     MOVE 'WDR601' TO SSA1                                                
559800     MOVE '  II' TO GODK-STATUSKODER                                      
559900     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
560000     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
560100     PERFORM IMS-STATUSKONTROLL                                           
560200     .                                                                    
560300 IMS-STATUSKONTROLL SECTION.                                              
560400                                                                          
560500     SET STATUS-IX TO 1                                                   
560600     SEARCH GODK-STATUS                                                   
560700       AT END CALL FELLOG                                                 
560800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
560900     END-SEARCH                                                           
561000     .                                                                    
561100 DB2-STATUSKONTROLL  SECTION.                                             
561200                                                                          
561300     SET SQLCODE-IX TO 1                                                  
561400     SEARCH GODK-SQLCODE                                                  
561500       AT END                                                             
561600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
561700          DELIMITED BY SIZE INTO FELTEXT                                  
561800          CALL ABEND USING RKOD-ABEND-DB2                                 
561900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
562000     END-SEARCH                                                           
562100     .                                                                    
562200                                                                          
