000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4024400.                                                
000400 AUTHOR.         GÖRAN KJELLSON  GUIDE DATAKONSULT AB                     
000500 DATE-WRITTEN.   NOV   -90.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR EXTERN TILLÄGG AV ORDERRADER.                
001100*        DÄR ORDERN / ORDERDELAR HAR STATUS HÖGRE ÄN R                    
001200*        SKAPAS TPO2:OR.                                                  
001300*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA HÄMTAS FRÅN          
001400*        ARTIKELREGISTRET.                                                
001500*                                                                         
001600*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001700*        ORDERAVSLUT 4246.                                                
001800*                                                                         
001900*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002000*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
002100*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002200*        PROGRAMMET LÄSER      WLORQL (WDQ2)  ORDERHUVUD SEK-IX           
002300*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002400*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
002500*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
002600*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002700*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
002800*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
002900*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
003000*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003100*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
003200*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003300*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
003400*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
003500*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003600*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003700*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
003800*                                                                         
003900*    INDATA.                                                              
004000*        TRANSAKTION: W4T244                                              
004100*        MID:         W4I24401                                            
004200*    UTDATA.                                                              
004300*        MOD:         W4O24401                                            
004400*                                                                         
004500* CHANGE LOG:                                                             
004600*                                                                         
004700*    E'TRACKER: 5444132 DATED 2007-09-18                                  
004800*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
004900*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
005000*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
005100*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
005200*                                                                         
005300     EJECT                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500                                                                          
005600 DATA DIVISION.                                                           
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000     SKIP3                                                                
006100 77  IDPGM                       PIC X(08)   VALUE 'W4024400'.            
006200 77  HOPP-TILL-4203              PIC X(1)   VALUE 'N'.                    
006300 77  YES                         PIC X(1)   VALUE 'Y'.                    
006400 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
006500 77  CURRENT-SECTION             PIC X(20)  VALUE SPACE.                  
006600 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
006700 77  NEJ                         PIC X(1)   VALUE 'N'.                    
006800 77  JA                          PIC X(1)   VALUE 'J'.                    
006900 77  SPEC-FORBI                  PIC X(1)   VALUE 'S'.                    
007000 77  SW-KDORDSTA-ALL-E-FLAG      PIC X(1)   VALUE 'J'.                    
007100 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)  VALUE 'J'.                    
007200 77  SW-KDORDSTA-O-STATUS-FLAG   PIC X(1)   VALUE 'J'.                    
007300 77  SW-KDORDSTA-STATUS-FLAG     PIC X(1)   VALUE 'J'.                    
007400                                                                          
007500 01  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
007600*01  -COPY WWDCKONS                                                       
007700                                                                          
007800 77  OBEHORIG                    PIC X(1)   VALUE 'F'.                    
007900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
008000 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +810  COMP SYNC.        
008300 77  IX-DCCLEAR-MAX              PIC S9(9)   COMP SYNC VALUE +99.         
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
009600 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009700 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
009800 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
009900 77  SPAR-BEHORIGHETS-KONTR      PIC  X(1)   VALUE SPACE.                 
010000 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
010100 77  WX-KDORDBEK                 PIC S9(2)   VALUE +0.                    
010200 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
010300 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010400 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010500 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010600 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
010700 77  WS-ARB-KDORDSTA-PRIM        PIC X(2)    VALUE SPACE.                 
010800 77  WS-ARB-KDORDSTA-O-PRIM      PIC X(2)    VALUE SPACE.                 
010900                                                                          
011000 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
011100 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011200     03 WS-TIHHMM                PIC 9(4).                                
011300     03 FILLER                   PIC 9(2).                                
011400     EJECT                                                                
011500                                                                          
011600 01 W-GMT-IDDC-CLEAR-GRP.                                                 
011700*                                 GRUPP AV IDDC-CLEAR                     
011800     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
011900                                 PIC X(2)    VALUE SPACE.                 
012000*                                                                         
012100 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
012200                                                                          
012300*    ----DIST79-DEALER-PRICE----                                          
012400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
012500     EJECT                                                                
012600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012700     88  ALLT-OK                             VALUE 'J'.                   
012800                                                                          
012900 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
013000     88  FIRST-TIME                          VALUE 'J'.                   
013100                                                                          
013200 77  TILLK-SW                    PIC X       VALUE 'N'.                   
013300     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
013400     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
013500                                                                          
013600 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
013700     88  KOLLA-ERS                           VALUE 'J'.                   
013800                                                                          
013900 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
014000     88  SVARSBILD                           VALUE 'J'.                   
014100                                                                          
014200 77  OBKR-SW                     PIC X       VALUE 'N'.                   
014300     88  SKRIV-OBKR                          VALUE 'J'.                   
014400     88  OBKR-SKRIVEN                        VALUE 'S'.                   
014500                                                                          
014600 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
014700     88  EGET-CL-RAD                         VALUE 'J'.                   
014800                                                                          
014900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015000     88  EGEN-MID                            VALUE '4244'.                
015100     88  GODK-MID                            VALUE '4241' '4242'          
015200                                                   '4203' '4244'.         
015300     EJECT                                                                
015400                                                                          
015500 77  TILLAEGG-TPO-SW             PIC X       VALUE 'N'.                   
015600     88  TILLAEGG-TPO                        VALUE 'J'.                   
015700                                                                          
015800 77  TILLAEGG-SDC-SW             PIC X       VALUE 'N'.                   
015900     88  TILLAEGG-SDC                        VALUE 'J'.                   
016000     88  EJ-TILLAEGG-SDC                     VALUE 'N'.                   
016100                                                                          
016200 77  TILLAEGG-LDC-SW             PIC X       VALUE 'N'.                   
016300     88  TILLAEGG-LDC                        VALUE 'J'.                   
016400     88  EJ-TILLAEGG-LDC                     VALUE 'N'.                   
016500                                                                          
016600 77  VANLIGA-RADER-C1-SW         PIC X       VALUE 'N'.                   
016700     88 VANLIGA-RADER-C1                     VALUE 'J'.                   
016800     88 VANLIGA-RADER-C1-EJ                  VALUE 'N'.                   
016900                                                                          
017000 77  VANLIGA-RADER-C2-SW         PIC X       VALUE 'N'.                   
017100     88 VANLIGA-RADER-C2                     VALUE 'J'.                   
017200     88 VANLIGA-RADER-C2-EJ                  VALUE 'N'.                   
017300                                                                          
017400 77  KOLLA-ARBTAB-C1-SW          PIC X       VALUE 'N'.                   
017500     88 KOLLA-ARBTAB-C1                      VALUE 'J'.                   
017600     88 KOLLA-ARBTAB-C1-EJ                   VALUE 'N'.                   
017700                                                                          
017800 77  RAD-GODKAND-SW              PIC X       VALUE 'N'.                   
017900     88 RAD-GODKAND                          VALUE 'J'.                   
018000                                                                          
018100 77  RAD-LO-60-SW                PIC X       VALUE 'N'.                   
018200     88 RAD-LO-60                            VALUE 'J'.                   
018300                                                                          
018400 77  RAD-LO-61-SW                PIC X       VALUE 'N'.                   
018500     88 RAD-LO-61                            VALUE 'J'.                   
018600                                                                          
018700     EJECT                                                                
018800 01  WS-ALFA-1.                                                           
018900     03  WS-NUM-1                PIC 9(1).                                
019000 01  WS-ALFA-2.                                                           
019100     03  WS-NUM-2                PIC 9(2).                                
019200 01  WS-ALFA-6.                                                           
019300     03  WS-NUM-6                PIC 9(6).                                
019400 01  WS-ALFA-7.                                                           
019500     03  WS-NUM-7                PIC 9(7).                                
019600 01  WS-ALFA-8.                                                           
019700     03  WS-NUM-8                PIC 9(8).                                
019800     03  FILLER REDEFINES WS-NUM-8.                                       
019900         05  WS-NUM-1--4         PIC 9(4).                                
020000         05  WS-NUM-5--8         PIC 9(4).                                
020100 01  WS-ALFA-12.                                                          
020200     03  WS-NUM-12               PIC 9(12).                               
020300 01  WS-ALFA-2V3.                                                         
020400     03  WS-ALFA-1--2            PIC X(2).                                
020500     03  WS-NUM-PUNKT            PIC X(1).                                
020600     03  WS-ALFA-3--5            PIC X(3).                                
020700 01  WS-NUM-2V3                  PIC 9(2)V9(3).                           
020800 01  FILLER REDEFINES WS-NUM-2V3.                                         
020900     03  WS-NUM-1--2             PIC 9(2).                                
021000     03  WS-NUM-3--5             PIC 9(3).                                
021100     EJECT                                                                
021200                                                                          
021300 01  WS-IDKONTO.                                                          
021400     03  WS-IDKONTO-1--4         PIC 9(4).                                
021500     03  WS-IDKONTO-5            PIC X(1).                                
021600     03  WS-IDKONTO-6--9         PIC 9(4).                                
021700     03  FILLER                  PIC X(1).                                
021800                                                                          
021900 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
022000 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
022100     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
022200     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
022300                                                                          
022400 01  SPAR-KDVALISO               PIC X(3)          VALUE SPACE.           
022500                                                                          
022600*                                                                         
022700*    -COPY WWPRODSL                                                       
022800*                                                                         
022900*    -COPY W411TILK                                                       
023000     EJECT                                                                
023100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023200 01  GENERELLA-SUBPROGRAM.                                                
023300     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
023400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
024000     EJECT                                                                
024100 01  GEMENSAMMA-SUBPROGRAM.                                               
024200     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
024300*        PRISTILLÄMPNING                                                  
024400     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
024500*        HÄMTA PRISFRÅGENR                                                
024600     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
024700*        DEALER PRISFRÅGABEHANDLING                                       
024800     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
024900*        LÄSNING ARTIKELREGISTER                                          
025000     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
025100*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
025200     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
025300*        KONTROLL DIREKTLEVERANS                                          
025400     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
025500*        KONTROLL TPO4 - KAMPANJ                                          
025600     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
025700*        KONTROLL ERSÄTTNINGAR                                            
025800     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
025900*        KONTROLL KVANTANPASSNING                                         
026000     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
026100*        KONTROLL ENHETSLAST                                              
026200     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
026300*        FORMELLA KONTROLLER AV INDATA                                    
026400     EJECT                                                                
026500     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
026600*        KONTROLL PRELIMINÄRAVBOKNING                                     
026700     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
026800*        BERÄKNA RANSONERING                                              
026900     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
027000*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
027100     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
027200*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
027300     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
027400*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
027500     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
027600*        KONTROLL SPÄRRAR                                                 
027700     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
027800*        KONTROLL STORA UTTAG                                             
027900     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
028000*        KONTROLL TPO1                                                    
028100     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
028200*        KONTROLL TPO2                                                    
028300     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
028400*        KONTROLL RELS                                                    
028500     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
028600*        RÄKNA OM VALUTA  DDI                                             
028700     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
028800*        WDB601-SEGMENT FÖR CLARING-DC                                    
028900     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
029000*        WOPS RADBEHANDLING                                               
029100     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
029200*        OMVANDLING AV LAGOMR + PLATS                                     
029300*                                                                         
029400*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
029500*   -COPY WSECAREA                                                        
029600     EJECT                                                                
029700*                                                                         
029800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
029900*   -COPY WMSGINIT                                                        
030000     EJECT                                                                
030100*                                                                         
030200*                                                                         
030300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
030400*                                                                         
030500*                                                                         
030600*   -COPY WMEDAREA                                                        
030700     EJECT                                                                
030800 01  MESSAGE-CODES.                                                       
030900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
031000     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
031100     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
031200     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '718'.                 
031300     03  ERR-ORDER-EJ-AVSLUT     PIC X(3)    VALUE '053'.                 
031400     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
031500     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
031600     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
031700     03  ERR-ORDER-ANNULL        PIC X(3)    VALUE '052'.                 
031800     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
031900     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
032000     03  ERR-ORDERLINES-MISSING  PIC X(3)    VALUE '029'.                 
032100     EJECT                                                                
032200*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
032300*                                                                         
032400 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
032500*   -COPY W335PRIS                                                        
032600*                                                                         
032700     EJECT                                                                
032800*                                                                         
032900 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
033000*   -COPY W335PRNO                                                        
033100*                                                                         
033200     EJECT                                                                
033300*                                                                         
033400 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
033500*   -COPY W335PRQU                                                        
033600*                                                                         
033700     EJECT                                                                
033800*                                                                         
033900 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
034000*   -COPY W411AREG                                                        
034100*                                                                         
034200     EJECT                                                                
034300*                                                                         
034400 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
034500*   -COPY W411ARTM                                                        
034600*                                                                         
034700     EJECT                                                                
034800*                                                                         
034900*                                                                         
035000 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
035100*   -COPY W411DLEV                                                        
035200*                                                                         
035300     EJECT                                                                
035400*                                                                         
035500 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
035600*   -COPY W411KAMP                                                        
035700*                                                                         
035800     EJECT                                                                
035900*                                                                         
036000 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
036100*   -COPY W411KERS                                                        
036200*                                                                         
036300     EJECT                                                                
036400*                                                                         
036500*                                                                         
036600 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
036700*   -COPY W411KVAN                                                        
036800*                                                                         
036900     EJECT                                                                
037000*                                                                         
037100 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
037200*   -COPY W411LAST                                                        
037300*                                                                         
037400     EJECT                                                                
037500*                                                                         
037600 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
037700*   -COPY W411ORFK                                                        
037800*                                                                         
037900     EJECT                                                                
038000*                                                                         
038100 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
038200*   -COPY W411CDCA                                                        
038300*                                                                         
038400     EJECT                                                                
038500*                                                                         
038600 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
038700*   -COPY W411RANS                                                        
038800*                                                                         
038900     EJECT                                                                
039000*                                                                         
039100 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
039200*   -COPY W411NDCA                                                        
039300*                                                                         
039400     EJECT                                                                
039500*                                                                         
039600 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
039700*   -COPY W411XDCA                                                        
039800*                                                                         
039900     EJECT                                                                
040000 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
040100*   -COPY W411XDK7 -PRE NDCA-                                             
040200*                                                                         
040300     EJECT                                                                
040400 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
040500*   -COPY W411SDCA                                                        
040600*                                                                         
040700     EJECT                                                                
040800 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
040900*   -COPY W411SPAR                                                        
041000*                                                                         
041100     EJECT                                                                
041200*                                                                         
041300 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
041400*   -COPY W411STOR                                                        
041500*                                                                         
041600     EJECT                                                                
041700*                                                                         
041800 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
041900*   -COPY W411TPO1                                                        
042000*                                                                         
042100     EJECT                                                                
042200*                                                                         
042300 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
042400*   -COPY W411TPO2                                                        
042500*                                                                         
042600     EJECT                                                                
042700 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
042800*   -COPY W411RELS                                                        
042900*                                                                         
043000     EJECT                                                                
043100*                                                                         
043200     EJECT                                                                
043300 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
043400*   -COPY W411CLDC                                                        
043500     EJECT                                                                
043600*                                                                         
043700 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
043800*   -COPY W413AVSR                                                        
043900*                                                                         
044000     SKIP2                                                                
044100*                                                                         
044200 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
044300*   -COPY W413ADRS                                                        
044400*                                                                         
044500     EJECT                                                                
044600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
044700*                                                                         
044800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
044900     SKIP3                                                                
045000*01  MID -COPY W4I24401                                                   
045100     EJECT                                                                
045200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
045300     SKIP3                                                                
045400*01  -COPY WMSGAREA                                                       
045500     EJECT                                                                
045600*    03  MOD -COPY W4O24401   -RED MSG-AREA.                              
045700     EJECT                                                                
045800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
045900     SKIP3                                                                
046000*01  -COPY WMFSAREA                                                       
046100     EJECT                                                                
046200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046300*                                                                         
046400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046500     SKIP3                                                                
046600 01  NYCKLAR-TILL-DLI.                                                    
046700                                                                          
046800                                                                          
046900     03  W-IDGMTREF-X.                                                    
047000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
047100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
047200         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
047300                                                                          
047400     03  W-WDQ211KY-X.                                                    
047500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
047600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
047700                                                                          
047800     03  W-IDARTNR-X.                                                     
047900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
048000                                                                          
048100     03  W-IDDC-X.                                                        
048200         05  W-IDDC-WDQ212       PIC X(2)    VALUE SPACE.                 
048300                                                                          
048400                                                                          
048500     03  W-ADLAGOMR-X.                                                    
048600         05  W-ADLAGOMR          PIC S9(3)   VALUE ZERO COMP-3.           
048700                                                                          
048800     03  W-WDQ101KY-MIN-X.                                                
048900         05  W-IDORDER-Q1-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
049000         05  W-IDARTNR-Q1-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
049100         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
049200         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
049300         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
049400                                                                          
049500     03  W-WDQ101KY-MAX-X.                                                
049600         05  W-IDORDER-Q1-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
049700         05  W-IDARTNR-Q1-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
049800         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
049900         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
050000         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
050100                                                                          
050200     03  W-IDGMT-X.                                                       
050300         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
050400         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
050500                                                                          
050600     03  W-WDB101KY-X.                                                    
050700       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
050800       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
050900                                                                          
051000     03  W-IDDC-B6-X.                                                     
051100         05 W-IDDC-B6                  PIC X(2).                          
051200                                                                          
051300     EJECT                                                                
051400                                                                          
051500*    --- STATUS-KOD FRÅN IMS                                              
051600 01  STATUS-WS                   PIC XX.                                  
051700     88  SEGMENT-FINNS                       VALUE '  '.                  
051800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
051900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
052000     88  BASEN-SLUT                          VALUE 'GB'.                  
052100     SKIP2                                                                
052200 01  GODK-STATUSKODER.                                                    
052300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052400                                                                          
052500 01  SSA1                        PIC X(96).                               
052600 01  SSA2                        PIC X(64).                               
052700 01  SSA3                        PIC X(64).                               
052800 01  SSA4                        PIC X(64).                               
052900     EJECT                                                                
053000*    --- IMS FUNKTIONSKODER                                               
053100*01  -COPY W0003                                                          
053200     EJECT                                                                
053300*    ---  DLI INPUT-OUTPUT AREA                                           
053400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
053500     SKIP3                                                                
053600 01  DLI-IO-AREA-OBKR.                                                    
053700     03  WLORQM01.                                                        
053800*        05  -COPY WDQ101                                                 
053900     EJECT                                                                
054000 01  DLI-IO-AREA-OHUV.                                                    
054100     03  WLORQI01.                                                        
054200*        05  -COPY WDQ201                                                 
054300     EJECT                                                                
054400 01  DLI-IO-AREA-ARB.                                                     
054500     03  WLORQI12.                                                        
054600*        05  -COPY WDQ212                                                 
054700 01  DLI-IO-AREA-LOR.                                                     
054800     03  WLORQI21.                                                        
054900*        05  -COPY WDQ221                                                 
055000     EJECT                                                                
055100 01  DLI-IO-AREA-DLEV.                                                    
055200     03  WLORQI11.                                                        
055300*        05  -COPY WDQ211                                                 
055400     EJECT                                                                
055500 01  DLI-IO-AREA-ORAD.                                                    
055600     03  WLORQF01.                                                        
055700*        05  -COPY WDQ401                                                 
055800     EJECT                                                                
055900 01  DLI-IO-AREA-ART.                                                     
056000     03  WLARTM01.                                                        
056100*        05  -COPY WDK901                                                 
056200     EJECT                                                                
056300 01  DLI-IO-AREA-WDB101.                                                  
056400     03  WLBETC01.                                                        
056500*        05  -COPY WDB101                                                 
056600     EJECT                                                                
056700 01  DLI-IO-AREA-WDB201.                                                  
056800*    03  -COPY WDB201                                                     
056900                                                                          
057000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
057100 01   DLI-IO-AREA-B601.                                                   
057200*     03  -COPY WDB601                                                    
057300     EJECT                                                                
057400 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
057500 01   DLI-IO-AREA-R601.                                                   
057600*     03  -COPY WDR601                                                    
057700*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
057800     EJECT                                                                
057900                                                                          
058000                                                                          
058100*---MSG-AREA FÖR HOPP TILL 4203-SVARSBILDEN                               
058200                                                                          
058300 01  FILLER                  PIC X(16)    VALUE 'P-TO-P-SW'.              
058400 01  4203-MSG-IO-AREA.                                                    
058500     03  4203-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
058600     03  4203-Z1               PIC X.                                     
058700     03  4203-Z2               PIC X.                                     
058800     03  4203-TRANSKOD         PIC X(8)   VALUE 'W4T203  '.               
058900     03  4203-IDTRANS          PIC X(4)   VALUE '4244'.                   
059000     03  4203-SPRAK            PIC X.                                     
059100     03  4203-IDDISTR-IN       PIC X(4).                                  
059200     03  4203-IDKUNDNR-IN      PIC X(6).                                  
059300     03  4203-IDORDNR-IN       PIC X(5).                                  
059400     03  4203-IDDISTR-UT       PIC X(4).                                  
059500     03  4203-IDKUNDNR-UT      PIC X(6).                                  
059600     03  4203-IDORDNR-UT       PIC X(5).                                  
059700     EJECT                                                                
059800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
059900     SKIP3                                                                
060000 01  -COPY WZ01SEND                                                       
060100     EJECT                                                                
060200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
060300     SKIP3                                                                
060400 01  SEND-AREA.                                                           
060500*    03  -COPY WZ01REQU  -PRE 3039-                                       
060600*    03  -COPY W30391I1  -PRE 3039-                                       
060700     EJECT                                                                
060800 LINKAGE SECTION.                                                         
060900                                                                          
061000*01  -COPY W0009      -PRE MSG-                                           
061100                                                                          
061200 01  2109-PCB                    PIC X.                                   
061300 01  PRQRY-PCB                   PIC X.                                   
061400                                                                          
061500*01  -COPY W0009      -PRE 4203-                                          
061600*01  -COPY W0009      -PRE 4203V-                                         
061700     SKIP2                                                                
061800 01  AVSR-ALT2-PCB               PIC X.                                   
061900     EJECT                                                                
062000*01  -COPY W0008      -PRE USEA-                                          
062100     05  FILLER                  PIC X.                                   
062200     SKIP2                                                                
062300*01  -COPY W0008      -PRE ORQM-                                          
062400     05  FILLER                  PIC X.                                   
062500     SKIP2                                                                
062600*01  -COPY W0008      -PRE ORQI-                                          
062700     05  FILLER                  PIC X.                                   
062800     EJECT                                                                
062900*01  -COPY W0008      -PRE ORQF-                                          
063000     05  FILLER                  PIC X.                                   
063100     EJECT                                                                
063200*01  -COPY W0008      -PRE ARTM-                                          
063300     05  FILLER                  PIC X.                                   
063400     EJECT                                                                
063500*01  -COPY W0008      -PRE WDB2-                                          
063600     05  FILLER                  PIC X.                                   
063700     EJECT                                                                
063800*01  -COPY W0008      -PRE WDB1-                                          
063900     05  FILLER                  PIC X.                                   
064000     EJECT                                                                
064100*01  -COPY W0008      -PRE WDB6-                                          
064200     05  FILLER                  PIC X.                                   
064300     EJECT                                                                
064400*01  -COPY W0008      -PRE WDR6-                                          
064500     05  FILLER                  PIC X.                                   
064600 01  PRIS-ARTC-PCB               PIC X.                                   
064700 01  PRIS-WDK7-PCB               PIC X.                                   
064800 01  PRIS-GMTA-PCB               PIC X.                                   
064900 01  PRIS-BETA-PCB               PIC X.                                   
065000 01  PRIS-GPRIA-PCB              PIC X.                                   
065100 01  PRIS-GPRIB-PCB              PIC X.                                   
065200 01  PRIS-COST-WDK6-PCB          PIC X.                                   
065300 01  PRIS-COST-WDK7-PCB          PIC X.                                   
065400 01  PRIS-COST-WDF1-PCB          PIC X.                                   
065500 01  PRIS-COST-9305-PCB          PIC X.                                   
065600 01  PRIS-COST-WDK72-PCB         PIC X.                                   
065700 01  PRIS-COST-WDB6-PCB          PIC X.                                   
065800 01  PRNO-3107-PCB               PIC X.                                   
065900 01  PRQU-WDG2-PCB               PIC X.                                   
066000 01  PRQU-WDC7-PCB               PIC X.                                   
066100 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
066200 01  AREG-WDK6-PCB               PIC X.                                   
066300 01  AREG-WDK7-PCB               PIC X.                                   
066400 01  ARTM-ARTM-PCB               PIC X.                                   
066500 01  DLEV-LEVF-PCB               PIC X.                                   
066600 01  DLEV-LEVG-PCB               PIC X.                                   
066700 01  DLEV-LEVA-PCB               PIC X.                                   
066800 01  DLEV-ARTS-PCB               PIC X.                                   
066900 01  DLEV-WDB6-PCB               PIC X.                                   
067000 01  SPAR-WDF8-PCB               PIC X.                                   
067100 01  SPAR-WDF8A-PCB              PIC X.                                   
067200 01  SPAR-WDK6-PCB               PIC X.                                   
067300 01  KAMP-ORDP-PCB               PIC X.                                   
067400 01  KAMP-ZZAC-PCB               PIC X.                                   
067500 01  KAMP-WDM2-PCB               PIC X.                                   
067600 01  KERS-ARTC-PCB               PIC X.                                   
067700 01  KERS-ERSA-PCB               PIC X.                                   
067800 01  NDCA-USEA-PCB               PIC X.                                   
067900 01  NDCA-WDK7-PCB               PIC X.                                   
068000 01  NDCA-WDL6-PCB               PIC X.                                   
068100 01  NDCA-WDB6-PCB               PIC X.                                   
068200 01  SDCA-ARTS-PCB               PIC X.                                   
068300 01  SDCA-WDB6-PCB               PIC X.                                   
068400 01  SDCA-WDK9-PCB               PIC X.                                   
068500 01  SDCA-WDR6-PCB               PIC X.                                   
068600 01  SDCA-WDK6-PCB               PIC X.                                   
068700 01  SDCA-WDQ4B-PCB              PIC X.                                   
068800 01  SDCA-WDQ2-PCB               PIC X.                                   
068900 01  SDCA-WDQ4-PCB               PIC X.                                   
069000 01  SDCA-WDB6-2-PCB             PIC X.                                   
069100 01  SDCA-WDK6-2-PCB             PIC X.                                   
069200 01  SDCA-WDK7-2-PCB             PIC X.                                   
069300 01  SDCA-WDK7-3-PCB             PIC X.                                   
069400 01  CDCA-ARTM-PCB               PIC X.                                   
069500 01  CDCA-INLB-PCB               PIC X.                                   
069600 01  CDCA-WDB2-PCB               PIC X.                                   
069700 01  CDCA-WDC1-PCB               PIC X.                                   
069800 01  RANS-XXKM-PCB               PIC X.                                   
069900 01  RANS-ARTM-PCB               PIC X.                                   
070000 01  RANS-ARTS-PCB               PIC X.                                   
070100     EJECT                                                                
070200 01  TPO1-ORDP-PCB               PIC X.                                   
070300 01  TPO1-ARTM-PCB               PIC X.                                   
070400 01  TPO1-ZZAC-PCB               PIC X.                                   
070500 01  TPO2-ORDP-PCB               PIC X.                                   
070600 01  TPO2-XXBU-PCB               PIC X.                                   
070700 01  TPO2-XXBV-PCB               PIC X.                                   
070800 01  TPO2-ARTM-PCB               PIC X.                                   
070900 01  TPO2-FILA-PCB               PIC X.                                   
071000 01  TPO2-XXBX-PCB               PIC X.                                   
071100 01  RELS-ORDP-PCB               PIC X.                                   
071200 01  RELS-FILA-PCB               PIC X.                                   
071300 01  RELS-ARTM-PCB               PIC X.                                   
071400 01  TIME-4437-PCB               PIC X.                                   
071500 01  AVSR-ORQI-PCB               PIC X.                                   
071600 01  AVSR-GMTB-PCB               PIC X.                                   
071700 01  AVSR-GMTC-PCB               PIC X.                                   
071800 01  AVSR-WDB2-PCB               PIC X.                                   
071900 01  AVSR-WDB6-PCB               PIC X.                                   
072000 01  TRAN-XXKB-PCB               PIC X.                                   
072100 01  KVAN-WDB2-PCB               PIC X.                                   
072200 01  KVAN-WDC1-PCB               PIC X.                                   
072300 01  XDCA-USEA-PCB               PIC X.                                   
072400 01  XDCA-WDB6-PCB               PIC X.                                   
072500 01  XDCA-WDK6-PCB               PIC X.                                   
072600 01  XDCA-WDK7-PCB               PIC X.                                   
072700 01  XDCA-WDK9-PCB               PIC X.                                   
072800 01  XDCA-WDL6-PCB               PIC X.                                   
072900 01  XDCA-WDQ4B-PCB              PIC X.                                   
073000 01  XDCA-WDQ2-PCB               PIC X.                                   
073100 01  XDCA-WDQ4-PCB               PIC X.                                   
073200 01  XDCA-WDR6-PCB               PIC X.                                   
073300 01  XDCA-WDB6-2-PCB             PIC X.                                   
073400 01  XDCA-WDK6-2-PCB             PIC X.                                   
073500 01  XDCA-WDK7-2-PCB             PIC X.                                   
073600 01  XDCA-WDK7-3-PCB             PIC X.                                   
073700     EJECT                                                                
073800 PROCEDURE DIVISION  USING MSG-PCB 2109-PCB                               
073900        PRQRY-PCB     4203-PCB 4203V-PCB                                  
074000        AVSR-ALT2-PCB USEA-PCB ORQM-PCB ORQI-PCB                          
074100        ORQF-PCB ARTM-PCB  WDB2-PCB WDB1-PCB WDB6-PCB                     
074200        WDR6-PCB                                                          
074300        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
074400        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
074500        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
074600        PRIS-COST-WDK6-PCB                                                
074700        PRIS-COST-WDK7-PCB                                                
074800        PRIS-COST-WDF1-PCB                                                
074900        PRIS-COST-9305-PCB                                                
075000        PRIS-COST-WDK72-PCB                                               
075100        PRIS-COST-WDB6-PCB                                                
075200        PRNO-3107-PCB                                                     
075300        PRQU-WDG2-PCB                                                     
075400        PRQU-WDC7-PCB                                                     
075500        PRQU-SJKO-WDK6-PCB                                                
075600        AREG-WDK6-PCB                                                     
075700        AREG-WDK7-PCB                                                     
075800        ARTM-ARTM-PCB                                                     
075900        DLEV-LEVF-PCB                                                     
076000        DLEV-LEVG-PCB                                                     
076100        DLEV-LEVA-PCB                                                     
076200        DLEV-ARTS-PCB                                                     
076300        DLEV-WDB6-PCB                                                     
076400        SPAR-WDF8-PCB                                                     
076500        SPAR-WDF8A-PCB                                                    
076600        SPAR-WDK6-PCB                                                     
076700        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
076800        KERS-ARTC-PCB KERS-ERSA-PCB                                       
076900        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
077000        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
077100        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
077200        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
077300        SDCA-WDK7-3-PCB                                                   
077400        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
077500        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
077600        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
077700        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
077800        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
077900        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
078000        TIME-4437-PCB                                                     
078100        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
078200        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
078300        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
078400        XDCA-USEA-PCB                                                     
078500        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
078600        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
078700        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
078800        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
078900        XDCA-WDK7-3-PCB.                                                  
079000     EJECT                                                                
079100                                                                          
079200     ENTRY 'DLITCBL' USING MSG-PCB 2109-PCB                               
079300        PRQRY-PCB     4203-PCB 4203V-PCB                                  
079400        AVSR-ALT2-PCB USEA-PCB ORQM-PCB ORQI-PCB                          
079500        ORQF-PCB ARTM-PCB  WDB2-PCB WDB1-PCB WDB6-PCB                     
079600        WDR6-PCB                                                          
079700        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
079800        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
079900        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
080000        PRIS-COST-WDK6-PCB                                                
080100        PRIS-COST-WDK7-PCB                                                
080200        PRIS-COST-WDF1-PCB                                                
080300        PRIS-COST-9305-PCB                                                
080400        PRIS-COST-WDK72-PCB                                               
080500        PRIS-COST-WDB6-PCB                                                
080600        PRNO-3107-PCB                                                     
080700        PRQU-WDG2-PCB                                                     
080800        PRQU-WDC7-PCB                                                     
080900        PRQU-SJKO-WDK6-PCB                                                
081000        AREG-WDK6-PCB                                                     
081100        AREG-WDK7-PCB                                                     
081200        ARTM-ARTM-PCB                                                     
081300        DLEV-LEVF-PCB                                                     
081400        DLEV-LEVG-PCB                                                     
081500        DLEV-LEVA-PCB                                                     
081600        DLEV-ARTS-PCB                                                     
081700        DLEV-WDB6-PCB                                                     
081800        SPAR-WDF8-PCB                                                     
081900        SPAR-WDF8A-PCB                                                    
082000        SPAR-WDK6-PCB                                                     
082100        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
082200        KERS-ARTC-PCB KERS-ERSA-PCB                                       
082300        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
082400        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
082500        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB  SDCA-WDQ4-PCB         
082600        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
082700        SDCA-WDK7-3-PCB                                                   
082800        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
082900        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
083000        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
083100        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
083200        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
083300        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
083400        TIME-4437-PCB                                                     
083500        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
083600        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
083700        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
083800        XDCA-USEA-PCB                                                     
083900        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
084000        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
084100        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
084200        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
084300        XDCA-WDK7-3-PCB.                                                  
084400     EJECT                                                                
084500                                                                          
084700     PERFORM IMS-GET-MSG                                                  
084800     IF SEGMENT-FINNS                                                     
084900        PERFORM A-INIT                                                    
085000        IF ALLT-OK                                                        
085100          PERFORM B-KOLLA-NYCKLAR                                         
085200        END-IF                                                            
085300        IF ALLT-OK                                                        
085400          PERFORM H-KONTROLLA-BEHORIGHET                                  
085500        END-IF                                                            
085600        IF ALLT-OK                                                        
085700           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
085800           IF ALLT-OK                                                     
085900              PERFORM D-FORMELL-KONTROLL                                  
086000              IF ALLT-OK                                                  
086100                 PERFORM E-BEHANDLA-RADER                                 
086200              END-IF                                                      
086300           END-IF                                                         
086400        END-IF                                                            
086500        IF ALLT-OK                                                        
086600           IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+' OR                  
086700                                              SVARSBILD                   
086800              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
086900                 PERFORM I-SKICKA-PRISFRAGA                               
087000              END-IF                                                      
087100              PERFORM F-HOPPA-TILL-SVARSBILD                              
087200           ELSE                                                           
087300              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
087400                PERFORM I-SKICKA-PRISFRAGA                                
087500              END-IF                                                      
087600              PERFORM G-VISA-TOM-SIDA                                     
087700           END-IF                                                         
087800        END-IF                                                            
087900        IF HOPP-TILL-4203 = NEJ                                           
088000           PERFORM Z-FINIT-INSERT-MSG                                     
088100        END-IF                                                            
088200     END-IF                                                               
088300     MOVE +0 TO RETURN-CODE                                               
088400     GOBACK                                                               
088500     .                                                                    
088600     EJECT                                                                
088700 A-INIT SECTION.                                                          
088800     MOVE JA                   TO ALLT-SW                                 
088900     MOVE SPACE                TO MED-IDMFSFEL                            
089000                                                                          
089100     IF MSG-DUBBLA-TRANSKODER                                             
089200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I24401                 
089300       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
089400       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
089500     ELSE                                                                 
089600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I24401                  
089700       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
089800       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
089900     END-IF                                                               
090000                                                                          
090100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
090200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
090300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
090400     IF W-IDTRANS = '4203' AND MSG-KDTRANS-1  = 'W4T244U '                
090500        MOVE JA TO SVARSBILD-SW                                           
090600     END-IF                                                               
090700     MOVE LOW-VALUE            TO MSG-AREA                                
090800     MOVE 'W4O24401'           TO MFS-IDMOD                               
090900     MOVE '4244'               TO MOD-IDTRANS                             
091000     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
091100                                  MOD-TEMFSINF                            
091200     IF NOT EGEN-MID AND NOT SVARSBILD                                    
091300       MOVE SPACE              TO MFS-KDTRTYP                             
091400       MOVE '7'                TO MFS-IDPFK                               
091500     END-IF                                                               
091600     IF ENGLISH-TEXT                                                      
091700       MOVE +2                 TO SPRAK-IX                                
091800       MOVE 'GB '              TO MED-IDSKYLT                             
091900     ELSE                                                                 
092000       MOVE +1                 TO SPRAK-IX                                
092100       MOVE 'S  '              TO MED-IDSKYLT                             
092200     END-IF                                                               
092300                                                                          
092400     PERFORM AA-NOLLA-WOPS-TABELL                                         
092500                                                                          
092600     .                                                                    
092700     EJECT                                                                
092800 AA-NOLLA-WOPS-TABELL SECTION.                                            
092900                                                                          
093000     MOVE +1                   TO WS-INDEX-WOPS                           
093100     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
093200        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
093300        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
093400        MOVE ZERO              TO AVSR-IDDC(WS-INDEX-WOPS)                
093500        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
093600        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
093700        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
093800        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
093900        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
094000        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
094100        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
094200        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
094300        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
094400        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
094500        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
094600        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
094700        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
094800                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
094900        ADD +1                 TO WS-INDEX-WOPS                           
095000     END-PERFORM                                                          
095100                                                                          
095200     MOVE +1                   TO WS-INDEX-WOPS                           
095300     .                                                                    
095400     EJECT                                                                
095500 B-KOLLA-NYCKLAR SECTION.                                                 
095600                                                                          
095700     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
095800                                  MOD-IDKUNDNR-IN                         
095900                                  MOD-IDORDNR-IN                          
096000                                                                          
096100     IF MID-IDDISTR-IN NOT = ALL '+'                                      
096200        MOVE MID-IDDISTR-IN    TO WS-IDDISTR                              
096300        MOVE SPACE             TO MFS-KDTRTYP                             
096400        MOVE '7'               TO MFS-IDPFK                               
096500     ELSE                                                                 
096600       MOVE MID-IDDISTR-UT     TO WS-IDDISTR                              
096700       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
096800     END-IF                                                               
096900                                                                          
097000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
097100        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
097200        MOVE SPACE             TO MFS-KDTRTYP                             
097300        MOVE '7'               TO MFS-IDPFK                               
097400     ELSE                                                                 
097500        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
097600        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
097700     END-IF                                                               
097800                                                                          
097900     IF MID-IDORDNR-IN NOT = ALL '+'                                      
098000        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
098100        MOVE SPACE             TO MFS-KDTRTYP                             
098200        MOVE '7'               TO MFS-IDPFK                               
098300     ELSE                                                                 
098400        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
098500        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
098600     END-IF                                                               
098700                                                                          
098800                                                                          
098900     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
099000        MOVE WS-IDDISTR           TO W-IDDISTR                            
099100     ELSE                                                                 
099200        MOVE NEJ                  TO ALLT-SW                              
099300        MOVE ZERO                 TO WS-IDDISTR                           
099400     END-IF                                                               
099500                                                                          
099600     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
099700     IF DIST79-DEALER-PRICE                                               
099800        IF ENGLISH-TEXT                                                   
099900           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
100000        ELSE                                                              
100100           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
100200        END-IF                                                            
100300     ELSE                                                                 
100400        MOVE SPACE                TO MOD-TEDDI                            
100500     END-IF                                                               
100600                                                                          
100700     IF WS-IDKUNDNR NUMERIC                                               
100800        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
100900     ELSE                                                                 
101000        MOVE NEJ                  TO ALLT-SW                              
101100     END-IF                                                               
101200                                                                          
101300     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
101400        MOVE WS-IDORDNR           TO WS-NUM-7                             
101500        MOVE WS-NUM-7             TO W-IDKUNDRF                           
101600     ELSE                                                                 
101700        MOVE NEJ                  TO ALLT-SW                              
101800     END-IF                                                               
101900                                                                          
102000     IF NOT ALLT-OK                                                       
102100       IF SVARSBILD                                                       
102200         MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4203'           
102300                              TO FELTEXT                                  
102400         CALL ABEND USING RKOD-ABEND                                      
102500       END-IF                                                             
102600       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
102700     END-IF                                                               
102800                                                                          
102900     IF GODK-MID OR ALLT-OK                                               
103000       MOVE WS-IDDISTR              TO MOD-IDDISTR-UT                     
103100       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
103200                                                                          
103300       IF WS-IDKUNDNR = ZERO                                              
103400         MOVE '     0'              TO MOD-IDKUNDNR-UT                    
103500       ELSE                                                               
103600         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR-UT                    
103700         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
103800       END-IF                                                             
103900                                                                          
104000       MOVE WS-IDORDNR              TO MOD-IDORDNR-UT                     
104100       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
104200                                                                          
104300       IF NOT ALLT-OK                                                     
104400          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                     
104500                                       MOD-IDKUNDNR-UT                    
104600                                       MOD-IDORDNR-UT                     
104700       END-IF                                                             
104800                                                                          
104900     ELSE                                                                 
105000       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-UT                     
105100                                       MOD-IDKUNDNR-UT                    
105200                                       MOD-IDORDNR-UT                     
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
105700                                                                          
105800     PERFORM IMS-01-GU-ORQI-WDQ201                                        
106100     IF SEGMENT-FINNS                                                     
106200       PERFORM CA-KDORDSTA-VALIDATE                                       
106300       PERFORM CB-HAMTA-KUND                                              
106400       PERFORM CC-HAMTA-WDB6-INFO                                         
106500       IF (OHUV-KDORDKL = 1 AND                                           
106600           GMT-FLORDTIL-KL1 = JA)                                         
106700       OR                                                                 
106800          (OHUV-KDORDKL = 2 AND                                           
106900           GMT-FLORDTIL-KL2 = JA)                                         
107000       OR                                                                 
107100          (OHUV-KDORDKL = 3 AND                                           
107200           GMT-FLORDTIL-KL3 = JA)                                         
107300       OR                                                                 
107400          (OHUV-KDORDKL = 4 AND                                           
107500           GMT-FLORDTIL-KL4 = JA)                                         
107600           MOVE NEJ             TO ALLT-SW                                
107700           MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                           
107800       END-IF                                                             
107900       IF ALLT-OK                                                         
108000         IF OHUV-FLBORT = NEJ                                             
108100                                                                          
108200           IF OHUV-IDDC-TVS > ZERO                                        
108300             MOVE OHUV-IDDC-TVS       TO WS-IDDC                          
108400           ELSE                                                           
108500             MOVE OHUV-IDDC-PRIM      TO WS-IDDC                          
108600           END-IF                                                         
108900           IF OHUV-IDSYSTEM NOT = '4211' AND '4221' AND '4231'            
109000                                   AND '4202'                             
109100             IF (OHUV-FLKLAR = NEJ AND NOT SVARSBILD AND                  
109200                (OHUV-IDSYSTEM NOT = '4202' AND '4244')) OR               
109300                (SW-KDORDSTA-ALL-E-FLAG = JA AND                          
109400                 SW-KDORDSTA-O-ALL-SPACE-FLAG = JA )                      
109500                 MOVE ERR-ORDER-EJ-AVSLUT TO MED-IDMFSFEL                 
109600                 MOVE NEJ               TO ALLT-SW                        
109700             ELSE                                                         
109800               IF OHUV-FLFORBI = JA OR                                    
109900                  OHUV-FLFORBI = SPEC-FORBI OR                            
110000                  OHUV-FLORDSPE = JA OR                                   
110100                  OHUV-KDTPOTYP = +3 OR OHUV-KDORDKL = +0 OR              
110200                  OHUV-FLOVRLEV = JA OR OHUV-IDSYSTEM = 'LDC ' OR         
110300                 (OHUV-IDSYSTEM (1:3) = 'LYN' OR 'ECO' OR 'VOU' OR        
110400                                        'TAD' OR 'ACC' OR                 
110500                                        'APA' OR 'APB' OR                 
110600                                        'APC' OR 'APD' OR                 
110700                                        'APE' OR 'APF' OR                 
110800                                        'APG' OR 'APH' OR                 
110900                                        'API' OR 'APJ')                   
111000                  MOVE NEJ            TO ALLT-SW                          
111100                  MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                    
111200               ELSE                                                       
111300                  MOVE OHUV-KDORDKL   TO MOD-KDORDKL-UT                   
111400                  PERFORM S10-HAMTA-WDB6-INFO                             
111500                  IF DCS-CDC                                              
111600                    PERFORM CD-KONTROLLERA-STATUS-CDC                     
111700                  ELSE                                                    
111800                    IF DCS-NDC AND NOT DCS-CHINA                          
111900                       PERFORM CE-KONTROLLERA-STATUS-NDC                  
112000                    ELSE                                                  
112100*                      IF OHUV-IDDC-CLEAR(3) NOT = SPACE                  
112200*                         PERFORM CF-KONTROLLERA-STATUS-LDC               
112300*                      ELSE                                               
112400*                        PERFORM CB-KONTROLLERA-STATUS-SDC                
112500*                      END-IF                                             
112600                       PERFORM CF-KONTROLLERA-STATUS-EJ-CDC               
112700                    END-IF                                                
112800                  END-IF                                                  
112900                                                                          
113000                  PERFORM CG-LAS-ARBETSTABELLER                           
113100                  PERFORM CH-FIXA-LOKAL-TID                               
113200               END-IF                                                     
113300             END-IF                                                       
113400           ELSE                                                           
113500             MOVE ERR-EJ-TILLAEGG      TO MED-IDMFSFEL                    
113600             MOVE NEJ                  TO ALLT-SW                         
113700           END-IF                                                         
113800         ELSE                                                             
113900           MOVE ERR-ORDER-ANNULL       TO MED-IDMFSFEL                    
114000           MOVE NEJ                    TO ALLT-SW                         
114100         END-IF                                                           
114200       END-IF                                                             
114300     ELSE                                                                 
114400        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
114500        MOVE NEJ                     TO ALLT-SW                           
114600     END-IF                                                               
114700                                                                          
114800     IF MFS-FIRST AND ALLT-OK                                             
114900        MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-ATTR(1)                  
115000        PERFORM MFS-RENSA-MOD-RADER                                       
115100        MOVE NEJ                    TO ALLT-SW                            
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 CA-KDORDSTA-VALIDATE  SECTION.                                           
115600                                                                          
115700     MOVE JA TO SW-KDORDSTA-ALL-E-FLAG                                    
115800     MOVE JA TO SW-KDORDSTA-O-ALL-SPACE-FLAG                              
115900     MOVE JA TO SW-KDORDSTA-O-STATUS-FLAG                                 
116000     MOVE JA TO SW-KDORDSTA-STATUS-FLAG                                   
116100                                                                          
116200     PERFORM IMS-GNP-ORQI-WDQ212                                          
116300                                                                          
116400     PERFORM UNTIL SEGMENT-SAKNAS                                         
116500        IF ARB-IDDC = OHUV-IDDC-PRIM                                      
116600           MOVE ARB-KDORDSTA   TO WS-ARB-KDORDSTA-PRIM                    
116700           MOVE ARB-KDORDSTA-O TO WS-ARB-KDORDSTA-O-PRIM                  
116800        END-IF                                                            
116900                                                                          
117000        IF ARB-KDORDSTA NOT = 'E'                                         
117100           MOVE NEJ TO SW-KDORDSTA-ALL-E-FLAG                             
117200        END-IF                                                            
117300        IF ARB-KDORDSTA-O NOT = SPACE                                     
117400           MOVE NEJ TO SW-KDORDSTA-O-ALL-SPACE-FLAG                       
117500        END-IF                                                            
117600        IF ARB-KDORDSTA-O NOT = ' ' AND                                   
117700           ARB-KDORDSTA-O NOT = 'B' AND                                   
117800           ARB-KDORDSTA-O NOT = 'C' AND                                   
117900           ARB-KDORDSTA-O NOT = 'R'                                       
118000           MOVE NEJ TO SW-KDORDSTA-O-STATUS-FLAG                          
118100        END-IF                                                            
118200        IF ARB-KDORDSTA   NOT = ' ' AND                                   
118300           ARB-KDORDSTA   NOT = 'B' AND                                   
118400           ARB-KDORDSTA   NOT = 'C' AND                                   
118500           ARB-KDORDSTA   NOT = 'R'                                       
118600           MOVE NEJ TO SW-KDORDSTA-STATUS-FLAG                            
118700        END-IF                                                            
118800                                                                          
118900        PERFORM IMS-GNP-ORQI-WDQ212                                       
119000     END-PERFORM                                                          
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000 CB-HAMTA-KUND       SECTION.                                             
120100     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
120200     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
120300     PERFORM IMS-GU-WDB201                                                
120400                                                                          
120500     IF OHUV-KDORDKL > 1                                                  
120600                                                                          
120700        MOVE +1 TO WS-INDEX                                               
120800        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
120900           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
121000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
121100           ADD +1 TO WS-INDEX                                             
121200        END-PERFORM                                                       
121300                                                                          
121400     ELSE                                                                 
121500       IF OHUV-KDORDKL = 1                                                
121600                                                                          
121700          MOVE +1 TO WS-INDEX                                             
121800          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
121900             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
122000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
122100             ADD +1 TO WS-INDEX                                           
122200          END-PERFORM                                                     
122300                                                                          
122400       ELSE                                                               
122500         IF OHUV-KDORDKL = 0                                              
122600                                                                          
122700            MOVE +1 TO WS-INDEX                                           
122800            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
122900               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
123000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
123100               ADD +1 TO WS-INDEX                                         
123200            END-PERFORM                                                   
123300                                                                          
123400         END-IF                                                           
123500       END-IF                                                             
123600     END-IF                                                               
124500     .                                                                    
124600     EJECT                                                                
124700                                                                          
124800 CC-HAMTA-WDB6-INFO SECTION.                                              
124900                                                                          
125000     MOVE SPACE                TO CLDC-W411CLDC                           
125100     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
125200                                                                          
125300     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
125400     .                                                                    
125500     EJECT                                                                
125600 CD-KONTROLLERA-STATUS-CDC SECTION.                                       
125700                                                                          
125800     IF WS-ARB-KDORDSTA-O-PRIM NOT = SPACE                                
125900       IF WS-ARB-KDORDSTA-O-PRIM                                          
126000                             = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
126100                          OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
126200                          OR 'S*' OR 'S' OR 'SF'                          
126300          MOVE JA                        TO TILLAEGG-TPO-SW               
126400                                            RAD-GODKAND-SW                
126500       ELSE                                                               
126600          IF (WS-ARB-KDORDSTA-O-PRIM = 'B' OR 'C' OR 'R')                 
126700             MOVE JA                     TO VANLIGA-RADER-C1-SW           
126800                                            RAD-GODKAND-SW                
126900          ELSE                                                            
127000             IF WS-ARB-KDORDSTA-O-PRIM  = 'R*'                            
127100                MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
127200                                            RAD-GODKAND-SW                
127300             ELSE                                                         
127400                MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
127500                MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
127600                MOVE NEJ                 TO TILLAEGG-TPO-SW               
127700             END-IF                                                       
127800          END-IF                                                          
127900       END-IF                                                             
128000     ELSE                                                                 
128100       IF WS-ARB-KDORDSTA-PRIM                                            
128200                           = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
128300                      'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
128400                       OR 'S*' OR 'S' OR 'SF'                             
128500          MOVE JA                        TO TILLAEGG-TPO-SW               
128600                                            RAD-GODKAND-SW                
128700       ELSE                                                               
128800          IF WS-ARB-KDORDSTA-PRIM = 'B' OR 'C' OR 'R'                     
128900             MOVE JA                     TO VANLIGA-RADER-C1-SW           
129000                                            RAD-GODKAND-SW                
129100          ELSE                                                            
129200             IF WS-ARB-KDORDSTA-PRIM = 'R*'                               
129300                MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
129400                                            RAD-GODKAND-SW                
129500             ELSE                                                         
129600                MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
129700                MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
129800                MOVE NEJ                 TO TILLAEGG-TPO-SW               
129900             END-IF                                                       
130000          END-IF                                                          
130100       END-IF                                                             
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500*                                                                         
130600*CB-KONTROLLERA-STATUS-SDC SECTION.                                       
130700*                                                                         
130800*    IF OHUV-KDORDSTA-O(2) NOT = SPACE                                    
130900*      IF OHUV-KDORDSTA-O(1) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
131000*                         OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
131100*                         OR 'S*' OR 'S' OR 'SF'                          
131200*         MOVE JA                        TO TILLAEGG-TPO-SW               
131300*                                           RAD-GODKAND-SW                
131400*      ELSE                                                               
131500*         IF (OHUV-KDORDSTA-O(2) = 'B' OR 'C' OR 'R')                     
131600*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
131700*                                           RAD-GODKAND-SW                
131800*         ELSE                                                            
131900*            IF OHUV-KDORDSTA-O(2) = 'R*'                                 
132000*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
132100*                                           RAD-GODKAND-SW                
132200*            ELSE                                                         
132300*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
132400*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
132500*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
132600*            END-IF                                                       
132700*         END-IF                                                          
132800*      END-IF                                                             
132900*      IF OHUV-KDORDSTA-O(1) = 'B' OR 'C' OR 'R'                          
133000*         MOVE JA                     TO RAD-GODKAND-SW                   
133100*         MOVE JA                     TO TILLAEGG-SDC-SW                  
133200*      ELSE                                                               
133300*         MOVE NEJ                    TO TILLAEGG-SDC-SW                  
133400*      END-IF                                                             
133500*    ELSE                                                                 
133600*      IF OHUV-KDORDSTA(2) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
133700*                     'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
133800*                     OR 'S*' OR 'S' OR 'SF'                              
133900*         MOVE JA                        TO TILLAEGG-TPO-SW               
134000*                                           RAD-GODKAND-SW                
134100*      ELSE                                                               
134200*         IF (OHUV-KDORDSTA(2) = 'B' OR 'C' OR 'R')                       
134300*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
134400*                                           RAD-GODKAND-SW                
134500*         ELSE                                                            
134600*            IF OHUV-KDORDSTA(2) = 'R*'                                   
134700*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
134800*                                           RAD-GODKAND-SW                
134900*            ELSE                                                         
135000*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
135100*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
135200*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
135300*            END-IF                                                       
135400*         END-IF                                                          
135500*      END-IF                                                             
135600*      IF (OHUV-KDORDSTA(1) = 'B' OR 'C' OR 'R')                          
135700*         MOVE JA                     TO RAD-GODKAND-SW                   
135800*         MOVE JA                     TO TILLAEGG-SDC-SW                  
135900*      ELSE                                                               
136000*         MOVE NEJ                    TO TILLAEGG-SDC-SW                  
136100*      END-IF                                                             
136200*    END-IF                                                               
136300*    .                                                                    
136400*    EJECT                                                                
136500*CF-KONTROLLERA-STATUS-LDC SECTION.                                       
136600*                                                                         
136700*    IF OHUV-KDORDSTA-O(3) NOT = SPACE                                    
136800*      IF OHUV-KDORDSTA-O(3) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L'          
136900*                         OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
137000*                         OR 'S*' OR 'S' OR 'SF'                          
137100*         MOVE JA                        TO TILLAEGG-TPO-SW               
137200*                                           RAD-GODKAND-SW                
137300*      ELSE                                                               
137400*         IF (OHUV-KDORDSTA-O(3) = 'B' OR 'C' OR 'R')                     
137500*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
137600*                                           RAD-GODKAND-SW                
137700*         ELSE                                                            
137800*            IF OHUV-KDORDSTA-O(3) = 'R*'                                 
137900*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
138000*                                           RAD-GODKAND-SW                
138100*            ELSE                                                         
138200*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
138300*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
138400*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
138500*            END-IF                                                       
138600*         END-IF                                                          
138700*      END-IF                                                             
138800*      IF  (OHUV-KDORDSTA-O(1) = 'B' OR 'C' OR 'R')                       
138900*      AND (OHUV-KDORDSTA-O(2) = 'B' OR 'C' OR 'R')                       
139000*         MOVE JA                     TO RAD-GODKAND-SW                   
139100*         MOVE JA                     TO TILLAEGG-SDC-SW                  
139200*         MOVE JA                     TO TILLAEGG-LDC-SW                  
139300*      ELSE                                                               
139400*         MOVE NEJ                    TO TILLAEGG-SDC-SW                  
139500*         MOVE NEJ                    TO TILLAEGG-LDC-SW                  
139600*      END-IF                                                             
139700*    ELSE                                                                 
139800*      IF OHUV-KDORDSTA(3) = 'U' OR 'U*' OR 'P' OR 'P*' OR 'L' OR         
139900*                     'E' OR 'L*' OR 'F' OR 'F*' OR 'FL' OR 'LF'          
140000*                         OR 'S*' OR 'S' OR 'SF'                          
140100*         MOVE JA                        TO TILLAEGG-TPO-SW               
140200*                                           RAD-GODKAND-SW                
140300*      ELSE                                                               
140400*         IF (OHUV-KDORDSTA(3) = 'B' OR 'C' OR 'R')                       
140500*            MOVE JA                     TO VANLIGA-RADER-C1-SW           
140600*                                           RAD-GODKAND-SW                
140700*         ELSE                                                            
140800*            IF OHUV-KDORDSTA(3) = 'R*'                                   
140900*               MOVE JA                  TO KOLLA-ARBTAB-C1-SW            
141000*                                           RAD-GODKAND-SW                
141100*            ELSE                                                         
141200*               MOVE NEJ                 TO KOLLA-ARBTAB-C1-SW            
141300*               MOVE NEJ                 TO VANLIGA-RADER-C1-SW           
141400*               MOVE NEJ                 TO TILLAEGG-TPO-SW               
141500*            END-IF                                                       
141600*         END-IF                                                          
141700*      END-IF                                                             
141800*      IF  (OHUV-KDORDSTA(1) = 'B' OR 'C' OR 'R')                         
141900*      AND (OHUV-KDORDSTA(2) = 'B' OR 'C' OR 'R')                         
142000*         MOVE JA                     TO RAD-GODKAND-SW                   
142100*         MOVE JA                     TO TILLAEGG-SDC-SW                  
142200*         MOVE JA                     TO TILLAEGG-LDC-SW                  
142300*      ELSE                                                               
142400*         MOVE NEJ                    TO TILLAEGG-SDC-SW                  
142500*         MOVE NEJ                    TO TILLAEGG-LDC-SW                  
142600*      END-IF                                                             
142700*    END-IF                                                               
142800*    .                                                                    
142900*    EJECT                                                                
143000 CE-KONTROLLERA-STATUS-NDC SECTION.                                       
143100                                                                          
143200     IF WS-ARB-KDORDSTA-O-PRIM NOT = SPACE                                
143300       IF WS-ARB-KDORDSTA-O-PRIM = 'R'                                    
143400          MOVE JA                        TO RAD-GODKAND-SW                
143500       END-IF                                                             
143600     ELSE                                                                 
143700       IF WS-ARB-KDORDSTA-PRIM   = 'R'                                    
143800          MOVE JA                        TO RAD-GODKAND-SW                
143900       END-IF                                                             
144000     END-IF                                                               
144100     .                                                                    
144200     EJECT                                                                
144300                                                                          
144400 CF-KONTROLLERA-STATUS-EJ-CDC SECTION.                                    
144500                                                                          
144600     IF   SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                              
144700                                                                          
144800          IF SW-KDORDSTA-O-STATUS-FLAG = JA                               
144900                                                                          
145000               MOVE JA    TO VANLIGA-RADER-C1-SW                          
145100                             RAD-GODKAND-SW                               
145200          ELSE                                                            
145300             IF NOT DCS-CHINA                                             
145400                MOVE JA   TO TILLAEGG-TPO-SW                              
145500                             RAD-GODKAND-SW                               
145600             ELSE                                                         
145700                MOVE NEJ  TO TILLAEGG-TPO-SW                              
145800                             RAD-GODKAND-SW                               
145900             END-IF                                                       
146000          END-IF                                                          
146100     ELSE                                                                 
146200          IF  SW-KDORDSTA-STATUS-FLAG = JA                                
146300                                                                          
146400                  MOVE JA TO VANLIGA-RADER-C1-SW                          
146500                             RAD-GODKAND-SW                               
146600          ELSE                                                            
146700             IF NOT DCS-CHINA                                             
146800                MOVE JA   TO TILLAEGG-TPO-SW                              
146900                             RAD-GODKAND-SW                               
147000             ELSE                                                         
147100                MOVE NEJ  TO TILLAEGG-TPO-SW                              
147200                             RAD-GODKAND-SW                               
147300             END-IF                                                       
147400          END-IF                                                          
147500     END-IF                                                               
147600     .                                                                    
147700     EJECT                                                                
147800 CG-LAS-ARBETSTABELLER SECTION.                                           
147900                                                                          
148000     MOVE WS-IDDC         TO W-IDDC-WDQ212                                
148100     PERFORM IMS-GNP-ORQI-WDQ212-FIRST                                    
148200     IF SEGMENT-FINNS                                                     
148300       IF ARB-IDDC = OHUV-IDDC-PRIM                                       
148400          MOVE ARB-KDFRAKT         TO MOD-KDFRAKT-UT                      
148500       END-IF                                                             
148600     ELSE                                                                 
148700       MOVE NEJ             TO ALLT-SW                                    
148800       MOVE ERR-ORDERLINES-MISSING TO MED-IDMFSFEL                        
148900     END-IF                                                               
149000     .                                                                    
149100     EJECT                                                                
149200                                                                          
149300 CH-FIXA-LOKAL-TID SECTION.                                               
149400                                                                          
149500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
149600     MOVE '013'             TO MSGI-KDCALL                                
149700     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
149800     MOVE WS-IDDC           TO MSGI-IDUSER(6:2)                           
149900                                                                          
150000     MOVE '4244'            TO MSGI-IDTRANS                               
150100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
150200                                                                          
150300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
150400     .                                                                    
150500     EJECT                                                                
150600                                                                          
150700 D-FORMELL-KONTROLL SECTION.                                              
150800                                                                          
150900     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
151000     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
151100     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
151200     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
151300     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
151400     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
151500     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
151600     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
151700     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
151800     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
151900     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
152000     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
152100     MOVE +1                   TO WS-INDEX-MID                            
152200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
152300        MOVE NEJ               TO ORFK-FLINVEST(WS-INDEX-MID)             
152400        IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                            
152500           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN(WS-INDEX-MID)              
152600        ELSE                                                              
152700           IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                             
152800              MOVE JA          TO MID-FLRESTN(WS-INDEX-MID)               
152900           END-IF                                                         
153000           MOVE MID-FLRESTN(WS-INDEX-MID)                                 
153100                               TO ORFK-FLRESTN(WS-INDEX-MID)              
153200        END-IF                                                            
153300                                                                          
153400        IF MID-FLSLATT(WS-INDEX-MID) = ALL '+'                            
153500           MOVE JA             TO ORFK-FLSLATT(WS-INDEX-MID)              
153600        ELSE                                                              
153700           IF MID-FLSLATT(WS-INDEX-MID) = 'Y'                             
153800              MOVE JA          TO MID-FLSLATT(WS-INDEX-MID)               
153900           END-IF                                                         
154000           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
154100                               TO ORFK-FLSLATT(WS-INDEX-MID)              
154200        END-IF                                                            
154300        MOVE ZERO              TO ORFK-IDKONTO(WS-INDEX-MID)              
154400                                                                          
154500        MOVE MID-IDARTNR(WS-INDEX-MID)                                    
154600                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
154700                                                                          
154800        MOVE SPACE             TO ORFK-IDKST(WS-INDEX-MID)                
154900                                                                          
155000        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
155100                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
155200                                                                          
155300        MOVE OHUV-KDVRINFO     TO ORFK-KDVRINFO(WS-INDEX-MID)             
155400                                                                          
155500        MOVE ALL '+'           TO ORFK-PRARTNTO(WS-INDEX-MID)             
155600        MOVE ALL '+'      TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)              
155700        MOVE ALL '+'      TO ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)          
155800        MOVE ALL '+'      TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)              
155900                                                                          
156000        MOVE MID-KVBEART(WS-INDEX-MID)                                    
156100                               TO ORFK-KVBEART(WS-INDEX-MID)              
156200        MOVE MID-TITPO(WS-INDEX-MID)                                      
156300                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
156400        ADD +1                 TO WS-INDEX-MID                            
156500     END-PERFORM                                                          
156600                                                                          
156700     CALL W411ORFK USING ORFK-W411ORFK                                    
156800                         AREG-WDK6-PCB                                    
156900                         AREG-WDK7-PCB                                    
157000                                                                          
157100     MOVE +1                   TO WS-INDEX-MID                            
157200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
157300        PERFORM DA-KOLLA-FEL-FK                                           
157400        ADD +1                 TO WS-INDEX-MID                            
157500     END-PERFORM                                                          
157600     IF SVARSBILD AND NOT ALLT-OK                                         
157700       MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4203'            
157800                             TO FELTEXT                                   
157900       CALL ABEND USING RKOD-ABEND                                        
158000     END-IF                                                               
158100     .                                                                    
158200     EJECT                                                                
158300 DA-KOLLA-FEL-FK SECTION.                                                 
158400                                                                          
158500     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
158600        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
158700        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR(WS-INDEX-MID)        
158800        MOVE NEJ                 TO ALLT-SW                               
158900     END-IF                                                               
159000                                                                          
159100     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
159200        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
159300        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSLATT-ATTR(WS-INDEX-MID)        
159400        MOVE NEJ                 TO ALLT-SW                               
159500     END-IF                                                               
159600                                                                          
159700                                                                          
159800     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
159900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
160000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
160100        MOVE NEJ                 TO ALLT-SW                               
160200     ELSE                                                                 
160300        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
160400                  AND NOT MFS-UPDATE                                      
160500          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
160600          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
160700          MOVE NEJ               TO ALLT-SW                               
160800        END-IF                                                            
160900     END-IF                                                               
161000     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
161100        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
161200        MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)         
161300        MOVE NEJ                 TO ALLT-SW                               
161400     END-IF                                                               
161500                                                                          
161600     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
161700        IF MED-IDMFSFEL = SPACE                                           
161800           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC                          
161900              IF NOT MFS-UPDATE                                           
162000                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
162100                 MOVE MFS-NUM-FAELT-FEL   TO                              
162200                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
162300                 MOVE NEJ             TO ALLT-SW                          
162400              END-IF                                                      
162500           ELSE                                                           
162600            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
162700            MOVE MFS-NUM-FAELT-FEL   TO                                   
162800                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
162900            MOVE NEJ                 TO ALLT-SW                           
163000           END-IF                                                         
163100        ELSE                                                              
163200           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
163300           MOVE MFS-NUM-FAELT-FEL   TO                                    
163400                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
163500           MOVE NEJ                 TO ALLT-SW                            
163600        END-IF                                                            
163700     END-IF                                                               
163800                                                                          
163900     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
164000        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
164100        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR(WS-INDEX-MID)          
164200        MOVE NEJ                 TO ALLT-SW                               
164300     END-IF                                                               
164400     .                                                                    
164500     EJECT                                                                
164600 E-BEHANDLA-RADER SECTION.                                                
164700                                                                          
164800                                                                          
164900     MOVE +1 TO WS-INDEX-MID                                              
165000     MOVE NEJ                     TO TILLK-SW                             
165100                                     OBKR-SW                              
165200     MOVE +0                      TO WS-IDPRQUES                          
165300                                                                          
165400     MOVE JA                      TO FIRST-TIME-SW                        
165500     MOVE SPACE                   TO WS-IDDC-DDGS                         
165600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
165700        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
165800           PERFORM S02-RENSA-TILLK-TAB                                    
165900           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
166000                                  TO AREG-W411AREG-001                    
166100           PERFORM EC-BEHANDLA-RAD                                        
166200           MOVE SPACE             TO WS-IDDC-DDGS                         
166300           MOVE JA                TO TILLK-SW                             
166400           MOVE +1                TO WS-INDEX-TILLK                       
166500           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
166600              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
166700              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
166800                 PERFORM ED-LAES-TILLK-DATA                               
166900                 PERFORM EC-BEHANDLA-RAD                                  
167000                MOVE SPACE        TO WS-IDDC-DDGS                         
167100              END-IF                                                      
167200              ADD +1              TO WS-INDEX-TILLK                       
167300           END-PERFORM                                                    
167400        END-IF                                                            
167500        MOVE NEJ                  TO TILLK-SW                             
167600                                     OBKR-SW                              
167700        MOVE SPACE                TO WS-IDDC-DDGS                         
167800        ADD +1 TO WS-INDEX-MID                                            
167900     END-PERFORM                                                          
168000                                                                          
168100     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
168200       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
168300       MOVE +3                      TO PRNO-KDCALL                        
168400                                                                          
168500       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
168600     END-IF                                                               
168700                                                                          
168800     IF AVSR-IDDC(1) > '00'                                               
168900        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT2-PCB                   
169000        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
169100        AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB                         
169200     END-IF                                                               
169300     MOVE JA                      TO ALLT-SW                              
169400     .                                                                    
169500     EJECT                                                                
169600 EC-BEHANDLA-RAD SECTION.                                                 
169700                                                                          
169800     IF FIRST-TIME AND EGEN-MID                                           
169900       PERFORM ECE-OEPNA-ORDERN                                           
170000     END-IF                                                               
170100                                                                          
170200     PERFORM ECA-NOLLSTALL-OBKR                                           
170300     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
170400     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
170500     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
170600     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
170700                                                                          
170800     IF NOT TILLKOMMANDE-RAD                                              
170900        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
171000     END-IF                                                               
171100                                                                          
171200     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
171300     PERFORM ECJ-KOMPLETTERA-PRIS                                         
171400     PERFORM ECM-KOMPLETTERA-TPO1                                         
171500     PERFORM ECN-KOMPLETTERA-TPO2                                         
171600     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
171700     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
171800     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
171900     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
172000     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
172100     PERFORM ECG-PREL-AVBOKNING-XDC                                       
172200     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
172300     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
172400     IF TILLAEGG-TPO  AND                                                 
172500       ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                               
172600        MOVE NEJ             TO ALLT-SW                                   
172700        MOVE JA              TO OBKR-SW                                   
172800        MOVE NEJ             TO RAD-GODKAND-SW                            
172900     END-IF                                                               
173000     PERFORM ECR-PREL-AVBOKNING-CDC                                       
173100                                                                          
173200     IF NOT TILLKOMMANDE-RAD                                              
173300        IF SKRIV-OBKR                                                     
173400           PERFORM ECS-SKRIV-OBKR                                         
173500           IF NOT OBKR-SKRIVEN OR                                         
173600              EGET-CL-RAD                                                 
173700              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
173800              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
173900           END-IF                                                         
174000        ELSE                                                              
174100           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
174200                               OR KAMP-FLKLAR = JA                        
174300                               OR RELS-FLKLAR = JA                        
174400                               OR TILLAEGG-TPO                            
174500              CONTINUE                                                    
174600           ELSE                                                           
174700              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
174800              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
174900           END-IF                                                         
175000        END-IF                                                            
175100     ELSE                                                                 
175200        IF SKRIV-OBKR                                                     
175300           PERFORM ECS-SKRIV-OBKR                                         
175400        END-IF                                                            
175500     END-IF                                                               
175600     .                                                                    
175700     EJECT                                                                
175800 ECA-NOLLSTALL-OBKR SECTION.                                              
175900                                                                          
176000     MOVE +0                   TO KVAN-KDORDBEK-UT                        
176100     MOVE +0                   TO DLEV-KDORDBEK-UT                        
176200     MOVE +0                   TO KERS-KDERS                              
176300     IF NOT TILLKOMMANDE-RAD                                              
176400        MOVE +0                TO KERS-KDORDBEK                           
176500     ELSE                                                                 
176600        MOVE JA                TO OBKR-SW                                 
176700     END-IF                                                               
176800     MOVE +0                   TO TPO1-KDORDBEK                           
176900     MOVE +0                   TO TPO2-KDORDBEK                           
177000     MOVE +0                   TO RELS-KDORDBEK                           
177100     MOVE +0                   TO KAMP-KDORDBEK                           
177200     MOVE +0                   TO STOR-KDORDBEK                           
177300     MOVE +0                   TO XDCA-KDORDBEK                           
177400*    MOVE +0                   TO NDCA-KDORDBEK                           
177500     MOVE +0                   TO SDCA-KDORDBEK                           
177600     MOVE +0                   TO SDCA-KDORDBEK-FIRST-SDC                 
177700     MOVE +0                   TO SDCA-KDORDBEK-SECOND-SDC                
177800     MOVE +0                   TO CDCA-KDORDBEK-UT                        
177900     MOVE +1                   TO WS-INDEX                                
178000     MOVE ZERO                 TO SPAR-KDORDBEK                           
178100                                                                          
178200     MOVE JA                   TO ALLT-SW                                 
178300     MOVE NEJ                  TO EGET-CL-RAD-SW                          
178400                                                                          
178500     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
178600       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
178700       MOVE +7                          TO W-KDTPOTYP                     
178800     ELSE                                                                 
178900       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
179000         MOVE NEJ               TO ALLT-SW                                
179100                                   KOLLA-ERS-SW                           
179200         MOVE JA                TO OBKR-SW                                
179300       END-IF                                                             
179400     END-IF                                                               
179500     .                                                                    
179600     EJECT                                                                
179700 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
179800                                                                          
179900     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
180000     IF OHUV-IDDC-TVS          >  ZERO                                    
180100        MOVE OHUV-IDDC-TVS     TO ORAD-IDDC                               
180200     ELSE                                                                 
180300       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
180400     END-IF                                                               
180500     IF TILLAEGG-TPO                                                      
180600        MOVE WC-CDC-SE         TO ORAD-IDDC                               
180700     END-IF                                                               
180800     MOVE ORAD-IDDC            TO WS-IDDC                                 
180900     MOVE +0                   TO ORAD-ADLAGOMR                           
181000     MOVE +0                   TO ORAD-ADGANG                             
181100     MOVE +0                   TO ORAD-ADPLATS                            
181200     IF TILLKOMMANDE-RAD                                                  
181300        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
181400     ELSE                                                                 
181500        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
181600                               TO ORAD-IDARTNR                            
181700     END-IF                                                               
181800     MOVE +1                   TO ORAD-IDLOPNR                            
181900                                                                          
182000     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
182100        MOVE SPACE             TO ORAD-BERADREF                           
182200     ELSE                                                                 
182300        MOVE MID-BERADREF(WS-INDEX-MID)                                   
182400                               TO ORAD-BERADREF                           
182500     END-IF                                                               
182600                                                                          
182700     MOVE OHUV-BEKUNDRF        TO ORAD-BEVOLREF                           
182800     MOVE SPACE                TO ORAD-FLAKPLOC                           
182900                                                                          
183000     MOVE NEJ                  TO ORAD-FLINVEST                           
183100                                  ORAD-FLSDCLEV                           
183200     MOVE JA                   TO ORAD-FLOBTRAN                           
183300     IF TILLKOMMANDE-RAD                                                  
183400       IF DIST79-DEALER-PRICE                                             
183500          MOVE NEJ             TO ORAD-FLPRTILL                           
183600       ELSE                                                               
183700        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
183800           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
183900                               TO ORAD-FLPRTILL                           
184000        ELSE                                                              
184100           MOVE NEJ            TO ORAD-FLPRTILL                           
184200        END-IF                                                            
184300       END-IF                                                             
184400     ELSE                                                                 
184500        MOVE NEJ               TO ORAD-FLPRTILL                           
184600     END-IF                                                               
184700     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
184800        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
184900     ELSE                                                                 
185000        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
185100                               TO ORAD-FLRESTN                            
185200     END-IF                                                               
185300     IF TILLKOMMANDE-RAD                                                  
185400        MOVE JA                TO ORAD-FLTILLK                            
185500     ELSE                                                                 
185600        MOVE NEJ               TO ORAD-FLTILLK                            
185700     END-IF                                                               
185800     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
185900     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
186000     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
186100     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
186200     MOVE SPACE                TO ORAD-IDLEVNR                            
186300     MOVE +0                   TO ORAD-IDLOPNR-RO                         
186400     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
186500     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
186600     MOVE ZERO                 TO ORAD-IDSPECEMB                          
186700     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
186800     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
186900     MOVE OHUV-KDVRINFO        TO ORAD-KDDSP                              
187000     IF ORAD-KDDSP = +0                                                   
187100        MOVE +1                TO ORAD-KDDSP                              
187200     END-IF                                                               
187300     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
187400     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
187500        MOVE +0                TO ORAD-KDKVBRYT                           
187600     ELSE                                                                 
187700        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
187800                               TO ORAD-KDKVBRYT                           
187900     END-IF                                                               
188000     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
188100     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
188200     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
188300                                  TEST-KDPRODSL                           
188400     IF ORAD-KDORDING = +3                                                
188500       MOVE SPACE              TO ORAD-KDOI                               
188600     ELSE                                                                 
188700       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
188800         MOVE 'CD'             TO ORAD-KDOI                               
188900       ELSE                                                               
189000         MOVE 'DT'             TO ORAD-KDOI                               
189100       END-IF                                                             
189200     END-IF                                                               
189300     MOVE SPACE                TO ORAD-CLEARGROUP                         
189400                                                                          
189500     IF TILLKOMMANDE-RAD                                                  
189600       IF DIST79-DEALER-PRICE                                             
189700           MOVE SPACE          TO ORAD-KDPRTYP                            
189800       ELSE                                                               
189900        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
190000           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
190100                               TO ORAD-KDPRTYP                            
190200        ELSE                                                              
190300           MOVE SPACE          TO ORAD-KDPRTYP                            
190400        END-IF                                                            
190500       END-IF                                                             
190600     ELSE                                                                 
190700        MOVE SPACE             TO ORAD-KDPRTYP                            
190800     END-IF                                                               
190900     MOVE ZERO                 TO ORAD-KDSPEEMB                           
191000     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
191100        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
191200     ELSE                                                                 
191300        IF OHUV-KDTPOTYP = +0    AND                                      
191400                   MID-TITPO(WS-INDEX-MID) NOT = ALL '+'                  
191500           IF OHUV-IDKAMPRF > +0                                          
191600              MOVE +4          TO ORAD-KDTPOTYP                           
191700           ELSE                                                           
191800              MOVE +2          TO ORAD-KDTPOTYP                           
191900              IF ORAD-KDORDING = +3                                       
192000                 CONTINUE                                                 
192100              ELSE                                                        
192200                 MOVE +2       TO ORAD-KDORDING                           
192300              END-IF                                                      
192400           END-IF                                                         
192500        ELSE                                                              
192600           MOVE +0             TO ORAD-KDTPOTYP                           
192700        END-IF                                                            
192800     END-IF                                                               
192900     MOVE JA                   TO ORAD-FLORDING                           
193000     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
193100                                                                          
193200     IF TILLKOMMANDE-RAD                                                  
193300       MOVE TILK-KVBEART(WS-INDEX-TILLK)                                  
193400                              TO ORAD-KVBEART                             
193500     ELSE                                                                 
193600       MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                       
193700       MOVE WS-NUM-6           TO ORAD-KVBEART                            
193800     END-IF                                                               
193900                                                                          
194000     MOVE +0                   TO ORAD-KVBEART-Q                          
194100     MOVE +0                   TO ORAD-KVPREAVB                           
194200     MOVE +0                   TO ORAD-KVPRERO                            
194300     MOVE +0                   TO ORAD-KVOKS-PREL                         
194400     MOVE +0                   TO ORAD-IDPRQUES                           
194500     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
194600     MOVE +0                   TO ORAD-RERAB                              
194700**   GLOBAL EXPORT. KDVALISO SHOULD NOT BE SPACE.                         
194800     MOVE 'N/A'                TO ORAD-KDVALISO                           
194900     MOVE SPACE                TO ORAD-KDVAT                              
195000     MOVE SPACE                TO ORAD-KDRAB                              
195100     MOVE SPACE                TO ORAD-BEART-VIPS                         
195200                                                                          
195300     IF TILLKOMMANDE-RAD                                                  
195400      IF DIST79-DEALER-PRICE                                              
195500           MOVE +0             TO ORAD-PRARTNTO                           
195600           MOVE TILK-PRARTNTO-LOC(WS-INDEX-TILLK)                         
195700                               TO ORAD-PRARTNTO-LOC                       
195800           MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                     
195900                               TO ORAD-PRARTNTO-LOCPREL                   
196000      ELSE                                                                
196100       IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                              
196200           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
196300                               TO ORAD-PRARTNTO                           
196400           MOVE ZERO           TO ORAD-PRARTNTO-LOC                       
196500       ELSE                                                               
196600           MOVE +0             TO ORAD-PRARTNTO                           
196700           MOVE +0             TO ORAD-PRARTNTO-LOC                       
196800           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
196900       END-IF                                                             
197000      END-IF                                                              
197100     ELSE                                                                 
197200       MOVE +0             TO ORAD-PRARTNTO                               
197300       MOVE +0             TO ORAD-PRARTNTO-LOC                           
197400       MOVE +0             TO ORAD-PRARTNTO-LOCPREL                       
197500       MOVE +0             TO ORAD-PRBPRIS                                
197600     END-IF                                                               
197700                                                                          
197800     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
197900        MOVE MID-IDARTNR(WS-INDEX-MID) (11:1)                             
198000                               TO ORAD-REKSIFFR                           
198100     ELSE                                                                 
198200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
198300     END-IF                                                               
198400     MOVE +0                   TO ORAD-RERF-RAD                           
198500     MOVE +0                   TO ORAD-KVSLATT                            
198600                                                                          
198700     IF TILLKOMMANDE-RAD                                                  
198800       IF DIST79-DEALER-PRICE                                             
198900           MOVE +0             TO ORAD-TIPRIS                             
199000       ELSE                                                               
199100        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
199200           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
199300                               TO ORAD-TIPRIS                             
199400        ELSE                                                              
199500           MOVE +0             TO ORAD-TIPRIS                             
199600        END-IF                                                            
199700       END-IF                                                             
199800     ELSE                                                                 
199900        MOVE +0                TO ORAD-TIPRIS                             
200000     END-IF                                                               
200100                                                                          
200200     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
200300     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
200400     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
200500     MOVE +0                   TO ORAD-TIRODAT                            
200600     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
200700        MOVE OHUV-TITPO        TO ORAD-TITPO                              
200800     ELSE                                                                 
200900        MOVE MID-TITPO(WS-INDEX-MID) TO WS-ALFA-6                         
201000        MOVE WS-NUM-6          TO ORAD-TITPO                              
201100     END-IF                                                               
201200     MOVE AREG-VKART           TO ORAD-VKART                              
201300     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
201400     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
201500     MOVE SPACE                TO ORAD-IDBIL                              
201600                                  ORAD-IDKLIENT                           
201700                                  ORAD-IDARBREF                           
201800                                  ORAD-IDVIN                              
201900                                                                          
202000     IF ORAD-KDORDKL = 1 AND                                              
202100        GMT-FLLDCKND = JA                                                 
202200        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
202300     ELSE                                                                 
202400        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
202500     END-IF                                                               
202600     MOVE +0                   TO ORAD-PRAVCOST                           
202700     .                                                                    
202800     EJECT                                                                
202900 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
203000                                                                          
203100     IF ALLT-OK                                                           
203200                                                                          
203300     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
203400     PERFORM IMS-13-GU-WLARTM-WDK901                                      
203500                                                                          
203600     IF SEGMENT-SAKNAS                                                    
203700        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
203800        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
203900     END-IF                                                               
204000                                                                          
204100     END-IF                                                               
204200     .                                                                    
204300     EJECT                                                                
204400 ECE-OEPNA-ORDERN SECTION.                                                
204500                                                                          
204600     PERFORM IMS-02-GHU-ORQI-WDQ201                                       
204700     MOVE NEJ                    TO OHUV-FLKLAR                           
204800     MOVE '4244'                 TO OHUV-IDSYSTEM                         
204900     MOVE NEJ                    TO FIRST-TIME-SW                         
205000     PERFORM IMS-04-REPL-ORQI-WDQ201                                      
205100                                                                          
205200     IF SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                                
205300        CONTINUE                                                          
205400     ELSE                                                                 
205500        PERFORM IMS-GHNP-ORQI-WDQ212                                      
205600                                                                          
205700        PERFORM UNTIL SEGMENT-SAKNAS                                      
205800                                                                          
205900           MOVE ARB-KDORDSTA     TO ARB-KDORDSTA-O                        
206000           MOVE 'E '             TO ARB-KDORDSTA                          
206100           PERFORM IMS-REPL-ORQI-WDQ212                                   
206200                                                                          
206300           PERFORM IMS-GHNP-ORQI-WDQ212                                   
206400        END-PERFORM                                                       
206500                                                                          
206600     END-IF                                                               
206700* TO RESTORE ARB-IO-AREA WITH PRIMARY DC                                  
206800     MOVE OHUV-IDDC-PRIM    TO W-IDDC-WDQ212                              
206900     PERFORM IMS-GNP-ORQI-WDQ212-FIRST                                    
207000*                                                                         
207100     .                                                                    
207200     EJECT                                                                
207300 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
207400                                                                          
207500     IF ALLT-OK                                                           
207600                                                                          
207700     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
207800     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
207900     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
208000     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
208100     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
208200     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
208300     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
208400     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
208500     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
208600     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
208700     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
208800     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
208900     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
209000     MOVE OHUV-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
209100     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
209200     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
209300     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
209400     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
209500     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
209600                                                                          
209700     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
209800                                                                          
209900     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
210000     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
210100                                                                          
210200     IF KVAN-KDORDBEK-UT > +0                                             
210300        MOVE JA                    TO OBKR-SW                             
210400     END-IF                                                               
210500                                                                          
210600     END-IF                                                               
210700     .                                                                    
210800     EJECT                                                                
210900 ECG-PREL-AVBOKNING-XDC SECTION.                                          
211000                                                                          
211100     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
211200                                                                          
211300       PERFORM S10-HAMTA-WDB6-INFO                                        
211400       IF DCS-NDC                                                         
211500         MOVE JA TO ALLT-SW                                               
211600         PERFORM ECGX-PREL-AVBOKNING-XDC                                  
211700                                                                          
211800*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
211900*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
212000*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
212100*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
212200*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
212300*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
212400*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
212500*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
212600*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
212700*        MOVE OHUV-IDDC-CLEAR(1)   TO NDCA-IDDC-TVS                       
212800*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
212900*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
213000*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
213100*        MOVE AREG-KDERS           TO NDCA-KDERS                          
213200*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
213300*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
213400*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
213500*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
213600*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
213700*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
213800*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
213900*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
214000*        MOVE ORAD-VKART           TO NDCA-VKART                          
214100*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
214200*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
214300*        MOVE +2                   TO NDCA-KDCALL                         
214400*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
214500*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
214600*                                     NDCA-XDK7-KVOKS-DAG                 
214700*                                     NDCA-XDK7-KVOKS-BULK                
214800                                                                          
214900*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
215000*                                          NDCA-USEA-PCB                  
215100*                                          NDCA-WDK7-PCB                  
215200*                                          NDCA-WDL6-PCB                  
215300*                                          NDCA-WDB6-PCB                  
215400*                                          NDCA-XDK7-W411XDK7             
215500                                                                          
215600*        PERFORM ECGX-CHECK-DIFF                                          
215700         IF XDCA-KDORDBEK > ZERO                                          
215800           IF SPAR-FLPUBCDC = YES                                         
215900*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE            
216000*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
216100*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
216200              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
216300                 AND XDCA-DAPUBL > ZERO                                   
216400                 MOVE ZERO TO SPAR-KDORDBEK                               
216500              ELSE                                                        
216600                 MOVE ZERO TO XDCA-KDORDBEK                               
216700              END-IF                                                      
216800           END-IF                                                         
216900           IF KOLLA-ERS                                                   
217000              IF XDCA-KVPREAVB > 0                                        
217100                MOVE ZERO            TO KERS-KDORDBEK                     
217200                PERFORM S02-RENSA-TILLK-TAB                               
217300                MOVE ZERO            TO SPAR-KDORDBEK                     
217400              ELSE                                                        
217500                MOVE ZERO            TO XDCA-KDORDBEK                     
217600              END-IF                                                      
217700           ELSE                                                           
217800             IF XDCA-KDORDBEK = 15                                        
217900                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
218000                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
218100                END-IF                                                    
218200                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
218300                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
218400                END-IF                                                    
218500                IF SDCA-KDORDBEK = 15                                     
218600                   MOVE ZERO         TO SDCA-KDORDBEK                     
218700                END-IF                                                    
218800             END-IF                                                       
218900             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
219000                 MOVE ZERO           TO XDCA-KDORDBEK                     
219100             END-IF                                                       
219200           END-IF                                                         
219300           MOVE JA                   TO OBKR-SW                           
219400         ELSE                                                             
219500           IF KOLLA-ERS    OR                                             
219600             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
219700             IF XDCA-KVPREAVB > 0                                         
219800               MOVE ZERO             TO KERS-KDORDBEK                     
219900               PERFORM S02-RENSA-TILLK-TAB                                
220000               MOVE ZERO             TO SPAR-KDORDBEK                     
220100             ELSE                                                         
220200               MOVE JA               TO OBKR-SW                           
220300             END-IF                                                       
220400           ELSE                                                           
220400            IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                  
220400               MOVE ZERO             TO SPAR-KDORDBEK                     
220400            END-IF                                                        
220400           END-IF                                                         
220500           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
220600*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
220700*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
220800              MOVE 0  TO SPAR-KDORDBEK                                    
220900              MOVE JA  TO ALLT-SW                                         
221000              MOVE NEJ TO OBKR-SW                                         
221100           END-IF                                                         
221200         END-IF                                                           
221300         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
221400         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
221500         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
221600         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
221700         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
221800         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
221900         MOVE XDCA-KDOI              TO ORAD-KDOI                         
222000         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
222100         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
222200         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
222300         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
222400         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
222500         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
222600         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
222700         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
222800         MOVE NEJ                    TO ALLT-SW                           
222900       END-IF                                                             
223000                                                                          
223100     END-IF                                                               
223200     .                                                                    
223300     EJECT                                                                
223400 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
223500                                                                          
223600     IF ALLT-OK OR KOLLA-ERS                                              
223700                                                                          
223800       PERFORM S10-HAMTA-WDB6-INFO                                        
223900                                                                          
224000       IF DCS-NDC                                                         
224100                                                                          
224200* XDCA-INPUT                                                              
224300         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
224400           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
224500                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
224600           ADD +1 TO WS-INDEX                                             
224700         END-PERFORM                                                      
224800         MOVE OHUV-IDDC-PRIM       TO XDCA-IDDC-TVS                       
224900         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
225000         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
225100         MOVE ORAD-IDDC            TO XDCA-IDDC                           
225200         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
225300         MOVE AREG-KDERS           TO XDCA-KDERS                          
225400         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
225500         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
225600         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
225700         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
225800         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
225900         MOVE ORAD-VKART           TO XDCA-VKART                          
226000         MOVE +1                   TO XDCA-KDCALL                         
226100                                                                          
226200* XDCA-OUTPUT                                                             
226300         MOVE SPACE                TO XDCA-IDDC-OUT                       
226400                                      XDCA-IDDC-RO                        
226500                                      XDCA-KDARTURS                       
226600                                      XDCA-KDOI                           
226700                                      XDCA-CLEARGROUP                     
226800         MOVE ZERO                 TO XDCA-ADLAGOMR                       
226900                                      XDCA-ADGANG                         
227000                                      XDCA-ADPLATS                        
227100                                      XDCA-KDORDBEK                       
227200                                      XDCA-KVPREAVB                       
227300                                      XDCA-KVPRERO                        
227400                                      XDCA-TIREGDAT-OUT                   
227500                                      XDCA-TIREGTID-OUT                   
227600                                      XDCA-VKART-OUT                      
227700                                      XDCA-VKART-NTO                      
227800                                      XDCA-VLARTNTO                       
227900         MOVE ZERO                 TO                                     
228000                                      XDCA-KVOKS-DAG                      
228100                                      XDCA-KVOKS-BULK                     
228200                                                                          
228300         IF XDCA-DAPUBL NOT = 99999999                                    
228400            MOVE ZERO              TO XDCA-DAPUBL                         
228500         END-IF                                                           
228600                                                                          
228700         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
228800         XDCA-USEA-PCB                                                    
228900         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
229000         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
229100         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
229200         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
229300         XDCA-WDK7-3-PCB                                                  
229400                                                                          
229500* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
229600* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
229700         IF XDCA-KDARTURS = SPACE                                         
229800           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
229900         END-IF                                                           
230000         IF XDCA-VKART-NTO = ZERO                                         
230100           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
230200         END-IF                                                           
230300         IF XDCA-VLARTNTO = ZERO                                          
230400           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
230500         END-IF                                                           
230600       END-IF                                                             
230700     END-IF                                                               
230800     .                                                                    
230900     EJECT                                                                
231000 ECGX-CHECK-DIFF SECTION.                                                 
231100                                                                          
231200     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
231300     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
231400     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
231500     AND NDCA-ADGANG     = XDCA-ADGANG                                    
231600     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
231700     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
231800     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
231900     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
232000     AND NDCA-KDOI       = XDCA-KDOI                                      
232100     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
232200     AND NDCA-VKART      = XDCA-VKART-OUT                                 
232300     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
232400     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
232500     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
232600     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
232700     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
232800     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
232900         MOVE NEJ TO DIFF-FLSVAR                                          
233000     ELSE                                                                 
233100        MOVE JA           TO DIFF-FLSVAR                                  
233200     END-IF                                                               
233300                                                                          
233400* ORDER LOG INFO                                                          
233500     IF DIFF-FLSVAR = JA                                                  
233600       MOVE IDPGM         TO FIL-IDPGM                                    
233700       ACCEPT FIL-TIREGDAT FROM DATE                                      
233800       ACCEPT FIL-TIKLOCK FROM TIME                                       
233900       MOVE 1             TO FIL-IDSEKVNR                                 
234000       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
234100       MOVE 'A'           TO FIL-CT-IDVTYP                                
234200       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
234300                                                                          
234400*   ORDER LINE INFO                                                       
234500       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
234600       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
234700       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
234800       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
234900       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
235000       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
235100       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
235200       MOVE '4244'         TO DIFF-IDSYSTEM                               
235300                                                                          
235400*   NDCA INFO                                                             
235500       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
235600       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
235700       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
235800       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
235900       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
236000       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
236100       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
236200       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
236300       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
236400       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
236500       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
236600       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
236700       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
236800       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
236900       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
237000       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
237100                                                                          
237200*   XDCA INFO                                                             
237300       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
237400       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
237500       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
237600       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
237700       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
237800       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
237900       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
238000       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
238100       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
238200       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
238300       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
238400       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
238500       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
238600       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
238700       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
238800       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
238900                                                                          
239000       PERFORM IMS-ISRT-WDR601                                            
239100       IF SEGMENT-FINNS-REDAN                                             
239200          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
239300             ADD 1 TO FIL-IDSEKVNR                                        
239400             PERFORM IMS-ISRT-WDR601                                      
239500          END-PERFORM                                                     
239600       END-IF                                                             
239700     END-IF                                                               
239800     .                                                                    
239900     EJECT                                                                
240000 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
240100                                                                          
240200                                                                          
240300     IF ALLT-OK OR KOLLA-ERS                                              
240400                                                                          
240500       PERFORM S10-HAMTA-WDB6-INFO                                        
240600       IF DCS-SDC                                                         
240700                                                                          
240800         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
240900         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
241000         MOVE NEJ                  TO SDCA-FLORDSPE                       
241100         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
241200         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
241300         MOVE ORAD-IDDC            TO SDCA-IDDC                           
241400         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
241500         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
241600         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
241700         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
241800         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
241900         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
242000         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
242100         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
242200         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
242300         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
242400         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
242500         MOVE +0                   TO SDCA-TIREPDAT                       
242600         MOVE +0                   TO SDCA-KVOKS-PREL                     
242700         MOVE +1                   TO SDCA-KDCALL                         
242800         MOVE +1                   TO SDCA-IXDCCLEAR                      
242900                                                                          
243000         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
243100                                           SDCA-WDB6-PCB                  
243200                                           SDCA-WDK9-PCB                  
243300                                           SDCA-WDR6-PCB                  
243400                                           SDCA-WDK6-PCB                  
243500                                           SDCA-WDQ4B-PCB                 
243600                                           SDCA-WDQ2-PCB                  
243700                                           SDCA-WDQ4-PCB                  
243800                                           SDCA-WDB6-2-PCB                
243900                                           SDCA-WDK6-2-PCB                
244000                                           SDCA-WDK7-2-PCB                
244100                                           SDCA-WDK7-3-PCB                
244200                                                                          
244300         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
244400         MOVE ZERO          TO SDCA-KDORDBEK                              
244500                                                                          
244600         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
244700           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
244800*            IF OHUV-IDDC-CLEAR(2) > '19'                                 
244900*              MOVE ZERO               TO SDCA-KDORDBEK-FIRST-SDC         
245000*              MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                       
245100*              MOVE ORAD-IDDC          TO WS-IDDC                         
245200*              IF SDCA-KDOI = 'XX'                                        
245300*                 MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
245400*              END-IF                                                     
245500*            ELSE                                                         
245600               MOVE JA        TO OBKR-SW                                  
245700               MOVE NEJ       TO ALLT-SW                                  
245800*            END-IF                                                       
245900           ELSE                                                           
246000             IF ORAD-KDORDKL > 0                                          
246100             AND ORAD-IDSYSTEM NOT = 'OREL'                               
246200             AND (AREG-KDUART = 'L'                                       
246300             OR AREG-KDUART = 'P')                                        
246400             AND OHUV-FLORDSPE NOT = JA                                   
246500             AND OHUV-FLOVRLEV NOT = JA                                   
246600             AND OHUV-FLFORBI = NEJ                                       
246700                MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                   
246800                MOVE 70      TO TPO2-KDORDBEK                             
246900                MOVE JA      TO OBKR-SW                                   
247000                MOVE NEJ     TO ALLT-SW                                   
247100                MOVE WC-CDC-SE TO ORAD-IDDC                               
247200                MOVE ORAD-IDDC TO WS-IDDC                                 
247300                MOVE 6       TO ORAD-KDTPOTYP                             
247400                IF ORAD-KDPRTYP NOT = 'P'                                 
247500                   MOVE ZERO  TO ORAD-PRARTNTO                            
247600*                                ORAD-PRARTNTO-LOC                        
247700*                                ORAD-PRARTNTO-LOCPREL                    
247800                   MOVE SPACE TO ORAD-KDPRTYP                             
247900**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030613                      
248000                  IF NOT DIST79-DEALER-PRICE                              
248100                    MOVE ZERO        TO ORAD-PRARTNTO-LOC                 
248200                    MOVE NEJ         TO ORAD-FLPRTILL                     
248300                  END-IF                                                  
248400*************                                                             
248500                END-IF                                                    
248600             ELSE                                                         
248700               IF KOLLA-ERS                                               
248800                  MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
248900                  IF SPAR-KDORDBEK = ZERO                                 
249000                    MOVE W-GMT-IDDC-CLEAR(2)                              
249100                                       TO ORAD-IDDC                       
249200                    MOVE ORAD-IDDC     TO WS-IDDC                         
249300                  ELSE                                                    
249400                    MOVE NEJ           TO KOLLA-ERS-SW                    
249500                  END-IF                                                  
249600               ELSE                                                       
249700                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
249800                    MOVE JA            TO OBKR-SW                         
249900                    MOVE W-GMT-IDDC-CLEAR(2)                              
250000                                       TO ORAD-IDDC                       
250100                    MOVE ORAD-IDDC     TO WS-IDDC                         
250200                 ELSE                                                     
250300                    MOVE JA            TO OBKR-SW                         
250400                    MOVE NEJ           TO ALLT-SW                         
250500                 END-IF                                                   
250600               END-IF                                                     
250700             END-IF                                                       
250800           END-IF                                                         
250900         ELSE                                                             
251000           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
251100           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
251200           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
251300           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
251400           MOVE NEJ                  TO ALLT-SW                           
251500           IF KVAN-KDORDBEK-UT = ZERO                                     
251600             MOVE JA                 TO EGET-CL-RAD-SW                    
251700           END-IF                                                         
251800           IF KOLLA-ERS OR                                                
251900             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
252000              MOVE NEJ               TO KOLLA-ERS-SW                      
252100              MOVE ZERO              TO KERS-KDORDBEK                     
252200              PERFORM S02-RENSA-TILLK-TAB                                 
252300              MOVE ZERO              TO SPAR-KDORDBEK                     
252400           END-IF                                                         
252500         END-IF                                                           
252600         MOVE SDCA-KDOI              TO ORAD-KDOI                         
252700         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
252800       END-IF                                                             
252900                                                                          
253000     END-IF                                                               
253100     .                                                                    
253200     EJECT                                                                
253300 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
253400                                                                          
253500     IF ALLT-OK OR KOLLA-ERS                                              
253600                                                                          
253700       PERFORM S10-HAMTA-WDB6-INFO                                        
253800       IF DCS-SDC                                                         
253900                                                                          
254000         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
254100         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
254200         MOVE NEJ                  TO SDCA-FLORDSPE                       
254300         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
254400         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
254500         MOVE ORAD-IDDC            TO SDCA-IDDC                           
254600         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
254700         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
254800         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
254900         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
255000         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
255100         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
255200         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
255300         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
255400         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
255500         MOVE +1                   TO SDCA-KDCALL                         
255600         MOVE +2                   TO SDCA-IXDCCLEAR                      
255700                                                                          
255800         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
255900                                           SDCA-WDB6-PCB                  
256000                                           SDCA-WDK9-PCB                  
256100                                           SDCA-WDR6-PCB                  
256200                                           SDCA-WDK6-PCB                  
256300                                           SDCA-WDQ4B-PCB                 
256400                                           SDCA-WDQ2-PCB                  
256500                                           SDCA-WDQ4-PCB                  
256600                                           SDCA-WDB6-2-PCB                
256700                                           SDCA-WDK6-2-PCB                
256800                                           SDCA-WDK7-2-PCB                
256900                                           SDCA-WDK7-3-PCB                
257000                                                                          
257100         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
257200         MOVE ZERO          TO SDCA-KDORDBEK                              
257300                                                                          
257400         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
257500           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
257600*            IF OHUV-IDDC-CLEAR(3) > '19'                                 
257700*              MOVE ZERO               TO SDCA-KDORDBEK-SECOND-SDC        
257800*              MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                       
257900*              MOVE ORAD-IDDC          TO WS-IDDC                         
258000*              IF SDCA-KDOI = 'XX'                                        
258100*                 MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
258200*              END-IF                                                     
258300*            ELSE                                                         
258400               MOVE JA        TO OBKR-SW                                  
258500               MOVE NEJ       TO ALLT-SW                                  
258600*            END-IF                                                       
258700           ELSE                                                           
258800             IF ORAD-KDORDKL > 0                                          
258900             AND ORAD-IDSYSTEM NOT = 'OREL'                               
259000             AND (AREG-KDUART = 'L'                                       
259100             OR AREG-KDUART = 'P')                                        
259200             AND OHUV-FLORDSPE NOT = JA                                   
259300             AND OHUV-FLOVRLEV NOT = JA                                   
259400             AND OHUV-FLFORBI = NEJ                                       
259500                MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                  
259600                MOVE 70      TO TPO2-KDORDBEK                             
259700                MOVE JA      TO OBKR-SW                                   
259800                MOVE NEJ     TO ALLT-SW                                   
259900                MOVE WC-CDC-SE TO ORAD-IDDC                               
260000                MOVE ORAD-IDDC TO WS-IDDC                                 
260100                MOVE 6       TO ORAD-KDTPOTYP                             
260200                IF ORAD-KDPRTYP NOT = 'P'                                 
260300                   MOVE ZERO  TO ORAD-PRARTNTO                            
260400*                                ORAD-PRARTNTO-LOC                        
260500*                                ORAD-PRARTNTO-LOCPREL                    
260600                   MOVE SPACE TO ORAD-KDPRTYP                             
260700**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030613                      
260800                  IF NOT DIST79-DEALER-PRICE                              
260900                    MOVE ZERO        TO ORAD-PRARTNTO-LOC                 
261000                    MOVE NEJ         TO ORAD-FLPRTILL                     
261100                  END-IF                                                  
261200*************                                                             
261300                END-IF                                                    
261400             ELSE                                                         
261500               IF KOLLA-ERS                                               
261600                  MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
261700                  IF SPAR-KDORDBEK = ZERO                                 
261800                    MOVE W-GMT-IDDC-CLEAR(3)                              
261900                                       TO ORAD-IDDC                       
262000                    MOVE ORAD-IDDC     TO WS-IDDC                         
262100                  ELSE                                                    
262200                    MOVE NEJ           TO KOLLA-ERS-SW                    
262300                  END-IF                                                  
262400               ELSE                                                       
262500                 IF SDCA-KDORDBEK-SECOND-SDC = 15                         
262600                    IF SDCA-KDORDBEK-FIRST-SDC = 15                       
262700                       MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC               
262800                    END-IF                                                
262900                    MOVE JA            TO OBKR-SW                         
263000                    MOVE W-GMT-IDDC-CLEAR(3)                              
263100                                       TO ORAD-IDDC                       
263200                    MOVE ORAD-IDDC     TO WS-IDDC                         
263300                 ELSE                                                     
263400                    MOVE JA            TO OBKR-SW                         
263500                    MOVE NEJ           TO ALLT-SW                         
263600                 END-IF                                                   
263700               END-IF                                                     
263800             END-IF                                                       
263900           END-IF                                                         
264000         ELSE                                                             
264100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
264200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
264300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
264400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
264500           MOVE NEJ                  TO ALLT-SW                           
264600           IF KVAN-KDORDBEK-UT = ZERO                                     
264700           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
264800             MOVE JA                 TO EGET-CL-RAD-SW                    
264900           END-IF                                                         
265000           IF KOLLA-ERS OR                                                
265100             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
265200              MOVE NEJ               TO KOLLA-ERS-SW                      
265300              MOVE ZERO              TO KERS-KDORDBEK                     
265400              PERFORM S02-RENSA-TILLK-TAB                                 
265500              MOVE ZERO              TO SPAR-KDORDBEK                     
265600           END-IF                                                         
265700         END-IF                                                           
265800         MOVE SDCA-KDOI              TO ORAD-KDOI                         
265900         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
266000       END-IF                                                             
266100                                                                          
266200     END-IF                                                               
266300     .                                                                    
266400     EJECT                                                                
266500 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
266600                                                                          
266700     IF ALLT-OK OR KOLLA-ERS                                              
266800                                                                          
266900       PERFORM S10-HAMTA-WDB6-INFO                                        
267000                                                                          
267100       IF  DCS-SDC                                                        
267200                                                                          
267300         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
267400         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
267500         MOVE NEJ                  TO SDCA-FLORDSPE                       
267600         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
267700         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
267800         MOVE ORAD-IDDC            TO SDCA-IDDC                           
267900         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
268000         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
268100         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
268200         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
268300         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
268400         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
268500         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
268600         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
268700         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
268800         MOVE +1                   TO SDCA-KDCALL                         
268900         MOVE +3                   TO SDCA-IXDCCLEAR                      
269000                                                                          
269100         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
269200                                           SDCA-WDB6-PCB                  
269300                                           SDCA-WDK9-PCB                  
269400                                           SDCA-WDR6-PCB                  
269500                                           SDCA-WDK6-PCB                  
269600                                           SDCA-WDQ4B-PCB                 
269700                                           SDCA-WDQ2-PCB                  
269800                                           SDCA-WDQ4-PCB                  
269900                                           SDCA-WDB6-2-PCB                
270000                                           SDCA-WDK6-2-PCB                
270100                                           SDCA-WDK7-2-PCB                
270200                                           SDCA-WDK7-3-PCB                
270300                                                                          
270400         IF SDCA-KDORDBEK > ZERO                                          
270500           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
270600              MOVE JA        TO OBKR-SW                                   
270700              MOVE NEJ       TO ALLT-SW                                   
270800           ELSE                                                           
270900             IF ORAD-KDORDKL > 0                                          
271000             AND ORAD-IDSYSTEM NOT = 'OREL'                               
271100             AND (AREG-KDUART = 'L'                                       
271200             OR AREG-KDUART = 'P')                                        
271300             AND OHUV-FLORDSPE NOT = JA                                   
271400             AND OHUV-FLOVRLEV NOT = JA                                   
271500             AND OHUV-FLFORBI = NEJ                                       
271600                MOVE ZERO    TO SDCA-KDORDBEK                             
271700                MOVE 70      TO TPO2-KDORDBEK                             
271800                MOVE JA      TO OBKR-SW                                   
271900                MOVE NEJ     TO ALLT-SW                                   
272000                MOVE WC-CDC-SE TO ORAD-IDDC                               
272100                MOVE ORAD-IDDC TO WS-IDDC                                 
272200                MOVE 6       TO ORAD-KDTPOTYP                             
272300                IF ORAD-KDPRTYP NOT = 'P'                                 
272400                   MOVE ZERO  TO ORAD-PRARTNTO                            
272500*                                ORAD-PRARTNTO-LOC                        
272600*                                ORAD-PRARTNTO-LOCPREL                    
272700                   MOVE SPACE TO ORAD-KDPRTYP                             
272800**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030613                      
272900                   IF NOT DIST79-DEALER-PRICE                             
273000                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
273100                     MOVE NEJ         TO ORAD-FLPRTILL                    
273200                   END-IF                                                 
273300*************                                                             
273400                END-IF                                                    
273500             ELSE                                                         
273600               IF KOLLA-ERS                                               
273700                  MOVE NEJ             TO KOLLA-ERS-SW                    
273800                  MOVE ZERO            TO SDCA-KDORDBEK                   
273900                  IF SPAR-KDORDBEK = ZERO                                 
274000                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
274100                    MOVE ORAD-IDDC     TO WS-IDDC                         
274200                  END-IF                                                  
274300               ELSE                                                       
274400                 IF SDCA-KDORDBEK = 15                                    
274500                    IF SDCA-KDORDBEK-SECOND-SDC = 15                      
274600                       MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC              
274700                    END-IF                                                
274800                    MOVE JA            TO OBKR-SW                         
274900                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
275000                    MOVE ORAD-IDDC     TO WS-IDDC                         
275100                 ELSE                                                     
275200                    MOVE JA            TO OBKR-SW                         
275300                    MOVE NEJ           TO ALLT-SW                         
275400                 END-IF                                                   
275500               END-IF                                                     
275600             END-IF                                                       
275700           END-IF                                                         
275800         ELSE                                                             
275900           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
276000           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
276100           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
276200           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
276300           MOVE NEJ                  TO ALLT-SW                           
276400           IF KVAN-KDORDBEK-UT = ZERO                                     
276500           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
276600           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
276700             MOVE JA                 TO EGET-CL-RAD-SW                    
276800           END-IF                                                         
276900           IF KOLLA-ERS OR                                                
277000             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
277100              MOVE NEJ               TO KOLLA-ERS-SW                      
277200              MOVE ZERO              TO KERS-KDORDBEK                     
277300              PERFORM S02-RENSA-TILLK-TAB                                 
277400              MOVE ZERO              TO SPAR-KDORDBEK                     
277500           END-IF                                                         
277600         END-IF                                                           
277700                                                                          
277800         MOVE SDCA-KDOI              TO ORAD-KDOI                         
277900         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
278000       END-IF                                                             
278100                                                                          
278200     END-IF                                                               
278300     .                                                                    
278400     EJECT                                                                
278500 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
278600                                                                          
278700     PERFORM S10-HAMTA-WDB6-INFO                                          
278800                                                                          
278900     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
279000                     DCS-SDC)                                             
279100                                                                          
279200       MOVE ORAD-IDDISTR       TO DLEV-IDDISTR-IN                         
279300       MOVE ORAD-IDKUNDNR      TO DLEV-IDKUNDNR-IN                        
279400       MOVE OHUV-KDORDKL       TO DLEV-KDORDKL-IN                         
279500       MOVE ORAD-IDARTNR       TO DLEV-IDARTNR-IN                         
279600       MOVE AREG-IDLEVNR       TO DLEV-IDLEVNR-IN                         
279700       MOVE ORAD-KVBEART-Q     TO DLEV-KVBEART-Q-IN                       
279800       MOVE AREG-REDIRLEV      TO DLEV-REDIRLEV-IN                        
279900       MOVE ORAD-IDDC          TO DLEV-IDDC-IN                            
280000       MOVE ORAD-IDDC          TO DLEV-IDDC-ORD-IN                        
280100       MOVE ORAD-IDKAMPRF      TO DLEV-IDKAMPRF-IN                        
280200       MOVE ORAD-KDTPOTYP      TO DLEV-KDTPOTYP-IN                        
280300       MOVE AREG-KDUART        TO DLEV-KDUART-IN                          
280400       MOVE OHUV-FLFORBI       TO DLEV-FLFORBI-IN                         
280500       MOVE ORAD-FLRESTN       TO DLEV-FLRESTN-IN                         
280600       MOVE AREG-FLREFILL      TO DLEV-FLREFILL-IN                        
280700       MOVE ORAD-KDORDING      TO DLEV-KDORDING-IN                        
280800       MOVE OHUV-IDKUNDRF      TO DLEV-IDKUNDRF-IN                        
280900                                                                          
281000        MOVE +1 TO WS-INDEX                                               
281100        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
281200           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
281300                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
281400           ADD +1 TO WS-INDEX                                             
281500        END-PERFORM                                                       
281600                                                                          
281700       MOVE 1                  TO DLEV-KDCALL                             
281800       MOVE SPACE              TO DLEV-CLEARGROUP                         
281900       MOVE ORAD-KDOI          TO DLEV-KDOI-UT                            
282000                                                                          
282100       CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                    
282200                                         DLEV-LEVG-PCB                    
282300                                         DLEV-LEVA-PCB                    
282400                                         DLEV-ARTS-PCB                    
282500                                         DLEV-WDB6-PCB                    
282600                                         TPO2-FILA-PCB                    
282700       IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                               
282800          MOVE JA                TO OBKR-SW                               
282900          MOVE NEJ               TO ALLT-SW                               
283000          MOVE ZERO              TO KVAN-KDORDBEK-UT                      
283100       ELSE                                                               
283200          IF DLEV-KDORDBEK-UT = 95                                        
283300             MOVE JA             TO OBKR-SW                               
283400          END-IF                                                          
283500          IF DLEV-IDLEVNR-UT NOT = SPACE                                  
283600             IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                           
283700               MOVE DLEV-IDDC-UT    TO ORAD-IDDC                          
283800                                       WS-IDDC                            
283900                                       WS-IDDC-DDGS                       
284000             END-IF                                                       
284100                                                                          
284200             MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                      
284300             MOVE AREG-ADGANG       TO ORAD-ADGANG                        
284400             MOVE AREG-ADPLATS      TO ORAD-ADPLATS                       
284500             PERFORM S10-HAMTA-WDB6-INFO                                  
284600             IF TILLAEGG-TPO OR DCS-DDC                                   
284700                MOVE 82             TO DLEV-KDORDBEK-UT                   
284800                MOVE NEJ            TO ALLT-SW                            
284900                MOVE JA             TO OBKR-SW                            
285000                MOVE ZERO           TO KVAN-KDORDBEK-UT                   
285100                IF DCS-DDC                                                
285200                  MOVE DLEV-IDDC-IN TO ORAD-IDDC                          
285300                                       WS-IDDC                            
285400                END-IF                                                    
285500             ELSE                                                         
285600                IF KOLLA-ARBTAB-C1                                        
285700                   PERFORM ECIA-KOLLA-UTSKRIVEN-DLEV                      
285800                END-IF                                                    
285900             END-IF                                                       
286000          END-IF                                                          
286100          MOVE DLEV-KDORDSTA-UT   TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
286200          MOVE DLEV-KDVIA-UT      TO AVSR-KDVIA    (WS-INDEX-WOPS)        
286300          MOVE DLEV-KVDAGAR-DIFF-UT TO                                    
286400                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
286500          MOVE DLEV-TISKEPPN-DDC-UT TO                                    
286600                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
286700                                                                          
286800          MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                       
286900          MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                      
287000          MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                       
287100          IF DLEV-FLSDCLEV-UT = NEJ                                       
287200            MOVE DLEV-KDOI-UT       TO ORAD-KDOI                          
287300            MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                    
287400          ELSE                                                            
287500            MOVE DLEV-IDDC-UT       TO ORAD-IDDC                          
287600                                       WS-IDDC                            
287700          END-IF                                                          
287800                                                                          
287900       END-IF                                                             
288000                                                                          
288100       IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                     
288200          AND ORAD-IDDC = WC-CDC-SE                                       
288300          MOVE 26               TO DLEV-KDORDBEK-UT                       
288400          MOVE JA               TO OBKR-SW                                
288500       END-IF                                                             
288600                                                                          
288700     END-IF                                                               
288800     .                                                                    
288900     EJECT                                                                
289000 ECIA-KOLLA-UTSKRIVEN-DLEV SECTION.                                       
289100                                                                          
289200     MOVE WS-IDDC              TO W-IDDC                                  
289300     MOVE DLEV-IDLEVNR-UT      TO W-IDLEVNR                               
289400     PERFORM IMS-16-GNP-ORQI-WDQ211                                       
289500     IF SEGMENT-FINNS                                                     
289600        IF DIRL-KVRADER = +0                                              
289700           MOVE 82          TO DLEV-KDORDBEK-UT                           
289800           MOVE NEJ         TO ALLT-SW                                    
289900           MOVE JA          TO OBKR-SW                                    
290000           MOVE ZERO        TO KVAN-KDORDBEK-UT                           
290100        ELSE                                                              
290200           MOVE JA          TO RAD-GODKAND-SW                             
290300        END-IF                                                            
290400     ELSE                                                                 
290500        MOVE JA             TO RAD-GODKAND-SW                             
290600     END-IF                                                               
290700     .                                                                    
290800     EJECT                                                                
290900 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
291000                                                                          
291100     IF ALLT-OK                                                           
291200                                                                          
291300     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
291400     MOVE ORAD-IDDC            TO KERS-IDDC                               
291500     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
291600     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
291700     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
291800     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
291900     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
292000     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
292100     MOVE AREG-KDERS           TO KERS-KDERS                              
292200     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
292300     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
292400     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
292500     MOVE AREG-KDUART          TO KERS-KDUART                             
292600     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
292700     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
292800     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
292900     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
293000     MOVE ZERO                 TO WX-KDORDBEK                             
293100                                                                          
293200     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
293300                         KERS-ARTC-PCB KERS-ERSA-PCB                      
293400                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
293500                                                                          
293600     IF KERS-KDORDBEK > ZERO   AND                                        
293700        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
293800          KERS-KDERS > 10 )                                               
293900        MOVE JA                  TO OBKR-SW                               
294000        MOVE NEJ                 TO ALLT-SW                               
294100     END-IF                                                               
294200                                                                          
294300     PERFORM S10-HAMTA-WDB6-INFO                                          
294400                                                                          
294500     IF KERS-KDORDBEK > +0                                                
294600        IF DCS-SDC                                                        
294700          IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR 19          
294800                       OR 21 OR 22 OR 24 OR 25 OR 27 OR 28 OR 29          
294900             MOVE JA            TO KOLLA-ERS-SW                           
295000          END-IF                                                          
295100        END-IF                                                            
295200        IF DCS-NDC                                                        
295300          IF AREG-KDERS > 18                                              
295400            MOVE JA                 TO KOLLA-ERS-SW                       
295500          ELSE                                                            
295600            MOVE ZERO               TO KERS-KDORDBEK                      
295700            PERFORM S02-RENSA-TILLK-TAB                                   
295800            MOVE JA                 TO ALLT-SW                            
295900          END-IF                                                          
296000        END-IF                                                            
296100     END-IF                                                               
296200     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
296300        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
296400***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
296500***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
296600***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
296700***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
296800           CONTINUE                                                       
296900        ELSE                                                              
297000           MOVE +0               TO KVAN-KDORDBEK-UT                      
297100           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
297200        END-IF                                                            
297300     END-IF                                                               
297400     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
297500        MOVE +0                  TO DLEV-KDORDBEK-UT                      
297600     END-IF                                                               
297700                                                                          
297800     ELSE                                                                 
297900        MOVE +0                  TO KERS-KDERS                            
298000     END-IF                                                               
298100     .                                                                    
298200     EJECT                                                                
298300 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
298400                                                                          
298500     IF ALLT-OK OR KOLLA-ERS                                              
298600                                                                          
298700     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
298800     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
298900     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
299000     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
299100     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
299200     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
299300     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
299400     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
299500     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
299600     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
299700     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
299800     MOVE AREG-FLIART          TO SPAR-FLIART                             
299900     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
300000     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
300100     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
300200     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
300300     MOVE ORAD-IDDC            TO SPAR-IDDC                               
300400     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
300500     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
300600     MOVE AREG-KDERS           TO SPAR-KDERS                              
300700     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
300800     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
300900     MOVE +1                   TO SPAR-KDORDBEH                           
301000     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
301100     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
301200     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
301300     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
301400     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
301500     MOVE AREG-KDUART          TO SPAR-KDUART                             
301600     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
301700     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
301800     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
301900     MOVE ORAD-TITPO           TO SPAR-TITPO                              
302000     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
302100     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
302200                                                                          
302300     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
302400                                       SPAR-WDF8A-PCB                     
302500                                       SPAR-WDK6-PCB                      
302600                                                                          
302700     IF SPAR-KDORDBEK        > ZERO                                       
302800       MOVE JA                TO OBKR-SW                                  
302900       MOVE NEJ               TO ALLT-SW                                  
303000       IF SPAR-KDORDBEK = 51 OR 67 OR 58                                  
303100         MOVE NEJ             TO KOLLA-ERS-SW                             
303200         MOVE ZERO            TO KERS-KDORDBEK                            
303300         PERFORM S02-RENSA-TILLK-TAB                                      
303400       END-IF                                                             
303400       MOVE ZERO             TO XDCA-DAPUBL                               
303500*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411XDCA                         
303600        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
303700          MOVE 99999999       TO XDCA-DAPUBL                              
303800*         MOVE 99999999       TO NDCA-DAPUBL                              
303900        END-IF                                                            
304000       IF KVAN-KDORDBEK-UT > +0                                           
304100          MOVE +0             TO KVAN-KDORDBEK-UT                         
304200          MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                           
304300       END-IF                                                             
304400       IF DLEV-KDORDBEK-UT > +0                                           
304500          MOVE +0             TO DLEV-KDORDBEK-UT                         
304600       END-IF                                                             
304700     END-IF                                                               
304800     IF AREG-KDSORT = 'SW'                                                
304900        MOVE 67                     TO SPAR-KDORDBEK                      
305000        MOVE JA                     TO OBKR-SW                            
305100        MOVE NEJ                    TO ALLT-SW                            
305200        IF KOLLA-ERS-SW = JA                                              
305300           MOVE NEJ             TO KOLLA-ERS-SW                           
305400           MOVE ZERO            TO KERS-KDORDBEK                          
305500           PERFORM S02-RENSA-TILLK-TAB                                    
305600        END-IF                                                            
305700     END-IF                                                               
305800                                                                          
305900     END-IF                                                               
306000     .                                                                    
306100     EJECT                                                                
306200 ECJ-KOMPLETTERA-PRIS SECTION.                                            
306300                                                                          
306400     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
306500                                                                          
306600     IF DIST79-DEALER-PRICE                                               
306700       IF WS-IDPRQUES                = +0                                 
306800          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
306900          MOVE +1                    TO PRNO-KDCALL                       
307000                                                                          
307100          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
307200                                                                          
307300          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
307400                                        WS-IDPRQUES                       
307500          MOVE +1                    TO PRQU-KDCALL                       
307600       ELSE                                                               
307700          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
307800          MOVE +2                    TO PRNO-KDCALL                       
307900                                                                          
308000          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
308100                                                                          
308200          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
308300                                        WS-IDPRQUES                       
308400          MOVE +2                    TO PRQU-KDCALL                       
308500       END-IF                                                             
308600                                                                          
308700       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
308800       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
308900       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
309000       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
309100       MOVE 'N'                      TO PRQU-KDPRSTA                      
309200       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
309300       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
309400       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
309500                                                                          
309600       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
309700       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
309800       PERFORM IMS-GU-WDB101                                              
309900       MOVE BET-KDVALISO             TO ORAD-KDVALISO                     
310000                                        PRQU-KDVALISO                     
310100                                                                          
310200       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
310300       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
310400       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
310500                                                                          
310600       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
310700                                          PRQU-WDC7-PCB                   
310800                                          PRQU-SJKO-WDK6-PCB              
310900                                                                          
311000       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
311100                                         WS-IDPRQUES                      
311200       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
311300                                                                          
311400       IF ORAD-PRARTNTO-LOC = +0                                          
311500          MOVE PRQU-PRARTNTO-LOCPREL TO ORAD-PRARTNTO-LOCPREL             
311600       END-IF                                                             
311700                                                                          
311800                                                                          
311900       IF ORAD-PRARTNTO-LOC NOT = +0                                      
312000         IF ORAD-KDPRTYP = SPACE                                          
312100           MOVE 'P'            TO ORAD-KDPRTYP                            
312200           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
312300         END-IF                                                           
312400       END-IF                                                             
312500                                                                          
312600     ELSE                                                                 
312700*        *NOT DIST79-DEALER-PRICE                                         
312800                                                                          
312900         PERFORM S10-HAMTA-WDB6-INFO                                      
313000         IF DCS-NDC OR DCS-SDC OR DCS-DDC OR                              
313100           (ORAD-KDTPOTYP = +0 AND AREG-KDUART = SPACE)                   
313200                                                                          
313300           IF ORAD-PRARTNTO NOT = +0                                      
313400*            *FETCH ONLY KDVALISO FROM W335PRIS                           
313500             MOVE 2                  TO PRIS-KDCALL                       
313600           ELSE                                                           
313700*            *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS          
313800             MOVE 1                  TO PRIS-KDCALL                       
313900           END-IF                                                         
314000         ELSE                                                             
314100             MOVE 2                  TO PRIS-KDCALL                       
314200         END-IF                                                           
314300                                                                          
314400         MOVE IDPGM                TO PRIS-IDPGM                          
314500         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
314600         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
314700         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
314800         MOVE ORAD-IDDC            TO PRIS-IDDC                           
314900         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
315000         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
315100         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
315200                                                                          
315300         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
315400                             PRIS-WDK7-PCB                                
315500                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
315600                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
315700                             PRIS-COST-WDK6-PCB                           
315800                             PRIS-COST-WDK7-PCB                           
315900                             PRIS-COST-WDF1-PCB                           
316000                             PRIS-COST-9305-PCB                           
316100                             PRIS-COST-WDK72-PCB                          
316200                             PRIS-COST-WDB6-PCB                           
316300                                                                          
316400         IF PRIS-KDSVAR = '2'                                             
316500           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
316600                              TO FELTEXT                                  
316700           CALL ABEND USING RKOD-ABEND                                    
316800         END-IF                                                           
316900                                                                          
317000         IF PRIS-KDCALL = 2                                               
317100           MOVE PRIS-KDVALISO    TO ORAD-KDVALISO                         
317200           IF ORAD-KDPRTYP = SPACE                                        
317300             MOVE 'P'            TO ORAD-KDPRTYP                          
317400             MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                           
317500           END-IF                                                         
317600         ELSE                                                             
317700           MOVE PRIS-PRARTNTO     TO ORAD-PRARTNTO                        
317800           MOVE PRIS-FLPRTILL     TO ORAD-FLPRTILL                        
317900           MOVE PRIS-KDPRTYP      TO ORAD-KDPRTYP                         
318000           MOVE PRIS-PRBPRIS      TO ORAD-PRBPRIS                         
318100           MOVE ORAD-TIREGDAT     TO ORAD-TIPRIS                          
318200           MOVE PRIS-KDVALISO     TO ORAD-KDVALISO                        
318300           MOVE PRIS-PRAVCOST     TO ORAD-PRAVCOST                        
318400         END-IF                                                           
318500     END-IF                                                               
318600     END-IF                                                               
318700     .                                                                    
318800     EJECT                                                                
318900 ECM-KOMPLETTERA-TPO1 SECTION.                                            
319000                                                                          
319100     PERFORM S10-HAMTA-WDB6-INFO                                          
319200                                                                          
319300     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
319400                     DCS-SDC)                                             
319500                                                                          
319600     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
319700     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
319800     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
319900     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
320000     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
320100     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
320200     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
320300     MOVE OHUV-IDKST           TO TPO1-IDKST                              
320400     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
320500     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
320600     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
320700     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
320800     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
320900     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
321000     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
321100     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
321200     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
321300     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
321400     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
321500     MOVE ORAD-TITPO           TO TPO1-TITPO                              
321600     IF ORAD-KDPRTYP = 'P'                                                
321700        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
321800        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
321900        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
322000        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
322100     ELSE                                                                 
322200        MOVE ZERO              TO TPO1-PRARTNTO                           
322300                                  TPO1-PRARTNTO-LOC                       
322400                                  TPO1-PRARTNTO-LOCPREL                   
322500        MOVE SPACE             TO TPO1-KDPRTYP                            
322600        MOVE NEJ               TO TPO1-FLPRTILL                           
322700     END-IF                                                               
322800     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
322900     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
323000     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
323100     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
323200     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
323300     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
323400     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
323500     MOVE +1                   TO TPO1-KDORDBEH                           
323600     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
323700     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
323800     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
323900     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
324000     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
324100                                                                          
324200     MOVE +0                   TO TPO1-KDORDBEK                           
324300     MOVE SPACE                TO TPO1-FLKLAR                             
324400                                                                          
324500     MOVE OHUV-KDORDTYP-LDC   TO TPO1-KDORDTYP-LDC                        
324600     MOVE OHUV-TIREPDAT       TO TPO1-TIREPDAT                            
324700     MOVE ORAD-IDKUNDRF-WIP   TO TPO1-IDKUNDRF-WIP                        
324800                                                                          
324900     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
325000                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
325100                                                                          
325200     IF TPO1-KDORDBEK > +0                                                
325300        MOVE JA                TO OBKR-SW                                 
325400        MOVE NEJ               TO ALLT-SW                                 
325500        MOVE WC-CDC-SE         TO ORAD-IDDC                               
325600        IF KVAN-KDORDBEK-UT > +0                                          
325700           MOVE +0             TO KVAN-KDORDBEK-UT                        
325800           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
325900        END-IF                                                            
326000        IF DLEV-KDORDBEK-UT > +0                                          
326100           MOVE +0             TO DLEV-KDORDBEK-UT                        
326200        END-IF                                                            
326300     ELSE                                                                 
326400        IF TPO1-FLKLAR = JA                                               
326500           MOVE NEJ            TO ALLT-SW                                 
326600           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
326700              MOVE ZERO        TO KERS-KDORDBEK                           
326800              PERFORM S02-RENSA-TILLK-TAB                                 
326900              MOVE NEJ         TO TILLK-SW                                
327000           END-IF                                                         
327100        END-IF                                                            
327200     END-IF                                                               
327300                                                                          
327400     END-IF                                                               
327500     .                                                                    
327600     EJECT                                                                
327700 ECN-KOMPLETTERA-TPO2 SECTION.                                            
327800                                                                          
327900     PERFORM S10-HAMTA-WDB6-INFO                                          
328000                                                                          
328100     IF DCS-SDC           AND                                             
328200        ORAD-KDORDKL  = 1 AND                                             
328300        ORAD-IDKAMPRF = 0 AND                                             
328400       (AREG-KDUART   = 'L' OR  AREG-KDUART = 'P')                        
328500                                                                          
328600       CONTINUE                                                           
328700     ELSE                                                                 
328800       IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                           
328900                       DCS-SDC)                                           
329000                                                                          
329100       MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                          
329200       MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                         
329300       MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                         
329400       MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                          
329500       MOVE ORAD-BERADREF        TO TPO2-BERADREF                         
329600       MOVE AREG-IDANSK          TO TPO2-IDANSK                           
329700       MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                          
329800       MOVE OHUV-IDKST           TO TPO2-IDKST                            
329900       MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                         
330000       MOVE ORAD-KDDSP           TO TPO2-KDDSP                            
330100       MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                         
330200       MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                          
330300       MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                         
330400       MOVE ORAD-KDORDING        TO TPO2-KDORDING                         
330500       MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                          
330600       MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                         
330700       MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                         
330800       MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                        
330900       MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                         
331000       MOVE ORAD-TITPO           TO TPO2-TITPO                            
331100       MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                         
331200       MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                     
331300       MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                          
331400       MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                         
331500       MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                         
331600       MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                         
331700       MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                         
331800       MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                         
331900       MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                         
332000       MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                         
332100       MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                          
332200       MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                          
332300       MOVE AREG-KDUART          TO TPO2-KDUART                           
332400       MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                         
332500       MOVE +1                   TO TPO2-KDORDBEH                         
332600       MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                         
332700       MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                         
332800       MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                          
332900       MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                         
333000       MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                         
333100       MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                        
333200       MOVE ORAD-KVBEART         TO TPO2-KVBEART                          
333300                                                                          
333400       MOVE +0                   TO TPO2-KDORDBEK                         
333500       MOVE SPACE                TO TPO2-FLKLAR                           
333600                                                                          
333700       MOVE OHUV-KDORDTYP-LDC   TO TPO2-KDORDTYP-LDC                      
333800       MOVE OHUV-TIREPDAT       TO TPO2-TIREPDAT                          
333900       MOVE ORAD-IDKUNDRF-WIP   TO TPO2-IDKUNDRF-WIP                      
334000                                                                          
334100       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
334200                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
334300                         TPO2-ARTM-PCB TPO2-FILA-PCB TIME-4437-PCB        
334400                         TPO2-XXBX-PCB                                    
334500                                                                          
334600       IF TPO2-KDORDBEK > +0                                              
334700          MOVE JA                TO OBKR-SW                               
334800          MOVE NEJ               TO ALLT-SW                               
334900          MOVE WC-CDC-SE         TO ORAD-IDDC                             
335000          MOVE ORAD-IDDC         TO WS-IDDC                               
335100          MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                         
335200          IF ORAD-KDTPOTYP = 6                                            
335300             IF ORAD-KDPRTYP NOT = 'P'                                    
335400                MOVE ZERO        TO ORAD-PRARTNTO                         
335500*                                   ORAD-PRARTNTO-LOC                     
335600*                                   ORAD-PRARTNTO-LOCPREL                 
335700                MOVE SPACE       TO ORAD-KDPRTYP                          
335800**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030613                      
335900                IF NOT DIST79-DEALER-PRICE                                
336000                  MOVE ZERO        TO ORAD-PRARTNTO-LOC                   
336100                  MOVE NEJ         TO ORAD-FLPRTILL                       
336200                END-IF                                                    
336300*************                                                             
336400             END-IF                                                       
336500          END-IF                                                          
336600       ELSE                                                               
336700          IF TPO2-FLKLAR = JA                                             
336800             MOVE NEJ            TO ALLT-SW                               
336900             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
337000                MOVE ZERO        TO KERS-KDORDBEK                         
337100                PERFORM S02-RENSA-TILLK-TAB                               
337200                MOVE NEJ         TO TILLK-SW                              
337300             END-IF                                                       
337400          END-IF                                                          
337500       END-IF                                                             
337600                                                                          
337700       END-IF                                                             
337800     END-IF                                                               
337900     .                                                                    
338000     EJECT                                                                
338100 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
338200                                                                          
338300     PERFORM S10-HAMTA-WDB6-INFO                                          
338400                                                                          
338500     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
338600                     DCS-SDC)                                             
338700                                                                          
338800       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
338900       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
339000       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
339100       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
339200       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
339300       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
339400       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
339500       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
339600       MOVE OHUV-IDKST         TO KAMP-IDKST                              
339700       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
339800       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
339900       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
340000       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
340100       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
340200       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
340300       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
340400       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
340500       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
340600       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
340700       MOVE ORAD-TITPO         TO KAMP-TITPO                              
340800       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
340900          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
341000          MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                     
341100          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
341200          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
341300       ELSE                                                               
341400          MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                     
341500          MOVE ZERO            TO KAMP-PRARTNTO                           
341600          MOVE SPACE           TO KAMP-KDPRTYP                            
341700          MOVE NEJ             TO KAMP-FLPRTILL                           
341800       END-IF                                                             
341900       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
342000       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
342100       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
342200       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
342300       MOVE ORAD-IDDC          TO KAMP-IDDC                               
342400       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
342500       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
342600       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
342700       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
342800       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
342900       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
343000       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
343100                                                                          
343200       MOVE +0                 TO KAMP-KDORDBEK                           
343300       MOVE SPACE              TO KAMP-FLKLAR                             
343400                                                                          
343500       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
343600                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
343700                                                                          
343800       IF KAMP-KDORDBEK > +0                                              
343900          MOVE JA              TO OBKR-SW                                 
344000          MOVE NEJ             TO ALLT-SW                                 
344100          MOVE WC-CDC-SE       TO ORAD-IDDC                               
344200          IF KVAN-KDORDBEK-UT > +0                                        
344300             MOVE +0           TO KVAN-KDORDBEK-UT                        
344400             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
344500          END-IF                                                          
344600          IF DLEV-KDORDBEK-UT > +0                                        
344700             MOVE +0           TO DLEV-KDORDBEK-UT                        
344800          END-IF                                                          
344900       ELSE                                                               
345000          IF KAMP-FLKLAR = JA                                             
345100             MOVE NEJ          TO ALLT-SW                                 
345200             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
345300                MOVE ZERO      TO KERS-KDORDBEK                           
345400                PERFORM S02-RENSA-TILLK-TAB                               
345500                MOVE NEJ       TO TILLK-SW                                
345600             END-IF                                                       
345700          END-IF                                                          
345800       END-IF                                                             
345900                                                                          
346000     END-IF                                                               
346100     .                                                                    
346200     EJECT                                                                
346300 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
346400                                                                          
346500       IF ALLT-OK AND W-KDORDBEK = 56                                     
346600                                                                          
346700         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
346800         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
346900         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
347000         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
347100         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
347200         MOVE AREG-IDANSK          TO RELS-IDANSK                         
347300         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
347400         MOVE OHUV-IDKST           TO RELS-IDKST                          
347500         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
347600         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
347700         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
347800         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
347900         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
348000         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
348100         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
348200         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
348300         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
348400         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
348500         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
348600         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
348700         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
348800         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
348900         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
349000         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
349100         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
349200         EJECT                                                            
349300         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
349400         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
349500         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
349600         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
349700         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
349800         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
349900         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
350000         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
350100         MOVE AREG-KDUART          TO RELS-KDUART                         
350200         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
350300         MOVE +1                   TO RELS-KDORDBEH                       
350400         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
350500         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
350600         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
350700         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
350800         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
350900         MOVE 0                    TO RELS-KVQPACK-1                      
351000         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
351100         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
351200                                                                          
351300         MOVE OHUV-KDORDTYP-LDC   TO RELS-KDORDTYP-LDC                    
351400         MOVE OHUV-TIREPDAT       TO RELS-TIREPDAT                        
351500         MOVE ORAD-IDKUNDRF-WIP   TO RELS-IDKUNDRF-WIP                    
351600                                                                          
351700         MOVE SPACE                TO RELS-FLKLAR                         
351800                                                                          
351900         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
352000                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
352100                                                                          
352200         PERFORM ECTA-ANDRA-WDC711                                        
352300         IF RELS-KDORDBEK > +0                                            
352400            MOVE JA                TO OBKR-SW                             
352500            MOVE NEJ               TO ALLT-SW                             
352600            MOVE WC-CDC-SE         TO ORAD-IDDC                           
352700            MOVE ORAD-IDDC         TO WS-IDDC                             
352800         ELSE                                                             
352900            IF RELS-FLKLAR = JA                                           
353000               MOVE NEJ            TO ALLT-SW                             
353100               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
353200                  MOVE ZERO        TO KERS-KDORDBEK                       
353300                  PERFORM S02-RENSA-TILLK-TAB                             
353400               END-IF                                                     
353500            END-IF                                                        
353600         END-IF                                                           
353700                                                                          
353800       MOVE +0               TO W-KDORDBEK                                
353900       MOVE SPACE            TO RELS-FLKLAR                               
354000                                                                          
354100       END-IF                                                             
354200     .                                                                    
354300     EJECT                                                                
354400 ECTA-ANDRA-WDC711 SECTION.                                               
354500                                                                          
354600     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
354700       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
354800         INITIALIZE PRQU-W335PRQU                                         
354900         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
355000         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
355100         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
355200         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
355300         MOVE 6                  TO PRQU-KDCALL                           
355400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
355500                                            PRQU-WDC7-PCB                 
355600                                            PRQU-SJKO-WDK6-PCB            
355700         MOVE 'N'                TO ORAD-FLPRTILL                         
355800       END-IF                                                             
355900     END-IF                                                               
356000     .                                                                    
356100     EJECT                                                                
356200                                                                          
356300 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
356400                                                                          
356500*    IF ALLT-OK                                                           
356600     IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                    
356700                                                                          
356800       MOVE ORAD-BERADREF      TO RANS-BERADREF                           
356900       MOVE OHUV-FLEMBORD      TO RANS-FLEMBORD                           
357000       MOVE OHUV-FLFORBI       TO RANS-FLFORBI                            
357100       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
357200       MOVE OHUV-FLOVRLEV      TO RANS-FLOVRLEV                           
357300       MOVE ORAD-IDKAMPRF      TO RANS-IDKAMPRF                           
357400       MOVE ORAD-IDARTNR       TO RANS-IDARTNR                            
357500       MOVE ORAD-IDLEVNR       TO RANS-IDLEVNR                            
357600       MOVE OHUV-IDRFTAB       TO RANS-IDRFTAB                            
357700       MOVE ORAD-TIRODAT       TO RANS-TIRODAT                            
357800       MOVE OHUV-KDORDKL       TO RANS-KDORDKL                            
357900       MOVE +1                 TO RANS-KDORDBEH                           
358000       MOVE ORAD-KVBEART-Q     TO RANS-KVBEART-Q                          
358100       MOVE ORAD-KDTPOTYP      TO RANS-KDTPOTYP                           
358200       MOVE AREG-KDERS         TO RANS-KDERS                              
358300       MOVE AREG-KVLS          TO RANS-KVLS                               
358400       MOVE AREG-KVPB-SATS     TO RANS-KVPB-SATS                          
358500       MOVE AREG-KVPB-SEP      TO RANS-KVPB-SEP                           
358600       MOVE AREG-REDIRLEV      TO RANS-REDIRLEV                           
358700       MOVE AREG-KVRESS        TO RANS-KVRESS                             
358800       MOVE AREG-KVSPANT       TO RANS-KVSPANT                            
358900       MOVE AREG-KVUTRS        TO RANS-KVUTRS                             
359000       MOVE AREG-TIDISPIN      TO RANS-TIDISPIN                           
359100                                                                          
359200       MOVE AREG-KDPRODSL      TO TEST-KDPRODSL                           
359300       IF KDPRODSL-BIMA                                                   
359400         MOVE 1                TO ORAD-RERF-RAD                           
359500                                  RANS-RERF-RAD-UT                        
359600         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
359700                                  RANS-SUTPO-EJPB-UT                      
359800                                  RANS-RERF-ART-UT                        
359900       ELSE                                                               
360000         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
360100                             RANS-ARTM-PCB RANS-ARTS-PCB                  
360200                                                                          
360300         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
360400       END-IF                                                             
360500     ELSE                                                                 
360600       MOVE +0                 TO RANS-RERF-RAD-UT                        
360700       MOVE +0                 TO RANS-RERF-ART-UT                        
360800     END-IF                                                               
360900     .                                                                    
361000     EJECT                                                                
361100 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
361200                                                                          
361300     IF ALLT-OK                                                           
361400                                                                          
361500     MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                           
361600     MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                            
361700     MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                        
361800     MOVE ORAD-BERADREF        TO STOR-BERADREF                           
361900     MOVE OHUV-FLFORBI         TO STOR-FLFORBI                            
362000     MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                           
362100     MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                           
362200     MOVE OHUV-KDORDKL         TO STOR-KDORDKL                            
362300     MOVE SPACE                TO STOR-KDPROTYP                           
362400     MOVE AREG-KDERS           TO STOR-KDERS                              
362500     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
362600     MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                          
362700     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
362800     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
362900     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
363000     MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                           
363100     MOVE ORAD-IDDISTR         TO STOR-IDDISTR                            
363200     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
363300                                                                          
363400     CALL W411STOR USING STOR-W411STOR                                    
363500                                                                          
363600     IF STOR-KDORDBEK > +0                                                
363700        MOVE +6                   TO ORAD-KDTPOTYP                        
363800        IF ORAD-KDPRTYP NOT = 'P'                                         
363900          MOVE +0                 TO ORAD-PRARTNTO                        
364000*                                    ORAD-PRARTNTO-LOC                    
364100*                                    ORAD-PRARTNTO-LOCPREL                
364200          MOVE SPACE              TO ORAD-KDPRTYP                         
364300**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030613                      
364400          IF NOT DIST79-DEALER-PRICE                                      
364500            MOVE ZERO        TO ORAD-PRARTNTO-LOC                         
364600            MOVE NEJ         TO ORAD-FLPRTILL                             
364700          END-IF                                                          
364800*************                                                             
364900        END-IF                                                            
365000        MOVE JA                   TO OBKR-SW                              
365100        MOVE NEJ                  TO ALLT-SW                              
365200     END-IF                                                               
365300                                                                          
365400     END-IF                                                               
365500     .                                                                    
365600     EJECT                                                                
365700 ECR-PREL-AVBOKNING-CDC SECTION.                                          
365800                                                                          
365900     IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                    
366000                                                                          
366100       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
366200                                 TO CDCA-FLFINLV-IN                       
366300       MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                       
366400       MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                      
366500       MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                      
366600       MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                      
366700       MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                       
366800       MOVE ORFK-FLSLATT(WS-INDEX-MID)                                    
366900                                 TO CDCA-FLSLATT-IN                       
367000       MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                       
367100       MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
367200       MOVE ORAD-IDDC            TO CDCA-IDDC-IN                          
367300       MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                      
367400       MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                   
367500       MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
367600       MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
367700       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
367800       MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
367900       MOVE SPACE                TO CDCA-KDPROTYP-IN                      
368000       MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                       
368100       MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
368200       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
368300       MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
368400       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
368500       MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                       
368600       MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
368700       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
368800       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
368900       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
369000       MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                      
369100       MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                      
369200       MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                       
369300       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
369400       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
369500       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
369600       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
369700       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
369800       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
369900       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
370000       MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
370100       MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                      
370200       MOVE +1                   TO CDCA-KDCALL                           
370300                                                                          
370400       IF KOLLA-ARBTAB-C1                                                 
370500         PERFORM ECRA-KOLLA-UTSKRIVET-LO                                  
370600       END-IF                                                             
370700       IF ALLT-OK  OR (TILLAEGG-TPO AND KERS-KDERS = 01)                  
370800         CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                  
370900                                           CDCA-INLB-PCB                  
371000                                           CDCA-WDB2-PCB                  
371100                                           CDCA-WDC1-PCB                  
371200                                                                          
371300         IF KERS-KDERS > 0 AND < 10                                       
371400            IF CDCA-KVPREAVB-UT = +0  AND  CDCA-KVPRERO-UT = +0           
371500               MOVE JA             TO OBKR-SW                             
371600            ELSE                                                          
371700               PERFORM S02-RENSA-TILLK-TAB                                
371800               MOVE ZERO           TO KERS-KDORDBEK                       
371900               MOVE NEJ            TO TILLK-SW                            
372000            END-IF                                                        
372100         END-IF                                                           
372200                                                                          
372300************                                                              
372400         IF NOT TILLAEGG-TPO                                              
372500*OM DET ÄR EN TILLÄGGSTPO SÅ SKALL INGEN RAD SKRIVAS. TL 050127           
372600           MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                     
372700                                                                          
372800           IF ORAD-IDLEVNR NOT = SPACE                                    
372900              CONTINUE                                                    
373000           ELSE                                                           
373100              MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                   
373200              MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                 
373300           END-IF                                                         
373400                                                                          
373500           MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                     
373600           MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                      
373700           MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                      
373800           MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                     
373900                                                                          
374000           IF CDCA-KDORDBEK-UT > ZERO                                     
374100              MOVE JA                TO OBKR-SW                           
374200           END-IF                                                         
374300           IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND         
374400               CDCA-KDORDBEK-UT  = +0 AND                                 
374500               KVAN-KDORDBEK-UT  = +0 AND                                 
374600               DLEV-KDORDBEK-UT  = +0 AND                                 
374700               KERS-KDORDBEK     = +0 AND                                 
374800               TPO1-KDORDBEK     = +0 AND                                 
374900               TPO2-KDORDBEK     = +0 AND                                 
375000               KAMP-KDORDBEK     = +0 AND                                 
375100               STOR-KDORDBEK     = +0 AND                                 
375200               XDCA-KDORDBEK     = +0 AND                                 
375300               SDCA-KDORDBEK     = +0 AND                                 
375400               SDCA-KDORDBEK-FIRST-SDC = +0 AND                           
375500               SDCA-KDORDBEK-SECOND-SDC = +0 AND                          
375600               SPAR-KDORDBEK     = ZERO                                   
375700               MOVE JA                  TO EGET-CL-RAD-SW                 
375800           END-IF                                                         
375900           MOVE AREG-ADLAGOMR       TO ORAD-ADLAGOMR                      
376000           MOVE AREG-ADGANG         TO ORAD-ADGANG                        
376100           MOVE AREG-ADPLATS        TO ORAD-ADPLATS                       
376200         END-IF                                                           
376300       END-IF                                                             
376400     END-IF                                                               
376500     .                                                                    
376600     EJECT                                                                
376700 ECRA-KOLLA-UTSKRIVET-LO SECTION.                                         
376800                                                                          
376900     MOVE NEJ       TO RAD-GODKAND-SW                                     
377000                                                                          
377100     MOVE ARB-IDDC         TO W-IDDC-WDQ212                               
377200     IF AREG-ADLAGOMR = +0                                                
377300        MOVE +1            TO W-ADLAGOMR                                  
377400     ELSE                                                                 
377500        MOVE AREG-ADLAGOMR TO W-ADLAGOMR                                  
377600     END-IF                                                               
377700                                                                          
377800     PERFORM IMS-GNP-ORQI-WDQ221                                          
377900     IF SEGMENT-FINNS                                                     
378000        MOVE JA               TO RAD-GODKAND-SW                           
378100     END-IF                                                               
378200                                                                          
378300     IF NOT RAD-GODKAND                                                   
378400        MOVE NEJ              TO ALLT-SW                                  
378500        MOVE JA               TO OBKR-SW                                  
378600     END-IF                                                               
378700     .                                                                    
378800     EJECT                                                                
378900 ECS-SKRIV-OBKR SECTION.                                                  
379000                                                                          
379100*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
379200*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT CLAGER 'SLÄPAR'              
379300*    ISRT AV RADEN.                                                       
379400*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
379500*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
379600*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
379700*---                                                                      
379800     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
379900                                                                          
380000     IF TILLKOMMANDE-RAD                                                  
380100        IF KERS-KDORDBEK = 41                                             
380200           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
380300           MOVE '4244KER1'        TO OBKR-IDPGM                           
380400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
380500                                  TO OBKR-KVBEART-TILLK                   
380600           COMPUTE OBKR-DIERS-KVOT =                                      
380700           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
380800                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
380900           MOVE 'S'               TO OBKR-SW                              
381000        END-IF                                                            
381100     END-IF                                                               
381200                                                                          
381300                                                                          
381400     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
381500       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
381600*----(KOD  58, 59, 98)                                                    
381700        IF OBKR-SKRIVEN                                                   
381800           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
381900           ADD +1              TO OBKR-IDSEKVNR                           
382000        END-IF                                                            
382100        IF ORFK-KDORDBEK(WS-INDEX-MID) = 98                               
382200          MOVE 82              TO OBKR-KDORDBEK                           
382300          MOVE IDPGM           TO OBKR-IDPGM                              
382400        ELSE                                                              
382500          MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                
382600                               TO OBKR-KDORDBEK                           
382700          MOVE '4244ORFK'      TO OBKR-IDPGM                              
382800        END-IF                                                            
382900        MOVE 'S'               TO OBKR-SW                                 
383000     END-IF                                                               
383100     EJECT                                                                
383200                                                                          
383300     IF KVAN-KDORDBEK-UT > +0                                             
383400*----(KOD 43, 44)                                                         
383500        IF OBKR-SKRIVEN                                                   
383600           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
383700           ADD +1              TO OBKR-IDSEKVNR                           
383800        END-IF                                                            
383900        IF TILLKOMMANDE-RAD                                               
384000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
384100                               TO OBKR-KVBEART-TILLK                      
384200           COMPUTE OBKR-DIERS-KVOT =                                      
384300           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
384400                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
384500        END-IF                                                            
384600        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
384700        MOVE '4244KVAN'        TO OBKR-IDPGM                              
384800        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
384900        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
385000        MOVE 'S'               TO OBKR-SW                                 
385100     END-IF                                                               
385200     EJECT                                                                
385300                                                                          
385400     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
385500*----(KOD 21, 53, 82, 95) ,26                                             
385600        IF OBKR-SKRIVEN                                                   
385700           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
385800           ADD +1              TO OBKR-IDSEKVNR                           
385900        END-IF                                                            
386000        IF TILLKOMMANDE-RAD                                               
386100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
386200                               TO OBKR-KVBEART-TILLK                      
386300           COMPUTE OBKR-DIERS-KVOT =                                      
386400           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
386500                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
386600        END-IF                                                            
386700        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
386800        MOVE '4244DLEV'        TO OBKR-IDPGM                              
386900        MOVE 'S'               TO OBKR-SW                                 
387000     END-IF                                                               
387100     EJECT                                                                
387200     IF KERS-KDORDBEK > +0                                                
387300*----(KOD 41, 61)                                                         
387400                                                                          
387500        IF KERS-KDORDBEK = 61                                             
387600*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
387700*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
387800*----RADBEHANDLINGEN                                                      
387900           IF OBKR-SKRIVEN                                                
388000              PERFORM IMS-11-ISRT-WLORQM01-WDQ101                         
388100              ADD +1           TO OBKR-IDSEKVNR                           
388200           END-IF                                                         
388300           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
388400           MOVE '4244KER2'     TO OBKR-IDPGM                              
388500           MOVE 'S'            TO OBKR-SW                                 
388600           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
388700           PERFORM S02-RENSA-TILLK-TAB                                    
388800        ELSE                                                              
388900           IF NOT TILLKOMMANDE-RAD                                        
389000              IF OBKR-SKRIVEN                                             
389100                 PERFORM IMS-11-ISRT-WLORQM01-WDQ101                      
389200                 ADD +1        TO OBKR-IDSEKVNR                           
389300              END-IF                                                      
389400              MOVE 'S'            TO OBKR-SW                              
389500              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
389600              MOVE '4244KER3'     TO OBKR-IDPGM                           
389700           END-IF                                                         
389800        END-IF                                                            
389900     END-IF                                                               
390000     EJECT                                                                
390100     IF SPAR-KDORDBEK                > +0                                 
390200*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90, 92)                 
390300        IF OBKR-SKRIVEN                                                   
390400           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
390500           ADD +1              TO OBKR-IDSEKVNR                           
390600        END-IF                                                            
390700        IF TILLKOMMANDE-RAD                                               
390800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
390900                               TO OBKR-KVBEART-TILLK                      
324610          IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                      
324620             TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                           
324630            MOVE +0            TO OBKR-DIERS-KVOT                         
324640          ELSE                                                            
391000           COMPUTE OBKR-DIERS-KVOT =                                      
391100           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
391200                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
391300          END-IF                                                          
391300        END-IF                                                            
391400        MOVE SPAR-KDORDBEK                                                
391500                               TO OBKR-KDORDBEK                           
391600        MOVE '4244SPAR'        TO OBKR-IDPGM                              
391700        MOVE 'S'               TO OBKR-SW                                 
391800     END-IF                                                               
391900     EJECT                                                                
392000                                                                          
392100     IF TPO1-KDORDBEK > +0                                                
392200*----(KOD 72, 73, 74, 85)                                                 
392300        IF OBKR-SKRIVEN                                                   
392400           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
392500           ADD +1              TO OBKR-IDSEKVNR                           
392600        END-IF                                                            
392700        IF TILLKOMMANDE-RAD                                               
392800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
392900                               TO OBKR-KVBEART-TILLK                      
393000           COMPUTE OBKR-DIERS-KVOT =                                      
393100           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
393200                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
393300        END-IF                                                            
393400        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
393500        MOVE '4244TPO1'        TO OBKR-IDPGM                              
393600        IF TPO1-KDORDBEK = 85                                             
393700           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
393800        END-IF                                                            
393900        MOVE 'S'               TO OBKR-SW                                 
394000     END-IF                                                               
394100     EJECT                                                                
394200     IF TPO2-KDORDBEK > +0                                                
394300*----(KOD 70)                                                             
394400        IF OBKR-SKRIVEN                                                   
394500           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
394600           ADD +1              TO OBKR-IDSEKVNR                           
394700        END-IF                                                            
394800        IF TILLKOMMANDE-RAD                                               
394900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
395000                               TO OBKR-KVBEART-TILLK                      
395100           COMPUTE OBKR-DIERS-KVOT =                                      
395200           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
395300                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
395400        END-IF                                                            
395500        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
395600        MOVE '4244TPO2'        TO OBKR-IDPGM                              
395700        MOVE 'S'               TO OBKR-SW                                 
395800     END-IF                                                               
395900     EJECT                                                                
396000                                                                          
396100     IF KAMP-KDORDBEK > +0                                                
396200*----(KOD 72, 75, 76)                                                     
396300        IF OBKR-SKRIVEN                                                   
396400           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
396500           ADD +1              TO OBKR-IDSEKVNR                           
396600        END-IF                                                            
396700        IF TILLKOMMANDE-RAD                                               
396800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
396900                               TO OBKR-KVBEART-TILLK                      
397000           COMPUTE OBKR-DIERS-KVOT =                                      
397100           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
397200                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
397300        END-IF                                                            
397400        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
397500        MOVE '4244KAMP'        TO OBKR-IDPGM                              
397600        MOVE 'S'               TO OBKR-SW                                 
397700     END-IF                                                               
397800     EJECT                                                                
397900                                                                          
398000     IF RELS-KDORDBEK > 0                                                 
398100*----(KOD 56)                                                             
398200          IF OBKR-SKRIVEN                                                 
398300             PERFORM IMS-11-ISRT-WLORQM01-WDQ101                          
398400             ADD +1              TO OBKR-IDSEKVNR                         
398500          END-IF                                                          
398600          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
398700          MOVE '4244ORFK'        TO OBKR-IDPGM                            
398800          MOVE 'S'               TO OBKR-SW                               
398900     END-IF                                                               
399000    EJECT                                                                 
399100     IF XDCA-KDORDBEK > ZERO                                              
399200*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
399300        IF OBKR-SKRIVEN                                                   
399400           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
399500           ADD +1              TO OBKR-IDSEKVNR                           
399600        END-IF                                                            
399700        IF TILLKOMMANDE-RAD                                               
399800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
399900                               TO OBKR-KVBEART-TILLK                      
400000           COMPUTE OBKR-DIERS-KVOT =                                      
400100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
400200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
400300        END-IF                                                            
400400                                                                          
400500        IF XDCA-KDORDBEK NOT = 15                                         
400600          IF OHUV-IDDC-TVS = SPACE                                        
400700            IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                     
400800              MOVE 15          TO OBKR-KDORDBEK                           
400900              MOVE IDPGM       TO OBKR-IDPGM                              
401000              MOVE 'S'         TO OBKR-SW                                 
401100                                                                          
401200* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
401300              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
401400                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
401500              END-IF                                                      
401600              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
401700                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
401800              END-IF                                                      
401900              IF SDCA-KDORDBEK = 15                                       
402000                 MOVE ZERO     TO SDCA-KDORDBEK                           
402100              END-IF                                                      
402200            END-IF                                                        
402300            IF OBKR-SKRIVEN                                               
402400              PERFORM IMS-11-ISRT-WLORQM01-WDQ101                         
402500              ADD +1           TO OBKR-IDSEKVNR                           
402600            END-IF                                                        
402700          END-IF                                                          
402800        END-IF                                                            
402900        IF XDCA-KDORDBEK = 80                                             
403000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
403100        END-IF                                                            
403200        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
403300        MOVE '4244XDCA'           TO OBKR-IDPGM                           
403400        MOVE 'S'               TO OBKR-SW                                 
403500     END-IF                                                               
403600     EJECT                                                                
403700                                                                          
403800     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
403900*----(KOD 15, 53, 80, 92)                                                 
404000        IF OBKR-SKRIVEN                                                   
404100           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
404200           ADD +1              TO OBKR-IDSEKVNR                           
404300        END-IF                                                            
404400        IF TILLKOMMANDE-RAD                                               
404500           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
404600                               TO OBKR-KVBEART-TILLK                      
404700           COMPUTE OBKR-DIERS-KVOT =                                      
404800                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
404900                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
405000        END-IF                                                            
405100        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
405200           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
405300        END-IF                                                            
405400        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
405500        MOVE '4244SDCA'           TO OBKR-IDPGM                           
405600        MOVE 'S'               TO OBKR-SW                                 
405700     END-IF                                                               
405800     EJECT                                                                
405900                                                                          
406000     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
406100*----(KOD 15, 53, 80, 92)                                                 
406200        IF OBKR-SKRIVEN                                                   
406300           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
406400           ADD +1              TO OBKR-IDSEKVNR                           
406500        END-IF                                                            
406600        IF TILLKOMMANDE-RAD                                               
406700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
406800                               TO OBKR-KVBEART-TILLK                      
406900           COMPUTE OBKR-DIERS-KVOT =                                      
407000                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
407100                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
407200        END-IF                                                            
407300        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
407400           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
407500        END-IF                                                            
407600        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
407700        MOVE '4244SDCA'           TO OBKR-IDPGM                           
407800        MOVE 'S'               TO OBKR-SW                                 
407900     END-IF                                                               
408000     EJECT                                                                
408100                                                                          
408200     IF SDCA-KDORDBEK > ZERO                                              
408300*----(KOD 15, 53, 80, 92)                                                 
408400        IF OBKR-SKRIVEN                                                   
408500           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
408600           ADD +1              TO OBKR-IDSEKVNR                           
408700        END-IF                                                            
408800        IF TILLKOMMANDE-RAD                                               
408900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
409000                               TO OBKR-KVBEART-TILLK                      
409100           COMPUTE OBKR-DIERS-KVOT =                                      
409200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
409300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
409400        END-IF                                                            
409500        IF SDCA-KDORDBEK = 80                                             
409600           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
409700        END-IF                                                            
409800        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
409900        MOVE '4244SDCA'           TO OBKR-IDPGM                           
410000        MOVE 'S'               TO OBKR-SW                                 
410100     END-IF                                                               
410200     EJECT                                                                
410300                                                                          
410400     IF STOR-KDORDBEK > +0                                                
410500*----(KOD 70)                                                             
410600        IF OBKR-SKRIVEN                                                   
410700           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
410800           ADD +1              TO OBKR-IDSEKVNR                           
410900        END-IF                                                            
411000        IF TILLKOMMANDE-RAD                                               
411100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
411200                               TO OBKR-KVBEART-TILLK                      
411300           COMPUTE OBKR-DIERS-KVOT =                                      
411400           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
411500                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
411600        END-IF                                                            
411700        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
411800        MOVE '4244STOR'           TO OBKR-IDPGM                           
411900        MOVE 'S'               TO OBKR-SW                                 
412000     END-IF                                                               
412100     EJECT                                                                
412200     IF CDCA-KDORDBEK-UT > +0                                             
412300*----(KOD 80, 92, 99)                                                     
412400        IF OBKR-SKRIVEN                                                   
412500           PERFORM IMS-11-ISRT-WLORQM01-WDQ101                            
412600           ADD +1              TO OBKR-IDSEKVNR                           
412700        END-IF                                                            
412800        IF TILLKOMMANDE-RAD                                               
412900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
413000                               TO OBKR-KVBEART-TILLK                      
413100           COMPUTE OBKR-DIERS-KVOT =                                      
413200           TILK-DIERS-TILLK(WS-INDEX-TILLK) /                             
413300                                  TILK-DIERS-ERS(WS-INDEX-TILLK)          
413400        END-IF                                                            
413500        IF CDCA-KDORDBEK-UT = +80                                         
413600           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
413700        END-IF                                                            
413800        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
413900        MOVE '4244CDCA'           TO OBKR-IDPGM                           
414000        MOVE 'S'               TO OBKR-SW                                 
414100     END-IF                                                               
414200* PÅ SISTA RADEN FÖR KUNDENS NORMALA CLAGER LÄGGS DE AVBOKADE             
414300* ANTALEN!                                                                
414400     IF OBKR-SKRIVEN                                                      
414500         MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                          
414600         MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                           
414700******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
414800         IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                    
414900           PERFORM S05-DELETE-PRICE-Q-LINE                                
415000           INITIALIZE OBKR-DEAL-PR-LINE                                   
415100         END-IF                                                           
415200*************TL 030514                                                    
415300        PERFORM IMS-11-ISRT-WLORQM01-WDQ101                               
415400        ADD +1                 TO OBKR-IDSEKVNR                           
415500     END-IF                                                               
415600                                                                          
415700                                                                          
415800*** TILLÄGGSTPO SKAPAS                                                    
415900     IF NOT OBKR-SKRIVEN OR                                               
416000       (KVAN-KDORDBEK-UT > ZERO AND STOR-KDORDBEK = ZERO)  OR             
416100       (TILLKOMMANDE-RAD AND ORFK-KDORDBEK(WS-INDEX-MID) = ZERO           
416200        AND DLEV-KDORDBEK-UT = ZERO                                       
416300        AND SPAR-KDORDBEK    = ZERO                                       
416400        AND TPO1-KDORDBEK = ZERO AND TPO2-KDORDBEK = ZERO                 
416500        AND KAMP-KDORDBEK = ZERO                                          
416600        AND STOR-KDORDBEK = ZERO AND CDCA-KDORDBEK-UT = ZERO )            
416700        IF NOT RAD-GODKAND                                                
416800           IF TPO1-FLKLAR = NEJ AND TPO2-FLKLAR = NEJ AND                 
416900              KAMP-FLKLAR = NEJ                                           
417000              PERFORM ECSB-SKAPA-TPO2                                     
417100           END-IF                                                         
417200        END-IF                                                            
417300     END-IF                                                               
417400     .                                                                    
417500     EJECT                                                                
417600 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
417700                                                                          
417800     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
417900     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
418000                               TO OBKR-IDARTNR                            
418100     IF NOT TILLKOMMANDE-RAD                                              
418200        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
418300                                  W-IDORDER-Q1-MAX                        
418400        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
418500                                  W-IDARTNR-Q1-MAX                        
418600        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
418700                                  W-IDLOPNR-Q1-MAX                        
418800                                  W-IDSEKVNR-Q1-MIN                       
418900                                  W-IDSEKVNR-Q1-MAX                       
419000        PERFORM IMS-07-GU-ORQM-WDQ101                                     
419100        PERFORM UNTIL SEGMENT-SAKNAS                                      
419200           ADD +1              TO W-IDLOPNR-Q1-MIN                        
419300                                  W-IDLOPNR-Q1-MAX                        
419400           PERFORM IMS-07-GU-ORQM-WDQ101                                  
419500        END-PERFORM                                                       
419600        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
419700        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
419800     END-IF                                                               
419900     IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                                
420000        IF OHUV-IDDC-TVS > ZERO                                           
420100           MOVE OHUV-IDDC-TVS       TO OBKR-IDDC                          
420200                                       ORAD-IDDC                          
420300        ELSE                                                              
420400          MOVE OHUV-IDDC-PRIM     TO OBKR-IDDC                            
420500                                       ORAD-IDDC                          
420600        END-IF                                                            
420700     ELSE                                                                 
420800        MOVE ORAD-IDDC         TO OBKR-IDDC                               
420900     END-IF                                                               
421000     IF WS-IDDC-DDGS NOT = SPACE                                          
421100       MOVE WS-IDDC-DDGS       TO OBKR-IDDC                               
421200     END-IF                                                               
421300     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
421400     MOVE +0                   TO OBKR-KDORDBEK                           
421500     MOVE SPACE                TO OBKR-BEERS                              
421600     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
421700     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
421800     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
421900     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
422000     MOVE +0                   TO OBKR-DIERS-KVOT                         
422100     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
422200     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
422300     MOVE NEJ                  TO OBKR-FLOBOK                             
422400     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
422500     MOVE NEJ                  TO OBKR-FLOBPRT                            
422600     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
422700     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
422800     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
422900                               TO OBKR-FLSLATT                            
423000     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
423100     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
423200        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
423300     ELSE                                                                 
423400        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
423500                               TO OBKR-REKSIFFR                           
423600     END-IF                                                               
423700     IF TILLKOMMANDE-RAD                                                  
423800        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
423900                               TO OBKR-IDARTNR-TILLK                      
424000        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
424100                               TO OBKR-REKSIFFR-TILLK                     
424200     ELSE                                                                 
424300        MOVE +0                TO OBKR-IDARTNR-TILLK                      
424400        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
424500     END-IF                                                               
424600     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
424700     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
424800     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
424900     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
425000     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
425100     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
425200     IF NOT TILLKOMMANDE-RAD                                              
425300        MOVE AREG-KDERS        TO OBKR-KDERS                              
425400     END-IF                                                               
425500     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
425600     MOVE ORAD-KDOI            TO OBKR-KDOI                               
425700     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
425800     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
425900     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
426000     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
426100     MOVE +0                   TO OBKR-KVANNANT                           
426200     MOVE +0                   TO OBKR-KVAVBART                           
426300     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
426400     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
426500     MOVE +0                   TO OBKR-KVBEART-TILLK                      
426600     MOVE +0                   TO OBKR-KVPREAVB                           
426700     MOVE +0                   TO OBKR-KVPRERO                            
426800     IF ALLT-OK                                                           
426900       MOVE KVAN-KVQPACK-UT    TO OBKR-KVQPACK                            
427000     ELSE                                                                 
427100       MOVE +0                 TO OBKR-KVQPACK                            
427200     END-IF                                                               
427300     MOVE +0                   TO OBKR-KVRO                               
427400     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
427500     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
427600     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
427700     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
427800     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
427900     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
428000     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
428100     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
428200     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
428300     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
428400     MOVE +0                   TO OBKR-TIRODAT                            
428500     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
428600     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
428700       MOVE 20                 TO WS-SEKEL-9KOMPL                         
428800     ELSE                                                                 
428900       MOVE 19                 TO WS-SEKEL-9KOMPL                         
429000     END-IF                                                               
429100     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
429200     MOVE ORAD-TITPO           TO OBKR-TITPO                              
429300     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
429400     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
429500     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
429600     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
429700     MOVE SPACE                TO OBKR-IDBIL                              
429800                                                                          
429900     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
430000     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
430100     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
430200     MOVE ZERO                TO OBKR-TIDLEVDAT                           
430300     MOVE ORAD-PRAVCOST       TO OBKR-PRAVCOST                            
430400     MOVE ORAD-KDVALISO       TO OBKR-KDVALISO                            
430500     .                                                                    
430600     EJECT                                                                
430700 ECSB-SKAPA-TPO2 SECTION.                                                 
430800                                                                          
430900     MOVE +2                   TO OBKR-KDTPOTYP                           
431000     MOVE +71                  TO OBKR-KDORDBEK                           
431100     MOVE IDPGM                TO OBKR-IDPGM                              
431200     MOVE ORAD-TITPO           TO OBKR-TITPO                              
431300     IF ORAD-KDORDING = +3                                                
431400       MOVE SPACE              TO OBKR-KDOI                               
431500     ELSE                                                                 
431600       MOVE ORAD-KDPRODSL      TO TEST-KDPRODSL                           
431700       IF KDPRODSL-VOLVO-EMB                                              
431800         MOVE 'CD'             TO OBKR-KDOI                               
431900       ELSE                                                               
432000         MOVE 'DT'             TO OBKR-KDOI                               
432100       END-IF                                                             
432200     END-IF                                                               
432300     MOVE SPACE                TO OBKR-CLEARGROUP                         
432400     MOVE NEJ                  TO ALLT-SW                                 
432500     IF TILLKOMMANDE-RAD                                                  
432600        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
432700                               TO OBKR-KVBEART-TILLK                      
432800        COMPUTE OBKR-DIERS-KVOT =                                         
432900                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
433000                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
433100     END-IF                                                               
433200     MOVE 'S'                  TO OBKR-SW                                 
433300     PERFORM IMS-11-ISRT-WLORQM01-WDQ101                                  
433400     ADD +1                    TO OBKR-IDSEKVNR                           
433500     .                                                                    
433600     EJECT                                                                
433700 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
433800                                                                          
433900     MOVE +1                   TO WS-INDEX-TILLK                          
434000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
434100                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
434200        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
434300           IF OBKR-SKRIVEN                                                
434400              PERFORM IMS-11-ISRT-WLORQM01-WDQ101                         
434500              ADD +1              TO OBKR-IDSEKVNR                        
434600           END-IF                                                         
434700           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
434800           MOVE '4244KER4'     TO OBKR-IDPGM                              
434900           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
435000                               TO OBKR-IDARTNR-TILLK                      
435100           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
435200                               TO OBKR-REKSIFFR-TILLK                     
435300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
435400                               TO OBKR-KVBEART-TILLK                      
435500           COMPUTE OBKR-DIERS-KVOT =                                      
435600                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
435700                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
435800           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
435900                               TO OBKR-BEERS                              
436000                                                                          
436100           MOVE 'S'            TO OBKR-SW                                 
436200        END-IF                                                            
436300        ADD +1                 TO WS-INDEX-TILLK                          
436400     END-PERFORM                                                          
436500     IF WS-INDEX-TILLK = +1                                               
436600       MOVE +0                 TO OBKR-KDERS                              
436700     END-IF                                                               
436800     .                                                                    
436900     EJECT                                                                
437000 ECU-KONTROLLERA-ENHETSLAST SECTION.                                      
437100                                                                          
437200     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
437300     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
437400     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
437500     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
437600     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
437700     MOVE ORAD-IDDC            TO LAST-IDDC                               
437800     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
437900     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
438000     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
438100     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
438200                                                                          
438300     PERFORM S10-HAMTA-WDB6-INFO                                          
438400     IF VANLIGA-RADER-C1 AND DCS-CDC                                      
438500       CALL W411LAST USING LAST-W411LAST                                  
438600     ELSE                                                                 
438700       IF KOLLA-ARBTAB-C1 AND DCS-CDC AND                                 
438800          (AREG-KVQPACK-3 > +0 OR AREG-KVQPACK-4 > +0)                    
438900          PERFORM ECUA-KOLLA-LO-60-61                                     
439000          IF RAD-LO-60 AND RAD-LO-61                                      
439100             CALL W411LAST USING LAST-W411LAST                            
439200          ELSE                                                            
439300             MOVE +0            TO LAST-ADLAGOMR-UT                       
439400             MOVE +0            TO LAST-KVANTAL-UT                        
439500             MOVE +0            TO LAST-KVBEART-UT                        
439600          END-IF                                                          
439700       ELSE                                                               
439800          MOVE +0               TO LAST-ADLAGOMR-UT                       
439900          MOVE +0               TO LAST-KVANTAL-UT                        
440000          MOVE +0               TO LAST-KVBEART-UT                        
440100       END-IF                                                             
440200     END-IF                                                               
440300     .                                                                    
440400     EJECT                                                                
440500 ECUA-KOLLA-LO-60-61 SECTION.                                             
440600                                                                          
440700     MOVE NEJ      TO RAD-LO-60-SW                                        
440800     MOVE NEJ      TO RAD-LO-61-SW                                        
440900                                                                          
441000     MOVE ARB-IDDC         TO W-IDDC-WDQ212                               
441100     MOVE +60              TO W-ADLAGOMR                                  
441200     PERFORM IMS-GNP-ORQI-WDQ221                                          
441300     IF SEGMENT-FINNS                                                     
441400        MOVE JA               TO RAD-LO-60-SW                             
441500     END-IF                                                               
441600                                                                          
441700     MOVE +61              TO W-ADLAGOMR                                  
441800     PERFORM IMS-GNP-ORQI-WDQ221                                          
441900     IF SEGMENT-FINNS                                                     
442000        MOVE JA               TO RAD-LO-61-SW                             
442100     END-IF                                                               
442200     .                                                                    
442300     EJECT                                                                
442400 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
442500     IF LAST-ADLAGOMR-UT = +0 AND                                         
442600        LAST-KVANTAL-UT  = +0 AND                                         
442700        LAST-KVBEART-UT  = +0                                             
442800*------------------------------------------------------------*            
442900*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
443000*------------------------------------------------------------*            
443100        PERFORM ECVA-FIXA-LAGEROMR-PLATS                                  
443200        PERFORM ECVB-REDIGERA-WOPS-AREA                                   
443300        PERFORM IMS-12-ISRT-ORQF-WDQ401                                   
443400        PERFORM UNTIL SEGMENT-FINNS                                       
443500           ADD +1                    TO ORAD-IDLOPNR                      
443600           PERFORM IMS-12-ISRT-ORQF-WDQ401                                
443700        END-PERFORM                                                       
443800     ELSE                                                                 
443900*------------------------------------------------------------*            
444000*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
444100*------------------------------------------------------------*            
444200        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
444300*------------------------------------------------------------*            
444400*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
444500*------------------------------------------------------------*            
444600           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
444700           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
444800                                    ORAD-KVPRERO                          
444900           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
445000           PERFORM ECVC-BERAEKNA-KVSLATT                                  
445100           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
445200           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
445300           MOVE ORAD-ADGANG          TO WS-ADGANG                         
445400           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
445500           PERFORM ECVB-REDIGERA-WOPS-AREA                                
445600           PERFORM IMS-12-ISRT-ORQF-WDQ401                                
445700           PERFORM UNTIL SEGMENT-FINNS                                    
445800              ADD +1                 TO ORAD-IDLOPNR                      
445900              PERFORM IMS-12-ISRT-ORQF-WDQ401                             
446000           END-PERFORM                                                    
446100*------------------------------------------------------------*            
446200*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
446300*------------------------------------------------------------*            
446400           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
446500           MOVE WS-ADGANG            TO ORAD-ADGANG                       
446600           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
446700                                                                          
446800           MOVE +0                   TO ORAD-KVBEART                      
446900           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
447000                                        ORAD-KVPREAVB                     
447100           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
447200           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
447300             CONTINUE                                                     
447400           ELSE                                                           
447500             IF LAST-ADGANG-UT > ZERO                                     
447600               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
447700             END-IF                                                       
447800           END-IF                                                         
447900           MOVE +0                   TO ORAD-KVPRERO                      
448000           MOVE 1.0000               TO ORAD-RERF-RAD                     
448100           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
448200           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
448300           PERFORM ECVB-REDIGERA-WOPS-AREA                                
448400           PERFORM IMS-12-ISRT-ORQF-WDQ401                                
448500           PERFORM UNTIL SEGMENT-FINNS                                    
448600              ADD +1                 TO ORAD-IDLOPNR                      
448700              PERFORM IMS-12-ISRT-ORQF-WDQ401                             
448800           END-PERFORM                                                    
448900        ELSE                                                              
449000*------------------------------------------------------------*            
449100*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
449200*------------------------------------------------------------*            
449300           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
449400           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
449500             CONTINUE                                                     
449600           ELSE                                                           
449700             IF LAST-ADGANG-UT > ZERO                                     
449800               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
449900             END-IF                                                       
450000           END-IF                                                         
450100           MOVE 1.0000            TO ORAD-RERF-RAD                        
450200           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
450300           PERFORM ECVB-REDIGERA-WOPS-AREA                                
450400           PERFORM IMS-12-ISRT-ORQF-WDQ401                                
450500           PERFORM UNTIL SEGMENT-FINNS                                    
450600              ADD +1              TO ORAD-IDLOPNR                         
450700              PERFORM IMS-12-ISRT-ORQF-WDQ401                             
450800           END-PERFORM                                                    
450900        END-IF                                                            
451000     END-IF                                                               
451100     .                                                                    
451200     EJECT                                                                
451300 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
451400                                                                          
451500     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
451600     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
451700     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
451800     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
451900     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
452000     MOVE 1                    TO ADRS-KDCALL-IN                          
452100     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
452200     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
452300     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
452400     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
452500                                                                          
452600     CALL W413ADRS USING ADRS-W413ADRS                                    
452700                                                                          
452800     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
452900     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
453000                                                                          
453100     .                                                                    
453200     EJECT                                                                
453300 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
453400                                                                          
453500     MOVE +1                   TO AVSR-KDCALL                             
453600     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
453700     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
453800     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
453900     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
454000     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
454100     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
454200                                                                          
454300     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
454400     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
454500     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
454600     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
454700     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
454800     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
454900     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
455000     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
455100     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
455200     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
455300     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
455400     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
455500     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
455600                                                                          
455700     ADD +1                    TO WS-INDEX-WOPS                           
455800     .                                                                    
455900     EJECT                                                                
456000 ECVC-BERAEKNA-KVSLATT SECTION.                                           
456100                                                                          
456200     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
456300                                                                          
456400        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
456500                                                                          
456600        COMPUTE ORAD-KVSLATT ROUNDED =                                    
456700               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
456800     END-IF                                                               
456900     .                                                                    
457000     EJECT                                                                
457100 ED-LAES-TILLK-DATA SECTION.                                              
457200                                                                          
457300     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
457400                               TO AREG-IDARTNR                            
457500                                                                          
457600     CALL W411AREG USING AREG-W411AREG                                    
457700                         AREG-WDK6-PCB                                    
457800                         AREG-WDK7-PCB                                    
457900     .                                                                    
458000     EJECT                                                                
458100 F-HOPPA-TILL-SVARSBILD SECTION.                                          
458200                                                                          
458300     MOVE MFS-KDMFSFOR           TO 4203-SPRAK                            
458400     IF MID-KDTRTYP = 'V'                                                 
458500        MOVE 'W4T203V '          TO 4203-TRANSKOD                         
458600        MOVE ALL '+'             TO 4203-IDDISTR-IN                       
458700                                    4203-IDKUNDNR-IN                      
458800                                    4203-IDORDNR-IN                       
458900        MOVE WS-IDDISTR          TO 4203-IDDISTR-UT                       
459000        MOVE WS-IDKUNDNR         TO 4203-IDKUNDNR-UT                      
459100        MOVE WS-IDORDNR          TO 4203-IDORDNR-UT                       
459200                                                                          
459300        PERFORM IMS-INSERT-4203V-MSG                                      
459400     ELSE                                                                 
459500        MOVE WS-IDDISTR          TO 4203-IDDISTR-IN                       
459600        MOVE WS-IDKUNDNR         TO 4203-IDKUNDNR-IN                      
459700        MOVE WS-IDORDNR          TO 4203-IDORDNR-IN                       
459800        MOVE MFS-RENSA-FAELT     TO 4203-IDDISTR-UT                       
459900                                    4203-IDKUNDNR-UT                      
460000                                    4203-IDORDNR-UT                       
460100                                                                          
460200        PERFORM IMS-INSERT-4203-MSG                                       
460300     END-IF                                                               
460400                                                                          
460500                                                                          
460600     MOVE JA                     TO HOPP-TILL-4203                        
460700     .                                                                    
460800     EJECT                                                                
460900 G-VISA-TOM-SIDA SECTION.                                                 
461000                                                                          
461100     MOVE +1 TO WS-INDEX                                                  
461200     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
461300       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
461400                                 MOD-KVBEART(WS-INDEX)                    
461500                                 MOD-TITPO(WS-INDEX)                      
461600                                 MOD-FLRESTN(WS-INDEX)                    
461700                                 MOD-FLSLATT(WS-INDEX)                    
461800                                 MOD-KDKVBRYT(WS-INDEX)                   
461900                                 MOD-BERADREF(WS-INDEX)                   
462000       ADD  +1 TO WS-INDEX                                                
462100     END-PERFORM                                                          
462200     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
462300     .                                                                    
462400     EJECT                                                                
462500 H-KONTROLLA-BEHORIGHET SECTION.                                          
462600                                                                          
462700     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
462800     MOVE '4244'            TO    SEC-IDTRANS                             
462900     MOVE WS-IDDISTR        TO    SEC-IDKEY                               
463000                                                                          
463100     CALL WSECURIT          USING SEC-IDUSER                              
463200                                  SEC-IDTRANS                             
463300                                  SEC-IDKEY                               
463400                                  SEC-KDSVAR                              
463500                                                                          
463600     IF SEC-KDSVAR = OBEHORIG                                             
463700        MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                 
463800        MOVE NEJ          TO ALLT-SW                                      
463900        MOVE OBEHORIG     TO SPAR-BEHORIGHETS-KONTR                       
464000     ELSE                                                                 
464100        CONTINUE                                                          
464200     END-IF                                                               
464300     .                                                                    
464400     EJECT                                                                
464500 I-SKICKA-PRISFRAGA SECTION.                                              
464600                                                                          
464700     MOVE 1                      TO 3039-REQU-IDMSGVER                    
464800     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
464900     MOVE 'W4024400'             TO 3039-REQU-IDUSER                      
465000                                                                          
465100     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
465200     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
465300     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
465400     IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'                           
465500       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
465600       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
465700       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
465800     END-IF                                                               
465900     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
466000                                                                          
466100     PERFORM S04-SKICKA-OPEN                                              
466200     PERFORM S04-SKICKA-MEDDELANDE                                        
466300     PERFORM S04-SKICKA-CLOSE                                             
466400                                                                          
466500     .                                                                    
466600     EJECT                                                                
466700 Z-FINIT-INSERT-MSG SECTION.                                              
466800                                                                          
466900     IF SPAR-BEHORIGHETS-KONTR = OBEHORIG                                 
467000         CALL    WMEDKONV   USING MED-WMEDAREA                            
467100         MOVE    MED-MFSFEL TO    MOD-TEMFSFEL                            
467200         PERFORM MFS-RENSA-MOD-RADER                                      
467300     ELSE                                                                 
467400                                                                          
467500       IF MED-IDMFSFEL NOT = SPACE                                        
467600           CALL WMEDKONV   USING MED-WMEDAREA                             
467700           MOVE MED-MFSFEL TO    MOD-TEMFSFEL                             
467800       END-IF                                                             
467900                                                                          
468000       IF NOT ALLT-OK                                                     
468100          PERFORM MFS-ROER-EJ-BILD                                        
468200       END-IF                                                             
468300     END-IF                                                               
468400                                                                          
468500     MOVE    MAX-MOD-LAENGD TO MSG-KVLL                                   
468600     PERFORM IMS-INSERT-MSG                                               
468700     .                                                                    
468800     EJECT                                                                
468900 S02-RENSA-TILLK-TAB SECTION.                                             
469000                                                                          
469100     MOVE +1              TO WS-INDEX-TILLK                               
469200     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
469300        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
469400        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
469500        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
469600        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
469700        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
469800        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
469900        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
470000        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
470100        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
470200        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
470300        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
470400        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
470500        INITIALIZE           TILK-DEAL-PR-LINE(WS-INDEX-TILLK)            
470600        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
470700        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
470800        ADD +1            TO WS-INDEX-TILLK                               
470900     END-PERFORM                                                          
471000     MOVE +1              TO WS-INDEX-TILLK                               
471100     .                                                                    
471200     EJECT                                                                
471300                                                                          
471400 S04-SKICKA-OPEN SECTION.                                                 
471500                                                                          
471600     MOVE 'OPEN'                     TO SEND-KDFUNC                       
471700     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
471800     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
471900                                                                          
472000     IF SEND-KDRC > 0                                                     
472100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
472200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
472300       DELIMITED BY SIZE INTO FELTEXT                                     
472400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
472500     END-IF                                                               
472600     .                                                                    
472700     SKIP3                                                                
472800 S04-SKICKA-MEDDELANDE SECTION.                                           
472900                                                                          
473000     MOVE 'PUT'                      TO SEND-KDFUNC                       
473100     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
473200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
473300                                                                          
473400     IF SEND-KDRC > 0                                                     
473500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
473600       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
473700       DELIMITED BY SIZE INTO FELTEXT                                     
473800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
473900     END-IF                                                               
474000     .                                                                    
474100     SKIP3                                                                
474200 S04-SKICKA-CLOSE SECTION.                                                
474300                                                                          
474400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
474500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
474600                                                                          
474700     IF SEND-KDRC > 0                                                     
474800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
474900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
475000       DELIMITED BY SIZE INTO FELTEXT                                     
475100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
475200     END-IF                                                               
475300     .                                                                    
475400     EJECT                                                                
475500                                                                          
475600 S05-DELETE-PRICE-Q-LINE SECTION.                                         
475700                                                                          
475800     IF DIST79-DEALER-PRICE                                               
475900       IF OBKR-IDPRQUES > ZERO                                            
476000         INITIALIZE PRQU-W335PRQU                                         
476100         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
476200         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
476300         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
476400         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
476500         MOVE 4                  TO PRQU-KDCALL                           
476600         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
476700                                            PRQU-WDC7-PCB                 
476800                                            PRQU-SJKO-WDK6-PCB            
476900       END-IF                                                             
477000     END-IF                                                               
477100     .                                                                    
477200                                                                          
477300 S10-HAMTA-WDB6-INFO      SECTION.                                        
477400                                                                          
477500     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
477600       CONTINUE                                                           
477700     ELSE                                                                 
477800       IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                            
477900                                                                          
478000          MOVE 1 TO WS-CLDC-IX                                            
478100          PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                    
478200                        CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR               
478300                        CLDC-IDDC (WS-CLDC-IX) = SPACE                    
478400             ADD 1 TO WS-CLDC-IX                                          
478500          END-PERFORM                                                     
478600                                                                          
478700       END-IF                                                             
478800     END-IF                                                               
478900     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
479000        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
479100        MOVE WS-IDDC TO W-IDDC-B6                                         
479200        PERFORM IMS-GU-WDB601                                             
479300     ELSE                                                                 
479400        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
479500     END-IF                                                               
479600     .                                                                    
479700     EJECT                                                                
479800 MFS-RENSA-MOD-RADER SECTION.                                             
479900                                                                          
480000     MOVE +1 TO WS-INDEX                                                  
480100     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
480200       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
480300                                 MOD-KVBEART(WS-INDEX)                    
480400                                 MOD-TITPO(WS-INDEX)                      
480500                                 MOD-FLRESTN(WS-INDEX)                    
480600                                 MOD-FLSLATT(WS-INDEX)                    
480700                                 MOD-KDKVBRYT(WS-INDEX)                   
480800                                 MOD-BERADREF(WS-INDEX)                   
480900       ADD  +1 TO WS-INDEX                                                
481000     END-PERFORM                                                          
481100     .                                                                    
481200     EJECT                                                                
481300                                                                          
481400 MFS-ROER-EJ-BILD SECTION.                                                
481500                                                                          
481600     MOVE +1 TO WS-INDEX                                                  
481700     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
481800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(WS-INDEX)                    
481900                                 MOD-KVBEART(WS-INDEX)                    
482000                                 MOD-TITPO(WS-INDEX)                      
482100                                 MOD-FLRESTN(WS-INDEX)                    
482200                                 MOD-FLSLATT(WS-INDEX)                    
482300                                 MOD-KDKVBRYT(WS-INDEX)                   
482400                                 MOD-BERADREF(WS-INDEX)                   
482500       ADD  +1 TO WS-INDEX                                                
482600     END-PERFORM                                                          
482700     .                                                                    
482800     EJECT                                                                
482900* --- IMS SEKTIONER ---                                                   
483000                                                                          
483100 IMS-GET-MSG SECTION.                                                     
483200                                                                          
483300     MOVE '  QC' TO GODK-STATUSKODER                                      
483400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
483500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
483600     PERFORM IMS-STATUSKONTROLL                                           
483700     .                                                                    
483800     SKIP2                                                                
483900 IMS-INSERT-MSG SECTION.                                                  
484000                                                                          
484100     IF ENGLISH-TEXT                                                      
484200       MOVE 'N' TO MFS-KDHUVOMR                                           
484300     END-IF                                                               
484400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
484500     MOVE SPACE TO GODK-STATUSKODER                                       
484600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
484700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
484800     PERFORM IMS-STATUSKONTROLL                                           
484900     .                                                                    
485000     SKIP2                                                                
485100 IMS-INSERT-4203-MSG SECTION.                                             
485200                                                                          
485300     IF ENGLISH-TEXT                                                      
485400       MOVE 'N' TO MFS-KDHUVOMR                                           
485500     END-IF                                                               
485600     MOVE LOW-VALUE TO 4203-Z1 4203-Z2                                    
485700     MOVE SPACE TO GODK-STATUSKODER                                       
485800     CALL CBLTDLI USING ISRT 4203-PCB 4203-MSG-IO-AREA                    
485900     MOVE 4203-STATUS-CODE TO STATUS-WS                                   
486000     PERFORM IMS-STATUSKONTROLL                                           
486100     .                                                                    
486200     EJECT                                                                
486300 IMS-INSERT-4203V-MSG SECTION.                                            
486400                                                                          
486500     IF ENGLISH-TEXT                                                      
486600       MOVE 'N' TO MFS-KDHUVOMR                                           
486700     END-IF                                                               
486800     MOVE LOW-VALUE TO 4203-Z1 4203-Z2                                    
486900     MOVE SPACE TO GODK-STATUSKODER                                       
487000     CALL CBLTDLI USING ISRT 4203V-PCB 4203-MSG-IO-AREA                   
487100     MOVE 4203V-STATUS-CODE TO STATUS-WS                                  
487200     PERFORM IMS-STATUSKONTROLL                                           
487300     .                                                                    
487400     SKIP2                                                                
487500 IMS-01-GU-ORQI-WDQ201 SECTION.                                           
487600                                                                          
487700     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
487800          DELIMITED BY SIZE INTO SSA1                                     
487900     MOVE '  GE'               TO GODK-STATUSKODER                        
488000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
488100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
488200     PERFORM IMS-STATUSKONTROLL                                           
488300     .                                                                    
488400     EJECT                                                                
488500 IMS-02-GHU-ORQI-WDQ201 SECTION.                                          
488600                                                                          
488700     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
488800          DELIMITED BY SIZE INTO SSA1                                     
488900     MOVE '    '               TO GODK-STATUSKODER                        
489000     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
489100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
489200     PERFORM IMS-STATUSKONTROLL                                           
489300     .                                                                    
489400     SKIP2                                                                
489500 IMS-GNP-ORQI-WDQ212     SECTION.                                         
489600                                                                          
489700     MOVE 'WLORQI12'       TO SSA1                                        
489800     MOVE '  GE'           TO GODK-STATUSKODER                            
489900     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
490000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
490100     PERFORM IMS-STATUSKONTROLL                                           
490200     .                                                                    
490300     SKIP3                                                                
490400 IMS-GHNP-ORQI-WDQ212    SECTION.                                         
490500                                                                          
490600     MOVE 'WLORQI12'       TO SSA1                                        
490700     MOVE '  GE'           TO GODK-STATUSKODER                            
490800     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ARB SSA1                
490900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
491000     PERFORM IMS-STATUSKONTROLL                                           
491100     .                                                                    
491200     SKIP3                                                                
491300 IMS-GNP-ORQI-WDQ212-FIRST   SECTION.                                     
491400                                                                          
491500     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
491600          DELIMITED BY SIZE INTO SSA1                                     
491700     MOVE '  GE' TO GODK-STATUSKODER                                      
491800     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
491900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
492000     PERFORM IMS-STATUSKONTROLL                                           
492100     .                                                                    
492200     SKIP3                                                                
492300 IMS-GNP-ORQI-WDQ221    SECTION.                                          
492400                                                                          
492500     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
492600          DELIMITED BY SIZE INTO SSA1                                     
492700     STRING 'WLORQI21*F(ADLAGOMR =' W-ADLAGOMR-X ')'                      
492800            DELIMITED BY SIZE INTO SSA2                                   
492900     MOVE '  GE' TO GODK-STATUSKODER                                      
493000     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-LOR SSA1 SSA2            
493100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
493200     PERFORM IMS-STATUSKONTROLL                                           
493300     .                                                                    
493400     SKIP3                                                                
493500 IMS-04-REPL-ORQI-WDQ201 SECTION.                                         
493600                                                                          
493700     MOVE '    '               TO GODK-STATUSKODER                        
493800     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
493900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
494000     PERFORM IMS-STATUSKONTROLL                                           
494100     .                                                                    
494200     EJECT                                                                
494300 IMS-REPL-ORQI-WDQ212    SECTION.                                         
494400                                                                          
494500     MOVE '    '               TO GODK-STATUSKODER                        
494600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ARB                     
494700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
494800     PERFORM IMS-STATUSKONTROLL                                           
494900     .                                                                    
495000     EJECT                                                                
495100 IMS-07-GU-ORQM-WDQ101 SECTION.                                           
495200                                                                          
495300     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
495400                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
495500          DELIMITED BY SIZE INTO SSA1                                     
495600     MOVE '  GE'             TO GODK-STATUSKODER                          
495700     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
495800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
495900     PERFORM IMS-STATUSKONTROLL                                           
496000     .                                                                    
496100     SKIP2                                                                
496200 IMS-11-ISRT-WLORQM01-WDQ101 SECTION.                                     
496300                                                                          
496400     MOVE 'WLORQM01 '          TO SSA1                                    
496500     MOVE '    '               TO GODK-STATUSKODER                        
496600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
496700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
496800     PERFORM IMS-STATUSKONTROLL                                           
496900     .                                                                    
497000     SKIP2                                                                
497100 IMS-12-ISRT-ORQF-WDQ401 SECTION.                                         
497200                                                                          
497300     MOVE 'WLORQF01 '          TO SSA1                                    
497400     MOVE '  II'               TO GODK-STATUSKODER                        
497500     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
497600     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
497700     PERFORM IMS-STATUSKONTROLL                                           
497800     .                                                                    
497900     EJECT                                                                
498000 IMS-13-GU-WLARTM-WDK901 SECTION.                                         
498100                                                                          
498200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
498300          DELIMITED BY SIZE INTO SSA1                                     
498400     MOVE '  GE'               TO GODK-STATUSKODER                        
498500     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
498600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
498700     PERFORM IMS-STATUSKONTROLL                                           
498800     .                                                                    
498900     SKIP2                                                                
499000 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
499100                                                                          
499200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
499300          DELIMITED BY SIZE INTO SSA1                                     
499400     MOVE '    '               TO GODK-STATUSKODER                        
499500     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
499600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
499700     PERFORM IMS-STATUSKONTROLL                                           
499800     .                                                                    
499900                                                                          
500000 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
500100                                                                          
500200     MOVE '    '               TO GODK-STATUSKODER                        
500300     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
500400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
500500     PERFORM IMS-STATUSKONTROLL                                           
500600     .                                                                    
500700     EJECT                                                                
500800 IMS-16-GNP-ORQI-WDQ211 SECTION.                                          
500900                                                                          
501000     STRING 'WLORQI11*F(WDQ211KY =' W-WDQ211KY-X ')'                      
501100          DELIMITED BY SIZE INTO SSA1                                     
501200     MOVE '  GE'               TO GODK-STATUSKODER                        
501300     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-DLEV SSA1               
501400     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
501500     PERFORM IMS-STATUSKONTROLL                                           
501600     .                                                                    
501700     SKIP2                                                                
501800 IMS-GU-WDB201       SECTION.                                             
501900                                                                          
502000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
502100          DELIMITED BY SIZE INTO SSA1                                     
502200     MOVE '  '                 TO GODK-STATUSKODER                        
502300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
502400     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
502500     PERFORM IMS-STATUSKONTROLL                                           
502600     .                                                                    
502700     SKIP2                                                                
502800 IMS-GU-WDB101 SECTION.                                                   
502900                                                                          
503000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
503100          DELIMITED BY SIZE INTO SSA1                                     
503200     MOVE '  '                 TO GODK-STATUSKODER                        
503300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
503400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
503500     PERFORM IMS-STATUSKONTROLL                                           
503600     .                                                                    
503700                                                                          
503800 IMS-GU-WDB601    SECTION.                                                
503900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
504000          DELIMITED BY SIZE INTO SSA1                                     
504100     MOVE '  GE' TO GODK-STATUSKODER                                      
504200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
504300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
504400     PERFORM IMS-STATUSKONTROLL                                           
504500     IF SEGMENT-SAKNAS                                                    
504600        MOVE SPACE TO DCS-KDDC                                            
504700     END-IF                                                               
504800     .                                                                    
504900     SKIP2                                                                
505000 IMS-ISRT-WDR601 SECTION.                                                 
505100                                                                          
505200     MOVE 'WDR601' TO SSA1                                                
505300     MOVE '  II' TO GODK-STATUSKODER                                      
505400     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
505500     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
505600     PERFORM IMS-STATUSKONTROLL                                           
505700     .                                                                    
505800 IMS-STATUSKONTROLL SECTION.                                              
505900                                                                          
506000     SET STATUS-IX TO 1                                                   
506100     SEARCH GODK-STATUS                                                   
506200       AT END CALL FELLOG                                                 
506300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
506400     END-SEARCH                                                           
506500     .                                                                    
506600                                                                          
