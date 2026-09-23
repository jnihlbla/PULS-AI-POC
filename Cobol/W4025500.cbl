000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4025500.                                                
000400 AUTHOR.         KERSTIN JOHANSSON  GUIDE DATAKONSULT AB                  
000500 DATE-WRITTEN.   DEC -90.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR TILLÄGG AV ORDER-            
001100*        RADER. DÄR ORDERN / ORDERDELAR ÄR UTSKRIVNA ELLER HAR            
001200*        STATUS HÖGRE ÄN STATUS R SKAPAS TPO2-RADER.                      
001300*        INPUTTRANSAKTIONEN ÄR SKAPAD FRÅN EN BATCH SEKVENSFIL            
001400*        OCH UPPDATERAD PÅ KOMMUNIKATIONS DB. EN GENERELL MPP -           
001500*        DISPATCHER HÄMTAR TRANSAKTIONEN PÅ KOMMUNIKATIONS DB             
001600*        OCH STARTAR DENNA BAKGRUNDSTRANS.                                
001700*                                                                         
001800*        ANGIVNA VÄRDEN KONTROLLERAS OCH EJ ANGIVNA VÄRDEN HÄMTAS         
001900*        FRÅN ARTIKELREGISTRET.                                           
002000*        DÅ SISTA ORDERRADEN UPPDATERATS STARTAS ORDERAVSLUT.             
002100*        ETT FEL/KLAR MEDDELANDE SKICKAS TILL DISPATCHERN DÅ              
002200*        TRANSAKTIONEN BEHANDLATS.                                        
002300*                                                                         
002400*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002500*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
002600*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)  ORDERBEKR.BAS               
002700*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002800*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002900*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
003000*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
003100*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
003200*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
003300*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
003400*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
003500*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003600*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
003700*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003800*        PROGRAMMET LÄSER             (WDM2)  KAMPANJREGISTER             
003900*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
004000*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
004100*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
004200*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
004300*                                                                         
004400*    INDATA.                                                              
004500*        TRANSAKTION: W4T255X                                             
004600*        MID:         W4I25501                                            
004700*                     WMSGKOM                                             
004800*    UTDATA.                                                              
004900*        MOD:         WMSGKOM    FEL/KLAR MED TILL DISPATHER              
005000*                     W40291     VCBV ÖVERFÖRING                          
005100* CHANGE LOG:                                                             
005200*                                                                         
005300*                                                                         
005400*    E'TRACKER: 5444132 DATED 2007-09-18                                  
005500*    E'TRACKER: 6292887 DATED 2008-04-14                                  
005600*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
005700*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
005800*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
005900*    STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                              
006000**                                                                        
006100     EJECT                                                                
006200 ENVIRONMENT DIVISION.                                                    
006300                                                                          
006400 DATA DIVISION.                                                           
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800     SKIP3                                                                
006900 77  IDPGM                       PIC X(08)   VALUE 'W4025500'.            
007000 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
007100 77  CURRENT-SECTION             PIC X(20)   VALUE SPACE.                 
007200 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
007300 77  YES                         PIC X(1)    VALUE 'Y'.                   
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600 77  SPEC-FORBI                  PIC X(1)    VALUE 'S'.                   
007700 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)   VALUE 'J'.                   
007800 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
007900 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
008000                                                                          
008100 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
008200*01  -COPY WWDCKONS                                                       
008300                                                                          
008400*01  -COPY WWPRODSL                                                       
008500                                                                          
008600 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008700 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +8.          
008900 77  WS-INDEX-ORFK               PIC S9(9)   COMP SYNC VALUE ZERO.        
009000 77  WS-INDEX-ORFK-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
009100 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
009200 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
009300 77  WS-INDEX-LAGOMR             PIC S9(9)   COMP SYNC VALUE ZERO.        
009400 77  WS-INDEX-LAGOMR-MAX         PIC S9(9)   COMP SYNC VALUE +99.         
009500 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009600 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
009700 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009800 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
009900 77  WS-IDDC-DDGS                PIC X(2)    VALUE SPACE.                 
010000 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
010100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010200 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
010300 77  WX-KDORDBEK                 PIC S9(2)   VALUE +0.                    
010400 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
010500 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010600 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010700 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010800                                                                          
010900                                                                          
011000     EJECT                                                                
011100                                                                          
011200 01  W-GMT-IDDC-CLEAR-GRP.                                                
011300*                                 GRUPP AV IDDC-CLEAR                     
011400     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
011500                                 PIC X(2)    VALUE SPACE.                 
011600                                                                          
011700 01  DUMMY-PCB                   PIC X(4)   VALUE LOW-VALUE.              
011800                                                                          
011900 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
012000 01  FILLER REDEFINES WS-TIHHMMSS.                                        
012100     03 WS-TIHHMM                PIC 9(4).                                
012200     03 FILLER                   PIC 9(2).                                
012300                                                                          
012400 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
012500*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
012600*    ----DISTR-DEALER-PRICE----                                           
012700*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
012800     EJECT                                                                
012900                                                                          
013000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013100     88  ALLT-OK                             VALUE 'J'.                   
013200                                                                          
013300 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
013400     88  KOLLA-ERS                           VALUE 'J'.                   
013500                                                                          
013600 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
013700     88  EGET-CL-RAD                         VALUE 'J'.                   
013800                                                                          
013900 77  VANLIGA-RADER-SW            PIC X       VALUE 'N'.                   
014000     88  VANLIGA-RADER                       VALUE 'J'.                   
014100                                                                          
014200 77  ADDLINE-SW                  PIC X       VALUE 'J'.                   
014300     88  ADDLINE-OK                          VALUE 'J'.                   
014400     88  ADDLINE-NOTOK                       VALUE 'N'.                   
014500                                                                          
014600 77  STARTA-4251-SW              PIC X       VALUE 'N'.                   
014700     88  STARTA-4251                         VALUE 'J'.                   
014800                                                                          
014900 77  TILLK-SW                    PIC X       VALUE 'N'.                   
015000     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
015100     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
015200                                                                          
015300 77  OBKR-SW                     PIC X       VALUE 'N'.                   
015400     88  SKRIV-OBKR                          VALUE 'J'.                   
015500     88  OBKR-SKRIVEN                        VALUE 'S'.                   
015600                                                                          
015700 01  WS-ALFA-1.                                                           
015800     03  WS-NUM-1                PIC 9(1).                                
015900 01  WS-ALFA-6.                                                           
016000     03  WS-NUM-6                PIC 9(6).                                
016100                                                                          
016200 01  WS-IDARTNR                  PIC 9(11) VALUE ZERO.                    
016300                                                                          
016400 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
016500 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
016600     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
016700     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
016800 01  WS-TIRFS                    PIC 9(10)   VALUE ZERO.                  
016900 01  FILLER REDEFINES WS-TIRFS.                                           
017000     03  WS-TIRFS-DATUM          PIC 9(6).                                
017100     03  WS-TIRFS-TID            PIC 9(4).                                
017200     EJECT                                                                
017300 01 FILLER                       PIC X(8) VALUE 'W411TILK'.               
017400*    -COPY W411TILK                                                       
017500     EJECT                                                                
017600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017700 01  GENERELLA-SUBPROGRAM.                                                
017800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018300 01  RKOD-ABEND-33               PIC S9(4) COMP VALUE +33.                
018400 01  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
018500*                                                                         
018600     EJECT                                                                
018700*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
018800*                                                                         
018900 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
019000*   -COPY WMSGINIT                                                        
019100     EJECT                                                                
019200 01  GEMENSAMMA-SUBPROGRAM.                                               
019300     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
019400*        PRISTILLÄMPNING                                                  
019500     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
019600*        HÄMTA PRISFRÅGENR                                                
019700     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
019800*        DEALER PRISFRÅGABEHANDLING                                       
019900     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
020000*        LÄSNING ARTIKELREGISTER                                          
020100     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
020200*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
020300     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
020400*        KONTROLL DIREKTLEVERANS                                          
020500     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
020600*        KONTROLL TPO4 - KAMPANJ                                          
020700     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
020800*        KONTROLL ERSÄTTNINGAR                                            
020900     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
021000*        KONTROLL KVANTANPASSNING                                         
021100     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
021200*        KONTROLL ENHETSLAST                                              
021300     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
021400*        FORMELLA KONTROLLER AV INDATA                                    
021500     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
021600*        KONTROLL PRELIMINÄRAVBOKNING NDC                                 
021700     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
021800*        KONTROLL PRELIMINÄRAVBOKNING-XDC                                 
021900     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
022000*        KONTROLL PRELIMINÄRAVBOKNING                                     
022100     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
022200*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
022300     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
022400*        BERÄKNA RANSONERING                                              
022500     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
022600*        KONTROLL SPÄRRAR                                                 
022700     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
022800*        KONTROLL STORA UTTAG                                             
022900     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
023000*        KONTROLL TPO1                                                    
023100     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
023200*        KONTROLL TPO2                                                    
023300     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
023400*        KONTROLL RELS                                                    
023500     03  W411TPO3                PIC X(8)    VALUE 'W411TPO3'.            
023600*        UPPDATERING TPO3                                                 
023700     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
023800*        UPPDATERING TPO6                                                 
023900     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
024000*        WDB601-SEGMENT FÖR CLARING-DC                                    
024100     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
024200*        WOPS RADBEHANDLING                                               
024300     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
024400*        OMVANDLING AV LAGOMR + PLATS                                     
024500     EJECT                                                                
024600                                                                          
024700 01  MESSAGE-CODES.                                                       
024800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024900     03  ERR-ORDER-ANNULL        PIC X(3)    VALUE '052'.                 
025000     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
025100     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
025200     EJECT                                                                
025300*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
025400*                                                                         
025500 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
025600*   -COPY W335PRIS                                                        
025700     EJECT                                                                
025800 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
025900*   -COPY W335PRNO                                                        
026000     EJECT                                                                
026100 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
026200*   -COPY W335PRQU                                                        
026300     EJECT                                                                
026400 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
026500*   -COPY W411AREG                                                        
026600     EJECT                                                                
026700 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
026800*   -COPY W411ARTM                                                        
026900     EJECT                                                                
027000 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
027100*   -COPY W411DLEV                                                        
027200     EJECT                                                                
027300 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
027400*   -COPY W411KAMP                                                        
027500     EJECT                                                                
027600 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
027700*   -COPY W411KERS                                                        
027800     EJECT                                                                
027900 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
028000*   -COPY W411KVAN                                                        
028100     EJECT                                                                
028200 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
028300*   -COPY W411LAST                                                        
028400     EJECT                                                                
028500 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
028600*   -COPY W411ORFK                                                        
028700     EJECT                                                                
028800 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
028900*   -COPY W411NDCA                                                        
029000     EJECT                                                                
029100 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
029200*   -COPY W411XDCA                                                        
029300     EJECT                                                                
029400 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
029500*   -COPY W411XDK7 -PRE NDCA-                                             
029600     EJECT                                                                
029700 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
029800*   -COPY W411CDCA                                                        
029900     EJECT                                                                
030000 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
030100*   -COPY W411SDCA                                                        
030200     EJECT                                                                
030300 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
030400*   -COPY W411RANS                                                        
030500     EJECT                                                                
030600 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
030700*   -COPY W411SPAR                                                        
030800     EJECT                                                                
030900 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
031000*   -COPY W411STOR                                                        
031100     EJECT                                                                
031200 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
031300*   -COPY W411TPO1                                                        
031400     EJECT                                                                
031500 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
031600*   -COPY W411TPO2                                                        
031700     EJECT                                                                
031800 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
031900*   -COPY W411RELS                                                        
032000     EJECT                                                                
032100 01 FILLER                       PIC X(8) VALUE 'W411TPO3'.               
032200*   -COPY W411TPO3                                                        
032300     EJECT                                                                
032400 01 FILLER                       PIC X(8) VALUE 'W411TPO6'.               
032500*   -COPY W411TPO6                                                        
032600     EJECT                                                                
032700 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
032800*   -COPY W411CLDC                                                        
032900     EJECT                                                                
033000 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
033100*   -COPY W413AVSR                                                        
033200     SKIP2                                                                
033300 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
033400*   -COPY W413ADRS                                                        
033500     EJECT                                                                
033600*    --- AREOR FÖR MID                                                    
033700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
033800     SKIP3                                                                
033900 01  MID-AREA.                                                            
034000*03  MID -COPY W4I25501                                                   
034100     EJECT                                                                
034200 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
034300     SKIP3                                                                
034400*01  -COPY WMSGAREA                                                       
034500     EJECT                                                                
034600*05  -COPY W4I25501 -PRE 4251-  -RED MSG-MID-OUT                          
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
034900     SKIP3                                                                
035000 01  KOM-IO-AREA.                                                         
035100*03  -COPY WMSGKOM                                                        
035200     EJECT                                                                
035300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035400*                                                                         
035500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
035600     SKIP3                                                                
035700 01  NYCKLAR-TILL-DLI.                                                    
035800                                                                          
035900     03  W-IDGMTREF-X.                                                    
036000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
036100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
036200         05  W-IDKUNDRF.                                                  
036300            07  W-IDORDNR        PIC 9(7)    VALUE ZERO.                  
036400            07  FILLER           PIC X(3)    VALUE SPACE.                 
036500                                                                          
036600     03  W-IDDC-X.                                                        
036700         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
036800                                                                          
036900     03  W-IDLEVNR-X             PIC X(5)    VALUE SPACE.                 
037000                                                                          
037100     03  W-IDARTNR-X.                                                     
037200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
037300                                                                          
037400     03  W-WDQ101KY-MIN-X.                                                
037500         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
037600         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
037700         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
037800         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
037900         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
038000                                                                          
038100     03  W-WDQ101KY-MAX-X.                                                
038200         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
038300         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
038400         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
038500         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
038600         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
038700                                                                          
038800     03  W-IDGMT-X.                                                       
038900         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
039000         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
039100                                                                          
039200     03  W-WDB101KY-X.                                                    
039300         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
039400         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
039500                                                                          
039600     03  W-IDDC-B6-X.                                                     
039700         05 W-IDDC-B6                  PIC X(2).                          
039800     EJECT                                                                
039900                                                                          
040000*    --- STATUS-KOD FRÅN IMS                                              
040100 01  STATUS-WS                   PIC XX.                                  
040200     88  SEGMENT-FINNS                       VALUE '  '.                  
040300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040500     88  BASEN-SLUT                          VALUE 'GB'.                  
040600     SKIP2                                                                
040700 01  GODK-STATUSKODER.                                                    
040800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040900                                                                          
041000 01  SSA1                        PIC X(160).                              
041100 01  SSA2                        PIC X(96).                               
041200 01  SSA3                        PIC X(64).                               
041300 01  SSA4                        PIC X(64).                               
041400     EJECT                                                                
041500                                                                          
041600*    --- IMS FUNKTIONSKODER                                               
041700*01  -COPY W0003                                                          
041800     EJECT                                                                
041900*    ---  DLI INPUT-OUTPUT AREA                                           
042000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
042100     SKIP3                                                                
042200 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
042300 01  DLI-IO-AREA-OBKR.                                                    
042400     03  WLORQM01.                                                        
042500*        05  -COPY WDQ101                                                 
042600     EJECT                                                                
042700 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
042800 01  DLI-IO-AREA-OHUV.                                                    
042900     03  WLORQI01.                                                        
043000*        05  -COPY WDQ201                                                 
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
043300 01  DLI-IO-AREA-ARB.                                                     
043400     03  WLORQI12.                                                        
043500*        05  -COPY WDQ212                                                 
043600     EJECT                                                                
043700 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
043800 01  DLI-IO-AREA-ORAD.                                                    
043900     03  WLORQF01.                                                        
044000*        05  -COPY WDQ401                                                 
044100     EJECT                                                                
044200 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
044300 01  DLI-IO-AREA-ART.                                                     
044400     03  WLARTM01.                                                        
044500*        05  -COPY WDK901                                                 
044600     EJECT                                                                
044700 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
044800 01  DLI-IO-AREA-WDB201.                                                  
044900*     03  -COPY WDB201                                                    
045000     EJECT                                                                
045100 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
045200 01  DLI-IO-AREA-WDB101.                                                  
045300     03  WLBETC01.                                                        
045400         05  -COPY WDB101                                                 
045500                                                                          
045600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045700 01   DLI-IO-AREA-B601.                                                   
045800*     03  -COPY WDB601                                                    
045900     EJECT                                                                
046000 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
046100 01   DLI-IO-AREA-R601.                                                   
046200*     03  -COPY WDR601                                                    
046300*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
046400     EJECT                                                                
046500 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW-AREA'.           
046600                                                                          
046700 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
046800 01  4298-MSG-IO-AREA.                                                    
046900     03  4298-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
047000     03  4298-Z1               PIC X.                                     
047100     03  4298-Z2               PIC X.                                     
047200     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
047300     03  4298-IDTRANS          PIC X(4)   VALUE '4255'.                   
047400     03  4298-SPRAK            PIC X      VALUE SPACE.                    
047500*    03  -COPY W4I29801  -PRE 4298-                                       
047600     EJECT                                                                
047700 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
047800     SKIP3                                                                
047900 01  -COPY WZ01SEND                                                       
048000     EJECT                                                                
048100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
048200     SKIP3                                                                
048300 01  SEND-AREA.                                                           
048400*    03  -COPY WZ01REQU  -PRE 3039-                                       
048500*    03  -COPY W30391I1  -PRE 3039-                                       
048600     EJECT                                                                
048700 LINKAGE SECTION.                                                         
048800*01  -COPY W0009   -PRE MSG-                                              
048900*01  -COPY W0009   -PRE DISP-                                             
049000     EJECT                                                                
049100*01  -COPY W0009   -PRE 4251-                                             
049200*01  -COPY W0009   -PRE 4298-                                             
049300*01  -COPY W0009   -PRE PRQRY-                                            
049400     SKIP2                                                                
049500     EJECT                                                                
049600*01  -COPY W0008   -PRE USEA-                                             
049700     05  FILLER                  PIC X.                                   
049800     SKIP2                                                                
049900*01  -COPY W0008   -PRE ORQF-                                             
050000     05  FILLER                  PIC X.                                   
050100     SKIP2                                                                
050200*01  -COPY W0008   -PRE ORQI-                                             
050300     05  FILLER                  PIC X.                                   
050400     EJECT                                                                
050500*01  -COPY W0008   -PRE ORQM-                                             
050600     05  FILLER                  PIC X.                                   
050700     SKIP2                                                                
050800*01  -COPY W0008   -PRE ARTM-                                             
050900     05  FILLER                  PIC X.                                   
051000     SKIP2                                                                
051100*01  -COPY W0008   -PRE WDB2-                                             
051200     05  FILLER                  PIC X.                                   
051300     SKIP2                                                                
051400*01  -COPY W0008   -PRE WDB1-                                             
051500     05  FILLER                  PIC X.                                   
051600     SKIP2                                                                
051700*01  -COPY W0008   -PRE WDB6-                                             
051800     05  FILLER                  PIC X.                                   
051900     EJECT                                                                
052000*01  -COPY W0008   -PRE WDK6-                                             
052100     05  FILLER                  PIC X.                                   
052200     SKIP2                                                                
052300*01  -COPY W0008   -PRE WDK7-                                             
052400     05  FILLER                  PIC X.                                   
052500     SKIP2                                                                
052600*01  -COPY W0008   -PRE WDR6-                                             
052700     05  FILLER                  PIC X.                                   
052800     EJECT                                                                
052900                                                                          
053000 01  PRIS-ARTC-PCB               PIC X.                                   
053100 01  PRIS-WDK7-PCB               PIC X.                                   
053200 01  PRIS-GMTA-PCB               PIC X.                                   
053300 01  PRIS-BETA-PCB               PIC X.                                   
053400 01  PRIS-GPRIA-PCB              PIC X.                                   
053500 01  PRIS-GPRIB-PCB              PIC X.                                   
053600 01  PRIS-COST-WDK6-PCB          PIC X.                                   
053700 01  PRIS-COST-WDK7-PCB          PIC X.                                   
053800 01  PRIS-COST-WDF1-PCB          PIC X.                                   
053900 01  PRIS-COST-9305-PCB          PIC X.                                   
054000 01  PRIS-COST-WDK72-PCB         PIC X.                                   
054100 01  PRIS-COST-WDB6-PCB          PIC X.                                   
054200                                                                          
054300 01  PRNO-3107-PCB               PIC X.                                   
054400 01  PRQU-WDC7-PCB               PIC X.                                   
054500 01  PRQU-WDG2-PCB               PIC X.                                   
054600 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
054700                                                                          
054800 01  AREG-WDK6-PCB               PIC X.                                   
054900 01  AREG-WDK7-PCB               PIC X.                                   
055000                                                                          
055100 01  ARTM-ARTM-PCB               PIC X.                                   
055200                                                                          
055300 01  DLEV-LEVF-PCB               PIC X.                                   
055400 01  DLEV-LEVG-PCB               PIC X.                                   
055500 01  DLEV-LEVA-PCB               PIC X.                                   
055600 01  DLEV-ARTS-PCB               PIC X.                                   
055700 01  DLEV-WDB6-PCB               PIC X.                                   
055800                                                                          
055900 01  SPAR-WDF8-PCB               PIC X.                                   
056000 01  SPAR-WDF8A-PCB              PIC X.                                   
056100 01  SPAR-WDK6-PCB               PIC X.                                   
056200                                                                          
056300 01  KAMP-ORDP-PCB               PIC X.                                   
056400 01  KAMP-ZZAC-PCB               PIC X.                                   
056500 01  KAMP-WDM2-PCB               PIC X.                                   
056600                                                                          
056700 01  KERS-ARTC-PCB               PIC X.                                   
056800 01  KERS-ERSA-PCB               PIC X.                                   
056900                                                                          
057000 01  NDCA-USEA-PCB               PIC X.                                   
057100 01  NDCA-WDK7-PCB               PIC X.                                   
057200 01  NDCA-INLC-PCB               PIC X.                                   
057300 01  NDCA-WDB6-PCB               PIC X.                                   
057400                                                                          
057500 01  SDCA-ARTS-PCB               PIC X.                                   
057600 01  SDCA-WDB6-PCB               PIC X.                                   
057700 01  SDCA-WDK9-PCB               PIC X.                                   
057800 01  SDCA-WDR6-PCB               PIC X.                                   
057900 01  SDCA-WDK6-PCB               PIC X.                                   
058000 01  SDCA-WDQ4B-PCB              PIC X.                                   
058100 01  SDCA-WDQ2-PCB               PIC X.                                   
058200 01  SDCA-WDQ4-PCB               PIC X.                                   
058300 01  SDCA-WDB6-2-PCB             PIC X.                                   
058400 01  SDCA-WDK6-2-PCB             PIC X.                                   
058500 01  SDCA-WDK7-2-PCB             PIC X.                                   
058600 01  SDCA-WDK7-3-PCB             PIC X.                                   
058700                                                                          
058800 01  CDCA-ARTM-PCB               PIC X.                                   
058900 01  CDCA-INLB-PCB               PIC X.                                   
059000 01  CDCA-WDB2-PCB               PIC X.                                   
059100 01  CDCA-WDC1-PCB               PIC X.                                   
059200                                                                          
059300 01  RANS-XXKM-PCB               PIC X.                                   
059400 01  RANS-ARTM-PCB               PIC X.                                   
059500 01  RANS-ARTS-PCB               PIC X.                                   
059600     EJECT                                                                
059700                                                                          
059800 01  TPO1-ORDP-PCB               PIC X.                                   
059900 01  TPO1-ARTM-PCB               PIC X.                                   
060000 01  TPO1-ZZAC-PCB               PIC X.                                   
060100                                                                          
060200 01  TPO2-ORDP-PCB               PIC X.                                   
060300 01  TPO2-XXBU-PCB               PIC X.                                   
060400 01  TPO2-XXBV-PCB               PIC X.                                   
060500 01  TPO2-ARTM-PCB               PIC X.                                   
060600 01  TPO2-FILA-PCB               PIC X.                                   
060700 01  TPO2-XXBX-PCB               PIC X.                                   
060800                                                                          
060900 01  RELS-ORDP-PCB               PIC X.                                   
061000 01  RELS-FILA-PCB               PIC X.                                   
061100 01  RELS-ARTM-PCB               PIC X.                                   
061200                                                                          
061300 01  TPO3-ORDP-PCB               PIC X.                                   
061400 01  TPO3-ZZAC-PCB               PIC X.                                   
061500                                                                          
061600 01  2109-PCB                    PIC X.                                   
061700 01  TPO6-ORDP-PCB               PIC X.                                   
061800 01  TPO6-XXBU-PCB               PIC X.                                   
061900 01  TPO6-XXBV-PCB               PIC X.                                   
062000 01  TPO6-XXBX-PCB               PIC X.                                   
062100 01  TPO6-ARTS-PCB               PIC X.                                   
062200                                                                          
062300 01  TIME-4437-PCB               PIC X.                                   
062400                                                                          
062500 01  AVSR-LIST-PCB               PIC X.                                   
062600 01  AVSR-ORQI-PCB               PIC X.                                   
062700 01  AVSR-GMTB-PCB               PIC X.                                   
062800 01  AVSR-GMTC-PCB               PIC X.                                   
062900 01  AVSR-WDB2-PCB               PIC X.                                   
063000 01  AVSR-WDB6-PCB               PIC X.                                   
063100 01  TRAN-XXKB-PCB               PIC X.                                   
063200 01  KVAN-WDB2-PCB               PIC X.                                   
063300 01  KVAN-WDC1-PCB               PIC X.                                   
063400 01  XDCA-USEA-PCB               PIC X.                                   
063500 01  XDCA-WDB6-PCB               PIC X.                                   
063600 01  XDCA-WDK6-PCB               PIC X.                                   
063700 01  XDCA-WDK7-PCB               PIC X.                                   
063800 01  XDCA-WDK9-PCB               PIC X.                                   
063900 01  XDCA-WDL6-PCB               PIC X.                                   
064000 01  XDCA-WDQ4B-PCB              PIC X.                                   
064100 01  XDCA-WDQ2-PCB               PIC X.                                   
064200 01  XDCA-WDQ4-PCB               PIC X.                                   
064300 01  XDCA-WDR6-PCB               PIC X.                                   
064400 01  XDCA-WDB6-2-PCB             PIC X.                                   
064500 01  XDCA-WDK6-2-PCB             PIC X.                                   
064600 01  XDCA-WDK7-2-PCB             PIC X.                                   
064700 01  XDCA-WDK7-3-PCB             PIC X.                                   
064800     EJECT                                                                
064900 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4251-PCB 4298-PCB             
065000      AVSR-LIST-PCB 2109-PCB PRQRY-PCB USEA-PCB                           
065100      ORQF-PCB ORQI-PCB ORQM-PCB ARTM-PCB WDB2-PCB WDB1-PCB               
065200      WDB6-PCB WDK6-PCB WDK7-PCB WDR6-PCB                                 
065300      PRIS-ARTC-PCB                                                       
065400      PRIS-WDK7-PCB                                                       
065500      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
065600      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
065700      PRIS-COST-WDK6-PCB                                                  
065800      PRIS-COST-WDK7-PCB                                                  
065900      PRIS-COST-WDF1-PCB                                                  
066000      PRIS-COST-9305-PCB                                                  
066100      PRIS-COST-WDK72-PCB                                                 
066200      PRIS-COST-WDB6-PCB                                                  
066300      PRNO-3107-PCB                                                       
066400      PRQU-WDG2-PCB                                                       
066500      PRQU-WDC7-PCB                                                       
066600      PRQU-SJKO-WDK6-PCB                                                  
066700      AREG-WDK6-PCB                                                       
066800      AREG-WDK7-PCB                                                       
066900      ARTM-ARTM-PCB                                                       
067000      DLEV-LEVF-PCB                                                       
067100      DLEV-LEVG-PCB                                                       
067200      DLEV-LEVA-PCB                                                       
067300      DLEV-ARTS-PCB                                                       
067400      DLEV-WDB6-PCB                                                       
067500      SPAR-WDF8-PCB                                                       
067600      SPAR-WDF8A-PCB                                                      
067700      SPAR-WDK6-PCB                                                       
067800      KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                           
067900      KERS-ARTC-PCB KERS-ERSA-PCB                                         
068000      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-INLC-PCB NDCA-WDB6-PCB             
068100      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
068200      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
068300      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
068400      SDCA-WDK7-3-PCB                                                     
068500      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
068600      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
068700      TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                           
068800      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
068900      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
069000      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
069100      TPO3-ORDP-PCB TPO3-ZZAC-PCB                                         
069200      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
069300      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
069400      TIME-4437-PCB                                                       
069500      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
069600      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
069700      TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                           
069800      XDCA-USEA-PCB                                                       
069900      XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                           
070000      XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                          
070100      XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                           
070200      XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                     
070300      XDCA-WDK7-3-PCB.                                                    
070400                                                                          
070500     EJECT                                                                
070600                                                                          
070700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4251-PCB 4298-PCB             
070800      AVSR-LIST-PCB 2109-PCB USEA-PCB                                     
070900      ORQF-PCB ORQI-PCB ORQM-PCB ARTM-PCB WDB2-PCB WDB1-PCB               
071000      WDB6-PCB WDK6-PCB WDK7-PCB WDR6-PCB                                 
071100      PRIS-ARTC-PCB                                                       
071200      PRIS-WDK7-PCB                                                       
071300      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
071400      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
071500      PRIS-COST-WDK6-PCB                                                  
071600      PRIS-COST-WDK7-PCB                                                  
071700      PRIS-COST-WDF1-PCB                                                  
071800      PRIS-COST-9305-PCB                                                  
071900      PRIS-COST-WDK72-PCB                                                 
072000      PRIS-COST-WDB6-PCB                                                  
072100      PRNO-3107-PCB                                                       
072200      PRQU-WDG2-PCB                                                       
072300      PRQU-WDC7-PCB                                                       
072400      PRQU-SJKO-WDK6-PCB                                                  
072500      AREG-WDK6-PCB                                                       
072600      AREG-WDK7-PCB                                                       
072700      ARTM-ARTM-PCB                                                       
072800      DLEV-LEVF-PCB                                                       
072900      DLEV-LEVG-PCB                                                       
073000      DLEV-LEVA-PCB                                                       
073100      DLEV-ARTS-PCB                                                       
073200      DLEV-WDB6-PCB                                                       
073300      SPAR-WDF8-PCB                                                       
073400      SPAR-WDF8A-PCB                                                      
073500      SPAR-WDK6-PCB                                                       
073600      KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                           
073700      KERS-ARTC-PCB KERS-ERSA-PCB                                         
073800      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-INLC-PCB NDCA-WDB6-PCB             
073900      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
074000      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
074100      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
074200      SDCA-WDK7-3-PCB                                                     
074300      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
074400      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
074500      TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                           
074600      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
074700      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
074800      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
074900      TPO3-ORDP-PCB TPO3-ZZAC-PCB                                         
075000      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
075100      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
075200      TIME-4437-PCB                                                       
075300      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
075400      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
075500      TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                           
075600      XDCA-USEA-PCB                                                       
075700      XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                           
075800      XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                          
075900      XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                           
076000      XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                     
076100      XDCA-WDK7-3-PCB.                                                    
076200     EJECT                                                                
076300                                                                          
076400     PERFORM IMS-GET-MSG                                                  
076600     IF SEGMENT-FINNS                                                     
076700        PERFORM IMS-GN-MSG                                                
076800                                                                          
076900        PERFORM A-INIT                                                    
077000                                                                          
077100        PERFORM B-KONTROLL-ATT-ORDER-FINNS                                
077200        IF ALLT-OK                                                        
077300           PERFORM C-FORMELL-KONTROLL                                     
077400                                                                          
077500           IF ALLT-OK                                                     
077600              PERFORM E-BEHANDLA-RADER                                    
077700                                                                          
077800              IF MID-FLSLUT = 'J'                                         
077900                IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC'                       
078000                                          OR 'ECOC' OR 'VOUC'             
078100                                          OR 'TADC' OR 'ACCC'             
078200                                          OR 'APAC' OR 'APBC'             
078300                                          OR 'APCC' OR 'APDC'             
078400                                          OR 'APEC' OR 'APFC'             
078500                                          OR 'APGC' OR 'APHC'             
078600                                          OR 'APIC' OR 'APJC')            
078700                    AND WS-IDPRQUES = 0                                   
078800                  CONTINUE                                                
078900                ELSE                                                      
079000                 IF DIST79-DEALER-PRICE                                   
079100                    PERFORM I-SKICKA-PRISFRAGA                            
079200                 END-IF                                                   
079300                END-IF                                                    
079400                PERFORM G-AVSLUTA-ORDERN                                  
079500              END-IF                                                      
079600                                                                          
079700           END-IF                                                         
079800        END-IF                                                            
079900                                                                          
080000        IF STARTA-4251                                                    
080100           PERFORM J-STARTA-4251                                          
080200        ELSE                                                              
080300           PERFORM Z-FINIT                                                
080400        END-IF                                                            
080500     END-IF                                                               
080600     MOVE +0 TO RETURN-CODE                                               
080700     GOBACK                                                               
080800     .                                                                    
080900     EJECT                                                                
081000 A-INIT SECTION.                                                          
081100                                                                          
081200     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
081300                               TO MID-AREA                                
081400     MOVE MSG-IDTRANS-1        TO W-IDTRANS                               
081500                                                                          
081600     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
081700                                                                          
081800     MOVE JA                   TO ALLT-SW                                 
081900                                                                          
082000     PERFORM AA-NOLLA-WOPS-TABELL                                         
082100     .                                                                    
082200     EJECT                                                                
082300                                                                          
082400 AA-NOLLA-WOPS-TABELL SECTION.                                            
082500                                                                          
082600     MOVE +1                   TO WS-INDEX-WOPS                           
082700     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
082800        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
082900        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
083000        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
083100        MOVE +0                TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
083200        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
083300        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
083400        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
083500        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
083600        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
083700        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
083800        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
083900        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
084000                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
084100        ADD +1                 TO WS-INDEX-WOPS                           
084200     END-PERFORM                                                          
084300                                                                          
084400     MOVE +1                   TO WS-INDEX-WOPS                           
084500     .                                                                    
084600     EJECT                                                                
084700 B-KONTROLL-ATT-ORDER-FINNS SECTION.                                      
084900                                                                          
085000     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
085100        MOVE MID-IDDISTR          TO W-IDDISTR                            
085200                                     TEST-IDDISTR                         
085300     ELSE                                                                 
085400        MOVE NEJ                  TO ALLT-SW                              
085500     END-IF                                                               
085600                                                                          
085700     IF MID-IDKUNDNR = SPACE                                              
085800        MOVE ZERO                 TO W-IDKUNDNR                           
085900     ELSE                                                                 
086000        IF MID-IDKUNDNR NUMERIC                                           
086100           MOVE MID-IDKUNDNR      TO W-IDKUNDNR                           
086200        ELSE                                                              
086300           MOVE NEJ               TO ALLT-SW                              
086400        END-IF                                                            
086500     END-IF                                                               
086600                                                                          
086700     IF MID-IDORDNR NUMERIC AND MID-IDORDNR > ZERO                        
086800        MOVE MID-IDORDNR          TO W-IDKUNDRF                           
086900     ELSE                                                                 
087000        MOVE NEJ                  TO ALLT-SW                              
087100     END-IF                                                               
087200                                                                          
087300     IF NOT ALLT-OK                                                       
087400        MOVE ERR-WRONG-KEY        TO MSG-KOM-IDMFSMED                     
087500        MOVE 'R'                  TO MSG-KOM-KDSVAR                       
087600        CALL ABEND USING RKOD-ABEND-33                                    
087700     ELSE                                                                 
087800        IF W-IDTRANS = '4251' AND MID-IDMFSMED NOT = SPACE                
087900           MOVE MID-IDMFSMED      TO MSG-KOM-IDMFSMED                     
088000           MOVE '4'               TO MSG-KOM-KDSVAR                       
088100           MOVE NEJ               TO ALLT-SW                              
088200        ELSE                                                              
088300          PERFORM IMS-01-GU-ORQI-WDQ201                                   
088400          IF SEGMENT-FINNS                                                
088500             PERFORM BA-HAMTA-KUND                                        
088600             PERFORM BB-HAMTA-WDB6-INFO                                   
088700             IF (MID-IDSYSTEM = 'LDCS' OR 'LYNS' OR 'ECOS'                
088800                                                 OR 'VOUS'                
088900                                                 OR 'TADS'                
089000                                                 OR 'ACCS'                
089100                                                 OR 'APAS'                
089200                                                 OR 'APBS'                
089300                                                 OR 'APCS'                
089400                                                 OR 'APDS'                
089500                                                 OR 'APES'                
089600                                                 OR 'APFS'                
089700                                                 OR 'APGS'                
089800                                                 OR 'APHS'                
089900                                                 OR 'APIS'                
090000                                                 OR 'APJS')               
090100               MOVE OHUV-IDDC-PRIM       TO WS-IDDC                       
090200                                            W-IDDC                        
090300             ELSE                                                         
090400               IF MID-IDSYSTEM(1:3) = 'LDC' OR                            
090500                  MID-IDSYSTEM(1:3) = 'LYN' OR                            
090600                  MID-IDSYSTEM(1:3) = 'ECO' OR                            
090700                  MID-IDSYSTEM(1:3) = 'VOU' OR                            
090800                  MID-IDSYSTEM(1:3) = 'TAD' OR                            
090900                  MID-IDSYSTEM(1:3) = 'ACC' OR                            
091000                  MID-IDSYSTEM(1:3) = 'APA' OR                            
091100                  MID-IDSYSTEM(1:3) = 'APB' OR                            
091200                  MID-IDSYSTEM(1:3) = 'APC' OR                            
091300                  MID-IDSYSTEM(1:3) = 'APD' OR                            
091400                  MID-IDSYSTEM(1:3) = 'APE' OR                            
091500                  MID-IDSYSTEM(1:3) = 'APF' OR                            
091600                  MID-IDSYSTEM(1:3) = 'APG' OR                            
091700                  MID-IDSYSTEM(1:3) = 'APH' OR                            
091800                  MID-IDSYSTEM(1:3) = 'API' OR                            
091900                  MID-IDSYSTEM(1:3) = 'APJ'                               
092000                 MOVE W-GMT-IDDC-CLEAR(2) TO WS-IDDC                      
092100                 MOVE OHUV-IDDC-PRIM      TO W-IDDC                       
092200               ELSE                                                       
092300                 MOVE OHUV-IDDC-PRIM     TO WS-IDDC                       
092400                                            W-IDDC                        
092500               END-IF                                                     
092600             END-IF                                                       
092700                                                                          
092800             PERFORM S40-HAMTA-WDB6-INFO                                  
092900                                                                          
093000             IF OHUV-FLBORT = NEJ                                         
093100               IF OHUV-FLFORBI = JA  OR                                   
093200                  OHUV-FLFORBI = SPEC-FORBI OR                            
093300                  OHUV-FLORDSPE = JA  OR                                  
093400                  OHUV-KDTPOTYP > 0  OR OHUV-IDKAMPRF > 0   OR            
093500                  OHUV-FLOVRLEV = JA OR OHUV-KDFAKTYP = 'G' OR            
093600                 (OHUV-KDORDKL < +3 AND                                   
093700                 (MID-IDSYSTEM(1:3) NOT = 'LDC' AND                       
093800                  MID-IDSYSTEM(1:3) NOT = 'LYN' AND                       
093900                  MID-IDSYSTEM(1:3) NOT = 'ECO' AND                       
094000                  MID-IDSYSTEM(1:3) NOT = 'VOU' AND                       
094100                  MID-IDSYSTEM(1:3) NOT = 'TAD' AND                       
094200                  MID-IDSYSTEM(1:3) NOT = 'ACC' AND                       
094300                  MID-IDSYSTEM(1:3) NOT = 'APA' AND                       
094400                  MID-IDSYSTEM(1:3) NOT = 'APB' AND                       
094500                  MID-IDSYSTEM(1:3) NOT = 'APC' AND                       
094600                  MID-IDSYSTEM(1:3) NOT = 'APD' AND                       
094700                  MID-IDSYSTEM(1:3) NOT = 'APE' AND                       
094800                  MID-IDSYSTEM(1:3) NOT = 'APF' AND                       
094900                  MID-IDSYSTEM(1:3) NOT = 'APG' AND                       
095000                  MID-IDSYSTEM(1:3) NOT = 'APH' AND                       
095100                  MID-IDSYSTEM(1:3) NOT = 'API' AND                       
095200                  MID-IDSYSTEM(1:3) NOT = 'APJ' ))                        
095300                                                                          
095400                  MOVE ERR-EJ-TILLAEGG      TO MSG-KOM-IDMFSMED           
095500                  MOVE 'R'                  TO MSG-KOM-KDSVAR             
095600                  MOVE NEJ                  TO ALLT-SW                    
095700               END-IF                                                     
095800             ELSE                                                         
095900                MOVE ERR-ORDER-ANNULL      TO MSG-KOM-IDMFSMED            
096000                MOVE 'R'                   TO MSG-KOM-KDSVAR              
096100                MOVE NEJ                   TO ALLT-SW                     
096200             END-IF                                                       
096300          ELSE                                                            
096400             MOVE JA                   TO STARTA-4251-SW                  
096500             MOVE NEJ                  TO ALLT-SW                         
096600          END-IF                                                          
096700        END-IF                                                            
096800     END-IF                                                               
096900     IF ALLT-OK                                                           
097000        IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU'          
097100                                      OR 'TAD' OR 'ACC' OR 'APA'          
097200                                      OR 'APB' OR 'APC' OR 'APD'          
097300                                      OR 'APE' OR 'APF' OR 'APG'          
097400                                      OR 'APH' OR 'API' OR 'APJ')         
097500             MOVE JA                     TO VANLIGA-RADER-SW              
097600        ELSE                                                              
097700            PERFORM BC-KONTROLLERA-ORDSTA                                 
097800        END-IF                                                            
097900                                                                          
098000        PERFORM BD-FIXA-LOKAL-TID                                         
098100                                                                          
098200        PERFORM IMS-03-GNP-ORQI-WDQ212                                    
098300        IF SEGMENT-FINNS                                                  
098400          CONTINUE                                                        
098500        ELSE                                                              
098600*** RAD HAR GÅTT I REST - Q212 RENSAD I VECOBATCH  GERRY 000201           
098700          MOVE ERR-EJ-TILLAEGG              TO MSG-KOM-IDMFSMED           
098800          MOVE 'R'                          TO MSG-KOM-KDSVAR             
098900          MOVE NEJ                          TO ALLT-SW                    
099000        END-IF                                                            
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400                                                                          
099500 BA-HAMTA-KUND      SECTION.                                              
099600                                                                          
099700     MOVE W-IDDISTR                TO W-IDDISTR-WDB2                      
099800     MOVE W-IDKUNDNR               TO W-IDKUNDNR-WDB2                     
099900     PERFORM IMS-GU-WDB201                                                
100000                                                                          
100100     IF OHUV-KDORDKL > 1                                                  
100200                                                                          
100300        MOVE +1 TO WS-INDEX                                               
100400        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
100500           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
100600                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
100700           ADD +1 TO WS-INDEX                                             
100800        END-PERFORM                                                       
100900                                                                          
101000     ELSE                                                                 
101100       IF OHUV-KDORDKL = 1                                                
101200                                                                          
101300          MOVE +1 TO WS-INDEX                                             
101400          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
101500             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
101600                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
101700             ADD +1 TO WS-INDEX                                           
101800          END-PERFORM                                                     
101900                                                                          
102000       ELSE                                                               
102100         IF OHUV-KDORDKL = 0                                              
102200                                                                          
102300            MOVE +1 TO WS-INDEX                                           
102400            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
102500               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
102600                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
102700               ADD +1 TO WS-INDEX                                         
102800            END-PERFORM                                                   
102900                                                                          
103000         END-IF                                                           
103100       END-IF                                                             
103200     END-IF                                                               
103300     .                                                                    
103400     EJECT                                                                
103500                                                                          
103600 BB-HAMTA-WDB6-INFO SECTION.                                              
103700                                                                          
103800     MOVE SPACE                TO CLDC-W411CLDC                           
103900     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
104000                                                                          
104100     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 BC-KONTROLLERA-ORDSTA     SECTION.                                       
104600                                                                          
104700     MOVE JA TO SW-KDORDSTA-O-ALL-SPACE-FLAG                              
104800                                                                          
104900     PERFORM IMS-GHNP-WDQ212-OKVAL                                        
105000                                                                          
105100     PERFORM UNTIL SEGMENT-SAKNAS                                         
105200                                                                          
105300        IF ARB-KDORDSTA-O NOT = SPACE                                     
105400           MOVE NEJ TO SW-KDORDSTA-O-ALL-SPACE-FLAG                       
105500        END-IF                                                            
105600                                                                          
105700        IF ARB-KDORDSTA  NOT = SPACE                                      
105800          IF (ARB-KDORDSTA     = 'E' OR 'B' OR 'C' OR 'R')                
105900          AND  (ARB-KDORDSTA-O     = ' ' OR 'B' OR 'C' OR 'R')            
106000            MOVE JA             TO VANLIGA-RADER-SW                       
106100          ELSE                                                            
106200            MOVE NEJ            TO ADDLINE-SW                             
106300          END-IF                                                          
106400        END-IF                                                            
106500                                                                          
106600        PERFORM IMS-GHNP-WDQ212-OKVAL                                     
106700     END-PERFORM                                                          
106800     .                                                                    
106900     EJECT                                                                
107000                                                                          
107100 BD-FIXA-LOKAL-TID SECTION.                                               
107200                                                                          
107300     MOVE ALL '+'              TO MSGI-WMSGINIT                           
107400     MOVE '013'                TO MSGI-KDCALL                             
107500     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
107600     IF OHUV-IDDC-TVS = SPACE                                             
107700       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
107800     ELSE                                                                 
107900       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
108000     END-IF                                                               
108100                                                                          
108200     MOVE '4255'               TO MSGI-IDTRANS                            
108300     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
108400                                                                          
108500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
108600     .                                                                    
108700     EJECT                                                                
108800                                                                          
108900 C-FORMELL-KONTROLL SECTION.                                              
109000                                                                          
109100     IF MID-IDSYSTEM = 'LDCC'                                             
109200*    IF MID-IDSYSTEM = 'LDCC' OR 'LDCH'                                   
109300       MOVE MID-IDSYSTEM(1:3)        TO ORFK-IDSYSTEM                     
109400     ELSE                                                                 
109500       IF MID-IDSYSTEM = 'LYNC'                                           
109600          MOVE 'LYNK'                TO ORFK-IDSYSTEM                     
109700       ELSE                                                               
109800         IF MID-IDSYSTEM = 'ECOC'                                         
109900            MOVE 'ECOM'              TO ORFK-IDSYSTEM                     
110000         ELSE                                                             
110100           IF MID-IDSYSTEM = 'VOUC'                                       
110200              MOVE 'VOUI'            TO ORFK-IDSYSTEM                     
110300           ELSE                                                           
110400             IF MID-IDSYSTEM = 'TADC'                                     
110500                MOVE 'TAD '          TO ORFK-IDSYSTEM                     
110600             ELSE                                                         
110700               IF MID-IDSYSTEM = 'ACCC'                                   
110800                  MOVE 'ACC '        TO ORFK-IDSYSTEM                     
110900               ELSE                                                       
111000                 IF MID-IDSYSTEM = 'APAC'                                 
111100                   MOVE 'APA ' TO ORFK-IDSYSTEM                           
111200                 ELSE                                                     
111300                   IF MID-IDSYSTEM = 'APBC'                               
111400                     MOVE 'APB ' TO ORFK-IDSYSTEM                         
111500                   ELSE                                                   
111600                     IF MID-IDSYSTEM = 'APCC'                             
111700                       MOVE 'APC ' TO ORFK-IDSYSTEM                       
111800                     ELSE                                                 
111900                       IF MID-IDSYSTEM = 'APDC'                           
112000                         MOVE 'APD ' TO ORFK-IDSYSTEM                     
112100                       ELSE                                               
112200                         IF MID-IDSYSTEM = 'APEC'                         
112300                           MOVE 'APE ' TO ORFK-IDSYSTEM                   
112400                         ELSE                                             
112500                           IF MID-IDSYSTEM = 'APFC'                       
112600                             MOVE 'APF ' TO ORFK-IDSYSTEM                 
112700                           ELSE                                           
112800                             IF MID-IDSYSTEM = 'APGC'                     
112900                               MOVE 'APG ' TO ORFK-IDSYSTEM               
113000                             ELSE                                         
113100                               IF MID-IDSYSTEM = 'APHC'                   
113200                                 MOVE 'APH ' TO ORFK-IDSYSTEM             
113300                               ELSE                                       
113400                                 IF MID-IDSYSTEM = 'APIC'                 
113500                                   MOVE 'API ' TO ORFK-IDSYSTEM           
113600                                 ELSE                                     
113700                                   IF MID-IDSYSTEM = 'APJC'               
113800                                     MOVE 'APJ ' TO ORFK-IDSYSTEM         
113900                                   ELSE                                   
114000                                      MOVE MID-IDSYSTEM                   
114100                                                 TO ORFK-IDSYSTEM         
114200                                   END-IF                                 
114300                                 END-IF                                   
114400                               END-IF                                     
114500                             END-IF                                       
114600                           END-IF                                         
114700                         END-IF                                           
114800                        END-IF                                            
114900                     END-IF                                               
115000                   END-IF                                                 
115100                 END-IF                                                   
115200               END-IF                                                     
115300             END-IF                                                       
115400           END-IF                                                         
115500         END-IF                                                           
115600       END-IF                                                             
115700     END-IF                                                               
115800     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
115900     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
116000     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
116100     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
116200     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
116300     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
116400     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
116500     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
116600     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
116700     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
116800     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
116900                                                                          
117000     MOVE +1                   TO WS-INDEX-ORFK                           
117100     PERFORM UNTIL WS-INDEX-ORFK > WS-INDEX-ORFK-MAX                      
117200                                                                          
117300        MOVE ALL '+'       TO ORFK-IDARTNR-IN      (WS-INDEX-ORFK)        
117400        MOVE OHUV-FLRESTN  TO ORFK-FLRESTN         (WS-INDEX-ORFK)        
117500        MOVE OHUV-KDVRINFO TO ORFK-KDVRINFO        (WS-INDEX-ORFK)        
117600        MOVE ZERO          TO ORFK-IDKONTO         (WS-INDEX-ORFK)        
117700        MOVE SPACE         TO ORFK-IDKST           (WS-INDEX-ORFK)        
117800        MOVE NEJ           TO ORFK-FLINVEST        (WS-INDEX-ORFK)        
117900        MOVE ALL '+'       TO ORFK-PRARTNTO        (WS-INDEX-ORFK)        
118000        MOVE ALL '+'       TO ORFK-TITPO-RAD       (WS-INDEX-ORFK)        
118100        MOVE ALL '+'       TO ORFK-KDKVBRYT        (WS-INDEX-ORFK)        
118200        MOVE ALL '+'       TO ORFK-KVBEART         (WS-INDEX-ORFK)        
118300        MOVE JA            TO ORFK-FLSLATT         (WS-INDEX-ORFK)        
118400        MOVE SPACE         TO ORFK-IDKST           (WS-INDEX-ORFK)        
118500        MOVE ALL '+'       TO ORFK-PRARTNTO-LOCPREL(WS-INDEX-ORFK)        
118600        MOVE ALL '+'       TO ORFK-PRARTNTO-LOC    (WS-INDEX-ORFK)        
118700        MOVE ALL '+'       TO ORFK-PRARTBTO-LOC    (WS-INDEX-ORFK)        
118800        ADD +1             TO WS-INDEX-ORFK                               
118900     END-PERFORM                                                          
119000                                                                          
119100     MOVE +1                   TO WS-INDEX-MID                            
119200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
119300                                                                          
119400        IF MID-IDARTNR (WS-INDEX-MID) = SPACE                             
119500           MOVE ALL '+'        TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
119600                                  MID-IDARTNR (WS-INDEX-MID)              
119700        ELSE                                                              
119800                                                                          
119900           MOVE MID-IDARTNR(WS-INDEX-MID) TO WS-IDARTNR                   
120000                                                                          
120100           MOVE WS-IDARTNR     TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
120200                                                                          
120300           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN  (WS-INDEX-MID)            
120400           MOVE OHUV-KDVRINFO  TO ORFK-KDVRINFO (WS-INDEX-MID)            
120500           MOVE ZERO           TO ORFK-IDKONTO  (WS-INDEX-MID)            
120600           MOVE SPACE          TO ORFK-IDKST    (WS-INDEX-MID)            
120700           MOVE NEJ            TO ORFK-FLINVEST (WS-INDEX-MID)            
120800           MOVE ALL '+'        TO ORFK-PRARTNTO (WS-INDEX-MID)            
120900           MOVE ALL '+'        TO ORFK-TITPO-RAD(WS-INDEX-MID)            
121000                                                                          
121100           IF MID-KDKVBRYT(WS-INDEX-MID) = SPACE                          
121200              MOVE ALL '+'     TO MID-KDKVBRYT(WS-INDEX-MID)              
121300           END-IF                                                         
121400           IF MID-KDKVBRYT(WS-INDEX-MID) = '5'                            
121500              MOVE ALL '+'     TO MID-KDKVBRYT(WS-INDEX-MID)              
121600           END-IF                                                         
121700           MOVE MID-KDKVBRYT(WS-INDEX-MID)                                
121800                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
121900                                                                          
122000           IF MID-KVBEART (WS-INDEX-MID) = SPACE                          
122100              MOVE ALL '+'     TO MID-KVBEART (WS-INDEX-MID)              
122200           END-IF                                                         
122300           MOVE MID-KVBEART(WS-INDEX-MID)                                 
122400                               TO ORFK-KVBEART(WS-INDEX-MID)              
122500                                                                          
122600           IF MID-FLSLATT (WS-INDEX-MID) = SPACE                          
122700              MOVE JA          TO MID-FLSLATT (WS-INDEX-MID)              
122800           ELSE                                                           
122900              IF MID-FLSLATT (WS-INDEX-MID) = YES                         
123000                 MOVE JA       TO MID-FLSLATT (WS-INDEX-MID)              
123100              END-IF                                                      
123200           END-IF                                                         
123300           MOVE MID-FLSLATT (WS-INDEX-MID)                                
123400                               TO ORFK-FLSLATT (WS-INDEX-MID)             
123500           MOVE SPACE          TO ORFK-IDKST   (WS-INDEX-MID)             
123600                                                                          
123700           IF MID-PRARTNTO-LOCPREL(WS-INDEX-MID) = SPACE                  
123800              MOVE ALL '+' TO MID-PRARTNTO-LOCPREL(WS-INDEX-MID)          
123900           END-IF                                                         
124000           MOVE MID-PRARTNTO-LOCPREL(WS-INDEX-MID)                        
124100                          TO ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)          
124200                                                                          
124300           IF MID-PRARTNTO-LOC(WS-INDEX-MID) = SPACE                      
124400              MOVE ALL '+' TO MID-PRARTNTO-LOC(WS-INDEX-MID)              
124500           END-IF                                                         
124600           MOVE MID-PRARTNTO-LOC(WS-INDEX-MID)                            
124700                           TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)             
124800                                                                          
124900           IF MID-PRARTBTO-LOC(WS-INDEX-MID) = SPACE                      
125000              MOVE ALL '+' TO MID-PRARTBTO-LOC(WS-INDEX-MID)              
125100           END-IF                                                         
125200           MOVE MID-PRARTBTO-LOC(WS-INDEX-MID)                            
125300                           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)             
125400                                                                          
125500*          IF MID-RERAB(WS-INDEX-MID) = SPACE                             
125600*             MOVE ALL '+'     TO MID-RERAB(WS-INDEX-MID)                 
125700*          END-IF                                                         
125800*          MOVE MID-RERAB(WS-INDEX-MID)                                   
125900*                              TO ORFK-RERAB(WS-INDEX-MID)                
126000        END-IF                                                            
126100                                                                          
126200        ADD +1                 TO WS-INDEX-MID                            
126300     END-PERFORM                                                          
126400                                                                          
126500     CALL W411ORFK USING ORFK-W411ORFK                                    
126600                         AREG-WDK6-PCB                                    
126700                         AREG-WDK7-PCB                                    
126800                                                                          
126900     MOVE +1                   TO WS-INDEX-MID                            
127000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
127100        PERFORM CA-KOLLA-FEL-FK                                           
127200        ADD +1                 TO WS-INDEX-MID                            
127300     END-PERFORM                                                          
127400     .                                                                    
127500     EJECT                                                                
127600                                                                          
127700 CA-KOLLA-FEL-FK SECTION.                                                 
127800                                                                          
127900     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
128000        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
128100     END-IF                                                               
128200                                                                          
128300     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
128400        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
128500     END-IF                                                               
128600                                                                          
128700     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
128800        IF ORFK-IDARTNR-IN(WS-INDEX-MID) NOT NUMERIC                      
128900           MOVE ZERO             TO ORFK-IDARTNR(WS-INDEX-MID)            
129000        END-IF                                                            
129100        IF ORFK-KDORDBEK (WS-INDEX-MID) = ZERO                            
129200           MOVE 58               TO ORFK-KDORDBEK(WS-INDEX-MID)           
129300        END-IF                                                            
129400     END-IF                                                               
129500                                                                          
129600     IF ORFK-IDKONTO-OK(WS-INDEX-MID) = NEJ                               
129700        IF ORFK-IDKONTO(WS-INDEX-MID) NOT NUMERIC                         
129800           MOVE ZERO             TO ORFK-IDKONTO(WS-INDEX-MID)            
129900        END-IF                                                            
130000        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
130100     END-IF                                                               
130200                                                                          
130300     IF ORFK-IDKST-OK(WS-INDEX-MID) = NEJ                                 
130400        IF ORFK-IDKST(WS-INDEX-MID) NOT NUMERIC                           
130500           MOVE SPACE            TO ORFK-IDKST(WS-INDEX-MID)              
130600        END-IF                                                            
130700        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
130800     END-IF                                                               
130900                                                                          
131000     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
131100        IF ORFK-KDKVBRYT(WS-INDEX-MID) NOT NUMERIC                        
131200           MOVE ZERO             TO ORFK-KDKVBRYT(WS-INDEX-MID)           
131300        END-IF                                                            
131400        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
131500     END-IF                                                               
131600                                                                          
131700     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
131800        IF ORFK-KDVRINFO(WS-INDEX-MID) NOT NUMERIC                        
131900           MOVE ZERO             TO ORFK-KDVRINFO(WS-INDEX-MID)           
132000        END-IF                                                            
132100        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
132200     END-IF                                                               
132300                                                                          
132400     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
132500        IF ORFK-KVBEART(WS-INDEX-MID) NOT NUMERIC                         
132600           MOVE ZERO             TO ORFK-KVBEART(WS-INDEX-MID)            
132700        END-IF                                                            
132800        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
132900     END-IF                                                               
133000                                                                          
133100     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
133200        IF ORFK-PRARTNTO(WS-INDEX-MID) NOT NUMERIC                        
133300           MOVE ZERO             TO ORFK-PRARTNTO-UT(WS-INDEX-MID)        
133400        END-IF                                                            
133500        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
133600     END-IF                                                               
133700                                                                          
133800     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
133900        IF ORFK-PRARTNTO-LOC(WS-INDEX-MID) NOT NUMERIC                    
134000           MOVE ZERO        TO ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)         
134100        END-IF                                                            
134200        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
134300     END-IF                                                               
134400                                                                          
134500     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
134600        IF ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID) NOT NUMERIC                
134700           MOVE ZERO   TO ORFK-PRARTNTO-LOCPREL-UT(WS-INDEX-MID)          
134800        END-IF                                                            
134900        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
135000     END-IF                                                               
135100                                                                          
135200     IF ORFK-PRARTBTO-LOC-OK(WS-INDEX-MID) = NEJ                          
135300        IF ORFK-PRARTBTO-LOC(WS-INDEX-MID) NOT NUMERIC                    
135400           MOVE ZERO        TO ORFK-PRARTBTO-LOC-UT(WS-INDEX-MID)         
135500        END-IF                                                            
135600        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
135700     END-IF                                                               
135800                                                                          
135900*    IF ORFK-RERAB-OK(WS-INDEX-MID) = NEJ                                 
136000*       IF ORFK-RERAB(WS-INDEX-MID) NOT NUMERIC                           
136100*          MOVE ZERO        TO ORFK-RERAB-UT(WS-INDEX-MID)                
136200*       END-IF                                                            
136300*       MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
136400*    END-IF                                                               
136500                                                                          
136600     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
136700        IF ORFK-TITPO-RAD(WS-INDEX-MID) NOT NUMERIC                       
136800           MOVE ZERO             TO ORFK-TITPO-RAD(WS-INDEX-MID)          
136900        END-IF                                                            
137000        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
137100     END-IF                                                               
137200                                                                          
137300     IF MID-KDDSP (WS-INDEX-MID) = SPACE                                  
137400        MOVE ZERO              TO MID-KDDSP(WS-INDEX-MID)                 
137500     ELSE                                                                 
137600        IF MID-KDDSP (WS-INDEX-MID) = '1' OR '2'                          
137700           CONTINUE                                                       
137800        ELSE                                                              
137900           MOVE ZERO             TO MID-KDDSP(WS-INDEX-MID)               
138000           MOVE 58               TO ORFK-KDORDBEK(WS-INDEX-MID)           
138100        END-IF                                                            
138200     END-IF                                                               
138300                                                                          
138400     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
138500        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
138600     END-IF                                                               
138700                                                                          
138800     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
138900       CONTINUE                                                           
139000     ELSE                                                                 
139100       IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                              
139200          MOVE SPACE               TO ORFK-IDLEVNR(WS-INDEX-MID)          
139300          MOVE ZERO                TO ORFK-KDFARLIG(WS-INDEX-MID)         
139400                                      ORFK-KDPRODSL(WS-INDEX-MID)         
139500                                      ORFK-REKSIFFR(WS-INDEX-MID)         
139600                                      ORFK-VKART(WS-INDEX-MID)            
139700                                      ORFK-VLARTNTO(WS-INDEX-MID)         
139800                                      ORFK-KDERS(WS-INDEX-MID)            
139900                                      ORFK-TIDISPIN(WS-INDEX-MID)         
140000                                      ORFK-KVQPACK-0(WS-INDEX-MID)        
140100                                      ORFK-KVQPACK-1(WS-INDEX-MID)        
140200       END-IF                                                             
140300     END-IF                                                               
140400                                                                          
140500     .                                                                    
140600     EJECT                                                                
140700 E-BEHANDLA-RADER SECTION.                                                
140800                                                                          
140900     PERFORM EA-ANDRA-ORDERSTATUS                                         
141000     MOVE +1 TO WS-INDEX-MID                                              
141100     MOVE NEJ                     TO TILLK-SW                             
141200                                     OBKR-SW                              
141300     MOVE +0                      TO WS-IDPRQUES                          
141400                                                                          
141500     MOVE SPACE                   TO WS-IDDC-DDGS                         
141600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
141700        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
141800           PERFORM S02-RENSA-TILLK-TAB                                    
141900           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
142000                                  TO AREG-W411AREG-001                    
142100           PERFORM EC-BEHANDLA-RAD                                        
142200           MOVE JA                TO TILLK-SW                             
142300           MOVE +1                TO WS-INDEX-TILLK                       
142400           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
142500              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
142600              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
142700                 PERFORM ED-LAES-TILLK-DATA                               
142800                 PERFORM EC-BEHANDLA-RAD                                  
142900              END-IF                                                      
143000              ADD +1              TO WS-INDEX-TILLK                       
143100           END-PERFORM                                                    
143200        END-IF                                                            
143300        MOVE NEJ                  TO TILLK-SW                             
143400                                     OBKR-SW                              
143500        MOVE SPACE                TO WS-IDDC-DDGS                         
143600        ADD +1 TO WS-INDEX-MID                                            
143700     END-PERFORM                                                          
143800                                                                          
143900     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
144000       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
144100       MOVE +3                      TO PRNO-KDCALL                        
144200                                                                          
144300       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
144400     END-IF                                                               
144500                                                                          
144600     IF AVSR-IDDC(1) > ZERO                                               
144700        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
144800          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
144900          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
145000          TRAN-XXKB-PCB                                                   
145100     END-IF                                                               
145200     MOVE JA                      TO ALLT-SW                              
145300     .                                                                    
145400     EJECT                                                                
145500 EA-ANDRA-ORDERSTATUS SECTION.                                            
145600                                                                          
145700     PERFORM IMS-02-GHU-ORQI-WDQ201                                       
145800                                                                          
145900     IF OHUV-FLKLAR = 'J'                                                 
146000        MOVE NEJ               TO OHUV-FLKLAR                             
146100        IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU'          
146200                                      OR 'TAD' OR 'ACC' OR 'APA'          
146300                                      OR 'APB' OR 'APC' OR 'APD'          
146400                                      OR 'APE' OR 'APF' OR 'APG'          
146500                                      OR 'APH' OR 'API' OR 'APJ')         
146600          CONTINUE                                                        
146700        ELSE                                                              
146800          MOVE '4255'          TO OHUV-IDSYSTEM                           
146900        END-IF                                                            
147000     END-IF                                                               
147100                                                                          
147200     PERFORM IMS-07-REPL-ORQI-WDQ201                                      
147300                                                                          
147400     IF SW-KDORDSTA-O-ALL-SPACE-FLAG   = NEJ                              
147500        CONTINUE                                                          
147600     ELSE                                                                 
147700        PERFORM IMS-GHNP-WDQ212-OKVAL                                     
147800                                                                          
147900        PERFORM UNTIL SEGMENT-SAKNAS                                      
148000                                                                          
148100           MOVE ARB-KDORDSTA     TO ARB-KDORDSTA-O                        
148200           MOVE 'E '             TO ARB-KDORDSTA                          
148300           PERFORM IMS-REPL-WDQ212                                        
148400                                                                          
148500           PERFORM IMS-GHNP-WDQ212-OKVAL                                  
148600        END-PERFORM                                                       
148700     END-IF                                                               
148800     .                                                                    
148900     EJECT                                                                
149000                                                                          
149100 EC-BEHANDLA-RAD SECTION.                                                 
149200                                                                          
149300     PERFORM ECA-NOLLSTALL-OBKR-KODER                                     
149400     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
149500     IF ADDLINE-OK                                                        
149600        PERFORM ECC-LAS-NYA-ARTIKELREG                                    
149700        PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                              
149800        PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                            
149900                                                                          
150000        IF NOT TILLKOMMANDE-RAD                                           
150100           PERFORM ECK-KOMPLETTERA-ERSATTNING                             
150200        END-IF                                                            
150300                                                                          
150400        PERFORM ECL-KOMPLETTERA-SPARRAR                                   
150401*SINCE ECOM SENDS US PRICE/CURRENCY WE SKIP ECJ FOR THEM                  
150402*BUT WE WILL FETCH KDVALISO FOR NIGHTBATCH ORDERLINE DC11                 
150403*ADDITION                                                                 
150410        IF MID-IDSYSTEM(1:3) NOT = 'ECO'                                  
150500          PERFORM ECJ-KOMPLETTERA-PRIS                                    
150510        END-IF                                                            
150520        IF MID-IDSYSTEM(1:3) = 'ECO'                                      
150530          PERFORM ECJB-HAEMTA-KDVALISO-DNI                                
150540        END-IF                                                            
150600        PERFORM ECM-KOMPLETTERA-TPO1                                      
150700        PERFORM ECN-KOMPLETTERA-TPO2                                      
150800        PERFORM ECO-KOMPLETTERA-TPO3                                      
150900        PERFORM ECP-KOMPLETTERA-KAMPANJER                                 
151000        PERFORM ECT-KOMPLETTERA-RELEASESPARR                              
151100        PERFORM ECH-PREL-AVBOKNING-SDC                                    
151200        PERFORM ECG-PREL-AVBOKNING-XDC                                    
151300        PERFORM ECQ-KOMPLETTERA-RANSONERING                               
151400        PERFORM ECR-KOMPLETTERA-STORA-UTTAG                               
151500        PERFORM ECD-PREL-AVBOKNING                                        
151600     END-IF                                                               
151700*    FIX END                                                              
151800                                                                          
151900     IF SKRIV-OBKR OR ADDLINE-NOTOK                                       
152000        PERFORM ECS-SKRIV-OBKR-OCH-VOR-RAD                                
152100***OM DET ÄR TILLÄGGSTPO SÅ SKRIVS INGEN RAD. TL050201                    
152200        IF ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0                        
152300           PERFORM S09-KONTROLLERA-ENHETSLAST                             
152400           PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                            
152500        END-IF                                                            
152600     ELSE                                                                 
152700        IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA OR                        
152800           TPO3-FLKLAR = JA OR KAMP-FLKLAR = JA OR                        
152900           RELS-FLKLAR = JA                                               
153000           CONTINUE                                                       
153100        ELSE                                                              
153200           PERFORM S09-KONTROLLERA-ENHETSLAST                             
153300           PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                            
153400        END-IF                                                            
153500     END-IF                                                               
153600     .                                                                    
153700     EJECT                                                                
153800                                                                          
153900 ECA-NOLLSTALL-OBKR-KODER SECTION.                                        
154000                                                                          
154100     MOVE +0                   TO KVAN-KDORDBEK-UT                        
154200                                  DLEV-KDORDBEK-UT                        
154300                                  KERS-KDERS                              
154400     IF NOT TILLKOMMANDE-RAD                                              
154500        MOVE +0                TO KERS-KDORDBEK                           
154600     ELSE                                                                 
154700        MOVE JA                TO OBKR-SW                                 
154800     END-IF                                                               
154900     MOVE +0                   TO TPO1-KDORDBEK                           
155000                                  TPO2-KDORDBEK                           
155100                                  RELS-KDORDBEK                           
155200                                  TPO3-KDORDBEK                           
155300                                  TPO6-KDORDBEK                           
155400                                  KAMP-KDORDBEK                           
155500                                  STOR-KDORDBEK                           
155600*                                 NDCA-KDORDBEK                           
155700                                  XDCA-KDORDBEK                           
155800                                  SDCA-KDORDBEK                           
155900                                  CDCA-KDORDBEK-UT                        
156000                                  SPAR-KDORDBEK                           
156100                                                                          
156200     MOVE JA                   TO ALLT-SW                                 
156300     MOVE NEJ                  TO EGET-CL-RAD-SW                          
156400                                                                          
156500     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
156600       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
156700       MOVE +7                          TO W-KDTPOTYP                     
156800     ELSE                                                                 
156900       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
157000          MOVE NEJ               TO ALLT-SW                               
157100                                    KOLLA-ERS-SW                          
157200          MOVE JA                TO OBKR-SW                               
157300       END-IF                                                             
157400     END-IF                                                               
157500                                                                          
157600     MOVE NEJ                  TO TPO1-FLKLAR                             
157700                                  TPO2-FLKLAR                             
157800                                  TPO3-FLKLAR                             
157900                                  KAMP-FLKLAR                             
158000                                  TPO6-FLKLAR                             
158100     .                                                                    
158200     EJECT                                                                
158300                                                                          
158400 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
158500                                                                          
158600     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
158700     IF (MID-IDSYSTEM = 'LDCS' OR 'LYNS' OR 'ECOS' OR 'VOUS' OR           
158800                        'TADS' OR 'ACCS' OR 'APAS' OR 'APBS' OR           
158900                        'APCS' OR 'APDS' OR 'APES' OR 'APFS' OR           
159000                        'APGS' OR 'APHS' OR 'APIS' OR 'APJS')             
159100       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
159200     ELSE                                                                 
159300         IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU'         
159400                                       OR 'TAD' OR 'ACC' OR 'APA'         
159500                                       OR 'APB' OR 'APC' OR 'APD'         
159600                                       OR 'APE' OR 'APF' OR 'APG'         
159700                                       OR 'APH' OR 'API' OR 'APJ')        
159800           MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                          
159900         ELSE                                                             
160000           MOVE OHUV-IDDC-PRIM      TO ORAD-IDDC                          
160100         END-IF                                                           
160200     END-IF                                                               
160300                                                                          
160400     MOVE AREG-ADLAGOMR        TO ORAD-ADLAGOMR                           
160500     MOVE AREG-ADGANG          TO ORAD-ADGANG                             
160600     MOVE AREG-ADPLATS         TO ORAD-ADPLATS                            
160700     IF TILLKOMMANDE-RAD                                                  
160800        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
160900     ELSE                                                                 
161000        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
161100                               TO ORAD-IDARTNR                            
161200     END-IF                                                               
161300     MOVE +1                   TO ORAD-IDLOPNR                            
161400                                                                          
161500     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
161600        MOVE SPACE             TO ORAD-BERADREF                           
161700     ELSE                                                                 
161800        MOVE MID-BERADREF(WS-INDEX-MID)                                   
161900                               TO ORAD-BERADREF                           
162000     END-IF                                                               
162200     MOVE OHUV-BEKUNDRF        TO ORAD-BEVOLREF                           
162300     MOVE SPACE                TO ORAD-FLAKPLOC                           
162400     MOVE SPACE                TO ORAD-IDBIL                              
162500                                  ORAD-IDKLIENT                           
162600                                  ORAD-IDARBREF                           
162700                                  ORAD-IDVIN                              
162800     MOVE NEJ                  TO ORAD-FLINVEST                           
162900                                  ORAD-FLSDCLEV                           
163000     MOVE JA                   TO ORAD-FLOBTRAN                           
163100     IF TILLKOMMANDE-RAD                                                  
163200       IF DIST79-DEALER-PRICE                                             
163300          MOVE NEJ             TO ORAD-FLPRTILL                           
163400       ELSE                                                               
163500        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
163600           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
163700                               TO ORAD-FLPRTILL                           
163800        ELSE                                                              
163900           MOVE NEJ            TO ORAD-FLPRTILL                           
164000        END-IF                                                            
164100       END-IF                                                             
164200     ELSE                                                                 
164300        MOVE NEJ               TO ORAD-FLPRTILL                           
164400     END-IF                                                               
164500     MOVE OHUV-FLRESTN         TO ORAD-FLRESTN                            
164600     IF TILLKOMMANDE-RAD                                                  
164700        MOVE JA                TO ORAD-FLTILLK                            
164800     ELSE                                                                 
164900        MOVE NEJ               TO ORAD-FLTILLK                            
165000     END-IF                                                               
165100     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
165200     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
165300     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
165400     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
165500     MOVE SPACE                TO ORAD-IDLEVNR                            
165600     MOVE +0                   TO ORAD-IDLOPNR-RO                         
165700     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
165800     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
165900     MOVE ZERO                 TO ORAD-IDSPECEMB                          
166000     IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC' OR           
166100                        'TADC' OR 'ACCC' OR 'APAC' OR 'APBC' OR           
166200                        'APCC' OR 'APDC' OR 'APEC' OR 'APFC' OR           
166300                        'APGC' OR 'APHC' OR 'APIC' OR 'APJC')             
166400       IF MID-IDSYSTEM = 'LYNC'                                           
166500         MOVE 'LYNK'           TO ORAD-IDSYSTEM                           
166600       ELSE                                                               
166700         IF MID-IDSYSTEM = 'ECOC'                                         
166800           MOVE 'ECOM'         TO ORAD-IDSYSTEM                           
166900         ELSE                                                             
167000           IF MID-IDSYSTEM = 'VOUC'                                       
167100             MOVE 'VOUI'       TO ORAD-IDSYSTEM                           
167200           ELSE                                                           
167300             IF MID-IDSYSTEM = 'TADC'                                     
167400               MOVE 'TAD '     TO ORAD-IDSYSTEM                           
167500             ELSE                                                         
167600               IF MID-IDSYSTEM = 'ACCC'                                   
167700                 MOVE 'ACC '   TO ORAD-IDSYSTEM                           
167800               ELSE                                                       
167900                 IF MID-IDSYSTEM = 'APAC'                                 
168000                   MOVE 'APA ' TO ORAD-IDSYSTEM                           
168100                 ELSE                                                     
168200                   IF MID-IDSYSTEM = 'APBC'                               
168300                     MOVE 'APB ' TO ORAD-IDSYSTEM                         
168400                   ELSE                                                   
168500                     IF MID-IDSYSTEM = 'APCC'                             
168600                       MOVE 'APC ' TO ORAD-IDSYSTEM                       
168700                     ELSE                                                 
168800                       IF MID-IDSYSTEM = 'APDC'                           
168900                         MOVE 'APD ' TO ORAD-IDSYSTEM                     
169000                       ELSE                                               
169100                         IF MID-IDSYSTEM = 'APEC'                         
169200                           MOVE 'APE ' TO ORAD-IDSYSTEM                   
169300                         ELSE                                             
169400                           IF MID-IDSYSTEM = 'APFC'                       
169500                             MOVE 'APF ' TO ORAD-IDSYSTEM                 
169600                           ELSE                                           
169700                             IF MID-IDSYSTEM = 'APGC'                     
169800                               MOVE 'APG ' TO ORAD-IDSYSTEM               
169900                             ELSE                                         
170000                               IF MID-IDSYSTEM = 'APHC'                   
170100                                 MOVE 'APH ' TO ORAD-IDSYSTEM             
170200                               ELSE                                       
170300                                 IF MID-IDSYSTEM = 'APIC'                 
170400                                   MOVE 'API ' TO ORAD-IDSYSTEM           
170500                                 ELSE                                     
170600                                   IF MID-IDSYSTEM = 'APJC'               
170700                                     MOVE 'APJ ' TO ORAD-IDSYSTEM         
170800                                   ELSE                                   
170900                                     MOVE MID-IDSYSTEM(1:3)               
171000                                                  TO ORAD-IDSYSTEM        
171100                                   END-IF                                 
171200                                 END-IF                                   
171300                               END-IF                                     
171400                             END-IF                                       
171500                           END-IF                                         
171600                         END-IF                                           
171700                        END-IF                                            
171800                     END-IF                                               
171900                   END-IF                                                 
172000                 END-IF                                                   
172100               END-IF                                                     
172200             END-IF                                                       
172300           END-IF                                                         
172400         END-IF                                                           
172500       END-IF                                                             
172600     ELSE                                                                 
172700       MOVE MID-IDSYSTEM       TO ORAD-IDSYSTEM                           
172800     END-IF                                                               
172900     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
173000                                                                          
173100     MOVE +1                   TO ORAD-KDDSP                              
173200     MOVE +0                   TO ORAD-KDVRINFO                           
173300     IF MID-IDSYSTEM = 'VR  '                                             
173400        MOVE OHUV-KDVRINFO     TO ORAD-KDVRINFO                           
173500        MOVE +1                TO ORAD-KDDSP                              
173600     END-IF                                                               
173700     IF MID-IDSYSTEM = 'VIPS' OR 'VDI '                                   
173800        MOVE MID-KDDSP (WS-INDEX-MID)                                     
173900                               TO WS-ALFA-1                               
174000        MOVE WS-NUM-1          TO ORAD-KDDSP                              
174100     END-IF                                                               
174200                                                                          
174300     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
174400     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
174500        MOVE +0                TO ORAD-KDKVBRYT                           
174600     ELSE                                                                 
174700        MOVE ORFK-KDKVBRYT(WS-INDEX-MID)                                  
174800                               TO ORAD-KDKVBRYT                           
174900     END-IF                                                               
175000     IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC'              
175100                               OR 'TADC' OR 'ACCC' OR 'APAC'              
175200                               OR 'APBC' OR 'APCC' OR 'APDC'              
175300                               OR 'APEC' OR 'APFC' OR 'APGC'              
175400                               OR 'APHC' OR 'APIC' OR 'APJC')             
175500       MOVE +3                 TO ORAD-KDORDING                           
175600     ELSE                                                                 
175700       MOVE OHUV-KDORDING      TO ORAD-KDORDING                           
175800     END-IF                                                               
175900     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
176000     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
176100                                  TEST-KDPRODSL                           
176200     IF ORAD-KDORDING = +3                                                
176300       MOVE SPACE              TO ORAD-KDOI                               
176400     ELSE                                                                 
176500       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
176600         MOVE 'CD'             TO ORAD-KDOI                               
176700       ELSE                                                               
176800         MOVE 'DT'             TO ORAD-KDOI                               
176900       END-IF                                                             
177000     END-IF                                                               
177100     MOVE SPACE                TO ORAD-CLEARGROUP                         
177200                                                                          
177300     IF TILLKOMMANDE-RAD                                                  
177400       IF DIST79-DEALER-PRICE                                             
177500         MOVE SPACE            TO ORAD-KDPRTYP                            
177600       ELSE                                                               
177700        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
177800           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
177900                               TO ORAD-KDPRTYP                            
178000        ELSE                                                              
178100           MOVE SPACE          TO ORAD-KDPRTYP                            
178200        END-IF                                                            
178300       END-IF                                                             
178400     ELSE                                                                 
178500        MOVE SPACE             TO ORAD-KDPRTYP                            
178600     END-IF                                                               
178700     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
178800     MOVE OHUV-KDTPOTYP        TO ORAD-KDTPOTYP                           
178900     MOVE JA                   TO ORAD-FLORDING                           
179000                                                                          
179100     IF TILLKOMMANDE-RAD                                                  
179200        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
179300                               TO ORAD-KVBEART                            
179400     ELSE                                                                 
179500        MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                      
179600        MOVE WS-NUM-6          TO ORAD-KVBEART                            
179700     END-IF                                                               
179800                                                                          
179900     MOVE +0                   TO ORAD-KVBEART-Q                          
180000     MOVE +0                   TO ORAD-KVPREAVB                           
180100     MOVE +0                   TO ORAD-KVPRERO                            
180200     MOVE +0                   TO ORAD-KVPRERO                            
180300     MOVE +0                   TO ORAD-KVOKS-PREL                         
180400                                                                          
180500     IF ORFK-PRARTBTO-LOC(WS-INDEX-MID) = ALL '+'                         
180600       MOVE +0                             TO  ORAD-PRARTBTO-LOC          
180700     ELSE                                                                 
180800      MOVE ORFK-PRARTBTO-LOC-UT(WS-INDEX-MID) TO ORAD-PRARTBTO-LOC        
180900     END-IF                                                               
181000                                                                          
181100*    IF MID-KDVALISO(WS-INDEX-MID) NOT = SPACE                            
181200        MOVE MID-KDVALISO(WS-INDEX-MID) TO ORAD-KDVALISO                  
181300*    ELSE                                                                 
181400*       MOVE ARB-KDVALISO(1)            TO ORAD-KDVALISO                  
181500*    END-IF                                                               
181600     MOVE MID-KDVAT(WS-INDEX-MID)       TO ORAD-KDVAT                     
181700                                                                          
181800*    IF ORFK-RERAB(WS-INDEX-MID) = ALL '+'                                
181900*      MOVE +0                          TO  ORAD-RERAB                    
182000*    ELSE                                                                 
182100*      MOVE ORFK-RERAB(WS-INDEX-MID) TO ORAD-RERAB                        
182200*    END-IF                                                               
182300                                                                          
182400     MOVE MID-RERAB(WS-INDEX-MID)       TO ORAD-RERAB                     
182500     MOVE MID-KDRAB(WS-INDEX-MID)       TO ORAD-KDRAB                     
182600     MOVE MID-BEART-VIPS(WS-INDEX-MID)  TO ORAD-BEART-VIPS                
182700                                                                          
182800     IF TILLKOMMANDE-RAD                                                  
182900       IF DIST79-DEALER-PRICE                                             
183000           MOVE +0             TO ORAD-PRARTNTO                           
183100           INITIALIZE          ORAD-DEAL-PR-LINE                          
183200       ELSE                                                               
183300        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
183400           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
183500                               TO ORAD-PRARTNTO                           
183600           MOVE ZERO           TO ORAD-PRARTNTO-LOC                       
183700        ELSE                                                              
183800           MOVE +0             TO ORAD-PRARTNTO                           
183900           INITIALIZE          ORAD-DEAL-PR-LINE                          
184000        END-IF                                                            
184100       END-IF                                                             
184200     ELSE                                                                 
184300        IF MID-PRARTNTO-LOC(WS-INDEX-MID) = ALL '+'                       
184400           MOVE +0             TO ORAD-PRARTNTO-LOC                       
184500        ELSE                                                              
184600           MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                        
184700                               TO ORAD-PRARTNTO-LOC                       
184800        END-IF                                                            
184900        IF MID-PRARTNTO-LOCPREL(WS-INDEX-MID) = ALL '+'                   
185000           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
185100        ELSE                                                              
185200           MOVE ORFK-PRARTNTO-LOCPREL-UT(WS-INDEX-MID)                    
185300                               TO ORAD-PRARTNTO-LOCPREL                   
185400        END-IF                                                            
185500        MOVE +0                TO ORAD-PRARTNTO                           
185600                                  ORAD-PRBPRIS                            
185700     END-IF                                                               
185800                                                                          
185900     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
186000        MOVE +0                TO ORAD-REKSIFFR                           
186100     ELSE                                                                 
186200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
186300     END-IF                                                               
186400     MOVE +0                   TO ORAD-RERF-RAD                           
186500     MOVE +0                   TO ORAD-KVSLATT                            
186600                                                                          
186700     IF TILLKOMMANDE-RAD                                                  
186800       IF DIST79-DEALER-PRICE                                             
186900         MOVE +0               TO ORAD-TIPRIS                             
187000       ELSE                                                               
187100        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
187200           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
187300                               TO ORAD-TIPRIS                             
187400        ELSE                                                              
187500           MOVE +0             TO ORAD-TIPRIS                             
187600        END-IF                                                            
187700       END-IF                                                             
187800     ELSE                                                                 
187900        MOVE +0                TO ORAD-TIPRIS                             
188000     END-IF                                                               
188100                                                                          
188200     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
188300     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
188400     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
188500     MOVE +0                   TO ORAD-TIRODAT                            
188600     MOVE OHUV-TITPO           TO ORAD-TITPO                              
188700     MOVE AREG-VKART           TO ORAD-VKART                              
188800     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
188900     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
189000                                                                          
189100     IF (ORAD-KDORDKL = 1 OR                                              
189200         ORAD-KDORDKL = 3) AND                                            
189300        GMT-FLLDCKND = JA                                                 
189400        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
189500     ELSE                                                                 
189600        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
189700     END-IF                                                               
189800     MOVE +0                   TO ORAD-PRAVCOST                           
189900     .                                                                    
190000     EJECT                                                                
190100                                                                          
190200 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
190300                                                                          
190400     PERFORM S40-HAMTA-WDB6-INFO                                          
190500     IF ALLT-OK AND DCS-CDC                                               
190600                                                                          
190700     MOVE ORAD-IDARTNR                TO W-IDARTNR                        
190800     PERFORM IMS-10-GU-WLARTM-WDK901                                      
190900                                                                          
191000     IF SEGMENT-SAKNAS                                                    
191100        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
191200        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
191300     END-IF                                                               
191400                                                                          
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800                                                                          
191900 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
192000                                                                          
192100     IF ALLT-OK                                                           
192200                                                                          
192300     IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU' OR          
192400                             'TAD' OR 'ACC' OR 'APA' OR 'APB' OR          
192500                             'APC' OR 'APD' OR 'APE' OR 'APF' OR          
192600                             'APG' OR 'APH' OR 'API' OR                   
192700                             'APJ' )                                      
192800       MOVE JA                 TO KVAN-FLORDSPE-IN                        
192900     ELSE                                                                 
193000       MOVE OHUV-FLORDSPE      TO KVAN-FLORDSPE-IN                        
193100     END-IF                                                               
193200                                                                          
193300     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
193400     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
193500     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
193600     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
193700     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
193800     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
193900     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
194000     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
194100     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
194200     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
194300     MOVE OHUV-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
194400     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
194500     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
194600     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
194700     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
194800     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
194900     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
195000     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
195100                                                                          
195200     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
195300                                                                          
195400     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
195500     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
195600                                                                          
195700     IF KVAN-KDORDBEK-UT > +0                                             
195800        MOVE JA                    TO OBKR-SW                             
195900     END-IF                                                               
196000                                                                          
196100     END-IF                                                               
196200     .                                                                    
196300     EJECT                                                                
196400                                                                          
196500 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
196600                                                                          
196700     PERFORM S40-HAMTA-WDB6-INFO                                          
196800     IF ALLT-OK AND DCS-CDC                                               
196900                                                                          
197000     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
197100     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
197200     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
197300     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
197400     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
197500     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
197600     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
197700     MOVE ORAD-IDDC            TO DLEV-IDDC-IN                            
197800     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
197900     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
198000     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
198100     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
198200     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
198300     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
198400     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
198500     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
198600                                                                          
198700     MOVE +1 TO WS-INDEX                                                  
198800     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
198900        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
199000                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
199100        ADD +1 TO WS-INDEX                                                
199200     END-PERFORM                                                          
199300                                                                          
199400     MOVE ZERO                 TO DLEV-KDCALL                             
199500                                  DLEV-IDKUNDRF-IN                        
199600     MOVE SPACE                TO DLEV-CLEARGROUP                         
199700                                  DLEV-KDOI-UT                            
199800                                                                          
199900     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
200000                                       DLEV-LEVG-PCB                      
200100                                       DLEV-LEVA-PCB                      
200200                                       DLEV-ARTS-PCB                      
200300                                       DLEV-WDB6-PCB                      
200400                                       DUMMY-PCB                          
200500                                                                          
200600     IF OHUV-FLOVRLEV = JA                                                
200700        MOVE +0                  TO DLEV-KDORDBEK-UT                      
200800     END-IF                                                               
200900     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                                 
201000        MOVE JA                  TO OBKR-SW                               
201100        MOVE NEJ                 TO ALLT-SW                               
201200        MOVE ZERO                TO KVAN-KDORDBEK-UT                      
201300     ELSE                                                                 
201400***                                                                       
201500***  MAN FÅR EJ LÄGGA TILL DIREKTLEVERANSRADER FRÅN VDI                   
201600***                                                                       
201700        IF DLEV-IDLEVNR-UT NOT = SPACE OR                                 
201800           DLEV-FLSDCLEV-UT = JA                                          
201900           MOVE 82               TO DLEV-KDORDBEK-UT                      
202000           MOVE JA               TO OBKR-SW                               
202100           MOVE NEJ              TO ALLT-SW                               
202200           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
202300           IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                             
202400             MOVE DLEV-IDDC-UT   TO WS-IDDC-DDGS                          
202500           END-IF                                                         
202600        END-IF                                                            
202700        MOVE NEJ                 TO ORAD-FLSDCLEV                         
202800     END-IF                                                               
202900     IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                       
203000        AND ORAD-IDDC = WC-CDC-SE                                         
203100        MOVE 26               TO DLEV-KDORDBEK-UT                         
203200        MOVE JA               TO OBKR-SW                                  
203300     END-IF                                                               
203400                                                                          
203500     END-IF                                                               
203600     .                                                                    
203700     EJECT                                                                
203800 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
203900                                                                          
204000     IF ALLT-OK                                                           
204100                                                                          
204200     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
204300     MOVE ORAD-IDDC            TO KERS-IDDC                               
204400     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
204500     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
204600     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
204700     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
204800     MOVE AREG-KDERS           TO KERS-KDERS                              
204900     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
205000     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
205100     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
205200     MOVE AREG-KDUART          TO KERS-KDUART                             
205300     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
205400     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
205500     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
205600     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
205700     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
205800     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
205900     MOVE ZERO                 TO WX-KDORDBEK                             
206000                                                                          
206100     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
206200                         KERS-ARTC-PCB KERS-ERSA-PCB                      
206300                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
206400                                                                          
206500     IF KERS-KDORDBEK > ZERO   AND                                        
206600        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
206700          KERS-KDERS > 10 )                                               
206800        MOVE JA                  TO OBKR-SW                               
206900        MOVE NEJ                 TO ALLT-SW                               
207000     END-IF                                                               
207100                                                                          
207200     PERFORM S40-HAMTA-WDB6-INFO                                          
207300                                                                          
207400     IF KERS-KDORDBEK > +0                                                
207500        IF  DCS-NDC  AND AREG-KDERS > 18                                  
207600           MOVE JA                  TO KOLLA-ERS-SW                       
207700        END-IF                                                            
207800                                                                          
207900        IF DCS-SDC                                                        
208000          IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR 19          
208100                       OR 21 OR 22 OR 24 OR 25 OR 27 OR 28 OR 29          
208200             MOVE JA            TO KOLLA-ERS-SW                           
208300          END-IF                                                          
208400        END-IF                                                            
208500     END-IF                                                               
208600                                                                          
208700     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
208800        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
208900***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
209000***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
209100***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
209200***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
209300           CONTINUE                                                       
209400        ELSE                                                              
209500           MOVE +0               TO KVAN-KDORDBEK-UT                      
209600           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
209700        END-IF                                                            
209800     END-IF                                                               
209900     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
210000        MOVE +0                  TO DLEV-KDORDBEK-UT                      
210100     END-IF                                                               
210200                                                                          
210300     ELSE                                                                 
210400        MOVE +0                  TO KERS-KDERS                            
210500     END-IF                                                               
210600     .                                                                    
210700     EJECT                                                                
210800                                                                          
210900 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
211000                                                                          
211100     IF ALLT-OK OR KOLLA-ERS                                              
211200                                                                          
211300     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
211400     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
211500     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
211600     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
211700     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
211800     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
211900     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
212000     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
212100     MOVE AREG-FLIART          TO SPAR-FLIART                             
212200     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
212300     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
212400     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
212500     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
212600     MOVE ORAD-IDDC            TO SPAR-IDDC                               
212700     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
212800     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
212900     MOVE AREG-KDERS           TO SPAR-KDERS                              
213000     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
213100     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
213200     MOVE +1                   TO SPAR-KDORDBEH                           
213300     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
213400     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
213500     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
213600     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
213700     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
213800     MOVE AREG-KDUART          TO SPAR-KDUART                             
213900     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
214000     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
214100     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
214200     MOVE ORAD-TITPO           TO SPAR-TITPO                              
214300     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
214400     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
214500     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
214600     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
214700     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
214800                                                                          
214900     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
215000                                       SPAR-WDF8A-PCB                     
215100                                       SPAR-WDK6-PCB                      
215200                                                                          
215300     IF SPAR-KDORDBEK > +0                                                
215400        MOVE JA             TO OBKR-SW                                    
215500        MOVE NEJ            TO ALLT-SW                                    
215600        IF SPAR-KDORDBEK = 51 OR 67 OR 58                                 
215700          MOVE NEJ            TO KOLLA-ERS-SW                             
215800          MOVE ZERO            TO KERS-KDORDBEK                           
215900          PERFORM S02-RENSA-TILLK-TAB                                     
216000        END-IF                                                            
216100        MOVE ZERO             TO XDCA-DAPUBL                              
216100*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411XDCA                         
216200        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
216300*         MOVE 99999999       TO NDCA-DAPUBL                              
216400          MOVE 99999999       TO XDCA-DAPUBL                              
216500        END-IF                                                            
216600        IF KVAN-KDORDBEK-UT > +0                                          
216700           MOVE +0          TO KVAN-KDORDBEK-UT                           
216800           MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                            
216900        END-IF                                                            
217000        IF DLEV-KDORDBEK-UT > +0                                          
217100           MOVE +0          TO DLEV-KDORDBEK-UT                           
217200        END-IF                                                            
217300     END-IF                                                               
217400     IF AREG-KDSORT = 'SW'                                                
217500        MOVE 67                     TO SPAR-KDORDBEK                      
217600        MOVE JA                     TO OBKR-SW                            
217700        MOVE NEJ                    TO ALLT-SW                            
217800        IF KOLLA-ERS-SW = JA                                              
217900          MOVE NEJ            TO KOLLA-ERS-SW                             
218000          MOVE ZERO            TO KERS-KDORDBEK                           
218100          PERFORM S02-RENSA-TILLK-TAB                                     
218200        END-IF                                                            
218300     END-IF                                                               
218400                                                                          
218500     END-IF                                                               
218600     .                                                                    
218700     EJECT                                                                
218800                                                                          
218900 ECJ-KOMPLETTERA-PRIS SECTION.                                            
219000                                                                          
219100     IF ALLT-OK OR KOLLA-ERS                                              
219200                                                                          
219300     IF DIST79-DEALER-PRICE                                               
219400*/ETRACKER 6922887 PRISFRÅGENR MED VID CLEARING TILL NYTT DC              
219500      IF (MID-IDSYSTEM = 'LDCC') AND                                      
219600        MID-IDPRQUES(WS-INDEX-MID) > ZERO AND EJ-TILLKOMMANDE-RAD         
219700          MOVE MID-IDPRQUES(WS-INDEX-MID)  TO ORAD-IDPRQUES               
219800                                                                          
219900          IF ORAD-KDVALISO  = SPACE                                       
220000            PERFORM ECJB-HAEMTA-KDVALISO-DNI                              
220100          END-IF                                                          
220200                                                                          
220300         IF DIST03-SVERIGE                                                
220400           AND ORAD-KDORDKL < 3                                           
220500           MOVE JA               TO ORAD-FLPRTILL                         
220600         END-IF                                                           
220700                                                                          
220800      ELSE                                                                
220900       IF ORAD-PRARTNTO-LOC = +0    OR                                    
221000          ORAD-PRARTBTO-LOC = +0    OR                                    
221100          ORAD-KDVALISO     = SPACE OR                                    
221200          ORAD-KDVAT        = SPACE OR                                    
221300          ORAD-RERAB        = +0    OR                                    
221400          ORAD-KDRAB        = SPACE OR                                    
221500          ORAD-BEART-VIPS   = SPACE                                       
221600                                                                          
221700         IF WS-IDPRQUES                = +0                               
221800           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
221900           MOVE +1                    TO PRNO-KDCALL                      
222000                                                                          
222100           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
222200                                                                          
222300           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
222400                                         WS-IDPRQUES                      
222500           MOVE +1                    TO PRQU-KDCALL                      
222600         ELSE                                                             
222700           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
222800           MOVE +2                    TO PRNO-KDCALL                      
222900                                                                          
223000           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
223100                                                                          
223200           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
223300                                         WS-IDPRQUES                      
223400           MOVE +2                    TO PRQU-KDCALL                      
223500         END-IF                                                           
223600                                                                          
223700         MOVE W-IDDISTR                TO PRQU-IDDISTR                    
223800         MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                   
223900         MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                   
224000         MOVE ORAD-IDORDER             TO PRQU-IDORDER                    
224100         MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                    
224200         MOVE 'N'                      TO PRQU-KDPRSTA                    
224300         MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                    
224400         MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                  
224500         MOVE ORAD-KDVALISO            TO PRQU-KDVALISO                   
224600         MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC               
224700         MOVE +0                       TO PRQU-PRARTNTO-LOCPREL           
224800         MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                   
224900                                                                          
225000*        PERFORM ECJA-HAEMTA-STA-STO-DATUM                                
225100                                                                          
225200         IF ORAD-KDVALISO  = SPACE                                        
225300           PERFORM ECJB-HAEMTA-KDVALISO-DNI                               
225400         END-IF                                                           
225500                                                                          
225600         MOVE ORAD-KDVALISO            TO PRQU-KDVALISO                   
225700                                                                          
225800         CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                  
225900                                           PRQU-WDC7-PCB                  
226000                                           PRQU-SJKO-WDK6-PCB             
226100                                                                          
226200         MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                  
226300                                           WS-IDPRQUES                    
226400         MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                  
226500         IF ORAD-PRARTNTO-LOC = +0                                        
226600           MOVE PRQU-PRARTNTO-LOCPREL TO ORAD-PRARTNTO-LOCPREL            
226700         END-IF                                                           
226800                                                                          
226900       END-IF                                                             
227000       IF ORAD-IDSYSTEM = 'OREL'                                          
227100          IF ORAD-PRARTNTO-LOC NOT = +0                                   
227200            IF ORAD-KDPRTYP = SPACE                                       
227300              MOVE 'P'            TO ORAD-KDPRTYP                         
227400              MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                          
227500            END-IF                                                        
227600          END-IF                                                          
227700       END-IF                                                             
227800      END-IF                                                              
227900     ELSE                                                                 
228000*        *NOT DIST79-DEALER-PRICE                                         
228100         IF ORAD-KDTPOTYP = +0   AND                                      
228200            (AREG-KDUART NOT = 'M' AND 'P' AND 'S' AND 'L')               
228300                                                                          
229400             IF DIST79-ECOM-PRICE                                         
229500               CONTINUE                                                   
229600             ELSE                                                         
229700               IF ORAD-PRARTNTO NOT = +0                                  
229800*                *FETCH ONLY KDVALISO FROM W335PRIS                       
229900                 MOVE 2                TO PRIS-KDCALL                     
230000               ELSE                                                       
230100*              *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS        
230200                 MOVE 1                TO PRIS-KDCALL                     
230300               END-IF                                                     
230400             END-IF                                                       
230600         ELSE                                                             
230700             MOVE 2                  TO PRIS-KDCALL                       
230800         END-IF                                                           
230900                                                                          
231000         MOVE IDPGM                TO PRIS-IDPGM                          
231100         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
231200         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
231300         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
231400         MOVE ORAD-IDDC            TO PRIS-IDDC                           
231500         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
231600         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
231700         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
231800                                                                          
231900         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
232000                             PRIS-WDK7-PCB                                
232100                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
232200                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
232300                             PRIS-COST-WDK6-PCB                           
232400                             PRIS-COST-WDK7-PCB                           
232500                             PRIS-COST-WDF1-PCB                           
232600                             PRIS-COST-9305-PCB                           
232700                             PRIS-COST-WDK72-PCB                          
232800                             PRIS-COST-WDB6-PCB                           
232900                                                                          
233000         IF PRIS-KDSVAR = '2'                                             
233100           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
233200                                        TO FELTEXT                        
233300           CALL ABEND USING RKOD-ABEND-33                                 
233400         END-IF                                                           
233500                                                                          
233600         MOVE WS-IDPRQUES               TO ORAD-IDPRQUES                  
233700         IF PRIS-KDCALL = 2                                               
233800           MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                           
233900           IF ORAD-KDPRTYP = SPACE                                        
234000             MOVE 'P'           TO ORAD-KDPRTYP                           
234100             MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                            
234200           END-IF                                                         
234300         ELSE                                                             
234400           IF DIST79-ECOM-PRICE                                           
234600             MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO-LOC                     
234700                                    ORAD-PRARTBTO-LOC                     
234800           ELSE                                                           
234900             MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO                         
235000           END-IF                                                         
235100           MOVE PRIS-FLPRTILL   TO ORAD-FLPRTILL                          
235200           MOVE PRIS-KDPRTYP    TO ORAD-KDPRTYP                           
235300           MOVE PRIS-PRBPRIS    TO ORAD-PRBPRIS                           
235400           MOVE ORAD-TIREGDAT   TO ORAD-TIPRIS                            
235500           MOVE PRIS-KDVALISO   TO ORAD-KDVALISO                          
235600           MOVE PRIS-PRAVCOST   TO ORAD-PRAVCOST                          
235700         END-IF                                                           
235800     END-IF                                                               
235900                                                                          
236000     END-IF                                                               
236100     .                                                                    
236200     EJECT                                                                
236300                                                                          
236400*ECJA-HAEMTA-STA-STO-DATUM SECTION.                                       
236500*                                                                         
236600*     MOVE W-IDDISTR                TO W-IDDISTR-WDB2                     
236700*                                      W-IDDISTR-WDB2-MIN                 
236800*                                      W-IDDISTR-WDB2-MAX                 
236900*     MOVE W-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
237000*     PERFORM IMS-GET-WDB201-UNIK                                         
237100*     IF SEGMENT-FINNS                                                    
237200*        CONTINUE                                                         
237300*     ELSE                                                                
237400*        PERFORM IMS-GU-WDB201                                            
237500*     END-IF                                                              
237600*     .                                                                   
237700*     EJECT                                                               
237800 ECJB-HAEMTA-KDVALISO-DNI  SECTION.                                       
237900                                                                          
238000      MOVE ORAD-IDDC            TO WS-IDDC                                
238100      PERFORM S40-HAMTA-WDB6-INFO                                         
238200                                                                          
238300      MOVE GMT-IDPARTNR              TO W-WDB1-IDPARTNR                   
238400      MOVE GMT-IDFTG                 TO W-WDB1-IDFTG                      
238500                                                                          
238600      PERFORM IMS-GU-WDB101                                               
238700      MOVE BET-KDVALISO              TO ORAD-KDVALISO                     
238800      .                                                                   
238900                                                                          
239000 ECM-KOMPLETTERA-TPO1 SECTION.                                            
239100                                                                          
239200     PERFORM S40-HAMTA-WDB6-INFO                                          
239300     IF ALLT-OK AND (DCS-CDC OR                                           
239400                     DCS-SDC)                                             
239500                                                                          
239600     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
239700     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
239800     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
239900     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
240000     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
240100     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
240200     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
240300     MOVE OHUV-IDKST           TO TPO1-IDKST                              
240400     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
240500     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
240600     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
240700     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
240800     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
240900     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
241000     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
241100     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
241200     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
241300     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
241400     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
241500     MOVE ORAD-TITPO           TO TPO1-TITPO                              
241600     IF ORAD-KDPRTYP = 'P'                                                
241700        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
241800        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
241900        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
242000        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
242100     ELSE                                                                 
242200        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
242300        MOVE ZERO              TO TPO1-PRARTNTO                           
242400        MOVE SPACE             TO TPO1-KDPRTYP                            
242500        MOVE NEJ               TO TPO1-FLPRTILL                           
242600     END-IF                                                               
242700     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
242800     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
242900     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
243000     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
243100     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
243200     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
243300     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
243400     MOVE 1                    TO TPO1-KDORDBEH                           
243500     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
243600     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
243700     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
243800     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
243900     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
244000                                                                          
244100     MOVE OHUV-KDORDTYP-LDC   TO TPO1-KDORDTYP-LDC                        
244200     MOVE OHUV-TIREPDAT       TO TPO1-TIREPDAT                            
244300     MOVE ORAD-IDKUNDRF-WIP   TO TPO1-IDKUNDRF-WIP                        
244400                                                                          
244500     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
244600                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
244700     IF TPO1-KDORDBEK > 0                                                 
244800        MOVE JA                TO OBKR-SW                                 
244900        MOVE NEJ               TO ALLT-SW                                 
245000        IF KVAN-KDORDBEK-UT > +0                                          
245100           MOVE 0              TO KVAN-KDORDBEK-UT                        
245200           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
245300        END-IF                                                            
245400        IF DLEV-KDORDBEK-UT > +0                                          
245500           MOVE 0              TO DLEV-KDORDBEK-UT                        
245600        END-IF                                                            
245700     ELSE                                                                 
245800        IF TPO1-FLKLAR = JA                                               
245900           MOVE NEJ            TO ALLT-SW                                 
246000           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
246100              MOVE ZERO        TO KERS-KDORDBEK                           
246200              PERFORM S02-RENSA-TILLK-TAB                                 
246300              MOVE NEJ         TO TILLK-SW                                
246400           END-IF                                                         
246500        END-IF                                                            
246600     END-IF                                                               
246700                                                                          
246800     END-IF                                                               
246900     .                                                                    
247000     EJECT                                                                
247100 ECN-KOMPLETTERA-TPO2 SECTION.                                            
247200                                                                          
247300     PERFORM S40-HAMTA-WDB6-INFO                                          
247400     IF ALLT-OK AND (DCS-CDC OR                                           
247500                     DCS-SDC)                                             
247600                                                                          
247700     MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                            
247800     MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                           
247900     MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                           
248000     MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                            
248100     MOVE ORAD-BERADREF        TO TPO2-BERADREF                           
248200     MOVE AREG-IDANSK          TO TPO2-IDANSK                             
248300     MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                            
248400     MOVE OHUV-IDKST           TO TPO2-IDKST                              
248500     MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                           
248600     MOVE ORAD-KDDSP           TO TPO2-KDDSP                              
248700     MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                           
248800     MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                            
248900     MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                           
249000     MOVE ORAD-KDORDING        TO TPO2-KDORDING                           
249100     MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                            
249200     MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                           
249300     MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                           
249400     MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                          
249500     MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                           
249600     MOVE ORAD-TITPO           TO TPO2-TITPO                              
249700     MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                           
249800     MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                       
249900     MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                            
250000     MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                           
250100     MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                           
250200     MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                           
250300     MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                           
250400     MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                           
250500     MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                            
250600     MOVE AREG-KDUART          TO TPO2-KDUART                             
250700     MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                           
250800*    SÄTT KDORDBEH TILL 8 FÖR BATCH/MPP. DETTA INNEBÄR ATT EN             
250900*    KDUART-ARTIKEL MED EN GÅNG KOMMER ATT LÄGGAS UPP PÅ RESTORDEW        
251000*    REGISTER OCH ANSKAFFNINGSLARM SKAPAS.                                
251100     MOVE 8                    TO TPO2-KDORDBEH                           
251200     MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                           
251300     MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                           
251400     MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                           
251500     MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                           
251600     MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                            
251700     MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                            
251800     MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                           
251900     MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                           
252000     MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                          
252100     MOVE ORAD-KVBEART         TO TPO2-KVBEART                            
252200                                                                          
252300     MOVE OHUV-KDORDTYP-LDC   TO TPO2-KDORDTYP-LDC                        
252400     MOVE OHUV-TIREPDAT       TO TPO2-TIREPDAT                            
252500     MOVE ORAD-IDKUNDRF-WIP   TO TPO2-IDKUNDRF-WIP                        
252600                                                                          
252700     CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                      
252800                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
252900                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
253000                         TIME-4437-PCB                                    
253100     IF TPO2-KDORDBEK > 0                                                 
253200        MOVE JA                TO OBKR-SW                                 
253300        MOVE NEJ               TO ALLT-SW                                 
253400        MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                           
253500        IF ORAD-KDTPOTYP = 6                                              
253600           PERFORM S21-UPPDATERA-TPO6                                     
253700        END-IF                                                            
253800     ELSE                                                                 
253900        IF TPO2-FLKLAR = JA                                               
254000           MOVE NEJ            TO ALLT-SW                                 
254100           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
254200              MOVE ZERO        TO KERS-KDORDBEK                           
254300              PERFORM S02-RENSA-TILLK-TAB                                 
254400              MOVE NEJ         TO TILLK-SW                                
254500           END-IF                                                         
254600        END-IF                                                            
254700     END-IF                                                               
254800                                                                          
254900     END-IF                                                               
255000     .                                                                    
255100     EJECT                                                                
255200 ECO-KOMPLETTERA-TPO3 SECTION.                                            
255300                                                                          
255400     PERFORM S40-HAMTA-WDB6-INFO                                          
255500     IF ALLT-OK AND (DCS-CDC OR                                           
255600                     DCS-SDC)                                             
255700                                                                          
255800       MOVE ORAD-IDDISTR       TO TPO3-IDDISTR                            
255900       MOVE ORAD-IDKUNDNR      TO TPO3-IDKUNDNR                           
256000       MOVE ORAD-IDKUNDRF      TO TPO3-IDKUNDRF                           
256100       MOVE ORAD-IDARTNR       TO TPO3-IDARTNR                            
256200       MOVE ORAD-BERADREF      TO TPO3-BERADREF                           
256300       MOVE AREG-IDANSK        TO TPO3-IDANSK                             
256400       MOVE OHUV-IDKONTO       TO TPO3-IDKONTO                            
256500       MOVE OHUV-IDKST         TO TPO3-IDKST                              
256600       MOVE OHUV-IDANALYS      TO TPO3-IDANALYS                           
256700       MOVE ORAD-KDDSP         TO TPO3-KDDSP                              
256800       MOVE OHUV-KDFAKTYP      TO TPO3-KDFAKTYP                           
256900       MOVE ARB-KDFRAKT        TO TPO3-KDFRAKT                            
257000       MOVE ORAD-KDKVBRYT      TO TPO3-KDKVBRYT                           
257100       MOVE ORAD-KDORDING      TO TPO3-KDORDING                           
257200       MOVE OHUV-KDORDKL       TO TPO3-KDORDKL                            
257300       MOVE AREG-KDPRODSL      TO TPO3-KDPRODSL                           
257400       MOVE ORAD-KDVRINFO      TO TPO3-KDVRINFO                           
257500       MOVE ORAD-KVBEART-Q     TO TPO3-KVBEART-Q                          
257600       MOVE AREG-REKSIFFR      TO TPO3-REKSIFFR                           
257700       MOVE ORAD-TITPO         TO TPO3-TITPO                              
257800       IF ORAD-KDPRTYP = 'P'                                              
257900          MOVE ORAD-PRARTNTO   TO TPO3-PRARTNTO                           
258000          MOVE ORAD-DEAL-PR-LINE TO TPO3-DEAL-PR-LINE                     
258100          MOVE ORAD-KDPRTYP    TO TPO3-KDPRTYP                            
258200          MOVE ORAD-FLPRTILL   TO TPO3-FLPRTILL                           
258300       ELSE                                                               
258400          MOVE ORAD-DEAL-PR-LINE TO TPO3-DEAL-PR-LINE                     
258500          MOVE ZERO            TO TPO3-PRARTNTO                           
258600          MOVE SPACE           TO TPO3-KDPRTYP                            
258700          MOVE NEJ             TO TPO3-FLPRTILL                           
258800       END-IF                                                             
258900       MOVE ORAD-BEVOLREF      TO TPO3-BEVOLREF                           
259000       MOVE ORAD-IDKAMPRF      TO TPO3-IDKAMPRF                           
259100       MOVE ORAD-IDSYSTEM      TO TPO3-IDSYSTEM                           
259200       MOVE ORAD-FLINVEST      TO TPO3-FLINVEST                           
259300       MOVE ORAD-IDLEVNR       TO TPO3-IDLEVNR                            
259400       MOVE ORAD-KDTPOTYP      TO TPO3-KDTPOTYP                           
259500       MOVE OHUV-BEKUNDRF      TO TPO3-BEKUNDRF                           
259600      MOVE OHUV-FLFORBI       TO TPO3-FLFORBI                             
259700                                                                          
259800       MOVE OHUV-KDORDTYP-LDC TO TPO3-KDORDTYP-LDC                        
259900       MOVE OHUV-TIREPDAT     TO TPO3-TIREPDAT                            
260000       MOVE ORAD-IDKUNDRF-WIP TO TPO3-IDKUNDRF-WIP                        
260100                                                                          
260200       CALL W411TPO3 USING TPO3-W411TPO3 TPO3-ORDP-PCB                    
260300                           TPO3-ZZAC-PCB                                  
260400                                                                          
260500       IF TPO3-KDORDBEK > 0                                               
260600          MOVE JA              TO OBKR-SW                                 
260700          MOVE NEJ             TO ALLT-SW                                 
260800          IF KVAN-KDORDBEK-UT > 0                                         
260900             MOVE ZERO         TO KVAN-KDORDBEK-UT                        
261000             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
261100          END-IF                                                          
261200          IF DLEV-KDORDBEK-UT > 0                                         
261300             MOVE ZERO         TO DLEV-KDORDBEK-UT                        
261400          END-IF                                                          
261500       ELSE                                                               
261600          IF TPO3-FLKLAR = JA                                             
261700             MOVE NEJ          TO ALLT-SW                                 
261800             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
261900                MOVE ZERO      TO KERS-KDORDBEK                           
262000                PERFORM S02-RENSA-TILLK-TAB                               
262100                MOVE NEJ       TO TILLK-SW                                
262200             END-IF                                                       
262300          END-IF                                                          
262400       END-IF                                                             
262500                                                                          
262600     END-IF                                                               
262700     .                                                                    
262800     EJECT                                                                
262900 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
263000                                                                          
263100       IF ALLT-OK AND W-KDORDBEK = 56                                     
263200                                                                          
263300         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
263400         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
263500         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
263600         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
263700         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
263800         MOVE AREG-IDANSK          TO RELS-IDANSK                         
263900         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
264000         MOVE OHUV-IDKST           TO RELS-IDKST                          
264100         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
264200         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
264300         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
264400         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
264500         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
264600         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
264700         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
264800         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
264900         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
265000         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
265100         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
265200         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
265300         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
265400         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
265500         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
265600         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
265700         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
265800         EJECT                                                            
265900         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
266000         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
266100         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
266200         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
266300         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
266400         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
266500         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
266600         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
266700         MOVE AREG-KDUART          TO RELS-KDUART                         
266800         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
266900         MOVE +1                   TO RELS-KDORDBEH                       
267000         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
267100         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
267200         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
267300         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
267400         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
267500         MOVE 0                    TO RELS-KVQPACK-1                      
267600         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
267700         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
267800                                                                          
267900         MOVE SPACE                TO RELS-FLKLAR                         
268000                                                                          
268100         MOVE OHUV-KDORDTYP-LDC   TO RELS-KDORDTYP-LDC                    
268200         MOVE OHUV-TIREPDAT       TO RELS-TIREPDAT                        
268300         MOVE ORAD-IDKUNDRF-WIP   TO RELS-IDKUNDRF-WIP                    
268400                                                                          
268500         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
268600                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
268700                                                                          
268800         PERFORM ECTA-ANDRA-WDC711                                        
268900         IF RELS-KDORDBEK > +0                                            
269000            MOVE JA                TO OBKR-SW                             
269100            MOVE NEJ               TO ALLT-SW                             
269200            MOVE WC-CDC-SE         TO ORAD-IDDC                           
269300            MOVE ORAD-IDDC         TO WS-IDDC                             
269400         ELSE                                                             
269500            IF RELS-FLKLAR = JA                                           
269600               MOVE NEJ            TO ALLT-SW                             
269700               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
269800                  MOVE ZERO        TO KERS-KDORDBEK                       
269900                  PERFORM S02-RENSA-TILLK-TAB                             
270000               END-IF                                                     
270100            END-IF                                                        
270200         END-IF                                                           
270300                                                                          
270400       MOVE +0              TO W-KDORDBEK                                 
270500       MOVE SPACE           TO RELS-FLKLAR                                
270600                                                                          
270700       END-IF                                                             
270800     .                                                                    
270900     EJECT                                                                
271000 ECTA-ANDRA-WDC711 SECTION.                                               
271100                                                                          
271200     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
271300       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
271400         INITIALIZE PRQU-W335PRQU                                         
271500         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
271600         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
271700         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
271800         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
271900         MOVE 6                  TO PRQU-KDCALL                           
272000         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
272100                                            PRQU-WDC7-PCB                 
272200                                            PRQU-SJKO-WDK6-PCB            
272300         MOVE 'N'                TO ORAD-FLPRTILL                         
272400       END-IF                                                             
272500     END-IF                                                               
272600     .                                                                    
272700     EJECT                                                                
272800 ECP-KOMPLETTERA-KAMPANJER SECTION.                                       
272900                                                                          
273000     PERFORM S40-HAMTA-WDB6-INFO                                          
273100     IF ALLT-OK AND (DCS-CDC OR                                           
273200                     DCS-SDC)                                             
273300                                                                          
273400       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
273500       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
273600       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
273700       MOVE ORAD-IDDC          TO KAMP-IDDC                               
273800       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
273900       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
274000       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
274100       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
274200       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
274300       MOVE OHUV-IDKST         TO KAMP-IDKST                              
274400       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
274500       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
274600       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
274700       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
274800       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
274900       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
275000       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
275100       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
275200       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
275300       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
275400       MOVE ORAD-TITPO         TO KAMP-TITPO                              
275500       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
275600          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
275700          MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                     
275800          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
275900          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
276000       ELSE                                                               
276100          MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                     
276200          MOVE ZERO            TO KAMP-PRARTNTO                           
276300          MOVE SPACE           TO KAMP-KDPRTYP                            
276400          MOVE NEJ             TO KAMP-FLPRTILL                           
276500       END-IF                                                             
276600       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
276700       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
276800       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
276900       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
277000       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
277100       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
277200       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
277300       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
277400       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
277500       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
277600       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
277700                                                                          
277800       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
277900                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
278000       IF KAMP-KDORDBEK > +0                                              
278100          MOVE JA              TO OBKR-SW                                 
278200          MOVE NEJ             TO ALLT-SW                                 
278300          IF KVAN-KDORDBEK-UT > +0                                        
278400             MOVE +0           TO KVAN-KDORDBEK-UT                        
278500             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
278600          END-IF                                                          
278700          IF DLEV-KDORDBEK-UT > +0                                        
278800             MOVE +0           TO DLEV-KDORDBEK-UT                        
278900          END-IF                                                          
279000       ELSE                                                               
279100          IF KAMP-FLKLAR = JA                                             
279200             MOVE NEJ          TO ALLT-SW                                 
279300             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
279400                MOVE ZERO      TO KERS-KDORDBEK                           
279500                PERFORM S02-RENSA-TILLK-TAB                               
279600                MOVE NEJ       TO TILLK-SW                                
279700             END-IF                                                       
279800          END-IF                                                          
279900       END-IF                                                             
280000                                                                          
280100     END-IF                                                               
280200     .                                                                    
280300     EJECT                                                                
280400                                                                          
280500 ECG-PREL-AVBOKNING-XDC SECTION.                                          
280600                                                                          
280700     IF ALLT-OK OR KOLLA-ERS                                              
280800                                                                          
280900       PERFORM S40-HAMTA-WDB6-INFO                                        
281000       IF DCS-NDC                                                         
281100         MOVE JA TO ALLT-SW                                               
281200         PERFORM ECGX-PREL-AVBOKNING-XDC                                  
281300                                                                          
281400*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
281500*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
281600*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
281700*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
281800*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
281900*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
282000*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
282100*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
282200*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
282300*        MOVE OHUV-IDDC-PRIM       TO NDCA-IDDC-TVS                       
282400*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
282500*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
282600*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
282700*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
282800*        MOVE AREG-KDERS           TO NDCA-KDERS                          
282900*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
283000*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
283100*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
283200*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
283300*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
283400*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
283500*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
283600*        MOVE ORAD-VKART           TO NDCA-VKART                          
283700*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
283800*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
283900*        RUNNING NDCA IN PASSIVE MODE                                     
284000*        MOVE +2                   TO NDCA-KDCALL                         
284100*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
284200*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
284300*                                     NDCA-XDK7-KVOKS-DAG                 
284400*                                     NDCA-XDK7-KVOKS-BULK                
284500                                                                          
284600*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
284700*                                          NDCA-USEA-PCB                  
284800*                                          NDCA-WDK7-PCB                  
284900*                                          NDCA-INLC-PCB                  
285000*                                          NDCA-WDB6-PCB                  
285100*                                          NDCA-XDK7-W411XDK7             
285200                                                                          
285300*        PERFORM ECGX-CHECK-DIFF                                          
285400         IF XDCA-KDORDBEK > ZERO                                          
285500           IF KOLLA-ERS                                                   
285600              MOVE NEJ               TO KOLLA-ERS-SW                      
285700              MOVE ZERO              TO XDCA-KDORDBEK                     
285800              MOVE ZERO              TO SPAR-KDORDBEK                     
285900              IF XDCA-KVPREAVB > 0                                        
286000                MOVE NEJ             TO KOLLA-ERS-SW                      
286100                MOVE ZERO            TO KERS-KDORDBEK                     
286200                PERFORM S02-RENSA-TILLK-TAB                               
286300              END-IF                                                      
286400           ELSE                                                           
286500             IF XDCA-KDORDBEK = 15                                        
286600                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
286700                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
286800                END-IF                                                    
286900                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
287000                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
287100                END-IF                                                    
287200                IF SDCA-KDORDBEK = 15                                     
287300                   MOVE ZERO         TO SDCA-KDORDBEK                     
287400                END-IF                                                    
287500             END-IF                                                       
287600           END-IF                                                         
287700           MOVE JA                   TO OBKR-SW                           
287800         ELSE                                                             
287900           IF KOLLA-ERS    OR                                             
288000             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
288100              MOVE NEJ               TO KOLLA-ERS-SW                      
288200              MOVE ZERO              TO KERS-KDORDBEK                     
288300              PERFORM S02-RENSA-TILLK-TAB                                 
288400              MOVE ZERO              TO SPAR-KDORDBEK                     
288500           ELSE                                                           
288510             IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                 
288520                MOVE ZERO            TO SPAR-KDORDBEK                     
288530             END-IF                                                       
288550           END-IF                                                         
288600           IF KVAN-KDORDBEK-UT = ZERO                                     
288700             MOVE JA                 TO EGET-CL-RAD-SW                    
288800           END-IF                                                         
288900         END-IF                                                           
289000         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
289100         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
289200         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
289300         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
289400         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
289500         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
289600         MOVE XDCA-KDOI              TO ORAD-KDOI                         
289700         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
289800         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
289900         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
290000         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
290100         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
290200         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
290300         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
290400         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
290500         MOVE NEJ                    TO ALLT-SW                           
290600       END-IF                                                             
290700                                                                          
290800     END-IF                                                               
290900     .                                                                    
291000     EJECT                                                                
291100 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
291200                                                                          
291300     IF ALLT-OK OR KOLLA-ERS                                              
291400                                                                          
291500       PERFORM S40-HAMTA-WDB6-INFO                                        
291600                                                                          
291700       IF DCS-NDC                                                         
291800                                                                          
291900* XDCA-INPUT                                                              
292000         MOVE +1 TO WS-INDEX                                              
292100         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
292200           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
292300                             TO XDCA-IDDC-CLEAR-IN(WS-INDEX)              
292400           ADD +1 TO WS-INDEX                                             
292500         END-PERFORM                                                      
292600                                                                          
292700         MOVE OHUV-IDDC-PRIM       TO XDCA-IDDC-TVS                       
292800         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
292900         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
293000         MOVE ORAD-IDDC            TO XDCA-IDDC                           
293100         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
293200         MOVE AREG-KDERS           TO XDCA-KDERS                          
293300         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
293400         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
293500         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
293600         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
293700         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
293800         MOVE ORAD-VKART           TO XDCA-VKART                          
293900* RUNNING XDCA IN ACTIVE CALL                                             
294000         MOVE +1                   TO XDCA-KDCALL                         
294100                                                                          
294200* XDCA-OUTPUT                                                             
294300         MOVE SPACE                TO XDCA-IDDC-OUT                       
294400                                      XDCA-IDDC-RO                        
294500                                      XDCA-KDARTURS                       
294600                                      XDCA-KDOI                           
294700                                      XDCA-CLEARGROUP                     
294800         MOVE ZERO                 TO XDCA-ADLAGOMR                       
294900                                      XDCA-ADGANG                         
295000                                      XDCA-ADPLATS                        
295100                                      XDCA-KDORDBEK                       
295200                                      XDCA-KVPREAVB                       
295300                                      XDCA-KVPRERO                        
295400                                      XDCA-TIREGDAT-OUT                   
295500                                      XDCA-TIREGTID-OUT                   
295600                                      XDCA-VKART-OUT                      
295700                                      XDCA-VKART-NTO                      
295800                                      XDCA-VLARTNTO                       
296000         MOVE ZERO                 TO                                     
296100                                      XDCA-KVOKS-DAG                      
296200                                      XDCA-KVOKS-BULK                     
296300                                                                          
296310                                                                          
296311         IF XDCA-DAPUBL NOT = 99999999                                    
296312            MOVE ZERO              TO XDCA-DAPUBL                         
296313         END-IF                                                           
296320                                                                          
296330                                                                          
296400         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
296500         XDCA-USEA-PCB                                                    
296600         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
296700         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
296800         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
296900         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
297000         XDCA-WDK7-3-PCB                                                  
297100                                                                          
297200* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
297300* FROM XDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
297400         IF XDCA-KDARTURS = SPACE                                         
297500           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
297600         END-IF                                                           
297700         IF XDCA-VKART-NTO = ZERO                                         
297800           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
297900         END-IF                                                           
298000         IF XDCA-VLARTNTO = ZERO                                          
298100           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
298200         END-IF                                                           
298300       END-IF                                                             
298400     END-IF                                                               
298500     .                                                                    
298600     EJECT                                                                
298700 ECGX-CHECK-DIFF SECTION.                                                 
298800                                                                          
298900     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
299000     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
299100     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
299200     AND NDCA-ADGANG     = XDCA-ADGANG                                    
299300     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
299400     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
299500     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
299600     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
299700     AND NDCA-KDOI       = XDCA-KDOI                                      
299800     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
299900     AND NDCA-VKART      = XDCA-VKART-OUT                                 
300000     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
300100     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
300200     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
300300     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
300400     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
300500     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
300600         MOVE NEJ TO DIFF-FLSVAR                                          
300700     ELSE                                                                 
300800        MOVE JA           TO DIFF-FLSVAR                                  
300900     END-IF                                                               
301000                                                                          
301100* ORDER LOG INFO                                                          
301200     IF DIFF-FLSVAR = JA                                                  
301300       MOVE IDPGM         TO FIL-IDPGM                                    
301400       ACCEPT FIL-TIREGDAT FROM DATE                                      
301500       ACCEPT FIL-TIKLOCK FROM TIME                                       
301600       MOVE 1             TO FIL-IDSEKVNR                                 
301700       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
301800       MOVE 'A'           TO FIL-CT-IDVTYP                                
301900       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
302000                                                                          
302100*   ORDER LINE INFO                                                       
302200       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
302300       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
302400       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
302500       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
302600       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
302700       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
302800       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
302900       MOVE '4255'         TO DIFF-IDSYSTEM                               
303000                                                                          
303100*   NDCA INFO                                                             
303200       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
303300       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
303400       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
303500       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
303600       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
303700       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
303800       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
303900       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
304000       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
304100       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
304200       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
304300       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
304400       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
304500       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
304600       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
304700       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
304800                                                                          
304900*   XDCA INFO                                                             
305000       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
305100       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
305200       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
305300       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
305400       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
305500       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
305600       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
305700       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
305800       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
305900       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
306000       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
306100       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
306200       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
306300       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
306400       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
306500       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
306600                                                                          
306700       PERFORM IMS-ISRT-WDR601                                            
306800       IF SEGMENT-FINNS-REDAN                                             
306900          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
307000             ADD 1 TO FIL-IDSEKVNR                                        
307100             PERFORM IMS-ISRT-WDR601                                      
307200          END-PERFORM                                                     
307300       END-IF                                                             
307400     END-IF                                                               
307500     .                                                                    
307600     EJECT                                                                
307700 ECH-PREL-AVBOKNING-SDC SECTION.                                          
307800                                                                          
307900     IF (ALLT-OK OR KOLLA-ERS)                                            
308000     AND OHUV-FLOVRLEV = NEJ                                              
308100     AND OHUV-FLORDSPE = NEJ                                              
308200     AND ORAD-IDBIL = SPACE                                               
308300                                                                          
308400*LDC                                                                      
308500       PERFORM S40-HAMTA-WDB6-INFO                                        
308600       IF DCS-SDC                                                         
308700       AND (MID-IDSYSTEM = 'LDCC' OR 'LDCS' OR 'LYNC' OR 'LYNS'           
308800                                            OR 'ECOC' OR 'ECOS'           
308900                                            OR 'VOUC' OR 'VOUS'           
309000                                            OR 'TADC' OR 'TADS'           
309100                                            OR 'ACCC' OR 'ACCS'           
309200                                            OR 'APAC' OR 'APAS'           
309300                                            OR 'APBC' OR 'APBS'           
309400                                            OR 'APCC' OR 'APCS'           
309500                                            OR 'APDC' OR 'APDS'           
309600                                            OR 'APEC' OR 'APES'           
309700                                            OR 'APFC' OR 'APFS'           
309800                                            OR 'APGC' OR 'APGS'           
309900                                            OR 'APHC' OR 'APHS'           
310000                                            OR 'APIC' OR 'APIS'           
310100                                            OR 'APJC' OR 'APJS')          
310200                                                                          
310300                                                                          
310400         MOVE NEJ                  TO SDCA-FLORDSPE                       
310500         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
310600         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
310700         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
310800         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
310900         MOVE ORAD-IDDC            TO SDCA-IDDC                           
311000         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
311100         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
311200         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
311300         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
311400         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
311500         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
311600         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
311700         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
311800         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
311900         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
312000         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
312100         MOVE OHUV-TIREPDAT        TO SDCA-TIREPDAT                       
312200         MOVE ZERO                 TO SDCA-KVOKS-PREL                     
312300         MOVE +1                   TO SDCA-KDCALL                         
312400         MOVE +1                   TO SDCA-IXDCCLEAR                      
312500                                                                          
312600         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
312700                                           SDCA-WDB6-PCB                  
312800                                           SDCA-WDK9-PCB                  
312900                                           SDCA-WDR6-PCB                  
313000                                           SDCA-WDK6-PCB                  
313100                                           SDCA-WDQ4B-PCB                 
313200                                           SDCA-WDQ2-PCB                  
313300                                           SDCA-WDQ4-PCB                  
313400                                           SDCA-WDB6-2-PCB                
313500                                           SDCA-WDK6-2-PCB                
313600                                           SDCA-WDK7-2-PCB                
313700                                           SDCA-WDK7-3-PCB                
313800                                                                          
313900         MOVE SDCA-KVOKS-PREL         TO ORAD-KVOKS-PREL                  
314000         IF SDCA-KDORDBEK > ZERO                                          
314100           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
314200              MOVE JA        TO OBKR-SW                                   
314300              MOVE NEJ       TO ALLT-SW                                   
314400           ELSE                                                           
314500             IF KOLLA-ERS                                                 
314600                MOVE NEJ              TO KOLLA-ERS-SW                     
314700                MOVE ZERO             TO SDCA-KDORDBEK                    
314800                IF SPAR-KDORDBEK = ZERO                                   
314900                   MOVE WC-CDC-SE    TO ORAD-IDDC                         
315000                   MOVE ORAD-IDDC    TO WS-IDDC                           
315100                END-IF                                                    
315200             ELSE                                                         
315300               IF SDCA-KDORDBEK = 15                                      
315400                  MOVE WC-CDC-SE   TO ORAD-IDDC                           
315500                  MOVE ORAD-IDDC   TO WS-IDDC                             
315600                  MOVE JA          TO OBKR-SW                             
315700               ELSE                                                       
315800                  MOVE JA          TO OBKR-SW                             
315900                  MOVE NEJ         TO ALLT-SW                             
316000               END-IF                                                     
316100             END-IF                                                       
316200           END-IF                                                         
316300         ELSE                                                             
316400           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
316500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
316600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
316700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
316800           MOVE NEJ                  TO ALLT-SW                           
316900           IF KOLLA-ERS  OR                                               
317000             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
317100              MOVE NEJ               TO KOLLA-ERS-SW                      
317200              MOVE ZERO              TO KERS-KDORDBEK                     
317300              PERFORM S02-RENSA-TILLK-TAB                                 
317400              MOVE ZERO              TO SPAR-KDORDBEK                     
317500           END-IF                                                         
317600         END-IF                                                           
317700*********MOVE SDCA-KDOI              TO ORAD-KDOI                         
317800       END-IF                                                             
317900                                                                          
318000     END-IF                                                               
318100     .                                                                    
318200     EJECT                                                                
318300 ECQ-KOMPLETTERA-RANSONERING SECTION.                                     
318400                                                                          
318500     IF ALLT-OK                                                           
318600                                                                          
318700       MOVE ORAD-BERADREF      TO RANS-BERADREF                           
318800       MOVE OHUV-FLFORBI       TO RANS-FLFORBI                            
318900       MOVE OHUV-FLEMBORD      TO RANS-FLEMBORD                           
319000       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
319100       MOVE OHUV-FLOVRLEV      TO RANS-FLOVRLEV                           
319200       MOVE ORAD-IDKAMPRF      TO RANS-IDKAMPRF                           
319300       MOVE ORAD-IDARTNR       TO RANS-IDARTNR                            
319400       MOVE ORAD-IDLEVNR       TO RANS-IDLEVNR                            
319500       MOVE OHUV-IDRFTAB       TO RANS-IDRFTAB                            
319600       MOVE ORAD-TIRODAT       TO RANS-TIRODAT                            
319700       MOVE OHUV-KDORDKL       TO RANS-KDORDKL                            
319800       MOVE +1                 TO RANS-KDORDBEH                           
319900       MOVE ORAD-KVBEART-Q     TO RANS-KVBEART-Q                          
320000       MOVE ORAD-KDTPOTYP      TO RANS-KDTPOTYP                           
320100       MOVE AREG-KDERS         TO RANS-KDERS                              
320200       MOVE AREG-KVLS          TO RANS-KVLS                               
320300       MOVE AREG-KVPB-SATS     TO RANS-KVPB-SATS                          
320400       MOVE AREG-KVPB-SEP      TO RANS-KVPB-SEP                           
320500       MOVE AREG-REDIRLEV      TO RANS-REDIRLEV                           
320600       MOVE AREG-KVRESS        TO RANS-KVRESS                             
320700       MOVE AREG-KVSPANT       TO RANS-KVSPANT                            
320800       MOVE AREG-KVUTRS        TO RANS-KVUTRS                             
320900       MOVE AREG-TIDISPIN      TO RANS-TIDISPIN                           
321000                                                                          
321100       MOVE AREG-KDPRODSL      TO TEST-KDPRODSL                           
321200       IF KDPRODSL-BIMA                                                   
321300         MOVE 1                TO ORAD-RERF-RAD                           
321400                                  RANS-RERF-RAD-UT                        
321500         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
321600                                  RANS-SUTPO-EJPB-UT                      
321700                                  RANS-RERF-ART-UT                        
321800       ELSE                                                               
321900         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
322000                             RANS-ARTM-PCB RANS-ARTS-PCB                  
322100                                                                          
322200         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
322300       END-IF                                                             
322400     END-IF                                                               
322500     .                                                                    
322600     EJECT                                                                
322700                                                                          
322800 ECR-KOMPLETTERA-STORA-UTTAG SECTION.                                     
322900                                                                          
323000     IF ALLT-OK                                                           
323100                                                                          
323200       IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU'           
323300                                     OR 'TAD' OR 'ACC' OR 'APA'           
323400                                     OR 'APB' OR 'APC' OR 'APD'           
323500                                     OR 'APE' OR 'APF' OR 'APG'           
323600                                     OR 'APH' OR 'API' OR 'APJ')          
323700         CONTINUE                                                         
323800       ELSE                                                               
323900         MOVE ORAD-IDSYSTEM    TO STOR-IDSYSTEM                           
324000         MOVE ORAD-IDLEVNR     TO STOR-IDLEVNR                            
324100         MOVE ORAD-IDKUNDRF-RO TO STOR-IDKUNDRF-RO                        
324200         MOVE ORAD-BERADREF    TO STOR-BERADREF                           
324300         MOVE OHUV-FLFORBI     TO STOR-FLFORBI                            
324400         MOVE OHUV-KDORDKL     TO STOR-KDORDKL                            
324500         MOVE SPACE            TO STOR-KDPROTYP                           
324600         MOVE AREG-KDERS       TO STOR-KDERS                              
324700         MOVE AREG-KDVVKL      TO STOR-KDVVKL                             
324800         MOVE ORAD-KVBEART-Q   TO STOR-KVBEART-Q                          
324900         MOVE AREG-KVPB-SEP    TO STOR-KVPB-SEP                           
325000         MOVE AREG-KVSLUTKP    TO STOR-KVSLUTKP                           
325100         MOVE RANS-RERF-ART-UT TO STOR-RERF-ART                           
325200         MOVE OHUV-FLORDSPE    TO STOR-FLORDSPE                           
325300         MOVE OHUV-FLOVRLEV    TO STOR-FLOVRLEV                           
325400         MOVE OHUV-IDKAMPRF    TO STOR-IDKAMPRF                           
325500         MOVE ORAD-IDDISTR     TO STOR-IDDISTR                            
325600         MOVE AREG-KDPRODSL    TO STOR-KDPRODSL                           
325700                                                                          
325800         CALL W411STOR USING STOR-W411STOR                                
325900                                                                          
326000         IF STOR-KDORDBEK > +0                                            
326100            MOVE +6               TO ORAD-KDTPOTYP                        
326200            PERFORM S21-UPPDATERA-TPO6                                    
326300            MOVE JA               TO OBKR-SW                              
326400            MOVE NEJ              TO ALLT-SW                              
326500         END-IF                                                           
326600       END-IF                                                             
326700                                                                          
326800     END-IF                                                               
326900     .                                                                    
327000     EJECT                                                                
327100                                                                          
327200                                                                          
327300 ECD-PREL-AVBOKNING SECTION.                                              
327400                                                                          
327500     IF ALLT-OK                                                           
327600                                                                          
327700       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
327800                                 TO CDCA-FLFINLV-IN                       
327900       MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                       
328000       MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                      
328100       MOVE ORFK-FLSLATT(WS-INDEX-MID)                                    
328200                                 TO CDCA-FLSLATT-IN                       
328300       MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                       
328400       MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                       
328500       MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
328600       MOVE ORAD-IDDC            TO CDCA-IDDC-IN                          
328700       MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                      
328800       MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                   
328900       MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
329000       MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
329100       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
329200       MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
329300       MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                       
329400       MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
329500       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
329600       MOVE SPACE                TO CDCA-KDPROTYP-IN                      
329700       MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
329800       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
329900       MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                       
330000       MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
330100       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
330200       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
330300       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
330400       MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                      
330500       MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                      
330600       MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                       
330700       MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                      
330800       MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                      
330900       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
331000       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
331100       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
331200       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
331300       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
331400       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
331500       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
331600       MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
331700       MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                      
331800       MOVE +1                   TO CDCA-KDCALL                           
331900                                                                          
332000       CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                    
332100                                         CDCA-INLB-PCB                    
332200                                         CDCA-WDB2-PCB                    
332300                                         CDCA-WDC1-PCB                    
332400                                                                          
332500       IF KERS-KDERS > 0 AND < 10                                         
332600          IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0               
332700             MOVE JA               TO OBKR-SW                             
332800          ELSE                                                            
332900             PERFORM S02-RENSA-TILLK-TAB                                  
333000             MOVE ZERO             TO KERS-KDORDBEK                       
333100             MOVE NEJ              TO TILLK-SW                            
333200          END-IF                                                          
333300       END-IF                                                             
333400************                                                              
333500*OM DET ÄR EN TILLÄGGSTPO SÅ SKALL INGEN RAD SKRIVAS. TL 050127           
333600*DÄRFÖR GER JAG INTE NÅGRA VÄRDEN TILL ORAD-PREAVB OCH ORAD-KVPRER        
333700       MOVE CDCA-FLAKPLOC-UT         TO ORAD-FLAKPLOC                     
333800                                                                          
333900       IF ORAD-IDLEVNR NOT = SPACE                                        
334000          CONTINUE                                                        
334100       ELSE                                                               
334200          MOVE CDCA-KVBEART-UT          TO ORAD-KVBEART                   
334300          MOVE CDCA-KVBEART-Q-UT        TO ORAD-KVBEART-Q                 
334400       END-IF                                                             
334500       MOVE CDCA-KVPREAVB-UT         TO ORAD-KVPREAVB                     
334600       MOVE CDCA-KVPRERO-UT          TO ORAD-KVPRERO                      
334700       MOVE CDCA-KVSLATT-UT          TO ORAD-KVSLATT                      
334800       MOVE CDCA-RERF-RAD-UT         TO ORAD-RERF-RAD                     
334900                                                                          
335000       IF CDCA-KDORDBEK-UT > ZERO                                         
335100          MOVE JA                    TO OBKR-SW                           
335200       END-IF                                                             
335300     END-IF                                                               
335400     .                                                                    
335500     EJECT                                                                
335600 ECS-SKRIV-OBKR-OCH-VOR-RAD SECTION.                                      
335700                                                                          
335800*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
335900*    DEN SISTA ORDERBEKRÄFTELSERADEN 'SLÄPAR' ISRT AV RADEN               
336000*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
336100*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
336200*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
336300*---                                                                      
336400     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
336500                                                                          
336600     IF TILLKOMMANDE-RAD                                                  
336700        IF KERS-KDORDBEK = 41                                             
336800           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
336900           MOVE '4255KER1'        TO OBKR-IDPGM                           
337000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
337100                                  TO OBKR-KVBEART-TILLK                   
337110           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
337120              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
337130             MOVE +0              TO OBKR-DIERS-KVOT                      
337140           ELSE                                                           
337200             COMPUTE OBKR-DIERS-KVOT =                                    
337300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
337400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
337410           END-IF                                                         
337500           MOVE 'S'               TO OBKR-SW                              
337600        END-IF                                                            
337700     END-IF                                                               
337800                                                                          
337900     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
338000       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
338100*----(KOD 58, 59, 98)                                                     
338200        IF OBKR-SKRIVEN                                                   
338300           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
338400           ADD +1              TO OBKR-IDSEKVNR                           
338500        END-IF                                                            
338600        IF ORFK-KDORDBEK(WS-INDEX-MID) = 98                               
338700          MOVE 82              TO OBKR-KDORDBEK                           
338800          MOVE IDPGM           TO OBKR-IDPGM                              
338900        ELSE                                                              
339000          MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                
339100                               TO OBKR-KDORDBEK                           
339200          MOVE '4255ORFK'      TO OBKR-IDPGM                              
339300        END-IF                                                            
339400        MOVE 'S'               TO OBKR-SW                                 
339500     END-IF                                                               
339600                                                                          
339700     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 92 OR 26                     
339800*----(KOD 21, 53, 82, 92) , 26                                            
339900        IF OBKR-SKRIVEN                                                   
340000           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
340100           ADD +1              TO OBKR-IDSEKVNR                           
340200        END-IF                                                            
340300        IF TILLKOMMANDE-RAD                                               
340400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
340500                               TO OBKR-KVBEART-TILLK                      
340600           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
340700              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
340800             MOVE +0              TO OBKR-DIERS-KVOT                      
340810           ELSE                                                           
340820             COMPUTE OBKR-DIERS-KVOT =                                    
340830                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
340840                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
340850           END-IF                                                         
340900        END-IF                                                            
341000        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
341100        MOVE '4255DLEV'        TO OBKR-IDPGM                              
341200        MOVE 'S'               TO OBKR-SW                                 
341300     END-IF                                                               
341400                                                                          
341500     IF SPAR-KDORDBEK > +0                                                
341600*----(KOD 51, 52, 54, 55, 57, 58, 66, 67, 80, 90)                         
341700        IF OBKR-SKRIVEN                                                   
341800           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
341900           ADD +1              TO OBKR-IDSEKVNR                           
342000        END-IF                                                            
342100        IF TILLKOMMANDE-RAD                                               
342200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
342300                               TO OBKR-KVBEART-TILLK                      
342400           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
342500              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
342600             MOVE +0              TO OBKR-DIERS-KVOT                      
342610           ELSE                                                           
342620             COMPUTE OBKR-DIERS-KVOT =                                    
342630                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
342640                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
342650           END-IF                                                         
342700        END-IF                                                            
342800        MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                           
342900        MOVE '4255SPAR'        TO OBKR-IDPGM                              
343000        MOVE 'S'               TO OBKR-SW                                 
343100     END-IF                                                               
343200                                                                          
343300     IF TPO1-KDORDBEK > +0                                                
343400*----(KOD 72, 73, 74, 85)                                                 
343500        IF OBKR-SKRIVEN                                                   
343600           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
343700           ADD +1              TO OBKR-IDSEKVNR                           
343800        END-IF                                                            
343900        IF TILLKOMMANDE-RAD                                               
344000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
344100                               TO OBKR-KVBEART-TILLK                      
344200           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
344300              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
344400             MOVE +0              TO OBKR-DIERS-KVOT                      
344410           ELSE                                                           
344420             COMPUTE OBKR-DIERS-KVOT =                                    
344430                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
344440                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
344450           END-IF                                                         
344500        END-IF                                                            
344600        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
344700        MOVE '4255TPO1'        TO OBKR-IDPGM                              
344800        IF TPO1-KDORDBEK = 85                                             
344900           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
345000        END-IF                                                            
345100        MOVE 'S'               TO OBKR-SW                                 
345200     END-IF                                                               
345300                                                                          
345400     IF TPO3-KDORDBEK > +0                                                
345500*----(KOD 72)                                                             
345600        IF OBKR-SKRIVEN                                                   
345700           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
345800           ADD +1              TO OBKR-IDSEKVNR                           
345900        END-IF                                                            
346000        IF TILLKOMMANDE-RAD                                               
346100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
346200                               TO OBKR-KVBEART-TILLK                      
346300           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
346400              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
346500             MOVE +0              TO OBKR-DIERS-KVOT                      
346510           ELSE                                                           
346520             COMPUTE OBKR-DIERS-KVOT =                                    
346530                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
346540                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
346550           END-IF                                                         
346600        END-IF                                                            
346700        MOVE TPO3-KDORDBEK     TO OBKR-KDORDBEK                           
346800        MOVE '4255TPO3'        TO OBKR-IDPGM                              
346900        MOVE 'S'               TO OBKR-SW                                 
347000     END-IF                                                               
347100                                                                          
347200     IF KAMP-KDORDBEK > +0                                                
347300*----(KOD 72, 75, 76)                                                     
347400        IF OBKR-SKRIVEN                                                   
347500           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
347600           ADD +1              TO OBKR-IDSEKVNR                           
347700        END-IF                                                            
347800        IF TILLKOMMANDE-RAD                                               
347900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
348000                               TO OBKR-KVBEART-TILLK                      
348100           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
348200              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
348300             MOVE +0              TO OBKR-DIERS-KVOT                      
348310           ELSE                                                           
348320             COMPUTE OBKR-DIERS-KVOT =                                    
348330                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
348340                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
348350           END-IF                                                         
348400        END-IF                                                            
348500        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
348600        MOVE '4255KAMP'        TO OBKR-IDPGM                              
348700        MOVE 'S'               TO OBKR-SW                                 
348800     END-IF                                                               
348900                                                                          
349000     IF RELS-KDORDBEK > 0                                                 
349100*----(KOD 56)                                                             
349200          IF OBKR-SKRIVEN                                                 
349300             PERFORM IMS-25-ISRT-WLORQM01-WDQ101                          
349400             ADD +1              TO OBKR-IDSEKVNR                         
349500          END-IF                                                          
349600          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
349700          MOVE '4255ORFK'        TO OBKR-IDPGM                            
349800          MOVE 'S'               TO OBKR-SW                               
349900       END-IF                                                             
350000    EJECT                                                                 
350100     IF TPO2-KDORDBEK > +0                                                
350200*----(KOD 70)                                                             
350300        IF OBKR-SKRIVEN                                                   
350400           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
350500           ADD +1              TO OBKR-IDSEKVNR                           
350600        END-IF                                                            
350700        IF TILLKOMMANDE-RAD                                               
350800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
350900                               TO OBKR-KVBEART-TILLK                      
351000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
351100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
351200             MOVE +0              TO OBKR-DIERS-KVOT                      
351210           ELSE                                                           
351220             COMPUTE OBKR-DIERS-KVOT =                                    
351230                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
351240                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
351250           END-IF                                                         
351300        END-IF                                                            
351400        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
351500        MOVE '4255TPO2'        TO OBKR-IDPGM                              
351600        MOVE 'S'               TO OBKR-SW                                 
351700     END-IF                                                               
351800                                                                          
351900     IF STOR-KDORDBEK > +0                                                
352000*----(KOD 70)                                                             
352100        IF OBKR-SKRIVEN                                                   
352200           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
352300           ADD +1              TO OBKR-IDSEKVNR                           
352400        END-IF                                                            
352500        IF TILLKOMMANDE-RAD                                               
352600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
352700                               TO OBKR-KVBEART-TILLK                      
352800           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
352900              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
353000             MOVE +0              TO OBKR-DIERS-KVOT                      
353010           ELSE                                                           
353020             COMPUTE OBKR-DIERS-KVOT =                                    
353030                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
353040                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
353050           END-IF                                                         
353100        END-IF                                                            
353200        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
353300        MOVE '4255STOR'        TO OBKR-IDPGM                              
353400        MOVE 'S'               TO OBKR-SW                                 
353500     END-IF                                                               
353600                                                                          
353700     IF KVAN-KDORDBEK-UT > +0                                             
353800*----(KOD 43, 44)                                                         
353900        IF OBKR-SKRIVEN                                                   
354000           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
354100           ADD +1              TO OBKR-IDSEKVNR                           
354200        END-IF                                                            
354300        IF TILLKOMMANDE-RAD                                               
354400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
354500                               TO OBKR-KVBEART-TILLK                      
354600           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
354700              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
354800             MOVE +0              TO OBKR-DIERS-KVOT                      
354810           ELSE                                                           
354820             COMPUTE OBKR-DIERS-KVOT =                                    
354830                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
354840                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
354850           END-IF                                                         
354900        END-IF                                                            
355000        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
355100        MOVE '4255KVAN'        TO OBKR-IDPGM                              
355200        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
355300        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
355400        MOVE 'S'               TO OBKR-SW                                 
355500     END-IF                                                               
355600                                                                          
355700     IF KERS-KDORDBEK > +0                                                
355800*----(KOD 41, 61)                                                         
355900                                                                          
356000        IF KERS-KDORDBEK = 61                                             
356100*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
356200*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
356300*----RADBEHANDLINGEN                                                      
356400           IF OBKR-SKRIVEN                                                
356500              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
356600              ADD +1           TO OBKR-IDSEKVNR                           
356700           END-IF                                                         
356800           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
356900           MOVE '4255KER2'     TO OBKR-IDPGM                              
357000           MOVE 'S'            TO OBKR-SW                                 
357100           PERFORM S20-OBKR-FRAN-TILLK-TAB                                
357200           PERFORM S02-RENSA-TILLK-TAB                                    
357300        ELSE                                                              
357400           IF NOT TILLKOMMANDE-RAD                                        
357500              IF OBKR-SKRIVEN                                             
357600                 PERFORM IMS-25-ISRT-WLORQM01-WDQ101                      
357700                 ADD +1        TO OBKR-IDSEKVNR                           
357800              END-IF                                                      
357900              MOVE 'S'            TO OBKR-SW                              
358000              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
358100              MOVE '4255KER3'     TO OBKR-IDPGM                           
358200           END-IF                                                         
358300        END-IF                                                            
358400     END-IF                                                               
358500                                                                          
358600     IF XDCA-KDORDBEK > ZERO                                              
358700*----(KOD 15, 53, 55, 80, 92, 99)                                         
358800        IF OBKR-SKRIVEN                                                   
358900           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
359000           ADD +1              TO OBKR-IDSEKVNR                           
359100        END-IF                                                            
359200        IF TILLKOMMANDE-RAD                                               
359300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
359400                               TO OBKR-KVBEART-TILLK                      
359500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
359600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
359700             MOVE +0              TO OBKR-DIERS-KVOT                      
359710           ELSE                                                           
359720             COMPUTE OBKR-DIERS-KVOT =                                    
359730                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
359740                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
359750           END-IF                                                         
359800        END-IF                                                            
359900                                                                          
360000        IF XDCA-KDORDBEK NOT = 15                                         
360100          IF OHUV-IDDC-TVS = SPACE                                        
360200            IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                     
360300              MOVE 15          TO OBKR-KDORDBEK                           
360400              MOVE IDPGM       TO OBKR-IDPGM                              
360500              MOVE 'S'         TO OBKR-SW                                 
360600                                                                          
360700* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
360800              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
360900                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
361000              END-IF                                                      
361100              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
361200                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
361300              END-IF                                                      
361400              IF SDCA-KDORDBEK = 15                                       
361500                 MOVE ZERO     TO SDCA-KDORDBEK                           
361600              END-IF                                                      
361700            END-IF                                                        
361800            IF OBKR-SKRIVEN                                               
361900              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
362000              ADD +1           TO OBKR-IDSEKVNR                           
362100            END-IF                                                        
362200          END-IF                                                          
362300        END-IF                                                            
362400        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
362500        MOVE '4255XDCA'        TO OBKR-IDPGM                              
362600        MOVE 'S'               TO OBKR-SW                                 
362700     END-IF                                                               
362800     EJECT                                                                
362900                                                                          
363000     IF SDCA-KDORDBEK > ZERO                                              
363100*----(KOD 15, 53, 80, 92)                                                 
363200        IF OBKR-SKRIVEN                                                   
363300           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
363400           ADD +1              TO OBKR-IDSEKVNR                           
363500        END-IF                                                            
363600        IF TILLKOMMANDE-RAD                                               
363700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
363800                               TO OBKR-KVBEART-TILLK                      
363900           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
364000              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
364100             MOVE +0              TO OBKR-DIERS-KVOT                      
364110           ELSE                                                           
364120             COMPUTE OBKR-DIERS-KVOT =                                    
364130                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
364140                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
364150           END-IF                                                         
364200        END-IF                                                            
364300        IF SDCA-KDORDBEK = 80                                             
364400           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
364500        END-IF                                                            
364600        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
364700        MOVE '4255SDCA'        TO OBKR-IDPGM                              
364800        MOVE 'S'               TO OBKR-SW                                 
364900     END-IF                                                               
365000     EJECT                                                                
365100                                                                          
365200                                                                          
365300     IF CDCA-KDORDBEK-UT > +0                                             
365400*----(KOD 80, 92, 99)                                                     
365500        IF OBKR-SKRIVEN                                                   
365600           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
365700           ADD +1              TO OBKR-IDSEKVNR                           
365800        END-IF                                                            
365900        IF TILLKOMMANDE-RAD                                               
366000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
366100                               TO OBKR-KVBEART-TILLK                      
366200           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
366300              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
366400             MOVE +0              TO OBKR-DIERS-KVOT                      
366410           ELSE                                                           
366420             COMPUTE OBKR-DIERS-KVOT =                                    
366430                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
366440                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
366450           END-IF                                                         
366500        END-IF                                                            
366600        IF CDCA-KDORDBEK-UT = +80                                         
366700           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
366800        END-IF                                                            
366900        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
367000        MOVE '4255CDCA'        TO OBKR-IDPGM                              
367100        MOVE 'S'               TO OBKR-SW                                 
367200     END-IF                                                               
367300                                                                          
367400*                                                                         
367500* PÅ SISTA RADEN FÖR KUNDENS NORMALA CL LÄGGS DE AVBOKADE ANTALEN!        
367600     IF OBKR-SKRIVEN                                                      
367700        MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                           
367800        MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                            
367900******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
368000        IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                     
368100           PERFORM S05-DELETE-PRICE-Q-LINE                                
368200           INITIALIZE OBKR-DEAL-PR-LINE                                   
368300        END-IF                                                            
368400*************TL 030514                                                    
368500        PERFORM IMS-25-ISRT-WLORQM01-WDQ101                               
368600        ADD +1                 TO OBKR-IDSEKVNR                           
368700     END-IF                                                               
368800                                                                          
368900*LK* ADDITION NOT ALLOWED CONFIRMATION * NO ADDLINE-NOAD                  
369000     IF ADDLINE-NOTOK                                                     
369100        MOVE 82                TO OBKR-KDORDBEK                           
369200        MOVE '4255NOAD'        TO OBKR-IDPGM                              
369300        PERFORM IMS-25-ISRT-WLORQM01-WDQ101                               
369400     END-IF                                                               
369500                                                                          
369600     .                                                                    
369700     EJECT                                                                
369800 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
369900                                                                          
370000     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
370100     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
370200                               TO OBKR-IDARTNR                            
370300     IF NOT TILLKOMMANDE-RAD                                              
370400        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
370500                                  W-IDORDER-Q1-MAX                        
370600        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
370700                                  W-IDARTNR-Q1-MAX                        
370800        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
370900                                  W-IDLOPNR-Q1-MAX                        
371000                                  W-IDSEKVNR-Q1-MIN                       
371100                                  W-IDSEKVNR-Q1-MAX                       
371200        PERFORM IMS-24-GU-ORQM-WDQ101                                     
371300        PERFORM UNTIL SEGMENT-SAKNAS                                      
371400           ADD +1              TO W-IDLOPNR-Q1-MIN                        
371500                                  W-IDLOPNR-Q1-MAX                        
371600           PERFORM IMS-24-GU-ORQM-WDQ101                                  
371700        END-PERFORM                                                       
371800        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
371900        MOVE +1                TO OBKR-IDSEKVNR                           
372000     END-IF                                                               
372100                                                                          
372200     MOVE ORAD-IDDC            TO OBKR-IDDC                               
372300     IF WS-IDDC-DDGS NOT = SPACE                                          
372400       MOVE WS-IDDC-DDGS       TO OBKR-IDDC                               
372500     END-IF                                                               
372600     MOVE +0                   TO OBKR-KDORDBEK                           
372700     MOVE SPACE                TO OBKR-BEERS                              
372800     MOVE SPACE                TO OBKR-IDBIL                              
372900     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
373000     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
373100     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
373200     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
373300     MOVE +0                   TO OBKR-DIERS-KVOT                         
373400     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
373500     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
373600     MOVE JA                   TO OBKR-FLOBOK                             
373700     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
373800     MOVE NEJ                  TO OBKR-FLOBPRT                            
373900     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
374000     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
374100     MOVE ORFK-FLSLATT (WS-INDEX-MID)                                     
374200                               TO OBKR-FLSLATT                            
374300     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
374400     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
374500        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
374600     ELSE                                                                 
374700        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
374800                               TO OBKR-REKSIFFR                           
374900     END-IF                                                               
375000     IF TILLKOMMANDE-RAD                                                  
375100        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
375200                               TO OBKR-IDARTNR-TILLK                      
375300        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
375400                               TO OBKR-REKSIFFR-TILLK                     
375500     ELSE                                                                 
375600        MOVE +0                TO OBKR-IDARTNR-TILLK                      
375700        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
375800     END-IF                                                               
375900     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
376000     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
376100     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
376200     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
376300     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
376400     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
376500     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
376600     IF NOT TILLKOMMANDE-RAD                                              
376700        MOVE AREG-KDERS        TO OBKR-KDERS                              
376800     END-IF                                                               
376900     MOVE ORAD-KDOI            TO OBKR-KDOI                               
377000     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
377100     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
377200     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
377300     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
377400     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
377500     MOVE +0                   TO OBKR-KVANNANT                           
377600     MOVE +0                   TO OBKR-KVAVBART                           
377700     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
377800     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
377900     MOVE +0                   TO OBKR-KVBEART-TILLK                      
378000     MOVE +0                   TO OBKR-KVPREAVB                           
378100     MOVE +0                   TO OBKR-KVPRERO                            
378200     IF ALLT-OK AND ADDLINE-OK                                            
378300        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
378400     ELSE                                                                 
378500        MOVE +0                TO OBKR-KVQPACK                            
378600     END-IF                                                               
378700     MOVE +0                   TO OBKR-KVRO                               
378800     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
378900     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
379000     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
379100     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
379200     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
379300     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
379400     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
379500     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
379600     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
379700     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
379800     MOVE +0                   TO OBKR-TIRODAT                            
379900     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
380000                                                                          
380100     IF WS-AAMMDD-9KOMPL < 500000                                         
380200       MOVE 20                 TO WS-SEKEL-9KOMPL                         
380300     ELSE                                                                 
380400       MOVE 19                 TO WS-SEKEL-9KOMPL                         
380500     END-IF                                                               
380600                                                                          
380700     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
380800                                                                          
380900     MOVE ORAD-TITPO           TO OBKR-TITPO                              
381000     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
381100                                                                          
381200     IF WS-AAMMDD-9KOMPL < 500000                                         
381300       MOVE 20                 TO WS-SEKEL-9KOMPL                         
381400     ELSE                                                                 
381500       MOVE 19                 TO WS-SEKEL-9KOMPL                         
381600     END-IF                                                               
381700                                                                          
381800     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
381900                                                                          
382000     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
382100     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
382200                                                                          
382300     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
382400     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
382500     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
382600     MOVE ZERO                TO OBKR-TIDLEVDAT                           
382700     MOVE ORAD-PRAVCOST       TO OBKR-PRAVCOST                            
382800     MOVE ORAD-KDVALISO       TO OBKR-KDVALISO                            
382900     .                                                                    
383000     EJECT                                                                
383100                                                                          
383200 ED-LAES-TILLK-DATA SECTION.                                              
383300                                                                          
383400     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
383500                               TO AREG-IDARTNR                            
383600                                                                          
383700     CALL W411AREG USING AREG-W411AREG                                    
383800                         AREG-WDK6-PCB                                    
383900                         AREG-WDK7-PCB                                    
384000     .                                                                    
384100     EJECT                                                                
384200                                                                          
384300 G-AVSLUTA-ORDERN SECTION.                                                
384400                                                                          
384500     IF OHUV-FLKLAR = 'N' AND (OHUV-IDSYSTEM NOT = '4255' AND             
384600                               OHUV-IDSYSTEM(1:3) NOT = 'LDC' AND         
384700                               OHUV-IDSYSTEM(1:3) NOT = 'LYN' AND         
384800                               OHUV-IDSYSTEM(1:3) NOT = 'ECO' AND         
384900                               OHUV-IDSYSTEM(1:3) NOT = 'VOU' AND         
385000                               OHUV-IDSYSTEM(1:3) NOT = 'TAD' AND         
385100                               OHUV-IDSYSTEM(1:3) NOT = 'ACC' AND         
385200                               OHUV-IDSYSTEM(1:3) NOT = 'APA' AND         
385300                               OHUV-IDSYSTEM(1:3) NOT = 'APB' AND         
385400                               OHUV-IDSYSTEM(1:3) NOT = 'APC' AND         
385500                               OHUV-IDSYSTEM(1:3) NOT = 'APD' AND         
385600                               OHUV-IDSYSTEM(1:3) NOT = 'APE' AND         
385700                               OHUV-IDSYSTEM(1:3) NOT = 'APF' AND         
385800                               OHUV-IDSYSTEM(1:3) NOT = 'APG' AND         
385900                               OHUV-IDSYSTEM(1:3) NOT = 'APH' AND         
386000                               OHUV-IDSYSTEM(1:3) NOT = 'API' AND         
386100                               OHUV-IDSYSTEM(1:3) NOT = 'APJ')            
386200       CONTINUE                                                           
386300     ELSE                                                                 
386400       MOVE W-IDDISTR            TO 4298-MID-IDDISTR                      
386500       MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                     
386600       MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                     
386700       MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                      
386800       IF (MID-IDSYSTEM = 'LDCC' OR 'LYNC' OR 'ECOC' OR 'VOUC' OR         
386900                          'TADC' OR 'ACCC' OR 'APAC' OR                   
387000                          'APBC' OR 'APCC' OR 'APDC' OR                   
387100                          'APEC' OR 'APFC' OR 'APGC' OR                   
387200                          'APHC' OR 'APIC' OR 'APJC')                     
387300          MOVE 'FIXA'            TO 4298-IDTRANS                          
387400       END-IF                                                             
387500                                                                          
387600       COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                 
387700       PERFORM IMS-INSERT-4298-MSG                                        
387800     END-IF                                                               
387900     .                                                                    
388000     EJECT                                                                
388100 J-STARTA-4251 SECTION.                                                   
388200                                                                          
388300     MOVE 'W4T251X '           TO MSG-KDTRANS-1                           
388400     MOVE '4255'               TO MSG-IDTRANS-1                           
388500     PERFORM IMS-INSERT-4251-MSG                                          
388600     .                                                                    
388700     EJECT                                                                
388800                                                                          
388900 I-SKICKA-PRISFRAGA SECTION.                                              
389000                                                                          
389100     MOVE 1                      TO 3039-REQU-IDMSGVER                    
389200     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
389300     MOVE 'W4025500'             TO 3039-REQU-IDUSER                      
389400                                                                          
389500     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
389600     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
389700     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
389800     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
389900*    MOVE ORAD-IDPRQUES          TO 3039-MID-IDPRQUES                     
390000     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
390100                                                                          
390200     PERFORM S04-SKICKA-OPEN                                              
390300     PERFORM S04-SKICKA-MEDDELANDE                                        
390400     PERFORM S04-SKICKA-CLOSE                                             
390500                                                                          
390600     .                                                                    
390700     EJECT                                                                
390800 Z-FINIT  SECTION.                                                        
390900                                                                          
391000*    SKRIV MED TILL MPP DISPATCHERN                                       
391100     IF MSG-KOM-IDMFSMED = SPACE                                          
391200        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
391300     END-IF                                                               
391400     PERFORM IMS-INSERT-DISP-MSG                                          
391500     .                                                                    
391600     EJECT                                                                
391700 S02-RENSA-TILLK-TAB SECTION.                                             
391800                                                                          
391900     MOVE +1              TO WS-INDEX-TILLK                               
392000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
392100        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
392200        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
392300        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
392400        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
392500        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
392600        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
392700        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
392800        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
392900        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
393000        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
393100        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
393200        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
393300        INITIALIZE           TILK-DEAL-PR-LINE(WS-INDEX-TILLK)            
393400        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
393500        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
393600        ADD +1            TO WS-INDEX-TILLK                               
393700     END-PERFORM                                                          
393800     MOVE +1              TO WS-INDEX-TILLK                               
393900     .                                                                    
394000     EJECT                                                                
394100                                                                          
394200 S04-SKICKA-OPEN SECTION.                                                 
394300                                                                          
394400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
394500     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
394600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
394700                                                                          
394800     IF SEND-KDRC > 0                                                     
394900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
395000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
395100       DELIMITED BY SIZE INTO FELTEXT                                     
395200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
395300     END-IF                                                               
395400     .                                                                    
395500     SKIP3                                                                
395600 S04-SKICKA-MEDDELANDE SECTION.                                           
395700                                                                          
395800     MOVE 'PUT'                      TO SEND-KDFUNC                       
395900     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
396000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
396100                                                                          
396200     IF SEND-KDRC > 0                                                     
396300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
396400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
396500       DELIMITED BY SIZE INTO FELTEXT                                     
396600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
396700     END-IF                                                               
396800     .                                                                    
396900     SKIP3                                                                
397000 S04-SKICKA-CLOSE SECTION.                                                
397100                                                                          
397200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
397300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
397400                                                                          
397500     IF SEND-KDRC > 0                                                     
397600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
397700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
397800       DELIMITED BY SIZE INTO FELTEXT                                     
397900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
398000     END-IF                                                               
398100     .                                                                    
398200     EJECT                                                                
398300                                                                          
398400 S05-DELETE-PRICE-Q-LINE SECTION.                                         
398500                                                                          
398600     IF DIST79-DEALER-PRICE                                               
398700       IF OBKR-IDPRQUES > ZERO                                            
398800         INITIALIZE PRQU-W335PRQU                                         
398900         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
399000         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
399100         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
399200         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
399300         MOVE 4                  TO PRQU-KDCALL                           
399400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
399500                                            PRQU-WDC7-PCB                 
399600                                            PRQU-SJKO-WDK6-PCB            
399700       END-IF                                                             
399800     END-IF                                                               
399900     .                                                                    
400000                                                                          
400100 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
400200                                                                          
400300     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
400400     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
400500     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
400600     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
400700     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
400800     MOVE ORAD-IDDC            TO LAST-IDDC                               
400900     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
401000     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
401100     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
401200     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
401300                                                                          
401400     MOVE ORAD-IDDC            TO WS-IDDC                                 
401500     PERFORM S40-HAMTA-WDB6-INFO                                          
401600     IF VANLIGA-RADER                                                     
401700       CALL W411LAST USING LAST-W411LAST                                  
401800     ELSE                                                                 
401900       MOVE +0                 TO LAST-ADLAGOMR-UT                        
402000       MOVE +0                 TO LAST-KVANTAL-UT                         
402100       MOVE +0                 TO LAST-KVBEART-UT                         
402200     END-IF                                                               
402300     .                                                                    
402400     EJECT                                                                
402500 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
402700     IF LAST-ADLAGOMR-UT = +0 AND                                         
402800        LAST-KVANTAL-UT  = +0 AND                                         
402900        LAST-KVBEART-UT  = +0                                             
403000*------------------------------------------------------------*            
403100*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
403200*------------------------------------------------------------*            
403300        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
403400        PERFORM S10B-REDIGERA-WOPS-AREA                                   
403500        PERFORM IMS-22-ISRT-ORQF-WDQ401                                   
403600        PERFORM UNTIL SEGMENT-FINNS                                       
403700           ADD +1           TO ORAD-IDLOPNR                               
403800           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
403900        END-PERFORM                                                       
404000     ELSE                                                                 
404100                                                                          
404200        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
404300*------------------------------------------------------------*            
404400*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
404500*------------------------------------------------------------*            
404600                                                                          
404700           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
404800           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
404900                                    ORAD-KVPRERO                          
405000           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
405100           PERFORM S10C-BERAEKNA-KVSLATT                                  
405200           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
405300           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
405400           MOVE ORAD-ADGANG          TO WS-ADGANG                         
405500           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
405600           PERFORM S10B-REDIGERA-WOPS-AREA                                
405700           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
405800           PERFORM UNTIL SEGMENT-FINNS                                    
405900              ADD +1        TO ORAD-IDLOPNR                               
406000              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
406100           END-PERFORM                                                    
406200****************************************************************          
406300*                                                                         
406400*------------------------------------------------------------*            
406500*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
406600*------------------------------------------------------------*            
406700                                                                          
406800           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
406900           MOVE WS-ADGANG            TO ORAD-ADGANG                       
407000           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
407100                                                                          
407200           MOVE +0                   TO ORAD-KVBEART                      
407300           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
407400                                        ORAD-KVPREAVB                     
407500           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
407600           IF LAST-ADGANG-UT > ZERO                                       
407700             MOVE LAST-ADGANG-UT     TO ORAD-ADGANG                       
407800           END-IF                                                         
407900           MOVE +0                   TO ORAD-KVPRERO                      
408000           MOVE 1.0000               TO ORAD-RERF-RAD                     
408100           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
408200           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
408300           PERFORM S10B-REDIGERA-WOPS-AREA                                
408400           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
408500           PERFORM UNTIL SEGMENT-FINNS                                    
408600              ADD +1        TO ORAD-IDLOPNR                               
408700              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
408800           END-PERFORM                                                    
408900        ELSE                                                              
409000*------------------------------------------------------------*            
409100*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
409200*------------------------------------------------------------*            
409300           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
409400           IF LAST-ADGANG-UT > ZERO                                       
409500             MOVE LAST-ADGANG-UT     TO ORAD-ADGANG                       
409600           END-IF                                                         
409700           MOVE 1.0000            TO ORAD-RERF-RAD                        
409800           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
409900           PERFORM S10B-REDIGERA-WOPS-AREA                                
410000           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
410100           PERFORM UNTIL SEGMENT-FINNS                                    
410200              ADD +1        TO ORAD-IDLOPNR                               
410300              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
410400           END-PERFORM                                                    
410500        END-IF                                                            
410600     END-IF                                                               
410700     .                                                                    
410800     EJECT                                                                
410900 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
411000                                                                          
411100     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
411200     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
411300     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
411400     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
411500     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
411600     MOVE 1                    TO ADRS-KDCALL-IN                          
411700     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
411800     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
411900     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
412000     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
412100                                                                          
412200     CALL W413ADRS USING ADRS-W413ADRS                                    
412300                                                                          
412400     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
412500     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
412600                                                                          
412700     .                                                                    
412800     EJECT                                                                
412900 S10B-REDIGERA-WOPS-AREA SECTION.                                         
413000                                                                          
413100     IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU' OR          
413200                             'TAD' OR 'ACC' OR 'APA' OR                   
413300                             'APB' OR 'APC' OR 'APD' OR                   
413400                             'APE' OR 'APF' OR 'APG' OR                   
413500                             'APH' OR 'API' OR 'APJ')                     
413600       MOVE +3                 TO AVSR-KDCALL                             
413700     ELSE                                                                 
413800       MOVE +1                 TO AVSR-KDCALL                             
413900     END-IF                                                               
414000     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
414100     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
414200     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
414300     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
414400     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
414500     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
414600                                                                          
414700     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
414800     IF OHUV-FLEMBORD = JA OR OHUV-FLOVRLEV = JA                          
414900        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
415000     ELSE                                                                 
415100        MOVE ORAD-IDLEVNR      TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
415200     END-IF                                                               
415300     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
415400     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
415500     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
415600     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
415700     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
415800     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
415900     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
416000     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
416100     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
416200     IF (MID-IDSYSTEM(1:3) = 'LDC' OR 'LYN' OR 'ECO' OR 'VOU' OR          
416300                             'TAD' OR 'ACC' OR 'APA' OR                   
416400                             'APB' OR 'APC' OR 'APD' OR                   
416500                             'APE' OR 'APF' OR 'APG' OR                   
416600                             'APH' OR 'API' OR 'APJ')                     
416700       MOVE ARB-TIRFS          TO WS-TIRFS                                
416800       MOVE WS-TIRFS-DATUM     TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
416900     END-IF                                                               
417000     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
417100     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
417200                                                                          
417300     ADD +1                    TO WS-INDEX-WOPS                           
417400                                                                          
417500     IF WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                                 
417600                                                                          
417700        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
417800          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
417900          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
418000          TRAN-XXKB-PCB                                                   
418100                                                                          
418200        PERFORM AA-NOLLA-WOPS-TABELL                                      
418300     END-IF                                                               
418400     .                                                                    
418500     EJECT                                                                
418600 S10C-BERAEKNA-KVSLATT SECTION.                                           
418700                                                                          
418800     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
418900                                                                          
419000        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
419100                                                                          
419200        COMPUTE ORAD-KVSLATT ROUNDED =                                    
419300               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
419400     END-IF                                                               
419500     .                                                                    
419600     EJECT                                                                
419700 S20-OBKR-FRAN-TILLK-TAB SECTION.                                         
419800                                                                          
419900     MOVE +1                   TO WS-INDEX-TILLK                          
420000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
420100                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
420200        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
420300           IF OBKR-SKRIVEN                                                
420400              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
420500              ADD +1              TO OBKR-IDSEKVNR                        
420600           END-IF                                                         
420700           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
420800           MOVE '4255KER4'     TO OBKR-IDPGM                              
420900           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
421000                               TO OBKR-IDARTNR-TILLK                      
421100           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
421200                               TO OBKR-REKSIFFR-TILLK                     
421300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
421400                               TO OBKR-KVBEART-TILLK                      
421500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
421600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
421700             MOVE +0              TO OBKR-DIERS-KVOT                      
421710           ELSE                                                           
421720             COMPUTE OBKR-DIERS-KVOT =                                    
421730                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
421740                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
421750           END-IF                                                         
421800           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
421900                               TO OBKR-BEERS                              
422000                                                                          
422100           MOVE 'S'            TO OBKR-SW                                 
422200        END-IF                                                            
422300        ADD +1                 TO WS-INDEX-TILLK                          
422400     END-PERFORM                                                          
422500     IF WS-INDEX-TILLK = +1                                               
422600        MOVE +0                TO OBKR-KDERS                              
422700     END-IF                                                               
422800     .                                                                    
422900     EJECT                                                                
423000 S21-UPPDATERA-TPO6 SECTION.                                              
423100                                                                          
423200     MOVE ORAD-IDDISTR         TO TPO6-IDDISTR                            
423300     MOVE ORAD-IDKUNDNR        TO TPO6-IDKUNDNR                           
423400     MOVE ORAD-IDKUNDRF        TO TPO6-IDKUNDRF                           
423500     MOVE ORAD-IDARTNR         TO TPO6-IDARTNR                            
423600     MOVE OHUV-IDDC-PRIM       TO TPO6-IDDC-DAY                           
423700     MOVE AREG-FLREFILL        TO TPO6-FLREFILL                           
423800     MOVE AREG-KDUART          TO TPO6-KDUART                             
423900     MOVE AREG-REDIRLEV        TO TPO6-REDIRLEV                           
424000     MOVE ORAD-BERADREF        TO TPO6-BERADREF                           
424100     MOVE AREG-IDANSK          TO TPO6-IDANSK                             
424200     MOVE OHUV-IDKONTO         TO TPO6-IDKONTO                            
424300     MOVE OHUV-IDKST           TO TPO6-IDKST                              
424400     MOVE OHUV-IDANALYS        TO TPO6-IDANALYS                           
424500     MOVE ORAD-KDDSP           TO TPO6-KDDSP                              
424600     MOVE OHUV-KDFAKTYP        TO TPO6-KDFAKTYP                           
424700     MOVE ARB-KDFRAKT          TO TPO6-KDFRAKT                            
424800     MOVE ORAD-KDKVBRYT        TO TPO6-KDKVBRYT                           
424900     MOVE ORAD-KDORDING        TO TPO6-KDORDING                           
425000     MOVE OHUV-KDORDKL         TO TPO6-KDORDKL                            
425100     MOVE AREG-KDPRODSL        TO TPO6-KDPRODSL                           
425200     MOVE ORAD-KDVRINFO        TO TPO6-KDVRINFO                           
425300     MOVE ORAD-KVBEART-Q       TO TPO6-KVBEART-Q                          
425400     MOVE AREG-REKSIFFR        TO TPO6-REKSIFFR                           
425500     IF ORAD-KDPRTYP = 'P'                                                
425600        MOVE ORAD-PRARTNTO     TO TPO6-PRARTNTO                           
425700        MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                      
425800        MOVE ORAD-KDPRTYP      TO TPO6-KDPRTYP                            
425900        MOVE ORAD-FLPRTILL     TO TPO6-FLPRTILL                           
426000     ELSE                                                                 
426100        MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                      
426200        MOVE +0                TO TPO6-PRARTNTO                           
426300        MOVE SPACE             TO TPO6-KDPRTYP                            
426400        MOVE NEJ               TO TPO6-FLPRTILL                           
426500     END-IF                                                               
426600     MOVE ORAD-BEVOLREF        TO TPO6-BEVOLREF                           
426700     MOVE ORAD-IDKAMPRF        TO TPO6-IDKAMPRF                           
426800     MOVE ORAD-IDSYSTEM        TO TPO6-IDSYSTEM                           
426900     MOVE ORAD-FLINVEST        TO TPO6-FLINVEST                           
427000     MOVE ORAD-IDLEVNR         TO TPO6-IDLEVNR                            
427100     MOVE ORAD-KDTPOTYP        TO TPO6-KDTPOTYP                           
427200     MOVE OHUV-BEKUNDRF        TO TPO6-BEKUNDRF                           
427300     MOVE OHUV-FLFORBI         TO TPO6-FLFORBI                            
427400                                                                          
427500     MOVE ZERO                 TO TPO6-KDORDBEK                           
427600     MOVE SPACE                TO TPO6-FLKLAR                             
427700                                                                          
427800     MOVE OHUV-KDORDTYP-LDC   TO TPO6-KDORDTYP-LDC                        
427900     MOVE OHUV-TIREPDAT       TO TPO6-TIREPDAT                            
428000     MOVE ORAD-IDKUNDRF-WIP   TO TPO6-IDKUNDRF-WIP                        
428100                                                                          
428200     MOVE ORAD-KDOI           TO TPO6-KDOI                                
428300     MOVE ORAD-CLEARGROUP     TO TPO6-CLEARGROUP                          
428400                                                                          
428500     CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB             
428600                         TPO6-XXBU-PCB TPO6-XXBV-PCB                      
428700                         TPO6-XXBX-PCB TPO6-ARTS-PCB TIME-4437-PCB        
428800                                                                          
428900     .                                                                    
429000     EJECT                                                                
429100 S40-HAMTA-WDB6-INFO      SECTION.                                        
429200                                                                          
429300     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
429400        CONTINUE                                                          
429500     ELSE                                                                 
429600        IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                           
429700                                                                          
429800           MOVE 1 TO WS-CLDC-IX                                           
429900           PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                   
430000                         CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR              
430100                         CLDC-IDDC (WS-CLDC-IX) = SPACE                   
430200              ADD 1 TO WS-CLDC-IX                                         
430300           END-PERFORM                                                    
430400                                                                          
430500        END-IF                                                            
430600     END-IF                                                               
430700     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
430800        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
430900        MOVE WS-IDDC TO W-IDDC-B6                                         
431000        PERFORM IMS-GU-WDB601                                             
431100     ELSE                                                                 
431200        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
431300     END-IF                                                               
431400     .                                                                    
431500     EJECT                                                                
431600                                                                          
431700* --- IMS SEKTIONER ---                                                   
431800                                                                          
431900 IMS-GET-MSG SECTION.                                                     
432000                                                                          
432100     MOVE '  QC' TO GODK-STATUSKODER                                      
432200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
432300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
432400     PERFORM IMS-STATUSKONTROLL                                           
432500     .                                                                    
432600     SKIP2                                                                
432700 IMS-GN-MSG SECTION.                                                      
432800                                                                          
432900     MOVE '  '   TO GODK-STATUSKODER                                      
433000     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
433100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
433200     PERFORM IMS-STATUSKONTROLL                                           
433300     .                                                                    
433400     SKIP2                                                                
433500 IMS-INSERT-DISP-MSG SECTION.                                             
433600                                                                          
433700     MOVE SPACE TO GODK-STATUSKODER                                       
433800     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
433900     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
434000     PERFORM IMS-STATUSKONTROLL                                           
434100     .                                                                    
434200     EJECT                                                                
434300 IMS-INSERT-4251-MSG SECTION.                                             
434400                                                                          
434500     MOVE SPACE TO GODK-STATUSKODER                                       
434600     CALL CBLTDLI USING ISRT 4251-PCB MSG-IO-AREA                         
434700     MOVE 4251-STATUS-CODE TO STATUS-WS                                   
434800     PERFORM IMS-STATUSKONTROLL                                           
434900     CALL CBLTDLI USING ISRT 4251-PCB KOM-IO-AREA                         
435000     MOVE 4251-STATUS-CODE TO STATUS-WS                                   
435100     PERFORM IMS-STATUSKONTROLL                                           
435200     .                                                                    
435300     EJECT                                                                
435400 IMS-INSERT-4298-MSG SECTION.                                             
435500                                                                          
435600     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
435700     MOVE SPACE TO GODK-STATUSKODER                                       
435800     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
435900     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
436000     PERFORM IMS-STATUSKONTROLL                                           
436100     .                                                                    
436200     EJECT                                                                
436300                                                                          
436400 IMS-01-GU-ORQI-WDQ201 SECTION.                                           
436500                                                                          
436600     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
436700          DELIMITED BY SIZE INTO SSA1                                     
436800     MOVE '  GE'               TO GODK-STATUSKODER                        
436900     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
437000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
437100     PERFORM IMS-STATUSKONTROLL                                           
437200     .                                                                    
437300     SKIP2                                                                
437400 IMS-02-GHU-ORQI-WDQ201 SECTION.                                          
437500                                                                          
437600     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
437700          DELIMITED BY SIZE INTO SSA1                                     
437800     MOVE '  '                 TO GODK-STATUSKODER                        
437900     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
438000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
438100     PERFORM IMS-STATUSKONTROLL                                           
438200     .                                                                    
438300     SKIP2                                                                
438400 IMS-03-GNP-ORQI-WDQ212 SECTION.                                          
438500                                                                          
438600     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
438700          DELIMITED BY SIZE INTO SSA1                                     
438800     MOVE '  GE'               TO GODK-STATUSKODER                        
438900     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
439000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
439100     PERFORM IMS-STATUSKONTROLL                                           
439200     .                                                                    
439300     EJECT                                                                
439400 IMS-GHNP-WDQ212-OKVAL  SECTION.                                          
439500                                                                          
439600     MOVE 'WLORQI12'           TO SSA1                                    
439700     MOVE '  GE'               TO GODK-STATUSKODER                        
439800     CALL CBLTDLI USING GHNP  ORQI-PCB DLI-IO-AREA-ARB SSA1               
439900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
440000     PERFORM IMS-STATUSKONTROLL                                           
440100     .                                                                    
440200     EJECT                                                                
440300 IMS-07-REPL-ORQI-WDQ201 SECTION.                                         
440400                                                                          
440500     MOVE '    '               TO GODK-STATUSKODER                        
440600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
440700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
440800     PERFORM IMS-STATUSKONTROLL                                           
440900     .                                                                    
441000     SKIP2                                                                
441100 IMS-REPL-WDQ212         SECTION.                                         
441200                                                                          
441300     MOVE '    '               TO GODK-STATUSKODER                        
441400     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ARB                     
441500     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
441600     PERFORM IMS-STATUSKONTROLL                                           
441700     .                                                                    
441800     SKIP2                                                                
441900 IMS-10-GU-WLARTM-WDK901 SECTION.                                         
442000                                                                          
442100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
442200          DELIMITED BY SIZE INTO SSA1                                     
442300     MOVE '  GE'               TO GODK-STATUSKODER                        
442400     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
442500     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
442600     PERFORM IMS-STATUSKONTROLL                                           
442700     .                                                                    
442800     SKIP2                                                                
442900 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
443000                                                                          
443100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
443200          DELIMITED BY SIZE INTO SSA1                                     
443300     MOVE '    '               TO GODK-STATUSKODER                        
443400     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
443500     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
443600     PERFORM IMS-STATUSKONTROLL                                           
443700     .                                                                    
443800                                                                          
443900 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
444000                                                                          
444100     MOVE '    '               TO GODK-STATUSKODER                        
444200     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
444300     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
444400     PERFORM IMS-STATUSKONTROLL                                           
444500     .                                                                    
444600     EJECT                                                                
444700 IMS-22-ISRT-ORQF-WDQ401 SECTION.                                         
444800                                                                          
444900     MOVE 'WLORQF01 '          TO SSA1                                    
445000     MOVE '  II'               TO GODK-STATUSKODER                        
445100     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
445200     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
445300     PERFORM IMS-STATUSKONTROLL                                           
445400     .                                                                    
445500     EJECT                                                                
445600                                                                          
445700 IMS-24-GU-ORQM-WDQ101 SECTION.                                           
445800                                                                          
445900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
446000                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
446100          DELIMITED BY SIZE INTO SSA1                                     
446200     MOVE '  GE'               TO GODK-STATUSKODER                        
446300     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
446400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
446500     PERFORM IMS-STATUSKONTROLL                                           
446600     .                                                                    
446700     SKIP2                                                                
446800 IMS-25-ISRT-WLORQM01-WDQ101 SECTION.                                     
446900                                                                          
447000     MOVE 'WLORQM01 '          TO SSA1                                    
447100     MOVE '    '               TO GODK-STATUSKODER                        
447200     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
447300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
447400     PERFORM IMS-STATUSKONTROLL                                           
447500     .                                                                    
447600     SKIP2                                                                
447700 IMS-GU-WDB201 SECTION.                                                   
447800                                                                          
447900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
448000          DELIMITED BY SIZE INTO SSA1                                     
448100     MOVE '  '               TO GODK-STATUSKODER                          
448200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
448300     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
448400     PERFORM IMS-STATUSKONTROLL                                           
448500     .                                                                    
448600     SKIP2                                                                
448700 IMS-GU-WDB101 SECTION.                                                   
448800                                                                          
448900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
449000          DELIMITED BY SIZE INTO SSA1                                     
449100     MOVE '    '               TO GODK-STATUSKODER                        
449200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
449300     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
449400     PERFORM IMS-STATUSKONTROLL                                           
449500     .                                                                    
449600                                                                          
449700 IMS-GU-WDB601    SECTION.                                                
449800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
449900          DELIMITED BY SIZE INTO SSA1                                     
450000     MOVE '  '   TO GODK-STATUSKODER                                      
450100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
450200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
450300     PERFORM IMS-STATUSKONTROLL                                           
450400     .                                                                    
450500     SKIP2                                                                
450600 IMS-ISRT-WDR601 SECTION.                                                 
450700                                                                          
450800     MOVE 'WDR601' TO SSA1                                                
450900     MOVE '  II' TO GODK-STATUSKODER                                      
451000     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
451100     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
451200     PERFORM IMS-STATUSKONTROLL                                           
451300     .                                                                    
451400 IMS-STATUSKONTROLL SECTION.                                              
451500                                                                          
451600     SET STATUS-IX TO 1                                                   
451700     SEARCH GODK-STATUS                                                   
451800       AT END CALL FELLOG                                                 
451900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
452000     END-SEARCH                                                           
452100     .                                                                    
452200     EJECT                                                                
