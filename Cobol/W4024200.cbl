000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4024200.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000500 DATE-WRITTEN.   APRIL -90.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR EXTERN UPPLÄGGNING AV                        
001100*        ORDERRADER I ORDERKÖN.                                           
001200*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA                      
001300*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
001400*                                                                         
001500*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001600*        SVARSBILD - 4243.                                                
001700*                                                                         
001800*              I DE FALL ORAD-ADLAGOMR BLIR = +0                          
001900*              SÄTTER VI ORAD-ADLAGOMR = +1,                              
002000*                                                                         
002100*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
002200*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
002300*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
002400*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
002500*    DETTA I LAGEROMRÅDE 1.                                               
002600*                                                                         
002700*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002800*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
002900*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
003000*        PROGRAMMET LÄSER      WLORQA (WDQ3)  ORDERDELAR                  
003100*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
003200*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
003300*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
003400*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
003500*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
003600*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
003700*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
003800*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003900*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
004000*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
004100*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
004200*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
004300*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
004400*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
004500*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
004600*                                                                         
004700*    INDATA.                                                              
004800*        TRANSAKTION: W4T242                                              
004900*        MID:         W4I24201                                            
005000*    UTDATA.                                                              
005100*        MOD:         W4O24201                                            
005200*                                                                         
005300*    E'TRACKER: 5444132 DATED 2007-09-18                                  
005400*    E'TRACKER: 7450328 DATED 2008-HÖST    VOHF                           
005500*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
005600*    E'TRACKER: 10254592      2015         DECOMISSION VOHF               
005700*    E'TRACKER: 10263222      2015        FORCE TO END ORDER REG          
005800*                                                                         
005900     EJECT                                                                
006000 ENVIRONMENT DIVISION.                                                    
006100                                                                          
006200 DATA DIVISION.                                                           
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(08)   VALUE 'W4024200'.            
006700                                                                          
006800 01  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
006900*01  -COPY WWDCKONS                                                       
007000                                                                          
007100 77  HOPP                        PIC X(1)   VALUE 'N'.                    
007200 77  HOPP-TILL-0504              PIC X(1)   VALUE 'N'.                    
007300 77  YES                         PIC X(1)   VALUE 'Y'.                    
007400*    ---FOR MOD0504-IDTRANS                                               
007500 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
007600*                                                                         
007700 77  CURRENT-SECTION             PIC X(20)  VALUE SPACE.                  
007800 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
007900 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
008000 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008100 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
008200 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008300 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008400 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
008500 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
008600 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
008700 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-WOPS-MAX           PIC S9(9)  COMP SYNC VALUE +100.         
008900 77  WS-SAVE-INDEX               PIC S9(9)   COMP SYNC VALUE ZERO.        
009000 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
009100 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
009200 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
009300 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009400 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
009500 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
009600 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
009700 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
009800 77  WS-IXDCCLEAR                PIC S9(5)   VALUE ZERO COMP-3.           
009900 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
010100                                                                          
010400 77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
010500 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010600 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010700 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010800 77  W-TILLK-DC                  PIC X(2)    VALUE SPACE.                 
010900                                                                          
011000     EJECT                                                                
011100                                                                          
011200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011300     88  ALLT-OK                             VALUE 'J'.                   
011400                                                                          
011500 77  TILLK-SW                    PIC X       VALUE 'N'.                   
011600     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
011700     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
011800                                                                          
011900 77  BIPA-SW                     PIC X       VALUE 'N'.                   
012000     88  BIPA-JA                             VALUE 'J'.                   
012100                                                                          
012200 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
012300     88  KOLLA-ERS                           VALUE 'J'.                   
012400                                                                          
012500 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
012600     88  SVARSBILD                           VALUE 'J'.                   
012700                                                                          
012800 77  OBKR-SW                     PIC X       VALUE 'N'.                   
012900     88  SKRIV-OBKR                          VALUE 'J'.                   
013000     88  OBKR-SKRIVEN                        VALUE 'S'.                   
013100                                                                          
013200 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
013300     88  EGET-CL-RAD                         VALUE 'J'.                   
013400                                                                          
013500 77  SW-DDGS-TPO-OBKR71          PIC X       VALUE 'N'.                   
013600     88  DDGS-TPO-OBKR71                     VALUE 'J'.                   
013700                                                                          
013800 77  BAL-DC-FND-SW               PIC X       VALUE 'N'.                   
013900     88  BAL-DC-FND                          VALUE 'J'.                   
014000                                                                          
014100 77  TILLK-BAL-DC-FND-SW         PIC X       VALUE 'N'.                   
014200     88  TILLK-BAL-DC-FND                    VALUE 'J'.                   
014300                                                                          
014400 77  CDC-MOVE-SW                 PIC X       VALUE 'N'.                   
014500     88  CDC-MOVE                            VALUE 'J'.                   
014600                                                                          
014700 77  KDERS-CHAIN-SW              PIC X       VALUE 'N'.                   
014800     88  KDERS-CHAIN                         VALUE 'J'.                   
014900                                                                          
015000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015100     88  EGEN-MID                            VALUE '4242'.                
015200     88  GODK-MID                            VALUE '4241' '4242'          
015300                                                   '4243' '4244'.         
015400     EJECT                                                                
015500 01  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
015600                                                                          
015700 01  WS-ALFA-1.                                                           
015800     03  WS-NUM-1                PIC 9(1).                                
015900 01  WS-ALFA-6.                                                           
016000     03  WS-NUM-6                PIC 9(6).                                
016100 01  WS-ALFA-7.                                                           
016200     03  WS-NUM-7                PIC 9(7).                                
016300 01  WS-ALFA-8.                                                           
016400     03  WS-NUM-8                PIC 9(8).                                
016500     03  FILLER REDEFINES WS-NUM-8.                                       
016600         05  WS-NUM-1--4         PIC 9(4).                                
016700         05  WS-NUM-5--8         PIC 9(4).                                
016800 01  WS-NUM-2V3                  PIC 9(2)V9(3).                           
016900 01  FILLER REDEFINES WS-NUM-2V3.                                         
017000     03  WS-NUM-1--2             PIC 9(2).                                
017100     03  WS-NUM-3--5             PIC 9(3).                                
017200                                                                          
017300     SKIP2                                                                
017400 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
017500 01  FILLER REDEFINES WS-TIHHMMSS.                                        
017600     03 WS-TIHHMM                PIC 9(4).                                
017700     03 FILLER                   PIC 9(2).                                
017800                                                                          
017900 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
018000 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
018100     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
018200     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
018300 01  SPAR-AREA.                                                           
018400     03  SPAR-BEHORIGHETS-KONTR  PIC X(1)  VALUE SPACE.                   
018500 01  W-WORK-VAR.                                                          
018600     03 W-FLREFILL-MAIN          PIC X       VALUE SPACE.                 
018700     03 W-KDPRODSL-MAIN          PIC S9(3)   COMP-3 VALUE 0.              
018800     03 W-KDSORT-MAIN            PIC X(2)    VALUE SPACE.                 
018900     03 W-KVQPACK-1-MAIN         PIC S9(5)   COMP-3 VALUE 0.              
019000     03 W-REDIRLEV-MAIN          PIC S9V9(2) COMP-3 VALUE 0.              
019100     03 W-FLREFILL-REPL          PIC X       VALUE SPACE.                 
019200     03 W-KDPRODSL-REPL          PIC S9(3)   COMP-3 VALUE 0.              
019300     03 W-KDSORT-REPL            PIC X(2)    VALUE SPACE.                 
019400     03 W-KVQPACK-1-REPL         PIC S9(5)   COMP-3 VALUE 0.              
019500     03 W-REDIRLEV-REPL          PIC S9V9(2) COMP-3 VALUE 0.              
019600     03 W-IDARTNR-SDCA           PIC S9(9)   COMP-3 VALUE 0.              
019600     03 W-GMT-IDDC-CLEAR-GRP.                                             
019600*                                 GRUPP AV IDDC-CLEAR                     
019600        05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                             
019600                                 PIC X(2)    VALUE SPACE.                 
019700                                                                          
019800     EJECT                                                                
019900*                                                                         
020000*01  -COPY WWPRODSL                                                       
020100*                                                                         
020200 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
020300*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
020400*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
020500*    ----DISTR-DEALER-PRICE----                                           
020600     EJECT                                                                
020700 01 FILLER                    PIC X(16) VALUE 'TILLKOMMANDE TAB'.         
020800                                                                          
020900*    -COPY W411TILK                                                       
021000     EJECT                                                                
021100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021200 01  GENERELLA-SUBPROGRAM.                                                
021300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021400     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
021800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
021900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
022000*                                                                         
022100*                                                                         
022200*                                                                         
022300 01  GEMENSAMMA-SUBPROGRAM.                                               
022400     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
022500*        PRISTILLÄMPNING                                                  
022600     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
022700*        HÄMTA PRISFRÅGENR                                                
022800     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
022900*        DEALER PRISFRÅGABEHANDLING                                       
023000     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
023100*        LÄSNING ARTIKELREGISTER                                          
023200     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
023300*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
023400     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
023500*        KONTROLL DIREKTLEVERANS                                          
023600     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
023700*        KONTROLL TPO4 - KAMPANJ                                          
023800     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
023900*        KONTROLL ERSÄTTNINGAR                                            
024000     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
024100*        KONTROLL KVANTANPASSNING                                         
024200     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
024300*        KONTROLL ENHETSLAST                                              
024400     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
024500*        FORMELLA KONTROLLER AV INDATA                                    
024600     EJECT                                                                
024700     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
024800*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
024900     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
025000*        BERÄKNA RANSONERING                                              
025100     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
025200*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
025300     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
025400*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
025500     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
025600*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
025700     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
025800*        KONTROLL SPÄRRAR                                                 
025900     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
026000*        KONTROLL STORA UTTAG                                             
026100     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
026200*        KONTROLL TPO1                                                    
026300     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
026400*        KONTROLL TPO2                                                    
026500     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
026600*        KONTROLL RELS                                                    
026900     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
027000*        WDB601-SEGMENT FÖR CLARING-DC                                    
027100     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
027200*        RÄKNA OM VALUTA  DDI                                             
027300     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
027400*        WOPS RADBEHANDLING                                               
027500     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
027600*        OMVANDLING AV LAGOMR + PLATS                                     
027700     EJECT                                                                
027800*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
027900*   -COPY WSECAREA                                                        
028000     EJECT                                                                
028100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028200*   -COPY WMSGINIT                                                        
028300     EJECT                                                                
028400*   -COPY W402W001                                                        
028500     EJECT                                                                
028600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
028700*   -COPY WMEDAREA                                                        
028800     SKIP3                                                                
028900 01  FILLER.                                                              
029000   03  FELMEDD-AREA.                                                      
029100     05  FELMEDD-ENGLISH.                                                 
029200       10  FILLER                PIC X(50)                                
029300     VALUE '622 4242 NOT AVAILABLE ONLY VALID FROM 4241'.                 
029400     EJECT                                                                
029500 01  MESSAGE-CODES.                                                       
029600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
029700     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
029800     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
029900     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
030000     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
030100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
030200     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
030300     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
030400     EJECT                                                                
030500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
030600 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
030700*   -COPY W335PRIS                                                        
030800     EJECT                                                                
030900 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
031000*   -COPY W335PRNO                                                        
031100     EJECT                                                                
031200 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
031300*   -COPY W335PRQU                                                        
031400     EJECT                                                                
031500 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
031600*   -COPY W411AREG                                                        
031700     EJECT                                                                
031800 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
031900*   -COPY W411ARTM                                                        
032000     EJECT                                                                
032100 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
032200*   -COPY W411DLEV                                                        
032300     EJECT                                                                
032400 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
032500*   -COPY W411KAMP                                                        
032600     EJECT                                                                
032700 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
032800*   -COPY W411KERS                                                        
032900     EJECT                                                                
033000 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
033100*   -COPY W411KVAN                                                        
033200     EJECT                                                                
033300 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
033400*   -COPY W411LAST                                                        
033500     EJECT                                                                
033600 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
033700*   -COPY W411ORFK                                                        
033800     EJECT                                                                
033900 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
034000*   -COPY W411CDCA                                                        
034100     EJECT                                                                
034200 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
034300*   -COPY W411RANS                                                        
034400     EJECT                                                                
034500 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
034600*   -COPY W411NDCA                                                        
034700     EJECT                                                                
034800 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
034900*   -COPY W411XDCA                                                        
035000     EJECT                                                                
035100 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
035200*   -COPY W411XDK7 -PRE NDCA-                                             
035300     EJECT                                                                
035400 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
035500*   -COPY W411SDCA                                                        
035600     EJECT                                                                
035700 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
035800*   -COPY W411SPAR                                                        
035900     EJECT                                                                
036000 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
036100*   -COPY W411STOR                                                        
036200     EJECT                                                                
036300 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
036400*   -COPY W411TPO1                                                        
036500     EJECT                                                                
036600 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
036700*   -COPY W411TPO2                                                        
036800     EJECT                                                                
036900 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
037000*   -COPY W411RELS                                                        
037100     EJECT                                                                
037200 01 FILLER                       PIC X(8) VALUE 'W411EXCH'.               
037300*   -COPY W411EXCH                                                        
037400     EJECT                                                                
037800 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
037900*   -COPY W411CLDC                                                        
038000 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
038100*   -COPY W413AVSR                                                        
038200     SKIP2                                                                
038300 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
038400*   -COPY W413ADRS                                                        
038500     EJECT                                                                
038600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
038700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
038800     SKIP3                                                                
038900*01  MID -COPY W4I24201                                                   
039000     EJECT                                                                
039100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
039200     SKIP3                                                                
039300*01  -COPY WMSGAREA                                                       
039400     EJECT                                                                
039500*    03  MOD -COPY W4O24201   -RED MSG-AREA.                              
039600*   TO RETURN TO MAIN MENU                                                
039700*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
040000     SKIP3                                                                
040100*01  -COPY WMFSAREA                                                       
040200     EJECT                                                                
040300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
040400*                                                                         
040500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
040600     SKIP3                                                                
040700 01  NYCKLAR-TILL-DLI.                                                    
040800                                                                          
040900     03  W-IDGMTREF-X.                                                    
041000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
041100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
041200         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
041300                                                                          
041400     03  W-IDORDER-X.                                                     
041500         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
041600                                                                          
041700     03  W-IDARTNR-X.                                                     
041800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
041900                                                                          
042000                                                                          
042100                                                                          
042200     EJECT                                                                
042300                                                                          
042400     03  W-WDQ101KY-MIN-X.                                                
042500         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
042600         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
042700         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
042800         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
042900         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
043000                                                                          
043100     03  W-WDQ101KY-MAX-X.                                                
043200         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
043300         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
043400         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
043500         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
043600         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
043700     03  W-IDGMT-X.                                                       
043800         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
043900         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
044000*                                                                         
044900     03  W-WDB101KY-X.                                                    
045000       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
045100       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
045200                                                                          
045300     03  W-IDDC-B6-X.                                                     
045400         05 W-IDDC-B6                  PIC X(2).                          
045500*                                                                         
045600     03  W-WDB301KY-X.                                                    
045700         05  W-IDDC-WDB3          PIC X(2)    VALUE SPACE.                
045800         05  W-IDDISTR-WDB3       PIC S9(5)   COMP-3 VALUE ZERO.          
045900         05  W-IDKUNDNR-WDB3      PIC S9(7)   COMP-3 VALUE ZERO.          
046000*                                                                         
046100     03  W-WDB301KY-DEF-X.                                                
046200         05  W-IDDC-WDB3-DEF      PIC X(2)    VALUE SPACE.                
046300         05  W-IDDISTR-WDB3-DEF   PIC S9(5)   COMP-3 VALUE ZERO.          
046400         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
046500     EJECT                                                                
046600                                                                          
046700*    --- STATUS-KOD FRÅN IMS                                              
046800 01  STATUS-WS                   PIC XX.                                  
046900     88  SEGMENT-FINNS                       VALUE '  '.                  
047000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
047100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
047200     88  BASEN-SLUT                          VALUE 'GB'.                  
047300     SKIP2                                                                
047400 01  GODK-STATUSKODER.                                                    
047500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047600                                                                          
047700 01  SSA1                        PIC X(96).                               
047800 01  SSA2                        PIC X(64).                               
047900     EJECT                                                                
048000*    --- IMS FUNKTIONSKODER                                               
048100*01  -COPY W0003                                                          
048200     EJECT                                                                
048300*    ---  DLI INPUT-OUTPUT AREA                                           
048400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
048500     SKIP3                                                                
048600 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
048700 01  DLI-IO-AREA-OBKR.                                                    
048800     03  WLORQM01.                                                        
048900*        05  -COPY WDQ101                                                 
049000     EJECT                                                                
049100 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
049200 01  DLI-IO-AREA-OHUV.                                                    
049300     03  WLORQI01.                                                        
049400*        05  -COPY WDQ201                                                 
049500     EJECT                                                                
049600 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
049700 01  DLI-IO-AREA-ARB.                                                     
049800     03  WLORQI12.                                                        
049900*        05  -COPY WDQ212                                                 
050000     EJECT                                                                
050100 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
050200 01  DLI-IO-AREA-ORAD.                                                    
050300     03  WLORQF01.                                                        
050400*        05  -COPY WDQ401                                                 
050500     EJECT                                                                
050600 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
050700 01  DLI-IO-AREA-ART.                                                     
050800     03  WLARTM01.                                                        
050900*        05  -COPY WDK901                                                 
051000     EJECT                                                                
051100 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
051200 01  DLI-IO-AREA-WDB201.                                                  
051400*    03  -COPY WDB201                                                     
051500     EJECT                                                                
051600 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
051700 01  DLI-IO-AREA-WDB101.                                                  
051800     03  WLBETC01.                                                        
051900*        05  -COPY WDB101                                                 
052000                                                                          
052100 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
052200 01   DLI-IO-AREA-B601.                                                   
052300*    03  -COPY WDB601                                                     
052400                                                                          
052500 01  FILLER               PIC X(16)   VALUE 'WDB301 AREA'.                
052600 01   DLI-IO-AREA-WDB301.                                                 
052700*     03  -COPY WDB301                                                    
052800                                                                          
052900 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
053000 01   DLI-IO-AREA-R601.                                                   
053100*     03  -COPY WDR601                                                    
053200*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
053300                                                                          
053400 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
053500 01  4243-MSG-IO-AREA.                                                    
053600     03  4243-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
053700     03  4243-Z1               PIC X.                                     
053800     03  4243-Z2               PIC X.                                     
053900     03  4243-TRANSKOD         PIC X(8)   VALUE 'W4T243  '.               
054000     03  4243-IDTRANS          PIC X(4)   VALUE '4242'.                   
054100     03  4243-SPRAK            PIC X.                                     
054200     03  4243-IDDISTR-IN       PIC X(4).                                  
054300     03  4243-IDKUNDNR-IN      PIC X(6).                                  
054400     03  4243-IDORDNR-IN       PIC X(5).                                  
054500     03  4243-IDDISTR-UT       PIC X(4).                                  
054600     03  4243-IDKUNDNR-UT      PIC X(6).                                  
054700     03  4243-IDORDNR-UT       PIC X(5).                                  
054800     03  4243-KDORDKL-UT       PIC X      VALUE SPACE.                    
054900     03  4243-FLANNULL         PIC X      VALUE 'N'.                      
055000     03  FILLER                PIC X(20)  VALUE ZERO.                     
055100     EJECT                                                                
055200 01  4297-MSG-IO-AREA.                                                    
055300     03  4297-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
055400     03  4297-Z1               PIC X.                                     
055500     03  4297-Z2               PIC X.                                     
055600     03  4297-TRANSKOD         PIC X(8)   VALUE 'W4T297X '.               
055700     03  4297-IDTRANS          PIC X(4)   VALUE '4242'.                   
055800     03  4297-SPRAK            PIC X.                                     
055900*    03  -COPY W4I29701  -PRE 4297-                                       
056000     EJECT                                                                
056100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
056200     SKIP3                                                                
056300 01  -COPY WZ01SEND                                                       
056400     EJECT                                                                
056500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
056600     SKIP3                                                                
056700 01  SEND-AREA.                                                           
056800*    03  -COPY WZ01REQU  -PRE 3039-                                       
056900*    03  -COPY W30391I1  -PRE 3039-                                       
057000     EJECT                                                                
057100 LINKAGE SECTION.                                                         
057200                                                                          
057300*01  -COPY W0009   -PRE MSG-                                              
057400                                                                          
057500 01  2109-PCB                    PIC X.                                   
057600                                                                          
057700 01  AVSR-ALT-PCB                PIC X.                                   
057800     EJECT                                                                
057900                                                                          
058000*01  -COPY W0009   -PRE 4243-                                             
058100*01  -COPY W0009   -PRE 4243V-                                            
058200*01  -COPY W0009   -PRE 4297-                                             
058300*01  -COPY W0009   -PRE PRQRY-                                            
058400     SKIP2                                                                
058500     EJECT                                                                
058600*01  -COPY W0008   -PRE USEA-                                             
058700     05  FILLER                  PIC X.                                   
058800     SKIP2                                                                
058900*01  -COPY W0008   -PRE ORQM-                                             
059000     05  FILLER                  PIC X.                                   
059100     SKIP2                                                                
059200*01  -COPY W0008   -PRE ORQI-                                             
059300     05  FILLER                  PIC X.                                   
059400     EJECT                                                                
059500*01  -COPY W0008   -PRE ORQF-                                             
059600     05  FILLER                  PIC X.                                   
059700     EJECT                                                                
059800*01  -COPY W0008   -PRE ARTM-                                             
059900     05  FILLER                  PIC X.                                   
060000     EJECT                                                                
060100*01  -COPY W0008   -PRE WDB2-                                             
060200     05  FILLER                  PIC X.                                   
060300     EJECT                                                                
060400*01  -COPY W0008   -PRE WDB1-                                             
060500     05  FILLER                  PIC X.                                   
060600     EJECT                                                                
060700*01  -COPY W0008   -PRE WDB6-                                             
060800     05  FILLER                  PIC X.                                   
060900     EJECT                                                                
061000*01  -COPY W0008   -PRE WDB3-                                             
061100     05  FILLER                  PIC X.                                   
061200     EJECT                                                                
061300*01  -COPY W0008   -PRE WDR6-                                             
061400     05  FILLER                  PIC X.                                   
061500     EJECT                                                                
061600 01  PRIS-ARTC-PCB               PIC X.                                   
061700 01  PRIS-WDK7-PCB               PIC X.                                   
061800 01  PRIS-GMTA-PCB               PIC X.                                   
061900 01  PRIS-BETA-PCB               PIC X.                                   
062000 01  PRIS-GPRIA-PCB              PIC X.                                   
062100 01  PRIS-GPRIB-PCB              PIC X.                                   
062200 01  PRIS-COST-WDK6-PCB          PIC X.                                   
062300 01  PRIS-COST-WDK7-PCB          PIC X.                                   
062400 01  PRIS-COST-WDF1-PCB          PIC X.                                   
062500 01  PRIS-COST-9305-PCB          PIC X.                                   
062600 01  PRIS-COST-WDK72-PCB         PIC X.                                   
062700 01  PRIS-COST-WDB6-PCB          PIC X.                                   
062800 01  PRNO-3107-PCB               PIC X.                                   
062900 01  PRQU-WDG2-PCB               PIC X.                                   
063000 01  PRQU-WDC7-PCB               PIC X.                                   
063100 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
063200 01  AREG-WDK6-PCB               PIC X.                                   
063300 01  AREG-WDK7-PCB               PIC X.                                   
063400 01  ARTM-ARTM-PCB               PIC X.                                   
063500 01  DLEV-LEVF-PCB               PIC X.                                   
063600 01  DLEV-LEVG-PCB               PIC X.                                   
063700 01  DLEV-LEVA-PCB               PIC X.                                   
063800 01  DLEV-ARTS-PCB               PIC X.                                   
063900 01  DLEV-WDB6-PCB               PIC X.                                   
064000 01  SPAR-WDF8-PCB               PIC X.                                   
064100 01  SPAR-WDF8A-PCB              PIC X.                                   
064200 01  SPAR-WDK6-PCB               PIC X.                                   
064300 01  KAMP-ORDP-PCB               PIC X.                                   
064400 01  KAMP-ZZAC-PCB               PIC X.                                   
064500 01  KAMP-WDM2-PCB               PIC X.                                   
064600 01  KERS-ARTC-PCB               PIC X.                                   
064700 01  KERS-ERSA-PCB               PIC X.                                   
064800 01  NDCA-USEA-PCB               PIC X.                                   
064900 01  NDCA-WDK7-PCB               PIC X.                                   
065000 01  NDCA-WDL6-PCB               PIC X.                                   
065100 01  NDCA-WDB6-PCB               PIC X.                                   
065300 01  SDCA-ARTS-PCB               PIC X.                                   
065400 01  SDCA-WDB6-PCB               PIC X.                                   
065500 01  SDCA-WDK9-PCB               PIC X.                                   
065600 01  SDCA-WDR6-PCB               PIC X.                                   
065700 01  SDCA-WDK6-PCB               PIC X.                                   
065800 01  SDCA-WDQ4B-PCB              PIC X.                                   
065900 01  SDCA-WDQ2-PCB               PIC X.                                   
066000 01  SDCA-WDQ4-PCB               PIC X.                                   
066100 01  SDCA-WDB6-2-PCB             PIC X.                                   
066200 01  SDCA-WDK6-2-PCB             PIC X.                                   
066300 01  SDCA-WDK7-2-PCB             PIC X.                                   
066400 01  SDCA-WDK7-3-PCB             PIC X.                                   
066500 01  CDCA-ARTM-PCB               PIC X.                                   
066600 01  CDCA-INLB-PCB               PIC X.                                   
066700 01  CDCA-WDB2-PCB               PIC X.                                   
066800 01  CDCA-WDC1-PCB               PIC X.                                   
066900 01  RANS-XXKM-PCB               PIC X.                                   
067000 01  RANS-ARTM-PCB               PIC X.                                   
067100 01  RANS-ARTS-PCB               PIC X.                                   
067200     EJECT                                                                
067300 01  TPO1-ORDP-PCB               PIC X.                                   
067400 01  TPO1-ARTM-PCB               PIC X.                                   
067500 01  TPO1-ZZAC-PCB               PIC X.                                   
067600 01  TPO2-ORDP-PCB               PIC X.                                   
067700 01  TPO2-XXBU-PCB               PIC X.                                   
067800 01  TPO2-XXBV-PCB               PIC X.                                   
067900 01  TPO2-ARTM-PCB               PIC X.                                   
068000 01  TPO2-FILA-PCB               PIC X.                                   
068100 01  TPO2-XXBX-PCB               PIC X.                                   
068200 01  RELS-ORDP-PCB               PIC X.                                   
068300 01  RELS-FILA-PCB               PIC X.                                   
068400 01  RELS-ARTM-PCB               PIC X.                                   
068500 01  TIME-4437-PCB               PIC X.                                   
068600 01  AVSR-ORQI-PCB               PIC X.                                   
068700 01  AVSR-GMTB-PCB               PIC X.                                   
068800 01  AVSR-GMTC-PCB               PIC X.                                   
068900 01  AVSR-WDB2-PCB               PIC X.                                   
069000 01  AVSR-WDB6-PCB               PIC X.                                   
069100 01  TRAN-XXKB-PCB               PIC X.                                   
069200 01  KVAN-WDB2-PCB               PIC X.                                   
069300 01  KVAN-WDC1-PCB               PIC X.                                   
069400 01  XDCA-USEA-PCB               PIC X.                                   
069500 01  XDCA-WDB6-PCB               PIC X.                                   
069600 01  XDCA-WDK6-PCB               PIC X.                                   
069700 01  XDCA-WDK7-PCB               PIC X.                                   
069800 01  XDCA-WDK9-PCB               PIC X.                                   
069900 01  XDCA-WDL6-PCB               PIC X.                                   
070000 01  XDCA-WDQ4B-PCB              PIC X.                                   
070100 01  XDCA-WDQ2-PCB               PIC X.                                   
070200 01  XDCA-WDQ4-PCB               PIC X.                                   
070300 01  XDCA-WDR6-PCB               PIC X.                                   
070400 01  XDCA-WDB6-2-PCB             PIC X.                                   
070500 01  XDCA-WDK6-2-PCB             PIC X.                                   
070600 01  XDCA-WDK7-2-PCB             PIC X.                                   
070700 01  XDCA-WDK7-3-PCB             PIC X.                                   
070800     EJECT                                                                
070900 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB 2109-PCB PRQRY-PCB        
071000        4243-PCB 4243V-PCB                                                
071100        4297-PCB USEA-PCB ORQM-PCB ORQI-PCB ORQF-PCB                      
071200        ARTM-PCB WDB2-PCB WDB1-PCB WDB6-PCB WDB3-PCB                      
071300        WDR6-PCB                                                          
071400        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
071500        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
071600        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
071700        PRIS-COST-WDK6-PCB                                                
071800        PRIS-COST-WDK7-PCB                                                
071900        PRIS-COST-WDF1-PCB                                                
072000        PRIS-COST-9305-PCB                                                
072100        PRIS-COST-WDK72-PCB                                               
072200        PRIS-COST-WDB6-PCB                                                
072300        PRNO-3107-PCB                                                     
072400        PRQU-WDG2-PCB                                                     
072500        PRQU-WDC7-PCB                                                     
072600        PRQU-SJKO-WDK6-PCB                                                
072700        AREG-WDK6-PCB                                                     
072800        AREG-WDK7-PCB                                                     
072900        ARTM-ARTM-PCB                                                     
073000        DLEV-LEVF-PCB                                                     
073100        DLEV-LEVG-PCB                                                     
073200        DLEV-LEVA-PCB                                                     
073300        DLEV-ARTS-PCB                                                     
073400        DLEV-WDB6-PCB                                                     
073500        SPAR-WDF8-PCB                                                     
073600        SPAR-WDF8A-PCB                                                    
073700        SPAR-WDK6-PCB                                                     
073800        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
073900        KERS-ARTC-PCB KERS-ERSA-PCB                                       
074000        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
074200        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
074300        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
074400        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
074500        SDCA-WDK7-3-PCB                                                   
074600        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
074700        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
074800        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
074900        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
075000        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
075100        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
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
076200     EJECT                                                                
076300                                                                          
076400     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB 2109-PCB PRQRY-PCB        
076500        4243-PCB 4243V-PCB                                                
076600        4297-PCB USEA-PCB ORQM-PCB ORQI-PCB ORQF-PCB                      
076700        ARTM-PCB WDB2-PCB WDB1-PCB WDB6-PCB WDB3-PCB                      
076800        WDR6-PCB                                                          
076900        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
077000        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
077100        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
077200        PRIS-COST-WDK6-PCB                                                
077300        PRIS-COST-WDK7-PCB                                                
077400        PRIS-COST-WDF1-PCB                                                
077500        PRIS-COST-9305-PCB                                                
077600        PRIS-COST-WDK72-PCB                                               
077700        PRIS-COST-WDB6-PCB                                                
077800        PRNO-3107-PCB                                                     
077900        PRQU-WDG2-PCB                                                     
078000        PRQU-WDC7-PCB                                                     
078100        PRQU-SJKO-WDK6-PCB                                                
078200        AREG-WDK6-PCB                                                     
078300        AREG-WDK7-PCB                                                     
078400        ARTM-ARTM-PCB                                                     
078500        DLEV-LEVF-PCB                                                     
078600        DLEV-LEVG-PCB                                                     
078700        DLEV-LEVA-PCB                                                     
078800        DLEV-ARTS-PCB                                                     
078900        DLEV-WDB6-PCB                                                     
079000        SPAR-WDF8-PCB                                                     
079100        SPAR-WDF8A-PCB                                                    
079200        SPAR-WDK6-PCB                                                     
079300        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
079400        KERS-ARTC-PCB KERS-ERSA-PCB                                       
079500        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
079700        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
079800        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
079900        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
080000        SDCA-WDK7-3-PCB                                                   
080100        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
080200        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
080300        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
080400        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
080500        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
080600        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
080700        TIME-4437-PCB                                                     
080800        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
080900        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
081000        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
081100        XDCA-USEA-PCB                                                     
081200        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
081300        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
081400        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
081500        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
081600        XDCA-WDK7-3-PCB.                                                  
081700     EJECT                                                                
081800                                                                          
081900     PERFORM IMS-GET-MSG                                                  
082000     IF SEGMENT-FINNS                                                     
082100        PERFORM A-INIT                                                    
082200        PERFORM B-KOLLA-NYCKLAR                                           
082300        IF ALLT-OK                                                        
082400           PERFORM J-KOLLA-BEHORIGHET                                     
082500           IF ALLT-OK                                                     
082600              PERFORM C-KOLLA-ATT-ORDER-FINNS                             
082700              IF ALLT-OK                                                  
082800                 PERFORM D-FORMELL-KONTROLL                               
082900                 IF ALLT-OK                                               
083000                    PERFORM E-BEHANDLA-RADER                              
083100                 END-IF                                                   
083200              END-IF                                                      
083300           END-IF                                                         
083400        END-IF                                                            
083500        IF ALLT-OK                                                        
083600           IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+' OR SVARSBILD        
083700              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
083800                PERFORM I-SKICKA-PRISFRAGA                                
083900              END-IF                                                      
084000              IF BIPA-JA AND NOT SVARSBILD                                
084100                 PERFORM H-STARTA-BIPACKNINGEN                            
084200              ELSE                                                        
084300                 PERFORM F-HOPPA-TILL-SVARSBILD                           
084400              END-IF                                                      
084500           ELSE                                                           
084600              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
084700                PERFORM I-SKICKA-PRISFRAGA                                
084800              END-IF                                                      
084900              PERFORM G-VISA-TOM-SIDA                                     
085000           END-IF                                                         
085100        END-IF                                                            
085200        IF HOPP = NEJ                                                     
085300        AND HOPP-TILL-0504     = NEJ                                      
085400           PERFORM Z-FINIT-INSERT-MSG                                     
085500        END-IF                                                            
085600     END-IF                                                               
085700     MOVE +0 TO RETURN-CODE                                               
085800     GOBACK                                                               
085900     .                                                                    
086000     EJECT                                                                
086100 A-INIT SECTION.                                                          
086200                                                                          
086300     MOVE SPACE                TO MED-IDMFSFEL                            
086400                                                                          
086500     IF MSG-DUBBLA-TRANSKODER                                             
086600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I24201                 
086700       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
086800       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
086900     ELSE                                                                 
087000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I24201                  
087100       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
087200       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
087300     END-IF                                                               
087400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
087500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
087600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
087700     IF W-IDTRANS = '4243' AND MSG-KDTRANS-1  = 'W4T242U '                
087800        MOVE JA TO SVARSBILD-SW                                           
087900     END-IF                                                               
088000     MOVE LOW-VALUE            TO MSG-AREA                                
088100     MOVE 'W4O24201'           TO MFS-IDMOD                               
088200     MOVE '4242'               TO MOD-IDTRANS                             
088300     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
088400                                  MOD-TEMFSINF                            
088500                                                                          
088600     IF NOT EGEN-MID  AND  NOT SVARSBILD                                  
088700       MOVE NEJ  TO ALLT-SW                                               
088800       PERFORM S20-WRONG-PICTURE-MESSAGE                                  
088900*      MOVE SPACE              TO MFS-KDTRTYP                             
089000*      MOVE '7'                TO MFS-IDPFK                               
089100     END-IF                                                               
089200     IF ENGLISH-TEXT                                                      
089300       MOVE 'GB '              TO MED-IDSKYLT                             
089400     ELSE                                                                 
089500       MOVE 'S  '              TO MED-IDSKYLT                             
089600     END-IF                                                               
089700                                                                          
090200     PERFORM AA-NOLLA-WOPS-TABELL                                         
090300     .                                                                    
090400     EJECT                                                                
090500 AA-NOLLA-WOPS-TABELL SECTION.                                            
090600                                                                          
090700     MOVE +1                   TO WS-INDEX-WOPS                           
090800     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
090900        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
091000        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
091100        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
091200        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
091300        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
091400        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
091500        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
091600        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
091700        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
091800        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
091900        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
092000        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
092100        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
092200        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
092300        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
092400        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
092500                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
092600        ADD +1                 TO WS-INDEX-WOPS                           
092700     END-PERFORM                                                          
092800                                                                          
092900     MOVE +1                   TO WS-INDEX-WOPS                           
093000     .                                                                    
093100     EJECT                                                                
093200 B-KOLLA-NYCKLAR SECTION.                                                 
093300                                                                          
093400     MOVE MID-IDDISTR        TO WS-IDDISTR                                
093500     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
093600                                                                          
093700     MOVE MID-IDKUNDNR      TO WS-IDKUNDNR                                
093800     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
093900                                                                          
094000     MOVE MID-IDORDNR5      TO WS-IDORDNR                                 
094100     INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                   
094200                                                                          
094300     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
094400        MOVE WS-IDDISTR           TO W-IDDISTR                            
094500     ELSE                                                                 
094600        MOVE NEJ                  TO ALLT-SW                              
094700        MOVE ZERO                 TO WS-IDDISTR                           
094800     END-IF                                                               
094900                                                                          
095000     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
095100     IF DIST79-DEALER-PRICE                                               
095200        IF ENGLISH-TEXT                                                   
095300           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
095400        ELSE                                                              
095500           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
095600        END-IF                                                            
095700     ELSE                                                                 
095800        MOVE SPACES               TO MOD-TEDDI                            
095900     END-IF                                                               
096000                                                                          
096100     IF WS-IDKUNDNR NUMERIC                                               
096200        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
096300     ELSE                                                                 
096400        MOVE NEJ                  TO ALLT-SW                              
096500     END-IF                                                               
096600                                                                          
096700     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
096800        MOVE WS-IDORDNR           TO WS-NUM-7                             
096900        MOVE WS-NUM-7             TO W-IDKUNDRF                           
097000     ELSE                                                                 
097100        MOVE NEJ                  TO ALLT-SW                              
097200     END-IF                                                               
097300                                                                          
097400     IF NOT ALLT-OK                                                       
097500        IF SVARSBILD                                                      
097600          MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4243'          
097700                                  TO FELTEXT                              
097800          CALL ABEND USING RKOD-ABEND                                     
097900        END-IF                                                            
098000        MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                         
098100     END-IF                                                               
098200     EJECT                                                                
098300                                                                          
098400     IF GODK-MID OR ALLT-OK                                               
098500       MOVE WS-IDDISTR              TO MOD-IDDISTR                        
098600       INSPECT MOD-IDDISTR REPLACING LEADING ZERO BY SPACE                
098700                                                                          
098800       IF WS-IDKUNDNR = ZERO                                              
098900         MOVE '     0'              TO MOD-IDKUNDNR                       
099000       ELSE                                                               
099100         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR                       
099200         INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE             
099300       END-IF                                                             
099400                                                                          
099500       MOVE WS-IDORDNR              TO MOD-IDORDNR5                       
099600       INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
099700                                                                          
099800       IF W-IDTRANS = '4241' AND NOT ALLT-OK                              
099900          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR                        
100000                                       MOD-IDKUNDNR                       
100100                                       MOD-IDORDNR5                       
100200       END-IF                                                             
100300                                                                          
100400     ELSE                                                                 
100500       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR                        
100600                                       MOD-IDKUNDNR                       
100700                                       MOD-IDORDNR5                       
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
101200                                                                          
101300     PERFORM IMS-01-GHU-ORQI-WDQ201                                       
101400     IF SEGMENT-FINNS                                                     
101500                                                                          
101700        IF OHUV-FLKLAR = JA                                               
101800           MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                      
101900           MOVE NEJ                  TO ALLT-SW                           
102000        ELSE                                                              
102100           IF OHUV-IDSYSTEM NOT  = '4241'                                 
102200              MOVE ERR-FEL-BILDSERIE TO MED-IDMFSFEL                      
102300              MOVE NEJ               TO ALLT-SW                           
102400           ELSE                                                           
102500              IF OHUV-IDUSER NOT = MSG-SIGNON-USERID                      
102600                 MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                   
102700                 MOVE NEJ               TO ALLT-SW                        
102800              ELSE                                                        
102900                 MOVE OHUV-KDORDKL      TO MOD-KDORDKL                    
103000                 PERFORM IMS-03-GNP-ORQI-WDQ212                           
103100                 MOVE ARB-KDFRAKT       TO MOD-KDFRAKT                    
103100                 PERFORM CA-HAMTA-KUND                                    
103200                 PERFORM CB-FIXA-LOKAL-TID                                
103300                 PERFORM CC-HAMTA-WDB6-INFO                               
103400              END-IF                                                      
103500           END-IF                                                         
103600        END-IF                                                            
103700     ELSE                                                                 
103800        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
103900        MOVE NEJ                     TO ALLT-SW                           
104000     END-IF                                                               
104100                                                                          
104200     IF MFS-FIRST AND ALLT-OK                                             
104300        MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                
104400        PERFORM MFS-RENSA-MOD-RADER                                       
104500        MOVE NEJ                    TO ALLT-SW                            
104600     END-IF                                                               
104700     .                                                                    
104800     EJECT                                                                
104900                                                                          
107600 CA-HAMTA-KUND       SECTION.                                             
107700     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
108000     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
108100     PERFORM IMS-GU-WDB201                                                
108200                                                                          
108200     IF OHUV-KDORDKL > 1                                                  
108200                                                                          
108200        MOVE +1 TO WS-INDEX                                               
108200        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
108200           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
108200                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
108200           ADD +1 TO WS-INDEX                                             
108200        END-PERFORM                                                       
108200                                                                          
108200     ELSE                                                                 
108200       IF OHUV-KDORDKL = 1                                                
108200                                                                          
108200          MOVE +1 TO WS-INDEX                                             
108200          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
108200             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
108200                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
108200             ADD +1 TO WS-INDEX                                           
108200          END-PERFORM                                                     
108200                                                                          
108200       ELSE                                                               
108200         IF OHUV-KDORDKL = 0                                              
108200                                                                          
108200            MOVE +1 TO WS-INDEX                                           
108200            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
108200               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
108200                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
108200               ADD +1 TO WS-INDEX                                         
108200            END-PERFORM                                                   
108200                                                                          
108200         END-IF                                                           
108200       END-IF                                                             
108200     END-IF                                                               
108200     .                                                                    
108200     EJECT                                                                
105000 CB-FIXA-LOKAL-TID SECTION.                                               
105100                                                                          
105200     MOVE ALL '+'              TO MSGI-WMSGINIT                           
105300     MOVE '013'                TO MSGI-KDCALL                             
105400     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
105500     IF OHUV-IDDC-TVS = SPACE                                             
105600       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
105700     ELSE                                                                 
105800       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
105900     END-IF                                                               
106000                                                                          
106100     MOVE '4242'               TO MSGI-IDTRANS                            
106200     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
106300                                                                          
106400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
106500     .                                                                    
106600     EJECT                                                                
106700                                                                          
106800 CC-HAMTA-WDB6-INFO SECTION.                                              
106900                                                                          
107000     MOVE SPACE                TO CLDC-W411CLDC                           
107100     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
107200                                                                          
107300     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
107400     .                                                                    
107500     EJECT                                                                
109000 D-FORMELL-KONTROLL SECTION.                                              
109100                                                                          
109200     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
109300     IF OHUV-IDDC-TVS = SPACE                                             
109400       MOVE OHUV-IDDC-PRIM           TO ORFK-IDDC                         
109500     ELSE                                                                 
109600       MOVE OHUV-IDDC-TVS            TO ORFK-IDDC                         
109700     END-IF                                                               
109800     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
109900     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
110000     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
110100     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
110200     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
110300     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
110400     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
110500     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
110600     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
110700     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
110800     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
110900     MOVE +1                   TO WS-INDEX-MID                            
111000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
111100        MOVE NEJ               TO ORFK-FLINVEST(WS-INDEX-MID)             
111200                                                                          
111300        IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                            
111400           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN(WS-INDEX-MID)              
111500        ELSE                                                              
111600           IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                             
111700              MOVE JA          TO MID-FLRESTN(WS-INDEX-MID)               
111800           END-IF                                                         
111900           MOVE MID-FLRESTN(WS-INDEX-MID)                                 
112000                               TO ORFK-FLRESTN(WS-INDEX-MID)              
112100        END-IF                                                            
112200     EJECT                                                                
112300        IF MID-FLSLATT(WS-INDEX-MID) = ALL '+'                            
112400           MOVE JA             TO ORFK-FLSLATT(WS-INDEX-MID)              
112500        ELSE                                                              
112600           IF MID-FLSLATT(WS-INDEX-MID) = 'Y'                             
112700              MOVE JA          TO MID-FLSLATT(WS-INDEX-MID)               
112800           END-IF                                                         
112900           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
113000                               TO ORFK-FLSLATT(WS-INDEX-MID)              
113100        END-IF                                                            
113200                                                                          
113300        MOVE ZERO              TO ORFK-IDKONTO(WS-INDEX-MID)              
113400                                                                          
113500        MOVE MID-IDARTNR(WS-INDEX-MID)                                    
113600                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
113700        MOVE SPACE             TO ORFK-IDKST(WS-INDEX-MID)                
113800        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
113900                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
114000                                                                          
114100        MOVE OHUV-KDVRINFO     TO ORFK-KDVRINFO(WS-INDEX-MID)             
114200                                                                          
114300                                                                          
114400        MOVE MID-KVBEART(WS-INDEX-MID)                                    
114500                               TO ORFK-KVBEART(WS-INDEX-MID)              
114600                                                                          
114700                                                                          
114800        MOVE ALL '+'           TO ORFK-PRARTNTO(WS-INDEX-MID)             
114900        MOVE ALL '+'           TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
115000        MOVE ALL '+'       TO ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)         
115100        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
115200        MOVE MID-TITPO(WS-INDEX-MID)                                      
115300                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
115400        ADD +1                 TO WS-INDEX-MID                            
115500     END-PERFORM                                                          
115600     EJECT                                                                
115700                                                                          
115800     CALL W411ORFK USING ORFK-W411ORFK                                    
115900                         AREG-WDK6-PCB                                    
116000                         AREG-WDK7-PCB                                    
116100                                                                          
116200     MOVE +1                   TO WS-INDEX-MID                            
116300     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
116400        PERFORM DA-KOLLA-FEL-FK                                           
116500        ADD +1                 TO WS-INDEX-MID                            
116600     END-PERFORM                                                          
116700     IF SVARSBILD AND NOT ALLT-OK                                         
116800       MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4243'            
116900                               TO FELTEXT                                 
117000       CALL ABEND USING RKOD-ABEND                                        
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 DA-KOLLA-FEL-FK SECTION.                                                 
117500                                                                          
117600     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
117700        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
117800        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR(WS-INDEX-MID)        
117900        MOVE NEJ                 TO ALLT-SW                               
118000     END-IF                                                               
118100                                                                          
118200     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
118300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
118400        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSLATT-ATTR(WS-INDEX-MID)        
118500        MOVE NEJ                 TO ALLT-SW                               
118600     END-IF                                                               
118700                                                                          
118800     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
118900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
119000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
119100        MOVE NEJ                 TO ALLT-SW                               
119200     ELSE                                                                 
119300        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
119400                  AND NOT MFS-UPDATE                                      
119500          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
119600          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
119700          MOVE NEJ               TO ALLT-SW                               
119800        END-IF                                                            
119900     END-IF                                                               
120000                                                                          
120100     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
120200        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
120300        MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)         
120400        MOVE NEJ                 TO ALLT-SW                               
120500     END-IF                                                               
120600                                                                          
120700     EJECT                                                                
120800     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
120900        IF MED-IDMFSFEL = SPACE                                           
121000           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC AND                      
121100                   ORFK-KVBEART(WS-INDEX-MID) > ZERO                      
121200              IF NOT MFS-UPDATE                                           
121300                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
121400                 MOVE MFS-NUM-FAELT-FEL   TO                              
121500                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
121600                 MOVE NEJ             TO ALLT-SW                          
121700              END-IF                                                      
121800           ELSE                                                           
121900            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
122000            MOVE MFS-NUM-FAELT-FEL   TO                                   
122100                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
122200            MOVE NEJ                 TO ALLT-SW                           
122300           END-IF                                                         
122400        ELSE                                                              
122500           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
122600           MOVE MFS-NUM-FAELT-FEL   TO                                    
122700                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
122800           MOVE NEJ                 TO ALLT-SW                            
122900        END-IF                                                            
123000     END-IF                                                               
123100                                                                          
123200     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
123300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
123400        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR(WS-INDEX-MID)          
123500        MOVE NEJ                 TO ALLT-SW                               
123600     END-IF                                                               
123700     .                                                                    
123800     EJECT                                                                
123900 E-BEHANDLA-RADER SECTION.                                                
124000                                                                          
124100     MOVE +1 TO WS-INDEX-MID                                              
124200     MOVE NEJ                     TO TILLK-SW                             
124300                                     OBKR-SW                              
124400                                     CDC-MOVE-SW                          
124500     MOVE +0                      TO WS-IDPRQUES                          
124600                                                                          
124700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
124800        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
124900           PERFORM S02-RENSA-TILLK-TAB                                    
125000           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
125100                                  TO AREG-W411AREG-001                    
125200           PERFORM EC-BEHANDLA-RAD                                        
125300           MOVE JA                TO TILLK-SW                             
125400           MOVE NEJ               TO CDC-MOVE-SW                          
125500           MOVE +1                TO WS-INDEX-TILLK                       
125600           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
125700              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
125800              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
125900                 PERFORM ED-LAES-TILLK-DATA                               
126000                 PERFORM EC-BEHANDLA-RAD                                  
126100              END-IF                                                      
126200              ADD +1              TO WS-INDEX-TILLK                       
126300           END-PERFORM                                                    
126400        END-IF                                                            
126500        MOVE NEJ                  TO TILLK-SW                             
126600                                     OBKR-SW                              
126700                                     CDC-MOVE-SW                          
126800        ADD +1 TO WS-INDEX-MID                                            
126900     END-PERFORM                                                          
127000                                                                          
127100     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
127200       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
127300       MOVE +3                      TO PRNO-KDCALL                        
127400                                                                          
127500       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
127600     END-IF                                                               
127700                                                                          
127800     IF AVSR-IDDC(1) NOT = SPACE                                          
127900        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                    
128000                            AVSR-ORQI-PCB AVSR-GMTB-PCB                   
128100                            AVSR-GMTC-PCB AVSR-WDB2-PCB                   
128200                            AVSR-WDB6-PCB TRAN-XXKB-PCB                   
128300     END-IF                                                               
128400                                                                          
128500     IF AVSR-KDROPACK NOT = SPACE AND ZERO                                
128600        MOVE JA              TO BIPA-SW                                   
128700     END-IF                                                               
128800                                                                          
128900     MOVE JA                      TO ALLT-SW                              
129000     .                                                                    
129100     EJECT                                                                
129200 EC-BEHANDLA-RAD SECTION.                                                 
129300                                                                          
129400     PERFORM ECA-NOLLSTALL-OBKR                                           
129500     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
129600     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
129700     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
129800     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
129900                                                                          
130000     IF NOT TILLKOMMANDE-RAD                                              
130100        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
130200     END-IF                                                               
130300                                                                          
130400     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
130500     PERFORM ECJ-KOMPLETTERA-PRIS                                         
130600     PERFORM ECM-KOMPLETTERA-TPO1                                         
130700     PERFORM ECN-KOMPLETTERA-TPO2                                         
130800     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
130900     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
131000     IF KOLLA-ERS AND (DCS-CDC OR DCS-SDC)                                
131100        PERFORM ECZ-CHECK-KDERS-IN-DC                                     
131200     END-IF                                                               
131300     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
131400     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
131500     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
131600     PERFORM ECG-PREL-AVBOKNING-XDC                                       
131700     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
131800     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
131900     PERFORM ECR-PREL-AVBOKNING-CDC                                       
132000                                                                          
132100     EJECT                                                                
132200     IF NOT TILLKOMMANDE-RAD                                              
132300        IF SKRIV-OBKR                                                     
132400           PERFORM ECS-SKRIV-OBKR                                         
132500*          IF NOT OBKR-SKRIVEN OR                                         
132600           IF OBKR-SW = 'J'    OR                                         
132700              OBKR-SW = 'N'    OR                                         
132800              EGET-CL-RAD                                                 
132900              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
133000              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
133100           END-IF                                                         
133200        ELSE                                                              
133300           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
133400                               OR KAMP-FLKLAR = JA                        
133500                               OR RELS-FLKLAR = JA                        
133600              CONTINUE                                                    
133700           ELSE                                                           
133800              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
133900              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
134000           END-IF                                                         
134100        END-IF                                                            
134200     ELSE                                                                 
134300        IF SKRIV-OBKR                                                     
134400           PERFORM ECS-SKRIV-OBKR                                         
134500        END-IF                                                            
134600     END-IF                                                               
134700     .                                                                    
134800     EJECT                                                                
134900 ECA-NOLLSTALL-OBKR SECTION.                                              
135000                                                                          
135100     MOVE ZERO                 TO KVAN-KDORDBEK-UT                        
135200                                  DLEV-KDORDBEK-UT                        
135300                                  TPO1-KDORDBEK                           
135400                                  TPO2-KDORDBEK                           
135500                                  RELS-KDORDBEK                           
135600                                  KAMP-KDORDBEK                           
135700                                  STOR-KDORDBEK                           
135800                                  XDCA-KDORDBEK                           
135900*                                 NDCA-KDORDBEK                           
136000                                  SDCA-KDORDBEK                           
136100                                  SDCA-KDORDBEK-FIRST-SDC                 
136200                                  SDCA-KDORDBEK-SECOND-SDC                
136300                                  CDCA-KDORDBEK-UT                        
136400                                  SPAR-KDORDBEK                           
136500                                  KERS-KDERS                              
136600     IF NOT TILLKOMMANDE-RAD                                              
136700        MOVE ZERO              TO KERS-KDORDBEK                           
136800     ELSE                                                                 
136900        MOVE JA                TO OBKR-SW                                 
137000     END-IF                                                               
137100                                                                          
137200     MOVE JA                   TO ALLT-SW                                 
137300     MOVE NEJ                  TO EGET-CL-RAD-SW                          
137400                                  KOLLA-ERS-SW                            
137500                                                                          
137600     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
137700       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
137800       MOVE +7                          TO W-KDTPOTYP                     
137900     ELSE                                                                 
138000       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
138100         MOVE NEJ               TO ALLT-SW                                
138200         MOVE JA                TO OBKR-SW                                
138300       END-IF                                                             
138400     END-IF                                                               
138500                                                                          
138600     .                                                                    
138700     EJECT                                                                
138800 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
138900                                                                          
139000     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
139100     IF OHUV-IDDC-TVS = SPACE                                             
139200       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
139300     ELSE                                                                 
139400       MOVE OHUV-IDDC-TVS      TO ORAD-IDDC                               
139500     END-IF                                                               
139600     MOVE ORAD-IDDC            TO WS-IDDC                                 
139700     MOVE +0                   TO ORAD-ADLAGOMR                           
139800     MOVE +0                   TO ORAD-ADGANG                             
139900     MOVE +0                   TO ORAD-ADPLATS                            
140000     IF TILLKOMMANDE-RAD                                                  
140100        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
140200     ELSE                                                                 
140300        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
140400                               TO ORAD-IDARTNR                            
140500     END-IF                                                               
140600     MOVE +1                   TO ORAD-IDLOPNR                            
140700                                                                          
140800     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
140900        MOVE SPACE             TO ORAD-BERADREF                           
141000     ELSE                                                                 
141100        MOVE MID-BERADREF(WS-INDEX-MID)                                   
141200                               TO ORAD-BERADREF                           
141300     END-IF                                                               
141400     IF MID-BEVOLREF = SPACE                                              
141500        MOVE OHUV-BEKUNDRF     TO ORAD-BEVOLREF                           
141600     ELSE                                                                 
141700        MOVE MID-BEVOLREF      TO ORAD-BEVOLREF                           
141800     END-IF                                                               
141900     MOVE SPACE                TO ORAD-FLAKPLOC                           
142000                                                                          
142100     MOVE NEJ                  TO ORAD-FLINVEST                           
142200                                  ORAD-FLSDCLEV                           
142300     EJECT                                                                
142400     MOVE JA                   TO ORAD-FLOBTRAN                           
142500     IF TILLKOMMANDE-RAD                                                  
142600       IF DIST79-DEALER-PRICE                                             
142700          MOVE NEJ             TO ORAD-FLPRTILL                           
142800       ELSE                                                               
142900        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
143000           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
143100                               TO ORAD-FLPRTILL                           
143200        ELSE                                                              
143300           MOVE NEJ            TO ORAD-FLPRTILL                           
143400        END-IF                                                            
143500       END-IF                                                             
143600     ELSE                                                                 
143700        MOVE NEJ               TO ORAD-FLPRTILL                           
143800     END-IF                                                               
143900     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
144000        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
144100     ELSE                                                                 
144200        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
144300                               TO ORAD-FLRESTN                            
144400     END-IF                                                               
144500     IF TILLKOMMANDE-RAD                                                  
144600        MOVE JA                TO ORAD-FLTILLK                            
144700     ELSE                                                                 
144800        MOVE NEJ               TO ORAD-FLTILLK                            
144900     END-IF                                                               
145000     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
145100     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
145200     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
145300     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
145400     MOVE SPACE                TO ORAD-IDLEVNR                            
145500     MOVE +0                   TO ORAD-IDLOPNR-RO                         
145600     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
145700     EJECT                                                                
145800     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
145900     MOVE ZERO                 TO ORAD-IDSPECEMB                          
146000     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
146100     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
146200     MOVE OHUV-KDVRINFO        TO ORAD-KDDSP                              
146300     IF ORAD-KDDSP = +0                                                   
146400        MOVE +1                TO ORAD-KDDSP                              
146500     END-IF                                                               
146600     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
146700     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
146800        MOVE +0                TO ORAD-KDKVBRYT                           
146900     ELSE                                                                 
147000        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
147100                               TO ORAD-KDKVBRYT                           
147200     END-IF                                                               
147300     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
147400     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
147500     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
147600                                  TEST-KDPRODSL                           
147700     IF ORAD-KDORDING = +3                                                
147800       MOVE SPACE              TO ORAD-KDOI                               
147900     ELSE                                                                 
148000       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
148100         MOVE 'CD'             TO ORAD-KDOI                               
148200       ELSE                                                               
148300         MOVE 'DT'             TO ORAD-KDOI                               
148400       END-IF                                                             
148500     END-IF                                                               
148600     MOVE SPACE                TO ORAD-CLEARGROUP                         
148700                                                                          
148800     IF TILLKOMMANDE-RAD                                                  
148900       IF DIST79-DEALER-PRICE                                             
149000          MOVE SPACE           TO ORAD-KDPRTYP                            
149100       ELSE                                                               
149200        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
149300           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
149400                               TO ORAD-KDPRTYP                            
149500        ELSE                                                              
149600           MOVE SPACE          TO ORAD-KDPRTYP                            
149700        END-IF                                                            
149800       END-IF                                                             
149900     ELSE                                                                 
150000        MOVE SPACE             TO ORAD-KDPRTYP                            
150100     END-IF                                                               
150200     EJECT                                                                
150300     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
150400     IF OHUV-KDTPOTYP = +1 OR +2 OR +3 OR +4                              
150500        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
150600     ELSE                                                                 
150700        IF OHUV-KDTPOTYP = +0    AND                                      
150800                   MID-TITPO(WS-INDEX-MID) NOT = ALL '+'                  
150900           IF OHUV-IDKAMPRF > +0                                          
151000              MOVE +4          TO ORAD-KDTPOTYP                           
151100           ELSE                                                           
151200              MOVE +2          TO ORAD-KDTPOTYP                           
151300              IF ORAD-KDORDING = +3                                       
151400                 CONTINUE                                                 
151500              ELSE                                                        
151600                 MOVE +2       TO ORAD-KDORDING                           
151700              END-IF                                                      
151800           END-IF                                                         
151900        ELSE                                                              
152000           MOVE +0             TO ORAD-KDTPOTYP                           
152100        END-IF                                                            
152200     END-IF                                                               
152300     MOVE JA                   TO ORAD-FLORDING                           
152400     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
152500     IF TILLKOMMANDE-RAD                                                  
152600        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
152700                               TO ORAD-KVBEART                            
152800     ELSE                                                                 
152900        MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                      
153000        MOVE WS-NUM-6          TO ORAD-KVBEART                            
153100     END-IF                                                               
153200     EJECT                                                                
153300     MOVE +0                   TO ORAD-KVBEART-Q                          
153400     MOVE +0                   TO ORAD-KVPREAVB                           
153500     MOVE +0                   TO ORAD-KVPRERO                            
153600     MOVE +0                   TO ORAD-KVOKS-PREL                         
153700     MOVE +0                   TO ORAD-IDPRQUES                           
153800     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
153900     MOVE +0                   TO ORAD-RERAB                              
154000     MOVE SPACE                TO ORAD-KDVALISO                           
154100     MOVE SPACE                TO ORAD-KDVAT                              
154200     MOVE SPACE                TO ORAD-KDRAB                              
154300     MOVE SPACE                TO ORAD-BEART-VIPS                         
154400                                                                          
154500     IF TILLKOMMANDE-RAD                                                  
154600       IF DIST79-DEALER-PRICE                                             
154700           MOVE +0             TO ORAD-PRARTNTO                           
154800           MOVE TILK-PRARTNTO-LOC(WS-INDEX-TILLK)                         
154900                               TO ORAD-PRARTNTO-LOC                       
155000           MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                     
155100                               TO ORAD-PRARTNTO-LOCPREL                   
155200       ELSE                                                               
155300        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
155400           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
155500                               TO ORAD-PRARTNTO                           
155600           MOVE ZERO           TO ORAD-PRARTNTO-LOC                       
155700        ELSE                                                              
155800           MOVE +0             TO ORAD-PRARTNTO                           
155900           MOVE +0             TO ORAD-PRARTNTO-LOC                       
156000           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
156100        END-IF                                                            
156200       END-IF                                                             
156300     ELSE                                                                 
156400        MOVE +0                TO ORAD-PRARTNTO                           
156500        MOVE +0                TO ORAD-PRARTNTO-LOC                       
156600        MOVE +0                TO ORAD-PRARTNTO-LOCPREL                   
156700     END-IF                                                               
156800     MOVE +0                TO ORAD-PRBPRIS                               
156900     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
157000        MOVE MID-IDARTNR(WS-INDEX-MID) (11:1)                             
157100                               TO ORAD-REKSIFFR                           
157200     ELSE                                                                 
157300        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
157400     END-IF                                                               
157500     MOVE +0                   TO ORAD-RERF-RAD                           
157600     MOVE +0                   TO ORAD-KVSLATT                            
157700                                                                          
157800     EJECT                                                                
157900     IF TILLKOMMANDE-RAD                                                  
158000       IF DIST79-DEALER-PRICE                                             
158100         MOVE +0               TO ORAD-TIPRIS                             
158200       ELSE                                                               
158300        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
158400           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
158500                               TO ORAD-TIPRIS                             
158600        ELSE                                                              
158700           MOVE +0             TO ORAD-TIPRIS                             
158800        END-IF                                                            
158900       END-IF                                                             
159000     ELSE                                                                 
159100        MOVE +0                TO ORAD-TIPRIS                             
159200     END-IF                                                               
159300                                                                          
159400     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
159500     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
159600     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
159700     MOVE +0                   TO ORAD-TIRODAT                            
159800     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
159900        MOVE OHUV-TITPO        TO ORAD-TITPO                              
160000     ELSE                                                                 
160100        MOVE MID-TITPO(WS-INDEX-MID) TO WS-ALFA-6                         
160200        MOVE WS-NUM-6          TO ORAD-TITPO                              
160300     END-IF                                                               
160400     MOVE AREG-VKART           TO ORAD-VKART                              
160500     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
160600     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
160700     MOVE SPACE                TO ORAD-IDBIL                              
160800                                  ORAD-IDKLIENT                           
160900                                  ORAD-IDARBREF                           
161000                                  ORAD-IDVIN                              
161100                                                                          
161200     IF ORAD-KDORDKL = 1 AND                                              
161300        GMT-FLLDCKND = JA                                                 
161400        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
161500     ELSE                                                                 
161600        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
161700     END-IF                                                               
161800     MOVE +0                   TO ORAD-PRAVCOST                           
161900     .                                                                    
162000     EJECT                                                                
162100 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
162200                                                                          
162300     IF ALLT-OK                                                           
162400                                                                          
162500     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
162600     PERFORM IMS-10-GU-WLARTM-WDK901                                      
162700                                                                          
162800     IF SEGMENT-SAKNAS                                                    
162900        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
163000        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
163100     END-IF                                                               
163200                                                                          
163300     END-IF                                                               
163400     .                                                                    
163500     EJECT                                                                
163600 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
163700                                                                          
163800     IF ALLT-OK                                                           
163900                                                                          
164000     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
164100     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
164200     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
164300     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
164400     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
164500     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
164600     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
164700     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
164800     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
164900     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
165000     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
165100     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
165200     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
165300     MOVE ORAD-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
165400     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
165500     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
165600     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
165700     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
165800     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
165900                                                                          
166000     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
166100                                                                          
166200     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
166300     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
166400                                                                          
166500     IF KVAN-KDORDBEK-UT > +0                                             
166600        MOVE JA                    TO OBKR-SW                             
166700     END-IF                                                               
166800                                                                          
166900     END-IF                                                               
167000     .                                                                    
167100     EJECT                                                                
167200 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
167300                                                                          
167400     PERFORM S10-HAMTA-WDB6-INFO                                          
167500                                                                          
167600     IF ALLT-OK AND (DCS-CDC OR                                           
167700                    (DCS-SDC AND NOT DCS-CHINA))                          
167800                                                                          
167900     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
168000     MOVE OHUV-IDDC-TVS        TO DLEV-IDDC-IN                            
168100     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
168200     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
168300     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
168400     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
168500     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
168600     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
168700     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
168800     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
168900     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
169000     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
169100     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
169200     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
169300     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
169400     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
170200     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
170200                                                                          
170200     MOVE +1 TO WS-INDEX                                                  
170200     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
170200        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
170200                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
170200        ADD +1 TO WS-INDEX                                                
170200     END-PERFORM                                                          
170200                                                                          
170300     MOVE 1                    TO DLEV-KDCALL                             
170400     MOVE SPACE                TO DLEV-CLEARGROUP                         
170500     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
170600                                                                          
170700     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
170800                                       DLEV-LEVG-PCB                      
170900                                       DLEV-LEVA-PCB                      
171000                                       DLEV-ARTS-PCB                      
171100                                       DLEV-WDB6-PCB                      
171200                                       TPO2-FILA-PCB                      
171300                                                                          
171400*    JUSTERING FÖR DIREKTLEVERERAD TPO-RAD                                
171500     IF DLEV-KDORDBEK-UT = 95                                             
171600        IF DLEV-KDTPOTYP-IN = +2                                          
171700           IF OHUV-KDTPOTYP NOT = +2                                      
171800              MOVE 21             TO DLEV-KDORDBEK-UT                     
171900              MOVE SPACE          TO DLEV-IDLEVNR-UT                      
172000           ELSE                                                           
172100              PERFORM ECIA-KOLLA-TPODAT                                   
172200              MOVE +0             TO ORAD-KDTPOTYP                        
172300              MOVE ZERO           TO ORAD-TITPO                           
172400              MOVE 'CD'           TO DLEV-KDOI-UT                         
172500           END-IF                                                         
172600        END-IF                                                            
172700     END-IF                                                               
172800                                                                          
172900     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                                 
173000        MOVE JA                  TO OBKR-SW                               
173100        MOVE NEJ                 TO ALLT-SW                               
173200        MOVE ZERO                TO KVAN-KDORDBEK-UT                      
173300     ELSE                                                                 
173400        IF DLEV-KDORDBEK-UT = 95                                          
173500           MOVE JA               TO OBKR-SW                               
173600        END-IF                                                            
173700        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
173800           IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                             
173900             MOVE DLEV-IDDC-UT      TO ORAD-IDDC                          
174000                                       WS-IDDC                            
174100           END-IF                                                         
174200                                                                          
174300           MOVE AREG-ADLAGOMR    TO ORAD-ADLAGOMR                         
174400           MOVE AREG-ADGANG      TO ORAD-ADGANG                           
174500           MOVE AREG-ADPLATS     TO ORAD-ADPLATS                          
174600        END-IF                                                            
174700                                                                          
174800        MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
174900        MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)        
175000        MOVE DLEV-KVDAGAR-DIFF-UT TO                                      
175100                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
175200        MOVE DLEV-TISKEPPN-DDC-UT TO                                      
175300                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
175400                                                                          
175500        IF DLEV-FLSDCLEV-UT = JA                                          
175600          MOVE DLEV-IDDC-UT       TO ORAD-IDDC                            
175700                                     WS-IDDC                              
175800        ELSE                                                              
175900          MOVE DLEV-KDOI-UT       TO ORAD-KDOI                            
176000          MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                      
176100        END-IF                                                            
176200                                                                          
176300        MOVE DLEV-IDLEVNR-UT     TO ORAD-IDLEVNR                          
176400        MOVE DLEV-FLRESTN-UT     TO ORAD-FLRESTN                          
176500        MOVE DLEV-FLSDCLEV-UT    TO ORAD-FLSDCLEV                         
176600     END-IF                                                               
176700     IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                       
176800        AND ORAD-IDDC = WC-CDC-SE                                         
176900        MOVE OHUV-IDDISTR     TO TEST-IDDISTR                             
177000        IF DIST18-SKROT                                                   
177100          CONTINUE                                                        
177200        ELSE                                                              
177300          MOVE 26             TO DLEV-KDORDBEK-UT                         
177400          MOVE JA             TO OBKR-SW                                  
177500        END-IF                                                            
177600     END-IF                                                               
177700                                                                          
177800     END-IF                                                               
177900     .                                                                    
178000     EJECT                                                                
178100 ECIA-KOLLA-TPODAT  SECTION.                                              
178200                                                                          
178300     MOVE NEJ  TO SW-DDGS-TPO-OBKR71                                      
178400     IF OHUV-TITPO = ZERO                                                 
178500        PERFORM IMS-01-GHU-ORQI-WDQ201                                    
178600        MOVE ORAD-TITPO    TO OHUV-TITPO                                  
178700        PERFORM IMS-02-REPL-ORQI-WDQ201                                   
178800     ELSE                                                                 
178900        IF ORAD-TITPO NOT = OHUV-TITPO                                    
179000           MOVE OHUV-TITPO TO ORAD-TITPO                                  
179100           MOVE JA         TO SW-DDGS-TPO-OBKR71                          
179200        END-IF                                                            
179300     END-IF                                                               
179400     .                                                                    
179500     EJECT                                                                
179600 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
179700                                                                          
179800     IF ALLT-OK                                                           
179900                                                                          
180000     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
180100     MOVE ORAD-IDDC            TO KERS-IDDC                               
180200     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
180300     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
180400     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
180500     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
180600     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
180700     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
180800     MOVE AREG-KDERS           TO KERS-KDERS                              
180900     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
181000     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
181100     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
181200     MOVE AREG-KDUART          TO KERS-KDUART                             
181300     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
181400     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
181500     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
181600     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
181700     EJECT                                                                
181800     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
181900                         KERS-ARTC-PCB KERS-ERSA-PCB                      
182000                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
182100                                                                          
182200     IF KERS-KDORDBEK > ZERO   AND                                        
182300        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
182400          KERS-KDERS > 10 )                                               
182500        MOVE JA                  TO OBKR-SW                               
182600        MOVE NEJ                 TO ALLT-SW                               
182700     END-IF                                                               
182800                                                                          
182900     PERFORM S10-HAMTA-WDB6-INFO                                          
183000                                                                          
183010     IF KERS-KDORDBEK > +0                                                
183100        IF DCS-SDC OR DCS-CDC                                             
183200          IF AREG-KDERS = 11 OR 12 OR 17 OR                               
183300                          21 OR 22 OR 27                                  
183400             MOVE JA                TO KOLLA-ERS-SW                       
183500          ELSE                                                            
183600             IF AREG-KDERS = 14 OR 15 OR 18 OR 19 OR                      
183700                             24 OR 25 OR 28 OR 29                         
183800               MOVE JA              TO ALLT-SW                            
183900             END-IF                                                       
184000          END-IF                                                          
184100        END-IF                                                            
184200        IF DCS-NDC                                                        
184300          IF AREG-KDERS > 18                                              
184400            MOVE JA                 TO KOLLA-ERS-SW                       
184500          ELSE                                                            
184600            MOVE ZERO               TO KERS-KDORDBEK                      
184700            PERFORM S02-RENSA-TILLK-TAB                                   
184800            MOVE JA                 TO ALLT-SW                            
184900          END-IF                                                          
185000        END-IF                                                            
185010     END-IF                                                               
185100     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
185200        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
185300***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
185400***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
185500***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
185600***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
185700           CONTINUE                                                       
185800        ELSE                                                              
185900           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
186000           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
186100        END-IF                                                            
186200     END-IF                                                               
186300     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
186400        MOVE ZERO                TO DLEV-KDORDBEK-UT                      
186500     END-IF                                                               
186600                                                                          
186700     ELSE                                                                 
186800        MOVE +0                  TO KERS-KDERS                            
186900     END-IF                                                               
187000     .                                                                    
187100     EJECT                                                                
187200 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
187300                                                                          
187400     IF ALLT-OK OR KOLLA-ERS                                              
187500                                                                          
187600     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
187700     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
187800     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
187900     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
188000     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
188100     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
188200     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
188300     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
188400     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
188500     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
188600     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
188700     MOVE AREG-FLIART          TO SPAR-FLIART                             
188800     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
188900     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
189000     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
189100     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
189200     PERFORM S10-HAMTA-WDB6-INFO                                          
189300     IF DCS-DDC                                                           
189400       MOVE OHUV-IDDC-PRIM     TO SPAR-IDDC                               
189500     ELSE                                                                 
189600       MOVE ORAD-IDDC          TO SPAR-IDDC                               
189700     END-IF                                                               
189800     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
189900     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
190000     MOVE AREG-KDERS           TO SPAR-KDERS                              
190100     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
190200     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
190300     MOVE +1                   TO SPAR-KDORDBEH                           
190400     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
190500     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
190600     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
190700     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
190800     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
190900     MOVE AREG-KDUART          TO SPAR-KDUART                             
191000     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
191100     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
191200     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
191300     MOVE ORAD-TITPO           TO SPAR-TITPO                              
191400     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
191500     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
191600                                                                          
191700     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
191800                                       SPAR-WDF8A-PCB                     
191900                                       SPAR-WDK6-PCB                      
192000                                                                          
192100     IF SPAR-KDORDBEK > +0                                                
192200       MOVE JA                TO OBKR-SW                                  
192300       MOVE NEJ               TO ALLT-SW                                  
192400       IF SPAR-KDORDBEK = 51 OR 67 OR 58                                  
192500         MOVE NEJ             TO KOLLA-ERS-SW                             
192600         MOVE ZERO            TO KERS-KDORDBEK                            
192700         PERFORM S02-RENSA-TILLK-TAB                                      
192800       END-IF                                                             
192900       MOVE ZERO              TO XDCA-DAPUBL                              
192900*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411NDCA                         
193000        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
193100          MOVE 99999999       TO XDCA-DAPUBL                              
193200*         MOVE 99999999       TO NDCA-DAPUBL                              
193300        END-IF                                                            
193400       IF KVAN-KDORDBEK-UT > +0                                           
193500          MOVE +0             TO KVAN-KDORDBEK-UT                         
193600          MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                           
193700       END-IF                                                             
193800*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
193900*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
194000       IF SPAR-KDORDBEK = 54 AND NOT DCS-DDC                              
194100          MOVE JA             TO ALLT-SW                                  
194200       END-IF                                                             
194300       IF DLEV-KDORDBEK-UT > +0                                           
194400          MOVE +0             TO DLEV-KDORDBEK-UT                         
194500       END-IF                                                             
194600     END-IF                                                               
194700     IF AREG-KDSORT = 'SW'                                                
194800        MOVE 67                     TO SPAR-KDORDBEK                      
194900        MOVE JA                     TO OBKR-SW                            
195000        MOVE NEJ                    TO ALLT-SW                            
195100        IF KOLLA-ERS-SW = JA                                              
195200           MOVE NEJ             TO KOLLA-ERS-SW                           
195300           MOVE ZERO            TO KERS-KDORDBEK                          
195400           PERFORM S02-RENSA-TILLK-TAB                                    
195500        END-IF                                                            
195600     END-IF                                                               
195700                                                                          
195800     END-IF                                                               
195900     .                                                                    
196000     EJECT                                                                
196100 ECJ-KOMPLETTERA-PRIS SECTION.                                            
196200                                                                          
196300     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC= YES                        
196400                                                                          
196500     IF DIST79-DEALER-PRICE                                               
196600       IF WS-IDPRQUES                = +0                                 
196700          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
196800          MOVE +1                    TO PRNO-KDCALL                       
196900                                                                          
197000          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
197100                                                                          
197200          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
197300                                        WS-IDPRQUES                       
197400          MOVE +1                    TO PRQU-KDCALL                       
197500       ELSE                                                               
197600          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
197700          MOVE +2                    TO PRNO-KDCALL                       
197800                                                                          
197900          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
198000                                                                          
198100          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
198200                                        WS-IDPRQUES                       
198300          MOVE +2                    TO PRQU-KDCALL                       
198400       END-IF                                                             
198500                                                                          
198600       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
198700       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
198800       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
198900       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
199000       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
199100       MOVE 'N'                      TO PRQU-KDPRSTA                      
199200       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
199300       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
199400                                                                          
199500       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
199600       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
199700       PERFORM IMS-GU-WDB101                                              
199800       MOVE BET-KDVALISO             TO ORAD-KDVALISO                     
199900                                        PRQU-KDVALISO                     
200000                                                                          
200100       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
200200       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
200300       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
200400                                                                          
200500       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
200600                                          PRQU-WDC7-PCB                   
200700                                          PRQU-SJKO-WDK6-PCB              
200800                                                                          
200900       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
201000                                         WS-IDPRQUES                      
201100       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
201200       MOVE PRQU-PRARTNTO-LOCPREL    TO  ORAD-PRARTNTO-LOCPREL            
201300                                                                          
201400       IF ORAD-PRARTNTO-LOC NOT = +0                                      
201500         IF ORAD-KDPRTYP = SPACE                                          
201600           MOVE 'P'            TO ORAD-KDPRTYP                            
201700           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
201800         END-IF                                                           
201900       END-IF                                                             
202000                                                                          
202100     ELSE                                                                 
202200*        *NOT DIST79-DEALER-PRICE                                         
202300                                                                          
202400         PERFORM S10-HAMTA-WDB6-INFO                                      
202500                                                                          
202600         IF DCS-NDC OR DCS-SDC OR DCS-DDC OR                              
202700           (ORAD-KDTPOTYP = +0 AND AREG-KDUART = SPACE)                   
202800                                                                          
202900           IF ORAD-PRARTNTO NOT = +0                                      
203000              MOVE 2               TO PRIS-KDCALL                         
203100           ELSE                                                           
203200              MOVE 1               TO PRIS-KDCALL                         
203300           END-IF                                                         
203400         ELSE                                                             
203500           MOVE 2                  TO PRIS-KDCALL                         
203600         END-IF                                                           
203700                                                                          
203800         MOVE IDPGM                TO PRIS-IDPGM                          
203900         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
204000         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
204100         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
204200         MOVE ORAD-IDDC            TO PRIS-IDDC                           
204300         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
204400         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
204500         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
204600                                                                          
204700         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
204800                             PRIS-WDK7-PCB                                
204900                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
205000                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
205100                             PRIS-COST-WDK6-PCB                           
205200                             PRIS-COST-WDK7-PCB                           
205300                             PRIS-COST-WDF1-PCB                           
205400                             PRIS-COST-9305-PCB                           
205500                             PRIS-COST-WDK72-PCB                          
205600                             PRIS-COST-WDB6-PCB                           
205700                                                                          
205800         IF PRIS-KDSVAR = '2'                                             
205900           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
206000                                      TO FELTEXT                          
206100           CALL ABEND USING RKOD-ABEND                                    
206200         END-IF                                                           
206300                                                                          
206400         IF PRIS-KDCALL = 2                                               
206500           MOVE PRIS-KDVALISO     TO ORAD-KDVALISO                        
206600           IF ORAD-KDPRTYP = SPACE                                        
206700              MOVE 'P'            TO ORAD-KDPRTYP                         
206800              MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                          
206900           END-IF                                                         
207000         ELSE                                                             
207100           MOVE PRIS-PRARTNTO     TO ORAD-PRARTNTO                        
207200           MOVE PRIS-FLPRTILL     TO ORAD-FLPRTILL                        
207300           MOVE PRIS-KDPRTYP      TO ORAD-KDPRTYP                         
207400           MOVE PRIS-PRBPRIS      TO ORAD-PRBPRIS                         
207500           MOVE ORAD-TIREGDAT     TO ORAD-TIPRIS                          
207600           MOVE PRIS-KDVALISO     TO ORAD-KDVALISO                        
207700           MOVE PRIS-PRAVCOST     TO ORAD-PRAVCOST                        
207800         END-IF                                                           
207900     END-IF                                                               
208000     END-IF                                                               
208100     .                                                                    
208200     EJECT                                                                
208300 ECM-KOMPLETTERA-TPO1 SECTION.                                            
208400                                                                          
208500     PERFORM S10-HAMTA-WDB6-INFO                                          
208600                                                                          
208700     IF ALLT-OK AND (DCS-CDC OR                                           
208800                    (DCS-SDC AND NOT DCS-CHINA))                          
208900                                                                          
209000     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
209100     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
209200     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
209300     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
209400     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
209500     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
209600     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
209700     MOVE OHUV-IDKST           TO TPO1-IDKST                              
209800     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
209900     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
210000     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
210100     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
210200     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
210300     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
210400     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
210500     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
210600     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
210700     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
210800     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
210900     MOVE ORAD-TITPO           TO TPO1-TITPO                              
211000     IF ORAD-KDPRTYP = 'P'                                                
211100        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
211200        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
211300        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
211400        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
211500     ELSE                                                                 
211600        MOVE ZERO              TO TPO1-PRARTNTO                           
211700*                                 TPO1-PRARTNTO-LOC                       
211800*                                 TPO1-PRARTNTO-LOCPREL                   
211900        MOVE SPACE             TO TPO1-KDPRTYP                            
212000        MOVE NEJ               TO TPO1-FLPRTILL                           
212100     END-IF                                                               
212200     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
212300     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
212400     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
212500     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
212600     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
212700     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
212800     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
212900     MOVE +1                   TO TPO1-KDORDBEH                           
213000     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
213100     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
213200     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
213300     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
213400     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
213500                                                                          
213600     MOVE +0                   TO TPO1-KDORDBEK                           
213700     MOVE SPACE                TO TPO1-FLKLAR                             
213800                                                                          
213900     MOVE OHUV-KDORDTYP-LDC   TO TPO1-KDORDTYP-LDC                        
214000     MOVE OHUV-TIREPDAT       TO TPO1-TIREPDAT                            
214100     MOVE ORAD-IDKUNDRF-WIP   TO TPO1-IDKUNDRF-WIP                        
214200                                                                          
214300     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
214400                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
214500                                                                          
214600     IF TPO1-KDORDBEK > +0                                                
214700        MOVE JA                TO OBKR-SW                                 
214800        MOVE NEJ               TO ALLT-SW                                 
214900        MOVE WC-CDC-SE         TO ORAD-IDDC                               
215000        MOVE ORAD-IDDC         TO WS-IDDC                                 
215100        IF KVAN-KDORDBEK-UT > +0                                          
215200           MOVE +0             TO KVAN-KDORDBEK-UT                        
215300           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
215400        END-IF                                                            
215500        IF DLEV-KDORDBEK-UT > +0                                          
215600           MOVE +0             TO DLEV-KDORDBEK-UT                        
215700        END-IF                                                            
215800     ELSE                                                                 
215900        IF TPO1-FLKLAR = JA                                               
216000           MOVE NEJ            TO ALLT-SW                                 
216100           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
216200              MOVE ZERO        TO KERS-KDORDBEK                           
216300              PERFORM S02-RENSA-TILLK-TAB                                 
216400              MOVE NEJ         TO TILLK-SW                                
216500           END-IF                                                         
216600        END-IF                                                            
216700     END-IF                                                               
216800                                                                          
216900     END-IF                                                               
217000     .                                                                    
217100     EJECT                                                                
217200 ECN-KOMPLETTERA-TPO2 SECTION.                                            
217300                                                                          
217400     PERFORM S10-HAMTA-WDB6-INFO                                          
217500                                                                          
217600     IF DCS-SDC           AND                                             
217700        ORAD-KDORDKL  = 1 AND                                             
217800        ORAD-IDKAMPRF = 0 AND                                             
217900       (AREG-KDUART   = 'L' OR  AREG-KDUART = 'P')                        
218000                                                                          
218100       CONTINUE                                                           
218200     ELSE                                                                 
218300       IF ALLT-OK AND (DCS-CDC OR                                         
218400                      (DCS-SDC AND NOT DCS-CHINA))                        
218500                                                                          
218600       MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                          
218700       MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                         
218800       MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                         
218900       MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                          
219000       MOVE ORAD-BERADREF        TO TPO2-BERADREF                         
219100       MOVE AREG-IDANSK          TO TPO2-IDANSK                           
219200       MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                          
219300       MOVE OHUV-IDKST           TO TPO2-IDKST                            
219400       MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                         
219500       MOVE ORAD-KDDSP           TO TPO2-KDDSP                            
219600       MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                         
219700       MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                          
219800       MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                         
219900       MOVE ORAD-KDORDING        TO TPO2-KDORDING                         
220000       MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                          
220100       MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                         
220200       MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                         
220300       MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                        
220400       MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                         
220500       MOVE ORAD-TITPO           TO TPO2-TITPO                            
220600       MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                         
220700       MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                     
220800       MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                          
220900       MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                         
221000       MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                         
221100       MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                         
221200       MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                         
221300       MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                         
221400       MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                         
221500       MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                         
221600       MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                          
221700       MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                          
221800       MOVE AREG-KDUART          TO TPO2-KDUART                           
221900       MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                         
222000       MOVE +1                   TO TPO2-KDORDBEH                         
222100       MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                         
222200       MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                         
222300       MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                          
222400       MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                         
222500       MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                         
222600       MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                        
222700       MOVE ORAD-KVBEART         TO TPO2-KVBEART                          
222800                                                                          
222900       MOVE +0                   TO TPO2-KDORDBEK                         
223000       MOVE SPACE                TO TPO2-FLKLAR                           
223100                                                                          
223200       MOVE OHUV-KDORDTYP-LDC   TO TPO2-KDORDTYP-LDC                      
223300       MOVE OHUV-TIREPDAT       TO TPO2-TIREPDAT                          
223400       MOVE ORAD-IDKUNDRF-WIP   TO TPO2-IDKUNDRF-WIP                      
223500                                                                          
223600       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
223700                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
223800                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
223900                         TIME-4437-PCB                                    
224000                                                                          
224100       IF TPO2-KDORDBEK > +0                                              
224200          MOVE JA                TO OBKR-SW                               
224300          MOVE NEJ               TO ALLT-SW                               
224400          MOVE WC-CDC-SE         TO ORAD-IDDC                             
224500          MOVE ORAD-IDDC         TO WS-IDDC                               
224600          MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                         
224700          IF ORAD-KDTPOTYP = 6                                            
224800             IF ORAD-KDPRTYP NOT = 'P'                                    
224900                MOVE ZERO        TO ORAD-PRARTNTO                         
225000*                                   ORAD-PRARTNTO-LOC                     
225100*                                   ORAD-PRARTNTO-LOCPREL                 
225200                MOVE SPACE       TO ORAD-KDPRTYP                          
225300**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
225400                IF NOT DIST79-DEALER-PRICE                                
225500                  MOVE ZERO        TO ORAD-PRARTNTO-LOC                   
225600                  MOVE NEJ         TO ORAD-FLPRTILL                       
225700                END-IF                                                    
225800*************                                                             
225900             END-IF                                                       
226000          END-IF                                                          
226100       ELSE                                                               
226200          IF TPO2-FLKLAR = JA                                             
226300             MOVE NEJ            TO ALLT-SW                               
226400             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
226500                MOVE ZERO        TO KERS-KDORDBEK                         
226600                PERFORM S02-RENSA-TILLK-TAB                               
226700                MOVE NEJ         TO TILLK-SW                              
226800             END-IF                                                       
226900          END-IF                                                          
227000       END-IF                                                             
227100                                                                          
227200       END-IF                                                             
227300     END-IF                                                               
227400     .                                                                    
227500     EJECT                                                                
227600 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
227700                                                                          
227800     PERFORM S10-HAMTA-WDB6-INFO                                          
227900                                                                          
228000     IF ALLT-OK AND (DCS-CDC OR                                           
228100                    (DCS-SDC AND NOT DCS-CHINA))                          
228200                                                                          
228300       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
228400       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
228500       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
228600       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
228700       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
228800       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
228900       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
229000       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
229100       MOVE OHUV-IDKST         TO KAMP-IDKST                              
229200       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
229300       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
229400       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
229500       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
229600       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
229700       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
229800       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
229900       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
230000       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
230100       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
230200       MOVE ORAD-TITPO         TO KAMP-TITPO                              
230300       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
230400          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
230500          MOVE ORAD-DEAL-PR-LINE                                          
230600                               TO KAMP-DEAL-PR-LINE                       
230700          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
230800          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
230900       ELSE                                                               
231000          MOVE ORAD-DEAL-PR-LINE                                          
231100                               TO KAMP-DEAL-PR-LINE                       
231200          MOVE ZERO            TO KAMP-PRARTNTO                           
231300          MOVE SPACE           TO KAMP-KDPRTYP                            
231400          MOVE NEJ             TO KAMP-FLPRTILL                           
231500       END-IF                                                             
231600       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
231700       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
231800       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
231900       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
232000       MOVE ORAD-IDDC          TO KAMP-IDDC                               
232100       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
232200       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
232300       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
232400       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
232500       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
232600       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
232700       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
232800                                                                          
232900       MOVE +0                 TO KAMP-KDORDBEK                           
233000       MOVE SPACE              TO KAMP-FLKLAR                             
233100       EJECT                                                              
233200                                                                          
233300       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
233400                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
233500                                                                          
233600       IF KAMP-KDORDBEK > +0                                              
233700          MOVE JA              TO OBKR-SW                                 
233800          MOVE NEJ             TO ALLT-SW                                 
233900          MOVE WC-CDC-SE       TO ORAD-IDDC                               
234000          MOVE ORAD-IDDC       TO WS-IDDC                                 
234100          IF KVAN-KDORDBEK-UT > +0                                        
234200             MOVE +0           TO KVAN-KDORDBEK-UT                        
234300             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
234400          END-IF                                                          
234500          IF DLEV-KDORDBEK-UT > +0                                        
234600             MOVE +0           TO DLEV-KDORDBEK-UT                        
234700          END-IF                                                          
234800       ELSE                                                               
234900          IF KAMP-FLKLAR = JA                                             
235000             MOVE NEJ          TO ALLT-SW                                 
235100             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
235200                MOVE ZERO      TO KERS-KDORDBEK                           
235300                PERFORM S02-RENSA-TILLK-TAB                               
235400                MOVE NEJ       TO TILLK-SW                                
235500             END-IF                                                       
235600          END-IF                                                          
235700       END-IF                                                             
235800                                                                          
235900     END-IF                                                               
236000     .                                                                    
236100     EJECT                                                                
236200 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
236300                                                                          
236400       IF ALLT-OK AND W-KDORDBEK = 56                                     
236500                                                                          
236600         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
236700         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
236800         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
236900         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
237000         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
237100         MOVE AREG-IDANSK          TO RELS-IDANSK                         
237200         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
237300         MOVE OHUV-IDKST           TO RELS-IDKST                          
237400         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
237500         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
237600         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
237700         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
237800         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
237900         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
238000         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
238100         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
238200         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
238300         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
238400         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
238500         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
238600         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
238700         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
238800         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
238900         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
239000         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
239100         EJECT                                                            
239200         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
239300         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
239400         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
239500         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
239600         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
239700         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
239800         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
239900         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
240000         MOVE AREG-KDUART          TO RELS-KDUART                         
240100         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
240200         MOVE +1                   TO RELS-KDORDBEH                       
240300         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
240400         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
240500         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
240600         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
240700         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
240800         MOVE 0                    TO RELS-KVQPACK-1                      
240900         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
241000         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
241100                                                                          
241200         MOVE SPACE                TO RELS-FLKLAR                         
241300                                                                          
241400         MOVE OHUV-KDORDTYP-LDC   TO RELS-KDORDTYP-LDC                    
241500         MOVE OHUV-TIREPDAT       TO RELS-TIREPDAT                        
241600         MOVE ORAD-IDKUNDRF-WIP   TO RELS-IDKUNDRF-WIP                    
241700                                                                          
241800         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
241900                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
242000                                                                          
242100         PERFORM ECTA-ANDRA-WDC711                                        
242200         IF RELS-KDORDBEK > +0                                            
242300            MOVE JA                TO OBKR-SW                             
242400            MOVE NEJ               TO ALLT-SW                             
242500            MOVE WC-CDC-SE         TO ORAD-IDDC                           
242600            MOVE ORAD-IDDC         TO WS-IDDC                             
242700         ELSE                                                             
242800            IF RELS-FLKLAR = JA                                           
242900               MOVE NEJ            TO ALLT-SW                             
243000               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
243100                  MOVE ZERO        TO KERS-KDORDBEK                       
243200                  PERFORM S02-RENSA-TILLK-TAB                             
243300               END-IF                                                     
243400            END-IF                                                        
243500         END-IF                                                           
243600                                                                          
243700       MOVE +0              TO W-KDORDBEK                                 
243800       MOVE SPACE           TO RELS-FLKLAR                                
243900                                                                          
244000       END-IF                                                             
244100     .                                                                    
244200     EJECT                                                                
244300 ECTA-ANDRA-WDC711 SECTION.                                               
244400                                                                          
244500     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
244600       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
244700         INITIALIZE PRQU-W335PRQU                                         
244800         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
244900         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
245000         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
245100         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
245200         MOVE 6                  TO PRQU-KDCALL                           
245300         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
245400                                            PRQU-WDC7-PCB                 
245500                                            PRQU-SJKO-WDK6-PCB            
245600         MOVE 'N'                TO ORAD-FLPRTILL                         
245700       END-IF                                                             
245800     END-IF                                                               
245900     .                                                                    
246000     EJECT                                                                
246100                                                                          
246200 ECG-PREL-AVBOKNING-XDC SECTION.                                          
246300                                                                          
246400     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
246500                                                                          
246600       PERFORM S10-HAMTA-WDB6-INFO                                        
246700       IF DCS-NDC                                                         
246800                                                                          
247000           MOVE JA TO ALLT-SW                                             
247100           PERFORM ECGX-PREL-AVBOKNING-XDC                                
247300*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
247400*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
247500*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
247600*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
247700*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
247800*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
247900*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
248000*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
248100*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
248200*        MOVE OHUV-IDDC-TVS        TO NDCA-IDDC-TVS                       
248300*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
248400*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
248500*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
248600*        MOVE AREG-KDERS           TO NDCA-KDERS                          
248700*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
248800*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
248900*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
249000*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
249100*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
249200*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
249300*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
249400*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
249500*        MOVE ORAD-VKART           TO NDCA-VKART                          
249600*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
249700*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
249800*        MOVE +2                   TO NDCA-KDCALL                         
249900*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
250000*                                     NDCA-XDK7-KVOKS-DAG                 
250100*                                     NDCA-XDK7-KVOKS-BULK                
250200*                                                                         
250300*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
250400*                                          NDCA-USEA-PCB                  
250500*                                          NDCA-WDK7-PCB                  
250600*                                          NDCA-WDL6-PCB                  
250700*                                          NDCA-WDB6-PCB                  
250800*                                          NDCA-XDK7-W411XDK7             
250900*                                                                         
251100*          PERFORM ECGX-CHECK-DIFF                                        
251300         IF XDCA-KDORDBEK > ZERO                                          
251400           IF SPAR-FLPUBCDC = YES                                         
251500*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE            
251600*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
251700*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
251800              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
251900                 AND XDCA-DAPUBL > ZERO                                   
252000                 MOVE ZERO TO SPAR-KDORDBEK                               
252100              ELSE                                                        
252200                 MOVE ZERO TO XDCA-KDORDBEK                               
252300              END-IF                                                      
252400           END-IF                                                         
252500           IF KOLLA-ERS                                                   
252600              IF XDCA-KVPREAVB > 0                                        
252700                MOVE ZERO            TO KERS-KDORDBEK                     
252800                PERFORM S02-RENSA-TILLK-TAB                               
252900                MOVE ZERO            TO SPAR-KDORDBEK                     
253000              ELSE                                                        
253100                MOVE ZERO            TO XDCA-KDORDBEK                     
253200              END-IF                                                      
253300           ELSE                                                           
253400             IF XDCA-KDORDBEK = 15                                        
253500                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
253600                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
253700                END-IF                                                    
253800                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
253900                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
254000                END-IF                                                    
254100                IF SDCA-KDORDBEK = 15                                     
254200                   MOVE ZERO         TO SDCA-KDORDBEK                     
254300                END-IF                                                    
254400             END-IF                                                       
254500             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
254600                 MOVE ZERO           TO XDCA-KDORDBEK                     
254700             END-IF                                                       
254800           END-IF                                                         
254900           MOVE JA                   TO OBKR-SW                           
255000         ELSE                                                             
255100           IF KOLLA-ERS    OR                                             
255200             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
255300             IF XDCA-KVPREAVB > 0                                         
255400               MOVE ZERO             TO KERS-KDORDBEK                     
255500               PERFORM S02-RENSA-TILLK-TAB                                
255600               MOVE ZERO             TO SPAR-KDORDBEK                     
255700             ELSE                                                         
255800               MOVE JA               TO OBKR-SW                           
255900             END-IF                                                       
255900           ELSE                                                           
255900             IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                 
255900                MOVE ZERO             TO SPAR-KDORDBEK                    
255900             END-IF                                                       
255900           END-IF                                                         
256100           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
256200*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
256300*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
256400              MOVE 0  TO SPAR-KDORDBEK                                    
256500              MOVE JA  TO ALLT-SW                                         
256600              MOVE NEJ TO OBKR-SW                                         
256700           END-IF                                                         
256800         END-IF                                                           
256900         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
257000         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
257100         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
257200         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
257300         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
257400         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
257500         MOVE XDCA-KDOI              TO ORAD-KDOI                         
257600         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
257700         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
257800         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
257900         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
258000         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
258100         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
258200         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
258300         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
258400         MOVE NEJ                    TO ALLT-SW                           
258500       END-IF                                                             
258600                                                                          
258700     END-IF                                                               
258800     .                                                                    
258900     EJECT                                                                
259000                                                                          
259100 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
259200                                                                          
259300     IF ALLT-OK OR KOLLA-ERS                                              
259400                                                                          
259500       PERFORM S10-HAMTA-WDB6-INFO                                        
259600                                                                          
259700       IF DCS-NDC                                                         
259800                                                                          
259900* XDCA-INPUT                                                              
260000         MOVE +1 TO WS-INDEX                                              
260000         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
260000           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
260000                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
260000           ADD +1 TO WS-INDEX                                             
260000         END-PERFORM                                                      
260000                                                                          
260100         MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                       
260200         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
260300         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
260400         MOVE ORAD-IDDC            TO XDCA-IDDC                           
260500         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
260600         MOVE AREG-KDERS           TO XDCA-KDERS                          
260700         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
260800         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
260900         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
261000         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
261100         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
261200         MOVE ORAD-VKART           TO XDCA-VKART                          
261300         MOVE +1                   TO XDCA-KDCALL                         
261400                                                                          
261500* XDCA-OUTPUT                                                             
261600         MOVE SPACE                TO XDCA-IDDC-OUT                       
261700                                      XDCA-IDDC-RO                        
261800                                      XDCA-KDARTURS                       
261900                                      XDCA-KDOI                           
262000                                      XDCA-CLEARGROUP                     
262100         MOVE ZERO                 TO XDCA-ADLAGOMR                       
262200                                      XDCA-ADGANG                         
262300                                      XDCA-ADPLATS                        
262400                                      XDCA-KDORDBEK                       
262500                                      XDCA-KVPREAVB                       
262600                                      XDCA-KVPRERO                        
262700                                      XDCA-TIREGDAT-OUT                   
262800                                      XDCA-TIREGTID-OUT                   
262900                                      XDCA-VKART-OUT                      
263000                                      XDCA-VKART-NTO                      
263100                                      XDCA-VLARTNTO                       
263200         MOVE ZERO                 TO                                     
263300                                      XDCA-KVOKS-DAG                      
263400                                      XDCA-KVOKS-BULK                     
263500         IF XDCA-DAPUBL NOT = 99999999                                    
263600            MOVE ZERO              TO XDCA-DAPUBL                         
263700         END-IF                                                           
263800                                                                          
263900         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
264000         XDCA-USEA-PCB                                                    
264100         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
264200         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
264300         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
264400         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
264500         XDCA-WDK7-3-PCB                                                  
264600                                                                          
264700* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
264800* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
264900         IF XDCA-KDARTURS = SPACE                                         
265000           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
265100         END-IF                                                           
265200         IF XDCA-VKART-NTO = ZERO                                         
265300           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
265400         END-IF                                                           
265500         IF XDCA-VLARTNTO = ZERO                                          
265600           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
265700         END-IF                                                           
265800       END-IF                                                             
265900     END-IF                                                               
266000     .                                                                    
266100     EJECT                                                                
266200 ECGX-CHECK-DIFF SECTION.                                                 
266300                                                                          
266400     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
266500     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
266600     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
266700     AND NDCA-ADGANG     = XDCA-ADGANG                                    
266800     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
266900     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
267000     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
267100     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
267200     AND NDCA-KDOI       = XDCA-KDOI                                      
267300     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
267400     AND NDCA-VKART      = XDCA-VKART-OUT                                 
267500     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
267600     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
267700     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
267800     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
267900     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
268000     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
268100         MOVE NEJ TO DIFF-FLSVAR                                          
268200     ELSE                                                                 
268300        MOVE JA           TO DIFF-FLSVAR                                  
268400     END-IF                                                               
268500                                                                          
268600* ORDER LOG INFO                                                          
268700     IF DIFF-FLSVAR = JA                                                  
268800       MOVE IDPGM         TO FIL-IDPGM                                    
268900       ACCEPT FIL-TIREGDAT FROM DATE                                      
269000       ACCEPT FIL-TIKLOCK FROM TIME                                       
269100       MOVE 1             TO FIL-IDSEKVNR                                 
269200       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
269300       MOVE 'A'           TO FIL-CT-IDVTYP                                
269400       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
269500                                                                          
269600*   ORDER LINE INFO                                                       
269700       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
269800       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
269900       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
270000       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
270100       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
270200       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
270300       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
270400       MOVE '4242'         TO DIFF-IDSYSTEM                               
270500                                                                          
270600*   NDCA INFO                                                             
270700       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
270800       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
270900       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
271000       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
271100       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
271200       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
271300       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
271400       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
271500       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
271600       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
271700       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
271800       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
271900       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
272000       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
272100       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
272200       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
272300                                                                          
272400*   XDCA INFO                                                             
272500       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
272600       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
272700       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
272800       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
272900       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
273000       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
273100       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
273200       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
273300       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
273400       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
273500       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
273600       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
273700       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
273800       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
273900       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
274000       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
274100                                                                          
274200       PERFORM IMS-ISRT-WDR601                                            
274300       IF SEGMENT-FINNS-REDAN                                             
274400          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
274500             ADD 1 TO FIL-IDSEKVNR                                        
274600             PERFORM IMS-ISRT-WDR601                                      
274700          END-PERFORM                                                     
274800       END-IF                                                             
274900     END-IF                                                               
275000     .                                                                    
275100     EJECT                                                                
275200 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
275300                                                                          
275400     MOVE 1    TO WS-IXDCCLEAR                                            
275500                                                                          
275600     IF ALLT-OK OR KOLLA-ERS                                              
275700                                                                          
275800       PERFORM S10-HAMTA-WDB6-INFO                                        
275900                                                                          
276000       IF DCS-SDC                                                         
276100                                                                          
276200         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
276300         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
276400         MOVE NEJ                  TO SDCA-FLORDSPE                       
276500         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
276600         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
276700         MOVE ORAD-IDDC            TO SDCA-IDDC                           
276800         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
276900         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
277000         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
277100         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
277200         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
277300         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
277400         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
277500         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
277600         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
277700         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
277800         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
277900         MOVE +0                   TO SDCA-TIREPDAT                       
278000         MOVE +0                   TO SDCA-KVOKS-PREL                     
278100         MOVE +1                   TO SDCA-KDCALL                         
278200         MOVE +1                   TO SDCA-IXDCCLEAR                      
278300                                                                          
278400         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
278500                                           SDCA-WDB6-PCB                  
278600                                           SDCA-WDK9-PCB                  
278700                                           SDCA-WDR6-PCB                  
278800                                           SDCA-WDK6-PCB                  
278900                                           SDCA-WDQ4B-PCB                 
279000                                           SDCA-WDQ2-PCB                  
279100                                           SDCA-WDQ4-PCB                  
279200                                           SDCA-WDB6-2-PCB                
279300                                           SDCA-WDK6-2-PCB                
279400                                           SDCA-WDK7-2-PCB                
279500                                           SDCA-WDK7-3-PCB                
279600                                                                          
279700         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
279800         MOVE ZERO          TO SDCA-KDORDBEK                              
279900                                                                          
280000         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
280100           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
280200*            IF OHUV-IDDC-CLEAR(2) > '19'                                 
280300*               MOVE ZERO               TO SDCA-KDORDBEK-FIRST-SDC        
280400*               MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                      
280500*               MOVE ORAD-IDDC          TO WS-IDDC                        
280600*               IF SDCA-KDOI = 'XX'                                       
280700*                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
280800*               END-IF                                                    
280900*            ELSE                                                         
281000                MOVE JA        TO OBKR-SW                                 
281100                MOVE NEJ       TO ALLT-SW                                 
281200*            END-IF                                                       
281300           ELSE                                                           
281400             IF ORAD-KDORDKL > 0                                          
281500             AND ORAD-IDSYSTEM NOT = 'OREL'                               
281600             AND (AREG-KDUART = 'L'                                       
281700             OR AREG-KDUART = 'P')                                        
281800             AND OHUV-FLORDSPE NOT = JA                                   
281900             AND OHUV-FLOVRLEV NOT = JA                                   
282000             AND OHUV-FLFORBI = NEJ                                       
282100             AND NOT DCS-CHINA                                            
282200                MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                   
282300                MOVE 70      TO TPO2-KDORDBEK                             
282400                MOVE JA      TO OBKR-SW                                   
282500                MOVE NEJ     TO ALLT-SW                                   
282600                MOVE WC-CDC-SE TO ORAD-IDDC                               
282700                MOVE ORAD-IDDC TO WS-IDDC                                 
282800                MOVE 6       TO ORAD-KDTPOTYP                             
282900                IF ORAD-KDPRTYP NOT = 'P'                                 
283000                   MOVE ZERO  TO ORAD-PRARTNTO                            
283100                                                                          
283200                   MOVE SPACE TO ORAD-KDPRTYP                             
283300**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
283400                   IF NOT DIST79-DEALER-PRICE                             
283500                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
283600                     MOVE NEJ         TO ORAD-FLPRTILL                    
283700                   END-IF                                                 
283800*************                                                             
283900                END-IF                                                    
284000             ELSE                                                         
284100               IF KOLLA-ERS                                               
284200                  IF SPAR-KDORDBEK = ZERO                                 
284300                   IF ORAD-IDDC = W-TILLK-DC                              
284400                      MOVE JA          TO OBKR-SW                         
284500                      MOVE NEJ         TO ALLT-SW                         
284600                                          KOLLA-ERS-SW                    
284700                      MOVE ZERO        TO SDCA-KDORDBEK-FIRST-SDC         
284800                   ELSE                                                   
284900                    ADD 1              TO WS-IXDCCLEAR                    
285000                    MOVE W-GMT-IDDC-CLEAR(2)                              
285100                                       TO ORAD-IDDC                       
285200                    MOVE ORAD-IDDC     TO WS-IDDC                         
285300                    IF WS-IDDC = WC-CDC-SE                                
285400                       MOVE JA         TO CDC-MOVE-SW                     
285500                    END-IF                                                
285600                   END-IF                                                 
285700                  ELSE                                                    
285800                    MOVE NEJ           TO KOLLA-ERS-SW                    
285900                  END-IF                                                  
286000               ELSE                                                       
286100                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
286200                    ADD 1              TO WS-IXDCCLEAR                    
286300                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
286400                    MOVE ORAD-IDDC     TO WS-IDDC                         
286500                    MOVE JA            TO OBKR-SW                         
286600                 ELSE                                                     
286700                    MOVE JA            TO OBKR-SW                         
286800                    MOVE NEJ           TO ALLT-SW                         
286900                 END-IF                                                   
287000               END-IF                                                     
287100             END-IF                                                       
287200           END-IF                                                         
287300         ELSE                                                             
287400           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
287500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
287600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
287700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
288100           MOVE NEJ                  TO ALLT-SW                           
288200           IF KVAN-KDORDBEK-UT = ZERO                                     
288300             MOVE JA                 TO EGET-CL-RAD-SW                    
288400           END-IF                                                         
288500           IF KOLLA-ERS    OR                                             
288600             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
288700              MOVE NEJ               TO KOLLA-ERS-SW                      
288800              MOVE ZERO              TO KERS-KDORDBEK                     
288900              PERFORM S02-RENSA-TILLK-TAB                                 
289000              MOVE ZERO              TO SPAR-KDORDBEK                     
289100           END-IF                                                         
289200           IF KERS-KDERS = +19 OR +29                                     
289300              MOVE ZERO              TO SPAR-KDORDBEK                     
289400           END-IF                                                         
289500         END-IF                                                           
289600         MOVE SDCA-KDOI              TO ORAD-KDOI                         
289700         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
289800       END-IF                                                             
289900                                                                          
290000     END-IF                                                               
290100     .                                                                    
290200     EJECT                                                                
290300                                                                          
290400 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
290500                                                                          
290600     IF ALLT-OK OR KOLLA-ERS                                              
290700                                                                          
290800       PERFORM S10-HAMTA-WDB6-INFO                                        
290900                                                                          
291000       IF DCS-SDC                                                         
291100                                                                          
291200         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
291300         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
291400         MOVE NEJ                  TO SDCA-FLORDSPE                       
291500         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
291600         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
291700         MOVE ORAD-IDDC            TO SDCA-IDDC                           
291800         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
291900         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
292000         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
292100         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
292200         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
292300         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
292400         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
292500         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
292600         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
292700         MOVE +1                   TO SDCA-KDCALL                         
292800         MOVE +2                   TO SDCA-IXDCCLEAR                      
292900                                                                          
293000         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
293100                                           SDCA-WDB6-PCB                  
293200                                           SDCA-WDK9-PCB                  
293300                                           SDCA-WDR6-PCB                  
293400                                           SDCA-WDK6-PCB                  
293500                                           SDCA-WDQ4B-PCB                 
293600                                           SDCA-WDQ2-PCB                  
293700                                           SDCA-WDQ4-PCB                  
293800                                           SDCA-WDB6-2-PCB                
293900                                           SDCA-WDK6-2-PCB                
294000                                           SDCA-WDK7-2-PCB                
294100                                           SDCA-WDK7-3-PCB                
294200                                                                          
294300         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
294400         MOVE ZERO          TO SDCA-KDORDBEK                              
294500                                                                          
294600         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
294700           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
294800*            IF OHUV-IDDC-CLEAR(3) > '19'                                 
294900*               MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC          
295000*               MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                      
295100*               MOVE ORAD-IDDC          TO WS-IDDC                        
295200*               IF SDCA-KDOI = 'XX'                                       
295300*                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
295400*               END-IF                                                    
295500*            ELSE                                                         
295600                MOVE JA        TO OBKR-SW                                 
295700               MOVE NEJ       TO ALLT-SW                                  
295800*            END-IF                                                       
295900           ELSE                                                           
296000             IF ORAD-KDORDKL > 0                                          
296100             AND ORAD-IDSYSTEM NOT = 'OREL'                               
296200             AND (AREG-KDUART = 'L'                                       
296300             OR AREG-KDUART = 'P')                                        
296400             AND OHUV-FLORDSPE NOT = JA                                   
296500             AND OHUV-FLOVRLEV NOT = JA                                   
296600             AND OHUV-FLFORBI = NEJ                                       
296700             AND NOT DCS-CHINA                                            
296800                MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                  
296900                MOVE 70      TO TPO2-KDORDBEK                             
297000                MOVE JA      TO OBKR-SW                                   
297100                MOVE NEJ     TO ALLT-SW                                   
297200                MOVE WC-CDC-SE TO ORAD-IDDC                               
297300                MOVE ORAD-IDDC TO WS-IDDC                                 
297400                MOVE 6       TO ORAD-KDTPOTYP                             
297500                IF ORAD-KDPRTYP NOT = 'P'                                 
297600                   MOVE ZERO  TO ORAD-PRARTNTO                            
297700                                                                          
297800                   MOVE SPACE TO ORAD-KDPRTYP                             
297900**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
298000                   IF NOT DIST79-DEALER-PRICE                             
298100                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
298200                     MOVE NEJ         TO ORAD-FLPRTILL                    
298300                   END-IF                                                 
298400*************                                                             
298500                END-IF                                                    
298600             ELSE                                                         
298700               IF KOLLA-ERS                                               
298800                  MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
298900                  IF SPAR-KDORDBEK = ZERO                                 
299000                   IF ORAD-IDDC = W-TILLK-DC                              
299100                      MOVE JA          TO OBKR-SW                         
299200                      MOVE NEJ         TO ALLT-SW                         
299300                                            KOLLA-ERS-SW                  
299400                     MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC        
299500                   ELSE                                                   
299600                    ADD 1              TO WS-IXDCCLEAR                    
299700                    MOVE W-GMT-IDDC-CLEAR(3)                              
299800                                       TO ORAD-IDDC                       
299900                    MOVE ORAD-IDDC     TO WS-IDDC                         
300000                    IF WS-IDDC = WC-CDC-SE                                
300100                       MOVE JA         TO CDC-MOVE-SW                     
300200                    END-IF                                                
300300                   END-IF                                                 
300400                  ELSE                                                    
300500                    MOVE NEJ           TO KOLLA-ERS-SW                    
300600                  END-IF                                                  
300700               ELSE                                                       
300800                 IF SDCA-KDORDBEK-SECOND-SDC = 15                         
300900                    IF SDCA-KDORDBEK-FIRST-SDC = 15                       
301000                       MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC               
301100                    END-IF                                                
301200                    ADD 1              TO WS-IXDCCLEAR                    
301300                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
301400                    MOVE ORAD-IDDC     TO WS-IDDC                         
301500                    MOVE JA            TO OBKR-SW                         
301600                 ELSE                                                     
301700                    MOVE JA            TO OBKR-SW                         
301800                    MOVE NEJ           TO ALLT-SW                         
301900                 END-IF                                                   
302000               END-IF                                                     
302100             END-IF                                                       
302200           END-IF                                                         
302300         ELSE                                                             
302400           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
302500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
302600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
302700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
302800           MOVE NEJ                  TO ALLT-SW                           
303200           IF KVAN-KDORDBEK-UT = ZERO                                     
303300           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
303400             MOVE JA                 TO EGET-CL-RAD-SW                    
303500           END-IF                                                         
303600           IF KOLLA-ERS    OR                                             
303700             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
303800              MOVE NEJ               TO KOLLA-ERS-SW                      
303900              MOVE ZERO              TO KERS-KDORDBEK                     
304000              PERFORM S02-RENSA-TILLK-TAB                                 
304100              MOVE ZERO              TO SPAR-KDORDBEK                     
304200           END-IF                                                         
304300           IF KERS-KDERS = +19 OR +29                                     
304400              MOVE ZERO              TO SPAR-KDORDBEK                     
304500           END-IF                                                         
304600         END-IF                                                           
304700         MOVE SDCA-KDOI              TO ORAD-KDOI                         
304800         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
304900       END-IF                                                             
305000                                                                          
305100     END-IF                                                               
305200     .                                                                    
305300     EJECT                                                                
305400                                                                          
305500 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
305600                                                                          
305700     IF ALLT-OK OR KOLLA-ERS                                              
305800                                                                          
305900       PERFORM S10-HAMTA-WDB6-INFO                                        
306000                                                                          
306100       IF DCS-SDC                                                         
306200                                                                          
306300         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
306400         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
306500         MOVE NEJ                  TO SDCA-FLORDSPE                       
306600         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
306700         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
306800         MOVE ORAD-IDDC            TO SDCA-IDDC                           
306900         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
307000         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
307100         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
307200         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
307300         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
307400         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
307500         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
307600         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
307700         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
307800         MOVE +1                   TO SDCA-KDCALL                         
307900         MOVE +3                   TO SDCA-IXDCCLEAR                      
308000                                                                          
308100         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
308200                                           SDCA-WDB6-PCB                  
308300                                           SDCA-WDK9-PCB                  
308400                                           SDCA-WDR6-PCB                  
308500                                           SDCA-WDK6-PCB                  
308600                                           SDCA-WDQ4B-PCB                 
308700                                           SDCA-WDQ2-PCB                  
308800                                           SDCA-WDQ4-PCB                  
308900                                           SDCA-WDB6-2-PCB                
309000                                           SDCA-WDK6-2-PCB                
309100                                           SDCA-WDK7-2-PCB                
309200                                           SDCA-WDK7-3-PCB                
309300                                                                          
309400         IF SDCA-KDORDBEK > ZERO                                          
309500           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
309600              MOVE JA        TO OBKR-SW                                   
309700              MOVE NEJ       TO ALLT-SW                                   
309800           ELSE                                                           
309900             IF ORAD-KDORDKL > 0                                          
310000             AND ORAD-IDSYSTEM NOT = 'OREL'                               
310100             AND (AREG-KDUART = 'L'                                       
310200             OR AREG-KDUART = 'P')                                        
310300             AND OHUV-FLORDSPE NOT = JA                                   
310400             AND OHUV-FLOVRLEV NOT = JA                                   
310500             AND OHUV-FLFORBI = NEJ                                       
310600             AND NOT DCS-CHINA                                            
310700                MOVE ZERO    TO SDCA-KDORDBEK                             
310800                MOVE 70      TO TPO2-KDORDBEK                             
310900                MOVE JA      TO OBKR-SW                                   
311000                MOVE NEJ     TO ALLT-SW                                   
311100                MOVE WC-CDC-SE TO ORAD-IDDC                               
311200                MOVE ORAD-IDDC TO WS-IDDC                                 
311300                MOVE 6       TO ORAD-KDTPOTYP                             
311400                IF ORAD-KDPRTYP NOT = 'P'                                 
311500                   MOVE ZERO  TO ORAD-PRARTNTO                            
311600                   MOVE SPACE TO ORAD-KDPRTYP                             
311700**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
311800                   IF NOT DIST79-DEALER-PRICE                             
311900                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
312000                     MOVE NEJ         TO ORAD-FLPRTILL                    
312100                   END-IF                                                 
312200*************                                                             
312300                END-IF                                                    
312400             ELSE                                                         
312500               IF KOLLA-ERS                                               
312600*                 MOVE NEJ             TO KOLLA-ERS-SW                    
312700*                 MOVE ZERO            TO SDCA-KDORDBEK                   
312800                  MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
312900                  IF SPAR-KDORDBEK = ZERO                                 
313000                   IF ORAD-IDDC = W-TILLK-DC                              
313100                      MOVE JA          TO OBKR-SW                         
313200                      MOVE NEJ         TO ALLT-SW                         
313300                                          KOLLA-ERS-SW                    
313400                      MOVE ZERO        TO SDCA-KDORDBEK                   
313500                   ELSE                                                   
313600                    ADD 1              TO WS-IXDCCLEAR                    
313700                    IF DCS-CHINA                                          
313800                       MOVE W-GMT-IDDC-CLEAR(WS-IXDCCLEAR)                
313900                                       TO ORAD-IDDC                       
314000                    ELSE                                                  
314100                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
314200                    END-IF                                                
314300                    MOVE ORAD-IDDC     TO WS-IDDC                         
314400                    IF WS-IDDC = WC-CDC-SE                                
314500                       MOVE JA         TO CDC-MOVE-SW                     
314600                    END-IF                                                
314700                   END-IF                                                 
314800                  END-IF                                                  
314900               ELSE                                                       
315000                 IF SDCA-KDORDBEK = 15                                    
315100                    IF SDCA-KDORDBEK-SECOND-SDC = 15                      
315200                       MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC              
315300                    END-IF                                                
315400                    ADD 1              TO WS-IXDCCLEAR                    
315500                    IF DCS-CHINA                                          
315600                       MOVE W-GMT-IDDC-CLEAR(WS-IXDCCLEAR)                
315700                                       TO ORAD-IDDC                       
315800                    ELSE                                                  
315900                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
316000                    END-IF                                                
316100                    MOVE ORAD-IDDC     TO WS-IDDC                         
316200                    MOVE JA            TO OBKR-SW                         
316300                 ELSE                                                     
316400                    MOVE JA            TO OBKR-SW                         
316500                    MOVE NEJ           TO ALLT-SW                         
316600                 END-IF                                                   
316700               END-IF                                                     
316800             END-IF                                                       
316900           END-IF                                                         
317000         ELSE                                                             
317100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
317200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
317300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
317400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
317800           MOVE NEJ                  TO ALLT-SW                           
317900           IF  KVAN-KDORDBEK-UT  = ZERO                                   
318000           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
318100           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
318200             MOVE JA                 TO EGET-CL-RAD-SW                    
318300           END-IF                                                         
318400           IF KOLLA-ERS    OR                                             
318500             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
318600              MOVE NEJ               TO KOLLA-ERS-SW                      
318700              MOVE ZERO              TO KERS-KDORDBEK                     
318800              PERFORM S02-RENSA-TILLK-TAB                                 
318900              MOVE ZERO              TO SPAR-KDORDBEK                     
319000           END-IF                                                         
319100           IF KERS-KDERS = +19 OR +29                                     
319200              MOVE ZERO              TO SPAR-KDORDBEK                     
319300           END-IF                                                         
319400         END-IF                                                           
319500                                                                          
319600         MOVE SDCA-KDOI              TO ORAD-KDOI                         
319700         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
319800       END-IF                                                             
319900                                                                          
320000     END-IF                                                               
320100     .                                                                    
320200     EJECT                                                                
320300 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
320400                                                                          
320500     IF ALLT-OK OR CDC-MOVE                                               
320600                                                                          
320700       MOVE ORAD-BERADREF      TO RANS-BERADREF                           
320800       MOVE OHUV-FLEMBORD      TO RANS-FLEMBORD                           
320900       MOVE OHUV-FLFORBI       TO RANS-FLFORBI                            
321000       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
321100       MOVE OHUV-FLOVRLEV      TO RANS-FLOVRLEV                           
321200       MOVE ORAD-IDKAMPRF      TO RANS-IDKAMPRF                           
321300       MOVE ORAD-IDARTNR       TO RANS-IDARTNR                            
321400       MOVE ORAD-IDLEVNR       TO RANS-IDLEVNR                            
321500       MOVE OHUV-IDRFTAB       TO RANS-IDRFTAB                            
321600       MOVE ORAD-TIRODAT       TO RANS-TIRODAT                            
321700       MOVE OHUV-KDORDKL       TO RANS-KDORDKL                            
321800       MOVE +1                 TO RANS-KDORDBEH                           
321900       MOVE ORAD-KVBEART-Q     TO RANS-KVBEART-Q                          
322000       MOVE ORAD-KDTPOTYP      TO RANS-KDTPOTYP                           
322100       MOVE AREG-KDERS         TO RANS-KDERS                              
322200       MOVE AREG-KVLS          TO RANS-KVLS                               
322300       MOVE AREG-KVPB-SATS     TO RANS-KVPB-SATS                          
322400       MOVE AREG-KVPB-SEP      TO RANS-KVPB-SEP                           
322500       MOVE AREG-REDIRLEV      TO RANS-REDIRLEV                           
322600       MOVE AREG-KVRESS        TO RANS-KVRESS                             
322700       MOVE AREG-KVSPANT       TO RANS-KVSPANT                            
322800       MOVE AREG-KVUTRS        TO RANS-KVUTRS                             
322900       MOVE AREG-TIDISPIN      TO RANS-TIDISPIN                           
323000                                                                          
323100       MOVE AREG-KDPRODSL      TO TEST-KDPRODSL                           
323200       IF KDPRODSL-BIMA                                                   
323300         MOVE 1                TO ORAD-RERF-RAD                           
323400                                  RANS-RERF-RAD-UT                        
323500         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
323600                                  RANS-SUTPO-EJPB-UT                      
323700                                  RANS-RERF-ART-UT                        
323800       ELSE                                                               
323900         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
324000                             RANS-ARTM-PCB RANS-ARTS-PCB                  
324100                                                                          
324200         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
324300       END-IF                                                             
324400     END-IF                                                               
324500     .                                                                    
324600     EJECT                                                                
324700 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
324800                                                                          
324900     IF ALLT-OK                                                           
325000                                                                          
325100     MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                           
325200     MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                            
325300     MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                        
325400     MOVE SPACE                TO STOR-KDPROTYP                           
325500     MOVE ORAD-BERADREF        TO STOR-BERADREF                           
325600     MOVE OHUV-FLFORBI         TO STOR-FLFORBI                            
325700     MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                           
325800     MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                           
325900     MOVE OHUV-KDORDKL         TO STOR-KDORDKL                            
326000     MOVE AREG-KDERS           TO STOR-KDERS                              
326100     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
326200     MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                          
326300     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
326400     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
326500     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
326600     MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                           
326700     MOVE ORAD-IDDISTR         TO STOR-IDDISTR                            
326800     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
326900                                                                          
327000     CALL W411STOR USING STOR-W411STOR                                    
327100                                                                          
327200     IF STOR-KDORDBEK > +0                                                
327300        MOVE +6                   TO ORAD-KDTPOTYP                        
327400        IF ORAD-KDPRTYP NOT = 'P'                                         
327500           MOVE +0                TO ORAD-PRARTNTO                        
327600           MOVE SPACE             TO ORAD-KDPRTYP                         
327700**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
327800           IF NOT DIST79-DEALER-PRICE                                     
327900             MOVE ZERO        TO ORAD-PRARTNTO-LOC                        
328000             MOVE NEJ         TO ORAD-FLPRTILL                            
328100           END-IF                                                         
328200*************                                                             
328300        END-IF                                                            
328400        MOVE JA                   TO OBKR-SW                              
328500        MOVE NEJ                  TO ALLT-SW                              
328600     END-IF                                                               
328700                                                                          
328800     END-IF                                                               
328900     .                                                                    
329000     EJECT                                                                
329100 ECR-PREL-AVBOKNING-CDC SECTION.                                          
329200                                                                          
329300     IF ALLT-OK OR CDC-MOVE                                               
329400                                                                          
329500     MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                    
329600                               TO CDCA-FLFINLV-IN                         
329700     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
329800     MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                        
329900     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
330000     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
330100     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
330200     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
330300                               TO CDCA-FLSLATT-IN                         
330400     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
330500     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
330600     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
330700     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
330800     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
330900     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
331000     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
331100     MOVE AREG-KDERS           TO CDCA-KDERS-IN                           
331200     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
331300     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
331400     MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                         
331500     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
331600     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
331700     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
331800     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
331900     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
332000     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
332100     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
332200     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
332300     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
332400     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
332500     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
332600     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
332700     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
332800     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
332900     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
333000     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
333100     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
333200     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
333300     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
333400     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
333500     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
333600     MOVE +1                   TO CDCA-KDCALL                             
333700                                                                          
333800     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
333900                                       CDCA-INLB-PCB                      
334000                                       CDCA-WDB2-PCB                      
334100                                       CDCA-WDC1-PCB                      
334200     EJECT                                                                
334300                                                                          
334400     IF KERS-KDERS = 0                                                    
334500        CONTINUE                                                          
334600     ELSE                                                                 
334700        IF KERS-KDERS > 0 AND < 10                                        
334800           IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0              
334900              MOVE CDCA-KVBEART-Q-UT TO CDCA-KVPRERO-UT                   
335000           END-IF                                                         
335100           PERFORM S02-RENSA-TILLK-TAB                                    
335200           MOVE ZERO           TO KERS-KDORDBEK                           
335300           MOVE NEJ            TO TILLK-SW                                
335400        ELSE                                                              
335500*FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                          
335600          IF CDCA-KVPREAVB-UT > 0                                         
335700            IF KOLLA-ERS    OR                                            
335800              (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)              
335900               MOVE NEJ               TO KOLLA-ERS-SW                     
336000               MOVE ZERO              TO KERS-KDORDBEK                    
336100               PERFORM S02-RENSA-TILLK-TAB                                
336200               MOVE ZERO              TO SPAR-KDORDBEK                    
336300            ELSE                                                          
336400               IF KERS-KDERS = +19 OR +29                                 
336500                  MOVE ZERO           TO SPAR-KDORDBEK                    
336600               END-IF                                                     
336700            END-IF                                                        
336800          ELSE                                                            
336900             IF (CDCA-KVPREAVB-UT <= 0) AND                               
337000               (CDCA-KDORDBEK-UT = 92 OR 99)                              
337100                 MOVE ZEROES        TO CDCA-KDORDBEK-UT                   
337200             END-IF                                                       
337300             IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                      
337400             OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)              
337500             OR SPAR-KDORDBEK = 54                                        
337600                PERFORM S07-SPACE-SDCA-KDORDBEK                           
337700             END-IF                                                       
337800          END-IF                                                          
337900        END-IF                                                            
338000     END-IF                                                               
338100                                                                          
338200     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
338300                                                                          
338400     IF ORAD-IDLEVNR NOT = SPACE                                          
338500        CONTINUE                                                          
338600     ELSE                                                                 
338700        MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                         
338800        MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                       
338900     END-IF                                                               
339000     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
339100     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
339200     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
339300     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
339400                                                                          
339500     IF CDCA-KDORDBEK-UT > ZERO                                           
339600        MOVE JA                TO OBKR-SW                                 
339700     END-IF                                                               
339800                                                                          
339900     IF KVAN-KDORDBEK-UT > +0                                             
340000        IF CDCA-KVBEART-UT = CDCA-KVBEART-Q-UT                            
340100                                                                          
340200           MOVE +0             TO KVAN-KDORDBEK-UT                        
340300        END-IF                                                            
340400     END-IF                                                               
340500     EJECT                                                                
340600     IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND               
340700         CDCA-KDORDBEK-UT  = +0 AND                                       
340800         KVAN-KDORDBEK-UT  = +0 AND                                       
340900         DLEV-KDORDBEK-UT  = +0 AND                                       
341000         KERS-KDORDBEK     = +0 AND                                       
341100         TPO1-KDORDBEK     = +0 AND                                       
341200         TPO2-KDORDBEK     = +0 AND                                       
341300         KAMP-KDORDBEK     = +0 AND                                       
341400         STOR-KDORDBEK     = +0 AND                                       
341500         SDCA-KDORDBEK     = +0 AND                                       
341600         SDCA-KDORDBEK-FIRST-SDC = +0 AND                                 
341700         SDCA-KDORDBEK-SECOND-SDC = +0 AND                                
341800         SPAR-KDORDBEK     = +0                                           
341900         MOVE JA                  TO EGET-CL-RAD-SW                       
342000     END-IF                                                               
342100     MOVE AREG-ADLAGOMR          TO ORAD-ADLAGOMR                         
342200     MOVE AREG-ADGANG            TO ORAD-ADGANG                           
342300     MOVE AREG-ADPLATS           TO ORAD-ADPLATS                          
342400                                                                          
342500     END-IF                                                               
342600     .                                                                    
342700     EJECT                                                                
342800 ECS-SKRIV-OBKR SECTION.                                                  
342900                                                                          
343000*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
343100*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT IDDC 'SLÄPAR'                
343200*    ISRT AV RADEN.                                                       
343300*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
343400*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
343500*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
343600*---                                                                      
343700     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
343800                                                                          
343900     IF TILLKOMMANDE-RAD                                                  
344000        IF KERS-KDORDBEK = 41                                             
344100           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
344200           MOVE '4242KER1'        TO OBKR-IDPGM                           
344300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
344400                                  TO OBKR-KVBEART-TILLK                   
344500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
344600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
344700             MOVE +0              TO OBKR-DIERS-KVOT                      
344800           ELSE                                                           
344900             COMPUTE OBKR-DIERS-KVOT =                                    
345000                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
345100                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
345200           END-IF                                                         
345300           MOVE 'S'               TO OBKR-SW                              
345400        END-IF                                                            
345500     END-IF                                                               
345600                                                                          
345700     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
345800       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
345900*----(KOD 58, 59, 98)                                                     
346000        IF OBKR-SKRIVEN                                                   
346100           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
346200           ADD +1              TO OBKR-IDSEKVNR                           
346300        END-IF                                                            
346400        MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                  
346500                               TO OBKR-KDORDBEK                           
346600        MOVE '4242ORFK'        TO OBKR-IDPGM                              
346700        MOVE 'S'               TO OBKR-SW                                 
346800     END-IF                                                               
346900     EJECT                                                                
347000                                                                          
347100     IF KVAN-KDORDBEK-UT > +0                                             
347200*----(KOD 43, 44)                                                         
347300        IF OBKR-SKRIVEN                                                   
347400           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
347500           ADD +1              TO OBKR-IDSEKVNR                           
347600        END-IF                                                            
347700        IF TILLKOMMANDE-RAD                                               
347800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
347900                               TO OBKR-KVBEART-TILLK                      
348000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
348100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
348200             MOVE +0              TO OBKR-DIERS-KVOT                      
348300           ELSE                                                           
348400             COMPUTE OBKR-DIERS-KVOT =                                    
348500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
348600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
348700           END-IF                                                         
348800        END-IF                                                            
348900        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
349000        MOVE '4242KVAN'        TO OBKR-IDPGM                              
349100        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
349200        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
349300        MOVE 'S'               TO OBKR-SW                                 
349400     END-IF                                                               
349500     EJECT                                                                
349600                                                                          
349700     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
349800*----(KOD 21, 53, 82, 95) , 26                                            
349900        IF OBKR-SKRIVEN                                                   
350000           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
350100           ADD +1              TO OBKR-IDSEKVNR                           
350200        END-IF                                                            
350300        IF TILLKOMMANDE-RAD                                               
350400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
350500                               TO OBKR-KVBEART-TILLK                      
350600           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
350700              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
350800             MOVE +0              TO OBKR-DIERS-KVOT                      
350900           ELSE                                                           
351000             COMPUTE OBKR-DIERS-KVOT =                                    
351100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
351200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
351300           END-IF                                                         
351400        END-IF                                                            
351500        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
351600        MOVE '4242DLEV'        TO OBKR-IDPGM                              
351700        MOVE 'S'               TO OBKR-SW                                 
351800     END-IF                                                               
351900     EJECT                                                                
352000     IF KERS-KDORDBEK > +0                                                
352100*----(KOD 41, 61)                                                         
352200                                                                          
352300        IF KERS-KDORDBEK = 61                                             
352400*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
352500*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
352600*----RADBEHANDLINGEN                                                      
352700           IF OBKR-SKRIVEN                                                
352800              PERFORM IMS-08-ISRT-WLORQM01-WDQ101                         
352900              ADD +1           TO OBKR-IDSEKVNR                           
353000           END-IF                                                         
353100           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
353200           MOVE '4242KER2'     TO OBKR-IDPGM                              
353300           MOVE 'S'            TO OBKR-SW                                 
353400           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
353500           PERFORM S02-RENSA-TILLK-TAB                                    
353600        ELSE                                                              
353700           IF NOT TILLKOMMANDE-RAD                                        
353800              IF OBKR-SKRIVEN                                             
353900                 PERFORM IMS-08-ISRT-WLORQM01-WDQ101                      
354000                 ADD +1        TO OBKR-IDSEKVNR                           
354100              END-IF                                                      
354200              MOVE 'S'            TO OBKR-SW                              
354300              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
354400              MOVE '4242KER3'     TO OBKR-IDPGM                           
354500           END-IF                                                         
354600        END-IF                                                            
354700     END-IF                                                               
354800     EJECT                                                                
354900     IF SPAR-KDORDBEK > +0                                                
355000*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90)                     
355100        IF OBKR-SKRIVEN                                                   
355200           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
355300           ADD +1              TO OBKR-IDSEKVNR                           
355400        END-IF                                                            
355500        IF TILLKOMMANDE-RAD                                               
355600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
355700                               TO OBKR-KVBEART-TILLK                      
355800           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
355900              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
356000             MOVE +0              TO OBKR-DIERS-KVOT                      
356100           ELSE                                                           
356200             COMPUTE OBKR-DIERS-KVOT =                                    
356300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
356400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
356500           END-IF                                                         
356600        END-IF                                                            
356700        MOVE SPAR-KDORDBEK                                                
356800                               TO OBKR-KDORDBEK                           
356900        MOVE '4242SPAR'        TO OBKR-IDPGM                              
357000        MOVE 'S'               TO OBKR-SW                                 
357100     END-IF                                                               
357200     EJECT                                                                
357300                                                                          
357400     IF TPO1-KDORDBEK > +0                                                
357500*----(KOD 72, 73, 74, 85)                                                 
357600        IF OBKR-SKRIVEN                                                   
357700           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
357800           ADD +1              TO OBKR-IDSEKVNR                           
357900        END-IF                                                            
358000        IF TILLKOMMANDE-RAD                                               
358100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
358200                               TO OBKR-KVBEART-TILLK                      
358300           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
358400              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
358500             MOVE +0              TO OBKR-DIERS-KVOT                      
358600           ELSE                                                           
358700             COMPUTE OBKR-DIERS-KVOT =                                    
358800                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
358900                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
359000           END-IF                                                         
359100        END-IF                                                            
359200        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
359300        MOVE '4242TPO1'        TO OBKR-IDPGM                              
359400        IF TPO1-KDORDBEK = 85                                             
359500           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
359600        END-IF                                                            
359700        MOVE 'S'               TO OBKR-SW                                 
359800     END-IF                                                               
359900     EJECT                                                                
360000     IF TPO2-KDORDBEK > +0                                                
360100*----(KOD 70)                                                             
360200        IF OBKR-SKRIVEN                                                   
360300           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
360400           ADD +1              TO OBKR-IDSEKVNR                           
360500        END-IF                                                            
360600        IF TILLKOMMANDE-RAD                                               
360700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
360800                               TO OBKR-KVBEART-TILLK                      
360900           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
361000              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
361100             MOVE +0              TO OBKR-DIERS-KVOT                      
361200           ELSE                                                           
361300             COMPUTE OBKR-DIERS-KVOT =                                    
361400                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
361500                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
361600           END-IF                                                         
361700        END-IF                                                            
361800        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
361900        MOVE '4242TPO2'        TO OBKR-IDPGM                              
362000        MOVE 'S'               TO OBKR-SW                                 
362100     END-IF                                                               
362200     EJECT                                                                
362300                                                                          
362400     IF KAMP-KDORDBEK > +0                                                
362500*----(KOD 72, 75, 76)                                                     
362600        IF OBKR-SKRIVEN                                                   
362700           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
362800           ADD +1              TO OBKR-IDSEKVNR                           
362900        END-IF                                                            
363000        IF TILLKOMMANDE-RAD                                               
363100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
363200                               TO OBKR-KVBEART-TILLK                      
363300           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
363400              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
363500             MOVE +0              TO OBKR-DIERS-KVOT                      
363600           ELSE                                                           
363700             COMPUTE OBKR-DIERS-KVOT =                                    
363800                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
363900                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
364000           END-IF                                                         
364100        END-IF                                                            
364200        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
364300        MOVE '4242KAMP'        TO OBKR-IDPGM                              
364400        MOVE 'S'               TO OBKR-SW                                 
364500     END-IF                                                               
364600     EJECT                                                                
364700                                                                          
364800     IF RELS-KDORDBEK > 0                                                 
364900*----(KOD 56)                                                             
365000          IF OBKR-SKRIVEN                                                 
365100             PERFORM IMS-08-ISRT-WLORQM01-WDQ101                          
365200             ADD +1              TO OBKR-IDSEKVNR                         
365300          END-IF                                                          
365400          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
365500          MOVE '4242ORFK'        TO OBKR-IDPGM                            
365600          MOVE 'S'               TO OBKR-SW                               
365700     END-IF                                                               
365800    EJECT                                                                 
365900     IF XDCA-KDORDBEK > ZERO                                              
366000*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
366100        IF OBKR-SKRIVEN                                                   
366200           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
366300           ADD +1              TO OBKR-IDSEKVNR                           
366400        END-IF                                                            
366500        IF TILLKOMMANDE-RAD                                               
366600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
366700                               TO OBKR-KVBEART-TILLK                      
366800           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
366900              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
367000             MOVE +0              TO OBKR-DIERS-KVOT                      
367100           ELSE                                                           
367200             COMPUTE OBKR-DIERS-KVOT =                                    
367300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
367400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
367500           END-IF                                                         
367600        END-IF                                                            
367700                                                                          
367800        IF XDCA-KDORDBEK NOT = 15                                         
367900          IF OHUV-IDDC-TVS = SPACE                                        
368000            IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                     
368100              MOVE 15          TO OBKR-KDORDBEK                           
368200              MOVE IDPGM       TO OBKR-IDPGM                              
368300              MOVE 'S'         TO OBKR-SW                                 
368400                                                                          
368500* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
368600              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
368700                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
368800              END-IF                                                      
368900              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
369000                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
369100              END-IF                                                      
369200              IF SDCA-KDORDBEK = 15                                       
369300                 MOVE ZERO     TO SDCA-KDORDBEK                           
369400              END-IF                                                      
369500            END-IF                                                        
369600            IF OBKR-SKRIVEN                                               
369700              PERFORM IMS-08-ISRT-WLORQM01-WDQ101                         
369800              ADD +1           TO OBKR-IDSEKVNR                           
369900            END-IF                                                        
370000          END-IF                                                          
370100        END-IF                                                            
370200        IF XDCA-KDORDBEK = 80                                             
370300           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
370400        END-IF                                                            
370500        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
370600        MOVE '4242XDCA'        TO OBKR-IDPGM                              
370700        MOVE 'S'               TO OBKR-SW                                 
370800     END-IF                                                               
370900     EJECT                                                                
371000                                                                          
371100     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
371200*----(KOD 15, 53, 80, 92)                                                 
371300        IF OBKR-SKRIVEN                                                   
371400           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
371500           ADD +1              TO OBKR-IDSEKVNR                           
371600        END-IF                                                            
371700        IF TILLKOMMANDE-RAD                                               
371800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
371900                               TO OBKR-KVBEART-TILLK                      
372000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
372100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
372200             MOVE +0              TO OBKR-DIERS-KVOT                      
372300           ELSE                                                           
372400             COMPUTE OBKR-DIERS-KVOT =                                    
372500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
372600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
372700           END-IF                                                         
372800        END-IF                                                            
372900        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
373000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
373100        END-IF                                                            
373200        IF SDCA-KDORDBEK-SECOND-SDC = 80 OR 92                            
373300           MOVE 0              TO OBKR-IDARTNR-TILLK                      
373400        END-IF                                                            
373500        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
373600        MOVE '4242SDCA'        TO OBKR-IDPGM                              
373700        MOVE 'S'               TO OBKR-SW                                 
373800     END-IF                                                               
373900     EJECT                                                                
374000                                                                          
374100     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
374200*----(KOD 15, 53, 80, 92)                                                 
374300        IF OBKR-SKRIVEN                                                   
374400           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
374500           ADD +1              TO OBKR-IDSEKVNR                           
374600        END-IF                                                            
374700        IF TILLKOMMANDE-RAD                                               
374800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
374900                               TO OBKR-KVBEART-TILLK                      
375000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
375100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
375200             MOVE +0              TO OBKR-DIERS-KVOT                      
375300           ELSE                                                           
375400             COMPUTE OBKR-DIERS-KVOT =                                    
375500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
375600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
375700           END-IF                                                         
375800        END-IF                                                            
375900        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
376000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
376100        END-IF                                                            
376200        IF SDCA-KDORDBEK-FIRST-SDC = 80 OR 92                             
376300           MOVE 0              TO OBKR-IDARTNR-TILLK                      
376400        END-IF                                                            
376500        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
376600        MOVE '4242SDCA'        TO OBKR-IDPGM                              
376700        MOVE 'S'               TO OBKR-SW                                 
376800     END-IF                                                               
376900     EJECT                                                                
377000                                                                          
377100     IF SDCA-KDORDBEK > ZERO                                              
377200*----(KOD 15, 53, 80, 92)                                                 
377300        IF OBKR-SKRIVEN                                                   
377400           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
377500           ADD +1              TO OBKR-IDSEKVNR                           
377600        END-IF                                                            
377700        IF TILLKOMMANDE-RAD                                               
377800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
377900                               TO OBKR-KVBEART-TILLK                      
378000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
378100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
378200             MOVE +0              TO OBKR-DIERS-KVOT                      
378300           ELSE                                                           
378400             COMPUTE OBKR-DIERS-KVOT =                                    
378500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
378600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
378700           END-IF                                                         
378800        END-IF                                                            
378900        IF SDCA-KDORDBEK = 80                                             
379000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
379100        END-IF                                                            
379200        IF SDCA-KDORDBEK = 80 OR 92                                       
379300           MOVE 0              TO OBKR-IDARTNR-TILLK                      
379400        END-IF                                                            
379500        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
379600        MOVE '4242SDCA'        TO OBKR-IDPGM                              
379700        MOVE 'S'               TO OBKR-SW                                 
379800     END-IF                                                               
379900     EJECT                                                                
380000                                                                          
380100     IF STOR-KDORDBEK > +0                                                
380200*----(KOD 70)                                                             
380300        IF OBKR-SKRIVEN                                                   
380400           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
380500           ADD +1              TO OBKR-IDSEKVNR                           
380600        END-IF                                                            
380700        IF TILLKOMMANDE-RAD                                               
380800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
380900                               TO OBKR-KVBEART-TILLK                      
381000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
381100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
381200             MOVE +0              TO OBKR-DIERS-KVOT                      
381300           ELSE                                                           
381400             COMPUTE OBKR-DIERS-KVOT =                                    
381500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
381600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
381700           END-IF                                                         
381800        END-IF                                                            
381900        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
382000        MOVE '4242STOR'        TO OBKR-IDPGM                              
382100        MOVE 'S'               TO OBKR-SW                                 
382200     END-IF                                                               
382300     EJECT                                                                
382400     IF CDCA-KDORDBEK-UT > +0                                             
382500*----(KOD 80, 92, 99)                                                     
382600        IF OBKR-SKRIVEN                                                   
382700           PERFORM IMS-08-ISRT-WLORQM01-WDQ101                            
382800           ADD +1              TO OBKR-IDSEKVNR                           
382900        END-IF                                                            
383000        IF TILLKOMMANDE-RAD                                               
383100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
383200                               TO OBKR-KVBEART-TILLK                      
383300           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
383400              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
383500             MOVE +0              TO OBKR-DIERS-KVOT                      
383600           ELSE                                                           
383700             COMPUTE OBKR-DIERS-KVOT =                                    
383800                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
383900                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
384000           END-IF                                                         
384100        END-IF                                                            
384200        IF CDCA-KDORDBEK-UT = +80                                         
384300           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
384400        END-IF                                                            
384500        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
384600        MOVE '4242CDCA'        TO OBKR-IDPGM                              
384700        MOVE 'S'               TO OBKR-SW                                 
384800     END-IF                                                               
384900*                                                                         
385000* PÅ SISTA RADEN FÖR KUNDENS NORMALA DC LÄGGS DE AVBOKADE                 
385100* ANTALEN!                                                                
385200*                                                                         
385300     IF OBKR-SKRIVEN                                                      
385400*THE IF CONDITION (OCC 61 AND KVPREAVB,KVPRERO = 0) IS CODED,             
385500*BECAUSE IF A HAS NO STOCKS IN LDC,CDC, THEN KVPRERO 1 IS                 
385600*INSERTED FOR THE LAST SUPERSEEDING PART THEREBY CREATING                 
385700*ORDERLINE FOR THAT PART ALONG WITH OCC61.                                
385800       IF OBKR-KDORDBEK = 61 AND                                          
385900        (OBKR-KVPREAVB = 0 AND OBKR-KVPRERO = 0)                          
386000          CONTINUE                                                        
386100       ELSE                                                               
386200          MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                         
386300          MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                          
386400       END-IF                                                             
386500******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
386600         IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                    
386700           PERFORM S05-DELETE-PRICE-Q-LINE                                
386800           INITIALIZE OBKR-DEAL-PR-LINE                                   
386900           MOVE 'N/A'          TO OBKR-KDVALISO                           
387000         END-IF                                                           
387100*************TL 030514                                                    
387200        PERFORM IMS-08-ISRT-WLORQM01-WDQ101                               
387300        ADD +1                 TO OBKR-IDSEKVNR                           
387400     END-IF                                                               
387500                                                                          
387600*** TILLÄGGSTPO SKAPAS                                                    
387700                                                                          
387800     IF DDGS-TPO-OBKR71                                                   
387900        PERFORM ECSB-SKAPA-TPO2                                           
388000     END-IF                                                               
388100     .                                                                    
388200     EJECT                                                                
388300 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
388400                                                                          
388500     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
388600     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
388700                               TO OBKR-IDARTNR                            
388800     IF NOT TILLKOMMANDE-RAD                                              
388900        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
389000                                  W-IDORDER-Q1-MAX                        
389100        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
389200                                  W-IDARTNR-Q1-MAX                        
389300        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
389400                                  W-IDLOPNR-Q1-MAX                        
389500                                  W-IDSEKVNR-Q1-MIN                       
389600                                  W-IDSEKVNR-Q1-MAX                       
389700        PERFORM IMS-07-GU-ORQM-WDQ101                                     
389800        PERFORM UNTIL SEGMENT-SAKNAS                                      
389900           ADD +1              TO W-IDLOPNR-Q1-MIN                        
390000                                  W-IDLOPNR-Q1-MAX                        
390100           PERFORM IMS-07-GU-ORQM-WDQ101                                  
390200        END-PERFORM                                                       
390300        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
390400        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
390500     END-IF                                                               
390600     MOVE ORAD-IDDC            TO OBKR-IDDC                               
390700     MOVE +0                   TO OBKR-KDORDBEK                           
390800     MOVE SPACE                TO OBKR-BEERS                              
390900     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
391000     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
391100     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
391200     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
391300     MOVE +0                   TO OBKR-DIERS-KVOT                         
391400     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
391500     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
391600     MOVE NEJ                  TO OBKR-FLOBOK                             
391700     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
391800     MOVE NEJ                  TO OBKR-FLOBPRT                            
391900     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
392000     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
392100     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
392200                               TO OBKR-FLSLATT                            
392300     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
392400     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
392500        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
392600     ELSE                                                                 
392700        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
392800                               TO OBKR-REKSIFFR                           
392900     END-IF                                                               
393000     IF TILLKOMMANDE-RAD                                                  
393100        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
393200                               TO OBKR-IDARTNR-TILLK                      
393300        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
393400                               TO OBKR-REKSIFFR-TILLK                     
393500     ELSE                                                                 
393600        MOVE +0                TO OBKR-IDARTNR-TILLK                      
393700        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
393800     END-IF                                                               
393900     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
394000     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
394100     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
394200     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
394300     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
394400     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
394500     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
394600     IF NOT TILLKOMMANDE-RAD                                              
394700        MOVE AREG-KDERS        TO OBKR-KDERS                              
394800     END-IF                                                               
394900     MOVE ORAD-KDOI            TO OBKR-KDOI                               
395000     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
395100     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
395200     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
395300     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
395400     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
395500     MOVE +0                   TO OBKR-KVANNANT                           
395600     MOVE +0                   TO OBKR-KVAVBART                           
395700     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
395800     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
395900     MOVE +0                   TO OBKR-KVBEART-TILLK                      
396000     MOVE +0                   TO OBKR-KVPREAVB                           
396100     MOVE +0                   TO OBKR-KVPRERO                            
396200     IF ALLT-OK                                                           
396300        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
396400     ELSE                                                                 
396500        MOVE +0                TO OBKR-KVQPACK                            
396600     END-IF                                                               
396700     MOVE +0                   TO OBKR-KVRO                               
396800     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
396900     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
397000     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
397100     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
397200     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
397300     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
397400     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
397500     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
397600     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
397700     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
397800     MOVE +0                   TO OBKR-TIRODAT                            
397900     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
398000     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
398100       MOVE 20                 TO WS-SEKEL-9KOMPL                         
398200     ELSE                                                                 
398300       MOVE 19                 TO WS-SEKEL-9KOMPL                         
398400     END-IF                                                               
398500     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
398600     MOVE ORAD-TITPO           TO OBKR-TITPO                              
398700     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
398800     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
398900     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
399000     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
399100     MOVE SPACE                TO OBKR-IDBIL                              
399200                                                                          
399300     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
399400     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
399500     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
399600     MOVE ZERO                TO OBKR-TIDLEVDAT                           
399700     MOVE ORAD-PRAVCOST       TO OBKR-PRAVCOST                            
399800     MOVE ORAD-KDVALISO       TO OBKR-KDVALISO                            
399900*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
400000     IF OBKR-KDVALISO = SPACE                                             
400100        MOVE 'N/A'             TO OBKR-KDVALISO                           
400200     END-IF                                                               
400300     .                                                                    
400400     EJECT                                                                
400500 ECSB-SKAPA-TPO2 SECTION.                                                 
400600                                                                          
400700     MOVE WC-CDC-SE            TO W-IDDC-WDB3                             
400800                                  W-IDDC-WDB3-DEF                         
400900     MOVE OBKR-IDDISTR         TO W-IDDISTR-WDB3                          
401000                                  W-IDDISTR-WDB3-DEF                      
401100     MOVE OBKR-IDKUNDNR        TO W-IDKUNDNR-WDB3                         
401200     PERFORM IMS-GU-WDB301                                                
401300     IF SEGMENT-FINNS                                                     
401400        IF OBKR-KDORDKL = 1                                               
401500           MOVE DC-KDGENFRA-DO TO OBKR-KDFRAKT                            
401600        ELSE                                                              
401700           MOVE DC-KDGENFRA-MO TO OBKR-KDFRAKT                            
401800        END-IF                                                            
401900     END-IF                                                               
402000     MOVE +2                   TO OBKR-KDTPOTYP                           
402100     MOVE +71                  TO OBKR-KDORDBEK                           
402200     MOVE +0                   TO OBKR-KVPREAVB                           
402300     MOVE +0                   TO OBKR-KVPRERO                            
402400     MOVE IDPGM                TO OBKR-IDPGM                              
402500     IF DDGS-TPO-OBKR71                                                   
402600        MOVE OHUV-TITPO        TO OBKR-TITPO                              
402700        MOVE SPACE             TO OBKR-KDOI                               
402800     ELSE                                                                 
402900        MOVE OBKR-TIORDREG     TO OBKR-TITPO                              
403000        MOVE ORAD-KDOI         TO OBKR-KDOI                               
403100     END-IF                                                               
403200     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
403300     MOVE NEJ                  TO ALLT-SW                                 
403400     IF TILLKOMMANDE-RAD                                                  
403500        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
403600                               TO OBKR-KVBEART-TILLK                      
403700        IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                        
403800           TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                             
403900          MOVE +0              TO OBKR-DIERS-KVOT                         
404000        ELSE                                                              
404100          COMPUTE OBKR-DIERS-KVOT =                                       
404200                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
404300                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
404400        END-IF                                                            
404500     END-IF                                                               
404600     MOVE 'S'                  TO OBKR-SW                                 
404700     PERFORM IMS-08-ISRT-WLORQM01-WDQ101                                  
404800     ADD +1                    TO OBKR-IDSEKVNR                           
404900     .                                                                    
405000     EJECT                                                                
405100 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
405200                                                                          
405300     MOVE +1                   TO WS-INDEX-TILLK                          
405400     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
405500                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
405600        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
405700           IF OBKR-SKRIVEN                                                
405800              PERFORM IMS-08-ISRT-WLORQM01-WDQ101                         
405900              ADD +1              TO OBKR-IDSEKVNR                        
406000           END-IF                                                         
406100           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
406200           MOVE '4242KER4'     TO OBKR-IDPGM                              
406300           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
406400                               TO OBKR-IDARTNR-TILLK                      
406500           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
406600                               TO OBKR-REKSIFFR-TILLK                     
406700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
406800                               TO OBKR-KVBEART-TILLK                      
406900           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
407000              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
407100             MOVE +0              TO OBKR-DIERS-KVOT                      
407200           ELSE                                                           
407300             COMPUTE OBKR-DIERS-KVOT =                                    
407400                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
407500                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
407600           END-IF                                                         
407700           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
407800                               TO OBKR-BEERS                              
407900           MOVE ZEROES         TO OBKR-KVPREAVB                           
408000                                  OBKR-KVPRERO                            
408100                                                                          
408200           MOVE 'S'            TO OBKR-SW                                 
408300        END-IF                                                            
408400        ADD +1                 TO WS-INDEX-TILLK                          
408500     END-PERFORM                                                          
408600     IF WS-INDEX-TILLK = +1                                               
408700        MOVE +0                TO OBKR-KDERS                              
408800     END-IF                                                               
408900     .                                                                    
409000     EJECT                                                                
409100 ECU-KONTROLLERA-ENHETSLAST SECTION.                                      
409200                                                                          
409300     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
409400     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
409500     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
409600     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
409700     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
409800     MOVE ORAD-IDDC            TO LAST-IDDC                               
409900     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
410000     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
410100     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
410200     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
410300                                                                          
410400     CALL W411LAST USING LAST-W411LAST                                    
410500     .                                                                    
410600     EJECT                                                                
410700 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
410800     IF LAST-ADLAGOMR-UT = +0 AND                                         
410900        LAST-KVANTAL-UT  = +0 AND                                         
411000        LAST-KVBEART-UT  = +0                                             
411100*------------------------------------------------------------*            
411200*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
411300*------------------------------------------------------------*            
411400        PERFORM ECVA-FIXA-LAGEROMR-PLATS                                  
411500        PERFORM ECVB-REDIGERA-WOPS-AREA                                   
411600        PERFORM IMS-09-ISRT-ORQF-WDQ401                                   
411700        PERFORM UNTIL SEGMENT-FINNS                                       
411800           ADD +1                    TO ORAD-IDLOPNR                      
411900           PERFORM IMS-09-ISRT-ORQF-WDQ401                                
412000        END-PERFORM                                                       
412100     ELSE                                                                 
412200*------------------------------------------------------------*            
412300*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
412400*------------------------------------------------------------*            
412500                                                                          
412600        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
412700*------------------------------------------------------------*            
412800*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
412900*------------------------------------------------------------*            
413000                                                                          
413100           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
413200           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
413300                                    ORAD-KVPRERO                          
413400           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
413500           PERFORM ECVC-BERAEKNA-KVSLATT                                  
413600           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
413700           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
413800           MOVE ORAD-ADGANG          TO WS-ADGANG                         
413900           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
414000           PERFORM ECVB-REDIGERA-WOPS-AREA                                
414100           PERFORM IMS-09-ISRT-ORQF-WDQ401                                
414200           PERFORM UNTIL SEGMENT-FINNS                                    
414300              ADD +1                 TO ORAD-IDLOPNR                      
414400              PERFORM IMS-09-ISRT-ORQF-WDQ401                             
414500           END-PERFORM                                                    
414600*------------------------------------------------------------*            
414700*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
414800*------------------------------------------------------------*            
414900           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
415000           MOVE WS-ADGANG            TO ORAD-ADGANG                       
415100           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
415200                                                                          
415300           MOVE +0                   TO ORAD-KVBEART                      
415400           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
415500                                        ORAD-KVPREAVB                     
415600           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
415700           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
415800             CONTINUE                                                     
415900           ELSE                                                           
416000             IF LAST-ADGANG-UT > ZERO                                     
416100               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
416200             END-IF                                                       
416300           END-IF                                                         
416400           MOVE +0                   TO ORAD-KVPRERO                      
416500           MOVE 1.0000               TO ORAD-RERF-RAD                     
416600           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
416700           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
416800           PERFORM ECVB-REDIGERA-WOPS-AREA                                
416900           PERFORM IMS-09-ISRT-ORQF-WDQ401                                
417000           PERFORM UNTIL SEGMENT-FINNS                                    
417100              ADD +1                 TO ORAD-IDLOPNR                      
417200              PERFORM IMS-09-ISRT-ORQF-WDQ401                             
417300           END-PERFORM                                                    
417400        ELSE                                                              
417500*------------------------------------------------------------*            
417600*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
417700*------------------------------------------------------------*            
417800           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
417900           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
418000             CONTINUE                                                     
418100           ELSE                                                           
418200             IF LAST-ADGANG-UT > ZERO                                     
418300               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
418400             END-IF                                                       
418500           END-IF                                                         
418600           MOVE 1.0000            TO ORAD-RERF-RAD                        
418700           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
418800           PERFORM ECVB-REDIGERA-WOPS-AREA                                
418900           PERFORM IMS-09-ISRT-ORQF-WDQ401                                
419000           PERFORM UNTIL SEGMENT-FINNS                                    
419100              ADD +1              TO ORAD-IDLOPNR                         
419200              PERFORM IMS-09-ISRT-ORQF-WDQ401                             
419300           END-PERFORM                                                    
419400        END-IF                                                            
419500     END-IF                                                               
419600     .                                                                    
419700     EJECT                                                                
419800 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
419900                                                                          
420000     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
420100     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
420200     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
420300     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
420400     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
420500     MOVE 1                    TO ADRS-KDCALL-IN                          
420600     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
420700     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
420800     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
420900     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
421000                                                                          
421100     CALL W413ADRS USING ADRS-W413ADRS                                    
421200                                                                          
421300*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
421400     IF OHUV-IDKAMPRF > 0                                                 
421500       MOVE 8                  TO ORAD-ADLAGOMR                           
421600     ELSE                                                                 
421700       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
421800     END-IF                                                               
421900                                                                          
422000*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
422100     PERFORM S10-HAMTA-WDB6-INFO                                          
422200     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
422300     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
422400                                                                          
422500     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
422600     .                                                                    
422700     EJECT                                                                
422800 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
422900                                                                          
423000     MOVE +1                   TO AVSR-KDCALL                             
423100     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
423200     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
423300     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
423400     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
423500     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
423600     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
423700                                                                          
423800     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
423900     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
424000     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
424100     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
424200     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
424300     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
424400     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
424500     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
424600     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
424700     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
424800     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
424900     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
425000     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
425100                                                                          
425200     ADD +1                    TO WS-INDEX-WOPS                           
425300     .                                                                    
425400     EJECT                                                                
425500 ECVC-BERAEKNA-KVSLATT SECTION.                                           
425600                                                                          
425700     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
425800                                                                          
425900        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
426000                                                                          
426100        COMPUTE ORAD-KVSLATT ROUNDED =                                    
426200               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
426300     END-IF                                                               
426400     .                                                                    
426500     EJECT                                                                
428000****************************************************************          
428100*BEFORE ALLOTTING ORDERLINE,IF PART IS SUPERSEEDED, CHECK A & B           
428200*IN LDC1,LDC2 .CALL SDCA WITH KDCALL 2 TO CALC STOCK AND NO UPD           
428300****************************************************************          
428400 ECZ-CHECK-KDERS-IN-DC SECTION.                                           
428500                                                                          
428600     MOVE WS-INDEX-TILLK    TO WS-SAVE-INDEX                              
428700     MOVE +1                TO WS-INDEX-TILLK                             
428800                               IDDC-IX                                    
428900     MOVE NEJ               TO BAL-DC-FND-SW                              
429000                               TILLK-BAL-DC-FND-SW                        
429100                               KDERS-CHAIN-SW                             
429200     MOVE AREG-W411AREG-001 TO ORFK-W411AREG-001(WS-INDEX-MID)            
429300                                                                          
429400     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
429500                   TILK-IDARTNR(WS-INDEX-TILLK) = ZERO OR                 
429600                   BAL-DC-FND                          OR                 
429700                   TILLK-BAL-DC-FND                    OR                 
429800                   KDERS-CHAIN                                            
429900        PERFORM ECZD-CHECK-KDERS-CHAIN                                    
430000        IF KDERS-CHAIN-SW = NEJ                                           
430100          IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                      
430200             TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                
430300             TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                          
430400              PERFORM UNTIL W-GMT-IDDC-CLEAR(IDDC-IX) = SPACES OR         
430500                       IDDC-IX > 7 OR                                     
430600                       BAL-DC-FND OR                                      
430700                       TILLK-BAL-DC-FND                                   
430800                MOVE TILK-IDARTNR(WS-INDEX-TILLK)                         
430900                                         TO W-IDARTNR-SDCA                
431000                PERFORM ECZB-CALL-SDCA                                    
431100                IF SDCA-KDORDBEK > 0                                      
431200                   PERFORM ECZA-GET-TILLK-DATA                            
431300                   MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                
431400                                         TO W-IDARTNR-SDCA                
431500                   PERFORM ECZB-CALL-SDCA                                 
431600                   IF SDCA-KDORDBEK = 0                                   
431700                     MOVE JA             TO TILLK-BAL-DC-FND-SW           
431800                     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                       
431900                                         TO W-TILLK-DC                    
432000                   END-IF                                                 
432100                ELSE                                                      
432200                   MOVE JA               TO BAL-DC-FND-SW                 
432300                END-IF                                                    
432400                ADD +1                   TO IDDC-IX                       
432500              END-PERFORM                                                 
432600          END-IF                                                          
432700        END-IF                                                            
432800        ADD +1                          TO WS-INDEX-TILLK                 
432900     END-PERFORM                                                          
433000                                                                          
433100     MOVE WS-SAVE-INDEX                 TO WS-INDEX-TILLK                 
433200     MOVE ORFK-W411AREG-001(WS-INDEX-MID) TO AREG-W411AREG-001            
433210     MOVE ZEROES                        TO SDCA-KDORDBEK                  
433300                                                                          
433400     IF KDERS-CHAIN-SW = NEJ                                              
433500       IF BAL-DC-FND-SW = NEJ AND TILLK-BAL-DC-FND-SW = NEJ               
433600          MOVE '11'           TO W-TILLK-DC                               
433700       END-IF                                                             
433800     END-IF                                                               
433900     .                                                                    
434000     EJECT                                                                
434100*****************************************************************         
434200*IF A(KDERS 22) SUPERSEEDED BY B(KDERS-25) AND IS SUPERSEEDED             
434300*BY C1(KDERS 00) AND C2(KDERS 00),C1,C2 WILL BE SKIPPED AND               
434400*A WILL BE CHECKED FOR STOCKS, IF NOT OCC61                               
434500*****************************************************************         
434600 ECZD-CHECK-KDERS-CHAIN SECTION.                                          
434700                                                                          
434800     IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                           
434900        TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                     
435000        TILK-FLTILLK-X(WS-INDEX-TILLK) = NEJ AND                          
435100       (TILK-KDERS(WS-INDEX-TILLK) = 14 OR 15 OR 18 OR                    
435200                                     24 OR 25 OR 28)                      
435300          MOVE JA              TO KDERS-CHAIN-SW                          
435400     END-IF                                                               
435500     .                                                                    
435600     EJECT                                                                
435700 ECZA-GET-TILLK-DATA SECTION.                                             
435800                                                                          
435900     MOVE AREG-FLREFILL        TO W-FLREFILL-MAIN                         
436000     MOVE AREG-KDPRODSL        TO W-KDPRODSL-MAIN                         
436100     MOVE AREG-KDSORT          TO W-KDSORT-MAIN                           
436200     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-MAIN                        
436300     MOVE AREG-REDIRLEV        TO W-REDIRLEV-MAIN                         
436400                                                                          
436500     PERFORM ED-LAES-TILLK-DATA                                           
436600                                                                          
436700     MOVE AREG-FLREFILL        TO W-FLREFILL-REPL                         
436800     MOVE AREG-KDPRODSL        TO W-KDPRODSL-REPL                         
436900     MOVE AREG-KDSORT          TO W-KDSORT-REPL                           
437000     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-REPL                        
437100     MOVE AREG-REDIRLEV        TO W-REDIRLEV-REPL                         
437200                                                                          
437300     MOVE W-FLREFILL-MAIN      TO AREG-FLREFILL                           
437400     MOVE W-KDPRODSL-MAIN      TO AREG-KDPRODSL                           
437500     MOVE W-KDSORT-MAIN        TO AREG-KDSORT                             
437600     MOVE W-KVQPACK-1-MAIN     TO AREG-KVQPACK-1                          
437700     MOVE W-REDIRLEV-MAIN      TO AREG-REDIRLEV                           
437800     .                                                                    
437900     EJECT                                                                
438000 ECZB-CALL-SDCA   SECTION.                                                
438100                                                                          
438200     IF ORAD-IDARTNR = W-IDARTNR-SDCA                                     
438300        MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                         
438400        MOVE AREG-FLREFILL        TO SDCA-FLREFILL                        
438500        MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                        
438600        MOVE AREG-KDSORT          TO SDCA-KDSORT                          
438700        MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                       
438800        MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                        
438900     ELSE                                                                 
439000        MOVE W-IDARTNR-SDCA       TO SDCA-IDARTNR                         
439100        MOVE W-FLREFILL-REPL      TO SDCA-FLREFILL                        
439200        MOVE W-KDPRODSL-REPL      TO SDCA-KDPRODSL                        
439300        MOVE W-KDSORT-REPL        TO SDCA-KDSORT                          
439400        MOVE W-KVQPACK-1-REPL     TO SDCA-KVQPACK-1                       
439500        MOVE W-REDIRLEV-REPL      TO SDCA-REDIRLEV                        
439600     END-IF                                                               
439700                                                                          
439800     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
439900     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
440000     MOVE NEJ                  TO SDCA-FLORDSPE                           
440100     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                       
440200                               TO SDCA-IDDC                               
440300     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
440400     MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                            
440500     MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                           
440600     MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                            
440700     MOVE ORAD-KDORDING        TO SDCA-KDORDING                           
440800     MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                          
440900     MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                           
441000     MOVE +0                   TO SDCA-TIREPDAT                           
441100     MOVE +0                   TO SDCA-KVOKS-PREL                         
441200     MOVE +2                   TO SDCA-KDCALL                             
441300     MOVE +1                   TO SDCA-IXDCCLEAR                          
441400                                                                          
441500     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
441600                                       SDCA-WDB6-PCB                      
441700                                       SDCA-WDK9-PCB                      
441800                                       SDCA-WDR6-PCB                      
441900                                       SDCA-WDK6-PCB                      
442000                                       SDCA-WDQ4B-PCB                     
442100                                       SDCA-WDQ2-PCB                      
442200                                       SDCA-WDQ4-PCB                      
442300                                       SDCA-WDB6-2-PCB                    
442400                                       SDCA-WDK6-2-PCB                    
442500                                       SDCA-WDK7-2-PCB                    
442600                                       SDCA-WDK7-3-PCB                    
442700     .                                                                    
442800     EJECT                                                                
442900 S07-SPACE-SDCA-KDORDBEK  SECTION.                                        
443000                                                                          
443100     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
443200         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
443300     ELSE                                                                 
443400        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
443500           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
443600        ELSE                                                              
443700           IF SDCA-KDORDBEK > 0                                           
443800              MOVE ZEROES  TO SDCA-KDORDBEK                               
443900           END-IF                                                         
444000        END-IF                                                            
444100     END-IF                                                               
444200     .                                                                    
444300                                                                          
444400     EJECT                                                                
444500 ED-LAES-TILLK-DATA SECTION.                                              
444600                                                                          
444700     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
444800                               TO AREG-IDARTNR                            
444900                                                                          
445000     CALL W411AREG USING AREG-W411AREG                                    
445100                         AREG-WDK6-PCB                                    
445200                         AREG-WDK7-PCB                                    
445300     .                                                                    
445400     EJECT                                                                
445500 F-HOPPA-TILL-SVARSBILD SECTION.                                          
445600                                                                          
445700     MOVE MFS-KDMFSFOR           TO 4243-SPRAK                            
445800     IF MID-KDTRTYP = 'V'                                                 
445900        MOVE 'W4T243V '          TO 4243-TRANSKOD                         
446000        MOVE ALL '+'             TO 4243-IDDISTR-IN                       
446100                                    4243-IDKUNDNR-IN                      
446200                                    4243-IDORDNR-IN                       
446300        MOVE WS-IDDISTR          TO 4243-IDDISTR-UT                       
446400        MOVE WS-IDKUNDNR         TO 4243-IDKUNDNR-UT                      
446500        MOVE WS-IDORDNR          TO 4243-IDORDNR-UT                       
446600                                                                          
446700        PERFORM IMS-INSERT-4243V-MSG                                      
446800     ELSE                                                                 
446900        MOVE WS-IDDISTR          TO 4243-IDDISTR-IN                       
447000        MOVE WS-IDKUNDNR         TO 4243-IDKUNDNR-IN                      
447100        MOVE WS-IDORDNR          TO 4243-IDORDNR-IN                       
447200        MOVE MFS-RENSA-FAELT     TO 4243-IDDISTR-UT                       
447300                                    4243-IDKUNDNR-UT                      
447400                                    4243-IDORDNR-UT                       
447500                                                                          
447600        PERFORM IMS-INSERT-4243-MSG                                       
447700     END-IF                                                               
447800                                                                          
447900     MOVE JA                     TO HOPP                                  
448000     .                                                                    
448100     EJECT                                                                
448200 G-VISA-TOM-SIDA SECTION.                                                 
448300                                                                          
448400     MOVE +1 TO WS-INDEX                                                  
448500     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
448600       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
448700                                 MOD-KVBEART(WS-INDEX)                    
448800                                 MOD-TITPO(WS-INDEX)                      
448900                                 MOD-FLRESTN(WS-INDEX)                    
449000                                 MOD-FLSLATT(WS-INDEX)                    
449100                                 MOD-KDKVBRYT(WS-INDEX)                   
449200                                 MOD-BERADREF(WS-INDEX)                   
449300                                 MOD-FLORDING(WS-INDEX)                   
449400       ADD  +1 TO WS-INDEX                                                
449500     END-PERFORM                                                          
449600                                                                          
449700     MOVE MID-BEVOLREF           TO MOD-BEVOLREF                          
449800     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
449900     .                                                                    
450000     EJECT                                                                
450100 H-STARTA-BIPACKNINGEN SECTION.                                           
450200     COMPUTE 4297-LL = LENGTH OF 4297-MID-W4I29701 + 17                   
450300     MOVE MFS-KDMFSFOR           TO 4297-SPRAK                            
450400                                                                          
450500     MOVE W-IDDISTR              TO 4297-MID-IDDISTR                      
450600     MOVE W-IDKUNDNR             TO 4297-MID-IDKUNDNR                     
450700     MOVE W-IDKUNDRF             TO 4297-MID-IDKUNDRF                     
450800                                                                          
450900     MOVE OHUV-KDTPOTYP          TO 4297-MID-KDTPOTYP                     
451000     MOVE OHUV-KDORDKL           TO 4297-MID-KDORDKL                      
451100     MOVE OHUV-KDFAKTYP          TO 4297-MID-KDFAKTYP                     
451200     MOVE OHUV-IDKAMPRF          TO 4297-MID-IDKAMPRF                     
451300     MOVE OHUV-IDKONTO           TO 4297-MID-IDKONTO                      
451400     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
451500     MOVE OHUV-IDKST             TO 4297-MID-IDKST                        
451600     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
451700     MOVE OHUV-FLFORBI           TO 4297-MID-FLFORBI                      
451800     MOVE OHUV-IDORDER           TO 4297-MID-IDORDER                      
451900     MOVE OHUV-BEKUNDRF          TO 4297-MID-BEKUNDRF                     
452000     MOVE OHUV-TIREGDAT          TO 4297-MID-TIREGDAT                     
452100     MOVE OHUV-IDFTG             TO 4297-MID-IDFTG                        
452200     MOVE OHUV-BEVARREF          TO 4297-MID-BEVARREF                     
452300     MOVE OHUV-IDBIPREF          TO 4297-MID-IDBIPREF                     
452400     MOVE ARB-KDROPACK           TO 4297-MID-KDROPACK                     
452500     MOVE ARB-KDFRAKT            TO 4297-MID-KDFRAKT                      
452600     IF OHUV-IDDC-TVS NOT = SPACE                                         
452700       MOVE OHUV-IDDC-TVS        TO 4297-MID-IDDC                         
452800     ELSE                                                                 
452900       MOVE SPACE                TO 4297-MID-IDDC                         
453000     END-IF                                                               
453100                                                                          
453200     PERFORM IMS-INSERT-4297-MSG                                          
453300     MOVE JA                     TO HOPP                                  
453400     .                                                                    
453500     EJECT                                                                
453600 I-SKICKA-PRISFRAGA SECTION.                                              
453700                                                                          
453800     MOVE 1                      TO 3039-REQU-IDMSGVER                    
453900     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
454000     MOVE 'W4024200'             TO 3039-REQU-IDUSER                      
454100                                                                          
454200     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
454300     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
454400     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
454500     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
454600     IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'                           
454700       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
454800       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
454900       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
455000     END-IF                                                               
455100*    MOVE ORAD-IDPRQUES          TO 3039-MID-IDPRQUES                     
455200     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
455300                                                                          
455400     PERFORM S04-SKICKA-OPEN                                              
455500     PERFORM S04-SKICKA-MEDDELANDE                                        
455600     PERFORM S04-SKICKA-CLOSE                                             
455700     .                                                                    
455800     EJECT                                                                
455900 J-KOLLA-BEHORIGHET SECTION.                                              
456000     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
456100     MOVE '4242'            TO    SEC-IDTRANS                             
456200     MOVE WS-IDDISTR        TO    SEC-IDKEY                               
456300                                                                          
456400     CALL WSECURIT          USING SEC-IDUSER                              
456500                                  SEC-IDTRANS                             
456600                                  SEC-IDKEY                               
456700                                  SEC-KDSVAR                              
456800                                                                          
456900     IF SEC-KDSVAR = OBEHORIG                                             
457000        MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                 
457100        MOVE NEJ          TO ALLT-SW                                      
457200        MOVE OBEHORIG     TO SPAR-BEHORIGHETS-KONTR                       
457300     ELSE                                                                 
457400       CONTINUE                                                           
457500     END-IF                                                               
457600     .                                                                    
457700     EJECT                                                                
457800 Z-FINIT-INSERT-MSG SECTION.                                              
457900     IF SPAR-BEHORIGHETS-KONTR = OBEHORIG                                 
458000       CALL    WMEDKONV   USING MED-WMEDAREA                              
458100       MOVE    MED-MFSFEL TO    MOD-TEMFSFEL                              
458200       PERFORM MFS-RENSA-MOD-RADER                                        
458300     ELSE                                                                 
458400       IF MED-IDMFSFEL NOT = SPACE                                        
458500           CALL WMEDKONV USING MED-WMEDAREA                               
458600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
458700       END-IF                                                             
458800       IF NOT ALLT-OK                                                     
458900          PERFORM MFS-ROER-EJ-BILD                                        
459000       END-IF                                                             
459100     END-IF                                                               
459200                                                                          
459300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O24201 + 4                        
459400     PERFORM IMS-INSERT-MSG                                               
459500     .                                                                    
459600     EJECT                                                                
460900 S02-RENSA-TILLK-TAB SECTION.                                             
461000                                                                          
461100     MOVE +1              TO WS-INDEX-TILLK                               
461200     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
461300        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
461400        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
461500        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
461600        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
461700        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
461800        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
461900        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
462000        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
462100        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
462200        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
462300        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
462400        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
462500        INITIALIZE        TILK-DEAL-PR-LINE(WS-INDEX-TILLK)               
462600        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
462700        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
462800        ADD +1            TO WS-INDEX-TILLK                               
462900     END-PERFORM                                                          
463000     MOVE +1              TO WS-INDEX-TILLK                               
463100     .                                                                    
463200     EJECT                                                                
463300 S04-SKICKA-OPEN SECTION.                                                 
463400                                                                          
463500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
463600     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
463700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
463800                                                                          
463900     IF SEND-KDRC > 0                                                     
464000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
464100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
464200       DELIMITED BY SIZE INTO FELTEXT                                     
464300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
464400     END-IF                                                               
464500     .                                                                    
464600     SKIP3                                                                
464700 S04-SKICKA-MEDDELANDE SECTION.                                           
464800                                                                          
464900     MOVE 'PUT'                      TO SEND-KDFUNC                       
465000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
465100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
465200                                                                          
465300     IF SEND-KDRC > 0                                                     
465400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
465500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
465600       DELIMITED BY SIZE INTO FELTEXT                                     
465700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
465800     END-IF                                                               
465900     .                                                                    
466000     SKIP3                                                                
466100 S04-SKICKA-CLOSE SECTION.                                                
466200                                                                          
466300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
466400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
466500                                                                          
466600     IF SEND-KDRC > 0                                                     
466700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
466800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
466900       DELIMITED BY SIZE INTO FELTEXT                                     
467000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
467100     END-IF                                                               
467200     .                                                                    
467300     EJECT                                                                
467400 S05-DELETE-PRICE-Q-LINE SECTION.                                         
467500                                                                          
467600     IF DIST79-DEALER-PRICE                                               
467700       IF OBKR-IDPRQUES > ZERO                                            
467800         INITIALIZE PRQU-W335PRQU                                         
467900         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
468000         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
468100         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
468200         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
468300         MOVE 4                  TO PRQU-KDCALL                           
468400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
468500                                            PRQU-WDC7-PCB                 
468600                                            PRQU-SJKO-WDK6-PCB            
468700       END-IF                                                             
468800     END-IF                                                               
468900     .                                                                    
469000                                                                          
469100     EJECT                                                                
469100 S10-HAMTA-WDB6-INFO      SECTION.                                        
469100                                                                          
469100     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
469100       CONTINUE                                                           
469100     ELSE                                                                 
469100     IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                              
469100                                                                          
469100        MOVE 1 TO WS-CLDC-IX                                              
469100        PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                      
469100                      CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR                 
469100                      CLDC-IDDC (WS-CLDC-IX) = SPACE                      
469100           ADD 1 TO WS-CLDC-IX                                            
469100        END-PERFORM                                                       
469100                                                                          
469100     END-IF                                                               
469100     END-IF                                                               
469100     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
469100        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
469100        MOVE WS-IDDC TO W-IDDC-B6                                         
469100        PERFORM IMS-GU-WDB601                                             
469100     ELSE                                                                 
469100        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
469100     END-IF                                                               
469100     .                                                                    
469100     EJECT                                                                
471500 S20-WRONG-PICTURE-MESSAGE SECTION.                                       
471600     SKIP2                                                                
471700* *****************************************************                   
471800*                                                     *                   
471900* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
472000*                                                     *                   
472100* *****************************************************                   
472200     SKIP2                                                                
472300     MOVE JA                  TO HOPP-TILL-0504                           
472400     MOVE 'W0O50401'          TO MFS-IDMOD                                
472500     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
472600     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
472700     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
472800     PERFORM IMS-INSERT-MSG                                               
472900     .                                                                    
473000     EJECT                                                                
473100                                                                          
473200 MFS-RENSA-MOD-RADER SECTION.                                             
473300                                                                          
473400     MOVE MFS-RENSA-FAELT    TO  MOD-BEVOLREF                             
473500                                                                          
473600     MOVE +1 TO WS-INDEX                                                  
473700     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
473800       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
473900                                 MOD-KVBEART(WS-INDEX)                    
474000                                 MOD-TITPO(WS-INDEX)                      
474100                                 MOD-FLRESTN(WS-INDEX)                    
474200                                 MOD-FLSLATT(WS-INDEX)                    
474300                                 MOD-KDKVBRYT(WS-INDEX)                   
474400                                 MOD-BERADREF(WS-INDEX)                   
474500                                 MOD-FLORDING(WS-INDEX)                   
474600       ADD  +1 TO WS-INDEX                                                
474700     END-PERFORM                                                          
474800     .                                                                    
474900     EJECT                                                                
475000                                                                          
475100 MFS-ROER-EJ-BILD SECTION.                                                
475200                                                                          
475300     MOVE +1 TO WS-INDEX                                                  
475400     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
475500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(WS-INDEX)                    
475600                                 MOD-KVBEART(WS-INDEX)                    
475700                                 MOD-TITPO(WS-INDEX)                      
475800                                 MOD-FLRESTN(WS-INDEX)                    
475900                                 MOD-FLSLATT(WS-INDEX)                    
476000                                 MOD-KDKVBRYT(WS-INDEX)                   
476100                                 MOD-BERADREF(WS-INDEX)                   
476200                                 MOD-FLORDING(WS-INDEX)                   
476300       ADD  +1 TO WS-INDEX                                                
476400     END-PERFORM                                                          
476500     .                                                                    
476600     EJECT                                                                
476700* --- IMS SEKTIONER ---                                                   
476800                                                                          
476900 IMS-GET-MSG SECTION.                                                     
477000                                                                          
477100     MOVE '  QC' TO GODK-STATUSKODER                                      
477200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
477300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
477400     PERFORM IMS-STATUSKONTROLL                                           
477500     .                                                                    
477600     SKIP2                                                                
477700 IMS-INSERT-MSG SECTION.                                                  
477800                                                                          
477900     IF ENGLISH-TEXT                                                      
478000       MOVE 'N' TO MFS-KDHUVOMR                                           
478100     END-IF                                                               
478200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
478300     MOVE SPACE TO GODK-STATUSKODER                                       
478400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
478500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
478600     PERFORM IMS-STATUSKONTROLL                                           
478700     .                                                                    
478800     SKIP2                                                                
478900 IMS-INSERT-4243-MSG SECTION.                                             
479000                                                                          
479100     IF ENGLISH-TEXT                                                      
479200       MOVE 'N' TO MFS-KDHUVOMR                                           
479300     END-IF                                                               
479400     MOVE LOW-VALUE TO 4243-Z1 4243-Z2                                    
479500     MOVE SPACE TO GODK-STATUSKODER                                       
479600     CALL CBLTDLI USING ISRT 4243-PCB 4243-MSG-IO-AREA                    
479700     MOVE 4243-STATUS-CODE TO STATUS-WS                                   
479800     PERFORM IMS-STATUSKONTROLL                                           
479900     .                                                                    
480000     EJECT                                                                
480100 IMS-INSERT-4243V-MSG SECTION.                                            
480200                                                                          
480300     IF ENGLISH-TEXT                                                      
480400       MOVE 'N' TO MFS-KDHUVOMR                                           
480500     END-IF                                                               
480600     MOVE LOW-VALUE TO 4243-Z1 4243-Z2                                    
480700     MOVE SPACE TO GODK-STATUSKODER                                       
480800     CALL CBLTDLI USING ISRT 4243V-PCB 4243-MSG-IO-AREA                   
480900     MOVE 4243V-STATUS-CODE TO STATUS-WS                                  
481000     PERFORM IMS-STATUSKONTROLL                                           
481100     .                                                                    
481200     EJECT                                                                
481300 IMS-INSERT-4297-MSG SECTION.                                             
481400                                                                          
481500     MOVE LOW-VALUE TO 4297-Z1 4297-Z2                                    
481600     MOVE SPACE TO GODK-STATUSKODER                                       
481700     CALL CBLTDLI USING ISRT 4297-PCB 4297-MSG-IO-AREA                    
481800     MOVE 4297-STATUS-CODE TO STATUS-WS                                   
481900     PERFORM IMS-STATUSKONTROLL                                           
482000     .                                                                    
482100     EJECT                                                                
482200 IMS-01-GHU-ORQI-WDQ201 SECTION.                                          
482300                                                                          
482400     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
482500          DELIMITED BY SIZE INTO SSA1                                     
482600     MOVE '  GE'               TO GODK-STATUSKODER                        
482700     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
482800     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
482900     PERFORM IMS-STATUSKONTROLL                                           
483000     .                                                                    
483100     SKIP2                                                                
483200 IMS-02-REPL-ORQI-WDQ201 SECTION.                                         
483300                                                                          
483400     MOVE '    '               TO GODK-STATUSKODER                        
483500     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
483600     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
483700     PERFORM IMS-STATUSKONTROLL                                           
483800     .                                                                    
483900     EJECT                                                                
484000 IMS-03-GNP-ORQI-WDQ212 SECTION.                                          
484100                                                                          
484200     MOVE   'WLORQI12'         TO SSA1                                    
484300     MOVE '  GE'               TO GODK-STATUSKODER                        
484400     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
484500     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
484600     PERFORM IMS-STATUSKONTROLL                                           
484700     .                                                                    
484800     EJECT                                                                
484900 IMS-07-GU-ORQM-WDQ101 SECTION.                                           
485000                                                                          
485100     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
485200                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
485300          DELIMITED BY SIZE INTO SSA1                                     
485400     MOVE '  GE'               TO GODK-STATUSKODER                        
485500     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
485600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
485700     PERFORM IMS-STATUSKONTROLL                                           
485800     .                                                                    
485900     SKIP2                                                                
486000 IMS-08-ISRT-WLORQM01-WDQ101 SECTION.                                     
486100                                                                          
486200     MOVE 'WLORQM01 '          TO SSA1                                    
486300     MOVE '    '               TO GODK-STATUSKODER                        
486400     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
486500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
486600     PERFORM IMS-STATUSKONTROLL                                           
486700     .                                                                    
486800     SKIP3                                                                
486900 IMS-09-ISRT-ORQF-WDQ401 SECTION.                                         
487000                                                                          
487100     MOVE 'WLORQF01 '          TO SSA1                                    
487200     MOVE '  II'               TO GODK-STATUSKODER                        
487300     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
487400     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
487500     PERFORM IMS-STATUSKONTROLL                                           
487600     .                                                                    
487700     EJECT                                                                
487800 IMS-10-GU-WLARTM-WDK901 SECTION.                                         
487900                                                                          
488000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
488100          DELIMITED BY SIZE INTO SSA1                                     
488200     MOVE '  GE'               TO GODK-STATUSKODER                        
488300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
488400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
488500     PERFORM IMS-STATUSKONTROLL                                           
488600     .                                                                    
488700     SKIP2                                                                
488800 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
488900                                                                          
489000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
489100          DELIMITED BY SIZE INTO SSA1                                     
489200     MOVE '    '               TO GODK-STATUSKODER                        
489300     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
489400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
489500     PERFORM IMS-STATUSKONTROLL                                           
489600     .                                                                    
489700                                                                          
489800 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
489900                                                                          
490000     MOVE '    '               TO GODK-STATUSKODER                        
490100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
490200     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
490300     PERFORM IMS-STATUSKONTROLL                                           
490400     .                                                                    
490500     EJECT                                                                
490600 IMS-GU-WDB201 SECTION.                                                   
490600                                                                          
490600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
490600            DELIMITED BY SIZE INTO SSA1                                   
490600     MOVE '  GE'               TO GODK-STATUSKODER                        
490600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
490600     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
490600     PERFORM IMS-STATUSKONTROLL                                           
490600     .                                                                    
490600     SKIP2                                                                
492700 IMS-GU-WDB101 SECTION.                                                   
492800                                                                          
492900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
493000          DELIMITED BY SIZE INTO SSA1                                     
493100     MOVE '  '                 TO GODK-STATUSKODER                        
493200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
493300     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
493400     PERFORM IMS-STATUSKONTROLL                                           
493500     .                                                                    
493600                                                                          
493700 IMS-GU-WDB601    SECTION.                                                
493800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
493900          DELIMITED BY SIZE INTO SSA1                                     
494000     MOVE '  GE' TO GODK-STATUSKODER                                      
494100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
494200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
494300     PERFORM IMS-STATUSKONTROLL                                           
494400     IF SEGMENT-SAKNAS                                                    
494500        MOVE SPACE TO DCS-KDDC                                            
494600     END-IF                                                               
494700     .                                                                    
494800                                                                          
494900 IMS-GU-WDB301 SECTION.                                                   
495000                                                                          
495100     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
495200                    '!WDB301KY =' W-WDB301KY-DEF-X  ')'                   
495300          DELIMITED BY SIZE INTO SSA1                                     
495400     MOVE '  GE'              TO GODK-STATUSKODER                         
495500     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-AREA-WDB301 SSA1               
495600     MOVE WDB3-STATUS-CODE    TO STATUS-WS                                
495700     PERFORM IMS-STATUSKONTROLL                                           
495800     .                                                                    
495900                                                                          
496000                                                                          
496100 IMS-ISRT-WDR601 SECTION.                                                 
496200                                                                          
496300     MOVE 'WDR601' TO SSA1                                                
496400     MOVE '  II' TO GODK-STATUSKODER                                      
496500     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
496600     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
496700     PERFORM IMS-STATUSKONTROLL                                           
496800     .                                                                    
496900     EJECT                                                                
497000 IMS-STATUSKONTROLL SECTION.                                              
497100                                                                          
497200     SET STATUS-IX TO 1                                                   
497300     SEARCH GODK-STATUS                                                   
497400       AT END CALL FELLOG                                                 
497500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
497600     END-SEARCH                                                           
497700     .                                                                    
