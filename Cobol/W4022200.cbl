000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4022200.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   AUGUSTI 91.                                              
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR UPPLÄGGNING AV VORRADER OCH                  
001100*        FÖRBIORDERRADER PÅ ORDERKÖN.                                     
001200*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA                      
001300*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
001400*                                                                         
001500*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001600*        SVARSBILD - 4223.                                                
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
002700*                                                                         
002800*    E'TRACKER: 5444132 DATED 2007-09-18                                  
002900*    E'TRACKER: 2218613 DATED 2008-03-11                                  
003000*                                                                         
003100     EJECT                                                                
003200*                                                                         
003300*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
003400*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
003500*        PROGRAMMET LÄSER      WLWDQ2 (WDQ2)  ORDERHUVUD                  
003600*        PROGRAMMET LÄSER      WLORQA (WDQ3)  ORDERDELAR                  
003700*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
003800*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
003900*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
004000*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
004100*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
004200*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
004300*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
004400*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
004500*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
004600*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
004700*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
004800*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
004900*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
005000*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
005100*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
005200*        PROGRAMMET LÄSER      WDGX4542 (WDR4)  VORKÖN                    
005300*                                                                         
005400*    INDATA.                                                              
005500*        TRANSAKTION: W4T222                                              
005600*        MID:         W4I22201                                            
005700*    UTDATA.                                                              
005800*        MOD:         W4O22201                                            
005900*                                                                         
006000*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
006100*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
006200     EJECT                                                                
006300 ENVIRONMENT DIVISION.                                                    
006400                                                                          
006500 DATA DIVISION.                                                           
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800*    -- CHECKED BY WY2000                                                 
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(08)   VALUE 'W4022200'.            
007100 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
007200 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
007300 77  HOPP                        PIC X(1)   VALUE 'N'.                    
007400 77  HOPP-TILL-0504              PIC X(1)   VALUE 'N'.                    
007500 77  JA                          PIC X(1)   VALUE 'J'.                    
007600 77  YES                         PIC X(1)   VALUE 'Y'.                    
007700 77  NEJ                         PIC X(1)   VALUE 'N'.                    
007800 77  SPEC-FORBI                  PIC X(1)   VALUE 'S'.                    
007900*                                                                         
008000*    ---FOR MOD0504-IDTRANS                                               
008100 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
008200                                                                          
008300 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
008400*01  -COPY WWDCKONS                                                       
008500                                                                          
008600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
008700 77  HFAK-TAB-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
008800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
008900 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
009000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
009100 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
009200 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
009300 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
009400 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
009500 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
009600 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
009700 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009800 77  WS-INDEX-WOPS-MAX           PIC S9(9) COMP SYNC VALUE +100.          
009900 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
010000 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
010100 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
010200 77  WS-HFAK-REF-X10             PIC X(10)   VALUE SPACE.                 
010300 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
010400 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
010500 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
010600 77  WS-KVBEART-NUM              PIC  9(6)   VALUE ZERO.                  
010700 77  W-KDORDBEK                  PIC S9(2)   VALUE ZERO.                  
010800 77  WS-VOR-KDORDBEK             PIC S9(2)   VALUE ZERO.                  
010900 77  W-KDTPOTYP                  PIC S9      VALUE ZERO.                  
011000                                                                          
011100 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
011200 77  MAX-TVS-IX                  PIC S9(3)  VALUE +16 COMP-3.             
011300 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
011400     EJECT                                                                
011500                                                                          
011600 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
011700 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011800     03 WS-TIHHMM                PIC 9(4).                                
011900     03 FILLER                   PIC 9(2).                                
012000                                                                          
012100 01  WS-BERADREF-RED             PIC X(15).                               
012200 01  FILLER REDEFINES WS-BERADREF-RED.                                    
012300     03 WS-IDORDNR-NUM           PIC  9(5).                               
012400     03 FILLER                   PIC  X(10).                              
012500                                                                          
012600 01  ARTIKEL-REKSIFFRA.                                                   
012700     03  ARTIKEL-POS-1-2         PIC X(2)    VALUE ZERO.                  
012800     03  ARTIKEL-POS-3-11        PIC X(9)    VALUE ZERO.                  
012900                                                                          
013000 01  WS-PRARTNTO-NUM             PIC 9(7)V9(2).                           
013100 01  WS-PRARTNTO-RED             PIC 9(7).9(2).                           
013200 01  PRARTNTO-FILLER REDEFINES WS-PRARTNTO-RED.                           
013300     03  WS-PRARTNTO-ALFA        PIC X(10).                               
013400                                                                          
013500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013600     88  ALLT-OK                             VALUE 'J'.                   
013700                                                                          
013800 77  TVS-DC-SW                   PIC X       VALUE 'N'.                   
013900     88  TVS-DC-ALLOC                        VALUE 'J'.                   
014000                                                                          
014100 77  VOR-MAN-PRIS-SW             PIC X       VALUE 'N'.                   
014200     88  VOR-MAN-PRIS                        VALUE 'J'.                   
014300                                                                          
014400 77  TILLK-SW                    PIC X       VALUE 'N'.                   
014500     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
014600     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
014700                                                                          
014800 77  BIPA-SW                     PIC X       VALUE 'N'.                   
014900     88  BIPA-JA                             VALUE 'J'.                   
015000                                                                          
015100 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
015200     88  SVARSBILD                           VALUE 'J'.                   
015300                                                                          
015400 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
015500     88  KOLLA-ERS                           VALUE 'J'.                   
015600                                                                          
015700 77  OBKR-SW                     PIC X       VALUE 'N'.                   
015800     88  SKRIV-OBKR                          VALUE 'J'.                   
015900     88  OBKR-SKRIVEN                        VALUE 'S'.                   
016000                                                                          
016100 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
016200     88  EGET-CL-RAD                         VALUE 'J'.                   
016300                                                                          
016400 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
016500     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
016600                                                                          
016700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
016800     88  EGEN-MID                            VALUE '4222'.                
016900     88  GODK-MID                            VALUE '4221' '4222'          
017000                                                   '4223' '4224'          
017100                                                   'V412'.                
017200     EJECT                                                                
017300 01  WS-ALFA-1.                                                           
017400     03  WS-NUM-1                PIC 9(1).                                
017500 01  WS-ALFA-6.                                                           
017600     03  WS-NUM-6                PIC 9(6).                                
017700 01  WS-ALFA-7.                                                           
017800     03  WS-NUM-7                PIC 9(7).                                
017900 01  WS-ALFA-8.                                                           
018000     03  WS-NUM-8                PIC 9(8).                                
018100     03  FILLER REDEFINES WS-NUM-8.                                       
018200         05  WS-NUM-1--4         PIC 9(4).                                
018300         05  WS-NUM-5--8         PIC 9(4).                                
018400 01  WS-NUM-2V3                  PIC 9(2)V9(3).                           
018500 01  FILLER REDEFINES WS-NUM-2V3.                                         
018600     03  WS-NUM-1--2             PIC 9(2).                                
018700     03  WS-NUM-3--5             PIC 9(3).                                
018800                                                                          
018900 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
019000 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
019100     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
019200     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
019300                                                                          
019400 01  W-GMT-IDDC-CLEAR-GRP.                                                
019500*                                 GRUPP AV IDDC-CLEAR                     
019600     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
019700                                 PIC X(2)    VALUE SPACE.                 
019800     EJECT                                                                
019900                                                                          
020000 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
020100*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
020200     EJECT                                                                
020300*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
020400     EJECT                                                                
020500*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
020600*    ----DISTR-DEALER-PRICE----                                           
020700     EJECT                                                                
020800                                                                          
020900 01 FILLER                    PIC X(16) VALUE 'TILLKOMMANDE TAB'.         
021000                                                                          
021100*    -COPY W411TILK                                                       
021200     EJECT                                                                
021300*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
021400                                                                          
021500*   -COPY W413WHFA                                                        
021600     EJECT                                                                
021700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021800 01  GENERELLA-SUBPROGRAM.                                                
021900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
022000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
022300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
022500*                                                                         
022600*                                                                         
022700*                                                                         
022800 01  GEMENSAMMA-SUBPROGRAM.                                               
022900     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
023000*        PRISTILLÄMPNING                                                  
023100     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
023200*        HÄMTA PRISFRÅGENR                                                
023300     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
023400*        DEALER PRISFRÅGABEHANDLING                                       
023500     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
023600*        LÄSNING ARTIKELREGISTER                                          
023700     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
023800*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
023900     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
024000*        KONTROLL DIREKTLEVERANS                                          
024100     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
024200*        DATA TILL DEL NOTE NDC                                           
024300     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
024400*        KONTROLL TPO4 - KAMPANJ                                          
024500     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
024600*        KONTROLL ERSÄTTNINGAR                                            
024700     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
024800*        KONTROLL KVANTANPASSNING                                         
024900     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
025000*        KONTROLL ENHETSLAST                                              
025100     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
025200*        FORMELLA KONTROLLER AV INDATA                                    
025300     EJECT                                                                
025400     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
025500*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
025600     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
025700*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
025800     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
025900*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
026000     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
026100*        BERÄKNA RANSONERING                                              
026200     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
026300*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
026400     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
026500*        KONTROLL SPÄRRAR                                                 
026600     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
026700*        KONTROLL STORA UTTAG                                             
026800     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
026900*        KONTROLL TPO1                                                    
027000     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
027100*        KONTROLL TPO2                                                    
027200     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
027300*        KONTROLL RELS                                                    
027400     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
027500*        WOPS RADBEHANDLING                                               
027600     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
027700*        OMVANDLING AV LAGOMR + PLATS                                     
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028000*   -COPY WMSGINIT                                                        
028100     EJECT                                                                
028200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
028300*   -COPY WMEDAREA                                                        
028400     SKIP3                                                                
028500 01  FILLER.                                                              
028600   03  FELMEDD-AREA.                                                      
028700     05  FELMEDD-ENGLISH.                                                 
028800       10  FILLER                PIC X(50)                                
028900     VALUE '622 4222 NOT AVAILABLE ONLY VALID FROM 4221'.                 
029000     EJECT                                                                
029100 01  MESSAGE-CODES.                                                       
029200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
029300     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
029400     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
029500     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
029600     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
029700     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
029800     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
029900     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
030000     EJECT                                                                
030100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
030200 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
030300*   -COPY W335PRIS                                                        
030400     EJECT                                                                
030500 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
030600*   -COPY W335PRNO                                                        
030700     EJECT                                                                
030800 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
030900*   -COPY W335PRQU                                                        
031000     EJECT                                                                
031100 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
031200*   -COPY W411AREG                                                        
031300     EJECT                                                                
031400 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
031500*   -COPY W411ARTM                                                        
031600     EJECT                                                                
031700 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
031800*   -COPY W411DLEV                                                        
031900     EJECT                                                                
032000 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
032100*   -COPY W411DNOT                                                        
032200     EJECT                                                                
032300 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
032400*   -COPY W411KAMP                                                        
032500     EJECT                                                                
032600 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
032700*   -COPY W411KERS                                                        
032800     EJECT                                                                
032900 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
033000*   -COPY W411KVAN                                                        
033100     EJECT                                                                
033200 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
033300*   -COPY W411LAST                                                        
033400     EJECT                                                                
033500 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
033600*   -COPY W411ORFK                                                        
033700     EJECT                                                                
033800 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
033900*   -COPY W411CDCA                                                        
034000     EJECT                                                                
034100 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
034200*   -COPY W411NDCA                                                        
034300     EJECT                                                                
034400 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
034500*   -COPY W411XDCA                                                        
034600     EJECT                                                                
034700 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
034800*   -COPY W411XDK7 -PRE NDCA-                                             
034900     EJECT                                                                
035000 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
035100*   -COPY W411RANS                                                        
035200     EJECT                                                                
035300 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
035400*   -COPY W411SPAR                                                        
035500     EJECT                                                                
035600 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
035700*   -COPY W411SDCA                                                        
035800     EJECT                                                                
035900 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
036000*   -COPY W411STOR                                                        
036100     EJECT                                                                
036200 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
036300*   -COPY W411TPO1                                                        
036400     EJECT                                                                
036500 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
036600*   -COPY W411TPO2                                                        
036700     EJECT                                                                
036800 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
036900*   -COPY W411RELS                                                        
037000     EJECT                                                                
037100 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
037200*   -COPY W413AVSR                                                        
037300     SKIP2                                                                
037400 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
037500*   -COPY W413ADRS                                                        
037600     EJECT                                                                
037700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
037800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
037900     SKIP3                                                                
038000*01  MID -COPY W4I22201                                                   
038100                                                                          
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
038400     SKIP3                                                                
038500*01  -COPY WMSGAREA                                                       
038600     EJECT                                                                
038700*    03  MOD -COPY W4O22201   -RED MSG-AREA.                              
038800*   TO RETURN TO MAIN MENU                                                
038900*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
039000     EJECT                                                                
039100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
039200     SKIP3                                                                
039300*01  -COPY WMFSAREA                                                       
039400     EJECT                                                                
039500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039600*                                                                         
039700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039800     SKIP3                                                                
039900 01  NYCKLAR-TILL-DLI.                                                    
040000                                                                          
040100     03  W-IDGMTREF-X.                                                    
040200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
040300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
040400         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
040500                                                                          
040600     03  W-IDDC-X.                                                        
040700         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
040800                                                                          
040900     03  W-IDARTNR-X.                                                     
041000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
041100                                                                          
041200     03  W-IDARTNR-CROSS-X.                                               
041300         05  W-IDARTNR-CROSS     PIC S9(9)   VALUE ZERO COMP-3.           
041400                                                                          
041500     EJECT                                                                
041600                                                                          
041700     03  W-WDQ101KY-MIN-X.                                                
041800         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
041900         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
042000         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
042100         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
042200         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
042300                                                                          
042400     03  W-WDQ101KY-MAX-X.                                                
042500         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
042600         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
042700         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
042800         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
042900         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
043000                                                                          
043100     03  W-IDHTYP-X.                                                      
043200         05  W-IDHTYP            PIC  X(4)   VALUE '4541'.                
043300         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
043400                                                                          
043500     03  W-4542KEY-MIN-X.                                                 
043600         05  W-IDDISTR-4542-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
043700         05  W-IDANSK-4542-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
043800         05  W-IDARTNR-4542-MIN  PIC S9(9)   VALUE ZERO COMP-3.           
043900         05  W-IDLOPNR-4542-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
044000         05  W-IDORDER-4542-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
044100                                                                          
044200     03  W-KDVORATG-X.                                                    
044300         05  W-KDVORATG         PIC  X      VALUE '1'.                    
044400                                                                          
044500     03  W-4542KEY-MAX-X.                                                 
044600         05  W-IDDISTR-4542-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
044700         05  W-IDANSK-4542-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
044800         05  W-IDARTNR-4542-MAX  PIC S9(9)   VALUE ZERO COMP-3.           
044900         05  W-IDLOPNR-4542-MAX  PIC S9(3)   VALUE ZERO COMP-3.           
045000         05  W-IDORDER-4542-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
045100                                                                          
045200     03  W-WDA6F1KY-MIN-X.                                                
045300         05  W-IDDISTR-A6F1-MIN       PIC S9(5) VALUE ZERO COMP-3.        
045400         05  W-IDKUNDNR-A6F1-MIN      PIC S9(7) VALUE ZERO COMP-3.        
045500         05  W-TIREGDAT-AVV9-A6F1-MIN PIC S9(7) VALUE ZERO COMP-3.        
045600         05  W-TIREGTID-AVV9-A6F1-MIN PIC S9(9) VALUE ZERO COMP-3.        
045700                                                                          
045800     03  W-WDA6F1KY-MAX-X.                                                
045900         05  W-IDDISTR-A6F1-MAX       PIC S9(5) VALUE ZERO COMP-3.        
046000         05  W-IDKUNDNR-A6F1-MAX      PIC S9(7) VALUE ZERO COMP-3.        
046100         05  W-TIREGDAT-AVV9-A6F1-MAX PIC S9(7) VALUE ZERO COMP-3.        
046200         05  W-TIREGTID-AVV9-A6F1-MAX PIC S9(9) VALUE ZERO COMP-3.        
046300                                                                          
046400     03  W-WDA6FKY-X.                                                     
046500         05  W-IDDISTR-A6F            PIC S9(5) VALUE ZERO COMP-3.        
046600         05  W-IDKUNDNR-A6F           PIC S9(7) VALUE ZERO COMP-3.        
046700         05  W-TIREGDAT-AVV9-A6F      PIC S9(7) VALUE ZERO COMP-3.        
046800         05  W-TIREGTID-AVV9-A6F      PIC S9(9) VALUE ZERO COMP-3.        
046900                                                                          
047000     03  W-IDGMT-X.                                                       
047100         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
047200         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
047300*                                                                         
047400     03  W-WDB101KY-X.                                                    
047500         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
047600         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
047700                                                                          
047800     03  W-IDDC-B6-X.                                                     
047900         05 W-IDDC-B6                  PIC X(2).                          
048000*                                                                         
048100     EJECT                                                                
048200                                                                          
048300*    --- STATUS-KOD FRÅN IMS                                              
048400 01  STATUS-WS                   PIC XX.                                  
048500     88  SEGMENT-FINNS                       VALUE '  '.                  
048600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048800     88  BASEN-SLUT                          VALUE 'GB'.                  
048900     SKIP2                                                                
049000 01  GODK-STATUSKODER.                                                    
049100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049200                                                                          
049300 01  SSA1                        PIC X(96).                               
049400 01  SSA2                        PIC X(130).                              
049500     EJECT                                                                
049600*    --- IMS FUNKTIONSKODER                                               
049700*01  -COPY W0003                                                          
049800     EJECT                                                                
049900*    ---  DLI INPUT-OUTPUT AREA                                           
050000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
050100     SKIP3                                                                
050200 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
050300 01  DLI-IO-AREA-OBKR.                                                    
050400     03  WDQ101.                                                          
050500*        05  -COPY WDQ101                                                 
050600     EJECT                                                                
050700 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
050800 01  DLI-IO-AREA-OHUV.                                                    
050900     03  WDQ201.                                                          
051000*        05  -COPY WDQ201                                                 
051100     EJECT                                                                
051200 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
051300 01  DLI-IO-AREA-ARB.                                                     
051400     03  WDQ212.                                                          
051500*        05  -COPY WDQ212                                                 
051600     EJECT                                                                
051700 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
051800 01  DLI-IO-AREA-ORAD.                                                    
051900     03  WDQ401.                                                          
052000*        05  -COPY WDQ401                                                 
052100     EJECT                                                                
052200 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
052300 01  DLI-IO-AREA-ART.                                                     
052400     03  WLARTM01.                                                        
052500*        05  -COPY WDK901                                                 
052600     EJECT                                                                
052700                                                                          
052800 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
052900 01  DLI-IO-AREA-WDK7.                                                    
053000*    03  -COPY WDK711                                                     
053100     EJECT                                                                
053200                                                                          
053300 01  FILLER                      PIC X(16)   VALUE 'VOR-AREA   '.         
053400 01  DLI-IO-AREA-VOR.                                                     
053500     03  WL454111.                                                        
053600*        05  -COPY WDGX4542                                               
053700     EJECT                                                                
053800 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
053900 01  DLI-IO-AREA-WDB201.                                                  
054000*    03  -COPY WDB201                                                     
054100     EJECT                                                                
054200 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
054300 01  DLI-IO-AREA-WDB101.                                                  
054400     03  WLBETC01.                                                        
054500         05  -COPY WDB101                                                 
054600     EJECT                                                                
054700 01  FILLER                      PIC X(16)   VALUE 'WDA601-AREA'.         
054800 01  DLI-IO-AREA-WDA601.                                                  
054900     03  WDA601.                                                          
055000*        05  -COPY WDA601                                                 
055100     EJECT                                                                
055200 01  FILLER                      PIC X(16)   VALUE 'WDA6F1-AREA'.         
055300 01  DLI-IO-AREA-WDA6F1.                                                  
055400     03  WDA6F1.                                                          
055500*        05  -COPY WDA6F1                                                 
055600     EJECT                                                                
055700 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
055800 01  DLI-IO-AREA-WDK611.                                                  
055900     03  WDK611.                                                          
056000*        05  -COPY WDK611                                                 
056100                                                                          
056200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
056300 01   DLI-IO-AREA-B601    PIC X(400).                                     
056400 01   FILLER REDEFINES DLI-IO-AREA-B601.                                  
056500*     03  -COPY WDB601                                                    
056600     EJECT                                                                
056700                                                                          
056800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF501'.          
056900 01  DLI-IO-WDF501.                                                       
057000*    03  -COPY WDF501                                                     
057100                                                                          
057200 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
057300 01   DLI-IO-AREA-R601.                                                   
057400*     03  -COPY WDR601                                                    
057500*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
057600                                                                          
057700 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
057800 01  4223-MSG-IO-AREA.                                                    
057900     03  4223-LL               PIC S9(4)  VALUE +107 COMP SYNC.           
058000     03  4223-Z1               PIC X.                                     
058100     03  4223-Z2               PIC X.                                     
058200     03  4223-TRANSKOD         PIC X(8)   VALUE 'W4T223  '.               
058300     03  4223-IDTRANS          PIC X(4)   VALUE '4222'.                   
058400     03  4223-SPRAK            PIC X.                                     
058500     03  4223-IDDISTR-IN       PIC X(4).                                  
058600     03  4223-IDKUNDNR-IN      PIC X(6).                                  
058700     03  4223-IDORDNR-IN       PIC X(5).                                  
058800     03  4223-IDDISTR-UT       PIC X(4).                                  
058900     03  4223-IDKUNDNR-UT      PIC X(6).                                  
059000     03  4223-IDORDNR-UT       PIC X(5).                                  
059100**BELOW FIELDS ADDDED FOR CALL FROM W41218 BMP                            
059200     03  4223-KDORDKL-UT       PIC X.                                     
059300     03  4223-FLANNULL         PIC X.                                     
059400     03  4223-IDARTNR-NEXT     PIC 9(9).                                  
059500     03  4223-IDLOPNR-NEXT     PIC 9(3).                                  
059600     03  4223-IDSEKVNR-NEXT    PIC 9(3).                                  
059700     03  4223-IDDC-NEXT        PIC X(2).                                  
059800     03  4223-KDORDBEK-NEXT    PIC 9(2).                                  
059900     03  4223-RAD              OCCURS 13 TIMES.                           
060000         05 4223-KDORDBEK      PIC 9(2).                                  
060100         05 4223-KDBEHX        PIC X.                                     
060200     EJECT                                                                
060300 01  4297-MSG-IO-AREA.                                                    
060400     03  4297-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
060500     03  4297-Z1               PIC X.                                     
060600     03  4297-Z2               PIC X.                                     
060700     03  4297-TRANSKOD         PIC X(8)   VALUE 'W4T297X '.               
060800     03  4297-IDTRANS          PIC X(4)   VALUE '4222'.                   
060900     03  4297-SPRAK            PIC X.                                     
061000*    03  -COPY W4I29701  -PRE 4297-                                       
061100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
061200     SKIP3                                                                
061300 01  -COPY WZ01SEND                                                       
061400     EJECT                                                                
061500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
061600     SKIP3                                                                
061700 01  SEND-AREA.                                                           
061800*    03  -COPY WZ01REQU  -PRE 3039-                                       
061900*    03  -COPY W30391I1  -PRE 3039-                                       
062000     EJECT                                                                
062100 LINKAGE SECTION.                                                         
062200                                                                          
062300*01  -COPY W0009   -PRE MSG-                                              
062400                                                                          
062500 01  AVSR-ALT-PCB                PIC X.                                   
062600                                                                          
062700 01  2109-PCB                    PIC X.                                   
062800     EJECT                                                                
062900                                                                          
063000*01  -COPY W0009   -PRE PRQRY-                                            
063100*01  -COPY W0009   -PRE 4223-                                             
063200*01  -COPY W0009   -PRE 4223V-                                            
063300*01  -COPY W0009   -PRE 4297-                                             
063400     SKIP2                                                                
063500     EJECT                                                                
063600*01  -COPY W0008   -PRE USEA-                                             
063700     05  FILLER                  PIC X.                                   
063800     SKIP2                                                                
063900*01  -COPY W0008   -PRE WDQ1-                                             
064000     05  FILLER                  PIC X.                                   
064100     SKIP2                                                                
064200*01  -COPY W0008   -PRE WDQ2-                                             
064300     05  FILLER                  PIC X.                                   
064400     EJECT                                                                
064500*01  -COPY W0008   -PRE WDQ4-                                             
064600     05  FILLER                  PIC X.                                   
064700     EJECT                                                                
064800*01  -COPY W0008   -PRE ARTM-                                             
064900     05  FILLER                  PIC X.                                   
065000     EJECT                                                                
065100*01  -COPY W0008   -PRE WDK7-                                             
065200     05  FILLER                  PIC X.                                   
065300     EJECT                                                                
065400*01  -COPY W0008   -PRE 4541-                                             
065500     05  FILLER                  PIC X.                                   
065600     EJECT                                                                
065700*01  -COPY W0008   -PRE WDB2-                                             
065800     05  FILLER                  PIC X.                                   
065900     EJECT                                                                
066000*01  -COPY W0008   -PRE WDB1-                                             
066100     05  FILLER                  PIC X.                                   
066200     EJECT                                                                
066300*01  -COPY W0008   -PRE WDB6-                                             
066400     05  FILLER                  PIC X.                                   
066500     EJECT                                                                
066600*01  -COPY W0008   -PRE WDK6-                                             
066700     05  FILLER                  PIC X.                                   
066800     EJECT                                                                
066900*01  -COPY W0008   -PRE WDA6F1-                                           
067000     05  FILLER                  PIC X.                                   
067100     EJECT                                                                
067200*01  -COPY W0008   -PRE WDA6F-                                            
067300     05  FILLER                  PIC X.                                   
067400     EJECT                                                                
067500*01  -COPY W0008   -PRE WDF5-                                             
067600     05  FILLER                  PIC X.                                   
067700     EJECT                                                                
067800*01  -COPY W0008   -PRE WDR6-                                             
067900     05  FILLER                  PIC X.                                   
068000     EJECT                                                                
068100 01  PRIS-ARTC-PCB               PIC X.                                   
068200 01  PRIS-WDK7-PCB               PIC X.                                   
068300 01  PRIS-GMTA-PCB               PIC X.                                   
068400 01  PRIS-BETA-PCB               PIC X.                                   
068500 01  PRIS-GPRIA-PCB              PIC X.                                   
068600 01  PRIS-GPRIB-PCB              PIC X.                                   
068700 01  PRIS-COST-WDK6-PCB          PIC X.                                   
068800 01  PRIS-COST-WDK7-PCB          PIC X.                                   
068900 01  PRIS-COST-WDF1-PCB          PIC X.                                   
069000 01  PRIS-COST-9305-PCB          PIC X.                                   
069100 01  PRIS-COST-WDK72-PCB         PIC X.                                   
069200 01  PRIS-COST-WDB6-PCB          PIC X.                                   
069300 01  PRNO-3107-PCB               PIC X.                                   
069400 01  PRQU-WDG2-PCB               PIC X.                                   
069500 01  PRQU-WDC7-PCB               PIC X.                                   
069600 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
069700 01  AREG-WDK6-PCB               PIC X.                                   
069800 01  AREG-WDK7-PCB               PIC X.                                   
069900 01  ARTM-ARTM-PCB               PIC X.                                   
070000 01  DLEV-LEVF-PCB               PIC X.                                   
070100 01  DLEV-LEVG-PCB               PIC X.                                   
070200 01  DLEV-LEVA-PCB               PIC X.                                   
070300 01  DLEV-ARTS-PCB               PIC X.                                   
070400 01  DLEV-WDB6-PCB               PIC X.                                   
070500 01  SPAR-WDF8-PCB               PIC X.                                   
070600 01  SPAR-WDF8A-PCB              PIC X.                                   
070700 01  SPAR-WDK6-PCB               PIC X.                                   
070800 01  DNOT-ORQP-PCB               PIC X.                                   
070900 01  DNOT-ORQP2-PCB              PIC X.                                   
071000 01  DNOT-ORQP3-PCB              PIC X.                                   
071100 01  DNOT-4013-PCB               PIC X.                                   
071200 01  DNOT-BENA-PCB               PIC X.                                   
071300 01  KAMP-ORDP-PCB               PIC X.                                   
071400 01  KAMP-ZZAC-PCB               PIC X.                                   
071500 01  KAMP-WDM2-PCB               PIC X.                                   
071600 01  KERS-ARTC-PCB               PIC X.                                   
071700 01  KERS-ERSA-PCB               PIC X.                                   
071800 01  NDCA-USEA-PCB               PIC X.                                   
071900 01  NDCA-WDK7-PCB               PIC X.                                   
072000 01  NDCA-WDL6-PCB               PIC X.                                   
072100 01  NDCA-WDB6-PCB               PIC X.                                   
072200 01  SDCA-ARTS-PCB               PIC X.                                   
072300 01  SDCA-WDB6-PCB               PIC X.                                   
072400 01  SDCA-WDK9-PCB               PIC X.                                   
072500 01  SDCA-WDR6-PCB               PIC X.                                   
072600 01  SDCA-WDK6-PCB               PIC X.                                   
072700 01  SDCA-WDQ4B-PCB              PIC X.                                   
072800 01  SDCA-WDQ2-PCB               PIC X.                                   
072900 01  SDCA-WDQ4-PCB               PIC X.                                   
073000 01  SDCA-WDB6-2-PCB             PIC X.                                   
073100 01  SDCA-WDK6-2-PCB             PIC X.                                   
073200 01  SDCA-WDK7-2-PCB             PIC X.                                   
073300 01  SDCA-WDK7-3-PCB             PIC X.                                   
073400 01  CDCA-ARTM-PCB               PIC X.                                   
073500 01  CDCA-INLB-PCB               PIC X.                                   
073600 01  CDCA-WDB2-PCB               PIC X.                                   
073700 01  CDCA-WDC1-PCB               PIC X.                                   
073800 01  RANS-XXKM-PCB               PIC X.                                   
073900 01  RANS-ARTM-PCB               PIC X.                                   
074000 01  RANS-ARTS-PCB               PIC X.                                   
074100     EJECT                                                                
074200 01  TPO1-ORDP-PCB               PIC X.                                   
074300 01  TPO1-ARTM-PCB               PIC X.                                   
074400 01  TPO1-ZZAC-PCB               PIC X.                                   
074500 01  TPO2-ORDP-PCB               PIC X.                                   
074600 01  TPO2-XXBU-PCB               PIC X.                                   
074700 01  TPO2-XXBV-PCB               PIC X.                                   
074800 01  TPO2-ARTM-PCB               PIC X.                                   
074900 01  TPO2-FILA-PCB               PIC X.                                   
075000 01  TPO2-XXBX-PCB               PIC X.                                   
075100 01  RELS-ORDP-PCB               PIC X.                                   
075200 01  RELS-FILA-PCB               PIC X.                                   
075300 01  RELS-ARTM-PCB               PIC X.                                   
075400 01  TIME-4437-PCB               PIC X.                                   
075500                                                                          
075600 01  AVSR-ORQI-PCB               PIC X.                                   
075700 01  AVSR-GMTB-PCB               PIC X.                                   
075800 01  AVSR-GMTC-PCB               PIC X.                                   
075900 01  AVSR-WDB2-PCB            PIC X.                                      
076000 01  AVSR-WDB6-PCB            PIC X.                                      
076100 01  TRAN-XXKB-PCB               PIC X.                                   
076200 01  KVAN-WDB2-PCB            PIC X.                                      
076300 01  KVAN-WDC1-PCB            PIC X.                                      
076400 01  XDCA-USEA-PCB               PIC X.                                   
076500 01  XDCA-WDB6-PCB               PIC X.                                   
076600 01  XDCA-WDK6-PCB               PIC X.                                   
076700 01  XDCA-WDK7-PCB               PIC X.                                   
076800 01  XDCA-WDK9-PCB               PIC X.                                   
076900 01  XDCA-WDL6-PCB               PIC X.                                   
077000 01  XDCA-WDQ4B-PCB              PIC X.                                   
077100 01  XDCA-WDQ2-PCB               PIC X.                                   
077200 01  XDCA-WDQ4-PCB               PIC X.                                   
077300 01  XDCA-WDR6-PCB               PIC X.                                   
077400 01  XDCA-WDB6-2-PCB             PIC X.                                   
077500 01  XDCA-WDK6-2-PCB             PIC X.                                   
077600 01  XDCA-WDK7-2-PCB             PIC X.                                   
077700 01  XDCA-WDK7-3-PCB             PIC X.                                   
077800     EJECT                                                                
077900 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB 2109-PCB PRQRY-PCB        
078000                  4223-PCB 4223V-PCB 4297-PCB USEA-PCB                    
078100        WDQ1-PCB WDQ2-PCB WDQ4-PCB ARTM-PCB WDK7-PCB 4541-PCB             
078200        WDB2-PCB  WDB1-PCB WDB6-PCB                                       
078300        WDK6-PCB WDA6F1-PCB WDA6F-PCB WDF5-PCB WDR6-PCB                   
078400        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
078500        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
078600        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
078700        PRIS-COST-WDK6-PCB                                                
078800        PRIS-COST-WDK7-PCB                                                
078900        PRIS-COST-WDF1-PCB                                                
079000        PRIS-COST-9305-PCB                                                
079100        PRIS-COST-WDK72-PCB                                               
079200        PRIS-COST-WDB6-PCB                                                
079300        PRNO-3107-PCB                                                     
079400        PRQU-WDG2-PCB                                                     
079500        PRQU-WDC7-PCB                                                     
079600        PRQU-SJKO-WDK6-PCB                                                
079700        AREG-WDK6-PCB                                                     
079800        AREG-WDK7-PCB                                                     
079900        ARTM-ARTM-PCB                                                     
080000        DLEV-LEVF-PCB                                                     
080100        DLEV-LEVG-PCB                                                     
080200        DLEV-LEVA-PCB                                                     
080300        DLEV-ARTS-PCB                                                     
080400        DLEV-WDB6-PCB                                                     
080500        SPAR-WDF8-PCB                                                     
080600        SPAR-WDF8A-PCB                                                    
080700        SPAR-WDK6-PCB                                                     
080800        DNOT-ORQP-PCB                                                     
080900        DNOT-ORQP2-PCB                                                    
081000        DNOT-ORQP3-PCB                                                    
081100        DNOT-4013-PCB                                                     
081200        DNOT-BENA-PCB                                                     
081300        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
081400        KERS-ARTC-PCB KERS-ERSA-PCB                                       
081500        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
081600        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
081700        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
081800        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
081900        SDCA-WDK7-3-PCB                                                   
082000        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
082100        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
082200        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
082300        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
082400        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
082500        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
082600        TIME-4437-PCB                                                     
082700        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
082800        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
082900        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
083000        XDCA-USEA-PCB                                                     
083100        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
083200        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
083300        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
083400        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
083500        XDCA-WDK7-3-PCB.                                                  
083600     EJECT                                                                
083700                                                                          
083800     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB 2109-PCB PRQRY-PCB        
083900                           4223-PCB 4223V-PCB 4297-PCB USEA-PCB           
084000        WDQ1-PCB WDQ2-PCB WDQ4-PCB ARTM-PCB WDK7-PCB 4541-PCB             
084100        WDB2-PCB WDB1-PCB WDB6-PCB                                        
084200        WDK6-PCB WDA6F1-PCB WDA6F-PCB WDF5-PCB WDR6-PCB                   
084300        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
084400        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
084500        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
084600        PRIS-COST-WDK6-PCB                                                
084700        PRIS-COST-WDK7-PCB                                                
084800        PRIS-COST-WDF1-PCB                                                
084900        PRIS-COST-9305-PCB                                                
085000        PRIS-COST-WDK72-PCB                                               
085100        PRIS-COST-WDB6-PCB                                                
085200        PRNO-3107-PCB                                                     
085300        PRQU-WDG2-PCB                                                     
085400        PRQU-WDC7-PCB                                                     
085500        PRQU-SJKO-WDK6-PCB                                                
085600        AREG-WDK6-PCB                                                     
085700        AREG-WDK7-PCB                                                     
085800        ARTM-ARTM-PCB                                                     
085900        DLEV-LEVF-PCB                                                     
086000        DLEV-LEVG-PCB                                                     
086100        DLEV-LEVA-PCB                                                     
086200        DLEV-ARTS-PCB                                                     
086300        DLEV-WDB6-PCB                                                     
086400        SPAR-WDF8-PCB                                                     
086500        SPAR-WDF8A-PCB                                                    
086600        SPAR-WDK6-PCB                                                     
086700        DNOT-ORQP-PCB                                                     
086800        DNOT-ORQP2-PCB                                                    
086900        DNOT-ORQP3-PCB                                                    
087000        DNOT-4013-PCB                                                     
087100        DNOT-BENA-PCB                                                     
087200        KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                         
087300        KERS-ARTC-PCB KERS-ERSA-PCB                                       
087400        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
087500        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
087600        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
087700        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
087800        SDCA-WDK7-3-PCB                                                   
087900        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
088000        RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                         
088100        TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                         
088200        TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                         
088300        TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                         
088400        RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                         
088500        TIME-4437-PCB                                                     
088600        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
088700        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
088800        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
088900        XDCA-USEA-PCB                                                     
089000        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
089100        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
089200        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
089300        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
089400        XDCA-WDK7-3-PCB.                                                  
089500     EJECT                                                                
089600                                                                          
089700     PERFORM IMS-GET-MSG                                                  
089900     IF SEGMENT-FINNS                                                     
090000        PERFORM A-INIT                                                    
090100        PERFORM B-KOLLA-NYCKLAR                                           
090200        IF ALLT-OK                                                        
090300           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
090400           IF ALLT-OK                                                     
090500              IF OHUV-FLVORKO = YES                                       
090600                 PERFORM I-LAES-IN-4541-WDR4                              
090700              ELSE                                                        
090800                 IF OHUV-FLVORKO = JA                                     
090900                     PERFORM K-LAES-FRAN-NYVORKO                          
091000                 END-IF                                                   
091100              END-IF                                                      
091200              PERFORM D-FORMELL-KONTROLL                                  
091300              IF ALLT-OK                                                  
091400                 PERFORM E-BEHANDLA-RADER                                 
091500              END-IF                                                      
091600           END-IF                                                         
091700        END-IF                                                            
091800        IF ALLT-OK                                                        
091900           IF MID-IDARTNR-006(WS-INDEX-MID-MAX) = ALL '+'                 
092000           OR SVARSBILD                                                   
092100              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
092200                PERFORM J-SKICKA-PRISFRAGA                                
092300              END-IF                                                      
092400              IF BIPA-JA AND NOT SVARSBILD                                
092500                 PERFORM H-STARTA-BIPACKNINGEN                            
092600              ELSE                                                        
092700                 PERFORM F-HOPPA-TILL-SVARSBILD                           
092800              END-IF                                                      
092900           ELSE                                                           
093000              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
093100                PERFORM J-SKICKA-PRISFRAGA                                
093200              END-IF                                                      
093300              PERFORM G-VISA-TOM-SIDA                                     
093400           END-IF                                                         
093500        END-IF                                                            
093600*                                                                         
093700        IF W-IDTRANS = 'V412'                                             
093800          CONTINUE                                                        
093900        ELSE                                                              
094000          IF HOPP = NEJ                                                   
094100          AND HOPP-TILL-0504     = NEJ                                    
094200             PERFORM Z-FINIT-INSERT-MSG                                   
094300          END-IF                                                          
094400        END-IF                                                            
094500*                                                                         
094600     END-IF                                                               
094700     MOVE +0 TO RETURN-CODE                                               
094800     GOBACK                                                               
094900     .                                                                    
095000     EJECT                                                                
095100 A-INIT SECTION.                                                          
095200                                                                          
095300     MOVE SPACE                TO MED-IDMFSFEL                            
095400                                                                          
095500     IF MSG-DUBBLA-TRANSKODER                                             
095600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22201-CTX             
095700       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
095800       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
095900     ELSE                                                                 
096000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22201-CTX              
096100       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
096200       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
096300     END-IF                                                               
096400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
096500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
096600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
096700     IF W-IDTRANS = '4223' AND MSG-KDTRANS-1  = 'W4T222U '                
096800        MOVE JA TO SVARSBILD-SW                                           
096900     END-IF                                                               
097000                                                                          
097100     MOVE NEJ                  TO BEVARREF-I-HFAK-TAB-SW                  
097200     MOVE LOW-VALUE            TO MSG-AREA                                
097300     MOVE 'W4O22201'           TO MFS-IDMOD                               
097400     MOVE '4222'               TO MOD-IDTRANS                             
097500     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
097600                                  MOD-TEMFSINF                            
097700                                                                          
097800     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O22201-CTX + 4              
097900                                                                          
098000     IF NOT EGEN-MID  AND  NOT SVARSBILD                                  
098100       MOVE SPACE              TO MFS-KDTRTYP                             
098200       MOVE '7'                TO MFS-IDPFK                               
098300     END-IF                                                               
098400                                                                          
098500     IF NOT GODK-MID                                                      
098600       PERFORM S10-WRONG-PICTURE-MESSAGE                                  
098700     END-IF                                                               
098800                                                                          
098900     IF ENGLISH-TEXT                                                      
099000       MOVE +2                 TO SPRAK-IX                                
099100       MOVE 'GB '              TO MED-IDSKYLT                             
099200     ELSE                                                                 
099300       MOVE +1                 TO SPRAK-IX                                
099400       MOVE 'S  '              TO MED-IDSKYLT                             
099500     END-IF                                                               
099600                                                                          
099700     MOVE ZERO                 TO WS-IDPRQUES                             
099800                                                                          
099900     PERFORM AA-NOLLA-WOPS-TABELL                                         
100000     .                                                                    
100100     EJECT                                                                
100200 AA-NOLLA-WOPS-TABELL SECTION.                                            
100300                                                                          
100400     MOVE +1                   TO WS-INDEX-WOPS                           
100500     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
100600        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
100700        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
100800        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
100900        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
101000        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
101100        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
101200        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
101300        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
101400        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
101500        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
101600        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
101700        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
101800        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
101900        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
102000        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
102100        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
102200                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
102300        ADD +1                 TO WS-INDEX-WOPS                           
102400     END-PERFORM                                                          
102500                                                                          
102600     MOVE +1                   TO WS-INDEX-WOPS                           
102700     .                                                                    
102800     EJECT                                                                
102900 B-KOLLA-NYCKLAR SECTION.                                                 
103000                                                                          
103100     MOVE MID-IDDISTR       TO WS-IDDISTR                                 
103200       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
103300     MOVE MID-IDKUNDNR      TO WS-IDKUNDNR                                
103400       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
103500     MOVE MID-IDORDNR5      TO WS-IDORDNR                                 
103600       INSPECT WS-IDORDNR  REPLACING LEADING SPACE BY ZERO                
103700*    MOVE SPACE             TO MFS-KDTRTYP                                
103800*    MOVE '7'               TO MFS-IDPFK                                  
103900                                                                          
104000     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
104100        MOVE WS-IDDISTR           TO W-IDDISTR                            
104200     ELSE                                                                 
104300        MOVE NEJ                  TO ALLT-SW                              
104400        MOVE ZERO                 TO WS-IDDISTR                           
104500     END-IF                                                               
104600                                                                          
104700     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
104800     IF DIST79-DEALER-PRICE                                               
104900        IF ENGLISH-TEXT                                                   
105000           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
105100        ELSE                                                              
105200           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
105300        END-IF                                                            
105400     ELSE                                                                 
105500        MOVE SPACES               TO MOD-TEDDI                            
105600     END-IF                                                               
105700                                                                          
105800     IF WS-IDKUNDNR NUMERIC                                               
105900        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
106000     ELSE                                                                 
106100        MOVE NEJ                  TO ALLT-SW                              
106200     END-IF                                                               
106300                                                                          
106400     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
106500        MOVE WS-IDORDNR           TO WS-NUM-7                             
106600        MOVE WS-NUM-7             TO W-IDKUNDRF                           
106700     ELSE                                                                 
106800        MOVE NEJ                  TO ALLT-SW                              
106900     END-IF                                                               
107000                                                                          
107100     IF NOT ALLT-OK                                                       
107200        IF SVARSBILD                                                      
107300          MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4223'          
107400                                  TO FELTEXT                              
107500          CALL ABEND USING RKOD-ABEND                                     
107600        END-IF                                                            
107700        MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                         
107800     END-IF                                                               
107900     EJECT                                                                
108000                                                                          
108100     IF GODK-MID OR ALLT-OK                                               
108200       MOVE WS-IDDISTR              TO MOD-IDDISTR                        
108300       INSPECT MOD-IDDISTR REPLACING LEADING ZERO BY SPACE                
108400                                                                          
108500       IF WS-IDKUNDNR = ZERO                                              
108600         MOVE '     0'              TO MOD-IDKUNDNR                       
108700       ELSE                                                               
108800         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR                       
108900         INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE             
109000       END-IF                                                             
109100                                                                          
109200       MOVE WS-IDORDNR              TO MOD-IDORDNR5                       
109300       INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
109400                                                                          
109500       IF W-IDTRANS = '4221' AND NOT ALLT-OK                              
109600          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR                        
109700                                       MOD-IDKUNDNR                       
109800                                       MOD-IDORDNR5                       
109900       END-IF                                                             
110000                                                                          
110100     ELSE                                                                 
110200       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR                        
110300                                       MOD-IDKUNDNR                       
110400                                       MOD-IDORDNR5                       
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
110900                                                                          
111000     PERFORM IMS-01-GU-WDQ2-WDQ201                                        
111100     IF SEGMENT-FINNS                                                     
111200        IF OHUV-FLKLAR = JA                                               
111300           MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                      
111400           MOVE NEJ                  TO ALLT-SW                           
111500        ELSE                                                              
111600           IF OHUV-IDSYSTEM NOT  = '4221' AND                             
111700              OHUV-IDSYSTEM NOT  = 'LYNV'                                 
111900              MOVE ERR-FEL-BILDSERIE TO MED-IDMFSFEL                      
112000              MOVE NEJ               TO ALLT-SW                           
112100           ELSE                                                           
112200              IF OHUV-IDUSER NOT = MSG-SIGNON-USERID                      
112300                 MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                   
112400                 MOVE NEJ               TO ALLT-SW                        
112500              ELSE                                                        
112600                 MOVE OHUV-KDORDKL      TO MOD-KDORDKL                    
112700                 PERFORM CA-HAMTA-KUND                                    
112800                 PERFORM CB-LAES-ARBETSTABELL                             
112900                 PERFORM CC-FIXA-LOKAL-TID                                
113000              END-IF                                                      
113100           END-IF                                                         
113200        END-IF                                                            
113300     ELSE                                                                 
113400        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
113500        MOVE NEJ                     TO ALLT-SW                           
113600     END-IF                                                               
113700     MOVE SPACE                     TO MOD-KDVALISO                       
113800                                                                          
113900     IF MFS-FIRST AND ALLT-OK                                             
114000     AND  MID-FLVORKO NOT = YES                                           
114100     AND  MID-FLVORKO NOT = JA                                            
114200        MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                
114300        PERFORM MFS-RENSA-MOD-RADER                                       
114400        MOVE NEJ                    TO ALLT-SW                            
114500     END-IF                                                               
114600     .                                                                    
114700     EJECT                                                                
114800 CA-HAMTA-KUND      SECTION.                                              
114900     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
115000     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
115100     PERFORM IMS-GU-WDB201                                                
115200                                                                          
115300     IF OHUV-KDORDKL > 1                                                  
115400                                                                          
115500        MOVE +1 TO WS-INDEX                                               
115600        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
115700           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
115800                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
115900           ADD +1 TO WS-INDEX                                             
116000        END-PERFORM                                                       
116100                                                                          
116200     ELSE                                                                 
116300       IF OHUV-KDORDKL = 1                                                
116400                                                                          
116500          MOVE +1 TO WS-INDEX                                             
116600          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
116700             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
116800                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
116900             ADD +1 TO WS-INDEX                                           
117000          END-PERFORM                                                     
117100                                                                          
117200       ELSE                                                               
117300         IF OHUV-KDORDKL = 0                                              
117400                                                                          
117500            MOVE +1 TO WS-INDEX                                           
117600            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
117700               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
117800                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
117900               ADD +1 TO WS-INDEX                                         
118000            END-PERFORM                                                   
118100                                                                          
118200         END-IF                                                           
118300       END-IF                                                             
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
118700 CB-LAES-ARBETSTABELL SECTION.                                            
118800                                                                          
118900     MOVE OHUV-IDDC-TVS     TO W-IDDC                                     
119000     PERFORM IMS-03-GNP-WDQ2-WDQ212                                       
119100     MOVE ARB-KDFRAKT    TO MOD-KDFRAKT                                   
119200     .                                                                    
119300     EJECT                                                                
119400                                                                          
119500 CC-FIXA-LOKAL-TID SECTION.                                               
119600                                                                          
119700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
119800     MOVE '013'             TO MSGI-KDCALL                                
119900     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
120000     MOVE OHUV-IDDC-TVS     TO MSGI-IDUSER(6:2)                           
120100     MOVE '4222'            TO MSGI-IDTRANS                               
120200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
120300                                                                          
120400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
120500     .                                                                    
120600     EJECT                                                                
120700                                                                          
120800 D-FORMELL-KONTROLL SECTION.                                              
120900                                                                          
121000     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
121100     MOVE OHUV-IDDC-TVS              TO ORFK-IDDC                         
121200     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
121300     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
121400     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
121500     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
121600     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
121700     MOVE +0                         TO ORFK-KDTPOTYP                     
121800     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
121900     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
122000     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
122100     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
122200     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
122300     MOVE +1                   TO WS-INDEX-MID                            
122400     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
122500        IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                           
122600           MOVE NEJ            TO ORFK-FLINVEST(WS-INDEX-MID)             
122700        ELSE                                                              
122800           IF MID-FLINVEST(WS-INDEX-MID) = 'Y'                            
122900              MOVE JA          TO MID-FLINVEST(WS-INDEX-MID)              
123000           END-IF                                                         
123100           MOVE MID-FLINVEST(WS-INDEX-MID)                                
123200                               TO ORFK-FLINVEST(WS-INDEX-MID)             
123300        END-IF                                                            
123400        MOVE OHUV-FLRESTN      TO ORFK-FLRESTN(WS-INDEX-MID)              
123500     EJECT                                                                
123600        MOVE NEJ               TO ORFK-FLSLATT(WS-INDEX-MID)              
123700                                                                          
123800        MOVE MID-IDARTNR-006(WS-INDEX-MID)                                
123900                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
124000                                                                          
124100        MOVE '+'               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
124200                                                                          
124300        IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                           
124400         MOVE OHUV-KDVRINFO    TO ORFK-KDVRINFO(WS-INDEX-MID)             
124500        ELSE                                                              
124600         MOVE MID-KDVRINFO(WS-INDEX-MID)                                  
124700                               TO ORFK-KDVRINFO(WS-INDEX-MID)             
124800        END-IF                                                            
124900                                                                          
125000        MOVE MID-KVBEART(WS-INDEX-MID)                                    
125100                               TO ORFK-KVBEART(WS-INDEX-MID)              
125200        IF DIST79-DEALER-PRICE                                            
125300           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
125400                               TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
125500                                                                          
125600           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)             
125700           MOVE ALL '+'        TO                                         
125800                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
125900        ELSE                                                              
126700           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
126800                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
126900           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
127000           MOVE ALL '+'        TO                                         
127100                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
127300        END-IF                                                            
127400                                                                          
127500        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
127600        MOVE ALL '+'           TO ORFK-TITPO-RAD(WS-INDEX-MID)            
127700        ADD +1                 TO WS-INDEX-MID                            
127800     END-PERFORM                                                          
127900                                                                          
128000     CALL W411ORFK USING ORFK-W411ORFK                                    
128100                         AREG-WDK6-PCB                                    
128200                         AREG-WDK7-PCB                                    
128300                                                                          
128400     MOVE +1                   TO WS-INDEX-MID                            
128500     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
128600        PERFORM DA-KOLLA-FEL-FK                                           
128700        ADD +1                 TO WS-INDEX-MID                            
128800     END-PERFORM                                                          
128900                                                                          
129000     IF SVARSBILD AND NOT ALLT-OK                                         
129100        MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4223'           
129200                               TO FELTEXT                                 
129300        CALL ABEND USING RKOD-ABEND                                       
129400     END-IF                                                               
129500                                                                          
129600     .                                                                    
129700     EJECT                                                                
129800 DA-KOLLA-FEL-FK SECTION.                                                 
129900                                                                          
130000                                                                          
130100     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
130200        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
130300        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)        
130400        MOVE NEJ                 TO ALLT-SW                               
130500     END-IF                                                               
130600                                                                          
130700     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
130800        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
130900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
131000        MOVE NEJ                 TO ALLT-SW                               
131100     ELSE                                                                 
131200        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
131300                  AND NOT MFS-UPDATE                                      
131400          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
131500          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
131600          MOVE NEJ               TO ALLT-SW                               
131610        END-IF                                                            
132500     END-IF                                                               
132600                                                                          
132700     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
132800        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
132900        MOVE MFS-NUM-FAELT-FEL TO MOD-KDVRINFO-ATTR(WS-INDEX-MID)         
133000        MOVE NEJ                 TO ALLT-SW                               
133100     END-IF                                                               
133200     EJECT                                                                
133300                                                                          
133400     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
133500        IF MED-IDMFSFEL = SPACE                                           
133600           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC AND                      
133700                   ORFK-KVBEART(WS-INDEX-MID) > ZERO                      
133800              IF NOT MFS-UPDATE                                           
133900                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
134000                 MOVE MFS-NUM-FAELT-FEL   TO                              
134100                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
134200                 MOVE NEJ             TO ALLT-SW                          
134300              END-IF                                                      
134400           ELSE                                                           
134500            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
134600            MOVE MFS-NUM-FAELT-FEL   TO                                   
134700                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
134800            MOVE NEJ                 TO ALLT-SW                           
134900           END-IF                                                         
135000        ELSE                                                              
135100           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
135200           MOVE MFS-NUM-FAELT-FEL   TO                                    
135300                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
135400           MOVE NEJ                 TO ALLT-SW                            
135500        END-IF                                                            
135600     END-IF                                                               
135700                                                                          
135800     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
135900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
136000        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
136100        MOVE NEJ                 TO ALLT-SW                               
136200     END-IF                                                               
136300                                                                          
136400     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
136500        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
136600        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
136700        MOVE NEJ                 TO ALLT-SW                               
136800     END-IF                                                               
136900     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
137000        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
137100        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
137200        MOVE NEJ                 TO ALLT-SW                               
137300     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137600 DAA-CHECK-CROSS-TABLE SECTION.                                           
137700                                                                          
137800*    FOR THE TIME BEEING WE ONLY CHECK FOR LYNK-PARTS                     
137900*    IF A LYNK-ORDER WE NEED TO CHECK THAT GIVEN PARTNO                   
138000*    IS A LYNK PART                                                       
138100                                                                          
138200     PERFORM IMS-GU-WDF501                                                
138300     IF SEGMENT-SAKNAS                                                    
138400        MOVE 58                   TO ORFK-KDORDBEK(WS-INDEX-MID)          
138500        MOVE ERR-IDARTNR-SAKNAS   TO MED-IDMFSFEL                         
138600        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)          
138700        MOVE NEJ                  TO ALLT-SW                              
138800     END-IF                                                               
138900     .                                                                    
139000     EJECT                                                                
139100 E-BEHANDLA-RADER SECTION.                                                
139200                                                                          
139300     MOVE +1 TO WS-INDEX-MID                                              
139400     MOVE NEJ                     TO TILLK-SW                             
139500                                     OBKR-SW                              
139600                                                                          
139700     MOVE +0                      TO WS-IDPRQUES                          
139800                                                                          
139900     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
140000        IF MID-IDARTNR-006(WS-INDEX-MID) NOT = ALL '+'                    
140100           PERFORM S02-RENSA-TILLK-TAB                                    
140200           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
140300                                  TO AREG-W411AREG-001                    
140400           PERFORM EC-BEHANDLA-RAD                                        
140500           IF WS-IDDC NOT = W-IDDC-B6                                     
140600              MOVE WS-IDDC TO W-IDDC-B6                                   
140700              PERFORM IMS-GU-WDB601                                       
140800           END-IF                                                         
140900           IF DCS-NDC-NA                                                  
141000              PERFORM S04-DATA-TILL-DEL-NOTE                              
141100           END-IF                                                         
141200           MOVE JA                TO TILLK-SW                             
141300           MOVE +1                TO WS-INDEX-TILLK                       
141400           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
141500              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
141600              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
141700                 PERFORM ED-LAES-TILLK-DATA                               
141800                 PERFORM EC-BEHANDLA-RAD                                  
141900                 IF DCS-NDC-NA                                            
142000                    PERFORM S04-DATA-TILL-DEL-NOTE                        
142100                 END-IF                                                   
142200              END-IF                                                      
142300              ADD +1              TO WS-INDEX-TILLK                       
142400           END-PERFORM                                                    
142500        END-IF                                                            
142600        MOVE NEJ                  TO TILLK-SW                             
142700                                     OBKR-SW                              
142800        ADD +1 TO WS-INDEX-MID                                            
142900     END-PERFORM                                                          
143000                                                                          
143100     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
143200       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
143300       MOVE +3                      TO PRNO-KDCALL                        
143400                                                                          
143500       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
143600     END-IF                                                               
143700                                                                          
143800     IF AVSR-IDDC(1) > ZERO                                               
143900        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                    
144000             AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                    
144100             AVSR-WDB2-PCB AVSR-WDB6-PCB                                  
144200             TRAN-XXKB-PCB                                                
144300     END-IF                                                               
144400                                                                          
144500     IF W-IDTRANS = 'V412'                                                
144600       CONTINUE                                                           
144700     ELSE                                                                 
144800       IF AVSR-KDROPACK NOT = SPACE AND ZERO AND DCS-CDC                  
144900          MOVE JA          TO BIPA-SW                                     
145000       END-IF                                                             
145100     END-IF                                                               
145200                                                                          
145300     IF ARB-IDDC NOT = W-IDDC-B6                                          
145400        MOVE ARB-IDDC TO W-IDDC-B6                                        
145500        PERFORM IMS-GU-WDB601                                             
145600     END-IF                                                               
145700                                                                          
145800     MOVE JA                      TO ALLT-SW                              
145900     .                                                                    
146000     EJECT                                                                
146100 EC-BEHANDLA-RAD SECTION.                                                 
146200                                                                          
146300     PERFORM ECA-NOLLSTALL-OBKR                                           
146400     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
146500     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
146600     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
146700     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
146800                                                                          
146900     IF NOT TILLKOMMANDE-RAD                                              
147000        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
147100     END-IF                                                               
147200                                                                          
147300     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
147400     PERFORM ECJ-KOMPLETTERA-PRIS                                         
147500     PERFORM ECM-KOMPLETTERA-TPO1                                         
147600     PERFORM ECN-KOMPLETTERA-TPO2                                         
147700     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
147800     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
148000     PERFORM ECG-PREL-AVBOKNING-XDC                                       
148100     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
148200     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
148300     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
148400     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
148500     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
148600     PERFORM ECR-PREL-AVBOKNING-CDC                                       
148700                                                                          
148800     IF NOT TILLKOMMANDE-RAD                                              
148900        IF SKRIV-OBKR                                                     
149000           PERFORM ECS-SKRIV-OBKR                                         
149100           IF NOT OBKR-SKRIVEN OR                                         
149200              EGET-CL-RAD                                                 
149300              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
149400              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
149500           END-IF                                                         
149600        ELSE                                                              
149700           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
149800                               OR KAMP-FLKLAR = JA                        
149900              CONTINUE                                                    
150000           ELSE                                                           
150100              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
150200              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
150300           END-IF                                                         
150400        END-IF                                                            
150500     ELSE                                                                 
150600        IF SKRIV-OBKR                                                     
150700           PERFORM ECS-SKRIV-OBKR                                         
150800        END-IF                                                            
150900     END-IF                                                               
151000     .                                                                    
151100     EJECT                                                                
151200 ECA-NOLLSTALL-OBKR SECTION.                                              
151300                                                                          
151400     MOVE +0                   TO KVAN-KDORDBEK-UT                        
151500     MOVE +0                   TO DLEV-KDORDBEK-UT                        
151600     MOVE +0                   TO KERS-KDERS                              
151700     IF NOT TILLKOMMANDE-RAD                                              
151800        MOVE +0                TO KERS-KDORDBEK                           
151900     ELSE                                                                 
152000        MOVE JA                TO OBKR-SW                                 
152100     END-IF                                                               
152200     MOVE +0                   TO TPO1-KDORDBEK                           
152300     MOVE +0                   TO TPO2-KDORDBEK                           
152400     MOVE +0                   TO KAMP-KDORDBEK                           
152500     MOVE +0                   TO STOR-KDORDBEK                           
152600     MOVE +0                   TO XDCA-KDORDBEK                           
152700*    MOVE +0                   TO NDCA-KDORDBEK                           
152800     MOVE +0                   TO SDCA-KDORDBEK                           
152900     MOVE +0                   TO SDCA-KDORDBEK-FIRST-SDC                 
153000     MOVE +0                   TO SDCA-KDORDBEK-SECOND-SDC                
153100     MOVE +0                   TO CDCA-KDORDBEK-UT                        
153200     MOVE ZERO                 TO SPAR-KDORDBEK                           
153300                                                                          
153400     MOVE JA                   TO ALLT-SW                                 
153500     MOVE NEJ                  TO EGET-CL-RAD-SW                          
153600                                  KOLLA-ERS-SW                            
153700                                                                          
153800     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
153900       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
154000       MOVE +7                          TO W-KDTPOTYP                     
154100     ELSE                                                                 
154200       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
154300         MOVE NEJ               TO ALLT-SW                                
154400         MOVE JA                TO OBKR-SW                                
154500       END-IF                                                             
154600     END-IF                                                               
154700                                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
155100                                                                          
155200     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
155300     MOVE OHUV-IDDC-TVS        TO ORAD-IDDC                               
155400                                  WS-IDDC                                 
155500     MOVE +0                   TO ORAD-ADLAGOMR                           
155600     MOVE +0                   TO ORAD-ADGANG                             
155700     MOVE +0                   TO ORAD-ADPLATS                            
155800     IF TILLKOMMANDE-RAD                                                  
155900        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
156000     ELSE                                                                 
156100        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
156200                               TO ORAD-IDARTNR                            
156300     END-IF                                                               
156400     MOVE +1                   TO ORAD-IDLOPNR                            
156500                                                                          
156600     IF OHUV-BEVARREF = ALL '+' OR SPACE                                  
156700       MOVE MID-BERADREF(WS-INDEX-MID) TO WS-HFAK-REF-X10                 
156800     ELSE                                                                 
156900       MOVE OHUV-BEVARREF      TO WS-HFAK-REF-X10                         
157000       PERFORM S01-KOLLA-I-HFAK-TAB                                       
157100     END-IF                                                               
157200                                                                          
157300     IF BEVARREF-I-HFAK-TAB                                               
157400       MOVE OHUV-BEVARREF      TO ORAD-BERADREF                           
157500     ELSE                                                                 
157600       IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                            
157700         MOVE SPACE            TO ORAD-BERADREF                           
157800       ELSE                                                               
157900         MOVE MID-BERADREF(WS-INDEX-MID) TO ORAD-BERADREF                 
158000       END-IF                                                             
158100     END-IF                                                               
158200                                                                          
158300     MOVE OHUV-BEVARREF        TO ORAD-BEVOLREF                           
158400     MOVE SPACE                TO ORAD-FLAKPLOC                           
158500     MOVE 'N'                  TO ORAD-FLSDCLEV                           
158600                                                                          
158700     IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                              
158800        MOVE NEJ               TO ORAD-FLINVEST                           
158900     ELSE                                                                 
159000        MOVE MID-FLINVEST(WS-INDEX-MID)                                   
159100                               TO ORAD-FLINVEST                           
159200     END-IF                                                               
159300                                                                          
159400     MOVE JA                   TO ORAD-FLOBTRAN                           
159500     IF TILLKOMMANDE-RAD                                                  
159600       IF DIST79-DEALER-PRICE                                             
159700          MOVE NEJ             TO ORAD-FLPRTILL                           
159800       ELSE                                                               
159900        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
160000           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
160100                               TO ORAD-FLPRTILL                           
160200        ELSE                                                              
160300           MOVE NEJ            TO ORAD-FLPRTILL                           
160400        END-IF                                                            
160500       END-IF                                                             
160600     ELSE                                                                 
160700        MOVE NEJ               TO ORAD-FLPRTILL                           
160800     END-IF                                                               
160900     MOVE OHUV-FLRESTN         TO ORAD-FLRESTN                            
161000     IF TILLKOMMANDE-RAD                                                  
161100        MOVE JA                TO ORAD-FLTILLK                            
161200     ELSE                                                                 
161300        MOVE NEJ               TO ORAD-FLTILLK                            
161400     END-IF                                                               
161500     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
161600     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
161700     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
161800     MOVE +0                   TO ORAD-IDKAMPRF                           
161900     MOVE SPACE                TO ORAD-IDLEVNR                            
162000     MOVE +0                   TO ORAD-IDLOPNR-RO                         
162100     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
162200     EJECT                                                                
162300     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
162400     MOVE ZERO                 TO ORAD-IDSPECEMB                          
162500     IF OHUV-IDSYSTEM(1:3) = 'LYN'                                        
162600       MOVE OHUV-IDSYSTEM      TO ORAD-IDSYSTEM                           
162700     ELSE                                                                 
162800       MOVE 'IMS '             TO ORAD-IDSYSTEM                           
162900     END-IF                                                               
163000     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
163100     IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                              
163200        MOVE OHUV-KDVRINFO     TO ORAD-KDDSP                              
163300     ELSE                                                                 
163400        MOVE MID-KDVRINFO(WS-INDEX-MID)                                   
163500                               TO WS-ALFA-1                               
163600        MOVE WS-NUM-1          TO ORAD-KDDSP                              
163700     END-IF                                                               
163800     IF ORAD-KDDSP = +0                                                   
163900        MOVE +1                TO ORAD-KDDSP                              
164000     END-IF                                                               
164100     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
164200     EJECT                                                                
164300     IF OHUV-FLVORKO = YES                                                
164400     OR OHUV-FLVORKO = JA                                                 
164500        MOVE +2                TO ORAD-KDKVBRYT                           
164600     ELSE                                                                 
164700        MOVE +0                TO ORAD-KDKVBRYT                           
164800     END-IF                                                               
164900     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
165000     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
165100     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
165200     IF ORAD-KDORDING = +3                                                
165300       MOVE SPACE              TO ORAD-KDOI                               
165400     ELSE                                                                 
165500       IF ORAD-KDPRODSL = +19 OR +29 OR ORAD-KDORDING = +1                
165600         MOVE 'CD'             TO ORAD-KDOI                               
165700       ELSE                                                               
165800         MOVE 'DT'             TO ORAD-KDOI                               
165900       END-IF                                                             
166000     END-IF                                                               
166100     MOVE SPACE                TO ORAD-CLEARGROUP                         
166200                                                                          
166300     IF TILLKOMMANDE-RAD                                                  
166400       IF DIST79-DEALER-PRICE                                             
166500          MOVE SPACE           TO ORAD-KDPRTYP                            
166600       ELSE                                                               
166700        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
166800           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
166900                               TO ORAD-KDPRTYP                            
167000        ELSE                                                              
167100           MOVE SPACE          TO ORAD-KDPRTYP                            
167200        END-IF                                                            
167300       END-IF                                                             
167400     ELSE                                                                 
167500        MOVE SPACE             TO ORAD-KDPRTYP                            
167600     END-IF                                                               
167700     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
167800     MOVE +0                   TO ORAD-KDTPOTYP                           
167900     MOVE JA                   TO ORAD-FLORDING                           
168000     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
168100     IF TILLKOMMANDE-RAD                                                  
168200        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
168300                               TO ORAD-KVBEART                            
168400     ELSE                                                                 
168500        MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                      
168600        MOVE WS-NUM-6          TO ORAD-KVBEART                            
168700     END-IF                                                               
168800     EJECT                                                                
168900     MOVE +0                   TO ORAD-KVBEART-Q                          
169000     MOVE +0                   TO ORAD-KVPREAVB                           
169100     MOVE +0                   TO ORAD-KVPRERO                            
169200     MOVE +0                   TO ORAD-KVOKS-PREL                         
169300     MOVE +0                   TO ORAD-IDPRQUES                           
169400     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
169500     MOVE +0                   TO ORAD-RERAB                              
169600     MOVE SPACE                TO ORAD-KDVALISO                           
169700     MOVE SPACE                TO ORAD-KDVAT                              
169800     MOVE SPACE                TO ORAD-KDRAB                              
169900     MOVE SPACE                TO ORAD-BEART-VIPS                         
170000                                                                          
170100                                                                          
170200     IF TILLKOMMANDE-RAD                                                  
170300       IF DIST79-DEALER-PRICE                                             
170400         MOVE +0               TO ORAD-PRARTNTO                           
170500         MOVE TILK-PRARTNTO-LOC (WS-INDEX-TILLK)                          
170600                               TO ORAD-PRARTNTO-LOC                       
170700         MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                       
170800                               TO ORAD-PRARTNTO-LOCPREL                   
170900       ELSE                                                               
171000        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
171100           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
171200                               TO ORAD-PRARTNTO                           
171300           MOVE ZERO           TO ORAD-PRARTNTO-LOC                       
171400        ELSE                                                              
171500           MOVE +0             TO ORAD-PRARTNTO                           
171600           MOVE +0             TO ORAD-PRARTNTO-LOC                       
171700           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
171800        END-IF                                                            
171900       END-IF                                                             
172000     ELSE                                                                 
172100        IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                           
172200           MOVE +0             TO ORAD-PRARTNTO                           
172300           MOVE +0             TO ORAD-PRARTNTO-LOC                       
172400           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
172500        ELSE                                                              
172600           IF DIST79-DEALER-PRICE                                         
172700             MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                      
172800                               TO ORAD-PRARTNTO-LOC                       
172900             MOVE +0           TO ORAD-PRARTNTO-LOCPREL                   
173000             MOVE +0           TO ORAD-PRARTNTO                           
173100           ELSE                                                           
173900             MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                          
174000                               TO ORAD-PRARTNTO                           
174100             MOVE +0           TO ORAD-PRARTNTO-LOC                       
174200             MOVE +0           TO ORAD-PRARTNTO-LOCPREL                   
174400           END-IF                                                         
174500        END-IF                                                            
174600        MOVE +0                TO ORAD-PRBPRIS                            
174700     END-IF                                                               
174800     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
174900        MOVE MID-IDARTNR-006(WS-INDEX-MID) (11:1)                         
175000                               TO ORAD-REKSIFFR                           
175100     ELSE                                                                 
175200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
175300     END-IF                                                               
175400     MOVE +0                   TO ORAD-RERF-RAD                           
175500     MOVE +0                   TO ORAD-KVSLATT                            
175600                                                                          
175700     EJECT                                                                
175800     IF TILLKOMMANDE-RAD                                                  
175900       IF DIST79-DEALER-PRICE                                             
176000           MOVE +0             TO ORAD-TIPRIS                             
176100       ELSE                                                               
176200        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
176300           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
176400                               TO ORAD-TIPRIS                             
176500        ELSE                                                              
176600           MOVE +0             TO ORAD-TIPRIS                             
176700        END-IF                                                            
176800       END-IF                                                             
176900     ELSE                                                                 
177000        MOVE +0                TO ORAD-TIPRIS                             
177100     END-IF                                                               
177200                                                                          
177300     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
177400     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
177500     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
177600     MOVE +0                   TO ORAD-TIRODAT                            
177700     MOVE +0                   TO ORAD-TITPO                              
177800     MOVE AREG-VKART           TO ORAD-VKART                              
177900     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
178000     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
178100     MOVE SPACE                TO ORAD-IDBIL                              
178200                                  ORAD-IDKLIENT                           
178300                                  ORAD-IDARBREF                           
178400                                  ORAD-IDVIN                              
178500                                                                          
178600     IF ORAD-KDORDKL = 1 AND                                              
178700        GMT-FLLDCKND = JA                                                 
178800        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
178900     ELSE                                                                 
179000        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
179100     END-IF                                                               
179200     MOVE +0                   TO ORAD-PRAVCOST                           
179300                                                                          
179400     .                                                                    
179500     EJECT                                                                
179600 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
179700                                                                          
179800     IF ALLT-OK                                                           
179900                                                                          
180000     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
180100     PERFORM IMS-10-GU-WLARTM-WDK901                                      
180200                                                                          
180300     IF SEGMENT-SAKNAS                                                    
180400        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
180500        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
180600     END-IF                                                               
180700                                                                          
180800     END-IF                                                               
180900     .                                                                    
181000     EJECT                                                                
181100 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
181200                                                                          
181300     IF ALLT-OK                                                           
181400                                                                          
181500     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
181600     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
181700     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
181800     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
181900     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
182000     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
182100     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
182200     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
182300     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
182400     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
182500     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
182600     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
182700     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
182800     MOVE ORAD-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
182900     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
183000     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
183100     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
183200     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
183300     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
183400                                                                          
183500     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
183600                                                                          
183700     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
183800     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
183900                                                                          
184000     IF KVAN-KDORDBEK-UT > +0                                             
184100        MOVE JA                    TO OBKR-SW                             
184200     END-IF                                                               
184300                                                                          
184400     END-IF                                                               
184500     .                                                                    
184600     EJECT                                                                
184700 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
184800                                                                          
184900     IF WS-IDDC NOT = W-IDDC-B6                                           
185000        MOVE WS-IDDC TO W-IDDC-B6                                         
185100        PERFORM IMS-GU-WDB601                                             
185200     END-IF                                                               
185300                                                                          
185400     IF ALLT-OK AND (DCS-CDC OR                                           
185500                     DCS-SDC)                                             
185600                                                                          
185700     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
185800     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
185900     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
186000     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
186100     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
186200     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
186300     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
186400     MOVE ORAD-IDDC            TO DLEV-IDDC-IN                            
186500     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
186600     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
186700     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
186800     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
186900     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
187000     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
187100     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
187200     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
187300     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
187400                                                                          
187500     MOVE +1 TO WS-INDEX                                                  
187600     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
187700        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
187800                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
187900        ADD +1 TO WS-INDEX                                                
188000     END-PERFORM                                                          
188100                                                                          
188200     MOVE 1                    TO DLEV-KDCALL                             
188300     MOVE SPACE                TO DLEV-CLEARGROUP                         
188400     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
188500                                                                          
188600     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
188700                                       DLEV-LEVG-PCB                      
188800                                       DLEV-LEVA-PCB                      
188900                                       DLEV-ARTS-PCB                      
189000                                       DLEV-WDB6-PCB                      
189100                                       TPO2-FILA-PCB                      
189200                                                                          
189300     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82                                 
189400        MOVE JA                  TO OBKR-SW                               
189500        MOVE NEJ                 TO ALLT-SW                               
189600        MOVE ZERO                TO KVAN-KDORDBEK-UT                      
189700     ELSE                                                                 
189800        IF DLEV-KDORDBEK-UT = 95                                          
189900           MOVE JA               TO OBKR-SW                               
190000        END-IF                                                            
190100        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
190200           IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                             
190300             MOVE DLEV-IDDC-UT      TO ORAD-IDDC                          
190400                                       WS-IDDC                            
190500           END-IF                                                         
190600        END-IF                                                            
190700        MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
190800        MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)        
190900        MOVE DLEV-KVDAGAR-DIFF-UT TO                                      
191000                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
191100        MOVE DLEV-TISKEPPN-DDC-UT TO                                      
191200                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
191300                                                                          
191400        MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                         
191500        MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                        
191600        MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                         
191700        IF DLEV-FLSDCLEV-UT = JA                                          
191800          MOVE DLEV-IDDC-UT       TO ORAD-IDDC                            
191900                                     WS-IDDC                              
192000        ELSE                                                              
192100          MOVE DLEV-KDOI-UT       TO ORAD-KDOI                            
192200          MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                      
192300        END-IF                                                            
192400     END-IF                                                               
192500                                                                          
192600     IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                       
192700        AND ORAD-IDDC = WC-CDC-SE                                         
192800        MOVE OHUV-IDDISTR     TO TEST-IDDISTR                             
192900        IF DIST18-SKROT                                                   
193000          CONTINUE                                                        
193100        ELSE                                                              
193200          MOVE 26             TO DLEV-KDORDBEK-UT                         
193300          MOVE JA             TO OBKR-SW                                  
193400        END-IF                                                            
193500     END-IF                                                               
193600                                                                          
193700     END-IF                                                               
193800     .                                                                    
193900     EJECT                                                                
194000 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
194100                                                                          
194200     IF ALLT-OK                                                           
194300                                                                          
194400     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
194500     MOVE ORAD-IDDC            TO KERS-IDDC                               
194600     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
194700     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
194800     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
194900     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
195000     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
195100     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
195200     MOVE AREG-KDERS           TO KERS-KDERS                              
195300     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
195400     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
195500     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
195600     MOVE AREG-KDUART          TO KERS-KDUART                             
195700     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
195800     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
195900     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
196000     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
196100     EJECT                                                                
196200     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
196300                         KERS-ARTC-PCB KERS-ERSA-PCB                      
196400                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
196500                                                                          
196600     IF KERS-KDORDBEK > ZERO   AND                                        
196700        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
196800          KERS-KDERS > 10 )                                               
196900        MOVE JA                  TO OBKR-SW                               
197000        MOVE NEJ                 TO ALLT-SW                               
197100     END-IF                                                               
197200                                                                          
197300     IF WS-IDDC NOT = W-IDDC-B6                                           
197400        MOVE WS-IDDC TO W-IDDC-B6                                         
197500        PERFORM IMS-GU-WDB601                                             
197600     END-IF                                                               
197700     IF KERS-KDORDBEK > +0                                                
197800       IF DCS-SDC AND OHUV-FLFORBI = NEJ                                  
197900        IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR 19 OR         
198000                        21 OR 22 OR 24 OR 25 OR 27 OR 28 OR 29            
198100             MOVE JA            TO KOLLA-ERS-SW                           
198200        END-IF                                                            
198300       END-IF                                                             
198400       IF DCS-NDC                                                         
198500         IF AREG-KDERS > 18                                               
198600           IF OHUV-FLFORBI = NEJ                                          
198700              MOVE JA              TO KOLLA-ERS-SW                        
198800           END-IF                                                         
198900         ELSE                                                             
199000           MOVE ZERO               TO KERS-KDORDBEK                       
199100           PERFORM S02-RENSA-TILLK-TAB                                    
199200           MOVE JA                 TO ALLT-SW                             
199300         END-IF                                                           
199400       END-IF                                                             
199500     END-IF                                                               
199600     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
199700        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
199800***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
199900***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
200000***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
200100***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
200200           CONTINUE                                                       
200300        ELSE                                                              
200400           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
200500           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
200600        END-IF                                                            
200700     END-IF                                                               
200800     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
200900        MOVE ZERO                TO DLEV-KDORDBEK-UT                      
201000     END-IF                                                               
201100                                                                          
201200     ELSE                                                                 
201300        MOVE +0                  TO KERS-KDERS                            
201400     END-IF                                                               
201500     .                                                                    
201600     EJECT                                                                
201700 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
201800                                                                          
201900     IF ALLT-OK OR KOLLA-ERS                                              
202000                                                                          
202100     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
202200     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
202300     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
202400     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
202500     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
202600     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
202700     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
202800     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
202900     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
203000     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
203100     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
203200     MOVE AREG-FLIART          TO SPAR-FLIART                             
203300     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
203400     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
203500     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
203600     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
203700     IF WS-IDDC NOT = W-IDDC-B6                                           
203800        MOVE WS-IDDC TO W-IDDC-B6                                         
203900        PERFORM IMS-GU-WDB601                                             
204000     END-IF                                                               
204100     IF DCS-DDC                                                           
204200       MOVE OHUV-IDDC-PRIM     TO SPAR-IDDC                               
204300     ELSE                                                                 
204400       MOVE ORAD-IDDC          TO SPAR-IDDC                               
204500     END-IF                                                               
204600     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
204700     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
204800     MOVE AREG-KDERS           TO SPAR-KDERS                              
204900     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
205000     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
205100     MOVE +1                   TO SPAR-KDORDBEH                           
205200     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
205300     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
205400     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
205500     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
205600     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
205700     MOVE AREG-KDUART          TO SPAR-KDUART                             
205800     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
205900     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
206000     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
206100     MOVE ORAD-TITPO           TO SPAR-TITPO                              
206200     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
206300     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
206400     EJECT                                                                
206500                                                                          
206600     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
206700                                       SPAR-WDF8A-PCB                     
206800                                       SPAR-WDK6-PCB                      
206900                                                                          
207000     IF SPAR-KDORDBEK        > ZERO                                       
207100       MOVE JA                TO OBKR-SW                                  
207200       MOVE NEJ               TO ALLT-SW                                  
207300       IF SPAR-KDORDBEK = 51 OR 67 OR 58                                  
207400         MOVE NEJ             TO KOLLA-ERS-SW                             
207500       END-IF                                                             
207800       MOVE ZERO           TO XDCA-DAPUBL                                 
207600*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411NDCA                         
207700        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
207800          MOVE 99999999       TO XDCA-DAPUBL                              
207900*         MOVE 99999999       TO NDCA-DAPUBL                              
208000        END-IF                                                            
208010*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
208020*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
208030        IF SPAR-KDORDBEK = 54 AND NOT DCS-DDC                             
208040           MOVE JA             TO ALLT-SW                                 
208050        END-IF                                                            
208100       IF KVAN-KDORDBEK-UT > +0                                           
208200          MOVE +0             TO KVAN-KDORDBEK-UT                         
208300          MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                           
208400       END-IF                                                             
208500       IF DLEV-KDORDBEK-UT > +0                                           
208600          MOVE +0             TO DLEV-KDORDBEK-UT                         
208700       END-IF                                                             
208800     END-IF                                                               
208900                                                                          
209000     IF AREG-KDSORT = 'SW'                                                
209100        MOVE 67                     TO SPAR-KDORDBEK                      
209200        MOVE JA                     TO OBKR-SW                            
209300        MOVE NEJ                    TO ALLT-SW                            
209400     END-IF                                                               
209500                                                                          
209600     END-IF                                                               
209700     .                                                                    
209800     EJECT                                                                
209900 ECJ-KOMPLETTERA-PRIS SECTION.                                            
210000                                                                          
210100     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
210200                                                                          
210300     IF DIST79-DEALER-PRICE                                               
210400       IF WS-IDPRQUES                = +0                                 
210500          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
210600          MOVE +1                    TO PRNO-KDCALL                       
210700                                                                          
210800          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
210900                                                                          
211000          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
211100                                        WS-IDPRQUES                       
211200          MOVE +1                    TO PRQU-KDCALL                       
211300       ELSE                                                               
211400          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
211500          MOVE +2                    TO PRNO-KDCALL                       
211600                                                                          
211700          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
211800                                                                          
211900          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
212000                                        WS-IDPRQUES                       
212100          MOVE +2                    TO PRQU-KDCALL                       
212200       END-IF                                                             
212300                                                                          
212400       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
212500       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
212600       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
212700       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
212800       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
212900       MOVE 'N'                      TO PRQU-KDPRSTA                      
213000       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
213100       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
213200                                                                          
213300       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
213400       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
213500       PERFORM IMS-GU-WDB101                                              
213600       MOVE BET-KDVALISO             TO ORAD-KDVALISO                     
213700                                        PRQU-KDVALISO                     
213800                                                                          
213900       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
214000       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
214100       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
214200                                                                          
214300       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
214400                                          PRQU-WDC7-PCB                   
214500                                          PRQU-SJKO-WDK6-PCB              
214600                                                                          
214700       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
214800                                         WS-IDPRQUES                      
214900       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
215000       IF ORAD-PRARTNTO-LOC = +0                                          
215100          MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL            
215200       END-IF                                                             
215300                                                                          
215400       IF ORAD-PRARTNTO-LOC NOT = +0                                      
215500         IF ORAD-KDPRTYP = SPACE                                          
215600           MOVE 'P'            TO ORAD-KDPRTYP                            
215700           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
215800         END-IF                                                           
215900       END-IF                                                             
216000                                                                          
216100     ELSE                                                                 
216200*         *NOT DIST79-DEALER-PRICE                                        
216300                                                                          
216400          IF WS-IDDC NOT = W-IDDC-B6                                      
216500             MOVE WS-IDDC TO W-IDDC-B6                                    
216600             PERFORM IMS-GU-WDB601                                        
216700          END-IF                                                          
216800                                                                          
216900          IF DCS-NDC OR DCS-SDC OR DCS-DDC OR                             
217000             AREG-KDUART = SPACE                                          
217100             PERFORM S03-PRISTILLAMPA                                     
217200          ELSE                                                            
217300             IF OHUV-FLFORBI = JA OR                                      
217400                OHUV-FLFORBI = SPEC-FORBI                                 
217500                PERFORM S03-PRISTILLAMPA                                  
217600             END-IF                                                       
217700          END-IF                                                          
217800     END-IF                                                               
217900     END-IF                                                               
218000     .                                                                    
218100     EJECT                                                                
218200 ECM-KOMPLETTERA-TPO1 SECTION.                                            
218300                                                                          
218400     IF WS-IDDC NOT = W-IDDC-B6                                           
218500        MOVE WS-IDDC TO W-IDDC-B6                                         
218600        PERFORM IMS-GU-WDB601                                             
218700     END-IF                                                               
218800                                                                          
218900     IF ALLT-OK AND (DCS-CDC OR                                           
219000                     DCS-SDC)                                             
219100                                                                          
219200     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
219300     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
219400     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
219500     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
219600     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
219700     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
219800     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
219900     MOVE OHUV-IDKST           TO TPO1-IDKST                              
220000     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
220100     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
220200     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
220300     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
220400     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
220500     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
220600     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
220700     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
220800     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
220900     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
221000     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
221100     MOVE ORAD-TITPO           TO TPO1-TITPO                              
221200     IF ORAD-KDPRTYP = 'P'                                                
221300        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
221400        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
221500        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
221600        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
221700     ELSE                                                                 
221800        MOVE ZERO              TO TPO1-PRARTNTO                           
221900        MOVE SPACE             TO TPO1-KDPRTYP                            
222000        MOVE NEJ               TO TPO1-FLPRTILL                           
222100     END-IF                                                               
222200     EJECT                                                                
222300     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
222400     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
222500     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
222600     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
222700     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
222800     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
222900     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
223000     MOVE +1                   TO TPO1-KDORDBEH                           
223100     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
223200     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
223300     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
223400     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
223500     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
223600                                                                          
223700     MOVE +0                   TO TPO1-KDORDBEK                           
223800     MOVE SPACE                TO TPO1-FLKLAR                             
223900                                                                          
224000     MOVE OHUV-KDORDTYP-LDC   TO TPO1-KDORDTYP-LDC                        
224100     MOVE OHUV-TIREPDAT       TO TPO1-TIREPDAT                            
224200     MOVE ORAD-IDKUNDRF-WIP   TO TPO1-IDKUNDRF-WIP                        
224300                                                                          
224400     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
224500                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
224600                                                                          
224700     IF TPO1-KDORDBEK > +0                                                
224800        MOVE JA                TO OBKR-SW                                 
224900        MOVE NEJ               TO ALLT-SW                                 
225000        MOVE WC-CDC-SE         TO ORAD-IDDC                               
225100        MOVE ORAD-IDDC         TO WS-IDDC                                 
225200        IF KVAN-KDORDBEK-UT > +0                                          
225300           MOVE +0             TO KVAN-KDORDBEK-UT                        
225400           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
225500        END-IF                                                            
225600        IF DLEV-KDORDBEK-UT > +0                                          
225700           MOVE +0             TO DLEV-KDORDBEK-UT                        
225800        END-IF                                                            
225900     ELSE                                                                 
226000        IF TPO1-FLKLAR = JA                                               
226100           MOVE NEJ            TO ALLT-SW                                 
226200           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
226300              MOVE ZERO        TO KERS-KDORDBEK                           
226400              PERFORM S02-RENSA-TILLK-TAB                                 
226500              MOVE NEJ         TO TILLK-SW                                
226600           END-IF                                                         
226700        END-IF                                                            
226800     END-IF                                                               
226900                                                                          
227000     END-IF                                                               
227100     .                                                                    
227200     EJECT                                                                
227300 ECN-KOMPLETTERA-TPO2 SECTION.                                            
227400                                                                          
227500     IF WS-IDDC NOT = W-IDDC-B6                                           
227600        MOVE WS-IDDC TO W-IDDC-B6                                         
227700        PERFORM IMS-GU-WDB601                                             
227800     END-IF                                                               
227900                                                                          
228000     IF DCS-SDC           AND                                             
228100        ORAD-KDORDKL  = 1 AND                                             
228200        ORAD-IDKAMPRF = 0 AND                                             
228300       (AREG-KDUART   = 'L' OR  AREG-KDUART = 'P')                        
228400                                                                          
228500       CONTINUE                                                           
228600     ELSE                                                                 
228700       IF ALLT-OK AND (DCS-CDC OR                                         
228800                       DCS-SDC)                                           
228900                                                                          
229000       MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                          
229100       MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                         
229200       MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                         
229300       MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                          
229400       MOVE ORAD-BERADREF        TO TPO2-BERADREF                         
229500       MOVE AREG-IDANSK          TO TPO2-IDANSK                           
229600       MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                          
229700       MOVE OHUV-IDKST           TO TPO2-IDKST                            
229800       MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                         
229900       MOVE ORAD-KDDSP           TO TPO2-KDDSP                            
230000       MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                         
230100       MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                          
230200       MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                         
230300       MOVE ORAD-KDORDING        TO TPO2-KDORDING                         
230400       MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                          
230500       MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                         
230600       MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                         
230700       MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                        
230800       MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                         
230900       MOVE ORAD-TITPO           TO TPO2-TITPO                            
231000       MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                         
231100       MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                     
231200       MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                          
231300       MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                         
231400       EJECT                                                              
231500       MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                         
231600       MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                         
231700       MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                         
231800       MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                         
231900       MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                         
232000       MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                         
232100       MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                          
232200       MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                          
232300       MOVE AREG-KDUART          TO TPO2-KDUART                           
232400       MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                         
232500       MOVE +1                   TO TPO2-KDORDBEH                         
232600       MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                         
232700       MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                         
232800       MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                          
232900       MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                         
233000       MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                         
233100       MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                        
233200       MOVE ORAD-KVBEART         TO TPO2-KVBEART                          
233300                                                                          
233400       MOVE +0                   TO TPO2-KDORDBEK                         
233500       MOVE SPACE                TO TPO2-FLKLAR                           
233600                                                                          
233700       MOVE OHUV-KDORDTYP-LDC   TO TPO2-KDORDTYP-LDC                      
233800       MOVE OHUV-TIREPDAT       TO TPO2-TIREPDAT                          
233900       MOVE ORAD-IDKUNDRF-WIP   TO TPO2-IDKUNDRF-WIP                      
234000                                                                          
234100       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
234200                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
234300                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
234400                         TIME-4437-PCB                                    
234500                                                                          
234600       IF TPO2-KDORDBEK > +0                                              
234700          MOVE JA                TO OBKR-SW                               
234800          MOVE NEJ               TO ALLT-SW                               
234900          MOVE WC-CDC-SE         TO ORAD-IDDC                             
235000          MOVE ORAD-IDDC         TO WS-IDDC                               
235100          MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                         
235200          IF ORAD-KDTPOTYP = 6                                            
235300             IF ORAD-KDPRTYP NOT = 'P'                                    
235400                MOVE ZERO        TO ORAD-PRARTNTO                         
235500                MOVE SPACE       TO ORAD-KDPRTYP                          
235600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
235700                IF NOT DIST79-DEALER-PRICE                                
235900                  MOVE ZERO        TO ORAD-PRARTNTO-LOC                   
236000                  MOVE NEJ         TO ORAD-FLPRTILL                       
236100                END-IF                                                    
236200*************                                                             
236300             END-IF                                                       
236400          END-IF                                                          
236500       ELSE                                                               
236600          IF TPO2-FLKLAR = JA                                             
236700             MOVE NEJ            TO ALLT-SW                               
236800             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
236900                MOVE ZERO        TO KERS-KDORDBEK                         
237000                PERFORM S02-RENSA-TILLK-TAB                               
237100                MOVE NEJ         TO TILLK-SW                              
237200             END-IF                                                       
237300          END-IF                                                          
237400       END-IF                                                             
237500                                                                          
237600       END-IF                                                             
237700     END-IF                                                               
237800     .                                                                    
237900     EJECT                                                                
238000 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
238100                                                                          
238200     IF WS-IDDC NOT = W-IDDC-B6                                           
238300        MOVE WS-IDDC TO W-IDDC-B6                                         
238400        PERFORM IMS-GU-WDB601                                             
238500     END-IF                                                               
238600                                                                          
238700     IF ALLT-OK AND (DCS-CDC OR                                           
238800                     DCS-SDC)                                             
238900                                                                          
239000       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
239100       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
239200       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
239300       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
239400       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
239500       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
239600       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
239700       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
239800       MOVE OHUV-IDKST         TO KAMP-IDKST                              
239900       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
240000       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
240100       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
240200       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
240300       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
240400       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
240500       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
240600       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
240700       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
240800       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
240900       MOVE ORAD-TITPO         TO KAMP-TITPO                              
241000       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
241100          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
241200          MOVE ORAD-DEAL-PR-LINE                                          
241300                               TO KAMP-DEAL-PR-LINE                       
241400          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
241500          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
241600       ELSE                                                               
241700          MOVE ZERO            TO KAMP-PRARTNTO                           
241800          MOVE SPACE           TO KAMP-KDPRTYP                            
241900          MOVE NEJ             TO KAMP-FLPRTILL                           
242000       END-IF                                                             
242100       EJECT                                                              
242200       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
242300       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
242400       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
242500       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
242600       MOVE ORAD-IDDC          TO KAMP-IDDC                               
242700       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
242800       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
242900       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
243000       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
243100       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
243200       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
243300       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
243400                                                                          
243500       MOVE +0                 TO KAMP-KDORDBEK                           
243600       MOVE SPACE              TO KAMP-FLKLAR                             
243700                                                                          
243800       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
243900                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
244000                                                                          
244100       IF KAMP-KDORDBEK > +0                                              
244200          MOVE JA              TO OBKR-SW                                 
244300          MOVE NEJ             TO ALLT-SW                                 
244400          MOVE WC-CDC-SE       TO ORAD-IDDC                               
244500          MOVE ORAD-IDDC       TO WS-IDDC                                 
244600          IF KVAN-KDORDBEK-UT > +0                                        
244700             MOVE +0           TO KVAN-KDORDBEK-UT                        
244800             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
244900          END-IF                                                          
245000          IF DLEV-KDORDBEK-UT > +0                                        
245100             MOVE +0           TO DLEV-KDORDBEK-UT                        
245200          END-IF                                                          
245300       ELSE                                                               
245400          IF KAMP-FLKLAR = JA                                             
245500             MOVE NEJ          TO ALLT-SW                                 
245600             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
245700                MOVE ZERO      TO KERS-KDORDBEK                           
245800                PERFORM S02-RENSA-TILLK-TAB                               
245900                MOVE NEJ       TO TILLK-SW                                
246000             END-IF                                                       
246100          END-IF                                                          
246200       END-IF                                                             
246300                                                                          
246400     END-IF                                                               
246500     .                                                                    
246600     EJECT                                                                
246700 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
246800                                                                          
246900       IF ALLT-OK AND W-KDORDBEK = 56                                     
247000                                                                          
247100         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
247200         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
247300         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
247400         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
247500         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
247600         MOVE AREG-IDANSK          TO RELS-IDANSK                         
247700         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
247800         MOVE OHUV-IDKST           TO RELS-IDKST                          
247900         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
248000         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
248100         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
248200         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
248300         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
248400         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
248500         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
248600         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
248700         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
248800         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
248900         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
249000         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
249100         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
249200         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
249300         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
249400         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
249500         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
249600         EJECT                                                            
249700         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
249800         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
249900         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
250000         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
250100         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
250200         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
250300         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
250400         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
250500         MOVE AREG-KDUART          TO RELS-KDUART                         
250600         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
250700         MOVE +1                   TO RELS-KDORDBEH                       
250800         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
250900         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
251000         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
251100         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
251200         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
251300         MOVE 0                    TO RELS-KVQPACK-1                      
251400         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
251500         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
251600                                                                          
251700         MOVE SPACE                TO RELS-FLKLAR                         
251800                                                                          
251900         MOVE OHUV-KDORDTYP-LDC   TO RELS-KDORDTYP-LDC                    
252000         MOVE OHUV-TIREPDAT       TO RELS-TIREPDAT                        
252100         MOVE ORAD-IDKUNDRF-WIP   TO RELS-IDKUNDRF-WIP                    
252200                                                                          
252300         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
252400                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
252500                                                                          
252600         PERFORM ECTA-ANDRA-WDC711                                        
252700         IF RELS-KDORDBEK > +0                                            
252800            MOVE JA                TO OBKR-SW                             
252900            MOVE NEJ               TO ALLT-SW                             
253000            MOVE WC-CDC-SE         TO ORAD-IDDC                           
253100            MOVE ORAD-IDDC         TO WS-IDDC                             
253200         ELSE                                                             
253300            IF RELS-FLKLAR = JA                                           
253400               MOVE NEJ            TO ALLT-SW                             
253500               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
253600                  MOVE ZERO        TO KERS-KDORDBEK                       
253700                  PERFORM S02-RENSA-TILLK-TAB                             
253800               END-IF                                                     
253900            END-IF                                                        
254000         END-IF                                                           
254100                                                                          
254200       MOVE SPACE                TO RELS-FLKLAR                           
254300       MOVE +0                   TO W-KDORDBEK                            
254400                                                                          
254500       END-IF                                                             
254600     .                                                                    
254700     EJECT                                                                
254800 ECTA-ANDRA-WDC711 SECTION.                                               
254900                                                                          
255000     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
255100       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
255200         INITIALIZE PRQU-W335PRQU                                         
255300         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
255400         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
255500         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
255600         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
255700         MOVE 6                  TO PRQU-KDCALL                           
255800         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
255900                                            PRQU-WDC7-PCB                 
256000                                            PRQU-SJKO-WDK6-PCB            
256100         MOVE 'N'                TO ORAD-FLPRTILL                         
256200       END-IF                                                             
256300     END-IF                                                               
256400     .                                                                    
256500     EJECT                                                                
256600                                                                          
256700                                                                          
256800 ECG-PREL-AVBOKNING-XDC SECTION.                                          
256900                                                                          
257100     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
257300                                                                          
257400       IF WS-IDDC NOT = W-IDDC-B6                                         
257500          MOVE WS-IDDC TO W-IDDC-B6                                       
257600          PERFORM IMS-GU-WDB601                                           
257700       END-IF                                                             
257800       IF DCS-NDC                                                         
257900*        IF OHUV-IDUSER = 'PCCN616'                                       
258000            MOVE JA TO ALLT-SW                                            
258200            PERFORM ECGX-PREL-AVBOKNING-XDC                               
258300*        END-IF                                                           
258400                                                                          
258500*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
258600*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
258700*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
258800*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
258900*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
259000*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
259100*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
259200*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
259300*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
259400*        MOVE OHUV-IDDC-TVS        TO NDCA-IDDC-TVS                       
259500*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
259600*        MOVE 1                    TO NDCA-IXDCCLEAR                      
259700*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
259800*        MOVE AREG-KDERS           TO NDCA-KDERS                          
259900*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
260000*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
260100*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
260200*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
260300*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
260400*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
260500*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
260600*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
260700*        MOVE ORAD-VKART           TO NDCA-VKART                          
260800*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
260900*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
261000                                                                          
261100*        MOVE SPACE                TO CLDC-W411CLDC                       
261200*        MOVE ORAD-IDDC            TO CLDC-IDDC-CLEAR(1)                  
261300*        MOVE DLI-IO-AREA-B601     TO CLDC-WDB601(1)                      
261400*        MOVE +2                   TO NDCA-KDCALL                         
261500*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
261600*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
261700*                                     NDCA-XDK7-KVOKS-DAG                 
261800*                                     NDCA-XDK7-KVOKS-BULK                
261900                                                                          
262000*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
262100*                                          NDCA-USEA-PCB                  
262200*                                          NDCA-WDK7-PCB                  
262300*                                          NDCA-WDL6-PCB                  
262400*                                          NDCA-WDB6-PCB                  
262500*                                          NDCA-XDK7-W411XDK7             
262600                                                                          
262700*          PERFORM ECGX-CHECK-DIFF                                        
262800                                                                          
263100         IF XDCA-KDORDBEK > ZERO                                          
263200           IF SPAR-FLPUBCDC = YES                                         
263300*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE            
263400*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
263500*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
263600              IF XDCA-KDORDBEK = 15 AND XDCA-DAPUBL > ZERO                
263700                 MOVE ZERO TO SPAR-KDORDBEK                               
263800              ELSE                                                        
263900                 MOVE ZERO TO XDCA-KDORDBEK                               
264000              END-IF                                                      
264100           END-IF                                                         
264200           IF KOLLA-ERS                                                   
264300              IF XDCA-KVPREAVB > 0                                        
264400                MOVE ZERO            TO KERS-KDORDBEK                     
264500                PERFORM S02-RENSA-TILLK-TAB                               
264600                MOVE ZERO            TO SPAR-KDORDBEK                     
264700              ELSE                                                        
264800                MOVE ZERO            TO XDCA-KDORDBEK                     
264900              END-IF                                                      
265000           ELSE                                                           
265100             IF XDCA-KDORDBEK = 15                                        
265200                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
265300                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
265400                END-IF                                                    
265500                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
265600                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
265700                END-IF                                                    
265800                IF SDCA-KDORDBEK = 15                                     
265900                   MOVE ZERO         TO SDCA-KDORDBEK                     
266000                END-IF                                                    
266100             END-IF                                                       
266200             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
266300                 MOVE ZERO           TO XDCA-KDORDBEK                     
266400             END-IF                                                       
266500           END-IF                                                         
266600           MOVE JA                   TO OBKR-SW                           
266700         ELSE                                                             
266900           IF KOLLA-ERS    OR                                             
267000             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
267100             IF XDCA-KVPREAVB > 0                                         
267200               MOVE ZERO             TO KERS-KDORDBEK                     
267300               PERFORM S02-RENSA-TILLK-TAB                                
267400               MOVE ZERO             TO SPAR-KDORDBEK                     
267500             ELSE                                                         
267600               MOVE JA               TO OBKR-SW                           
267700             END-IF                                                       
267800           ELSE                                                           
268100             IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                 
268200               MOVE ZERO             TO SPAR-KDORDBEK                     
268400             END-IF                                                       
268500           END-IF                                                         
268600           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
268700*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
268800*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
268900              MOVE 0  TO SPAR-KDORDBEK                                    
269000              MOVE JA  TO ALLT-SW                                         
269100              MOVE NEJ TO OBKR-SW                                         
269200           END-IF                                                         
269300         END-IF                                                           
269400         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
269500         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
269600         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
269700         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
269800         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
269900         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
270000         MOVE XDCA-KDOI              TO ORAD-KDOI                         
270100         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
270200         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
270300         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
270400         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
270500         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
270600         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
270700         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
270800         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
270900         MOVE NEJ                    TO ALLT-SW                           
271000       END-IF                                                             
271100                                                                          
271200     END-IF                                                               
271300     .                                                                    
271400     EJECT                                                                
271500                                                                          
271600 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
271700                                                                          
271800     IF ALLT-OK OR KOLLA-ERS                                              
271900                                                                          
272000       IF WS-IDDC NOT = W-IDDC-B6                                         
272100          MOVE WS-IDDC TO W-IDDC-B6                                       
272200          PERFORM IMS-GU-WDB601                                           
272300       END-IF                                                             
272400                                                                          
272500       IF DCS-NDC                                                         
272600                                                                          
272700* XDCA-INPUT                                                              
272800         MOVE +1 TO WS-INDEX                                              
272900         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
273000           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
273100                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
273200           ADD +1 TO WS-INDEX                                             
273300         END-PERFORM                                                      
273400                                                                          
273500         MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                       
273600         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
273700         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
273800         MOVE ORAD-IDDC            TO XDCA-IDDC                           
273900         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
274000         MOVE AREG-KDERS           TO XDCA-KDERS                          
274100         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
274200         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
274300         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
274400         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
274500         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
274600         MOVE ORAD-VKART           TO XDCA-VKART                          
274700         MOVE +1                   TO XDCA-KDCALL                         
274800                                                                          
274900* XDCA-OUTPUT                                                             
275000         MOVE SPACE                TO XDCA-IDDC-OUT                       
275100                                      XDCA-IDDC-RO                        
275200                                      XDCA-KDARTURS                       
275300                                      XDCA-KDOI                           
275400                                      XDCA-CLEARGROUP                     
275500         MOVE ZERO                 TO XDCA-ADLAGOMR                       
275600                                      XDCA-ADGANG                         
275700                                      XDCA-ADPLATS                        
275800                                      XDCA-KDORDBEK                       
275900                                      XDCA-KVPREAVB                       
276000                                      XDCA-KVPRERO                        
276100                                      XDCA-TIREGDAT-OUT                   
276200                                      XDCA-TIREGTID-OUT                   
276300                                      XDCA-VKART-OUT                      
276400                                      XDCA-VKART-NTO                      
276500                                      XDCA-VLARTNTO                       
276600         MOVE ZERO                 TO                                     
276700                                      XDCA-KVOKS-DAG                      
276800                                      XDCA-KVOKS-BULK                     
276900         IF XDCA-DAPUBL NOT = 99999999                                    
277000            MOVE ZERO              TO XDCA-DAPUBL                         
277100         END-IF                                                           
277200                                                                          
277300         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
277400         XDCA-USEA-PCB                                                    
277500         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
277600         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
277700         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
277800         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
277900         XDCA-WDK7-3-PCB                                                  
278000                                                                          
278100* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
278200* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
278300         IF XDCA-KDARTURS = SPACE                                         
278400           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
278500         END-IF                                                           
278600         IF XDCA-VKART-NTO = ZERO                                         
278700           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
278800         END-IF                                                           
278900         IF XDCA-VLARTNTO = ZERO                                          
279000           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
279100         END-IF                                                           
279200       END-IF                                                             
279300     END-IF                                                               
279400     .                                                                    
279500     EJECT                                                                
279600 ECGX-CHECK-DIFF SECTION.                                                 
279700                                                                          
279800     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
279900     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
280000     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
280100     AND NDCA-ADGANG     = XDCA-ADGANG                                    
280200     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
280300     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
280400     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
280500     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
280600     AND NDCA-KDOI       = XDCA-KDOI                                      
280700     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
280800     AND NDCA-VKART      = XDCA-VKART-OUT                                 
280900     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
281000     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
281100     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
281200     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
281300     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
281400     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
281500         MOVE NEJ TO DIFF-FLSVAR                                          
281600     ELSE                                                                 
281700        MOVE JA           TO DIFF-FLSVAR                                  
281800     END-IF                                                               
281900                                                                          
282000* ORDER LOG INFO                                                          
282100     IF DIFF-FLSVAR = JA                                                  
282200       MOVE IDPGM         TO FIL-IDPGM                                    
282300       ACCEPT FIL-TIREGDAT FROM DATE                                      
282400       ACCEPT FIL-TIKLOCK FROM TIME                                       
282500       MOVE 1             TO FIL-IDSEKVNR                                 
282600       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
282700       MOVE 'A'           TO FIL-CT-IDVTYP                                
282800       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
282900                                                                          
283000*   ORDER LINE INFO                                                       
283100       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
283200       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
283300       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
283400       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
283500       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
283600       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
283700       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
283800       MOVE '4222'         TO DIFF-IDSYSTEM                               
283900                                                                          
284000*   NDCA INFO                                                             
284100       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
284200       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
284300       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
284400       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
284500       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
284600       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
284700       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
284800       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
284900       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
285000       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
285100       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
285200       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
285300       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
285400       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
285500       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
285600       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
285700                                                                          
285800*   XDCA INFO                                                             
285900       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
286000       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
286100       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
286200       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
286300       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
286400       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
286500       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
286600       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
286700       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
286800       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
286900       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
287000       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
287100       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
287200       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
287300       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
287400       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
287500                                                                          
287600       PERFORM IMS-ISRT-WDR601                                            
287700       IF SEGMENT-FINNS-REDAN                                             
287800          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
287900             ADD 1 TO FIL-IDSEKVNR                                        
288000             PERFORM IMS-ISRT-WDR601                                      
288100          END-PERFORM                                                     
288200       END-IF                                                             
288300     END-IF                                                               
288400     .                                                                    
288500     EJECT                                                                
288600 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
288700                                                                          
288800     IF ALLT-OK OR KOLLA-ERS                                              
288900                                                                          
289000       IF WS-IDDC NOT  = W-IDDC-B6                                        
289100          MOVE WS-IDDC TO W-IDDC-B6                                       
289200          PERFORM IMS-GU-WDB601                                           
289300       END-IF                                                             
289400                                                                          
289500       IF  DCS-SDC                                                        
289600                                                                          
289700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
289800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
289900         MOVE NEJ                  TO SDCA-FLORDSPE                       
290000         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
290100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
290200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
290300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
290400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
290500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
290600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
290700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
290800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
290900         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
291000         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
291100         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
291200         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
291300         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
291400         MOVE +0                   TO SDCA-TIREPDAT                       
291500         MOVE +0                   TO SDCA-KVOKS-PREL                     
291600         MOVE +1                   TO SDCA-KDCALL                         
291700         MOVE +1                   TO SDCA-IXDCCLEAR                      
291800                                                                          
291900         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
292000                                           SDCA-WDB6-PCB                  
292100                                           SDCA-WDK9-PCB                  
292200                                           SDCA-WDR6-PCB                  
292300                                           SDCA-WDK6-PCB                  
292400                                           SDCA-WDQ4B-PCB                 
292500                                           SDCA-WDQ2-PCB                  
292600                                           SDCA-WDQ4-PCB                  
292700                                           SDCA-WDB6-2-PCB                
292800                                           SDCA-WDK6-2-PCB                
292900                                           SDCA-WDK7-2-PCB                
293000                                           SDCA-WDK7-3-PCB                
293100                                                                          
293200         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
293300         MOVE ZERO          TO SDCA-KDORDBEK                              
293400                                                                          
293500         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
293600           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
293700              MOVE JA        TO OBKR-SW                                   
293800              MOVE NEJ       TO ALLT-SW                                   
293900           ELSE                                                           
294000             IF ORAD-KDORDKL > 0                                          
294100             AND ORAD-IDSYSTEM NOT = 'OREL'                               
294200             AND (AREG-KDUART = 'L'                                       
294300             OR AREG-KDUART = 'P')                                        
294400             AND OHUV-FLORDSPE NOT = JA                                   
294500             AND OHUV-FLOVRLEV NOT = JA                                   
294600             AND OHUV-FLFORBI = NEJ                                       
294700               MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                    
294800               MOVE 70      TO TPO2-KDORDBEK                              
294900               MOVE JA      TO OBKR-SW                                    
295000               MOVE NEJ     TO ALLT-SW                                    
295100               MOVE WC-CDC-SE TO ORAD-IDDC                                
295200               MOVE ORAD-IDDC TO WS-IDDC                                  
295300               MOVE 6       TO ORAD-KDTPOTYP                              
295400               IF ORAD-KDPRTYP NOT = 'P'                                  
295500                  MOVE ZERO  TO ORAD-PRARTNTO                             
295600                  MOVE SPACE TO ORAD-KDPRTYP                              
295700**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
295800                  IF NOT DIST79-DEALER-PRICE                              
296000                    MOVE ZERO        TO ORAD-PRARTNTO-LOC                 
296100                    MOVE NEJ         TO ORAD-FLPRTILL                     
296200                  END-IF                                                  
296300*************                                                             
296400               END-IF                                                     
296500             ELSE                                                         
296600               MOVE JA                TO OBKR-SW                          
296700               MOVE NEJ               TO ALLT-SW                          
296800               IF SDCA-KDORDBEK-FIRST-SDC = 92                            
296900                 MOVE SDCA-KVBEART-Q  TO ORAD-KVPRERO                     
297000               END-IF                                                     
297100             END-IF                                                       
297200           END-IF                                                         
297300         ELSE                                                             
297400           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
297500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
297600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
297700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
297800           MOVE NEJ                  TO ALLT-SW                           
297900           IF KVAN-KDORDBEK-UT = ZERO                                     
298000             MOVE JA                 TO EGET-CL-RAD-SW                    
298100           END-IF                                                         
298200           IF KOLLA-ERS    OR                                             
298300             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
298400              MOVE NEJ               TO KOLLA-ERS-SW                      
298500              MOVE ZERO              TO KERS-KDORDBEK                     
298600              PERFORM S02-RENSA-TILLK-TAB                                 
298700              MOVE ZERO              TO SPAR-KDORDBEK                     
298800           END-IF                                                         
298900*TO CREATE CODE 26 FOR VOR LINES RELEASED FROM 4225 OR W41218.            
299000*CODE 26 SHOULD NOT BE CREATED FOR AIR FREIGHT                            
299100           IF OHUV-BEKUNDRF > SPACE                                       
299200           PERFORM ECWA-VOR-RELEASE-KDORDBEK-26                           
299300           END-IF                                                         
299400         END-IF                                                           
299500         MOVE SDCA-KDOI              TO ORAD-KDOI                         
299600         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
299700       END-IF                                                             
299800                                                                          
299900     END-IF                                                               
300000     .                                                                    
300100     EJECT                                                                
300200                                                                          
300300 ECWA-VOR-RELEASE-KDORDBEK-26 SECTION.                                    
300400                                                                          
300500     MOVE +1 TO IX                                                        
300600     PERFORM UNTIL IX > MAX-TVS-IX OR TVS-DC-ALLOC OR                     
300700                 GMT-IDDC-TVSVOR (IX) = SPACE                             
300800       IF SDCA-IDDC-TVS = GMT-IDDC-TVSVOR (IX)                            
300900*CODE 26 SHOULD NOT BE CREATED FOR AIR FREIGHT                            
301000         IF ARB-KDFRAKT NOT = 17                                          
301100           MOVE 26             TO WS-VOR-KDORDBEK                         
301200           MOVE JA             TO OBKR-SW                                 
301300           MOVE JA             TO TVS-DC-SW                               
301400         END-IF                                                           
301500       END-IF                                                             
301600       ADD +1 TO IX                                                       
301700     END-PERFORM                                                          
301800     .                                                                    
301900     EJECT                                                                
302000                                                                          
302100 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
302200                                                                          
302300     IF ALLT-OK OR KOLLA-ERS                                              
302400                                                                          
302500       IF WS-IDDC NOT  = W-IDDC-B6                                        
302600          MOVE WS-IDDC TO W-IDDC-B6                                       
302700          PERFORM IMS-GU-WDB601                                           
302800       END-IF                                                             
302900                                                                          
303000       IF  DCS-SDC                                                        
303100                                                                          
303200         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
303300         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
303400         MOVE NEJ                  TO SDCA-FLORDSPE                       
303500         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
303600         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
303700         MOVE ORAD-IDDC            TO SDCA-IDDC                           
303800         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
303900         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
304000         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
304100         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
304200         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
304300         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
304400         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
304500         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
304600         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
304700         MOVE +1                   TO SDCA-KDCALL                         
304800         MOVE +2                   TO SDCA-IXDCCLEAR                      
304900                                                                          
305000         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
305100                                           SDCA-WDB6-PCB                  
305200                                           SDCA-WDK9-PCB                  
305300                                           SDCA-WDR6-PCB                  
305400                                           SDCA-WDK6-PCB                  
305500                                           SDCA-WDQ4B-PCB                 
305600                                           SDCA-WDQ2-PCB                  
305700                                           SDCA-WDQ4-PCB                  
305800                                           SDCA-WDB6-2-PCB                
305900                                           SDCA-WDK6-2-PCB                
306000                                           SDCA-WDK7-2-PCB                
306100                                           SDCA-WDK7-3-PCB                
306200                                                                          
306300         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
306400         MOVE ZERO          TO SDCA-KDORDBEK                              
306500                                                                          
306600         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
306700           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
306800              MOVE JA        TO OBKR-SW                                   
306900              MOVE NEJ       TO ALLT-SW                                   
307000           ELSE                                                           
307100             IF ORAD-KDORDKL > 0                                          
307200             AND ORAD-IDSYSTEM NOT = 'OREL'                               
307300             AND (AREG-KDUART = 'L'                                       
307400             OR AREG-KDUART = 'P')                                        
307500             AND OHUV-FLORDSPE NOT = JA                                   
307600             AND OHUV-FLOVRLEV NOT = JA                                   
307700             AND OHUV-FLFORBI = NEJ                                       
307800               MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                   
307900               MOVE 70      TO TPO2-KDORDBEK                              
308000               MOVE JA      TO OBKR-SW                                    
308100               MOVE NEJ     TO ALLT-SW                                    
308200               MOVE WC-CDC-SE TO ORAD-IDDC                                
308300               MOVE ORAD-IDDC TO WS-IDDC                                  
308400               MOVE 6       TO ORAD-KDTPOTYP                              
308500               IF ORAD-KDPRTYP NOT = 'P'                                  
308600                  MOVE ZERO  TO ORAD-PRARTNTO                             
308700                  MOVE SPACE TO ORAD-KDPRTYP                              
308800**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
308900                  IF NOT DIST79-DEALER-PRICE                              
309100                    MOVE ZERO        TO ORAD-PRARTNTO-LOC                 
309200                    MOVE NEJ         TO ORAD-FLPRTILL                     
309300                  END-IF                                                  
309400*************                                                             
309500               END-IF                                                     
309600             ELSE                                                         
309700               MOVE JA                TO OBKR-SW                          
309800               MOVE NEJ               TO ALLT-SW                          
309900               IF SDCA-KDORDBEK-SECOND-SDC = 92                           
310000                 MOVE SDCA-KVBEART-Q  TO ORAD-KVPRERO                     
310100               END-IF                                                     
310200             END-IF                                                       
310300           END-IF                                                         
310400         ELSE                                                             
310500           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
310600           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
310700           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
310800           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
310900           MOVE NEJ                  TO ALLT-SW                           
311000           IF KVAN-KDORDBEK-UT = ZERO                                     
311100           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
311200             MOVE JA                 TO EGET-CL-RAD-SW                    
311300           END-IF                                                         
311400           IF KOLLA-ERS    OR                                             
311500             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
311600              MOVE NEJ               TO KOLLA-ERS-SW                      
311700              MOVE ZERO              TO KERS-KDORDBEK                     
311800              PERFORM S02-RENSA-TILLK-TAB                                 
311900              MOVE ZERO              TO SPAR-KDORDBEK                     
312000                                                                          
312100           END-IF                                                         
312200         END-IF                                                           
312300         MOVE SDCA-KDOI              TO ORAD-KDOI                         
312400         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
312500       END-IF                                                             
312600                                                                          
312700     END-IF                                                               
312800     .                                                                    
312900     EJECT                                                                
313000                                                                          
313100 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
313200                                                                          
313300     IF ALLT-OK OR KOLLA-ERS                                              
313400                                                                          
313500       IF WS-IDDC NOT = W-IDDC-B6                                         
313600          MOVE WS-IDDC TO W-IDDC-B6                                       
313700          PERFORM IMS-GU-WDB601                                           
313800       END-IF                                                             
313900                                                                          
314000       IF  DCS-SDC                                                        
314100                                                                          
314200         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
314300         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
314400         MOVE NEJ                  TO SDCA-FLORDSPE                       
314500         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
314600         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
314700         MOVE ORAD-IDDC            TO SDCA-IDDC                           
314800         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
314900         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
315000         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
315100         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
315200         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
315300         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
315400         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
315500         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
315600         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
315700         MOVE +1                   TO SDCA-KDCALL                         
315800         MOVE +3                   TO SDCA-IXDCCLEAR                      
315900                                                                          
316000         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
316100                                           SDCA-WDB6-PCB                  
316200                                           SDCA-WDK9-PCB                  
316300                                           SDCA-WDR6-PCB                  
316400                                           SDCA-WDK6-PCB                  
316500                                           SDCA-WDQ4B-PCB                 
316600                                           SDCA-WDQ2-PCB                  
316700                                           SDCA-WDQ4-PCB                  
316800                                           SDCA-WDB6-2-PCB                
316900                                           SDCA-WDK6-2-PCB                
317000                                           SDCA-WDK7-2-PCB                
317100                                           SDCA-WDK7-3-PCB                
317200                                                                          
317300         IF SDCA-KDORDBEK > ZERO                                          
317400           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
317500              MOVE JA        TO OBKR-SW                                   
317600              MOVE NEJ       TO ALLT-SW                                   
317700           ELSE                                                           
317800             IF ORAD-KDORDKL > 0                                          
317900             AND ORAD-IDSYSTEM NOT = 'OREL'                               
318000             AND (AREG-KDUART = 'L'                                       
318100             OR AREG-KDUART = 'P')                                        
318200             AND OHUV-FLORDSPE NOT = JA                                   
318300             AND OHUV-FLOVRLEV NOT = JA                                   
318400             AND OHUV-FLFORBI = NEJ                                       
318500               MOVE ZERO    TO SDCA-KDORDBEK                              
318600               MOVE 70      TO TPO2-KDORDBEK                              
318700               MOVE JA      TO OBKR-SW                                    
318800               MOVE NEJ     TO ALLT-SW                                    
318900               MOVE WC-CDC-SE TO ORAD-IDDC                                
319000               MOVE ORAD-IDDC TO WS-IDDC                                  
319100               MOVE 6       TO ORAD-KDTPOTYP                              
319200               IF ORAD-KDPRTYP NOT = 'P'                                  
319300                  MOVE ZERO  TO ORAD-PRARTNTO                             
319400                  MOVE SPACE TO ORAD-KDPRTYP                              
319500**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
319600                  IF NOT DIST79-DEALER-PRICE                              
319800                    MOVE ZERO        TO ORAD-PRARTNTO-LOC                 
319900                    MOVE NEJ         TO ORAD-FLPRTILL                     
320000                  END-IF                                                  
320100*************                                                             
320200               END-IF                                                     
320300             ELSE                                                         
320400               MOVE JA                TO OBKR-SW                          
320500               MOVE NEJ               TO ALLT-SW                          
320600               IF SDCA-KDORDBEK = 92                                      
320700                 MOVE SDCA-KVBEART-Q  TO ORAD-KVPRERO                     
320800               END-IF                                                     
320900             END-IF                                                       
321000           END-IF                                                         
321100         ELSE                                                             
321200           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
321300           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
321400           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
321500           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
321600           MOVE NEJ                  TO ALLT-SW                           
321700           IF  KVAN-KDORDBEK-UT  = ZERO                                   
321800           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
321900           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
322000             MOVE JA                 TO EGET-CL-RAD-SW                    
322100           END-IF                                                         
322200           IF KOLLA-ERS    OR                                             
322300             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
322400              MOVE NEJ               TO KOLLA-ERS-SW                      
322500              MOVE ZERO              TO KERS-KDORDBEK                     
322600              PERFORM S02-RENSA-TILLK-TAB                                 
322700              MOVE ZERO              TO SPAR-KDORDBEK                     
322800                                                                          
322900           END-IF                                                         
323000         END-IF                                                           
323100                                                                          
323200         MOVE SDCA-KDOI              TO ORAD-KDOI                         
323300         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
323400       END-IF                                                             
323500                                                                          
323600     END-IF                                                               
323700     .                                                                    
323800     EJECT                                                                
323900 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
324000                                                                          
324100     IF ALLT-OK                                                           
324200                                                                          
324300       MOVE ORAD-BERADREF      TO RANS-BERADREF                           
324400       MOVE OHUV-FLEMBORD      TO RANS-FLEMBORD                           
324500       MOVE OHUV-FLFORBI       TO RANS-FLFORBI                            
324600       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
324700       MOVE OHUV-FLOVRLEV      TO RANS-FLOVRLEV                           
324800       MOVE ORAD-IDKAMPRF      TO RANS-IDKAMPRF                           
324900       MOVE ORAD-IDARTNR       TO RANS-IDARTNR                            
325000       MOVE ORAD-IDLEVNR       TO RANS-IDLEVNR                            
325100       MOVE OHUV-IDRFTAB       TO RANS-IDRFTAB                            
325200       MOVE ORAD-TIRODAT       TO RANS-TIRODAT                            
325300       MOVE OHUV-KDORDKL       TO RANS-KDORDKL                            
325400       MOVE +1                 TO RANS-KDORDBEH                           
325500       MOVE ORAD-KVBEART-Q     TO RANS-KVBEART-Q                          
325600       MOVE ORAD-KDTPOTYP      TO RANS-KDTPOTYP                           
325700       MOVE AREG-KDERS         TO RANS-KDERS                              
325800       MOVE AREG-KVLS          TO RANS-KVLS                               
325900       MOVE AREG-KVPB-SATS     TO RANS-KVPB-SATS                          
326000       MOVE AREG-KVPB-SEP      TO RANS-KVPB-SEP                           
326100       MOVE AREG-REDIRLEV      TO RANS-REDIRLEV                           
326200       MOVE AREG-KVRESS        TO RANS-KVRESS                             
326300       MOVE AREG-KVSPANT       TO RANS-KVSPANT                            
326400       MOVE AREG-KVUTRS        TO RANS-KVUTRS                             
326500       MOVE AREG-TIDISPIN      TO RANS-TIDISPIN                           
326600                                                                          
326700       IF AREG-KDPRODSL = 71 OR 72 OR 73 OR 74                            
326800         MOVE 1                TO ORAD-RERF-RAD                           
326900                                  RANS-RERF-RAD-UT                        
327000         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
327100                                  RANS-SUTPO-EJPB-UT                      
327200                                  RANS-RERF-ART-UT                        
327300       ELSE                                                               
327400         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
327500                             RANS-ARTM-PCB RANS-ARTS-PCB                  
327600                                                                          
327700         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
327800       END-IF                                                             
327900     END-IF                                                               
328000     .                                                                    
328100     EJECT                                                                
328200 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
328300                                                                          
328400     IF ALLT-OK                                                           
328500                                                                          
328600     MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                           
328700     MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                            
328800     MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                        
328900     MOVE SPACE                TO STOR-KDPROTYP                           
329000     MOVE ORAD-BERADREF        TO STOR-BERADREF                           
329100     MOVE OHUV-FLFORBI         TO STOR-FLFORBI                            
329200     MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                           
329300     MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                           
329400     MOVE OHUV-KDORDKL         TO STOR-KDORDKL                            
329500     MOVE AREG-KDERS           TO STOR-KDERS                              
329600     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
329700     MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                          
329800     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
329900     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
330000     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
330100     MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                           
330200     MOVE ORAD-IDDISTR         TO STOR-IDDISTR                            
330300     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
330400                                                                          
330500     CALL W411STOR USING STOR-W411STOR                                    
330600                                                                          
330700     IF STOR-KDORDBEK > +0                                                
330800        MOVE +6                   TO ORAD-KDTPOTYP                        
330900        IF ORAD-KDPRTYP NOT = 'P'                                         
331000           MOVE +0                TO ORAD-PRARTNTO                        
331100           MOVE SPACE             TO ORAD-KDPRTYP                         
331200**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
331300           IF NOT DIST79-DEALER-PRICE                                     
331500             MOVE ZERO        TO ORAD-PRARTNTO-LOC                        
331600             MOVE NEJ         TO ORAD-FLPRTILL                            
331700           END-IF                                                         
331800*************                                                             
331900        END-IF                                                            
332000        MOVE JA                   TO OBKR-SW                              
332100        MOVE NEJ                  TO ALLT-SW                              
332200     END-IF                                                               
332300                                                                          
332400     END-IF                                                               
332500     .                                                                    
332600     EJECT                                                                
332700 ECR-PREL-AVBOKNING-CDC SECTION.                                          
332800                                                                          
332900     IF ALLT-OK                                                           
333000                                                                          
333100     MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                    
333200                               TO CDCA-FLFINLV-IN                         
333300     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
333400     MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                        
333500     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
333600     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
333700     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
333800     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
333900                               TO CDCA-FLSLATT-IN                         
334000     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
334100     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
334200     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
334300     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
334400     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
334500     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
334600     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
334700     MOVE AREG-KDERS           TO CDCA-KDERS-IN                           
334800     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
334900     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
335000     MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                         
335100     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
335200     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
335300     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
335400     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
335500     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
335600     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
335700     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
335800     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
335900     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
336000     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
336100     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
336200     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
336300     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
336400     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
336500     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
336600     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
336700     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
336800     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
336900     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
337000     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
337100     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
337200     MOVE +1                   TO CDCA-KDCALL                             
337300                                                                          
337400     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
337500                                       CDCA-INLB-PCB                      
337600                                       CDCA-WDB2-PCB                      
337700                                       CDCA-WDC1-PCB                      
337800     EJECT                                                                
337810     IF KERS-KDERS = 0                                                    
337820        CONTINUE                                                          
337830     ELSE                                                                 
337900        IF KERS-KDERS > 0 AND < 10                                        
338000           IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0              
338100              MOVE JA             TO OBKR-SW                              
338200           ELSE                                                           
338300              PERFORM S02-RENSA-TILLK-TAB                                 
338400              MOVE ZERO           TO KERS-KDORDBEK                        
338500              MOVE NEJ            TO TILLK-SW                             
338600           END-IF                                                         
338710        ELSE                                                              
338720******FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                     
338730           IF CDCA-KVPREAVB-UT > 0                                        
338740             IF KOLLA-ERS      OR                                         
338750                (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)            
338760                 MOVE NEJ          TO KOLLA-ERS-SW                        
338770                 MOVE ZERO         TO KERS-KDORDBEK                       
338780                 PERFORM S02-RENSA-TILLK-TAB                              
338790                 MOVE ZERO         TO SPAR-KDORDBEK                       
338791             ELSE                                                         
338792                 IF KERS-KDERS = +19 OR +29                               
338793                    MOVE ZERO      TO SPAR-KDORDBEK                       
338794                 END-IF                                                   
338795             END-IF                                                       
338796           ELSE                                                           
338798*             IF (CDCA-KVPREAVB-UT <= 0) AND                              
338799*                (CDCA-KDORDBEK-UT = 92 OR 99)                            
338800*                  MOVE ZEROES TO CDCA-KDORDBEK-UT                        
338801*             END-IF                                                      
338802              IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                     
338803              OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)             
338804              OR SPAR-KDORDBEK = 54                                       
338805                  PERFORM S07-SPACE-SDCA-KDORDBEK                         
338806              END-IF                                                      
338807           END-IF                                                         
338808        END-IF                                                            
338809     END-IF                                                               
338810                                                                          
338820                                                                          
338900     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
339000                                                                          
339100     IF ORAD-IDLEVNR NOT = SPACE                                          
339200        CONTINUE                                                          
339300     ELSE                                                                 
339400        MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                         
339500        MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                       
339600     END-IF                                                               
339700     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
339800     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
339900     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
340000     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
340100                                                                          
340200     IF CDCA-KDORDBEK-UT > +0                                             
340300                                                                          
340400        MOVE JA                TO OBKR-SW                                 
340500     END-IF                                                               
340600     EJECT                                                                
340700     IF KVAN-KDORDBEK-UT > +0                                             
340800        IF CDCA-KVBEART-UT = CDCA-KVBEART-Q-UT                            
340900           MOVE +0             TO KVAN-KDORDBEK-UT                        
341000        END-IF                                                            
341100     END-IF                                                               
341200     IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND               
341300         CDCA-KDORDBEK-UT  = +0 AND                                       
341400         KVAN-KDORDBEK-UT  = +0 AND                                       
341500         DLEV-KDORDBEK-UT  = +0 AND                                       
341600         KERS-KDORDBEK     = +0 AND                                       
341700         TPO1-KDORDBEK     = +0 AND                                       
341800         TPO2-KDORDBEK     = +0 AND                                       
341900         RELS-KDORDBEK     = +0 AND                                       
342000         KAMP-KDORDBEK     = +0 AND                                       
342100         STOR-KDORDBEK     = +0 AND                                       
342200         SDCA-KDORDBEK     = +0 AND                                       
342300         SDCA-KDORDBEK-FIRST-SDC = +0 AND                                 
342400         SDCA-KDORDBEK-SECOND-SDC = +0 AND                                
342500         SPAR-KDORDBEK     = +0                                           
342600         MOVE JA                  TO EGET-CL-RAD-SW                       
342700     END-IF                                                               
342800     MOVE AREG-ADLAGOMR  TO ORAD-ADLAGOMR                                 
342900     MOVE AREG-ADGANG    TO ORAD-ADGANG                                   
343000     MOVE AREG-ADPLATS   TO ORAD-ADPLATS                                  
343100                                                                          
343200     END-IF                                                               
343300     .                                                                    
343400     EJECT                                                                
343500 ECS-SKRIV-OBKR SECTION.                                                  
343600                                                                          
343700*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
343800*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT CLAGER 'SLÄPAR'              
343900*    ISRT AV RADEN.                                                       
344000*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
344100*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
344200*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
344300*---                                                                      
344400     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
344500                                                                          
344600     IF TILLKOMMANDE-RAD                                                  
344700        IF KERS-KDORDBEK = 41                                             
344800           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
344900           MOVE '4222KER1'        TO OBKR-IDPGM                           
345000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
345100                                  TO OBKR-KVBEART-TILLK                   
345200           COMPUTE OBKR-DIERS-KVOT =                                      
345300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
345400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
345500           MOVE 'S'               TO OBKR-SW                              
345600        END-IF                                                            
345700     END-IF                                                               
345800                                                                          
345900     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
346000       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
346100*----(KOD 56,58, 59, 98)                                                  
346200        IF OBKR-SKRIVEN                                                   
346300           PERFORM IMS-08-ISRT-WDQ101                                     
346400           ADD +1              TO OBKR-IDSEKVNR                           
346500        END-IF                                                            
346600        MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                  
346700                               TO OBKR-KDORDBEK                           
346800        MOVE '4222ORFK'        TO OBKR-IDPGM                              
346900        MOVE 'S'               TO OBKR-SW                                 
347000     END-IF                                                               
347100     EJECT                                                                
347200                                                                          
347300     IF KVAN-KDORDBEK-UT > +0                                             
347400*----(KOD 43, 44)                                                         
347500        IF OBKR-SKRIVEN                                                   
347600           PERFORM IMS-08-ISRT-WDQ101                                     
347700           ADD +1              TO OBKR-IDSEKVNR                           
347800        END-IF                                                            
347900        IF TILLKOMMANDE-RAD                                               
348000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
348100                               TO OBKR-KVBEART-TILLK                      
348200           COMPUTE OBKR-DIERS-KVOT =                                      
348300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
348400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
348500        END-IF                                                            
348600        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
348700        MOVE '4222KVAN'        TO OBKR-IDPGM                              
348800        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
348900        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
349000        MOVE 'S'               TO OBKR-SW                                 
349100     END-IF                                                               
349200     EJECT                                                                
349300                                                                          
349400     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
349500*----(KOD 21, 53, 82, 95) , 26                                            
349600        IF OBKR-SKRIVEN                                                   
349700           PERFORM IMS-08-ISRT-WDQ101                                     
349800           ADD +1              TO OBKR-IDSEKVNR                           
349900        END-IF                                                            
350000        IF TILLKOMMANDE-RAD                                               
350100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
350200                               TO OBKR-KVBEART-TILLK                      
350300           COMPUTE OBKR-DIERS-KVOT =                                      
350400                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
350500                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
350600        END-IF                                                            
350700        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
350800        MOVE '4222DLEV'        TO OBKR-IDPGM                              
350900        MOVE 'S'               TO OBKR-SW                                 
351000     END-IF                                                               
351100     EJECT                                                                
351200     IF KERS-KDORDBEK > +0                                                
351300*----(KOD 41, 61)                                                         
351400                                                                          
351500        IF KERS-KDORDBEK = 61                                             
351600*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
351700*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
351800*----RADBEHANDLINGEN                                                      
351900           IF OBKR-SKRIVEN                                                
352000              PERFORM IMS-08-ISRT-WDQ101                                  
352100              ADD +1           TO OBKR-IDSEKVNR                           
352200           END-IF                                                         
352300           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
352400           MOVE '4222KER2'     TO OBKR-IDPGM                              
352500           MOVE 'S'            TO OBKR-SW                                 
352600           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
352700           PERFORM S02-RENSA-TILLK-TAB                                    
352800        ELSE                                                              
352900           IF NOT TILLKOMMANDE-RAD                                        
353000              IF OBKR-SKRIVEN                                             
353100                 PERFORM IMS-08-ISRT-WDQ101                               
353200                 ADD +1        TO OBKR-IDSEKVNR                           
353300              END-IF                                                      
353400              MOVE 'S'            TO OBKR-SW                              
353500              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
353600              MOVE '4222KER3'  TO OBKR-IDPGM                              
353700           END-IF                                                         
353800        END-IF                                                            
353900     END-IF                                                               
354000     EJECT                                                                
354100     IF SPAR-KDORDBEK        > ZERO                                       
354200*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90)                     
354300        IF OBKR-SKRIVEN                                                   
354400           PERFORM IMS-08-ISRT-WDQ101                                     
354500           ADD +1              TO OBKR-IDSEKVNR                           
354600        END-IF                                                            
354700        IF TILLKOMMANDE-RAD                                               
354800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
354900                               TO OBKR-KVBEART-TILLK                      
355000           COMPUTE OBKR-DIERS-KVOT =                                      
355100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
355200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
355300        END-IF                                                            
355400        MOVE SPAR-KDORDBEK                                                
355500                               TO OBKR-KDORDBEK                           
355600        MOVE '4222SPAR'        TO OBKR-IDPGM                              
355700        MOVE 'S'               TO OBKR-SW                                 
355800     END-IF                                                               
355900     EJECT                                                                
356000                                                                          
356100     IF TPO1-KDORDBEK > +0                                                
356200*----(KOD 72, 73, 74, 85)                                                 
356300        IF OBKR-SKRIVEN                                                   
356400           PERFORM IMS-08-ISRT-WDQ101                                     
356500           ADD +1              TO OBKR-IDSEKVNR                           
356600        END-IF                                                            
356700        IF TILLKOMMANDE-RAD                                               
356800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
356900                               TO OBKR-KVBEART-TILLK                      
357000           COMPUTE OBKR-DIERS-KVOT =                                      
357100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
357200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
357300        END-IF                                                            
357400        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
357500        MOVE '4222TPO1'        TO OBKR-IDPGM                              
357600        IF TPO1-KDORDBEK = 85                                             
357700           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
357800        END-IF                                                            
357900        MOVE 'S'               TO OBKR-SW                                 
358000     END-IF                                                               
358100     EJECT                                                                
358200     IF TPO2-KDORDBEK > +0                                                
358300*----(KOD 70)                                                             
358400        IF OBKR-SKRIVEN                                                   
358500           PERFORM IMS-08-ISRT-WDQ101                                     
358600           ADD +1              TO OBKR-IDSEKVNR                           
358700        END-IF                                                            
358800        IF TILLKOMMANDE-RAD                                               
358900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
359000                               TO OBKR-KVBEART-TILLK                      
359100           COMPUTE OBKR-DIERS-KVOT =                                      
359200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
359300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
359400        END-IF                                                            
359500        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
359600        MOVE '4222TPO2'        TO OBKR-IDPGM                              
359700        MOVE 'S'               TO OBKR-SW                                 
359800     END-IF                                                               
359900     EJECT                                                                
360000                                                                          
360100     IF KAMP-KDORDBEK > +0                                                
360200*----(KOD 72, 75, 76)                                                     
360300        IF OBKR-SKRIVEN                                                   
360400           PERFORM IMS-08-ISRT-WDQ101                                     
360500           ADD +1              TO OBKR-IDSEKVNR                           
360600        END-IF                                                            
360700        IF TILLKOMMANDE-RAD                                               
360800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
360900                               TO OBKR-KVBEART-TILLK                      
361000           COMPUTE OBKR-DIERS-KVOT =                                      
361100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
361200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
361300        END-IF                                                            
361400        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
361500        MOVE '4222KAMP'        TO OBKR-IDPGM                              
361600        MOVE 'S'               TO OBKR-SW                                 
361700     END-IF                                                               
361800     EJECT                                                                
361900                                                                          
362000     IF RELS-KDORDBEK > 0                                                 
362100*----(KOD 56)                                                             
362200          IF OBKR-SKRIVEN                                                 
362300             PERFORM IMS-08-ISRT-WDQ101                                   
362400             ADD +1              TO OBKR-IDSEKVNR                         
362500          END-IF                                                          
362600          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
362700          MOVE '4222ORFK'        TO OBKR-IDPGM                            
362800          MOVE 'S'               TO OBKR-SW                               
362900     END-IF                                                               
363000    EJECT                                                                 
363100                                                                          
363200     IF XDCA-KDORDBEK > ZERO                                              
363300*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
363400        IF OBKR-SKRIVEN                                                   
363500           PERFORM IMS-08-ISRT-WDQ101                                     
363600           ADD +1              TO OBKR-IDSEKVNR                           
363700        END-IF                                                            
363800        IF TILLKOMMANDE-RAD                                               
363900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
364000                               TO OBKR-KVBEART-TILLK                      
364100           COMPUTE OBKR-DIERS-KVOT =                                      
364200                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
364300                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
364400        END-IF                                                            
364500                                                                          
364600        IF XDCA-KDORDBEK NOT = 15                                         
364700          IF OHUV-IDDC-TVS = SPACE                                        
364800            IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                     
364900              MOVE 15          TO OBKR-KDORDBEK                           
365000              MOVE IDPGM       TO OBKR-IDPGM                              
365100              MOVE 'S'         TO OBKR-SW                                 
365200                                                                          
365300* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
365400              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
365500                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
365600              END-IF                                                      
365700              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
365800                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
365900              END-IF                                                      
366000              IF SDCA-KDORDBEK = 15                                       
366100                 MOVE ZERO     TO SDCA-KDORDBEK                           
366200              END-IF                                                      
366300            END-IF                                                        
366400            IF OBKR-SKRIVEN                                               
366500              PERFORM IMS-08-ISRT-WDQ101                                  
366600              ADD +1           TO OBKR-IDSEKVNR                           
366700            END-IF                                                        
366800          END-IF                                                          
366900        END-IF                                                            
367000        IF XDCA-KDORDBEK = 80                                             
367100           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
367200        END-IF                                                            
367300        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
367400        MOVE '4222XDCA'        TO OBKR-IDPGM                              
367500        MOVE 'S'               TO OBKR-SW                                 
367600     END-IF                                                               
367700     EJECT                                                                
367800     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
367900*----(KOD 15, 53, 80, 92)                                                 
368000        IF OBKR-SKRIVEN                                                   
368100           PERFORM IMS-08-ISRT-WDQ101                                     
368200           ADD +1              TO OBKR-IDSEKVNR                           
368300        END-IF                                                            
368400        IF TILLKOMMANDE-RAD                                               
368500           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
368600                               TO OBKR-KVBEART-TILLK                      
368700           COMPUTE OBKR-DIERS-KVOT =                                      
368800                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
368900                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
369000        END-IF                                                            
369100        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
369200           MOVE ORAD-KVBEART   TO OBKR-KVANNANT                           
369300        END-IF                                                            
369400        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
369500        MOVE '4222SDCA'        TO OBKR-IDPGM                              
369600        MOVE 'S'               TO OBKR-SW                                 
369700     END-IF                                                               
369800     EJECT                                                                
369900     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
370000*----(KOD 15, 53, 80, 92)                                                 
370100        IF OBKR-SKRIVEN                                                   
370200           PERFORM IMS-08-ISRT-WDQ101                                     
370300           ADD +1              TO OBKR-IDSEKVNR                           
370400        END-IF                                                            
370500        IF TILLKOMMANDE-RAD                                               
370600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
370700                               TO OBKR-KVBEART-TILLK                      
370800           COMPUTE OBKR-DIERS-KVOT =                                      
370900                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
371000                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
371100        END-IF                                                            
371200        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
371300           MOVE ORAD-KVBEART   TO OBKR-KVANNANT                           
371400        END-IF                                                            
371500        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
371600        MOVE '4222SDCA'        TO OBKR-IDPGM                              
371700        MOVE 'S'               TO OBKR-SW                                 
371800     END-IF                                                               
371900     EJECT                                                                
372000     IF SDCA-KDORDBEK > ZERO                                              
372100*----(KOD 15, 53, 80, 92)                                                 
372200        IF OBKR-SKRIVEN                                                   
372300           PERFORM IMS-08-ISRT-WDQ101                                     
372400           ADD +1              TO OBKR-IDSEKVNR                           
372500        END-IF                                                            
372600        IF TILLKOMMANDE-RAD                                               
372700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
372800                               TO OBKR-KVBEART-TILLK                      
372900           COMPUTE OBKR-DIERS-KVOT =                                      
373000                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
373100                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
373200        END-IF                                                            
373300        IF SDCA-KDORDBEK = 80                                             
373400           MOVE ORAD-KVBEART   TO OBKR-KVANNANT                           
373500        END-IF                                                            
373600        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
373700        MOVE '4222SDCA'        TO OBKR-IDPGM                              
373800        MOVE 'S'               TO OBKR-SW                                 
373900     END-IF                                                               
374000     EJECT                                                                
374100     IF STOR-KDORDBEK > +0                                                
374200*----(KOD 70)                                                             
374300        IF OBKR-SKRIVEN                                                   
374400           PERFORM IMS-08-ISRT-WDQ101                                     
374500           ADD +1              TO OBKR-IDSEKVNR                           
374600        END-IF                                                            
374700        IF TILLKOMMANDE-RAD                                               
374800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
374900                               TO OBKR-KVBEART-TILLK                      
375000           COMPUTE OBKR-DIERS-KVOT =                                      
375100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
375200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
375300        END-IF                                                            
375400        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
375500        MOVE '4222STOR'        TO OBKR-IDPGM                              
375600        MOVE 'S'               TO OBKR-SW                                 
375700     END-IF                                                               
375800     EJECT                                                                
375900     IF CDCA-KDORDBEK-UT > +0                                             
376000*----(KOD 80, 92, 99)                                                     
376100        IF OBKR-SKRIVEN                                                   
376200           PERFORM IMS-08-ISRT-WDQ101                                     
376300           ADD +1              TO OBKR-IDSEKVNR                           
376400        END-IF                                                            
376500        IF TILLKOMMANDE-RAD                                               
376600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
376700                               TO OBKR-KVBEART-TILLK                      
376800           COMPUTE OBKR-DIERS-KVOT =                                      
376900                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
377000                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
377100        END-IF                                                            
377200        IF CDCA-KDORDBEK-UT = +80                                         
377300           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
377400        END-IF                                                            
377500        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
377600        MOVE '4222CDCA'        TO OBKR-IDPGM                              
377700        MOVE 'S'               TO OBKR-SW                                 
377800     END-IF                                                               
377900*                                                                         
378000     IF WS-VOR-KDORDBEK = 26                                              
378100        IF OBKR-SKRIVEN                                                   
378200           PERFORM IMS-08-ISRT-WDQ101                                     
378300           ADD +1              TO OBKR-IDSEKVNR                           
378400        END-IF                                                            
378500        IF TILLKOMMANDE-RAD                                               
378600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
378700                               TO OBKR-KVBEART-TILLK                      
378800           COMPUTE OBKR-DIERS-KVOT =                                      
378900                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
379000                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
379100        END-IF                                                            
379200        MOVE 26                TO OBKR-KDORDBEK                           
379300        MOVE '4222VOR '        TO OBKR-IDPGM                              
379400        MOVE 'S'               TO OBKR-SW                                 
379500     END-IF                                                               
379600*                                                                         
379700* PÅ SISTA RADEN FÖR KUNDENS NORMALA CLAGER LÄGGS DE AVBOKADE             
379800* ANTALEN!                                                                
379900*                                                                         
380000     IF OBKR-SKRIVEN                                                      
380100        MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                           
380200        MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                            
380300******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
380400         IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                    
380500           PERFORM S06-DELETE-PRICE-Q-LINE                                
380600           INITIALIZE OBKR-DEAL-PR-LINE                                   
380700           MOVE 'N/A'          TO OBKR-KDVALISO                           
380800         END-IF                                                           
380900*************TL 030514                                                    
381000        PERFORM IMS-08-ISRT-WDQ101                                        
381100        ADD +1                 TO OBKR-IDSEKVNR                           
381200     END-IF                                                               
381300     .                                                                    
381400     EJECT                                                                
381500 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
381600                                                                          
381700     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
381800     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
381900                               TO OBKR-IDARTNR                            
382000     IF NOT TILLKOMMANDE-RAD                                              
382100        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
382200                                  W-IDORDER-Q1-MAX                        
382300        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
382400                                  W-IDARTNR-Q1-MAX                        
382500        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
382600                                  W-IDLOPNR-Q1-MAX                        
382700                                  W-IDSEKVNR-Q1-MIN                       
382800                                  W-IDSEKVNR-Q1-MAX                       
382900        PERFORM IMS-07-GU-WDQ1-WDQ101                                     
383000        PERFORM UNTIL SEGMENT-SAKNAS                                      
383100           ADD +1              TO W-IDLOPNR-Q1-MIN                        
383200                                  W-IDLOPNR-Q1-MAX                        
383300           PERFORM IMS-07-GU-WDQ1-WDQ101                                  
383400        END-PERFORM                                                       
383500        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
383600        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
383700     END-IF                                                               
383800     IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                                
383900        MOVE OHUV-IDDC-TVS     TO OBKR-IDDC                               
384000                                  ORAD-IDDC                               
384100     ELSE                                                                 
384200        MOVE ORAD-IDDC         TO OBKR-IDDC                               
384300     END-IF                                                               
384400     MOVE +0                   TO OBKR-KDORDBEK                           
384500     MOVE SPACE                TO OBKR-BEERS                              
384600     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
384700     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
384800     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
384900     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
385000     MOVE +0                   TO OBKR-DIERS-KVOT                         
385100     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
385200     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
385300     MOVE NEJ                  TO OBKR-FLOBOK                             
385400     EJECT                                                                
385500     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
385600     MOVE NEJ                  TO OBKR-FLOBPRT                            
385700     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
385800     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
385900     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
386000                               TO OBKR-FLSLATT                            
386100     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
386200     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
386300        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
386400     ELSE                                                                 
386500        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
386600                               TO OBKR-REKSIFFR                           
386700     END-IF                                                               
386800     IF TILLKOMMANDE-RAD                                                  
386900        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
387000                               TO OBKR-IDARTNR-TILLK                      
387100        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
387200                               TO OBKR-REKSIFFR-TILLK                     
387300     ELSE                                                                 
387400        MOVE +0                TO OBKR-IDARTNR-TILLK                      
387500        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
387600     END-IF                                                               
387700     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
387800     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
387900     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
388000     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
388100     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
388200     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
388300     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
388400     IF NOT TILLKOMMANDE-RAD                                              
388500        MOVE AREG-KDERS        TO OBKR-KDERS                              
388600     END-IF                                                               
388700     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
388800     MOVE ORAD-KDOI            TO OBKR-KDOI                               
388900     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
389000     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
389100     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
389200     EJECT                                                                
389300     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
389400     MOVE +0                   TO OBKR-KVANNANT                           
389500     MOVE +0                   TO OBKR-KVAVBART                           
389600     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
389700     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
389800     MOVE +0                   TO OBKR-KVBEART-TILLK                      
389900     MOVE +0                   TO OBKR-KVPREAVB                           
390000     MOVE +0                   TO OBKR-KVPRERO                            
390100     IF ALLT-OK                                                           
390200        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
390300     ELSE                                                                 
390400        MOVE +0                TO OBKR-KVQPACK                            
390500     END-IF                                                               
390600     MOVE +0                   TO OBKR-KVRO                               
390700     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
390800     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
390900     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
391000     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
391100     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
391200     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
391300     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
391400     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
391500     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
391600     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
391700     MOVE +0                   TO OBKR-TIRODAT                            
391800     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
391900     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
392000       MOVE 20                 TO WS-SEKEL-9KOMPL                         
392100     ELSE                                                                 
392200       MOVE 19                 TO WS-SEKEL-9KOMPL                         
392300     END-IF                                                               
392400     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
392500     MOVE ORAD-TITPO           TO OBKR-TITPO                              
392600     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
392700     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
392800     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
392900     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
393000     MOVE SPACE                TO OBKR-IDBIL                              
393100                                                                          
393200     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
393300     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
393400     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
393500     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
393600     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
393700     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
393800*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
393900     IF OBKR-KDVALISO = SPACE                                             
394000        MOVE 'N/A'             TO OBKR-KDVALISO                           
394100     END-IF                                                               
394200     .                                                                    
394300                                                                          
394400 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
394500                                                                          
394600     MOVE +1                   TO WS-INDEX-TILLK                          
394700     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
394800                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
394900        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
395000           IF OBKR-SKRIVEN                                                
395100              PERFORM IMS-08-ISRT-WDQ101                                  
395200              ADD +1           TO OBKR-IDSEKVNR                           
395300           END-IF                                                         
395400           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
395500           MOVE '4222KER4'     TO OBKR-IDPGM                              
395600           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
395700                               TO OBKR-IDARTNR-TILLK                      
395800           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
395900                               TO OBKR-REKSIFFR-TILLK                     
396000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
396100                               TO OBKR-KVBEART-TILLK                      
396200           COMPUTE OBKR-DIERS-KVOT =                                      
396300                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
396400                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
396500           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
396600                               TO OBKR-BEERS                              
396700                                                                          
396800           MOVE 'S'            TO OBKR-SW                                 
396900        END-IF                                                            
397000        ADD +1                 TO WS-INDEX-TILLK                          
397100     END-PERFORM                                                          
397200     IF WS-INDEX-TILLK = +1                                               
397300        MOVE +0                TO OBKR-KDERS                              
397400     END-IF                                                               
397500     .                                                                    
397600     EJECT                                                                
397700 ECU-KONTROLLERA-ENHETSLAST SECTION.                                      
397800                                                                          
397900     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
398000     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
398100     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
398200     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
398300     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
398400     MOVE ORAD-IDDC            TO LAST-IDDC                               
398500     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
398600     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
398700     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
398800     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
398900                                                                          
399000     CALL W411LAST USING LAST-W411LAST                                    
399100     .                                                                    
399200     EJECT                                                                
399300 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
399400     IF LAST-ADLAGOMR-UT = +0 AND                                         
399500        LAST-KVANTAL-UT  = +0 AND                                         
399600        LAST-KVBEART-UT  = +0                                             
399700*------------------------------------------------------------*            
399800*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
399900*------------------------------------------------------------*            
400000        PERFORM ECVA-FIXA-LAGEROMR-PLATS                                  
400100        PERFORM ECVB-REDIGERA-WOPS-AREA                                   
400200        PERFORM IMS-09-ISRT-WDQ4-WDQ401                                   
400300        PERFORM UNTIL SEGMENT-FINNS                                       
400400           ADD +1                    TO ORAD-IDLOPNR                      
400500           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
400600        END-PERFORM                                                       
400700     ELSE                                                                 
400800*------------------------------------------------------------*            
400900*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
401000*------------------------------------------------------------*            
401100                                                                          
401200        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
401300*------------------------------------------------------------*            
401400*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
401500*------------------------------------------------------------*            
401600                                                                          
401700           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
401800           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
401900                                    ORAD-KVPRERO                          
402000           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
402100           PERFORM ECVC-BERAEKNA-KVSLATT                                  
402200           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
402300           PERFORM ECVB-REDIGERA-WOPS-AREA                                
402400           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
402500           PERFORM UNTIL SEGMENT-FINNS                                    
402600              ADD +1                 TO ORAD-IDLOPNR                      
402700              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
402800           END-PERFORM                                                    
402900*                                                                         
403000     EJECT                                                                
403100*------------------------------------------------------------*            
403200*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
403300*------------------------------------------------------------*            
403400                                                                          
403500           MOVE +0                   TO ORAD-KVBEART                      
403600           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
403700                                        ORAD-KVPREAVB                     
403800           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
403900           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
404000             CONTINUE                                                     
404100           ELSE                                                           
404200             IF LAST-ADGANG-UT > ZERO                                     
404300               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
404400             END-IF                                                       
404500           END-IF                                                         
404600           MOVE +0                   TO ORAD-KVPRERO                      
404700           MOVE 1.0000               TO ORAD-RERF-RAD                     
404800           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
404900           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
405000           PERFORM ECVB-REDIGERA-WOPS-AREA                                
405100           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
405200           PERFORM UNTIL SEGMENT-FINNS                                    
405300              ADD +1                 TO ORAD-IDLOPNR                      
405400              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
405500           END-PERFORM                                                    
405600        ELSE                                                              
405700*------------------------------------------------------------*            
405800*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
405900*------------------------------------------------------------*            
406000                                                                          
406100           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
406200           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
406300             CONTINUE                                                     
406400           ELSE                                                           
406500             IF LAST-ADGANG-UT > ZERO                                     
406600               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
406700             END-IF                                                       
406800           END-IF                                                         
406900           MOVE 1.0000            TO ORAD-RERF-RAD                        
407000           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
407100           PERFORM ECVB-REDIGERA-WOPS-AREA                                
407200           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
407300           PERFORM UNTIL SEGMENT-FINNS                                    
407400              ADD +1              TO ORAD-IDLOPNR                         
407500              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
407600           END-PERFORM                                                    
407700        END-IF                                                            
407800     END-IF                                                               
407900     .                                                                    
408000     EJECT                                                                
408100 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
408200                                                                          
408300     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
408400     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
408500                                                                          
408600     MOVE WS-HFAK-REF-X10      TO ADRS-BEVARREF-IN                        
408700     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
408800     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
408900     MOVE 1                    TO ADRS-KDCALL-IN                          
409000     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
409100     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
409200     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
409300     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
409400                                                                          
409500     CALL W413ADRS USING ADRS-W413ADRS                                    
409600                                                                          
409700*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
409800     IF OHUV-IDKAMPRF > 0                                                 
409900       MOVE 8                  TO ORAD-ADLAGOMR                           
410000     ELSE                                                                 
410100       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
410200     END-IF                                                               
410300                                                                          
410400*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
410500                                                                          
410600     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
410700     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
410800                                                                          
410900     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
411000     .                                                                    
411100     EJECT                                                                
411200 S01-KOLLA-I-HFAK-TAB SECTION.                                            
411300                                                                          
411400     IF WS-HFAK-REF-X10 NOT = SPACE                                       
411500        MOVE 1 TO HFAK-TAB-IX                                             
411600        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
411700           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
411800           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
411900           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
412000           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
412100              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
412200              MOVE 99 TO HFAK-TAB-IX                                      
412300           END-IF                                                         
412400           ADD 1 TO HFAK-TAB-IX                                           
412500        END-PERFORM                                                       
412600     END-IF                                                               
412700     .                                                                    
412800     EJECT                                                                
412900 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
413000                                                                          
413100     MOVE +1                   TO AVSR-KDCALL                             
413200     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
413300     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
413400     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
413500     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
413600     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
413700     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
413800                                                                          
413900     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
414000     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
414100     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
414200     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
414300     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
414400     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
414500     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
414600     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
414700     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
414800     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
414900     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
415000     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
415100     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
415200                                                                          
415300     ADD +1                    TO WS-INDEX-WOPS                           
415400     .                                                                    
415500     EJECT                                                                
415600 ECVC-BERAEKNA-KVSLATT SECTION.                                           
415700                                                                          
415800     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
415900                                                                          
416000        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
416100                                                                          
416200        COMPUTE ORAD-KVSLATT ROUNDED =                                    
416300               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
416400     END-IF                                                               
416500     .                                                                    
416600     EJECT                                                                
416610 S07-SPACE-SDCA-KDORDBEK  SECTION.                                        
416620                                                                          
416630     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
416640         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
416650     ELSE                                                                 
416660        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
416670           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
416680        ELSE                                                              
416690           IF SDCA-KDORDBEK > 0                                           
416691              MOVE ZEROES  TO SDCA-KDORDBEK                               
416692           END-IF                                                         
416693        END-IF                                                            
416694     END-IF                                                               
416695     .                                                                    
416696                                                                          
416697     EJECT                                                                
416700 ED-LAES-TILLK-DATA SECTION.                                              
416800                                                                          
416900     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
417000                               TO AREG-IDARTNR                            
417100                                                                          
417200     CALL W411AREG USING AREG-W411AREG                                    
417300                         AREG-WDK6-PCB                                    
417400                         AREG-WDK7-PCB                                    
417500     .                                                                    
417600     EJECT                                                                
417700 F-HOPPA-TILL-SVARSBILD SECTION.                                          
417800*                                                                         
417900* TO FORCE 4223 WITH 'V' TRANS WHEN RUN FROM BMP W41218                   
418000     IF W-IDTRANS = 'V412'                                                
418100        MOVE 'V412'              TO 4223-IDTRANS                          
418200        MOVE 'V'                 TO MID-KDTRTYP                           
418300        MOVE ALL '+'             TO 4223-KDORDKL-UT                       
418400                                    4223-FLANNULL                         
418500                                    4223-IDDC-NEXT                        
418600        MOVE ZEROS               TO 4223-IDARTNR-NEXT                     
418700                                    4223-IDLOPNR-NEXT                     
418800                                    4223-IDSEKVNR-NEXT                    
418900                                    4223-KDORDBEK-NEXT                    
419000        MOVE +1 TO WS-INDEX-MID                                           
419100        PERFORM UNTIL WS-INDEX-MID >= WS-INDEX-MID-MAX                    
419200          MOVE ZEROS             TO 4223-KDORDBEK(WS-INDEX-MID)           
419300          MOVE ALL '+'           TO 4223-KDBEHX(WS-INDEX-MID)             
419400          ADD  +1 TO WS-INDEX-MID                                         
419500        END-PERFORM                                                       
419600     END-IF                                                               
419700*                                                                         
419800     MOVE MFS-KDMFSFOR           TO 4223-SPRAK                            
419900     IF MID-KDTRTYP = 'V'                                                 
420000        MOVE 'W4T223V '          TO 4223-TRANSKOD                         
420100        MOVE ALL '+'             TO 4223-IDDISTR-IN                       
420200                                    4223-IDKUNDNR-IN                      
420300                                    4223-IDORDNR-IN                       
420400        MOVE WS-IDDISTR          TO 4223-IDDISTR-UT                       
420500        MOVE WS-IDKUNDNR         TO 4223-IDKUNDNR-UT                      
420600        MOVE WS-IDORDNR          TO 4223-IDORDNR-UT                       
420700        PERFORM IMS-INSERT-4223V-MSG                                      
420800     ELSE                                                                 
420900        MOVE WS-IDDISTR          TO 4223-IDDISTR-IN                       
421000        MOVE WS-IDKUNDNR         TO 4223-IDKUNDNR-IN                      
421100        MOVE WS-IDORDNR          TO 4223-IDORDNR-IN                       
421200        MOVE MFS-RENSA-FAELT     TO 4223-IDDISTR-UT                       
421300                                    4223-IDKUNDNR-UT                      
421400                                    4223-IDORDNR-UT                       
421500        PERFORM IMS-INSERT-4223-MSG                                       
421600     END-IF                                                               
421700                                                                          
421800                                                                          
421900     MOVE JA                     TO HOPP                                  
422000     .                                                                    
422100     EJECT                                                                
422200 G-VISA-TOM-SIDA SECTION.                                                 
422300                                                                          
422400     MOVE +1 TO WS-INDEX                                                  
422500     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
422600       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-006(WS-INDEX)                
422700                                 MOD-KVBEART(WS-INDEX)                    
422800                                 MOD-PRARTNTO(WS-INDEX)                   
422900                                 MOD-FLINVEST(WS-INDEX)                   
423000                                 MOD-KDVRINFO(WS-INDEX)                   
423100                                 MOD-BERADREF(WS-INDEX)                   
423200       ADD  +1 TO WS-INDEX                                                
423300     END-PERFORM                                                          
423400                                                                          
423500     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
423600     .                                                                    
423700     EJECT                                                                
423800 H-STARTA-BIPACKNINGEN SECTION.                                           
423900                                                                          
424000     COMPUTE 4297-LL = LENGTH OF 4297-MID-W4I29701 + 17                   
424100     MOVE MFS-KDMFSFOR           TO 4297-SPRAK                            
424200                                                                          
424300     MOVE W-IDDISTR              TO 4297-MID-IDDISTR                      
424400     MOVE W-IDKUNDNR             TO 4297-MID-IDKUNDNR                     
424500     MOVE W-IDKUNDRF             TO 4297-MID-IDKUNDRF                     
424600                                                                          
424700     MOVE OHUV-KDTPOTYP          TO 4297-MID-KDTPOTYP                     
424800     MOVE OHUV-KDORDKL           TO 4297-MID-KDORDKL                      
424900     MOVE OHUV-KDFAKTYP          TO 4297-MID-KDFAKTYP                     
425000     MOVE OHUV-IDKAMPRF          TO 4297-MID-IDKAMPRF                     
425100     MOVE OHUV-IDKONTO           TO 4297-MID-IDKONTO                      
425200     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
425300     MOVE OHUV-IDKST             TO 4297-MID-IDKST                        
425400     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
425500     MOVE OHUV-FLFORBI           TO 4297-MID-FLFORBI                      
425600     MOVE OHUV-IDORDER           TO 4297-MID-IDORDER                      
425700     MOVE OHUV-BEKUNDRF          TO 4297-MID-BEKUNDRF                     
425800     MOVE OHUV-TIREGDAT          TO 4297-MID-TIREGDAT                     
425900     MOVE OHUV-IDFTG             TO 4297-MID-IDFTG                        
426000     MOVE OHUV-BEVARREF          TO 4297-MID-BEVARREF                     
426100     MOVE OHUV-IDBIPREF          TO 4297-MID-IDBIPREF                     
426200     MOVE ARB-KDROPACK           TO 4297-MID-KDROPACK                     
426300     MOVE ARB-KDFRAKT            TO 4297-MID-KDFRAKT                      
426400     IF OHUV-IDDC-TVS NOT = SPACE                                         
426500       MOVE OHUV-IDDC-TVS        TO 4297-MID-IDDC                         
426600     ELSE                                                                 
426700       MOVE SPACE                TO 4297-MID-IDDC                         
426800     END-IF                                                               
426900                                                                          
427000     PERFORM IMS-INSERT-4297-MSG                                          
427100     MOVE JA                     TO HOPP                                  
427200     .                                                                    
427300     EJECT                                                                
427400 I-LAES-IN-4541-WDR4 SECTION.                                             
427500     MOVE LOW-VALUE                TO W-4542KEY-MIN-X                     
427600     MOVE HIGH-VALUE               TO W-4542KEY-MAX-X                     
427700     MOVE W-IDDISTR                TO W-IDDISTR-4542-MIN                  
427800                                      W-IDDISTR-4542-MAX                  
427900     MOVE +1                       TO WS-INDEX                            
428000     PERFORM IMS-16-GHN-4541-4542                                         
428100     PERFORM UNTIL SEGMENT-SAKNAS  OR BASEN-SLUT OR                       
428200                   WS-INDEX > WS-INDEX-MID-MAX                            
428300       IF SEGMENT-FINNS                   AND                             
428400          4542-IDKUNDNR = W-IDKUNDNR      AND                             
428500          4542-IDUSER   = MSG-SIGNON-USERID                               
428600                                                                          
428700         MOVE 4542-IDARTNR               TO ARTIKEL-POS-3-11              
428800         MOVE ARTIKEL-REKSIFFRA     TO MID-IDARTNR-006(WS-INDEX)          
428900                                       MOD-IDARTNR-006(WS-INDEX)          
429000         COMPUTE WS-KVBEART-NUM = 4542-KVBEART-Q - 4542-KVPREAVB          
429100         MOVE WS-KVBEART-NUM             TO MID-KVBEART(WS-INDEX)         
429200                                            MOD-KVBEART(WS-INDEX)         
429300         MOVE 4542-BERADREF              TO MID-BERADREF(WS-INDEX)        
429400                                            MOD-BERADREF(WS-INDEX)        
429500         IF 4542-KDPRTYP = 'P'                                            
429600            IF DIST79-DEALER-PRICE                                        
429700             IF 4542-PRARTNTO-LOC > +0                                    
429800              MOVE 4542-PRARTNTO-LOC     TO WS-PRARTNTO-NUM               
429900             ELSE                                                         
430000              MOVE 4542-PRARTNTO-LOCPREL TO WS-PRARTNTO-NUM               
430100             END-IF                                                       
430200            ELSE                                                          
430600             MOVE 4542-PRARTNTO          TO WS-PRARTNTO-NUM               
430800            END-IF                                                        
430900            MOVE WS-PRARTNTO-NUM         TO WS-PRARTNTO-RED               
431000            MOVE WS-PRARTNTO-RED         TO MOD-PRARTNTO(WS-INDEX)        
431100            MOVE WS-PRARTNTO-ALFA        TO MID-PRARTNTO(WS-INDEX)        
431200         END-IF                                                           
431300                                                                          
431400         IF 4542-IDLEVNR = SPACE                                          
431500            MOVE 4542-IDARTNR               TO W-IDARTNR                  
431600            IF 4542-IDDC NOT = W-IDDC-B6                                  
431700               MOVE 4542-IDDC TO W-IDDC-B6                                
431800               PERFORM IMS-GU-WDB601                                      
431900            END-IF                                                        
432000            IF DCS-CDC                                                    
432100              PERFORM IMS-17-GHU-ARTM-WDK901                              
432200              COMPUTE ART-KVOKS-VOR =                                     
432300                      ART-KVOKS-VOR - WS-KVBEART-NUM                      
432400              PERFORM IMS-18-REPL-ARTM-WDK901                             
432500            ELSE                                                          
432600              IF 4542-KDORDBEK = 92 OR 93 OR 98                           
432700                MOVE 4542-IDDC              TO W-IDDC                     
432800                PERFORM IMS-GHU-WDK711                                    
432900                COMPUTE SLAG-KVOKS-DAG =                                  
433000                        SLAG-KVOKS-DAG - WS-KVBEART-NUM                   
433100                PERFORM IMS-REPL-WDK711                                   
433200              END-IF                                                      
433300            END-IF                                                        
433400         END-IF                                                           
433500                                                                          
433600         ADD +1 TO WS-INDEX                                               
433700         MOVE '2'          TO 4542-KDVORATG                               
433800         PERFORM IMS-19-REPL-4541-4542                                    
433900       END-IF                                                             
434000       PERFORM IMS-16-GHN-4541-4542                                       
434100     END-PERFORM                                                          
434200     .                                                                    
434300     EJECT                                                                
434400 K-LAES-FRAN-NYVORKO SECTION.                                             
434500     MOVE LOW-VALUE                TO W-WDA6F1KY-MIN-X                    
434600     MOVE HIGH-VALUE               TO W-WDA6F1KY-MAX-X                    
434700     MOVE OHUV-IDDISTR             TO W-IDDISTR-A6F1-MIN                  
434800                                      W-IDDISTR-A6F1-MAX                  
434900     MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-A6F1-MIN                 
435000                                      W-IDKUNDNR-A6F1-MAX                 
435100     MOVE +1                       TO WS-INDEX                            
435200*----------------------------------------------- LETA RÄTT PÅ RAD         
435300*                                                                         
435400     PERFORM IMS-13-GN-VORKO-NY                                           
435500     PERFORM UNTIL SEGMENT-SAKNAS  OR BASEN-SLUT OR                       
435600                   WS-INDEX > WS-INDEX-MID-MAX                            
435700       IF  SEGMENT-FINNS                                                  
435800       AND SEQF-KDVORATG = '1'                                            
435900       AND SEQF-IDUSER   = MSG-SIGNON-USERID                              
436000*                                                                         
436100* WHEN THIS MPP IS BEING FROM BMP W41218, WE NEED TO PROCESS              
436200* ONLY THAT PARTICULAR ORDER NUMBER FOR DIST/KUND                         
436300         MOVE OHUV-BEKUNDRF TO WS-BERADREF-RED                            
436400         IF (W-IDTRANS = 'V412' AND                                       
436500            SEQF-IDORDNR7 = WS-IDORDNR-NUM) OR                            
436600            W-IDTRANS NOT = 'V412'                                        
436700                                                                          
436800         MOVE SEQF-IDDISTR         TO W-IDDISTR-A6F                       
436900         MOVE SEQF-IDKUNDNR        TO W-IDKUNDNR-A6F                      
437000         MOVE SEQF-TIREGDAT-AVV9   TO W-TIREGDAT-AVV9-A6F                 
437100         MOVE SEQF-TIREGTID-AVV9   TO W-TIREGTID-AVV9-A6F                 
437200                                                                          
437300*----------------------------------------------- LÄS FRAM RAD             
437400*                                                                         
437500         PERFORM IMS-14-GHU-VORKO-NY                                      
437600                                                                          
437700         MOVE VOR-IDARTNR           TO ARTIKEL-POS-3-11                   
437800         MOVE ARTIKEL-REKSIFFRA     TO MID-IDARTNR-006(WS-INDEX)          
437900                                       MOD-IDARTNR-006(WS-INDEX)          
438000         COMPUTE WS-KVBEART-NUM = VOR-KVBEART-Q - VOR-KVPREAVB            
438100         MOVE WS-KVBEART-NUM        TO MID-KVBEART(WS-INDEX)              
438200                                       MOD-KVBEART(WS-INDEX)              
438300         MOVE VOR-BERADREF          TO MID-BERADREF(WS-INDEX)             
438400                                       MOD-BERADREF(WS-INDEX)             
438500         IF VOR-KDPRTYP = 'P'                                             
438600            IF DIST79-DEALER-PRICE                                        
438700             IF VOR-PRARTNTO-LOC > +0                                     
438800              MOVE VOR-PRARTNTO-LOC TO WS-PRARTNTO-NUM                    
438900             ELSE                                                         
439000              MOVE VOR-PRARTNTO-LOCPREL TO WS-PRARTNTO-NUM                
439100             END-IF                                                       
439200            ELSE                                                          
439600             MOVE VOR-PRARTNTO      TO WS-PRARTNTO-NUM                    
439800            END-IF                                                        
439900            MOVE WS-PRARTNTO-NUM    TO WS-PRARTNTO-RED                    
440000            MOVE WS-PRARTNTO-RED    TO MOD-PRARTNTO(WS-INDEX)             
440100            MOVE WS-PRARTNTO-ALFA   TO MID-PRARTNTO(WS-INDEX)             
440200         END-IF                                                           
440300                                                                          
440400         MOVE VOR-IDARTNR               TO W-IDARTNR                      
440500                                                                          
440600         IF VOR-IDDC NOT = W-IDDC-B6                                      
440700            MOVE VOR-IDDC TO W-IDDC-B6                                    
440800            PERFORM IMS-GU-WDB601                                         
440900         END-IF                                                           
441000                                                                          
441100         IF DCS-CDC                                                       
441200           PERFORM IMS-17-GHU-ARTM-WDK901                                 
441300           COMPUTE ART-KVOKS-VOR =                                        
441400                   ART-KVOKS-VOR - WS-KVBEART-NUM                         
441500           PERFORM IMS-18-REPL-ARTM-WDK901                                
441600         ELSE                                                             
441700           IF VOR-KDORDBEK = 92 OR 93 OR 98                               
441800             MOVE VOR-IDDC              TO W-IDDC                         
441900             PERFORM IMS-GHU-WDK711                                       
442000             COMPUTE SLAG-KVOKS-DAG =                                     
442100                     SLAG-KVOKS-DAG - WS-KVBEART-NUM                      
442200             PERFORM IMS-REPL-WDK711                                      
442300           END-IF                                                         
442400         END-IF                                                           
442500                                                                          
442600         ADD +1 TO WS-INDEX                                               
442700                                                                          
442800         IF OHUV-IDDC-TVS NOT = W-IDDC-B6                                 
442900            MOVE OHUV-IDDC-TVS TO W-IDDC-B6                               
443000            PERFORM IMS-GU-WDB601                                         
443100         END-IF                                                           
443200                                                                          
443300         IF  DCS-NDC                                                      
443400           MOVE '6'                TO VOR-KDVORATG                        
443500           MOVE OHUV-IDKUNDRF      TO VOR-IDKUNDRF-LEV                    
443600           MOVE VOR-KVBEART-Q      TO VOR-KVPREAVB                        
443700           ACCEPT VOR-TIREGDAT-LEV FROM DATE                              
443800           ACCEPT VOR-TIREGTID-LEV FROM TIME                              
443900           IF VOR-TIKLAR = ZERO                                           
444000              MOVE VOR-TIREGDAT-LEV TO VOR-TIKLAR                         
444100              COMPUTE VOR-TIKLATID = VOR-TIREGTID-LEV                     
444200                                      / 100                               
444300              END-COMPUTE                                                 
444400           END-IF                                                         
444500         ELSE                                                             
444600           MOVE '2'                TO VOR-KDVORATG                        
444700           MOVE OHUV-IDKUNDRF      TO VOR-IDKUNDRF-LEV                    
444800           MOVE VOR-KVBEART-Q      TO VOR-KVPREAVB                        
444900           ACCEPT VOR-TIREGDAT-LEV FROM DATE                              
445000           ACCEPT VOR-TIREGTID-LEV FROM TIME                              
445100           IF OHUV-KDORDKL = 1                                            
445200              IF VOR-TIKLAR = ZERO                                        
445300                MOVE VOR-TIREGDAT-LEV TO VOR-TIKLAR                       
445400                COMPUTE VOR-TIKLATID = VOR-TIREGTID-LEV                   
445500                                      / 100                               
445600              END-IF                                                      
445700           END-IF                                                         
445800         END-IF                                                           
445900                                                                          
446000         PERFORM IMS-12-GHU-WDK611                                        
446100*        SUBTRACT VOR-KVBEART-Q    FROM CLAG-KVVORKO                      
446200         SUBTRACT WS-KVBEART-NUM   FROM CLAG-KVVORKO                      
446300         PERFORM IMS-11-REPL-WDK6                                         
446400                                                                          
446500         PERFORM IMS-15-REPL-VORKO-NY                                     
446600       END-IF                                                             
446700       END-IF                                                             
446800       PERFORM IMS-13-GN-VORKO-NY                                         
446900     END-PERFORM                                                          
447000     .                                                                    
447100     EJECT                                                                
447200 J-SKICKA-PRISFRAGA SECTION.                                              
447300                                                                          
447400     MOVE 1                      TO 3039-REQU-IDMSGVER                    
447500     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
447600     MOVE 'W4022200'             TO 3039-REQU-IDUSER                      
447700                                                                          
447800     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
447900     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
448000     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
448100     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
448200     IF MID-IDARTNR-006(WS-INDEX-MID-MAX) = ALL '+'                       
448300       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
448400       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
448500       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
448600     END-IF                                                               
448700*    MOVE ORAD-IDPRQUES          TO 3039-MID-IDPRQUES                     
448800     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
448900                                                                          
449000     PERFORM S05-SKICKA-OPEN                                              
449100     PERFORM S05-SKICKA-MEDDELANDE                                        
449200     PERFORM S05-SKICKA-CLOSE                                             
449300                                                                          
449400     .                                                                    
449500     EJECT                                                                
449600 Z-FINIT-INSERT-MSG SECTION.                                              
449700                                                                          
449800     IF MED-IDMFSFEL NOT = SPACE                                          
449900         CALL WMEDKONV USING MED-WMEDAREA                                 
450000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
450100     END-IF                                                               
450200                                                                          
450300     IF NOT ALLT-OK                                                       
450400        PERFORM MFS-ROER-EJ-BILD                                          
450500     END-IF                                                               
450600                                                                          
450700     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
450800     PERFORM IMS-INSERT-MSG                                               
450900     .                                                                    
451000     EJECT                                                                
451100 S02-RENSA-TILLK-TAB SECTION.                                             
451200                                                                          
451300     MOVE +1              TO WS-INDEX-TILLK                               
451400     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
451500        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
451600        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
451700        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
451800        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
451900        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
452000        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
452100        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
452200        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
452300        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
452400        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
452500        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
452600        INITIALIZE           TILK-DEAL-PR-LINE(WS-INDEX-TILLK)            
452700        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
452800                             TILK-PRARTNTO-LOC(WS-INDEX-TILLK)            
452900                             TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)        
453000        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
453100        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
453200        ADD +1            TO WS-INDEX-TILLK                               
453300     END-PERFORM                                                          
453400     MOVE +1              TO WS-INDEX-TILLK                               
453500     .                                                                    
453600     EJECT                                                                
453700 S03-PRISTILLAMPA SECTION.                                                
453800                                                                          
453900     MOVE NEJ                  TO VOR-MAN-PRIS-SW                         
454000                                                                          
454800     IF ORAD-PRARTNTO NOT = +0                                            
454900        IF OHUV-KDORDKL = 0 AND ORAD-IDDC = WC-CDC-SE AND                 
455000          (DIST07-KINA  OR DIST07-INDIEN OR DIST07-MEXICO OR              
455100           DIST07-KOREA OR DIST07-TURKEY OR DIST07-MALAYSIA OR            
455200           DIST07-THAILAND OR DIST07-TAIWAN OR                            
455210           DIST07-S-AFRICA OR DIST07-BRAZIL)                              
455300*VOR BOUNCE ORDER AND IF WE PUT AN MANUALLY                               
455400*NETTOPRICE THEN WE HAVE TO FETCH AVERAGE PRICE ALSO                      
455500           MOVE 1              TO PRIS-KDCALL                             
455600           MOVE JA             TO VOR-MAN-PRIS-SW                         
455700        ELSE                                                              
455800           MOVE 2              TO PRIS-KDCALL                             
455900        END-IF                                                            
456000     ELSE                                                                 
456100        MOVE 1                 TO PRIS-KDCALL                             
456200     END-IF                                                               
456400                                                                          
456500     MOVE IDPGM                TO PRIS-IDPGM                              
456600     MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                            
456700     MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                            
456800     MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                           
456900     MOVE ORAD-IDDC            TO PRIS-IDDC                               
457000     MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                            
457100     MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                            
457200     MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                           
457300                                                                          
457400     CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                      
457500                         PRIS-WDK7-PCB PRIS-GMTA-PCB                      
457600                         PRIS-BETA-PCB PRIS-GPRIA-PCB                     
457700                         PRIS-GPRIB-PCB                                   
457800                         PRIS-COST-WDK6-PCB                               
457900                         PRIS-COST-WDK7-PCB                               
458000                         PRIS-COST-WDF1-PCB                               
458100                         PRIS-COST-9305-PCB                               
458200                         PRIS-COST-WDK72-PCB                              
458300                         PRIS-COST-WDB6-PCB                               
458400                                                                          
458500     IF PRIS-KDSVAR = '2'                                                 
458600       MOVE 'S03-: DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'           
458700                                                   TO FELTEXT             
458800       CALL ABEND USING RKOD-ABEND                                        
458900     END-IF                                                               
459000                                                                          
459100     IF PRIS-KDCALL = 2                                                   
459200       MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                               
459300       IF ORAD-KDPRTYP = SPACE                                            
459400         MOVE 'P'           TO ORAD-KDPRTYP                               
459500         MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                                
459600       END-IF                                                             
459700     ELSE                                                                 
459800       IF VOR-MAN-PRIS                                                    
459900*FROM 4225, VOR-QUEUE                                                     
460000          MOVE PRIS-PRAVCOST    TO ORAD-PRAVCOST                          
460100          MOVE PRIS-KDVALISO    TO ORAD-KDVALISO                          
460200          IF ORAD-KDPRTYP = SPACE                                         
460300             MOVE 'P'              TO ORAD-KDPRTYP                        
460400             MOVE ORAD-TIREGDAT    TO ORAD-TIPRIS                         
460500          END-IF                                                          
460600       ELSE                                                               
461100         MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO                             
461300         MOVE PRIS-FLPRTILL  TO ORAD-FLPRTILL                             
461400         MOVE PRIS-KDPRTYP   TO ORAD-KDPRTYP                              
461500         MOVE PRIS-PRBPRIS   TO ORAD-PRBPRIS                              
461600         MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                               
461700         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
461800         MOVE PRIS-PRAVCOST  TO ORAD-PRAVCOST                             
461900       END-IF                                                             
462000     END-IF                                                               
462100     .                                                                    
462200     EJECT                                                                
462300                                                                          
462400                                                                          
462500 S04-DATA-TILL-DEL-NOTE SECTION.                                          
462600                                                                          
462700     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
462800     IF DIST07-USA-RETAILER-DNOTE                                         
462900     OR DIST07-CAN-RETAILER                                               
463000                                                                          
463100        INITIALIZE DNOT-ORDER-INFO                                        
463200                                                                          
463300        MOVE IDPGM                    TO DNOT-IDPGM                       
463400        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
463500        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
463600        MOVE ORAD-IDDC                TO DNOT-IDDC                        
463700        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
463800        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
463900        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
464000        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
464100        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
464200        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
464300        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
464400        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
464500        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
464600        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
464700        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
464800        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
464900        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
465000        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
465100        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
465200        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
465300        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
465400        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
465500        MOVE NEJ                      TO DNOT-FLDIRLEV                    
465600                                                                          
465700                                                                          
465800        IF WS-INDEX-MID > WS-INDEX-MID-MAX                                
465900           MOVE JA              TO DNOT-FL-ORAD-LAST                      
466000        END-IF                                                            
466100                                                                          
466200        CALL W411DNOT USING DNOT-W411DNOT                                 
466300                            DNOT-ORQP-PCB                                 
466400                            DNOT-ORQP2-PCB                                
466500                            DNOT-ORQP3-PCB                                
466600                            DNOT-4013-PCB                                 
466700                            DNOT-BENA-PCB                                 
466800     END-IF                                                               
466900     .                                                                    
467000     EJECT                                                                
467100                                                                          
467200 S05-SKICKA-OPEN SECTION.                                                 
467300                                                                          
467400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
467500     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
467600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
467700                                                                          
467800     IF SEND-KDRC > 0                                                     
467900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
468000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
468100       DELIMITED BY SIZE INTO FELTEXT                                     
468200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
468300     END-IF                                                               
468400     .                                                                    
468500     SKIP3                                                                
468600 S05-SKICKA-MEDDELANDE SECTION.                                           
468700                                                                          
468800     MOVE 'PUT'                      TO SEND-KDFUNC                       
468900     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
469000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
469100                                                                          
469200     IF SEND-KDRC > 0                                                     
469300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
469400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
469500       DELIMITED BY SIZE INTO FELTEXT                                     
469600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
469700     END-IF                                                               
469800     .                                                                    
469900     SKIP3                                                                
470000 S05-SKICKA-CLOSE SECTION.                                                
470100                                                                          
470200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
470300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
470400                                                                          
470500     IF SEND-KDRC > 0                                                     
470600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
470700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
470800       DELIMITED BY SIZE INTO FELTEXT                                     
470900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
471000     END-IF                                                               
471100     .                                                                    
471200     EJECT                                                                
471300 S06-DELETE-PRICE-Q-LINE SECTION.                                         
471400                                                                          
471500     IF DIST79-DEALER-PRICE                                               
471600       IF OBKR-IDPRQUES > ZERO                                            
471700         INITIALIZE PRQU-W335PRQU                                         
471800         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
471900         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
472000         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
472100         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
472200         MOVE 4                  TO PRQU-KDCALL                           
472300         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
472400                                            PRQU-WDC7-PCB                 
472500                                            PRQU-SJKO-WDK6-PCB            
472600       END-IF                                                             
472700     END-IF                                                               
472800     .                                                                    
472900                                                                          
473000     EJECT                                                                
473100 S10-WRONG-PICTURE-MESSAGE SECTION.                                       
473200     SKIP2                                                                
473300* *****************************************************                   
473400*                                                     *                   
473500* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
473600*                                                     *                   
473700* *****************************************************                   
473800     SKIP2                                                                
473900     MOVE JA                  TO HOPP-TILL-0504                           
474000     MOVE 'W0O50401'          TO MFS-IDMOD                                
474100     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
474200     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
474300     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
474400     PERFORM IMS-INSERT-MSG                                               
474500     .                                                                    
474600     EJECT                                                                
474700 MFS-RENSA-MOD-RADER SECTION.                                             
474800                                                                          
474900     MOVE +1 TO WS-INDEX                                                  
475000     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
475100       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-006(WS-INDEX)                
475200                                 MOD-KVBEART(WS-INDEX)                    
475300                                 MOD-PRARTNTO(WS-INDEX)                   
475400                                 MOD-FLINVEST(WS-INDEX)                   
475500                                 MOD-KDVRINFO(WS-INDEX)                   
475600                                 MOD-BERADREF(WS-INDEX)                   
475700       ADD  +1 TO WS-INDEX                                                
475800     END-PERFORM                                                          
475900     .                                                                    
476000     EJECT                                                                
476100                                                                          
476200                                                                          
476300 MFS-ROER-EJ-BILD SECTION.                                                
476400                                                                          
476500                                                                          
476600     MOVE +1 TO WS-INDEX                                                  
476700     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
476800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-006(WS-INDEX)                
476900                                 MOD-KVBEART(WS-INDEX)                    
477000                                 MOD-PRARTNTO(WS-INDEX)                   
477100                                 MOD-FLINVEST(WS-INDEX)                   
477200                                 MOD-KDVRINFO(WS-INDEX)                   
477300                                 MOD-BERADREF(WS-INDEX)                   
477400       ADD  +1 TO WS-INDEX                                                
477500     END-PERFORM                                                          
477600     .                                                                    
477700     EJECT                                                                
477800* --- IMS SEKTIONER ---                                                   
477900                                                                          
478000 IMS-GET-MSG SECTION.                                                     
478100                                                                          
478200     MOVE '  QC' TO GODK-STATUSKODER                                      
478300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
478400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
478500     PERFORM IMS-STATUSKONTROLL                                           
478600     .                                                                    
478700     SKIP2                                                                
478800 IMS-INSERT-MSG SECTION.                                                  
478900                                                                          
479000     IF ENGLISH-TEXT                                                      
479100       MOVE 'N' TO MFS-KDHUVOMR                                           
479200     END-IF                                                               
479300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
479400     MOVE SPACE TO GODK-STATUSKODER                                       
479500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
479600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
479700     PERFORM IMS-STATUSKONTROLL                                           
479800     .                                                                    
479900     SKIP2                                                                
480000 IMS-INSERT-4223-MSG SECTION.                                             
480100                                                                          
480200     IF ENGLISH-TEXT                                                      
480300       MOVE 'N' TO MFS-KDHUVOMR                                           
480400     END-IF                                                               
480500     MOVE LOW-VALUE TO 4223-Z1 4223-Z2                                    
480600     MOVE SPACE TO GODK-STATUSKODER                                       
480700*      STRING 'IMS-INSERT-4223-MSG'                                       
480800*           DELIMITED BY SIZE INTO FELTEXT                                
480900*      CALL FELLOG                                                        
481000     CALL CBLTDLI USING ISRT 4223-PCB 4223-MSG-IO-AREA                    
481100     MOVE 4223-STATUS-CODE TO STATUS-WS                                   
481200     PERFORM IMS-STATUSKONTROLL                                           
481300     .                                                                    
481400     EJECT                                                                
481500 IMS-INSERT-4223V-MSG SECTION.                                            
481600                                                                          
481700     IF ENGLISH-TEXT                                                      
481800       MOVE 'N' TO MFS-KDHUVOMR                                           
481900     END-IF                                                               
482000     MOVE LOW-VALUE TO 4223-Z1 4223-Z2                                    
482100     MOVE SPACE TO GODK-STATUSKODER                                       
482200*      STRING 'IMS-INSERT-4223V-MSG'                                      
482300*           DELIMITED BY SIZE INTO FELTEXT                                
482400*      CALL FELLOG                                                        
482500     CALL CBLTDLI USING ISRT 4223V-PCB 4223-MSG-IO-AREA                   
482600     MOVE 4223V-STATUS-CODE TO STATUS-WS                                  
482700     PERFORM IMS-STATUSKONTROLL                                           
482800     .                                                                    
482900     EJECT                                                                
483000 IMS-INSERT-4297-MSG SECTION.                                             
483100                                                                          
483200     MOVE LOW-VALUE TO 4297-Z1 4297-Z2                                    
483300     MOVE SPACE TO GODK-STATUSKODER                                       
483400     CALL CBLTDLI USING ISRT 4297-PCB 4297-MSG-IO-AREA                    
483500     MOVE 4297-STATUS-CODE TO STATUS-WS                                   
483600     PERFORM IMS-STATUSKONTROLL                                           
483700     .                                                                    
483800     EJECT                                                                
483900 IMS-01-GU-WDQ2-WDQ201 SECTION.                                           
484000                                                                          
484100     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
484200          DELIMITED BY SIZE INTO SSA1                                     
484300     MOVE '  GE'               TO GODK-STATUSKODER                        
484400     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                 
484500     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
484600     PERFORM IMS-STATUSKONTROLL                                           
484700     .                                                                    
484800     SKIP2                                                                
484900 IMS-03-GNP-WDQ2-WDQ212 SECTION.                                          
485000                                                                          
485100     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
485200          DELIMITED BY SIZE INTO SSA1                                     
485300     MOVE '    '               TO GODK-STATUSKODER                        
485400     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-AREA-ARB SSA1                
485500     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
485600     PERFORM IMS-STATUSKONTROLL                                           
485700     .                                                                    
485800     EJECT                                                                
485900 IMS-07-GU-WDQ1-WDQ101 SECTION.                                           
486000                                                                          
486100     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
486200                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
486300          DELIMITED BY SIZE INTO SSA1                                     
486400     MOVE '  GE'               TO GODK-STATUSKODER                        
486500     CALL CBLTDLI USING GU   WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
486600     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
486700     PERFORM IMS-STATUSKONTROLL                                           
486800     .                                                                    
486900     SKIP2                                                                
487000 IMS-08-ISRT-WDQ101 SECTION.                                              
487100                                                                          
487200     MOVE 'WDQ101   '          TO SSA1                                    
487300     MOVE '    '               TO GODK-STATUSKODER                        
487400     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
487500     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
487600     PERFORM IMS-STATUSKONTROLL                                           
487700     .                                                                    
487800     EJECT                                                                
487900 IMS-09-ISRT-WDQ4-WDQ401 SECTION.                                         
488000                                                                          
488100     MOVE 'WDQ401   '          TO SSA1                                    
488200     MOVE '  II'               TO GODK-STATUSKODER                        
488300     CALL CBLTDLI USING ISRT WDQ4-PCB DLI-IO-AREA-ORAD SSA1               
488400     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
488500     PERFORM IMS-STATUSKONTROLL                                           
488600     .                                                                    
488700     EJECT                                                                
488800 IMS-10-GU-WLARTM-WDK901 SECTION.                                         
488900                                                                          
489000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
489100          DELIMITED BY SIZE INTO SSA1                                     
489200     MOVE '  GE'               TO GODK-STATUSKODER                        
489300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
489400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
489500     PERFORM IMS-STATUSKONTROLL                                           
489600     .                                                                    
489700     EJECT                                                                
489800 IMS-12-GHU-WDK611 SECTION.                                               
489900                                                                          
490000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
490100          DELIMITED BY SIZE INTO SSA1                                     
490200     MOVE 'WDK611'               TO SSA2                                  
490300     MOVE '  '                   TO GODK-STATUSKODER                      
490400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
490500     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
490600     PERFORM IMS-STATUSKONTROLL                                           
490700     .                                                                    
490800     SKIP2                                                                
490900 IMS-11-REPL-WDK6 SECTION.                                                
491000                                                                          
491100     MOVE '  '                 TO GODK-STATUSKODER                        
491200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
491300     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
491400     PERFORM IMS-STATUSKONTROLL                                           
491500     .                                                                    
491600     EJECT                                                                
491700 IMS-13-GN-VORKO-NY SECTION.                                              
491800                                                                          
491900     STRING 'WDA6F1  (WDA6F1KY >' W-WDA6F1KY-MIN-X                        
492000                    '&WDA6F1KY <' W-WDA6F1KY-MAX-X ')'                    
492100          DELIMITED BY SIZE INTO SSA1                                     
492200     MOVE '  GEGB'               TO GODK-STATUSKODER                      
492300     CALL CBLTDLI USING GN   WDA6F1-PCB DLI-IO-AREA-WDA6F1 SSA1           
492400     MOVE WDA6F1-STATUS-CODE     TO STATUS-WS                             
492500     PERFORM IMS-STATUSKONTROLL                                           
492600     .                                                                    
492700     EJECT                                                                
492800 IMS-14-GHU-VORKO-NY SECTION.                                             
492900                                                                          
493000     STRING 'WDA601  (WDA6FSEQ =' W-WDA6FKY-X ')'                         
493100          DELIMITED BY SIZE INTO SSA1                                     
493200     MOVE '  '                   TO GODK-STATUSKODER                      
493300     CALL CBLTDLI USING GHU  WDA6F-PCB DLI-IO-AREA-WDA601 SSA1            
493400     MOVE WDA6F-STATUS-CODE      TO STATUS-WS                             
493500     PERFORM IMS-STATUSKONTROLL                                           
493600     .                                                                    
493700     SKIP2                                                                
493800 IMS-15-REPL-VORKO-NY SECTION.                                            
493900                                                                          
494000     MOVE '  '                 TO GODK-STATUSKODER                        
494100     CALL CBLTDLI USING REPL WDA6F-PCB DLI-IO-AREA-WDA601                 
494200     MOVE WDA6F-STATUS-CODE     TO STATUS-WS                              
494300     PERFORM IMS-STATUSKONTROLL                                           
494400     .                                                                    
494500     SKIP2                                                                
494600 IMS-16-GHN-4541-4542 SECTION.                                            
494700                                                                          
494800     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
494900          DELIMITED BY SIZE INTO SSA1                                     
495000     STRING 'WDGX4542(KY4542  >=' W-4542KEY-MIN-X                         
495100                    '&KY4542  <=' W-4542KEY-MAX-X                         
495200                    '&KDVORATG =' W-KDVORATG-X ')'                        
495300          DELIMITED BY SIZE INTO SSA2                                     
495400     MOVE '  GEGB'             TO GODK-STATUSKODER                        
495500     CALL CBLTDLI USING GHN 4541-PCB DLI-IO-AREA-VOR  SSA1 SSA2           
495600     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
495700     PERFORM IMS-STATUSKONTROLL                                           
495800     .                                                                    
495900     EJECT                                                                
496000 IMS-17-GHU-ARTM-WDK901 SECTION.                                          
496100                                                                          
496200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
496300          DELIMITED BY SIZE  INTO SSA1                                    
496400     MOVE '  '                 TO GODK-STATUSKODER                        
496500     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
496600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
496700     PERFORM IMS-STATUSKONTROLL                                           
496800     .                                                                    
496900     SKIP2                                                                
497000 IMS-18-REPL-ARTM-WDK901 SECTION.                                         
497100                                                                          
497200     MOVE '  '                 TO GODK-STATUSKODER                        
497300     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
497400     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
497500     PERFORM IMS-STATUSKONTROLL                                           
497600     .                                                                    
497700     EJECT                                                                
497800 IMS-GHU-WDK711 SECTION.                                                  
497900                                                                          
498000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
498100          DELIMITED BY SIZE  INTO SSA1                                    
498200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
498300          DELIMITED BY SIZE  INTO SSA2                                    
498400     MOVE '    '               TO GODK-STATUSKODER                        
498500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
498600     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
498700     PERFORM IMS-STATUSKONTROLL                                           
498800     .                                                                    
498900     SKIP3                                                                
499000 IMS-REPL-WDK711 SECTION.                                                 
499100                                                                          
499200     MOVE '    '               TO GODK-STATUSKODER                        
499300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
499400     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
499500     PERFORM IMS-STATUSKONTROLL                                           
499600     .                                                                    
499700     EJECT                                                                
499800                                                                          
499900 IMS-19-REPL-4541-4542 SECTION.                                           
500000                                                                          
500100     MOVE '  '                 TO GODK-STATUSKODER                        
500200     CALL CBLTDLI USING REPL 4541-PCB DLI-IO-AREA-VOR                     
500300     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
500400     PERFORM IMS-STATUSKONTROLL                                           
500500     .                                                                    
500600 IMS-GU-WDB201 SECTION.                                                   
500700                                                                          
500800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
500900          DELIMITED BY SIZE INTO SSA1                                     
501000     MOVE '  '                 TO GODK-STATUSKODER                        
501100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
501200     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
501300     PERFORM IMS-STATUSKONTROLL                                           
501400     .                                                                    
501500     SKIP2                                                                
501600 IMS-GU-WDB101 SECTION.                                                   
501700                                                                          
501800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
501900          DELIMITED BY SIZE INTO SSA1                                     
502000     MOVE '  '                 TO GODK-STATUSKODER                        
502100     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
502200     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
502300     PERFORM IMS-STATUSKONTROLL                                           
502400     .                                                                    
502500                                                                          
502600                                                                          
502700 IMS-GU-WDB601    SECTION.                                                
502800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
502900          DELIMITED BY SIZE INTO SSA1                                     
503000     MOVE '  GE' TO GODK-STATUSKODER                                      
503100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
503200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
503300     PERFORM IMS-STATUSKONTROLL                                           
503400     .                                                                    
503500     SKIP2                                                                
503600 IMS-GU-WDF501 SECTION.                                                   
503700                                                                          
503800     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-CROSS-X ')'                   
503900          DELIMITED BY SIZE INTO SSA1                                     
504100     MOVE '  GE'              TO GODK-STATUSKODER                         
504200     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
504300     MOVE WDF5-STATUS-CODE    TO STATUS-WS                                
504400     PERFORM IMS-STATUSKONTROLL                                           
504500     .                                                                    
504600                                                                          
504700 IMS-ISRT-WDR601 SECTION.                                                 
504800                                                                          
504900     MOVE 'WDR601'         TO SSA1                                        
505000     MOVE '  II'           TO GODK-STATUSKODER                            
505100     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
505200     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
505300     PERFORM IMS-STATUSKONTROLL                                           
505400     .                                                                    
505500                                                                          
505600                                                                          
505700 IMS-STATUSKONTROLL SECTION.                                              
505800                                                                          
505900     SET STATUS-IX TO 1                                                   
506000     SEARCH GODK-STATUS                                                   
506100       AT END CALL FELLOG                                                 
506200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
506300     END-SEARCH                                                           
506400     .                                                                    
