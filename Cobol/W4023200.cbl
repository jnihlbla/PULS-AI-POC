000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4023200.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   JAN  1991.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR UPPLÄGGNING AV ORDERRADER I                  
001100*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
001200*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
001300*                                                                         
001400*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001500*        SVARSBILD - 4233.                                                
001600*                                                                         
001700*              I DE FALL ORAD-ADLAGOMR BLIR = +0                          
001800*              SÄTTER VI ORAD-ADLAGOMR = +1,                              
001900*                                                                         
002000*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
002100*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
002200*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
002300*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
002400*    DETTA I LAGEROMRÅDE 1.                                               
002500*                                                                         
002600*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002700*        PROGRAMMET UPPDAT     WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
002800*        PROGRAMMET UPPDAT     WLORQI (WDQ2)  ORDERHUVUD                  
002900*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
003000*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
003100*        PROGRAMMET LÄSER              WDK7   ARTIKELREGISTER             
003200*                                                                         
003300*    INDATA.                                                              
003400*        TRANSAKTION: W4T232                                              
003500*        MID:         W4I23201                                            
003600*    UTDATA.                                                              
003700*        MOD:         W4O23201                                            
003800*                                                                         
003900*    ÄNDRINGAR:                                                           
004000*        E'TRACKER: 5444132 DATED 2007-08-21                              
004100*                                                                         
004200*        2008-02-XX.   E'TRACKER: 5174148                                 
004300*        "DIRECT ORDER SHOULD UPDATE RIGHT SUPPLIER"                      
004400*        WHEN FLLSBOK = 'N' AND NOT SOFTWARE, THE SUPPLIER                
004500*        IN WDQ201 "OHUV-IDLEVNR-EJLS" SHOULD BE USED.                    
004600*                                     SEE  ECB-BYGG-UPP-ORDERRAD.         
004700*        IF ONE ENTERED PARTNUMBER NOT HAVE VALID PRICE ON K621,          
004800*        NO ORDER-LINES ARE REGISTERED AND A MESSAGE IS DISPLAYED.        
004900*                                                                         
005000*        E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                         
005100*        E'TRACKER:10254592 DATED 2015       DECOMISSION VOHF             
005200*        E'TRACKER: 10263222      2015      FORCE TO END ORDER REG        
005300     EJECT                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500                                                                          
005600 DATA DIVISION.                                                           
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000     SKIP3                                                                
006100 77  IDPGM                       PIC X(08)   VALUE 'W4023200'.            
006200 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
006300 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
006400 77  HOPP-TILL-4233              PIC X(1)    VALUE 'N'.                   
006500 77  HOPP-TILL-0504              PIC X(1)   VALUE 'N'.                    
006600 77  JA                          PIC X(1)    VALUE 'J'.                   
006700 77  YES                         PIC X(1)    VALUE 'Y'.                   
006800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006900*                                                                         
007000*    ---FOR MOD0504-IDTRANS                                               
007100 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
007200                                                                          
007300*01  -COPY WWDCKONS                                                       
007400                                                                          
007500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
007600 77  RKOD-ABEND                  PIC S9(3)   COMP SYNC VALUE +33.         
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
007800 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
007900 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008000 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008100 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
008200 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
008300 77  WS-INDEX-WOPS-MAX           PIC S9(9) COMP SYNC VALUE +100.          
008400 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
008500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
008600 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
008700 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
008800 77  WS-KVBEART-NUM              PIC 9(6)    VALUE ZERO.                  
008900 77  WS-FLLSBOK                  PIC X       VALUE SPACE.                 
009000                                                                          
009100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009200     88  ALLT-OK                             VALUE 'J'.                   
009300                                                                          
009400 77  OBKR-SW                     PIC X       VALUE 'N'.                   
009500     88  SKRIV-OBKR                          VALUE 'J'.                   
009600     88  OBKR-SKRIVEN                        VALUE 'S'.                   
009700                                                                          
009800 77  VOR-MAN-PRIS-SW             PIC X       VALUE 'N'.                   
009900     88  VOR-MAN-PRIS                        VALUE 'J'.                   
010000                                                                          
010100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010200     88  EGEN-MID                            VALUE '4232'.                
010300     88  GODK-MID                            VALUE '4231' '4232'          
010400                                                   '4233'.                
010500     EJECT                                                                
010600 01  WS-ALFA-6.                                                           
010700     03  WS-NUM-6                PIC 9(6).                                
010800 01  WS-ALFA-7.                                                           
010900     03  WS-NUM-7                PIC 9(7).                                
011000                                                                          
011100 01  WS-IDARTNR.                                                          
011200     03  FILLER                  PIC X(2).                                
011300     03  WS-IDARTNR-3-11.                                                 
011400         05  WS-IDARTNR-3-9      PIC X(7).                                
011500         05  WS-IDARTNR-10       PIC X(1).                                
011600         05  WS-IDARTNR-11       PIC X(1).                                
011700                                                                          
011800 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
011900 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
012000     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
012100     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
012200                                                                          
012300 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
012400 01  FILLER REDEFINES WS-TIHHMMSS.                                        
012500     03 WS-TIHHMM                PIC 9(4).                                
012600     03 FILLER                   PIC 9(2).                                
012700     EJECT                                                                
012800                                                                          
012900 01  ARTIKEL-REKSIFFRA.                                                   
013000     03  ARTIKEL-POS-1-2         PIC X(2)    VALUE ZERO.                  
013100     03  ARTIKEL-POS-3-11        PIC X(9)    VALUE ZERO.                  
013200                                                                          
013300 01  WS-PRARTNTO-NUM             PIC 9(7)V9(2).                           
013400 01  WS-PRARTNTO-RED             PIC 9(7).9(2).                           
013500 01  PRARTNTO-FILLER REDEFINES WS-PRARTNTO-RED.                           
013600     03  WS-PRARTNTO-ALFA        PIC X(10).                               
013700                                                                          
013800                                                                          
013900 01  W-GMT-IDDC-CLEAR-GRP.                                                
014000*                                 GRUPP AV IDDC-CLEAR                     
014100     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
014200                                 PIC X(2)    VALUE SPACE.                 
014300                                                                          
014400 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
014500                                                                          
014600 01  FILLER REDEFINES TEST-IDDISTR.                                       
014700*    03 -COPY WWDIST07                                                    
014800 01  FILLER REDEFINES TEST-IDDISTR.                                       
014900*    03 -COPY WWDIST35                                                    
015000 01  FILLER REDEFINES TEST-IDDISTR.                                       
015100*    03 -COPY WWDIST79                                                    
015200     EJECT                                                                
015300                                                                          
015400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015500 01  GENERELLA-SUBPROGRAM.                                                
015600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016200     SKIP2                                                                
016300 01  GEMENSAMMA-SUBPROGRAM.                                               
016400     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
016500*        PRISTILLÄMPNING                                                  
016600     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
016700*        HÄMTA PRISFRÅGENR                                                
016800     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
016900*        DEALER PRISFRÅGABEHANDLING                                       
017000     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
017100*        KONTROLLERA SPÄRRAR                                              
017200     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
017300*        KONTROLLERA NDC-ARTREG                                           
017400     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
017500*        KONTROLLERA NDC-ARTREG                                           
017600     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
017700*        KONTROLLERA SDC-ARTREG                                           
017800     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
017900*        KONTROLL ENHETSLAST                                              
018000     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
018100*        FORMELLA KONTROLLER AV INDATA                                    
018200     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
018300*        WOPS RADBEHANDLING                                               
018400     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
018500*        OMVANDLING AV LAGOMR + PLATS                                     
018600     EJECT                                                                
018700 01  MESSAGE-CODES.                                                       
018800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018900     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
019000     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
019100     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
019200     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
019300     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
019400     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
019500     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
019600     03  ERR-ORDERRADER-AVBRYTS  PIC X(3)    VALUE '343'.                 
019700     03  INF-UPPDATERA-LEV       PIC X(3)    VALUE '290'.                 
019800     SKIP3                                                                
019900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020000*   -COPY WMEDAREA                                                        
020100     SKIP3                                                                
020200 01  FILLER.                                                              
020300   03  FELMEDD-AREA.                                                      
020400     05  FELMEDD-ENGLISH.                                                 
020500       10  FILLER                PIC X(50)                                
020600     VALUE '622 4232 NOT AVAILABLE ONLY VALID FROM 4231'.                 
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020900*   -COPY WMSGINIT                                                        
021000     EJECT                                                                
021100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
021200 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
021300*   -COPY W335PRIS                                                        
021400     EJECT                                                                
021500 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
021600*   -COPY W335PRNO                                                        
021700     EJECT                                                                
021800 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
021900*   -COPY W335PRQU                                                        
022000     EJECT                                                                
022100 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
022200*   -COPY W411SPAR                                                        
022300     EJECT                                                                
022400 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
022500*   -COPY W411NDCA                                                        
022600     EJECT                                                                
022700 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
022800*   -COPY W411XDCA                                                        
022900     EJECT                                                                
023000 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
023100*   -COPY W411XDK7 -PRE NDCA-                                             
023200     EJECT                                                                
023300 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
023400*   -COPY W411SDCA                                                        
023500     EJECT                                                                
023600 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
023700*   -COPY W411LAST                                                        
023800     EJECT                                                                
023900 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
024000*   -COPY W411ORFK                                                        
024100     EJECT                                                                
024200 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
024300*   -COPY W411AREG                                                        
024400     EJECT                                                                
024500 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
024600*   -COPY W413AVSR                                                        
024700     SKIP2                                                                
024800 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
024900*   -COPY W413ADRS                                                        
025000     EJECT                                                                
025100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
025200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025300     SKIP3                                                                
025400*01  MID -COPY W4I23201                                                   
025500     EJECT                                                                
025600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025700     SKIP3                                                                
025800*01  -COPY WMSGAREA                                                       
025900     EJECT                                                                
026000*    03  MOD -COPY W4O23201   -RED MSG-AREA.                              
026100                                                                          
026200*   TO RETURN TO MAIN MENU                                                
026300*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026600     SKIP3                                                                
026700*01  -COPY WMFSAREA                                                       
026800     EJECT                                                                
026900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027100 01  NYCKLAR-TILL-DLI.                                                    
027200                                                                          
027300     03  W-IDGMTREF-X.                                                    
027400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
027500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
027600         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
027700                                                                          
027800     03  W-WDQ101KY-MIN-X.                                                
027900         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
028000         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
028100         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
028200         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
028300         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
028400                                                                          
028500     03  W-WDQ101KY-MAX-X.                                                
028600         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
028700         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
028800         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
028900         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
029000         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
029100                                                                          
029200     03  W-IDDC-X.                                                        
029300         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
029400                                                                          
029500     03  W-IDARTNR-X.                                                     
029600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029700                                                                          
029800     03  W-IDLEVNR-X.                                                     
029900         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
030000     EJECT                                                                
030100                                                                          
030200     03  W-4542KEY-MIN-X.                                                 
030300         05  W-IDDISTR-4542-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
030400         05  W-IDANSK-4542-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
030500         05  W-IDARTNR-4542-MIN  PIC S9(9)   VALUE ZERO COMP-3.           
030600         05  W-IDLOPNR-4542-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
030700         05  W-IDORDER-4542-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
030800                                                                          
030900     03  W-KDVORATG-X.                                                    
031000         05  W-KDVORATG         PIC  X      VALUE '1'.                    
031100                                                                          
031200     03  W-4542KEY-MAX-X.                                                 
031300         05  W-IDDISTR-4542-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
031400         05  W-IDANSK-4542-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
031500         05  W-IDARTNR-4542-MAX  PIC S9(9)   VALUE ZERO COMP-3.           
031600         05  W-IDLOPNR-4542-MAX  PIC S9(3)   VALUE ZERO COMP-3.           
031700         05  W-IDORDER-4542-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
031800                                                                          
031900     03  W-WDA6F1KY-MIN-X.                                                
032000         05  W-IDDISTR-A6F1-MIN       PIC S9(5) VALUE ZERO COMP-3.        
032100         05  W-IDKUNDNR-A6F1-MIN      PIC S9(7) VALUE ZERO COMP-3.        
032200         05  W-TIREGDAT-AVV9-A6F1-MIN PIC S9(7) VALUE ZERO COMP-3.        
032300         05  W-TIREGTID-AVV9-A6F1-MIN PIC S9(9) VALUE ZERO COMP-3.        
032400                                                                          
032500     03  W-WDA6F1KY-MAX-X.                                                
032600         05  W-IDDISTR-A6F1-MAX       PIC S9(5) VALUE ZERO COMP-3.        
032700         05  W-IDKUNDNR-A6F1-MAX      PIC S9(7) VALUE ZERO COMP-3.        
032800         05  W-TIREGDAT-AVV9-A6F1-MAX PIC S9(7) VALUE ZERO COMP-3.        
032900         05  W-TIREGTID-AVV9-A6F1-MAX PIC S9(9) VALUE ZERO COMP-3.        
033000                                                                          
033100     03  W-WDA6FKY-X.                                                     
033200         05  W-IDDISTR-A6F            PIC S9(5) VALUE ZERO COMP-3.        
033300         05  W-IDKUNDNR-A6F           PIC S9(7) VALUE ZERO COMP-3.        
033400         05  W-TIREGDAT-AVV9-A6F      PIC S9(7) VALUE ZERO COMP-3.        
033500         05  W-TIREGTID-AVV9-A6F      PIC S9(9) VALUE ZERO COMP-3.        
033600                                                                          
033700     03  W-IDHTYP-X.                                                      
033800         05  W-IDHTYP            PIC  X(4)   VALUE '4541'.                
033900         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
034000                                                                          
034100     03  W-IDGMT-X.                                                       
034200         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
034300         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
034400                                                                          
034500     03  W-WDB101KY-X.                                                    
034600         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
034700         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
034800                                                                          
034900     03  W-IDDC-B6-X.                                                     
035000         05 W-IDDC-B6                  PIC X(2).                          
035100                                                                          
035200*                                                                         
035300     EJECT                                                                
035400                                                                          
035500*    --- STATUS-KOD FRÅN IMS                                              
035600 01  STATUS-WS                   PIC XX.                                  
035700     88  SEGMENT-FINNS                       VALUE '  '.                  
035800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036000     88  BASEN-SLUT                          VALUE 'GB'.                  
036100     SKIP2                                                                
036200 01  GODK-STATUSKODER.                                                    
036300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036400                                                                          
036500 01  SSA1                        PIC X(96).                               
036600 01  SSA2                        PIC X(96).                               
036700     EJECT                                                                
036800*    --- IMS FUNKTIONSKODER                                               
036900*01  -COPY W0003                                                          
037000     EJECT                                                                
037100*    ---  DLI INPUT-OUTPUT AREA                                           
037200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
037300     SKIP3                                                                
037400 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
037500 01  DLI-IO-AREA-OBKR.                                                    
037600     03  WLORQM01.                                                        
037700*        05  -COPY WDQ101                                                 
037800     EJECT                                                                
037900 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
038000 01  DLI-IO-AREA-OHUV.                                                    
038100     03  WLORQI01.                                                        
038200*        05  -COPY WDQ201                                                 
038300     EJECT                                                                
038400 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
038500 01  DLI-IO-AREA-ARB.                                                     
038600     03  WLORQI12.                                                        
038700*        05  -COPY WDQ212                                                 
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
039000 01  DLI-IO-AREA-ORAD.                                                    
039100     03  WLORQF01.                                                        
039200*        05  -COPY WDQ401                                                 
039300     EJECT                                                                
039400 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
039500 01  DLI-IO-AREA-ART.                                                     
039600     03  WLARTM01.                                                        
039700*        05  -COPY WDK901                                                 
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
040000 01  DLI-IO-AREA-WDK7.                                                    
040100*    03  -COPY WDK711                                                     
040200     EJECT                                                                
040300                                                                          
040400 01  DLI-IO-AREA-VOR.                                                     
040500     03  WL454111.                                                        
040600*        05  -COPY WDGX4542                                               
040700     EJECT                                                                
040800 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
040900 01  DLI-IO-AREA-WDB201.                                                  
041000*    03  -COPY WDB201                                                     
041100     EJECT                                                                
041200 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
041300 01  DLI-IO-AREA-WDB101.                                                  
041400     03  WLBETC01.                                                        
041500         05  -COPY WDB101                                                 
041600     EJECT                                                                
041700 01  FILLER                      PIC X(16)   VALUE 'WDA601-AREA'.         
041800 01  DLI-IO-AREA-WDA601.                                                  
041900     03  WDA601.                                                          
042000*        05  -COPY WDA601                                                 
042100     EJECT                                                                
042200 01  FILLER                      PIC X(16)   VALUE 'WDA6F1-AREA'.         
042300 01  DLI-IO-AREA-WDA6F1.                                                  
042400     03  WDA6F1.                                                          
042500*        05  -COPY WDA6F1                                                 
042600     EJECT                                                                
042700 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
042800 01  DLI-IO-AREA-WDK611.                                                  
042900     03  WDK611.                                                          
043000*        05  -COPY WDK611                                                 
043100 01  FILLER                      PIC X(16)   VALUE 'WDK621-AREA'.         
043200 01  DLI-IO-AREA-WDK621.                                                  
043300     03  WDK621.                                                          
043400*        05  -COPY WDK621                                                 
043500                                                                          
043600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
043700 01   DLI-IO-AREA-B601.                                                   
043800*     03  -COPY WDB601                                                    
043900                                                                          
044000 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
044100 01   DLI-IO-AREA-R601.                                                   
044200*     03  -COPY WDR601                                                    
044300*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
044400     EJECT                                                                
044500     EJECT                                                                
044600                                                                          
044700*---MSG-AREA FÖR HOPP TILL 4233-SVARSBILDEN                               
044800                                                                          
044900 01  FILLER                  PIC X(16)  VALUE '4233-MSG-IO-AREA'.         
045000 01  4233-MSG-IO-AREA.                                                    
045100     03  4233-LL               PIC S9(4)  VALUE +32  COMP SYNC.           
045200     03  4233-Z1               PIC X.                                     
045300     03  4233-Z2               PIC X.                                     
045400     03  4233-TRANSKOD         PIC X(8)   VALUE 'W4T233  '.               
045500     03  4233-IDTRANS          PIC X(4)   VALUE '4232'.                   
045600     03  4233-SPRAK            PIC X.                                     
045700     03  4233-IDDISTR-IN       PIC X(4).                                  
045800     03  4233-IDKUNDNR-IN      PIC X(6).                                  
045900     03  4233-IDORDNR-IN       PIC X(5).                                  
046000     EJECT                                                                
046100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
046200     SKIP3                                                                
046300 01  -COPY WZ01SEND                                                       
046400     EJECT                                                                
046500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
046600     SKIP3                                                                
046700 01  SEND-AREA.                                                           
046800*    03  -COPY WZ01REQU  -PRE 3039-                                       
046900*    03  -COPY W30391I1  -PRE 3039-                                       
047000     EJECT                                                                
047100 LINKAGE SECTION.                                                         
047200                                                                          
047300*01  -COPY W0009   -PRE MSG-                                              
047400                                                                          
047500 01  AVSR-ALT-PCB                PIC X.                                   
047600     EJECT                                                                
047700                                                                          
047800*01  -COPY W0009   -PRE PRQRY-                                            
047900     EJECT                                                                
048000*01  -COPY W0009   -PRE 4233-                                             
048100     EJECT                                                                
048200*01  -COPY W0008   -PRE USEA-                                             
048300     05  FILLER                  PIC X.                                   
048400     SKIP2                                                                
048500*01  -COPY W0008   -PRE ORQM-                                             
048600     05  FILLER                  PIC X.                                   
048700     SKIP2                                                                
048800*01  -COPY W0008   -PRE ORQI-                                             
048900     05  FILLER                  PIC X.                                   
049000     EJECT                                                                
049100*01  -COPY W0008   -PRE ORQF-                                             
049200     05  FILLER                  PIC X.                                   
049300     EJECT                                                                
049400*01  -COPY W0008   -PRE ARTM-                                             
049500     05  FILLER                  PIC X.                                   
049600     EJECT                                                                
049700*01  -COPY W0008   -PRE WDK7-                                             
049800     05  FILLER                  PIC X.                                   
049900     EJECT                                                                
050000*01  -COPY W0008   -PRE 4541-                                             
050100     05  FILLER                  PIC X.                                   
050200     EJECT                                                                
050300*01  -COPY W0008   -PRE WDB2-                                             
050400     05  FILLER              PIC X.                                       
050500     EJECT                                                                
050600*01  -COPY W0008   -PRE WDB1-                                             
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008   -PRE WDB6-                                             
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008   -PRE WDK6-                                             
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008   -PRE WDA6F1-                                           
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008   -PRE WDA6F-                                            
051900     05  FILLER                  PIC X.                                   
052000*01  -COPY W0008   -PRE WDR6-                                             
052100     05  FILLER                  PIC X.                                   
052200     EJECT                                                                
052300     EJECT                                                                
052400 01  PRIS-ARTC-PCB               PIC X.                                   
052500 01  PRIS-WDK7-PCB               PIC X.                                   
052600 01  PRIS-GMTA-PCB               PIC X.                                   
052700 01  PRIS-BETA-PCB               PIC X.                                   
052800 01  PRIS-GPRIA-PCB              PIC X.                                   
052900 01  PRIS-GPRIB-PCB              PIC X.                                   
053000 01  PRIS-COST-WDK6-PCB          PIC X.                                   
053100 01  PRIS-COST-WDK7-PCB          PIC X.                                   
053200 01  PRIS-COST-WDF1-PCB          PIC X.                                   
053300 01  PRIS-COST-9305-PCB          PIC X.                                   
053400 01  PRIS-COST-WDK72-PCB         PIC X.                                   
053500 01  PRIS-COST-WDB6-PCB          PIC X.                                   
053600 01  PRNO-3107-PCB               PIC X.                                   
053700 01  PRQU-WDG2-PCB               PIC X.                                   
053800 01  PRQU-WDC7-PCB               PIC X.                                   
053900 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
054000 01  AREG-WDK6-PCB               PIC X.                                   
054100 01  AREG-WDK7-PCB               PIC X.                                   
054200 01  NDCA-USEA-PCB               PIC X.                                   
054300 01  NDCA-WDK7-PCB               PIC X.                                   
054400 01  NDCA-WDL6-PCB               PIC X.                                   
054500 01  NDCA-WDB6-PCB               PIC X.                                   
054600 01  SDCA-ARTS-PCB               PIC X.                                   
054700 01  SDCA-WDB6-PCB               PIC X.                                   
054800 01  SDCA-WDK9-PCB               PIC X.                                   
054900 01  SDCA-WDR6-PCB               PIC X.                                   
055000 01  SDCA-WDK6-PCB               PIC X.                                   
055100 01  SDCA-WDQ4B-PCB              PIC X.                                   
055200 01  SDCA-WDQ2-PCB               PIC X.                                   
055300 01  SDCA-WDQ4-PCB               PIC X.                                   
055400 01  SDCA-WDB6-2-PCB             PIC X.                                   
055500 01  SDCA-WDK6-2-PCB             PIC X.                                   
055600 01  SDCA-WDK7-2-PCB             PIC X.                                   
055700 01  SDCA-WDK7-3-PCB             PIC X.                                   
055800 01  AVSR-ORQI-PCB               PIC X.                                   
055900 01  AVSR-GMTB-PCB               PIC X.                                   
056000 01  AVSR-GMTC-PCB               PIC X.                                   
056100 01  AVSR-WDB2-PCB            PIC X.                                      
056200 01  AVSR-WDB6-PCB            PIC X.                                      
056300 01  TRAN-XXKB-PCB               PIC X.                                   
056400 01  SPAR-WDF8-PCB               PIC X.                                   
056500 01  SPAR-WDF8A-PCB              PIC X.                                   
056600 01  SPAR-WDK6-PCB               PIC X.                                   
056700 01  XDCA-USEA-PCB               PIC X.                                   
056800 01  XDCA-WDB6-PCB               PIC X.                                   
056900 01  XDCA-WDK6-PCB               PIC X.                                   
057000 01  XDCA-WDK7-PCB               PIC X.                                   
057100 01  XDCA-WDK9-PCB               PIC X.                                   
057200 01  XDCA-WDL6-PCB               PIC X.                                   
057300 01  XDCA-WDQ4B-PCB              PIC X.                                   
057400 01  XDCA-WDQ2-PCB               PIC X.                                   
057500 01  XDCA-WDQ4-PCB               PIC X.                                   
057600 01  XDCA-WDR6-PCB               PIC X.                                   
057700 01  XDCA-WDB6-2-PCB             PIC X.                                   
057800 01  XDCA-WDK6-2-PCB             PIC X.                                   
057900 01  XDCA-WDK7-2-PCB             PIC X.                                   
058000 01  XDCA-WDK7-3-PCB             PIC X.                                   
058100     EJECT                                                                
058200                                                                          
058300 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB PRQRY-PCB 4233-PCB        
058400        USEA-PCB ORQM-PCB ORQI-PCB ORQF-PCB                               
058500        ARTM-PCB WDK7-PCB 4541-PCB                                        
058600        WDB2-PCB WDB1-PCB WDB6-PCB WDK6-PCB WDA6F1-PCB WDA6F-PCB          
058700        WDR6-PCB                                                          
058800        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
058900        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
059000        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
059100        PRIS-COST-WDK6-PCB                                                
059200        PRIS-COST-WDK7-PCB                                                
059300        PRIS-COST-WDF1-PCB                                                
059400        PRIS-COST-9305-PCB                                                
059500        PRIS-COST-WDK72-PCB                                               
059600        PRIS-COST-WDB6-PCB                                                
059700        PRNO-3107-PCB                                                     
059800        PRQU-WDG2-PCB                                                     
059900        PRQU-WDC7-PCB                                                     
060000        PRQU-SJKO-WDK6-PCB                                                
060100        AREG-WDK6-PCB                                                     
060200        AREG-WDK7-PCB                                                     
060300        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
060400        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
060500        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
060600        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
060700        SDCA-WDK7-3-PCB                                                   
060800        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
060900        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
061000        TRAN-XXKB-PCB                                                     
061100        SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB                        
061200        XDCA-USEA-PCB                                                     
061300        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
061400        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
061500        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
061600        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
061700        XDCA-WDK7-3-PCB                                                   
061800        .                                                                 
061900                                                                          
062000     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB PRQRY-PCB 4233-PCB        
062100        USEA-PCB ORQM-PCB ORQI-PCB ORQF-PCB                               
062200        ARTM-PCB WDK7-PCB 4541-PCB                                        
062300        WDB2-PCB WDB1-PCB WDB6-PCB WDK6-PCB WDA6F1-PCB WDA6F-PCB          
062400        WDR6-PCB                                                          
062500        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
062600        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
062700        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
062800        PRIS-COST-WDK6-PCB                                                
062900        PRIS-COST-WDK7-PCB                                                
063000        PRIS-COST-WDF1-PCB                                                
063100        PRIS-COST-9305-PCB                                                
063200        PRIS-COST-WDK72-PCB                                               
063300        PRIS-COST-WDB6-PCB                                                
063400        PRNO-3107-PCB                                                     
063500        PRQU-WDG2-PCB                                                     
063600        PRQU-WDC7-PCB                                                     
063700        PRQU-SJKO-WDK6-PCB                                                
063800        AREG-WDK6-PCB                                                     
063900        AREG-WDK7-PCB                                                     
064000        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
064100        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
064200        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
064300        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
064400        SDCA-WDK7-3-PCB                                                   
064500        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
064600        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
064700        TRAN-XXKB-PCB                                                     
064800        SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB                        
064900        XDCA-USEA-PCB                                                     
065000        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
065100        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
065200        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
065300        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
065400        XDCA-WDK7-3-PCB                                                   
065500        .                                                                 
065600     EJECT                                                                
065700                                                                          
065800     PERFORM IMS-GET-MSG                                                  
065900     IF SEGMENT-FINNS                                                     
066100        PERFORM A-INIT                                                    
066200        PERFORM B-KOLLA-NYCKLAR                                           
066300        IF ALLT-OK                                                        
066400           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
066500           IF ALLT-OK                                                     
066600              IF OHUV-FLVORKO = YES                                       
066700                 PERFORM I-LAES-IN-4541-WDR4                              
066800              ELSE                                                        
066900                 IF OHUV-FLVORKO = JA                                     
067000                     PERFORM K-LAES-FRAN-NYVORKO                          
067100                 END-IF                                                   
067200              END-IF                                                      
067300              PERFORM D-FORMELL-KONTROLL                                  
067400              IF ALLT-OK                                                  
067500                 PERFORM E-BEHANDLA-RADER                                 
067600              END-IF                                                      
067700           END-IF                                                         
067800        END-IF                                                            
067900                                                                          
068000        IF ALLT-OK                                                        
068100           IF MID-IDARTNR-006(WS-INDEX-MID-MAX) = ALL '+'                 
068200              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
068300                PERFORM H-SKICKA-PRISFRAGA                                
068400              END-IF                                                      
068500              PERFORM F-HOPPA-TILL-SVARSBILD                              
068600           ELSE                                                           
068700              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
068800                PERFORM H-SKICKA-PRISFRAGA                                
068900              END-IF                                                      
069000              PERFORM G-VISA-TOM-SIDA                                     
069100           END-IF                                                         
069200        END-IF                                                            
069300        IF HOPP-TILL-4233 = NEJ                                           
069400        AND HOPP-TILL-0504     = NEJ                                      
069500           PERFORM Z-FINIT-INSERT-MSG                                     
069600        END-IF                                                            
069700     END-IF                                                               
069800     MOVE +0 TO RETURN-CODE                                               
069900     GOBACK                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 A-INIT SECTION.                                                          
070300                                                                          
070400     MOVE SPACE                TO MED-IDMFSFEL                            
070500                                                                          
070600     IF MSG-DUBBLA-TRANSKODER                                             
070700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I23201-CTX             
070800       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
070900       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
071000     ELSE                                                                 
071100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I23201-CTX              
071200       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
071300       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
071400     END-IF                                                               
071500                                                                          
071600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
071700     MOVE MSG-IDPFK            TO MFS-IDPFK                               
071800     MOVE MFS-IDTRANS          TO W-IDTRANS                               
071900     MOVE LOW-VALUE            TO MSG-AREA                                
072000     MOVE 'W4O23201'           TO MFS-IDMOD                               
072100     MOVE '4232'               TO MOD-IDTRANS                             
072200     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
072300                                  MOD-TEMFSINF                            
072400                                                                          
072500     IF NOT EGEN-MID                                                      
072600       MOVE SPACE              TO MFS-KDTRTYP                             
072700       MOVE '7'                TO MFS-IDPFK                               
072800     END-IF                                                               
072900                                                                          
073100     IF NOT GODK-MID                                                      
073300       PERFORM S20-WRONG-PICTURE-MESSAGE                                  
073400     END-IF                                                               
073500                                                                          
073600     IF ENGLISH-TEXT                                                      
073700       MOVE +2                 TO SPRAK-IX                                
073800       MOVE 'GB '              TO MED-IDSKYLT                             
073900     ELSE                                                                 
074000       MOVE +1                 TO SPRAK-IX                                
074100       MOVE 'S  '              TO MED-IDSKYLT                             
074200     END-IF                                                               
074300                                                                          
074400     PERFORM AA-NOLLA-WOPS-TABELL                                         
074500     .                                                                    
074600     EJECT                                                                
074700 AA-NOLLA-WOPS-TABELL SECTION.                                            
074800                                                                          
074900     MOVE +1                   TO WS-INDEX-WOPS                           
075000     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
075100        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
075200        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
075300        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
075400        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
075500        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
075600        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
075700        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
075800        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
075900        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
076000        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
076100        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
076200        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
076300                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
076400                                                                          
076500        ADD +1                 TO WS-INDEX-WOPS                           
076600     END-PERFORM                                                          
076700                                                                          
076800     MOVE +1                   TO WS-INDEX-WOPS                           
076900     .                                                                    
077000     EJECT                                                                
077100 B-KOLLA-NYCKLAR SECTION.                                                 
077200                                                                          
077300     MOVE MID-IDDISTR        TO WS-IDDISTR                                
077400     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
077500                                                                          
077600     MOVE MID-IDKUNDNR      TO WS-IDKUNDNR                                
077700     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
077800                                                                          
077900     MOVE MID-IDORDNR5      TO WS-IDORDNR                                 
078000     INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                   
078100                                                                          
078200     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
078300        MOVE WS-IDDISTR           TO W-IDDISTR                            
078400                                     TEST-IDDISTR                         
078500     ELSE                                                                 
078600        MOVE NEJ                  TO ALLT-SW                              
078700        MOVE ZERO                 TO WS-IDDISTR                           
078800     END-IF                                                               
078900                                                                          
079000     IF DIST79-DEALER-PRICE                                               
079100        IF ENGLISH-TEXT                                                   
079200           MOVE 'DEALERPRICE'     TO MOD-TEDDI                            
079300        ELSE                                                              
079400           MOVE '    ÅF PRIS'     TO MOD-TEDDI                            
079500        END-IF                                                            
079600     ELSE                                                                 
079700        MOVE SPACES               TO MOD-TEDDI                            
079800     END-IF                                                               
079900                                                                          
080000     IF WS-IDKUNDNR NUMERIC                                               
080100        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
080200     ELSE                                                                 
080300        MOVE NEJ                  TO ALLT-SW                              
080400     END-IF                                                               
080500                                                                          
080600     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
080700        MOVE WS-IDORDNR           TO WS-NUM-7                             
080800        MOVE WS-NUM-7             TO W-IDKUNDRF                           
080900     ELSE                                                                 
081000        MOVE NEJ                  TO ALLT-SW                              
081100     END-IF                                                               
081200                                                                          
081300     IF NOT ALLT-OK                                                       
081400        MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                         
081500     END-IF                                                               
081600     EJECT                                                                
081700                                                                          
081800     IF GODK-MID OR ALLT-OK                                               
081900       MOVE WS-IDDISTR              TO MOD-IDDISTR                        
082000       INSPECT MOD-IDDISTR REPLACING LEADING ZERO BY SPACE                
082100                                                                          
082200       IF WS-IDKUNDNR = ZERO                                              
082300         MOVE '     0'              TO MOD-IDKUNDNR                       
082400       ELSE                                                               
082500         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR                       
082600         INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE             
082700       END-IF                                                             
082800                                                                          
082900       MOVE WS-IDORDNR              TO MOD-IDORDNR5                       
083000       INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
083100                                                                          
083200       IF W-IDTRANS = '4231' AND NOT ALLT-OK                              
083300          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR                        
083400                                       MOD-IDKUNDNR                       
083500                                       MOD-IDORDNR5                       
083600       END-IF                                                             
083700                                                                          
083800     ELSE                                                                 
083900       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR                        
084000                                       MOD-IDKUNDNR                       
084100                                       MOD-IDORDNR5                       
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
084600     PERFORM IMS-01-GU-ORQI-WDQ201                                        
084700     IF SEGMENT-FINNS                                                     
084800       MOVE OHUV-IDDC-TVS            TO W-IDDC-B6                         
084900       PERFORM IMS-GU-WDB601                                              
085000*      EFTERSOM VI HAR TVÅNGSSTYRNING                                     
085100*      BEHÖVS INGEN YTTERLIGARE LÄSNING                                   
085200                                                                          
085300       IF OHUV-FLKLAR = JA                                                
085400         MOVE ERR-ORDER-AVSLUTAD     TO MED-IDMFSFEL                      
085500         MOVE NEJ                    TO ALLT-SW                           
085600       ELSE                                                               
085700         IF OHUV-IDSYSTEM NOT = '4231'                                    
085800           MOVE ERR-FEL-BILDSERIE    TO MED-IDMFSFEL                      
085900           MOVE NEJ                  TO ALLT-SW                           
086000         ELSE                                                             
086100           IF OHUV-IDUSER NOT = MSG-SIGNON-USERID                         
086200              MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                      
086300              MOVE NEJ               TO ALLT-SW                           
086400           ELSE                                                           
086500              MOVE OHUV-KDORDKL      TO MOD-KDORDKL                       
086600              MOVE OHUV-IDDC-TVS TO W-IDDC                                
086700                                                                          
086800              PERFORM IMS-03-GNP-ORQI-WDQ212                              
086900              MOVE ARB-KDFRAKT       TO MOD-KDFRAKT                       
087000              PERFORM CA-HAMTA-KUND                                       
087100              PERFORM CB-FIXA-LOKAL-TID                                   
087200           END-IF                                                         
087300         END-IF                                                           
087400       END-IF                                                             
087500     ELSE                                                                 
087600        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
087700        MOVE NEJ                     TO ALLT-SW                           
087800     END-IF                                                               
087900     MOVE SPACE                     TO MOD-KDVALISO                       
088000                                                                          
088100     IF   MFS-FIRST AND ALLT-OK                                           
088200     AND  OHUV-FLVORKO NOT = JA                                           
088300     AND  OHUV-FLVORKO NOT = YES                                          
088400        MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                
088500        PERFORM MFS-RENSA-MOD-RADER                                       
088600        MOVE NEJ                    TO ALLT-SW                            
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 CA-HAMTA-KUND      SECTION.                                              
089100     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
089200     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
089300     PERFORM IMS-GU-WDB201                                                
089400                                                                          
089500     IF OHUV-KDORDKL > 1                                                  
089600                                                                          
089700        MOVE +1 TO WS-INDEX                                               
089800        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
089900           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
090000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
090100           ADD +1 TO WS-INDEX                                             
090200        END-PERFORM                                                       
090300                                                                          
090400     ELSE                                                                 
090500       IF OHUV-KDORDKL = 1                                                
090600                                                                          
090700          MOVE +1 TO WS-INDEX                                             
090800          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
090900             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
091000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
091100             ADD +1 TO WS-INDEX                                           
091200          END-PERFORM                                                     
091300                                                                          
091400       ELSE                                                               
091500         IF OHUV-KDORDKL = 0                                              
091600                                                                          
091700            MOVE +1 TO WS-INDEX                                           
091800            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
091900               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
092000                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
092100               ADD +1 TO WS-INDEX                                         
092200            END-PERFORM                                                   
092300                                                                          
092400         END-IF                                                           
092500       END-IF                                                             
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900                                                                          
093000 CB-FIXA-LOKAL-TID SECTION.                                               
093100                                                                          
093200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
093300     MOVE '013'             TO MSGI-KDCALL                                
093400     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
093500     MOVE OHUV-IDDC-TVS     TO MSGI-IDUSER (6:2)                          
093600     MOVE '4232'            TO MSGI-IDTRANS                               
093700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
093800                                                                          
093900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
094000     .                                                                    
094100     EJECT                                                                
094200                                                                          
094300 D-FORMELL-KONTROLL SECTION.                                              
094400                                                                          
094500     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
094600     MOVE OHUV-IDDC-TVS              TO ORFK-IDDC                         
094700     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
094800     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
094900     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
095000     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
095100     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
095200     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
095300     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
095400     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
095500     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
095600     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
095700     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
095800                                                                          
095900     MOVE +1                   TO WS-INDEX-MID                            
096000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
096100        IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                           
096200           MOVE NEJ            TO ORFK-FLINVEST(WS-INDEX-MID)             
096300        ELSE                                                              
096400           IF MID-FLINVEST(WS-INDEX-MID) = 'Y'                            
096500              MOVE JA          TO MID-FLINVEST(WS-INDEX-MID)              
096600           END-IF                                                         
096700           MOVE MID-FLINVEST(WS-INDEX-MID)                                
096800                               TO ORFK-FLINVEST(WS-INDEX-MID)             
096900        END-IF                                                            
097000        MOVE OHUV-FLRESTN      TO ORFK-FLRESTN(WS-INDEX-MID)              
097100                                                                          
097200        MOVE MID-IDARTNR-006(WS-INDEX-MID)                                
097300                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
097400                                                                          
097500        MOVE ALL '+'           TO ORFK-KDKVBRYT(WS-INDEX-MID)             
097600        MOVE OHUV-KDVRINFO     TO ORFK-KDVRINFO(WS-INDEX-MID)             
097700                                                                          
097800        MOVE MID-KVBEART(WS-INDEX-MID)                                    
097900                               TO ORFK-KVBEART(WS-INDEX-MID)              
098000        IF DIST79-DEALER-PRICE                                            
098100           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
098200                               TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
098300           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)             
098400           MOVE ALL '+'        TO                                         
098500                              ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)         
098600        ELSE                                                              
098700           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
098800                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
098900           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
099000           MOVE ALL '+'        TO                                         
099100                              ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)         
099200        END-IF                                                            
099300        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
099400        MOVE ALL '+'           TO ORFK-TITPO-RAD(WS-INDEX-MID)            
099500                                  ORFK-FLSLATT(WS-INDEX-MID)              
099600        ADD +1                 TO WS-INDEX-MID                            
099700     END-PERFORM                                                          
099800                                                                          
099900     IF OHUV-IDLEVNR-EJLS NOT = SPACE                                     
100000        MOVE OHUV-IDLEVNR-EJLS TO W-IDLEVNR                               
100100*       --- FÖR ORDER MED LAGERAVBOK=N, PLUS INMATAT LEVNR-EJLS           
100200*       --- KOLLA ATT ALLA ARTIKLARNA HAR ETT GODK. PRIS MOT LEV.         
100300*       ---             (ETRACKER 5147148)                                
100400        MOVE +1 TO WS-INDEX-MID                                           
100500        PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                     
100600           IF MID-IDARTNR-006(WS-INDEX-MID) NOT = ALL '+'                 
100700           AND MID-IDARTNR-006(WS-INDEX-MID) NUMERIC                      
100800              PERFORM DB-KOLLA-IDLEVNR-EJLS                               
100900           END-IF                                                         
101000           ADD +1 TO WS-INDEX-MID                                         
101100        END-PERFORM                                                       
101200     END-IF                                                               
101300                                                                          
101400     IF ALLT-OK                                                           
101500*       --- ÖVRIG FORMELL KONTROLL                                        
101600        CALL W411ORFK USING ORFK-W411ORFK                                 
101700                            AREG-WDK6-PCB                                 
101800                            AREG-WDK7-PCB                                 
101900                                                                          
102000        MOVE +1                TO WS-INDEX-MID                            
102100        PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                     
102200           PERFORM DA-KOLLA-FEL-FK                                        
102300           ADD +1              TO WS-INDEX-MID                            
102400        END-PERFORM                                                       
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800 DA-KOLLA-FEL-FK SECTION.                                                 
102900                                                                          
103000     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
103100        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
103200        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)        
103300        MOVE NEJ                 TO ALLT-SW                               
103400     END-IF                                                               
103500                                                                          
103600     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
103700        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
103800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
103900        MOVE NEJ                 TO ALLT-SW                               
104000     ELSE                                                                 
104100        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
104200                  AND NOT MFS-UPDATE                                      
104300          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
104400          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
104500          MOVE NEJ               TO ALLT-SW                               
104600        END-IF                                                            
104700     END-IF                                                               
104800                                                                          
104900     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
105000        IF MED-IDMFSFEL = SPACE                                           
105100           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC AND                      
105200                   ORFK-KVBEART(WS-INDEX-MID) > ZERO                      
105300              IF NOT MFS-UPDATE                                           
105400                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
105500                 MOVE MFS-NUM-FAELT-FEL   TO                              
105600                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
105700                 MOVE NEJ             TO ALLT-SW                          
105800              END-IF                                                      
105900           ELSE                                                           
106000            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
106100            MOVE MFS-NUM-FAELT-FEL   TO                                   
106200                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
106300            MOVE NEJ                 TO ALLT-SW                           
106400           END-IF                                                         
106500        ELSE                                                              
106600           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
106700           MOVE MFS-NUM-FAELT-FEL   TO                                    
106800                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
106900           MOVE NEJ                 TO ALLT-SW                            
107000        END-IF                                                            
107100     END-IF                                                               
107200                                                                          
107300     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
107400        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
107500        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
107600        MOVE NEJ               TO ALLT-SW                                 
107700     END-IF                                                               
107800     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
107900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
108000        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
108100        MOVE NEJ                 TO ALLT-SW                               
108200     END-IF                                                               
108300     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
108400        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
108500        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
108600        MOVE NEJ                 TO ALLT-SW                               
108700     END-IF                                                               
108800     .                                                                    
108900     EJECT                                                                
109000                                                                          
109100                                                                          
109200 DB-KOLLA-IDLEVNR-EJLS    SECTION.                                        
109300     SKIP2                                                                
109400     MOVE MID-IDARTNR-006(WS-INDEX-MID) TO W-IDARTNR                      
109500     PERFORM IMS-GU-WDK611                                                
109600     IF SEGMENT-FINNS                                                     
109700        PERFORM IMS-GNP-WDK621                                            
109800        IF SEGMENT-SAKNAS                                                 
109900*          --- PRIS-SEGMENT SAKNAS FÖR INMATAD LEVERANTÖR                 
110000           MOVE NEJ TO ALLT-SW                                            
110100           MOVE ERR-ORDERRADER-AVBRYTS TO MED-IDMFSFEL                    
110200           MOVE INF-UPPDATERA-LEV      TO MED-IDMFSINF                    
110300           MOVE MFS-NUM-FAELT-FEL                                         
110400                         TO MOD-IDARTNR-ATTR(WS-INDEX-MID)                
110500        END-IF                                                            
110600     ELSE                                                                 
110700*       --- PRIS-SEGMENT SAKNAS                                           
110800        MOVE NEJ TO ALLT-SW                                               
110900        MOVE ERR-ORDERRADER-AVBRYTS TO MED-IDMFSFEL                       
111000        MOVE INF-UPPDATERA-LEV      TO MED-IDMFSINF                       
111100        MOVE MFS-NUM-FAELT-FEL                                            
111200                        TO MOD-IDARTNR-ATTR(WS-INDEX-MID)                 
111300     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
111600                                                                          
111700 E-BEHANDLA-RADER SECTION.                                                
111800                                                                          
111900     MOVE NEJ                 TO OBKR-SW                                  
112000     MOVE +1                  TO WS-INDEX-MID                             
112100     MOVE +0                  TO WS-IDPRQUES                              
112200                                                                          
112300     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
112400        IF MID-IDARTNR-006(WS-INDEX-MID) NOT = ALL '+'                    
112500           MOVE ORFK-W411AREG-001(WS-INDEX-MID) TO                        
112600                         AREG-W411AREG-001                                
112700           PERFORM EC-BEHANDLA-RAD                                        
112800           MOVE NEJ            TO OBKR-SW                                 
112900        END-IF                                                            
113000                                                                          
113100        ADD +1                 TO WS-INDEX-MID                            
113200     END-PERFORM                                                          
113300                                                                          
113400     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
113500       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
113600       MOVE +3                      TO PRNO-KDCALL                        
113700                                                                          
113800       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
113900     END-IF                                                               
114000                                                                          
114100     IF AVSR-IDDC(1) > ZERO                                               
114200        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                    
114300             AVSR-ORQI-PCB  AVSR-GMTB-PCB AVSR-GMTC-PCB                   
114400             AVSR-WDB2-PCB  AVSR-WDB6-PCB                                 
114500             TRAN-XXKB-PCB                                                
114600     END-IF                                                               
114700     MOVE JA                      TO ALLT-SW                              
114800     .                                                                    
114900     EJECT                                                                
115000                                                                          
115100 EC-BEHANDLA-RAD SECTION.                                                 
115200                                                                          
115300     PERFORM ECA-NOLLSTALL-OBKR                                           
115400     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
115500     PERFORM ECJ-KOMPLETTERA-SPARRAR                                      
115600     PERFORM ECF-KOMPLETTERA-PRIS                                         
115700     PERFORM ECE-KOLLA-XDC-RAD                                            
115800     PERFORM ECK-KOLLA-SDC-RAD                                            
115900                                                                          
116000     IF SKRIV-OBKR                                                        
116100        PERFORM ECG-SKRIV-OBKR                                            
116200     ELSE                                                                 
116300                                                                          
116400        PERFORM ECH-KONTROLLERA-ENHETSLAST                                
116500        PERFORM ECI-BERAKNA-WOPS-SKRIV-ORAD                               
116600     END-IF                                                               
116700     .                                                                    
116800     EJECT                                                                
116900                                                                          
117000 ECA-NOLLSTALL-OBKR SECTION.                                              
117100     SKIP2                                                                
117200     MOVE JA                   TO ALLT-SW                                 
117300     IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                                
117400        MOVE NEJ               TO ALLT-SW                                 
117500        MOVE JA                TO OBKR-SW                                 
117600     END-IF                                                               
117700     MOVE ZERO                 TO SPAR-KDORDBEK                           
117800                                  XDCA-KDORDBEK                           
117900*                                 NDCA-KDORDBEK                           
118000                                  SDCA-KDORDBEK                           
118100     .                                                                    
118200     EJECT                                                                
118300                                                                          
118400 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
118500     SKIP2                                                                
118600     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
118700     MOVE OHUV-IDDC-TVS        TO ORAD-IDDC                               
118800                                                                          
118900     IF DCS-CDC                                                           
119000        MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                           
119100        MOVE AREG-ADGANG       TO ORAD-ADGANG                             
119200        MOVE AREG-ADPLATS      TO ORAD-ADPLATS                            
119300     ELSE                                                                 
119400        MOVE +0                TO ORAD-ADLAGOMR                           
119500                                  ORAD-ADGANG                             
119600                                  ORAD-ADPLATS                            
119700     END-IF                                                               
119800     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
119900                               TO ORAD-IDARTNR                            
120000     MOVE +1                   TO ORAD-IDLOPNR                            
120100                                                                          
120200     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
120300        MOVE SPACE             TO ORAD-BERADREF                           
120400     ELSE                                                                 
120500        MOVE MID-BERADREF(WS-INDEX-MID)                                   
120600                               TO ORAD-BERADREF                           
120700     END-IF                                                               
120800     MOVE SPACE                TO ORAD-BEVOLREF                           
120900     MOVE SPACE                TO ORAD-FLAKPLOC                           
121000     MOVE NEJ                  TO ORAD-FLSDCLEV                           
121100     IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                              
121200        MOVE NEJ               TO ORAD-FLINVEST                           
121300     ELSE                                                                 
121400        MOVE MID-FLINVEST(WS-INDEX-MID)                                   
121500                               TO ORAD-FLINVEST                           
121600     END-IF                                                               
121700     MOVE JA                   TO ORAD-FLOBTRAN                           
121800     MOVE NEJ                  TO ORAD-FLPRTILL                           
121900     MOVE OHUV-FLRESTN         TO ORAD-FLRESTN                            
122000     MOVE NEJ                  TO ORAD-FLTILLK                            
122100     MOVE SPACE                TO ORAD-IDDC-RO                            
122200     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
122300     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
122400     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
122500*    IF OHUV-FLLSBOK = 'N'                                                
122600*       --- MAN SKA KOLLA INMATAT OBLIGATORISKT LEVNR                     
122700*       --- PÅ ORDERRADEN PÅ  WDK621                                      
122800      MOVE SPACE               TO ORAD-IDLEVNR                            
122900*    END-IF                                                               
123000     MOVE +0                   TO ORAD-IDLOPNR-RO                         
123100     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
123200                                                                          
123300     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
123400     MOVE ZERO                 TO ORAD-IDSPECEMB                          
123500     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
123600     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
123700     MOVE OHUV-KDVRINFO        TO ORAD-KDDSP                              
123800     IF ORAD-KDDSP = +1 OR +2                                             
123900        MOVE +2                TO ORAD-KDDSP                              
124000     ELSE                                                                 
124100        MOVE +1                TO ORAD-KDDSP                              
124200     END-IF                                                               
124300     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
124400     IF OHUV-FLVORKO = JA                                                 
124500     OR OHUV-FLVORKO = YES                                                
124600       MOVE +2                   TO ORAD-KDKVBRYT                         
124700     ELSE                                                                 
124800       MOVE +0                   TO ORAD-KDKVBRYT                         
124900     END-IF                                                               
125000     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
125100     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
125200     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
125300     IF ORAD-KDORDING = +3                                                
125400       MOVE SPACE              TO ORAD-KDOI                               
125500     ELSE                                                                 
125600       IF ORAD-KDPRODSL = +19 OR +29 OR ORAD-KDORDING = +1                
125700         MOVE 'CD'             TO ORAD-KDOI                               
125800       ELSE                                                               
125900         MOVE 'DT'             TO ORAD-KDOI                               
126000       END-IF                                                             
126100     END-IF                                                               
126200     MOVE SPACE                TO ORAD-CLEARGROUP                         
126300     MOVE SPACE                TO ORAD-KDPRTYP                            
126400     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
126500     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
126600        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
126700     ELSE                                                                 
126800        MOVE +0                TO ORAD-KDTPOTYP                           
126900     END-IF                                                               
127000     MOVE JA                   TO ORAD-FLORDING                           
127100     EJECT                                                                
127200     MOVE ORFK-KDVRINFO(WS-INDEX-MID)                                     
127300                               TO ORAD-KDVRINFO                           
127400     MOVE ORFK-KVBEART(WS-INDEX-MID)                                      
127500                               TO WS-ALFA-6                               
127600     MOVE WS-NUM-6             TO ORAD-KVBEART                            
127700                                  ORAD-KVBEART-Q                          
127800     MOVE +0                   TO ORAD-KVPREAVB                           
127900     MOVE +0                   TO ORAD-KVPRERO                            
128000     MOVE +0                   TO ORAD-KVOKS-PREL                         
128100     MOVE +0                   TO ORAD-IDPRQUES                           
128200     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
128300     MOVE +0                   TO ORAD-RERAB                              
128400     MOVE SPACE                TO ORAD-KDVALISO                           
128500     MOVE SPACE                TO ORAD-KDVAT                              
128600     MOVE SPACE                TO ORAD-KDRAB                              
128700     MOVE SPACE                TO ORAD-BEART-VIPS                         
128800                                                                          
128900     IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                              
129000        MOVE +0                TO ORAD-PRARTNTO                           
129100                                  ORAD-PRARTNTO-LOC                       
129200                                  ORAD-PRARTNTO-LOCPREL                   
129300     ELSE                                                                 
129400        IF DIST79-DEALER-PRICE                                            
129500          MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                         
129600                               TO ORAD-PRARTNTO-LOC                       
129700          MOVE +0              TO ORAD-PRARTNTO-LOCPREL                   
129800                                  ORAD-PRARTNTO                           
129900        ELSE                                                              
130000          MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                             
130100                               TO ORAD-PRARTNTO                           
130200          MOVE +0              TO ORAD-PRARTNTO-LOC                       
130300          MOVE +0              TO ORAD-PRARTNTO-LOCPREL                   
130400        END-IF                                                            
130500     END-IF                                                               
130600     MOVE +0                   TO ORAD-PRBPRIS                            
130700     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
130800        MOVE MID-IDARTNR-006(WS-INDEX-MID)                                
130900                               TO WS-IDARTNR                              
131000        MOVE WS-IDARTNR-11     TO ORAD-REKSIFFR                           
131100     ELSE                                                                 
131200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
131300     END-IF                                                               
131400     MOVE +0                   TO ORAD-RERF-RAD                           
131500     MOVE +0                   TO ORAD-KVSLATT                            
131600     MOVE +0                   TO ORAD-TIPRIS                             
131700                                                                          
131800     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
131900     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
132000     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
132100     MOVE +0                   TO ORAD-TIRODAT                            
132200     MOVE OHUV-TITPO           TO ORAD-TITPO                              
132300     MOVE AREG-VKART           TO ORAD-VKART                              
132400     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
132500     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
132600     MOVE SPACE                TO ORAD-IDBIL                              
132700                                  ORAD-IDKLIENT                           
132800                                  ORAD-IDARBREF                           
132900                                  ORAD-IDVIN                              
133000                                                                          
133100     IF ORAD-KDORDKL = 1 AND                                              
133200        GMT-FLLDCKND = JA                                                 
133300        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
133400     ELSE                                                                 
133500        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
133600     END-IF                                                               
133700     MOVE +0                   TO ORAD-PRAVCOST                           
133800     .                                                                    
133900     EJECT                                                                
134000 ECJ-KOMPLETTERA-SPARRAR SECTION.                                         
134100                                                                          
134200     IF ALLT-OK                                                           
134300                                                                          
134400     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
134500     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
134600     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
134700     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
134800     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
134900     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
135000     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
135100     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
135200     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
135300     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
135400     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
135500     MOVE AREG-FLIART          TO SPAR-FLIART                             
135600     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
135700     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
135800     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
135900     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
136000     MOVE ORAD-IDDC            TO SPAR-IDDC                               
136100*SOFT OHUVIDSYSTEM TILL SPAR FÖR ATT SPÄRRA PIE ARTIKLAR                  
136200     MOVE OHUV-IDSYSTEM        TO SPAR-IDSYSTEM                           
136300     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
136400     MOVE AREG-KDERS           TO SPAR-KDERS                              
136500     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
136600     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
136700     MOVE +1                   TO SPAR-KDORDBEH                           
136800     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
136900     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
137000     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
137100     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
137200     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
137300     MOVE AREG-KDUART          TO SPAR-KDUART                             
137400     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
137500     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
137600     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
137700     MOVE ORAD-TITPO           TO SPAR-TITPO                              
137800     MOVE NEJ                  TO SPAR-FLSDCLEV                           
137900     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
138000                                                                          
138100     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
138200                                       SPAR-WDF8A-PCB                     
138300                                       SPAR-WDK6-PCB                      
138400                                                                          
138500     IF SPAR-KDORDBEK > +0                                                
138600        MOVE JA                     TO OBKR-SW                            
138700        MOVE NEJ                    TO ALLT-SW                            
138800     END-IF                                                               
138900     IF (OHUV-BEKUNDRF = 'SOFTWARE' AND AREG-KDSORT NOT = 'SW')           
139000     OR (OHUV-BEKUNDRF NOT = 'SOFTWARE' AND AREG-KDSORT = 'SW')           
139100        MOVE 67                     TO SPAR-KDORDBEK                      
139200        MOVE JA                     TO OBKR-SW                            
139300        MOVE NEJ                    TO ALLT-SW                            
139400     END-IF                                                               
139500*    COMMENTED AS NOT USED ANYMORE                                        
139600*    IF SPAR-KDORDBEK = 0 AND AREG-ADLAGOMR = 79                          
139700*       AND ORAD-IDDC = WC-CDC-SE                                         
139800*       MOVE OHUV-IDDISTR     TO TEST-IDDISTR                             
139900*       IF DIST18-SKROT                                                   
140000*         CONTINUE                                                        
140100*       ELSE                                                              
140200*         MOVE 26             TO SPAR-KDORDBEK                            
140300*         MOVE JA             TO OBKR-SW                                  
140400*       END-IF                                                            
140500*    END-IF                                                               
140600                                                                          
140700     END-IF                                                               
140800     .                                                                    
140900     EJECT                                                                
141000                                                                          
141100 ECF-KOMPLETTERA-PRIS SECTION.                                            
141200     SKIP2                                                                
141300     IF ALLT-OK                                                           
141400                                                                          
141500     IF DIST79-DEALER-PRICE                                               
141600       IF WS-IDPRQUES                = +0                                 
141700          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
141800          MOVE +1                    TO PRNO-KDCALL                       
141900                                                                          
142000          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
142100                                                                          
142200          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
142300                                        WS-IDPRQUES                       
142400          MOVE +1                    TO PRQU-KDCALL                       
142500       ELSE                                                               
142600          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
142700          MOVE +2                    TO PRNO-KDCALL                       
142800                                                                          
142900          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
143000                                                                          
143100          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
143200                                        WS-IDPRQUES                       
143300          MOVE +2                    TO PRQU-KDCALL                       
143400       END-IF                                                             
143500                                                                          
143600       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
143700       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
143800       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
143900       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
144000       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
144100       MOVE 'N'                      TO PRQU-KDPRSTA                      
144200       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
144300       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
144400                                                                          
144500       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
144600       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
144700       PERFORM IMS-GU-WDB101                                              
144800       MOVE BET-KDVALISO             TO ORAD-KDVALISO                     
144900                                        PRQU-KDVALISO                     
145000                                                                          
145100       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
145200       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
145300       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
145400                                                                          
145500       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
145600                                          PRQU-WDC7-PCB                   
145700                                          PRQU-SJKO-WDK6-PCB              
145800                                                                          
145900       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
146000                                         WS-IDPRQUES                      
146100       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
146200       IF ORAD-PRARTNTO-LOC = +0                                          
146300          MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL            
146400       END-IF                                                             
146500                                                                          
146600       IF ORAD-PRARTNTO-LOC NOT = +0                                      
146700         IF ORAD-KDPRTYP = SPACE                                          
146800           MOVE 'P'            TO ORAD-KDPRTYP                            
146900           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
147000         END-IF                                                           
147100       END-IF                                                             
147200                                                                          
147300     ELSE                                                                 
147400*      *NOT DIST79-DEALER-PRICE                                           
147500                                                                          
147600         MOVE NEJ                  TO VOR-MAN-PRIS-SW                     
147700         IF ORAD-PRARTNTO NOT = +0                                        
147800            IF OHUV-KDORDKL = 0 AND ORAD-IDDC = WC-CDC-SE AND             
147900               (DIST07-KINA  OR DIST07-INDIEN OR DIST07-MEXICO OR         
148000                DIST07-KOREA OR DIST07-TURKEY OR DIST07-MALAYSIA          
148100                OR DIST07-THAILAND OR DIST07-TAIWAN OR                    
148110                DIST07-S-AFRICA OR DIST07-BRAZIL)                         
148200*              *CAN BE BOUNCE ORDER AND IF WE PUT AN MANUALLY             
148300*              *NETTOPRICE THEN WE HAVE TO FETCH AVERAGE PRICE            
148400               MOVE 1              TO PRIS-KDCALL                         
148500               MOVE JA             TO VOR-MAN-PRIS-SW                     
148600            ELSE                                                          
148700               MOVE 2              TO PRIS-KDCALL                         
148800            END-IF                                                        
148900         ELSE                                                             
149000            MOVE 1                 TO PRIS-KDCALL                         
149100         END-IF                                                           
149200         MOVE IDPGM                TO PRIS-IDPGM                          
149300         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
149400         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
149500         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
149600         MOVE ORAD-IDDC            TO PRIS-IDDC                           
149700         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
149800         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
149900         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
150000                                                                          
150100         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
150200                             PRIS-WDK7-PCB                                
150300                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
150400                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
150500                             PRIS-COST-WDK6-PCB                           
150600                             PRIS-COST-WDK7-PCB                           
150700                             PRIS-COST-WDF1-PCB                           
150800                             PRIS-COST-9305-PCB                           
150900                             PRIS-COST-WDK72-PCB                          
151000                             PRIS-COST-WDB6-PCB                           
151100                                                                          
151200         IF PRIS-KDSVAR = '2'                                             
151300           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
151400                             TO FELTEXT                                   
151500           CALL ABEND USING RKOD-ABEND                                    
151600         END-IF                                                           
151700                                                                          
151800         IF PRIS-KDCALL = 2                                               
151900           MOVE PRIS-KDVALISO        TO ORAD-KDVALISO                     
152000           IF ORAD-KDPRTYP = SPACE                                        
152100             MOVE 'P'                TO ORAD-KDPRTYP                      
152200             MOVE ORAD-TIREGDAT      TO ORAD-TIPRIS                       
152300           END-IF                                                         
152400         ELSE                                                             
152500          IF VOR-MAN-PRIS                                                 
152600*           *FROM 4225, VOR-QUEUE                                         
152700            MOVE PRIS-PRAVCOST       TO ORAD-PRAVCOST                     
152800            MOVE PRIS-KDVALISO       TO ORAD-KDVALISO                     
152900            IF ORAD-KDPRTYP = SPACE                                       
153000              MOVE 'P'              TO ORAD-KDPRTYP                       
153100              MOVE ORAD-TIREGDAT    TO ORAD-TIPRIS                        
153200            END-IF                                                        
153300          ELSE                                                            
153400            MOVE PRIS-PRARTNTO       TO ORAD-PRARTNTO                     
153500            MOVE PRIS-FLPRTILL       TO ORAD-FLPRTILL                     
153600            MOVE PRIS-KDPRTYP        TO ORAD-KDPRTYP                      
153700            MOVE PRIS-PRBPRIS        TO ORAD-PRBPRIS                      
153800            MOVE ORAD-TIREGDAT       TO ORAD-TIPRIS                       
153900            MOVE PRIS-KDVALISO       TO ORAD-KDVALISO                     
154000            MOVE PRIS-PRAVCOST       TO ORAD-PRAVCOST                     
154100          END-IF                                                          
154200         END-IF                                                           
154300     END-IF                                                               
154400     END-IF                                                               
154500     .                                                                    
154600     EJECT                                                                
154700                                                                          
154800 ECE-KOLLA-XDC-RAD SECTION.                                               
154900     SKIP2                                                                
155000     IF ALLT-OK AND DCS-NDC                                               
155100                                                                          
155200       MOVE JA TO ALLT-SW                                                 
155300       PERFORM ECEX-PREL-AVBOKNING-XDC                                    
155400*      MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                          
155500*      MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                         
155600*      MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                         
155700*      MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                          
155800*      MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                         
155900*      MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                          
156000*      MOVE ORAD-IDDC            TO NDCA-IDDC                             
156100*      MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                   
156200*      MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                       
156300*      MOVE OHUV-IDDC-TVS        TO NDCA-IDDC-TVS                         
156400*      MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                          
156500*      MOVE 1                    TO NDCA-IXDCCLEAR                        
156600*      MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                         
156700*      MOVE AREG-KDERS           TO NDCA-KDERS                            
156800*      MOVE ORAD-KDORDING        TO NDCA-KDORDING                         
156900*      MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                          
157000*      MOVE AREG-KDSORT          TO NDCA-KDSORT                           
157100*      MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                      
157200*      MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                        
157300*      MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                        
157400*      MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                         
157500*      MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                         
157600*      MOVE ORAD-VKART           TO NDCA-VKART                            
157700*      MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                        
157800*      MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                         
157900*      MOVE +2                   TO NDCA-KDCALL                           
158000*      MOVE SPACE                TO NDCA-XDK7-IDDC                        
158100*      MOVE ZERO                 TO NDCA-XDK7-KDIDDC                      
158200*                                   NDCA-XDK7-KVOKS-DAG                   
158300*                                   NDCA-XDK7-KVOKS-BULK                  
158400                                                                          
158500*      MOVE SPACE                TO CLDC-W411CLDC                         
158600*      MOVE ORAD-IDDC            TO CLDC-IDDC-CLEAR(1)                    
158700*      MOVE DLI-IO-AREA-B601     TO CLDC-WDB601(1)                        
158800                                                                          
158900*      CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                    
159000*                                        NDCA-USEA-PCB                    
159100*                                        NDCA-WDK7-PCB                    
159200*                                        NDCA-WDL6-PCB                    
159300*                                        NDCA-WDB6-PCB                    
159400*                                        NDCA-XDK7-W411XDK7               
159500                                                                          
159600*          PERFORM ECEX-CHECK-DIFF                                        
159700       IF XDCA-KDORDBEK > ZERO                                            
159800          MOVE JA                  TO OBKR-SW                             
159900          IF XDCA-KDORDBEK = 15                                           
160000             IF SDCA-KDORDBEK-FIRST-SDC = 15                              
160100                MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC              
160200             END-IF                                                       
160300             IF SDCA-KDORDBEK-SECOND-SDC = 15                             
160400                MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC             
160500             END-IF                                                       
160600             IF SDCA-KDORDBEK = 15                                        
160700                MOVE ZERO         TO SDCA-KDORDBEK                        
160800             END-IF                                                       
160900          END-IF                                                          
161000       END-IF                                                             
161100       MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                       
161200       MOVE XDCA-ADGANG            TO ORAD-ADGANG                         
161300       MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                        
161400       MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                           
161500       MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                        
161600       MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                       
161700       MOVE XDCA-KDOI              TO ORAD-KDOI                           
161800       MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                     
161900       MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                       
162000       MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                        
162100       MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                       
162200       MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                       
162300       MOVE XDCA-VKART-OUT         TO ORAD-VKART                          
162400       MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                      
162500       MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                       
162600       MOVE NEJ                    TO ALLT-SW                             
162700                                                                          
162800     END-IF                                                               
162900     .                                                                    
163000     EJECT                                                                
163100                                                                          
163200 ECEX-PREL-AVBOKNING-XDC SECTION.                                         
163300                                                                          
163400     IF ALLT-OK                                                           
163500                                                                          
163600       IF DCS-NDC                                                         
163700                                                                          
163800* XDCA-INPUT                                                              
163900         MOVE +1 TO WS-INDEX                                              
164000         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
164100           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
164200                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
164500           ADD +1 TO WS-INDEX                                             
164600         END-PERFORM                                                      
164700                                                                          
164800         MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                       
164900         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
165000         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
165100         MOVE ORAD-IDDC            TO XDCA-IDDC                           
165200         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
165300         MOVE AREG-KDERS           TO XDCA-KDERS                          
165400         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
165500         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
165600         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
165700         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
165800         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
165900         MOVE ORAD-VKART           TO XDCA-VKART                          
166000         MOVE +1                   TO XDCA-KDCALL                         
166100                                                                          
166200* XDCA-OUTPUT                                                             
166300         MOVE SPACE                TO XDCA-IDDC-OUT                       
166400                                      XDCA-IDDC-RO                        
166500                                      XDCA-KDARTURS                       
166600                                      XDCA-KDOI                           
166700                                      XDCA-CLEARGROUP                     
166800         MOVE ZERO                 TO XDCA-ADLAGOMR                       
166900                                      XDCA-ADGANG                         
167000                                      XDCA-ADPLATS                        
167100                                      XDCA-KDORDBEK                       
167200                                      XDCA-KVPREAVB                       
167300                                      XDCA-KVPRERO                        
167400                                      XDCA-TIREGDAT-OUT                   
167500                                      XDCA-TIREGTID-OUT                   
167600                                      XDCA-VKART-OUT                      
167700                                      XDCA-VKART-NTO                      
167800                                      XDCA-VLARTNTO                       
168000         MOVE ZERO                 TO                                     
168100                                      XDCA-KVOKS-DAG                      
168200                                      XDCA-KVOKS-BULK                     
168300         IF XDCA-DAPUBL NOT = 99999999                                    
168310            MOVE ZERO      TO XDCA-DAPUBL                                 
168320         END-IF                                                           
168330                                                                          
168400         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
168500         XDCA-USEA-PCB                                                    
168600         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
168700         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
168800         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
168900         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
169000         XDCA-WDK7-3-PCB                                                  
169100                                                                          
169200* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
169300* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
169400         IF XDCA-KDARTURS = SPACE                                         
169500           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
169600         END-IF                                                           
169700         IF XDCA-VKART-NTO = ZERO                                         
169800           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
169900         END-IF                                                           
170000         IF XDCA-VLARTNTO = ZERO                                          
170100           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
170200         END-IF                                                           
170300       END-IF                                                             
170400     END-IF                                                               
170500     .                                                                    
170600     EJECT                                                                
170700 ECEX-CHECK-DIFF SECTION.                                                 
170800                                                                          
170900     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
171000     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
171100     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
171200     AND NDCA-ADGANG     = XDCA-ADGANG                                    
171300     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
171400     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
171500     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
171600     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
171700     AND NDCA-KDOI       = XDCA-KDOI                                      
171800     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
171900     AND NDCA-VKART      = XDCA-VKART-OUT                                 
172000     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
172100     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
172200     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
172300     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
172400     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
172500     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
172600         MOVE NEJ TO DIFF-FLSVAR                                          
172700     ELSE                                                                 
172800        MOVE JA           TO DIFF-FLSVAR                                  
172900     END-IF                                                               
173000                                                                          
173100* ORDER LOG INFO                                                          
173200     IF DIFF-FLSVAR = JA                                                  
173300       MOVE IDPGM         TO FIL-IDPGM                                    
173400       ACCEPT FIL-TIREGDAT FROM DATE                                      
173500       ACCEPT FIL-TIKLOCK FROM TIME                                       
173600       MOVE 1             TO FIL-IDSEKVNR                                 
173700       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
173800       MOVE 'A'           TO FIL-CT-IDVTYP                                
173900       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
174000                                                                          
174100*   ORDER LINE INFO                                                       
174200       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
174300       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
174400       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
174500       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
174600       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
174700       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
174800       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
174900       MOVE '4232'         TO DIFF-IDSYSTEM                               
175000                                                                          
175100*   NDCA INFO                                                             
175200       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
175300       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
175400       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
175500       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
175600       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
175700       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
175800       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
175900       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
176000       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
176100       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
176200       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
176300       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
176400       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
176500       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
176600       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
176700       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
176800                                                                          
176900*   XDCA INFO                                                             
177000       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
177100       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
177200       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
177300       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
177400       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
177500       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
177600       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
177700       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
177800       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
177900       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
178000       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
178100       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
178200       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
178300       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
178400       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
178500       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
178600                                                                          
178700       PERFORM IMS-ISRT-WDR601                                            
178800       IF SEGMENT-FINNS-REDAN                                             
178900          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
179000             ADD 1 TO FIL-IDSEKVNR                                        
179100             PERFORM IMS-ISRT-WDR601                                      
179200          END-PERFORM                                                     
179300       END-IF                                                             
179400     END-IF                                                               
179500     .                                                                    
179600     EJECT                                                                
179700 ECK-KOLLA-SDC-RAD SECTION.                                               
179800                                                                          
179900     IF ALLT-OK AND (DCS-SDC OR DIST35-REFILL-NA-JAP)                     
180000                                                                          
180100       MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                          
180200       MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                          
180300       MOVE JA                   TO SDCA-FLORDSPE                         
180400       MOVE AREG-FLREFILL        TO SDCA-FLREFILL                         
180500       MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                          
180600       MOVE ORAD-IDDC            TO SDCA-IDDC                             
180700       MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                         
180800       MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                          
180900       MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                          
181000       MOVE ORAD-KDORDING        TO SDCA-KDORDING                         
181100       MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                         
181200       MOVE AREG-KDSORT          TO SDCA-KDSORT                           
181300       MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                        
181400       MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                        
181500       MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                         
181600       MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                         
181700       MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                         
181800       MOVE +0                   TO SDCA-TIREPDAT                         
181900       MOVE +0                   TO SDCA-KVOKS-PREL                       
182000       MOVE +1                   TO SDCA-KDCALL                           
182100       MOVE +1                   TO SDCA-IXDCCLEAR                        
182200                                                                          
182300       CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                    
182400                                         SDCA-WDB6-PCB                    
182500                                         SDCA-WDK9-PCB                    
182600                                         SDCA-WDR6-PCB                    
182700                                         SDCA-WDK6-PCB                    
182800                                         SDCA-WDQ4B-PCB                   
182900                                         SDCA-WDQ2-PCB                    
183000                                         SDCA-WDQ4-PCB                    
183100                                         SDCA-WDB6-2-PCB                  
183200                                         SDCA-WDK6-2-PCB                  
183300                                         SDCA-WDK7-2-PCB                  
183400                                         SDCA-WDK7-3-PCB                  
183500                                                                          
183600                                                                          
183700       IF SDCA-KDORDBEK > ZERO                                            
183800         MOVE JA                   TO OBKR-SW                             
183900         MOVE NEJ                  TO ALLT-SW                             
184000       ELSE                                                               
184100         MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                       
184200         MOVE SDCA-ADGANG          TO ORAD-ADGANG                         
184300         MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                        
184400       END-IF                                                             
184500       IF DIST35-REFILL-NA-JAP                                            
184600         MOVE 'RE'                 TO ORAD-KDOI                           
184700         MOVE SPACE                TO ORAD-CLEARGROUP                     
184800       ELSE                                                               
184900         MOVE SDCA-KDOI            TO ORAD-KDOI                           
185000         MOVE SDCA-CLEARGROUP      TO ORAD-CLEARGROUP                     
185100       END-IF                                                             
185200     END-IF                                                               
185300     .                                                                    
185400     EJECT                                                                
185500 ECG-SKRIV-OBKR SECTION.                                                  
185600                                                                          
185700     PERFORM ECGA-REDIGERA-OBKR-RAD                                       
185800                                                                          
185900     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
186000*----(KOD 58, 59)                                                         
186100        MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                  
186200                               TO OBKR-KDORDBEK                           
186300        MOVE '4232ORFK'        TO OBKR-IDPGM                              
186400        MOVE 'S'               TO OBKR-SW                                 
186500     END-IF                                                               
186600                                                                          
186700     IF SPAR-KDORDBEK > +0                                                
186800*----(KOD 58) , 26                                                        
186900        IF OBKR-SKRIVEN                                                   
187000           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
187100           ADD +1              TO OBKR-IDSEKVNR                           
187200        END-IF                                                            
187300        MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                           
187400        MOVE '4232SPAR'        TO OBKR-IDPGM                              
187500        MOVE 'S'               TO OBKR-SW                                 
187600     END-IF                                                               
187700                                                                          
187800     IF XDCA-KDORDBEK > ZERO                                              
187900*----(KOD 53, 55, 80)                                                     
188000        IF OBKR-SKRIVEN                                                   
188100           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
188200           ADD +1              TO OBKR-IDSEKVNR                           
188300        END-IF                                                            
188400        IF XDCA-KDORDBEK = 80                                             
188500           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
188600        END-IF                                                            
188700        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
188800        MOVE '4232XDCA'        TO OBKR-IDPGM                              
188900        MOVE 'S'               TO OBKR-SW                                 
189000     END-IF                                                               
189100                                                                          
189200     IF SDCA-KDORDBEK > ZERO                                              
189300*----(KOD 53)                                                             
189400        IF OBKR-SKRIVEN                                                   
189500           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
189600           ADD +1              TO OBKR-IDSEKVNR                           
189700        END-IF                                                            
189800        IF SDCA-KDORDBEK = 80                                             
189900           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
190000        END-IF                                                            
190100        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
190200        MOVE '4232SDCA'        TO OBKR-IDPGM                              
190300        MOVE 'S'               TO OBKR-SW                                 
190400     END-IF                                                               
190500     IF OBKR-SKRIVEN                                                      
190600        PERFORM IMS-05-ISRT-WLORQM01-WDQ101                               
190700     END-IF                                                               
190800     .                                                                    
190900     EJECT                                                                
191000 ECGA-REDIGERA-OBKR-RAD SECTION.                                          
191100                                                                          
191200     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
191300     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
191400                               TO OBKR-IDARTNR                            
191500     MOVE OBKR-IDORDER         TO W-IDORDER-Q1-MIN                        
191600                                  W-IDORDER-Q1-MAX                        
191700     MOVE OBKR-IDARTNR         TO W-IDARTNR-Q1-MIN                        
191800                                  W-IDARTNR-Q1-MAX                        
191900     MOVE +1                   TO W-IDLOPNR-Q1-MIN                        
192000                                  W-IDLOPNR-Q1-MAX                        
192100                                  W-IDSEKVNR-Q1-MIN                       
192200                                  W-IDSEKVNR-Q1-MAX                       
192300     PERFORM IMS-04-GU-ORQM-WDQ101                                        
192400     PERFORM UNTIL SEGMENT-SAKNAS                                         
192500        ADD +1                 TO W-IDLOPNR-Q1-MIN                        
192600                                  W-IDLOPNR-Q1-MAX                        
192700        PERFORM IMS-04-GU-ORQM-WDQ101                                     
192800     END-PERFORM                                                          
192900     MOVE W-IDLOPNR-Q1-MIN     TO OBKR-IDLOPNR                            
193000     MOVE W-IDSEKVNR-Q1-MIN    TO OBKR-IDSEKVNR                           
193100     MOVE ORAD-IDDC            TO OBKR-IDDC                               
193200     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
193300     MOVE +0                   TO OBKR-KDORDBEK                           
193400     MOVE SPACE                TO OBKR-BEERS                              
193500     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
193600     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
193700     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
193800     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
193900     MOVE +0                   TO OBKR-DIERS-KVOT                         
194000     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
194100     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
194200     MOVE NEJ                  TO OBKR-FLOBOK                             
194300     EJECT                                                                
194400     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
194500     MOVE NEJ                  TO OBKR-FLOBPRT                            
194600     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
194700     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
194800     MOVE JA                   TO OBKR-FLSLATT                            
194900     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
195000     MOVE +0                   TO OBKR-IDARTNR-TILLK                      
195100                                  OBKR-REKSIFFR-TILLK                     
195200     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
195300     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
195400     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
195500     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
195600     MOVE 'IMS '               TO OBKR-IDSYSTEM                           
195700     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
195800     MOVE AREG-KDERS           TO OBKR-KDERS                              
195900     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
196000     MOVE ORAD-KDOI            TO OBKR-KDOI                               
196100     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
196200     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
196300     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
196400     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
196500     MOVE +0                   TO OBKR-KVANNANT                           
196600     MOVE +0                   TO OBKR-KVAVBART                           
196700     EJECT                                                                
196800     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
196900     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
197000     MOVE +0                   TO OBKR-KVBEART-TILLK                      
197100     MOVE +0                   TO OBKR-KVPREAVB                           
197200     MOVE +0                   TO OBKR-KVPRERO                            
197300     MOVE AREG-KVQPACK-1       TO OBKR-KVQPACK                            
197400     MOVE +0                   TO OBKR-KVRO                               
197500     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
197600     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
197700     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
197800     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
197900     MOVE ORAD-REKSIFFR        TO OBKR-REKSIFFR                           
198000     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
198100     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
198200     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
198300     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
198400     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
198500     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
198600     MOVE +0                   TO OBKR-TIRODAT                            
198700     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
198800     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
198900       MOVE 20                 TO WS-SEKEL-9KOMPL                         
199000     ELSE                                                                 
199100       MOVE 19                 TO WS-SEKEL-9KOMPL                         
199200     END-IF                                                               
199300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
199400     MOVE ORAD-TITPO           TO OBKR-TITPO                              
199500     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
199600     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
199700     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
199800     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
199900     MOVE SPACE                TO OBKR-IDBIL                              
200000                                                                          
200100     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
200200     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
200300     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
200400     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
200500     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
200600     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
200700*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
200800     IF OBKR-KDVALISO = SPACE                                             
200900        MOVE 'N/A'             TO OBKR-KDVALISO                           
201000     END-IF                                                               
201100     .                                                                    
201200     EJECT                                                                
201300 ECH-KONTROLLERA-ENHETSLAST SECTION.                                      
201400                                                                          
201500     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
201600     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
201700     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
201800     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
201900     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
202000     MOVE ORAD-IDDC            TO LAST-IDDC                               
202100     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
202200     MOVE ORAD-KVBEART-Q       TO LAST-KVPREAVB                           
202300     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
202400     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
202500                                                                          
202600     CALL W411LAST USING LAST-W411LAST                                    
202700     .                                                                    
202800     EJECT                                                                
202900 ECI-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
203000                                                                          
203100     IF LAST-ADLAGOMR-UT = +0 AND                                         
203200        LAST-KVANTAL-UT  = +0 AND                                         
203300        LAST-KVBEART-UT  = +0                                             
203400*------------------------------------------------------------*            
203500*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
203600*------------------------------------------------------------*            
203700        PERFORM ECIA-FIXA-LAGEROMR-PLATS                                  
203800        PERFORM ECIB-REDIGERA-WOPS-AREA                                   
203900        PERFORM IMS-06-ISRT-ORQF-WDQ401                                   
204000        PERFORM UNTIL SEGMENT-FINNS                                       
204100           ADD +1                    TO ORAD-IDLOPNR                      
204200           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
204300        END-PERFORM                                                       
204400     ELSE                                                                 
204500*------------------------------------------------------------*            
204600*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
204700*------------------------------------------------------------*            
204800                                                                          
204900        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
205000*------------------------------------------------------------*            
205100*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
205200*------------------------------------------------------------*            
205300                                                                          
205400           MOVE LAST-KVBEART-UT      TO ORAD-KVBEART-Q                    
205500           PERFORM ECIA-FIXA-LAGEROMR-PLATS                               
205600           PERFORM ECIB-REDIGERA-WOPS-AREA                                
205700           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
205800           PERFORM UNTIL SEGMENT-FINNS                                    
205900              ADD +1                 TO ORAD-IDLOPNR                      
206000              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
206100           END-PERFORM                                                    
206200     EJECT                                                                
206300*------------------------------------------------------------*            
206400*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
206500*------------------------------------------------------------*            
206600                                                                          
206700           MOVE +0                   TO ORAD-KVBEART                      
206800           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
206900           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
207000           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
207100             CONTINUE                                                     
207200           ELSE                                                           
207300             IF LAST-ADGANG-UT > ZERO                                     
207400               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
207500             END-IF                                                       
207600           END-IF                                                         
207700           MOVE 1.0000               TO ORAD-RERF-RAD                     
207800           PERFORM ECIB-REDIGERA-WOPS-AREA                                
207900           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
208000           PERFORM UNTIL SEGMENT-FINNS                                    
208100              ADD +1                 TO ORAD-IDLOPNR                      
208200              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
208300           END-PERFORM                                                    
208400        ELSE                                                              
208500*------------------------------------------------------------*            
208600*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
208700*------------------------------------------------------------*            
208800           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
208900           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
209000             CONTINUE                                                     
209100           ELSE                                                           
209200             IF LAST-ADGANG-UT > ZERO                                     
209300               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
209400             END-IF                                                       
209500           END-IF                                                         
209600           MOVE 1.0000            TO ORAD-RERF-RAD                        
209700           PERFORM ECIA-FIXA-LAGEROMR-PLATS                               
209800           PERFORM ECIB-REDIGERA-WOPS-AREA                                
209900           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
210000           PERFORM UNTIL SEGMENT-FINNS                                    
210100              ADD +1              TO ORAD-IDLOPNR                         
210200              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
210300           END-PERFORM                                                    
210400        END-IF                                                            
210500     END-IF                                                               
210600     .                                                                    
210700     EJECT                                                                
210800 ECIA-FIXA-LAGEROMR-PLATS SECTION.                                        
210900                                                                          
211000     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
211100     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
211200     MOVE ORAD-BERADREF        TO ADRS-BEVARREF-IN                        
211300     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
211400     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
211500     MOVE 1                    TO ADRS-KDCALL-IN                          
211600     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
211700     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
211800     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
211900     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
212000                                                                          
212100     CALL W413ADRS USING ADRS-W413ADRS                                    
212200                                                                          
212300*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
212400     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
212500     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
212600                                                                          
212700     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
212800     .                                                                    
212900     EJECT                                                                
213000 ECIB-REDIGERA-WOPS-AREA SECTION.                                         
213100                                                                          
213200     MOVE +1                   TO AVSR-KDCALL                             
213300     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
213400     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
213500     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
213600     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
213700     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
213800     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
213900                                                                          
214000     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
214100     MOVE SPACE                TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
214200     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
214300     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
214400     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
214500     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
214600     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
214700     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
214800     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
214900     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
215000     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
215100     MOVE SPACE                TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
215200     MOVE +0                   TO AVSR-KDVIA   (WS-INDEX-WOPS)            
215300     MOVE +0                   TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
215400     MOVE +0                   TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
215500     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
215600     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
215700                                                                          
215800     ADD +1                    TO WS-INDEX-WOPS                           
215900     .                                                                    
216000     EJECT                                                                
216100 F-HOPPA-TILL-SVARSBILD SECTION.                                          
216200                                                                          
216300                                                                          
216400     MOVE MFS-KDMFSFOR           TO 4233-SPRAK                            
216500                                                                          
216600     MOVE WS-IDDISTR             TO 4233-IDDISTR-IN                       
216700                                                                          
216800     MOVE WS-IDKUNDNR            TO 4233-IDKUNDNR-IN                      
216900                                                                          
217000     MOVE WS-IDORDNR             TO 4233-IDORDNR-IN                       
217100                                                                          
217200     PERFORM IMS-INSERT-4233-MSG                                          
217300                                                                          
217400     MOVE JA                     TO HOPP-TILL-4233                        
217500     .                                                                    
217600     EJECT                                                                
217700 G-VISA-TOM-SIDA SECTION.                                                 
217800                                                                          
217900     MOVE +1 TO WS-INDEX                                                  
218000     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
218100       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-006(WS-INDEX)                
218200                                 MOD-KVBEART(WS-INDEX)                    
218300                                 MOD-PRARTNTO(WS-INDEX)                   
218400                                 MOD-FLINVEST(WS-INDEX)                   
218500                                 MOD-BERADREF(WS-INDEX)                   
218600       ADD  +1 TO WS-INDEX                                                
218700     END-PERFORM                                                          
218800     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
218900     .                                                                    
219000     EJECT                                                                
219100 H-SKICKA-PRISFRAGA SECTION.                                              
219200                                                                          
219300     MOVE 1                      TO 3039-REQU-IDMSGVER                    
219400     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
219500     MOVE 'W4023200'             TO 3039-REQU-IDUSER                      
219600                                                                          
219700     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
219800     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
219900     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
220000     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
220100     IF MID-IDARTNR-006(WS-INDEX-MID-MAX) = ALL '+'                       
220200       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
220300       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
220400       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
220500     END-IF                                                               
220600*    MOVE ORAD-IDPRQUES          TO 3039-MID-IDPRQUES                     
220700     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
220800                                                                          
220900     PERFORM S04-SKICKA-OPEN                                              
221000     PERFORM S04-SKICKA-MEDDELANDE                                        
221100     PERFORM S04-SKICKA-CLOSE                                             
221200                                                                          
221300     .                                                                    
221400     EJECT                                                                
221500 I-LAES-IN-4541-WDR4 SECTION.                                             
221600     MOVE LOW-VALUE                TO W-4542KEY-MIN-X                     
221700     MOVE HIGH-VALUE               TO W-4542KEY-MAX-X                     
221800     MOVE W-IDDISTR                TO W-IDDISTR-4542-MIN                  
221900                                      W-IDDISTR-4542-MAX                  
222000     MOVE +1                       TO WS-INDEX                            
222100     PERFORM IMS-16-GHN-4541-4542                                         
222200     PERFORM UNTIL SEGMENT-SAKNAS  OR BASEN-SLUT OR                       
222300                   WS-INDEX > WS-INDEX-MID-MAX                            
222400       IF SEGMENT-FINNS                   AND                             
222500          4542-IDKUNDNR = W-IDKUNDNR      AND                             
222600          4542-IDUSER   = MSG-SIGNON-USERID                               
222700                                                                          
222800         MOVE 4542-IDARTNR               TO ARTIKEL-POS-3-11              
222900         MOVE ARTIKEL-REKSIFFRA  TO MID-IDARTNR-006(WS-INDEX)             
223000                                    MOD-IDARTNR-006(WS-INDEX)             
223100         COMPUTE WS-KVBEART-NUM = 4542-KVBEART-Q - 4542-KVPREAVB          
223200         MOVE WS-KVBEART-NUM             TO MID-KVBEART(WS-INDEX)         
223300                                            MOD-KVBEART(WS-INDEX)         
223400         MOVE 4542-BERADREF              TO MID-BERADREF(WS-INDEX)        
223500                                            MOD-BERADREF(WS-INDEX)        
223600         IF 4542-KDPRTYP = 'P'                                            
223700            IF DIST79-DEALER-PRICE                                        
223800              IF 4542-PRARTNTO-LOC > +0                                   
223900                MOVE 4542-PRARTNTO-LOC     TO WS-PRARTNTO-NUM             
224000              ELSE                                                        
224100                MOVE 4542-PRARTNTO-LOCPREL TO WS-PRARTNTO-NUM             
224200              END-IF                                                      
224300            ELSE                                                          
224400              MOVE 4542-PRARTNTO         TO WS-PRARTNTO-NUM               
224500            END-IF                                                        
224600            MOVE WS-PRARTNTO-NUM         TO WS-PRARTNTO-RED               
224700            MOVE WS-PRARTNTO-RED         TO MOD-PRARTNTO(WS-INDEX)        
224800            MOVE WS-PRARTNTO-ALFA        TO MID-PRARTNTO(WS-INDEX)        
224900         END-IF                                                           
225000                                                                          
225100         IF 4542-IDLEVNR = SPACE                                          
225200            MOVE 4542-IDARTNR               TO W-IDARTNR                  
225300            IF 4542-IDDC NOT = W-IDDC-B6                                  
225400               MOVE 4542-IDDC TO W-IDDC-B6                                
225500               PERFORM IMS-GU-WDB601                                      
225600            END-IF                                                        
225700            IF DCS-CDC                                                    
225800              PERFORM IMS-17-GHU-ARTM-WDK901                              
225900              COMPUTE ART-KVOKS-VOR =                                     
226000                      ART-KVOKS-VOR - WS-KVBEART-NUM                      
226100              PERFORM IMS-18-REPL-ARTM-WDK901                             
226200            ELSE                                                          
226300              IF 4542-KDORDBEK = 92 OR 93 OR 98                           
226400                MOVE 4542-IDDC              TO W-IDDC                     
226500                PERFORM IMS-GHU-WDK711                                    
226600                COMPUTE SLAG-KVOKS-DAG =                                  
226700                        SLAG-KVOKS-DAG - WS-KVBEART-NUM                   
226800                PERFORM IMS-REPL-WDK711                                   
226900              END-IF                                                      
227000            END-IF                                                        
227100         END-IF                                                           
227200                                                                          
227300         ADD +1 TO WS-INDEX                                               
227400         MOVE '2'          TO 4542-KDVORATG                               
227500         PERFORM IMS-19-REPL-4541-4542                                    
227600       END-IF                                                             
227700       PERFORM IMS-16-GHN-4541-4542                                       
227800     END-PERFORM                                                          
227900     IF OHUV-IDDC-TVS NOT = W-IDDC-B6                                     
228000        MOVE OHUV-IDDC-TVS TO W-IDDC-B6                                   
228100        PERFORM IMS-GU-WDB601                                             
228200     END-IF                                                               
228300     .                                                                    
228400     EJECT                                                                
228500 K-LAES-FRAN-NYVORKO SECTION.                                             
228600     MOVE LOW-VALUE                TO W-WDA6F1KY-MIN-X                    
228700     MOVE HIGH-VALUE               TO W-WDA6F1KY-MAX-X                    
228800     MOVE OHUV-IDDISTR             TO W-IDDISTR-A6F1-MIN                  
228900                                      W-IDDISTR-A6F1-MAX                  
229000     MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-A6F1-MIN                 
229100                                      W-IDKUNDNR-A6F1-MAX                 
229200     MOVE +1                       TO WS-INDEX                            
229300*----------------------------------------------- LETA RÄTT PÅ RAD         
229400*                                                                         
229500     PERFORM IMS-13-GN-VORKO-NY                                           
229600     PERFORM UNTIL SEGMENT-SAKNAS  OR BASEN-SLUT OR                       
229700                   WS-INDEX > WS-INDEX-MID-MAX                            
229800       IF  SEGMENT-FINNS                                                  
229900       AND SEQF-KDVORATG = '1'                                            
230000       AND SEQF-IDUSER   = MSG-SIGNON-USERID                              
230100                                                                          
230200         MOVE SEQF-IDDISTR         TO W-IDDISTR-A6F                       
230300         MOVE SEQF-IDKUNDNR        TO W-IDKUNDNR-A6F                      
230400         MOVE SEQF-TIREGDAT-AVV9   TO W-TIREGDAT-AVV9-A6F                 
230500         MOVE SEQF-TIREGTID-AVV9   TO W-TIREGTID-AVV9-A6F                 
230600                                                                          
230700*----------------------------------------------- LÄS FRAM RAD             
230800*                                                                         
230900         PERFORM IMS-14-GHU-VORKO-NY                                      
231000                                                                          
231100         MOVE VOR-IDARTNR           TO ARTIKEL-POS-3-11                   
231200         MOVE ARTIKEL-REKSIFFRA     TO MID-IDARTNR-006(WS-INDEX)          
231300                                       MOD-IDARTNR-006(WS-INDEX)          
231400         COMPUTE WS-KVBEART-NUM = VOR-KVBEART-Q - VOR-KVPREAVB            
231500         MOVE WS-KVBEART-NUM        TO MID-KVBEART(WS-INDEX)              
231600                                       MOD-KVBEART(WS-INDEX)              
231700         MOVE VOR-BERADREF          TO MID-BERADREF(WS-INDEX)             
231800                                       MOD-BERADREF(WS-INDEX)             
231900         IF VOR-KDPRTYP = 'P'                                             
232000            IF DIST79-DEALER-PRICE                                        
232100             IF VOR-PRARTNTO-LOC > +0                                     
232200              MOVE VOR-PRARTNTO-LOC TO WS-PRARTNTO-NUM                    
232300             ELSE                                                         
232400              MOVE VOR-PRARTNTO-LOCPREL TO WS-PRARTNTO-NUM                
232500             END-IF                                                       
232600            ELSE                                                          
232700              MOVE VOR-PRARTNTO     TO WS-PRARTNTO-NUM                    
232800            END-IF                                                        
232900            MOVE WS-PRARTNTO-NUM    TO WS-PRARTNTO-RED                    
233000            MOVE WS-PRARTNTO-RED    TO MOD-PRARTNTO(WS-INDEX)             
233100            MOVE WS-PRARTNTO-ALFA   TO MID-PRARTNTO(WS-INDEX)             
233200         END-IF                                                           
233300                                                                          
233400         MOVE VOR-IDARTNR               TO W-IDARTNR                      
233500         IF VOR-IDDC NOT = W-IDDC-B6                                      
233600            MOVE VOR-IDDC TO W-IDDC-B6                                    
233700            PERFORM IMS-GU-WDB601                                         
233800         END-IF                                                           
233900         IF DCS-CDC                                                       
234000           PERFORM IMS-17-GHU-ARTM-WDK901                                 
234100           COMPUTE ART-KVOKS-VOR =                                        
234200                   ART-KVOKS-VOR - WS-KVBEART-NUM                         
234300           PERFORM IMS-18-REPL-ARTM-WDK901                                
234400         ELSE                                                             
234500           IF VOR-KDORDBEK = 92 OR 93 OR 98                               
234600             MOVE VOR-IDDC              TO W-IDDC                         
234700             PERFORM IMS-GHU-WDK711                                       
234800             COMPUTE SLAG-KVOKS-DAG =                                     
234900                     SLAG-KVOKS-DAG - WS-KVBEART-NUM                      
235000             PERFORM IMS-REPL-WDK711                                      
235100           END-IF                                                         
235200         END-IF                                                           
235300                                                                          
235400         ADD +1 TO WS-INDEX                                               
235500         IF OHUV-IDDC-TVS NOT = W-IDDC-B6                                 
235600            MOVE OHUV-IDDC-TVS TO W-IDDC-B6                               
235700            PERFORM IMS-GU-WDB601                                         
235800         END-IF                                                           
235900         IF  DCS-NDC                                                      
236000           MOVE '6'                TO VOR-KDVORATG                        
236100           MOVE OHUV-IDKUNDRF      TO VOR-IDKUNDRF-LEV                    
236200           MOVE VOR-KVBEART-Q      TO VOR-KVPREAVB                        
236300           IF VOR-TIKLAR = ZERO                                           
236400              ACCEPT VOR-TIREGDAT-LEV FROM DATE                           
236500              MOVE   VOR-TIREGDAT-LEV TO VOR-TIKLAR                       
236600              ACCEPT VOR-TIREGTID-LEV FROM TIME                           
236700              COMPUTE VOR-TIKLATID    = VOR-TIREGTID-LEV                  
236800                                      / 100                               
236900              END-COMPUTE                                                 
237000           END-IF                                                         
237100         ELSE                                                             
237200           MOVE '2'                TO VOR-KDVORATG                        
237300           MOVE OHUV-IDKUNDRF      TO VOR-IDKUNDRF-LEV                    
237400           MOVE VOR-KVBEART-Q      TO VOR-KVPREAVB                        
237500           IF VOR-TIKLAR = ZERO                                           
237600              ACCEPT VOR-TIREGDAT-LEV FROM DATE                           
237700              ACCEPT VOR-TIREGTID-LEV FROM TIME                           
237800              IF  OHUV-KDORDKL = 1                                        
237900                MOVE VOR-TIREGDAT-LEV TO VOR-TIKLAR                       
238000                COMPUTE VOR-TIKLATID  = VOR-TIREGTID-LEV                  
238100                                   / 100                                  
238200              END-IF                                                      
238300           END-IF                                                         
238400         END-IF                                                           
238500                                                                          
238600         PERFORM IMS-12-GHU-WDK611                                        
238700*        SUBTRACT VOR-KVBEART-Q    FROM CLAG-KVVORKO                      
238800         SUBTRACT WS-KVBEART-NUM   FROM CLAG-KVVORKO                      
238900         PERFORM IMS-11-REPL-WDK6                                         
239000                                                                          
239100         PERFORM IMS-15-REPL-VORKO-NY                                     
239200       END-IF                                                             
239300       PERFORM IMS-13-GN-VORKO-NY                                         
239400     END-PERFORM                                                          
239500     .                                                                    
239600     EJECT                                                                
239700                                                                          
239800 Z-FINIT-INSERT-MSG SECTION.                                              
239900                                                                          
240000     IF MED-IDMFSFEL NOT = SPACE                                          
240100        CALL WMEDKONV USING MED-WMEDAREA                                  
240200        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
240300     END-IF                                                               
240400                                                                          
240500     IF MED-IDMFSINF NOT = SPACE                                          
240600        CALL WMEDKONV USING MED-WMEDAREA                                  
240700        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
240800     END-IF                                                               
240900                                                                          
241000     IF NOT ALLT-OK                                                       
241100        PERFORM MFS-ROER-EJ-BILD                                          
241200     END-IF                                                               
241300                                                                          
241400     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O23201-CTX + 4                    
241500     PERFORM IMS-INSERT-MSG                                               
241600     .                                                                    
241700     EJECT                                                                
241800                                                                          
241900 S04-SKICKA-OPEN SECTION.                                                 
242000                                                                          
242100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
242200     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
242300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
242400                                                                          
242500     IF SEND-KDRC > 0                                                     
242600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
242700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
242800       DELIMITED BY SIZE INTO FELTEXT                                     
242900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
243000     END-IF                                                               
243100     .                                                                    
243200     SKIP3                                                                
243300 S04-SKICKA-MEDDELANDE SECTION.                                           
243400                                                                          
243500     MOVE 'PUT'                      TO SEND-KDFUNC                       
243600     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
243700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
243800                                                                          
243900     IF SEND-KDRC > 0                                                     
244000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
244100       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
244200       DELIMITED BY SIZE INTO FELTEXT                                     
244300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
244400     END-IF                                                               
244500     .                                                                    
244600     SKIP3                                                                
244700 S04-SKICKA-CLOSE SECTION.                                                
244800                                                                          
244900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
245000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
245100                                                                          
245200     IF SEND-KDRC > 0                                                     
245300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
245400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
245500       DELIMITED BY SIZE INTO FELTEXT                                     
245600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
245700     END-IF                                                               
245800     .                                                                    
245900     EJECT                                                                
246000                                                                          
246100                                                                          
246200 S20-WRONG-PICTURE-MESSAGE SECTION.                                       
246300     SKIP2                                                                
246400* *****************************************************                   
246500*                                                     *                   
246600* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
246700*                                                     *                   
246800* *****************************************************                   
246900     SKIP2                                                                
247000     MOVE JA                  TO HOPP-TILL-0504                           
247100     MOVE 'W0O50401'          TO MFS-IDMOD                                
247200     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
247300     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
247400     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
247500     PERFORM IMS-INSERT-MSG                                               
247600     .                                                                    
247700     EJECT                                                                
247800 MFS-RENSA-MOD-RADER SECTION.                                             
247900                                                                          
248000     MOVE +1 TO WS-INDEX                                                  
248100     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
248200       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR-006(WS-INDEX)                
248300                                 MOD-KVBEART(WS-INDEX)                    
248400                                 MOD-PRARTNTO(WS-INDEX)                   
248500                                 MOD-FLINVEST(WS-INDEX)                   
248600                                 MOD-BERADREF(WS-INDEX)                   
248700       ADD  +1 TO WS-INDEX                                                
248800     END-PERFORM                                                          
248900     .                                                                    
249000     SKIP2                                                                
249100                                                                          
249200 MFS-ROER-EJ-BILD SECTION.                                                
249300                                                                          
249400     MOVE +1 TO WS-INDEX                                                  
249500     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
249600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-006(WS-INDEX)                
249700                                 MOD-KVBEART(WS-INDEX)                    
249800                                 MOD-PRARTNTO(WS-INDEX)                   
249900                                 MOD-FLINVEST(WS-INDEX)                   
250000                                 MOD-BERADREF(WS-INDEX)                   
250100       ADD  +1 TO WS-INDEX                                                
250200     END-PERFORM                                                          
250300                                                                          
250400     .                                                                    
250500     EJECT                                                                
250600* --- IMS SEKTIONER ---                                                   
250700                                                                          
250800 IMS-GET-MSG SECTION.                                                     
250900                                                                          
251000     MOVE '  QC' TO GODK-STATUSKODER                                      
251100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
251200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
251300     PERFORM IMS-STATUSKONTROLL                                           
251400     .                                                                    
251500     SKIP2                                                                
251600 IMS-INSERT-MSG SECTION.                                                  
251700                                                                          
251800     IF ENGLISH-TEXT                                                      
251900       MOVE 'N' TO MFS-KDHUVOMR                                           
252000     END-IF                                                               
252100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
252200     MOVE SPACE TO GODK-STATUSKODER                                       
252300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
252400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
252500     PERFORM IMS-STATUSKONTROLL                                           
252600     .                                                                    
252700     SKIP2                                                                
252800 IMS-INSERT-4233-MSG SECTION.                                             
252900                                                                          
253000     IF ENGLISH-TEXT                                                      
253100       MOVE 'N' TO MFS-KDHUVOMR                                           
253200     END-IF                                                               
253300     MOVE LOW-VALUE TO 4233-Z1 4233-Z2                                    
253400     MOVE SPACE TO GODK-STATUSKODER                                       
253500     CALL CBLTDLI USING ISRT 4233-PCB 4233-MSG-IO-AREA                    
253600     MOVE 4233-STATUS-CODE TO STATUS-WS                                   
253700     PERFORM IMS-STATUSKONTROLL                                           
253800     .                                                                    
253900     EJECT                                                                
254000 IMS-01-GU-ORQI-WDQ201 SECTION.                                           
254100                                                                          
254200     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
254300          DELIMITED BY SIZE INTO SSA1                                     
254400     MOVE '  GE'               TO GODK-STATUSKODER                        
254500     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
254600     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
254700     PERFORM IMS-STATUSKONTROLL                                           
254800     .                                                                    
254900     SKIP2                                                                
255000 IMS-03-GNP-ORQI-WDQ212 SECTION.                                          
255100                                                                          
255200     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
255300          DELIMITED BY SIZE INTO SSA1                                     
255400     MOVE '    '               TO GODK-STATUSKODER                        
255500     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
255600     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
255700     PERFORM IMS-STATUSKONTROLL                                           
255800     .                                                                    
255900     EJECT                                                                
256000 IMS-04-GU-ORQM-WDQ101 SECTION.                                           
256100                                                                          
256200     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
256300                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
256400          DELIMITED BY SIZE INTO SSA1                                     
256500     MOVE '  GE'               TO GODK-STATUSKODER                        
256600     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
256700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
256800     PERFORM IMS-STATUSKONTROLL                                           
256900     .                                                                    
257000     SKIP2                                                                
257100 IMS-05-ISRT-WLORQM01-WDQ101 SECTION.                                     
257200                                                                          
257300     MOVE 'WLORQM01 '          TO SSA1                                    
257400     MOVE '    '               TO GODK-STATUSKODER                        
257500     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
257600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
257700     PERFORM IMS-STATUSKONTROLL                                           
257800     .                                                                    
257900     SKIP2                                                                
258000 IMS-06-ISRT-ORQF-WDQ401 SECTION.                                         
258100                                                                          
258200     MOVE 'WLORQF01 '          TO SSA1                                    
258300     MOVE '  II'               TO GODK-STATUSKODER                        
258400     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
258500     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
258600     PERFORM IMS-STATUSKONTROLL                                           
258700     .                                                                    
258800     EJECT                                                                
258900 IMS-GU-WDK611 SECTION.                                                   
259000                                                                          
259100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
259200             DELIMITED BY SIZE INTO SSA1                                  
259300     MOVE 'WDK611 '              TO SSA2                                  
259400     MOVE '  GE'                 TO GODK-STATUSKODER                      
259500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
259600     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
259700     PERFORM IMS-STATUSKONTROLL                                           
259800     .                                                                    
259900     SKIP2                                                                
260000 IMS-GNP-WDK621 SECTION.                                                  
260100                                                                          
260200     STRING 'WDK621  (IDLEVNR  =' W-IDLEVNR-X ')'                         
260300          DELIMITED BY SIZE INTO SSA1                                     
260400     MOVE '  GE'                 TO GODK-STATUSKODER                      
260500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
260600     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
260700     PERFORM IMS-STATUSKONTROLL                                           
260800     .                                                                    
260900     SKIP2                                                                
261000 IMS-12-GHU-WDK611 SECTION.                                               
261100                                                                          
261200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
261300          DELIMITED BY SIZE INTO SSA1                                     
261400     MOVE 'WDK611'               TO SSA2                                  
261500     MOVE '  '                   TO GODK-STATUSKODER                      
261600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
261700     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
261800     PERFORM IMS-STATUSKONTROLL                                           
261900     .                                                                    
262000     SKIP2                                                                
262100 IMS-11-REPL-WDK6 SECTION.                                                
262200                                                                          
262300     MOVE '  '                 TO GODK-STATUSKODER                        
262400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
262500     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
262600     PERFORM IMS-STATUSKONTROLL                                           
262700     .                                                                    
262800     EJECT                                                                
262900 IMS-13-GN-VORKO-NY SECTION.                                              
263000                                                                          
263100     STRING 'WDA6F1  (WDA6F1KY >' W-WDA6F1KY-MIN-X                        
263200                    '&WDA6F1KY <' W-WDA6F1KY-MAX-X ')'                    
263300          DELIMITED BY SIZE INTO SSA1                                     
263400     MOVE '  GEGB'               TO GODK-STATUSKODER                      
263500     CALL CBLTDLI USING GN   WDA6F1-PCB DLI-IO-AREA-WDA6F1 SSA1           
263600     MOVE WDA6F1-STATUS-CODE     TO STATUS-WS                             
263700     PERFORM IMS-STATUSKONTROLL                                           
263800     .                                                                    
263900     EJECT                                                                
264000 IMS-14-GHU-VORKO-NY SECTION.                                             
264100                                                                          
264200     STRING 'WDA601  (WDA6FSEQ =' W-WDA6FKY-X ')'                         
264300          DELIMITED BY SIZE INTO SSA1                                     
264400     MOVE '  '                   TO GODK-STATUSKODER                      
264500     CALL CBLTDLI USING GHU  WDA6F-PCB DLI-IO-AREA-WDA601 SSA1            
264600     MOVE WDA6F-STATUS-CODE      TO STATUS-WS                             
264700     PERFORM IMS-STATUSKONTROLL                                           
264800     .                                                                    
264900     SKIP2                                                                
265000 IMS-15-REPL-VORKO-NY SECTION.                                            
265100                                                                          
265200     MOVE '  '                 TO GODK-STATUSKODER                        
265300     CALL CBLTDLI USING REPL WDA6F-PCB DLI-IO-AREA-WDA601                 
265400     MOVE WDA6F-STATUS-CODE     TO STATUS-WS                              
265500     PERFORM IMS-STATUSKONTROLL                                           
265600     .                                                                    
265700     EJECT                                                                
265800 IMS-16-GHN-4541-4542 SECTION.                                            
265900                                                                          
266000     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
266100          DELIMITED BY SIZE INTO SSA1                                     
266200     STRING 'WDGX4542(KY4542  >=' W-4542KEY-MIN-X                         
266300                    '&KY4542  <=' W-4542KEY-MAX-X                         
266400                    '&KDVORATG =' W-KDVORATG-X ')'                        
266500          DELIMITED BY SIZE INTO SSA2                                     
266600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
266700     CALL CBLTDLI USING GHN 4541-PCB DLI-IO-AREA-VOR  SSA1 SSA2           
266800     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
266900     PERFORM IMS-STATUSKONTROLL                                           
267000     .                                                                    
267100     EJECT                                                                
267200 IMS-17-GHU-ARTM-WDK901 SECTION.                                          
267300                                                                          
267400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
267500          DELIMITED BY SIZE  INTO SSA1                                    
267600     MOVE '  '                 TO GODK-STATUSKODER                        
267700     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
267800     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
267900     PERFORM IMS-STATUSKONTROLL                                           
268000     .                                                                    
268100     SKIP2                                                                
268200 IMS-18-REPL-ARTM-WDK901 SECTION.                                         
268300                                                                          
268400     MOVE '  '                 TO GODK-STATUSKODER                        
268500     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
268600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
268700     PERFORM IMS-STATUSKONTROLL                                           
268800     .                                                                    
268900     EJECT                                                                
269000 IMS-GHU-WDK711 SECTION.                                                  
269100                                                                          
269200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
269300          DELIMITED BY SIZE  INTO SSA1                                    
269400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
269500          DELIMITED BY SIZE  INTO SSA2                                    
269600     MOVE '    '               TO GODK-STATUSKODER                        
269700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
269800     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
269900     PERFORM IMS-STATUSKONTROLL                                           
270000     .                                                                    
270100     SKIP3                                                                
270200 IMS-REPL-WDK711 SECTION.                                                 
270300                                                                          
270400     MOVE '    '               TO GODK-STATUSKODER                        
270500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
270600     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
270700     PERFORM IMS-STATUSKONTROLL                                           
270800     .                                                                    
270900     EJECT                                                                
271000                                                                          
271100 IMS-19-REPL-4541-4542 SECTION.                                           
271200                                                                          
271300     MOVE '  '                 TO GODK-STATUSKODER                        
271400     CALL CBLTDLI USING REPL 4541-PCB DLI-IO-AREA-VOR                     
271500     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
271600     PERFORM IMS-STATUSKONTROLL                                           
271700     .                                                                    
271800 IMS-GU-WDB201 SECTION.                                                   
271900                                                                          
272000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
272100          DELIMITED BY SIZE INTO SSA1                                     
272200     MOVE '  '                 TO GODK-STATUSKODER                        
272300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
272400     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
272500     PERFORM IMS-STATUSKONTROLL                                           
272600     .                                                                    
272700     SKIP2                                                                
272800 IMS-GU-WDB101 SECTION.                                                   
272900                                                                          
273000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
273100          DELIMITED BY SIZE INTO SSA1                                     
273200     MOVE '  '                 TO GODK-STATUSKODER                        
273300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
273400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
273500     PERFORM IMS-STATUSKONTROLL                                           
273600     .                                                                    
273700                                                                          
273800 IMS-GU-WDB601    SECTION.                                                
273900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
274000          DELIMITED BY SIZE INTO SSA1                                     
274100     MOVE '  '   TO GODK-STATUSKODER                                      
274200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
274300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
274400     PERFORM IMS-STATUSKONTROLL                                           
274500     IF SEGMENT-SAKNAS                                                    
274600        MOVE SPACE TO DCS-KDDC                                            
274700     END-IF                                                               
274800     .                                                                    
274900                                                                          
275000 IMS-ISRT-WDR601 SECTION.                                                 
275100                                                                          
275200     MOVE 'WDR601' TO SSA1                                                
275300     MOVE '  II' TO GODK-STATUSKODER                                      
275400     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
275500     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
275600     PERFORM IMS-STATUSKONTROLL                                           
275700     .                                                                    
275800                                                                          
275900 IMS-STATUSKONTROLL SECTION.                                              
276000                                                                          
276100     SET STATUS-IX TO 1                                                   
276200     SEARCH GODK-STATUS                                                   
276300       AT END CALL FELLOG                                                 
276400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
276500     END-SEARCH                                                           
276600     .                                                                    
