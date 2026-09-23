000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL016600.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   APRIL 2005.                                              
000600                                                                          
000700                                                                          
000800     REMARKS.                                                             
000900* WL016600 PROGRAM IS A REPLICA OF W4023200 PROGRAM                       
001000* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001100*                                                                         
001200*    NAMN:       CARPARTS.LDC.SPECORDERLINE                               
001300*                                                                         
001400*    FUNKTION.                                                            
001500*        PROGRAMMET HANTERAR UPPLÄGGNING AV ORDERRADER I                  
001600*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
001700*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
001800*                                                                         
001900*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
002000*        SVARSBILD - WL016700.                                            
002100*                                                                         
002200*              I DE FALL ORAD-ADLAGOMR BLIR = +0                          
002300*              SÄTTER VI ORAD-ADLAGOMR = +1, PGA ATT DET INTE             
002400*              FINNS LAGEROMRÅDE 0.                                       
002500*                                                                         
002600*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
002700*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
002800*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
002900*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
003000*    DETTA I LAGEROMRÅDE 1.                                               
003100*                                                                         
003200*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
003300*        PROGRAMMET UPPDAT     WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
003400*        PROGRAMMET UPPDAT     WLORQI (WDQ2)  ORDERHUVUD                  
003500*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
003600*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
003700*        PROGRAMMET LÄSER              WDK7   ARTIKELREGISTER             
003800*                                                                         
003900*    INDATA.                                                              
004000*        TRANSAKTION: WL0166U                                             
004100*        REQUEST:     WL0166I1                                            
004200*    UTDATA.                                                              
004300*        RESPONSE:    WL0166O1                                            
004400*                                                                         
004500*    E-TRACKER: 7450328  2008-HÖST VOHF                                   
004600*    E-TRACKER: 8200058  2012-JULI MANAGEMENT SCRAPPING FOLLOW UP         
004700*    E-TRACKER: 10254592 2015      DECOMISSION VOHF                       
004800*                                                                         
004900     EJECT                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100                                                                          
005200 DATA DIVISION.                                                           
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(08)   VALUE 'WL016600'.            
005800 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
005900 77  HOPP-TILL-0167              PIC X(1)    VALUE 'N'.                   
006000 77  JA                          PIC X(1)    VALUE 'J'.                   
006100 77  YES                         PIC X(1)    VALUE 'Y'.                   
006200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006300 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
006400 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
006500 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
006600 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
006700 77  IX-DCCLEAR-MAX              PIC S9(3)  VALUE +99  COMP SYNC.         
006800 77  WS-INDEX-RESP               PIC S9(9)  COMP-3   VALUE ZERO.          
006900 77  WS-INDEX-100-MAX            PIC S9(9)  COMP-3   VALUE +100.          
007000 77  WS-INDEX-WOPS               PIC S9(9)  COMP-3   VALUE ZERO.          
007100 77  WS-INDEX-WOPS-MAX           PIC S9(9)  COMP-3   VALUE +100.          
007200 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
007300 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
007400 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
007500 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
007600 77  WS-KVRADER                  PIC 9(5)    VALUE ZERO.                  
007700 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
007800                                                                          
007900*01  -COPY WWDCKONS                                                       
008000                                                                          
008100*01  -COPY WWPRODSL                                                       
008200                                                                          
008300 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
008400 77  END-IX                      PIC  9(3)  VALUE ZERO.                   
008500 77  START-IX                    PIC  9(3)  VALUE ZERO.                   
008600                                                                          
008700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008800     88  ALLT-OK                             VALUE 'J'.                   
008900                                                                          
009000 77  OBKR-SW                     PIC X       VALUE 'N'.                   
009100     88  SKRIV-OBKR                          VALUE 'J'.                   
009200     88  OBKR-SKRIVEN                        VALUE 'S'.                   
009300                                                                          
009400 77  WS-IDARTNR-911              PIC 9(11)   VALUE ZERO.                  
009500                                                                          
009600 01  FILLER                      PIC X(08) VALUE 'DDDDDDDD'.              
009700 01  WS-AKT-KEYS.                                                         
009800     03 WS-AKT-IDLOPNR           PIC 9(3).                                
009900     03 WS-AKT-IDSEKVNR          PIC 9(3).                                
010000                                                                          
010100 01  WS-ALFA-6.                                                           
010200     03  WS-NUM-6                PIC 9(6).                                
010300 01  WS-ALFA-7.                                                           
010400     03  WS-NUM-7                PIC 9(7).                                
010500                                                                          
010600 01  WS-IDARTNR.                                                          
010700     03  FILLER                  PIC X(2).                                
010800     03  WS-IDARTNR-3-11.                                                 
010900         05  WS-IDARTNR-3-9      PIC X(7).                                
011000         05  WS-IDARTNR-10       PIC X(1).                                
011100         05  WS-IDARTNR-11       PIC X(1).                                
011200                                                                          
011300 01 W-GMT-IDDC-CLEAR-GRP.                                                 
011400*                                 GRUPP AV IDDC-CLEAR                     
011500     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
011600                                 PIC X(2)    VALUE SPACE.                 
011700                                                                          
011800 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
011900 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
012000     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
012100     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
012200                                                                          
012300 77  FILLER                      PIC X(08)   VALUE 'DDDDDDDD'.            
012400 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
012500 01  FILLER REDEFINES WS-TIHHMMSS.                                        
012600     03 WS-TIHHMM                PIC 9(4).                                
012700     03 FILLER                   PIC 9(2).                                
012800     EJECT                                                                
012900                                                                          
013000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013100 01  FILLER REDEFINES DAGENS-DATUM.                                       
013200     03  DAGENS-AA               PIC 9(2).                                
013300     03  DAGENS-MM               PIC 9(2).                                
013400     03  DAGENS-DD               PIC 9(2).                                
013500                                                                          
013600 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
013700                                                                          
013800 77  FILLER                      PIC X(08)   VALUE 'EEEEEEEE'.            
013900                                                                          
014000 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
014100                                                                          
014200 01  FILLER REDEFINES TEST-IDDISTR.                                       
014300*    03 -COPY WWDIST35                                                    
014400 01  FILLER REDEFINES TEST-IDDISTR.                                       
014500*    03 -COPY WWDIST79                                                    
014600*    ----DISTR-DEALER-PRICE----                                           
014700     EJECT                                                                
014800                                                                          
014900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015000 01  GENERELLA-SUBPROGRAM.                                                
015100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015800     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
015900*                                                                         
016000*    --- PARAMETERS TO ABEND                                              
016100                                                                          
016200 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
016300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
016400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016600     SKIP2                                                                
016700 01  GEMENSAMMA-SUBPROGRAM.                                               
016800     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
016900*        PRISTILLÄMPNING                                                  
017000     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
017100*        HÄMTA PRISFRÅGENR                                                
017200     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
017300*        DEALER PRISFRÅGABEHANDLING                                       
017400     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
017500*        KONTROLLERA SPÄRRAR                                              
017600     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
017700*        KONTROLLERA NDC-ARTREG                                           
017800     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
017900*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
018000     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
018100*        KONTROLLERA SDC-ARTREG                                           
018200     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
018300*        KONTROLL ENHETSLAST                                              
018400     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
018500*        FORMELLA KONTROLLER AV INDATA                                    
018600     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
018700*        WOPS RADBEHANDLING                                               
018800     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
018900*        OMVANDLING AV LAGOMR + PLATS                                     
019000     EJECT                                                                
019100 01  MESSAGE-CODES.                                                       
019200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
019300     03  ERR-OBEHORIG            PIC X(3)    VALUE '305'.                 
019400     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '304'.                 
019500     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '306'.                 
019600     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '307'.                 
019700     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
019800     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
019900     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
020000     SKIP3                                                                
020100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020200*   -COPY WMEDAREA                                                        
020300     EJECT                                                                
020400*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
020500 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
020600*   -COPY W335PRIS                                                        
020700     EJECT                                                                
020800 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
020900*   -COPY W335PRNO                                                        
021000     EJECT                                                                
021100 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
021200*   -COPY W335PRQU                                                        
021300     EJECT                                                                
021400 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
021500*   -COPY W411SPAR                                                        
021600     EJECT                                                                
021700 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
021800*   -COPY W411NDCA                                                        
021900     EJECT                                                                
022000 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
022100*   -COPY W411XDCA                                                        
022200     EJECT                                                                
022300 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
022400*   -COPY W411XDK7 -PRE NDCA-                                             
022500     EJECT                                                                
022600 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
022700*   -COPY W411SDCA                                                        
022800     EJECT                                                                
022900 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
023000*   -COPY W411LAST                                                        
023100     EJECT                                                                
023200 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
023300*   -COPY W411ORFK                                                        
023400     EJECT                                                                
023500 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
023600*   -COPY W411AREG                                                        
023700     EJECT                                                                
023800 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
023900*   -COPY W413AVSR                                                        
024000     SKIP2                                                                
024100 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
024200*   -COPY W413ADRS                                                        
024300     EJECT                                                                
024400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
024600     SKIP3                                                                
024700*01  -COPY WZ01SUB                                                        
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025000     SKIP3                                                                
025100 01  REQU-AREA.                                                           
025200*    03  -COPY WZ01REQU                                                   
025300*    03  -COPY WL0166I1                                                   
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
025600     SKIP3                                                                
025700 01  RESP-AREA.                                                           
025800*    03  -COPY WZ01RESP                                                   
025900*    03  -COPY WL0166O1                                                   
026000*    03  -COPY WL0167I1         -PRE 0167-                                
026100     SKIP3                                                                
026200*01  -COPY WL01TIDZ                                                       
026300     EJECT                                                                
026400***********************************************************               
026500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026700 01  NYCKLAR-TILL-DLI.                                                    
026800                                                                          
026900     03  W-IDGMTREF-X.                                                    
027000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
027100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
027200         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
027300                                                                          
027400     03  W-WDQ101KY-MIN-X.                                                
027500         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
027600         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
027700         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
027800         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
027900         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
028000                                                                          
028100     03  W-WDQ101KY-MAX-X.                                                
028200         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
028300         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
028400         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
028500         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
028600         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
028700                                                                          
028800     03  W-IDDC-X.                                                        
028900         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
029000                                                                          
029100     03  W-IDARTNR-X.                                                     
029200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029300     EJECT                                                                
029400                                                                          
029500     03  W-4542KEY-MIN-X.                                                 
029600         05  W-IDDISTR-4542-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
029700         05  W-IDANSK-4542-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
029800         05  W-IDARTNR-4542-MIN  PIC S9(9)   VALUE ZERO COMP-3.           
029900         05  W-IDLOPNR-4542-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
030000         05  W-IDORDER-4542-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
030100                                                                          
030200     03  W-KDVORATG-X.                                                    
030300         05  W-KDVORATG         PIC  X      VALUE '1'.                    
030400                                                                          
030500     03  W-4542KEY-MAX-X.                                                 
030600         05  W-IDDISTR-4542-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
030700         05  W-IDANSK-4542-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
030800         05  W-IDARTNR-4542-MAX  PIC S9(9)   VALUE ZERO COMP-3.           
030900         05  W-IDLOPNR-4542-MAX  PIC S9(3)   VALUE ZERO COMP-3.           
031000         05  W-IDORDER-4542-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
031100                                                                          
031200     03  W-IDHTYP-X.                                                      
031300         05  W-IDHTYP            PIC  X(4)   VALUE '4541'.                
031400         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
031500                                                                          
031600     03  W-IDGMT-X.                                                       
031700         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
031800         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
031900*                                                                         
032000     03  W-WDB101KY-X.                                                    
032100         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
032200         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
032300                                                                          
032400     03  W-IDDC-B6-X.                                                     
032500         05 W-IDDC-B6                  PIC X(2) VALUE SPACE.              
032600                                                                          
032700*                                                                         
032800     EJECT                                                                
032900 77  FILLER                      PIC X(08)   VALUE 'GGGGGGGG'.            
033000                                                                          
033100*    --- STATUS-KOD FRÅN IMS                                              
033200 01  STATUS-WS                   PIC XX.                                  
033300     88  SEGMENT-FINNS                       VALUE '  '.                  
033400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033600     88  BASEN-SLUT                          VALUE 'GB'.                  
033700     SKIP2                                                                
033800 01  GODK-STATUSKODER.                                                    
033900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034000                                                                          
034100 01  SSA1                        PIC X(96).                               
034200 01  SSA2                        PIC X(96).                               
034300     EJECT                                                                
034400*    --- IMS FUNKTIONSKODER                                               
034500*01  -COPY W0003                                                          
034600     EJECT                                                                
034700*    ---  DLI INPUT-OUTPUT AREA                                           
034800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
034900     SKIP3                                                                
035000 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
035100 01  DLI-IO-AREA-OBKR.                                                    
035200     03  WLORQM01.                                                        
035300*        05  -COPY WDQ101                                                 
035400     EJECT                                                                
035500 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
035600 01  DLI-IO-AREA-OHUV.                                                    
035700     03  WLORQI01.                                                        
035800*        05  -COPY WDQ201                                                 
035900     EJECT                                                                
036000 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
036100 01  DLI-IO-AREA-ARB.                                                     
036200     03  WLORQI12.                                                        
036300*        05  -COPY WDQ212                                                 
036400     EJECT                                                                
036500 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
036600 01  DLI-IO-AREA-ORAD.                                                    
036700     03  WLORQF01.                                                        
036800*        05  -COPY WDQ401                                                 
036900     EJECT                                                                
037000 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
037100 01  DLI-IO-AREA-ART.                                                     
037200     03  WLARTM01.                                                        
037300*        05  -COPY WDK901                                                 
037400     EJECT                                                                
037500 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
037600 01  DLI-IO-AREA-WDK7.                                                    
037700*    03  -COPY WDK711                                                     
037800     EJECT                                                                
037900                                                                          
038000 01  DLI-IO-AREA-VOR.                                                     
038100     03  WL454111.                                                        
038200*        05  -COPY WDGX4542                                               
038300     EJECT                                                                
038400 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
038500 01  DLI-IO-AREA-WDB201.                                                  
038600*    03  -COPY WDB201                                                     
038700     EJECT                                                                
038800 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
038900 01  DLI-IO-AREA-WDB101.                                                  
039000     03  WLBETC01.                                                        
039100         05  -COPY WDB101                                                 
039200     EJECT                                                                
039300 01  FILLER                      PIC X(16)   VALUE 'WDK601-AREA'.         
039400 01  DLI-IO-AREA-WDK601.                                                  
039500     03  WDK611.                                                          
039600*        05  -COPY WDK601                                                 
039700                                                                          
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
040000 01  DLI-IO-AREA-WDK611.                                                  
040100     03  WDK611.                                                          
040200*        05  -COPY WDK611                                                 
040300                                                                          
040400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040500 01   DLI-IO-AREA-B601.                                                   
040600*     03  -COPY WDB601                                                    
040700     EJECT                                                                
040800 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
040900 01   DLI-IO-AREA-R601.                                                   
041000*     03  -COPY WDR601                                                    
041100*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
041200     EJECT                                                                
041300                                                                          
041400*---MSG-AREA FÖR HOPP TILL 4233-SVARSBILDEN                               
041500*                                                                         
041600*01  FILLER                  PIC X(16)  VALUE '4233-MSG-IO-AREA'.         
041700*01  4233-MSG-IO-AREA.                                                    
041800*    03  4233-LL               PIC S9(4)  VALUE +32  COMP SYNC.           
041900*    03  4233-Z1               PIC X.                                     
042000*    03  4233-Z2               PIC X.                                     
042100*    03  4233-TRANSKOD         PIC X(8)   VALUE 'W4T233  '.               
042200*    03  4233-IDTRANS          PIC X(4)   VALUE '4232'.                   
042300*    03  4233-SPRAK            PIC X.                                     
042400*    03  4233-IDDISTR-IN       PIC X(4).                                  
042500*    03  4233-IDKUNDNR-IN      PIC X(6).                                  
042600*    03  4233-IDORDNR-IN       PIC X(5).                                  
042700*    EJECT                                                                
042800                                                                          
042900*--MSG-AREA FÖR HOPP TILL 4292-ORDERANNULLATION                           
043000                                                                          
043100 01  FILLER                  PIC X(16)  VALUE '4292-MSG-IO-AREA'.         
043200 01  4292-MSG-IO-AREA.                                                    
043300     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
043400     03  4292-Z1               PIC X.                                     
043500     03  4292-Z2               PIC X.                                     
043600     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
043700     03  4292-IDTRANS          PIC X(4)   VALUE '4233'.                   
043800     03  4292-SPRAK            PIC X.                                     
043900     03  4292-IDORDER          PIC X(7).                                  
044000     03  4292-IDDISTR          PIC X(4).                                  
044100     03  4292-IDKUNDNR         PIC X(6).                                  
044200     03  4292-IDKUNDRF         PIC X(7).                                  
044300     03  FILLER                PIC X(6)   VALUE SPACE.                    
044400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
044500     SKIP3                                                                
044600 01  -COPY WZ01SEND                                                       
044700     EJECT                                                                
044800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
044900     SKIP3                                                                
045000 01  SEND-AREA.                                                           
045100*    03  -COPY WZ01REQU  -PRE 3039-                                       
045200*    03  -COPY W30391I1  -PRE 3039-                                       
045300     EJECT                                                                
045400 77  FILLER                      PIC X(08)   VALUE 'LINKAGES'.            
045500 LINKAGE SECTION.                                                         
045600                                                                          
045700*01  -COPY W0009   -PRE MSG-                                              
045800                                                                          
045900 01  AVSR-ALT-PCB                PIC X.                                   
046000*01  -COPY W0009   -PRE 4292-                                             
046100     EJECT                                                                
046200                                                                          
046300*01  -COPY W0009   -PRE PRQRY-                                            
046400     EJECT                                                                
046500*01  -COPY W0008   -PRE ORQM-                                             
046600     05  FILLER                  PIC X.                                   
046700     SKIP2                                                                
046800*01  -COPY W0008   -PRE ORQI-                                             
046900     05  FILLER                  PIC X.                                   
047000     EJECT                                                                
047100*01  -COPY W0008   -PRE ORQF-                                             
047200     05  FILLER                  PIC X.                                   
047300     EJECT                                                                
047400*01  -COPY W0008   -PRE ARTM-                                             
047500     05  FILLER                  PIC X.                                   
047600     EJECT                                                                
047700*01  -COPY W0008   -PRE 4541-                                             
047800     05  FILLER                  PIC X.                                   
047900     EJECT                                                                
048000*01  -COPY W0008   -PRE WDB2-                                             
048100     05  FILLER              PIC X.                                       
048200     EJECT                                                                
048300*01  -COPY W0008   -PRE WDB1-                                             
048400     05  FILLER                  PIC X.                                   
048500     EJECT                                                                
048600*01  -COPY W0008   -PRE WDB6-                                             
048700     05  FILLER                  PIC X.                                   
048800     EJECT                                                                
048900*01  -COPY W0008   -PRE WDK6-                                             
049000     05  FILLER                  PIC X.                                   
049100     EJECT                                                                
049200*01  -COPY W0008   -PRE WDR6-                                             
049300     05  FILLER                  PIC X.                                   
049400     EJECT                                                                
049500 01  PRIS-ARTC-PCB               PIC X.                                   
049600 01  PRIS-WDK7-PCB               PIC X.                                   
049700 01  PRIS-GMTA-PCB               PIC X.                                   
049800 01  PRIS-BETA-PCB               PIC X.                                   
049900 01  PRIS-GPRIA-PCB              PIC X.                                   
050000 01  PRIS-GPRIB-PCB              PIC X.                                   
050100 01  PRIS-COST-WDK6-PCB          PIC X.                                   
050200 01  PRIS-COST-WDK7-PCB          PIC X.                                   
050300 01  PRIS-COST-WDF1-PCB          PIC X.                                   
050400 01  PRIS-COST-9305-PCB          PIC X.                                   
050500 01  PRIS-COST-WDK72-PCB         PIC X.                                   
050600 01  PRIS-COST-WDB6-PCB          PIC X.                                   
050700 01  PRNO-3107-PCB               PIC X.                                   
050800 01  PRQU-WDG2-PCB               PIC X.                                   
050900 01  PRQU-WDC7-PCB               PIC X.                                   
051000 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
051100 01  AREG-WDK6-PCB               PIC X.                                   
051200 01  AREG-WDK7-PCB               PIC X.                                   
051300 01  NDCA-USEA-PCB               PIC X.                                   
051400 01  NDCA-WDK7-PCB               PIC X.                                   
051500 01  NDCA-WDL6-PCB               PIC X.                                   
051600 01  NDCA-WDB6-PCB               PIC X.                                   
051700 01  SDCA-ARTS-PCB               PIC X.                                   
051800 01  SDCA-WDB6-PCB               PIC X.                                   
051900 01  SDCA-WDK9-PCB               PIC X.                                   
052000 01  SDCA-WDR6-PCB               PIC X.                                   
052100 01  SDCA-WDK6-PCB               PIC X.                                   
052200 01  SDCA-WDQ4B-PCB              PIC X.                                   
052300 01  SDCA-WDQ2-PCB               PIC X.                                   
052400 01  SDCA-WDQ4-PCB               PIC X.                                   
052500 01  SDCA-WDB6-2-PCB             PIC X.                                   
052600 01  SDCA-WDK6-2-PCB             PIC X.                                   
052700 01  SDCA-WDK7-2-PCB             PIC X.                                   
052800 01  SDCA-WDK7-3-PCB             PIC X.                                   
052900 01  AVSR-ORQI-PCB               PIC X.                                   
053000 01  AVSR-GMTB-PCB               PIC X.                                   
053100 01  AVSR-GMTC-PCB               PIC X.                                   
053200 01  AVSR-WDB2-PCB               PIC X.                                   
053300 01  AVSR-WDB6-PCB               PIC X.                                   
053400 01  TRAN-XXKB-PCB               PIC X.                                   
053500 01  SPAR-WDF8-PCB               PIC X.                                   
053600 01  SPAR-WDF8A-PCB              PIC X.                                   
053700 01  SPAR-WDK6-PCB               PIC X.                                   
053800 01  XDCA-USEA-PCB               PIC X.                                   
053900 01  XDCA-WDB6-PCB               PIC X.                                   
054000 01  XDCA-WDK6-PCB               PIC X.                                   
054100 01  XDCA-WDK7-PCB               PIC X.                                   
054200 01  XDCA-WDK9-PCB               PIC X.                                   
054300 01  XDCA-WDL6-PCB               PIC X.                                   
054400 01  XDCA-WDQ4B-PCB              PIC X.                                   
054500 01  XDCA-WDQ2-PCB               PIC X.                                   
054600 01  XDCA-WDQ4-PCB               PIC X.                                   
054700 01  XDCA-WDR6-PCB               PIC X.                                   
054800 01  XDCA-WDB6-2-PCB             PIC X.                                   
054900 01  XDCA-WDK6-2-PCB             PIC X.                                   
055000 01  XDCA-WDK7-2-PCB             PIC X.                                   
055100 01  XDCA-WDK7-3-PCB             PIC X.                                   
055200 PROCEDURE DIVISION  USING MSG-PCB 4292-PCB AVSR-ALT-PCB PRQRY-PCB        
055300        ORQM-PCB ORQI-PCB ORQF-PCB                                        
055400        ARTM-PCB 4541-PCB                                                 
055500        WDB2-PCB WDB1-PCB WDB6-PCB WDK6-PCB WDR6-PCB                      
055600        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
055700        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
055800        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
055900        PRIS-COST-WDK6-PCB                                                
056000        PRIS-COST-WDK7-PCB                                                
056100        PRIS-COST-WDF1-PCB                                                
056200        PRIS-COST-9305-PCB                                                
056300        PRIS-COST-WDK72-PCB                                               
056400        PRIS-COST-WDB6-PCB                                                
056500        PRNO-3107-PCB                                                     
056600        PRQU-WDG2-PCB                                                     
056700        PRQU-WDC7-PCB                                                     
056800        PRQU-SJKO-WDK6-PCB                                                
056900        AREG-WDK6-PCB                                                     
057000        AREG-WDK7-PCB                                                     
057100        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
057200        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
057300        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
057400        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
057500        SDCA-WDK7-3-PCB                                                   
057600        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
057700        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
057800        TRAN-XXKB-PCB                                                     
057900        SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB                        
058000        XDCA-USEA-PCB                                                     
058100        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
058200        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
058300        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
058400        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
058500        XDCA-WDK7-3-PCB.                                                  
058600                                                                          
058700     ENTRY 'DLITCBL' USING MSG-PCB 4292-PCB AVSR-ALT-PCB PRQRY-PCB        
058800        ORQM-PCB ORQI-PCB ORQF-PCB                                        
058900        ARTM-PCB 4541-PCB                                                 
059000        WDB2-PCB WDB1-PCB WDB6-PCB WDK6-PCB WDR6-PCB                      
059100        PRIS-ARTC-PCB PRIS-WDK7-PCB                                       
059200        PRIS-GMTA-PCB PRIS-BETA-PCB                                       
059300        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
059400        PRIS-COST-WDK6-PCB                                                
059500        PRIS-COST-WDK7-PCB                                                
059600        PRIS-COST-WDF1-PCB                                                
059700        PRIS-COST-9305-PCB                                                
059800        PRIS-COST-WDK72-PCB                                               
059900        PRIS-COST-WDB6-PCB                                                
060000        PRNO-3107-PCB                                                     
060100        PRQU-WDG2-PCB                                                     
060200        PRQU-WDC7-PCB                                                     
060300        PRQU-SJKO-WDK6-PCB                                                
060400        AREG-WDK6-PCB                                                     
060500        AREG-WDK7-PCB                                                     
060600        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
060700        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
060800        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
060900        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
061000        SDCA-WDK7-3-PCB                                                   
061100        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
061200        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
061300        TRAN-XXKB-PCB                                                     
061400        SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB                        
061500        XDCA-USEA-PCB                                                     
061600        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
061700        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
061800        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
061900        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
062000        XDCA-WDK7-3-PCB.                                                  
062100     SKIP2                                                                
062200                                                                          
062300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
062400     IF SUB-KDRC = 0                                                      
062500        PERFORM A-INIT                                                    
062600        PERFORM B-KOLLA-NYCKLAR-OCH-ANNULL                                
062700        IF ALLT-OK                                                        
062800           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
062900           IF ALLT-OK                                                     
063000              PERFORM D-FORMELL-KONTROLL                                  
063100              IF ALLT-OK                                                  
063200                IF REQU-KDPGMACT = 'E'                                    
063300                   IF REQU-FLANNULL = 'J'                                 
063400                      PERFORM G-ANNULLERA-ORDER                           
063500                   ELSE                                                   
063600                      PERFORM E-BEHANDLA-RADER                            
063700                   END-IF                                                 
063800                END-IF                                                    
063900              END-IF                                                      
064000           END-IF                                                         
064100        END-IF                                                            
064200        IF ALLT-OK                                                        
064300        AND REQU-FLANNULL NOT = 'J'                                       
064400           IF REQU-IDARTNR-006(WS-INDEX-100-MAX) = ALL '+'                
064500              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
064600                PERFORM H-SKICKA-PRISFRAGA                                
064700              END-IF                                                      
064800              PERFORM F-HOPPA-TILL-SVARSBILD                              
064900           ELSE                                                           
065000              MOVE ZERO      TO RESP-KVRADER                              
065100           END-IF                                                         
065200        END-IF                                                            
065300        PERFORM S02-RETURN-RESPONSE                                       
065400     END-IF                                                               
065500                                                                          
065600     MOVE +0 TO RETURN-CODE                                               
065700     GOBACK                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 A-INIT SECTION.                                                          
066100     MOVE 'STA A-INIT        ' TO PGM-POS                                 
066200                                                                          
066300     MOVE ALL '+'              TO RESP-AREA                               
066400     MOVE 001                  TO RESP-IDMSGVER                           
066500     MOVE SPACE                TO RESP-IDMSG-ERROR                        
066600                                  RESP-IDMSG-INFO                         
066700                                  RESP-IDELMT-ERROR                       
066800                                                                          
066900       MOVE +2                 TO SPRAK-IX                                
067000                                                                          
067100     ACCEPT DAGENS-DATUM FROM DATE                                        
067200     ACCEPT DAGENS-TID   FROM TIME                                        
067300     PERFORM AA-NOLLA-WOPS-TABELL                                         
067400     MOVE 'END A-INIT                     ' TO PGM-POS                    
067500     .                                                                    
067600     EJECT                                                                
067700 AA-NOLLA-WOPS-TABELL SECTION.                                            
067800     MOVE 'STA AA-NOLLA-WOPS-TABELL       ' TO PGM-POS                    
067900                                                                          
068000     MOVE +1                   TO WS-INDEX-WOPS                           
068100     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
068200        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
068300        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
068400        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
068500        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
068600        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
068700        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
068800        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
068900        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
069000        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
069100        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
069200        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
069300        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
069400                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
069500                                                                          
069600        ADD +1                 TO WS-INDEX-WOPS                           
069700     END-PERFORM                                                          
069800                                                                          
069900     MOVE +1                   TO WS-INDEX-WOPS                           
070000     MOVE 'END AA-NOLLA-WOPS-TABELL       ' TO PGM-POS                    
070100     .                                                                    
070200     EJECT                                                                
070300 B-KOLLA-NYCKLAR-OCH-ANNULL SECTION.                                      
070400     MOVE 'STA B-KOLLA-NYCKLAR            ' TO PGM-POS                    
070500                                                                          
070600     MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                           
070700                                                                          
070800     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
070900        MOVE REQU-IDDISTR-KEY  TO WS-IDDISTR                              
071000     END-IF                                                               
071100                                                                          
071200     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
071300        MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                             
071400     END-IF                                                               
071500                                                                          
071600     IF REQU-IDORDNR-KEY NOT = ALL '+'                                    
071700        MOVE REQU-IDORDNR-KEY  TO WS-IDORDNR                              
071800     END-IF                                                               
071900                                                                          
072000     EJECT                                                                
072100                                                                          
072200     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
072300        MOVE WS-IDDISTR           TO W-IDDISTR                            
072400                                     TEST-IDDISTR                         
072500     ELSE                                                                 
072600        MOVE NEJ                  TO ALLT-SW                              
072700     END-IF                                                               
072800                                                                          
072900     IF WS-IDKUNDNR NUMERIC                                               
073000        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
073100     ELSE                                                                 
073200        MOVE NEJ                  TO ALLT-SW                              
073300     END-IF                                                               
073400                                                                          
073500     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
073600        MOVE WS-IDORDNR           TO WS-NUM-7                             
073700        MOVE WS-NUM-7             TO W-IDKUNDRF                           
073800     ELSE                                                                 
073900        MOVE NEJ                  TO ALLT-SW                              
074000     END-IF                                                               
074100                                                                          
074200     IF NOT ALLT-OK                                                       
074300        MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                     
074400     END-IF                                                               
074500     EJECT                                                                
074600                                                                          
074700     IF ALLT-OK                                                           
074800       MOVE WS-IDDISTR              TO RESP-IDDISTR-KEY                   
074900                                                                          
075000       IF WS-IDKUNDNR = ZERO                                              
075100         MOVE ZERO                  TO RESP-IDKUNDNR-KEY                  
075200       ELSE                                                               
075300         MOVE WS-IDKUNDNR           TO RESP-IDKUNDNR-KEY                  
075400       END-IF                                                             
075500                                                                          
075600       MOVE WS-IDORDNR              TO RESP-IDORDNR-KEY                   
075700******** ADAPT DATE AND TIME FOR TIMEZONES                                
075800                                                                          
075900       MOVE '011'                TO MSGI-KDCALL                           
076000       MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                         
             MOVE DCS-IDDC             TO MSGI-IDDC                             
076100       MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                         
076200       MOVE DAGENS-TID           TO MSGI-TILOKTID                         
076300       CALL WL01TIDZ USING          MSGI-WL01TIDZ                         
076400       MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                            
076500       MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                         
076600********                                                                  
076700     END-IF                                                               
076800                                                                          
076900     IF ALLT-OK                                                           
077000        IF REQU-FLANNULL NOT = '+'                                        
077100           IF REQU-FLANNULL = 'J' OR 'N'                                  
077200              MOVE JA          TO ALLT-SW                                 
077300           ELSE                                                           
077400              MOVE ERR-UPPLYSTA-FEL TO RESP-IDMSG-ERROR                   
077500              MOVE 'FLANULL'      TO RESP-IDELMT-ERROR                    
077600              MOVE NEJ         TO ALLT-SW                                 
077700           END-IF                                                         
077800        ELSE                                                              
077900           MOVE NEJ            TO RESP-FLANNULL                           
078000        END-IF                                                            
078100     END-IF                                                               
078200                                                                          
078300     MOVE 'END B-KOLLA-NYCKLAR            ' TO PGM-POS                    
078400     .                                                                    
078500     EJECT                                                                
078600 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
078700     MOVE 'STA C-KOLLA-ATT-ORDER-FINNS    ' TO PGM-POS                    
078800                                                                          
078900     PERFORM IMS-01-GU-ORQI-WDQ201                                        
079000     IF SEGMENT-FINNS                                                     
079100       PERFORM CA-HAMTA-KUND                                              
079200       MOVE OHUV-IDDC-TVS            TO W-IDDC-B6                         
079300       PERFORM IMS-GU-WDB601                                              
079400*      VI HAR TVÅNGSSTYRNING                                              
079500*      SÅ INGEN YTTERLIGARE LÄSNING AV WDB6 BLIR NÖDVÄNDIG                
079600       IF OHUV-FLKLAR = JA                                                
079700         MOVE ERR-ORDER-AVSLUTAD     TO RESP-IDMSG-ERROR                  
079800         MOVE NEJ                    TO ALLT-SW                           
079900       ELSE                                                               
080000         IF OHUV-IDSYSTEM NOT = '4231'                                    
080100           MOVE ERR-FEL-BILDSERIE    TO RESP-IDMSG-ERROR                  
080200           MOVE NEJ                  TO ALLT-SW                           
080300         ELSE                                                             
080400                                                                          
080500           IF OHUV-IDUSER NOT = REQU-IDUSER                               
080600              MOVE ERR-OBEHORIG      TO RESP-IDMSG-ERROR                  
080700              MOVE NEJ               TO ALLT-SW                           
080800           ELSE                                                           
080900             MOVE OHUV-KDORDKL       TO RESP-KDORDKL-UT                   
081000                                                                          
081100             MOVE OHUV-IDDC-TVS TO W-IDDC                                 
081200             PERFORM IMS-03-GNP-ORQI-WDQ212                               
081300             MOVE ARB-KDFRAKT        TO RESP-KDFRAKT-UT                   
081400           END-IF                                                         
081500         END-IF                                                           
081600       END-IF                                                             
081700     ELSE                                                                 
081800        MOVE ERR-ORDER-SAKNAS        TO RESP-IDMSG-ERROR                  
081900        MOVE NEJ                     TO ALLT-SW                           
082000     END-IF                                                               
082100                                                                          
082200     MOVE 'END C-KOLLA-ATT-ORDER-FINNS    ' TO PGM-POS                    
082300     .                                                                    
082400     EJECT                                                                
082500                                                                          
082600 CA-HAMTA-KUND      SECTION.                                              
082700                                                                          
082800     MOVE 'STA CA-HAMTA-KUND      ' TO PGM-POS                            
082900                                                                          
083000     MOVE WS-IDDISTR                TO W-IDDISTR-WDB2                     
083100     MOVE WS-IDKUNDNR               TO W-IDKUNDNR-WDB2                    
083200     PERFORM IMS-GU-WDB201                                                
083300                                                                          
083400     IF OHUV-KDORDKL > 1                                                  
083500                                                                          
083600        MOVE +1 TO IX                                                     
083700        PERFORM UNTIL IX       > IX-DCCLEAR-MAX                           
083800           MOVE GMT-IDDC-BULK(IX) TO                                      
083900                                  W-GMT-IDDC-CLEAR  (IX)                  
084000           ADD +1 TO IX                                                   
084100        END-PERFORM                                                       
084200                                                                          
084300     ELSE                                                                 
084400       IF OHUV-KDORDKL = 1                                                
084500                                                                          
084600          MOVE +1 TO IX                                                   
084700          PERFORM UNTIL IX > IX-DCCLEAR-MAX                               
084800             MOVE GMT-IDDC-DAY(IX) TO                                     
084900                                  W-GMT-IDDC-CLEAR  (IX)                  
085000             ADD +1 TO IX                                                 
085100          END-PERFORM                                                     
085200                                                                          
085300       ELSE                                                               
085400         IF OHUV-KDORDKL = 0                                              
085500                                                                          
085600            MOVE +1 TO IX                                                 
085700            PERFORM UNTIL IX > IX-DCCLEAR-MAX                             
085800               MOVE GMT-IDDC-VOR(IX) TO                                   
085900                                  W-GMT-IDDC-CLEAR  (IX)                  
086000               ADD +1 TO IX                                               
086100            END-PERFORM                                                   
086200                                                                          
086300         END-IF                                                           
086400       END-IF                                                             
086500     END-IF                                                               
086600                                                                          
086700     MOVE 'END CA-HAMTA-KUND      '  TO PGM-POS                           
086800     .                                                                    
086900     EJECT                                                                
087000 D-FORMELL-KONTROLL SECTION.                                              
087100     MOVE 'STA D-FORMELL-KONTROLL         ' TO PGM-POS                    
087200                                                                          
087300     MOVE 'WEB '                     TO ORFK-IDSYSTEM                     
087400     MOVE OHUV-IDDC-TVS              TO ORFK-IDDC                         
087500     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
087600     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
087700     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
087800     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
087900     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
088000     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
088100     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
088200     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
088300     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
088400     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
088500     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
088600                                                                          
088700     MOVE +1                   TO WS-INDEX-RESP                           
088800     PERFORM UNTIL WS-INDEX-RESP > WS-INDEX-100-MAX                       
088900        IF REQU-FLINVEST(WS-INDEX-RESP) = ALL '+'                         
089000         MOVE NEJ              TO ORFK-FLINVEST(WS-INDEX-RESP)            
089100*       ELSE                                                              
089200*         IF REQU-FLINVEST(WS-INDEX-RESP) = 'Y'                           
089300*           MOVE JA            TO ORFK-FLINVEST(WS-INDEX-RESP)            
089400*         ELSE                                                            
089500*           MOVE REQU-FLINVEST(WS-INDEX-RESP)                             
089600*                              TO ORFK-FLINVEST(WS-INDEX-RESP)            
089700*         END-IF                                                          
089800        END-IF                                                            
089900                                                                          
090000        MOVE OHUV-FLRESTN      TO ORFK-FLRESTN(WS-INDEX-RESP)             
090100                                                                          
090200                                                                          
090300        IF REQU-IDARTNR-006(WS-INDEX-RESP) = ALL '+'                      
090400          MOVE '+++++++++++'  TO ORFK-IDARTNR-IN(WS-INDEX-RESP)           
090500        ELSE                                                              
090600          MOVE 1 TO START-IX                                              
090700          MOVE 8 TO END-IX                                                
090800          EVALUATE TRUE                                                   
090900          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(1:1) > ZERO                
091000             MOVE 1 TO START-IX                                           
091100             MOVE 8 TO END-IX                                             
091200          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(2:1) > ZERO                
091300             MOVE 2 TO START-IX                                           
091400             MOVE 7 TO END-IX                                             
091500          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(3:1) > ZERO                
091600             MOVE 3 TO START-IX                                           
091700             MOVE 6 TO END-IX                                             
091800          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(4:1) > ZERO                
091900             MOVE 4 TO START-IX                                           
092000             MOVE 5 TO END-IX                                             
092100          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(5:1) > ZERO                
092200             MOVE 5 TO START-IX                                           
092300             MOVE 4 TO END-IX                                             
092400          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(6:1) > ZERO                
092500             MOVE 6 TO START-IX                                           
092600             MOVE 3 TO END-IX                                             
092700          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(7:1) > ZERO                
092800             MOVE 7 TO START-IX                                           
092900             MOVE 2 TO END-IX                                             
093000          WHEN REQU-IDARTNR-006(WS-INDEX-RESP)(8:1) > ZERO                
093100             MOVE 8 TO START-IX                                           
093200             MOVE 1 TO END-IX                                             
093300          END-EVALUATE                                                    
093400                                                                          
093500          MOVE REQU-IDARTNR-006(WS-INDEX-RESP)(START-IX:END-IX)           
093600            TO WS-IDARTNR-911                                             
093700                                                                          
093800          MOVE WS-IDARTNR-911                                             
093900            TO ORFK-IDARTNR-IN(WS-INDEX-RESP)                             
094000        END-IF                                                            
094100                                                                          
094200        MOVE ALL '+'           TO ORFK-KDKVBRYT(WS-INDEX-RESP)            
094300        MOVE OHUV-KDVRINFO     TO ORFK-KDVRINFO(WS-INDEX-RESP)            
094400                                                                          
094500        MOVE REQU-KVBEART(WS-INDEX-RESP)                                  
094600                        TO ORFK-KVBEART(WS-INDEX-RESP)                    
094700                                                                          
094800        IF DIST79-DEALER-PRICE                                            
094900           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-RESP)        
095000           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-RESP)            
095100           MOVE ALL '+'        TO                                         
095200                              ORFK-PRARTNTO-LOCPREL(WS-INDEX-RESP)        
095300        ELSE                                                              
095400           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-RESP)            
095500           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-RESP)        
095600           MOVE ALL '+'        TO                                         
095700                              ORFK-PRARTNTO-LOCPREL(WS-INDEX-RESP)        
095800        END-IF                                                            
095900        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-RESP)        
096000        MOVE ALL '+'           TO ORFK-TITPO-RAD(WS-INDEX-RESP)           
096100                                  ORFK-FLSLATT(WS-INDEX-RESP)             
096200        ADD +1                 TO WS-INDEX-RESP                           
096300     END-PERFORM                                                          
096400                                                                          
096500     CALL W411ORFK USING ORFK-W411ORFK                                    
096600                         AREG-WDK6-PCB                                    
096700                         AREG-WDK7-PCB                                    
096800                                                                          
096900     MOVE +1                   TO WS-INDEX-RESP                           
097000     PERFORM UNTIL WS-INDEX-RESP > WS-INDEX-100-MAX                       
097100          OR REQU-IDARTNR-006(WS-INDEX-RESP) = ALL '+'                    
097200                                                                          
097300        PERFORM DA-KOLLA-FEL-FK                                           
097400                                                                          
097500        ADD +1                 TO WS-INDEX-RESP                           
097600     END-PERFORM                                                          
097700                                                                          
097800     IF ALLT-OK                                                           
097900       IF REQU-IDARTNR-006(1) = ALL '+'                                   
098000         IF REQU-FLANNULL NOT = 'J'                                       
098100           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
098200           MOVE '026'             TO RESP-IDMSG-ERROR                     
098300                                     RESP-IDMSG-ERROR-LINE(1)             
098400           MOVE NEJ               TO ALLT-SW                              
098500         END-IF                                                           
098600       ELSE                                                               
098700         MOVE ORFK-IDARTNR-IN(1)  TO W-IDARTNR                            
098800         PERFORM IMS-GHU-WDK601                                           
098900         IF SEGMENT-SAKNAS                                                
099000           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
099100           MOVE '025'             TO RESP-IDMSG-ERROR                     
099200                                     RESP-IDMSG-ERROR-LINE(1)             
099300           MOVE NEJ               TO ALLT-SW                              
099400         END-IF                                                           
099500       END-IF                                                             
099600     END-IF                                                               
099700                                                                          
099800     COMPUTE WS-KVRADER = WS-INDEX-RESP - 1                               
099900     END-COMPUTE                                                          
100000                                                                          
100100     MOVE WS-KVRADER           TO RESP-KVRADER                            
100200     MOVE WS-KVRADER           TO 0167-REQU-KVRADER                       
100300                                                                          
100400     MOVE 'END D-FORMELL-KONTROLL         ' TO PGM-POS                    
100500     .                                                                    
100600     EJECT                                                                
100700 DA-KOLLA-FEL-FK SECTION.                                                 
100800     MOVE 'STA DA-KOLLA-FEL-FK            ' TO PGM-POS                    
100900                                                                          
101000     IF  REQU-IDARTNR-006(WS-INDEX-RESP) NUMERIC                          
101100     AND REQU-IDARTNR-006(WS-INDEX-RESP) NOT = ALL '+'                    
101200       MOVE REQU-IDARTNR-006(WS-INDEX-RESP)                               
101300         TO RESP-IDARTNR-006(WS-INDEX-RESP)                               
101400     END-IF                                                               
101500     IF   REQU-KVBEART(WS-INDEX-RESP) NUMERIC                             
101600     AND  REQU-KVBEART(WS-INDEX-RESP) NOT = ALL '+'                       
101700       MOVE REQU-KVBEART(WS-INDEX-RESP)                                   
101800         TO RESP-KVBEART(WS-INDEX-RESP)                                   
101900     END-IF                                                               
102000     IF   REQU-BERADREF(WS-INDEX-RESP)  NOT = ALL '+'                     
102100     MOVE REQU-BERADREF(WS-INDEX-RESP)                                    
102200       TO RESP-BERADREF(WS-INDEX-RESP)                                    
102300     END-IF                                                               
102400                                                                          
102500*    IF   REQU-FLINVEST(WS-INDEX-RESP)  NOT = ALL '+'                     
102600*    MOVE REQU-FLINVEST(WS-INDEX-RESP)                                    
102700*      TO RESP-FLINVEST(WS-INDEX-RESP)                                    
102800*    END-IF                                                               
102900                                                                          
103000     IF ORFK-FLINVEST-OK(WS-INDEX-RESP) = NEJ                             
103100        MOVE 'FLINVEST'          TO RESP-IDELMT-ERROR                     
103200        MOVE '023'               TO RESP-IDMSG-ERROR                      
103300                           RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)           
103400        MOVE NEJ                 TO ALLT-SW                               
103500     END-IF                                                               
103600                                                                          
103700     IF ORFK-IDARTNR-OK(WS-INDEX-RESP) = NEJ                              
103800        MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                     
103900        MOVE '023'               TO RESP-IDMSG-ERROR                      
104000                           RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)           
104100        MOVE NEJ                 TO ALLT-SW                               
104200     ELSE                                                                 
104300        IF (ORFK-KDORDBEK(WS-INDEX-RESP) = 58 OR 59)                      
104400          MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                     
104500          MOVE '041'             TO RESP-IDMSG-ERROR                      
104600                           RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)           
104700          MOVE NEJ               TO ALLT-SW                               
104800        END-IF                                                            
104900     END-IF                                                               
105000                                                                          
105100     IF ORFK-KVBEART-OK(WS-INDEX-RESP) = NEJ                              
105200        IF RESP-IDMSG-ERROR = SPACE                                       
105300           IF ORFK-KVBEART(WS-INDEX-RESP) NUMERIC AND                     
105400                   ORFK-KVBEART(WS-INDEX-RESP) > ZERO                     
105500              IF NOT REQU-KDPGMACT = 'E'                                  
105600                 MOVE 'KVBEART'  TO RESP-IDELMT-ERROR                     
105700                 MOVE '298'      TO RESP-IDMSG-ERROR                      
105800                            RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)          
105900                 MOVE NEJ             TO ALLT-SW                          
106000              END-IF                                                      
106100           ELSE                                                           
106200            MOVE 'KVBEART'       TO RESP-IDELMT-ERROR                     
106300            MOVE '023'           TO RESP-IDMSG-ERROR                      
106400                           RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)           
106500            MOVE NEJ                 TO ALLT-SW                           
106600           END-IF                                                         
106700        ELSE                                                              
106800           MOVE 'KVBEART'        TO RESP-IDELMT-ERROR                     
106900           MOVE '023'            TO RESP-IDMSG-ERROR                      
107000                          RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)            
107100           MOVE NEJ              TO ALLT-SW                               
107200        END-IF                                                            
107300     END-IF                                                               
107400     IF ORFK-PRARTNTO-OK(WS-INDEX-RESP) = NEJ                             
107500           MOVE '023'            TO RESP-IDMSG-ERROR                      
107600                          RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)            
107700        MOVE NEJ               TO ALLT-SW                                 
107800     END-IF                                                               
107900     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-RESP) = NEJ                         
108000           MOVE '023'            TO RESP-IDMSG-ERROR                      
108100                          RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)            
108200        MOVE NEJ                 TO ALLT-SW                               
108300     END-IF                                                               
108400     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-RESP) = NEJ                     
108500           MOVE '023'            TO RESP-IDMSG-ERROR                      
108600                          RESP-IDMSG-ERROR-LINE(WS-INDEX-RESP)            
108700        MOVE NEJ                 TO ALLT-SW                               
108800     END-IF                                                               
108900     MOVE 'END DA-KOLLA-FEL-FK            ' TO PGM-POS                    
109000     .                                                                    
109100     EJECT                                                                
109200 E-BEHANDLA-RADER SECTION.                                                
109300     MOVE 'STA E-BEHANDLA-RADER           ' TO PGM-POS                    
109400                                                                          
109500     MOVE NEJ                 TO OBKR-SW                                  
109600     MOVE +1                  TO WS-INDEX-RESP                            
109700     MOVE +0                  TO WS-IDPRQUES                              
109800                                                                          
109900     PERFORM UNTIL WS-INDEX-RESP > WS-INDEX-100-MAX                       
110000          OR REQU-IDARTNR-006(WS-INDEX-RESP) = ALL '+'                    
110100                                                                          
110200        IF REQU-IDARTNR-006(WS-INDEX-RESP) NOT = ALL '+'                  
110300           MOVE ORFK-W411AREG-001(WS-INDEX-RESP) TO                       
110400                         AREG-W411AREG-001                                
110500           PERFORM EC-BEHANDLA-RAD                                        
110600           MOVE NEJ            TO OBKR-SW                                 
110700        END-IF                                                            
110800                                                                          
110900        ADD +1                 TO WS-INDEX-RESP                           
111000     END-PERFORM                                                          
111100                                                                          
111200     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
111300       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
111400       MOVE +3                      TO PRNO-KDCALL                        
111500                                                                          
111600       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
111700     END-IF                                                               
111800     IF AVSR-IDDC(1) > ZERO                                               
111900        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                    
112000             AVSR-ORQI-PCB  AVSR-GMTB-PCB AVSR-GMTC-PCB                   
112100             AVSR-WDB2-PCB  AVSR-WDB6-PCB                                 
112200             TRAN-XXKB-PCB                                                
112300     END-IF                                                               
112400     MOVE JA                      TO ALLT-SW                              
112500     MOVE 'END E-BEHANDLA-RADER           ' TO PGM-POS                    
112600     .                                                                    
112700     EJECT                                                                
112800                                                                          
112900 EC-BEHANDLA-RAD SECTION.                                                 
113000     MOVE 'STA EC-BEHANDLA-RAD            ' TO PGM-POS                    
113100                                                                          
113200     PERFORM ECA-NOLLSTALL-OBKR                                           
113300     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
113400     PERFORM ECJ-KOMPLETTERA-SPARRAR                                      
113500     PERFORM ECF-KOMPLETTERA-PRIS                                         
113600     PERFORM ECE-KOLLA-XDC-RAD                                            
113700     PERFORM ECK-KOLLA-SDC-RAD                                            
113800                                                                          
113900     IF SKRIV-OBKR                                                        
114000        PERFORM ECG-SKRIV-OBKR                                            
114100     ELSE                                                                 
114200                                                                          
114300        PERFORM ECH-KONTROLLERA-ENHETSLAST                                
114400        PERFORM ECI-BERAKNA-WOPS-SKRIV-ORAD                               
114500     END-IF                                                               
114600     MOVE 'END EC-BEHANDLA-RAD            ' TO PGM-POS                    
114700     .                                                                    
114800     EJECT                                                                
114900 ECA-NOLLSTALL-OBKR SECTION.                                              
115000     MOVE 'STA ECA-NOLLSTALL-OBKR         ' TO PGM-POS                    
115100                                                                          
115200     MOVE JA                   TO ALLT-SW                                 
115300     IF ORFK-KDORDBEK(WS-INDEX-RESP) > ZERO                               
115400        MOVE NEJ               TO ALLT-SW                                 
115500        MOVE JA                TO OBKR-SW                                 
115600     END-IF                                                               
115700     MOVE ZERO                 TO SPAR-KDORDBEK                           
115800                                  XDCA-KDORDBEK                           
115900*                                 NDCA-KDORDBEK                           
116000                                  SDCA-KDORDBEK                           
116100     MOVE 'END ECA-NOLLSTALL-OBKR         ' TO PGM-POS                    
116200     .                                                                    
116300     EJECT                                                                
116400 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
116500     MOVE 'STA ECB-BYGG-UPP-ORDERRAD      ' TO PGM-POS                    
116600                                                                          
116700     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
116800     MOVE OHUV-IDDC-TVS        TO ORAD-IDDC                               
116900                                                                          
117000     IF DCS-CDC                                                           
117100        MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                           
117200        MOVE AREG-ADGANG       TO ORAD-ADGANG                             
117300        MOVE AREG-ADPLATS      TO ORAD-ADPLATS                            
117400     ELSE                                                                 
117500        MOVE +0                TO ORAD-ADLAGOMR                           
117600                                  ORAD-ADGANG                             
117700                                  ORAD-ADPLATS                            
117800     END-IF                                                               
117900     MOVE ORFK-IDARTNR(WS-INDEX-RESP)                                     
118000                               TO ORAD-IDARTNR                            
118100     MOVE +1                   TO ORAD-IDLOPNR                            
118200                                                                          
118300     IF REQU-BERADREF(WS-INDEX-RESP) = ALL '+'                            
118400        MOVE SPACE             TO ORAD-BERADREF                           
118500     ELSE                                                                 
118600        MOVE REQU-BERADREF(WS-INDEX-RESP)                                 
118700                               TO ORAD-BERADREF                           
118800     END-IF                                                               
118900                                                                          
119000     IF OHUV-IDKONTO = DCS-IDKONTO-SKROT                                  
119100        MOVE 'INHSCRAP'        TO ORAD-BERADREF                           
119200     ELSE                                                                 
119300       IF OHUV-IDKONTO = DCS-IDKONTO-MIX                                  
119400         MOVE 'MIXSCRAP'       TO ORAD-BERADREF                           
119500       END-IF                                                             
119600     END-IF                                                               
119700                                                                          
119800     MOVE SPACE                TO ORAD-BEVOLREF                           
119900     MOVE SPACE                TO ORAD-FLAKPLOC                           
120000     MOVE NEJ                  TO ORAD-FLSDCLEV                           
120100     IF REQU-FLINVEST(WS-INDEX-RESP) = ALL '+'                            
120200        MOVE NEJ               TO ORAD-FLINVEST                           
120300     ELSE                                                                 
120400        MOVE REQU-FLINVEST(WS-INDEX-RESP)                                 
120500                               TO ORAD-FLINVEST                           
120600     END-IF                                                               
120700     MOVE JA                   TO ORAD-FLOBTRAN                           
120800     MOVE NEJ                  TO ORAD-FLPRTILL                           
120900     MOVE OHUV-FLRESTN         TO ORAD-FLRESTN                            
121000     MOVE NEJ                  TO ORAD-FLTILLK                            
121100     MOVE SPACE                TO ORAD-IDDC-RO                            
121200     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
121300     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
121400     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
121500     MOVE SPACE                TO ORAD-IDLEVNR                            
121600     MOVE +0                   TO ORAD-IDLOPNR-RO                         
121700     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
121800                                                                          
121900     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
122000     MOVE ZERO                 TO ORAD-IDSPECEMB                          
122100     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
122200     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
122300     MOVE OHUV-KDVRINFO        TO ORAD-KDDSP                              
122400     IF ORAD-KDDSP = +1 OR +2                                             
122500        MOVE +2                TO ORAD-KDDSP                              
122600     ELSE                                                                 
122700        MOVE +1                TO ORAD-KDDSP                              
122800     END-IF                                                               
122900     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
123000     MOVE +0                     TO ORAD-KDKVBRYT                         
123100                                                                          
123200     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
123300     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
123400     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
123500                                  TEST-KDPRODSL                           
123600     IF ORAD-KDORDING = +3                                                
123700       MOVE SPACE              TO ORAD-KDOI                               
123800     ELSE                                                                 
123900       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
124000         MOVE 'CD'             TO ORAD-KDOI                               
124100       ELSE                                                               
124200         MOVE 'DT'             TO ORAD-KDOI                               
124300       END-IF                                                             
124400     END-IF                                                               
124500     MOVE SPACE                TO ORAD-CLEARGROUP                         
124600     MOVE SPACE                TO ORAD-KDPRTYP                            
124700     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
124800     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
124900        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
125000     ELSE                                                                 
125100        MOVE +0                TO ORAD-KDTPOTYP                           
125200     END-IF                                                               
125300     MOVE JA                   TO ORAD-FLORDING                           
125400     SKIP2                                                                
125500     MOVE ORFK-KDVRINFO(WS-INDEX-RESP)                                    
125600                               TO ORAD-KDVRINFO                           
125700     MOVE ORFK-KVBEART(WS-INDEX-RESP)                                     
125800                               TO WS-ALFA-6                               
125900     MOVE WS-NUM-6             TO ORAD-KVBEART                            
126000                                  ORAD-KVBEART-Q                          
126100                                                                          
126200     MOVE +0                   TO ORAD-KVPREAVB                           
126300     MOVE +0                   TO ORAD-KVPRERO                            
126400     MOVE +0                   TO ORAD-KVOKS-PREL                         
126500     MOVE +0                   TO ORAD-IDPRQUES                           
126600     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
126700     MOVE +0                   TO ORAD-RERAB                              
126800     MOVE SPACE                TO ORAD-KDVALISO                           
126900     MOVE SPACE                TO ORAD-KDVAT                              
127000     MOVE SPACE                TO ORAD-KDRAB                              
127100     MOVE SPACE                TO ORAD-BEART-VIPS                         
127200                                                                          
127300        MOVE +0                TO ORAD-PRARTNTO                           
127400                                  ORAD-PRARTNTO-LOC                       
127500                                  ORAD-PRARTNTO-LOCPREL                   
127600     MOVE +0                   TO ORAD-PRBPRIS                            
127700     IF ORFK-KDORDBEK(WS-INDEX-RESP) = 59                                 
127800        MOVE REQU-IDARTNR-006(WS-INDEX-RESP)(START-IX:END-IX)             
127900                               TO WS-IDARTNR                              
128000        MOVE WS-IDARTNR-11     TO ORAD-REKSIFFR                           
128100     ELSE                                                                 
128200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
128300     END-IF                                                               
128400     MOVE +0                   TO ORAD-RERF-RAD                           
128500     MOVE +0                   TO ORAD-KVSLATT                            
128600     MOVE +0                   TO ORAD-TIPRIS                             
128700                                                                          
128800     MOVE DAGENS-DATUM                 TO ORAD-TIREGDAT                   
128900     MOVE DAGENS-TID                   TO WS-TIHHMM                       
129000                                                                          
129100     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
129200     MOVE +0                   TO ORAD-TIRODAT                            
129300     MOVE OHUV-TITPO           TO ORAD-TITPO                              
129400     MOVE AREG-VKART           TO ORAD-VKART                              
129500     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
129600     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
129700     MOVE SPACE                TO ORAD-IDBIL                              
129800                                  ORAD-IDKLIENT                           
129900                                  ORAD-IDARBREF                           
130000                                  ORAD-IDVIN                              
130100                                                                          
130200     MOVE SPACE                TO ORAD-IDKUNDRF-WIP                       
130300     MOVE +0                   TO ORAD-PRAVCOST                           
130400     MOVE 'END ECB-BYGG-UPP-ORDERRAD      ' TO PGM-POS                    
130500     .                                                                    
130600     EJECT                                                                
130700 ECJ-KOMPLETTERA-SPARRAR SECTION.                                         
130800     MOVE 'STA ECJ-KOMPLETTERA-SPARRAR    ' TO PGM-POS                    
130900                                                                          
131000     IF ALLT-OK                                                           
131100                                                                          
131200     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
131300     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
131400     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
131500     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
131600     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
131700     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
131800     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
131900     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
132000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
132100     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
132200     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
132300     MOVE AREG-FLIART          TO SPAR-FLIART                             
132400     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
132500     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
132600     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
132700     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
132800     MOVE ORAD-IDDC            TO SPAR-IDDC                               
132900*SOFT OHUVIDSYSTEM TILL SPAR FÖR ATT SPÄRRA PIE ARTIKLAR                  
133000     MOVE OHUV-IDSYSTEM        TO SPAR-IDSYSTEM                           
133100     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
133200     MOVE AREG-KDERS           TO SPAR-KDERS                              
133300     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
133400     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
133500     MOVE +1                   TO SPAR-KDORDBEH                           
133600     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
133700     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
133800     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
133900     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
134000     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
134100     MOVE AREG-KDUART          TO SPAR-KDUART                             
134200     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
134300     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
134400     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
134500     MOVE ORAD-TITPO           TO SPAR-TITPO                              
134600     MOVE NEJ                  TO SPAR-FLSDCLEV                           
134700     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
134800                                                                          
134900     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
135000                                       SPAR-WDF8A-PCB                     
135100                                       SPAR-WDK6-PCB                      
135200                                                                          
135300     IF SPAR-KDORDBEK > +0                                                
135400        MOVE JA                     TO OBKR-SW                            
135500        MOVE NEJ                    TO ALLT-SW                            
135600     END-IF                                                               
135700     IF (OHUV-BEKUNDRF = 'SOFTWARE' AND AREG-KDSORT NOT = 'SW')           
135800     OR (OHUV-BEKUNDRF NOT = 'SOFTWARE' AND AREG-KDSORT = 'SW')           
135900        MOVE 67                     TO SPAR-KDORDBEK                      
136000        MOVE JA                     TO OBKR-SW                            
136100        MOVE NEJ                    TO ALLT-SW                            
136200     END-IF                                                               
136300                                                                          
136400     END-IF                                                               
136500     MOVE 'END ECJ-KOMPLETTERA-SPARRAR    ' TO PGM-POS                    
136600     .                                                                    
136700     EJECT                                                                
136800 ECF-KOMPLETTERA-PRIS SECTION.                                            
136900     MOVE 'STA ECF-KOMPLETTERA-PRIS       ' TO PGM-POS                    
137000                                                                          
137100     IF ALLT-OK                                                           
137200                                                                          
137300     IF DIST79-DEALER-PRICE                                               
137400       IF WS-IDPRQUES                = +0                                 
137500          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
137600          MOVE +1                    TO PRNO-KDCALL                       
137700                                                                          
137800          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
137900                                                                          
138000          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
138100                                        WS-IDPRQUES                       
138200          MOVE +1                    TO PRQU-KDCALL                       
138300       ELSE                                                               
138400          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
138500          MOVE +2                    TO PRNO-KDCALL                       
138600                                                                          
138700          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
138800                                                                          
138900          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
139000                                        WS-IDPRQUES                       
139100          MOVE +2                    TO PRQU-KDCALL                       
139200       END-IF                                                             
139300                                                                          
139400       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
139500       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
139600       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
139700       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
139800       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
139900       MOVE 'N'                      TO PRQU-KDPRSTA                      
140000       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
140100       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
140200                                                                          
140300       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
140400       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
140500       PERFORM IMS-GU-WDB101                                              
140600       MOVE BET-KDVALISO             TO PRQU-KDVALISO                     
140700                                        ORAD-KDVALISO                     
140800       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
140900       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
141000       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
141100                                                                          
141200       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
141300                                          PRQU-WDC7-PCB                   
141400                                          PRQU-SJKO-WDK6-PCB              
141500                                                                          
141600       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
141700                                         WS-IDPRQUES                      
141800       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
141900       IF ORAD-PRARTNTO-LOC = +0                                          
142000          MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL            
142100       END-IF                                                             
142200                                                                          
142300       IF ORAD-PRARTNTO-LOC NOT = +0                                      
142400         IF ORAD-KDPRTYP = SPACE                                          
142500           MOVE 'P'            TO ORAD-KDPRTYP                            
142600           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
142700         END-IF                                                           
142800       END-IF                                                             
142900                                                                          
143000     ELSE                                                                 
143100                                                                          
143200         IF ORAD-PRARTNTO NOT = +0                                        
143300*          *FETCH ONLY KDVALISO FROM W335PRIS                             
143400           MOVE 2                  TO PRIS-KDCALL                         
143500         ELSE                                                             
143600*          *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS            
143700           MOVE 1                  TO PRIS-KDCALL                         
143800         END-IF                                                           
143900                                                                          
144000         MOVE IDPGM                TO PRIS-IDPGM                          
144100         MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                        
144200         MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                        
144300         MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                       
144400         MOVE ORAD-IDDC            TO PRIS-IDDC                           
144500         MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                        
144600         MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                        
144700         MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                       
144800                                                                          
144900         CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                  
145000                             PRIS-WDK7-PCB                                
145100                             PRIS-GMTA-PCB PRIS-BETA-PCB                  
145200                             PRIS-GPRIA-PCB PRIS-GPRIB-PCB                
145300                             PRIS-COST-WDK6-PCB                           
145400                             PRIS-COST-WDK7-PCB                           
145500                             PRIS-COST-WDF1-PCB                           
145600                             PRIS-COST-9305-PCB                           
145700                             PRIS-COST-WDK72-PCB                          
145800                             PRIS-COST-WDB6-PCB                           
145900                                                                          
146000         IF PRIS-KDSVAR = '2'                                             
146100           MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'             
146200                             TO FELTEXT                                   
146300           CALL ABEND USING RKOD-ABEND                                    
146400         END-IF                                                           
146500                                                                          
146600         IF PRIS-KDCALL = 2                                               
146700           MOVE PRIS-KDVALISO        TO ORAD-KDVALISO                     
146800           IF ORAD-KDPRTYP = SPACE                                        
146900             MOVE 'P'                TO ORAD-KDPRTYP                      
147000             MOVE ORAD-TIREGDAT      TO ORAD-TIPRIS                       
147100           END-IF                                                         
147200         ELSE                                                             
147300           MOVE PRIS-PRARTNTO        TO ORAD-PRARTNTO                     
147400           MOVE PRIS-FLPRTILL        TO ORAD-FLPRTILL                     
147500           MOVE PRIS-KDPRTYP         TO ORAD-KDPRTYP                      
147600           MOVE PRIS-PRBPRIS         TO ORAD-PRBPRIS                      
147700           MOVE ORAD-TIREGDAT        TO ORAD-TIPRIS                       
147800           MOVE PRIS-KDVALISO        TO ORAD-KDVALISO                     
147900           MOVE PRIS-PRAVCOST        TO ORAD-PRAVCOST                     
148000         END-IF                                                           
148100     END-IF                                                               
148200     END-IF                                                               
148300     MOVE 'END ECF-KOMPLETTERA-PRIS       ' TO PGM-POS                    
148400     .                                                                    
148500     EJECT                                                                
148600 ECE-KOLLA-XDC-RAD SECTION.                                               
148700     MOVE 'STA ECE-KOLLA-XDC-RAD          ' TO PGM-POS                    
148800                                                                          
148900     IF ALLT-OK AND DCS-NDC                                               
149000       MOVE JA TO ALLT-SW                                                 
149100       PERFORM ECGX-PREL-AVBOKNING-XDC                                    
149200                                                                          
149300*      MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                          
149400*      MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                         
149500*      MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                         
149600*      MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                          
149700*      MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                         
149800*      MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                          
149900*      MOVE ORAD-IDDC            TO NDCA-IDDC                             
150000*      MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                   
150100*      MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                       
150200*      MOVE OHUV-IDDC-TVS        TO NDCA-IDDC-TVS                         
150300*      MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                          
150400*      MOVE 1                    TO NDCA-IXDCCLEAR                        
150500*      MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                         
150600*      MOVE AREG-KDERS           TO NDCA-KDERS                            
150700*      MOVE ORAD-KDORDING        TO NDCA-KDORDING                         
150800*      MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                          
150900*      MOVE AREG-KDSORT          TO NDCA-KDSORT                           
151000*      MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                      
151100*      MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                        
151200*      MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                        
151300*      MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                         
151400*      MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                         
151500*      MOVE ORAD-VKART           TO NDCA-VKART                            
151600*      MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                        
151700*      MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                         
151800*      MOVE +2                   TO NDCA-KDCALL                           
151900*      MOVE SPACE                TO NDCA-XDK7-IDDC                        
152000*      MOVE ZERO                 TO NDCA-XDK7-KDIDDC                      
152100*                                   NDCA-XDK7-KVOKS-DAG                   
152200*                                   NDCA-XDK7-KVOKS-BULK                  
152300                                                                          
152400*      MOVE SPACE                TO CLDC-W411CLDC                         
152500*      MOVE ORAD-IDDC            TO CLDC-IDDC-CLEAR(1)                    
152600*      MOVE DLI-IO-AREA-B601     TO CLDC-WDB601(1)                        
152700*      CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                    
152800*                                        NDCA-USEA-PCB                    
152900*                                        NDCA-WDK7-PCB                    
153000*                                        NDCA-WDL6-PCB                    
153100*                                        NDCA-WDB6-PCB                    
153200*                                        NDCA-XDK7-W411XDK7               
153300                                                                          
153400*      PERFORM ECGX-CHECK-DIFF                                            
153500       IF XDCA-KDORDBEK > ZERO                                            
153600          MOVE JA                  TO OBKR-SW                             
153700       END-IF                                                             
153800       MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                       
153900       MOVE XDCA-ADGANG            TO ORAD-ADGANG                         
154000       MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                        
154100       MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                           
154200       MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                        
154300       MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                       
154400       MOVE XDCA-KDOI              TO ORAD-KDOI                           
154500       MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                     
154600       MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                       
154700       MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                        
154800       MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                       
154900       MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                       
155000       MOVE XDCA-VKART-OUT         TO ORAD-VKART                          
155100       MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                      
155200       MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                       
155300       MOVE NEJ                    TO ALLT-SW                             
155400                                                                          
155500     END-IF                                                               
155600     MOVE 'END ECE-KOLLA-XDC-RAD          ' TO PGM-POS                    
155700     .                                                                    
155800     EJECT                                                                
155900                                                                          
156000 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
156100                                                                          
156200     MOVE 'STA ECGX-XDC      '       TO   PGM-POS                         
156300     IF ALLT-OK                                                           
156400                                                                          
156500       IF DCS-NDC                                                         
156600                                                                          
156700* XDCA-INPUT                                                              
156800         MOVE +1 TO IX                                                    
156900         PERFORM UNTIL IX > IX-DCCLEAR-MAX                                
157000           MOVE W-GMT-IDDC-CLEAR  (IX)                                    
157100                                 TO XDCA-IDDC-CLEAR-IN(IX)                
157200           ADD +1 TO IX                                                   
157300         END-PERFORM                                                      
157400                                                                          
157500         MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                       
157600         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
157700         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
157800         MOVE ORAD-IDDC            TO XDCA-IDDC                           
157900         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
158000         MOVE AREG-KDERS           TO XDCA-KDERS                          
158100         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
158200         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
158300         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
158400         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
158500         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
158600         MOVE ORAD-VKART           TO XDCA-VKART                          
158700         MOVE +1                   TO XDCA-KDCALL                         
158800                                                                          
158900* XDCA-OUTPUT                                                             
159000         MOVE SPACE                TO XDCA-IDDC-OUT                       
159100                                      XDCA-IDDC-RO                        
159200                                      XDCA-KDARTURS                       
159300                                      XDCA-KDOI                           
159400                                      XDCA-CLEARGROUP                     
159500         MOVE ZERO                 TO XDCA-ADLAGOMR                       
159600                                      XDCA-ADGANG                         
159700                                      XDCA-ADPLATS                        
159800                                      XDCA-KDORDBEK                       
159900                                      XDCA-KVPREAVB                       
160000                                      XDCA-KVPRERO                        
160100                                      XDCA-TIREGDAT-OUT                   
160200                                      XDCA-TIREGTID-OUT                   
160300                                      XDCA-VKART-OUT                      
160400                                      XDCA-VKART-NTO                      
160500                                      XDCA-VLARTNTO                       
160600                                      XDCA-DAPUBL                         
160700         MOVE ZERO                 TO                                     
160800                                      XDCA-KVOKS-DAG                      
160900                                      XDCA-KVOKS-BULK                     
161000                                                                          
161100         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
161200         XDCA-USEA-PCB                                                    
161300         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
161400         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
161500         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
161600         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
161700         XDCA-WDK7-3-PCB                                                  
161800                                                                          
161900* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
162000* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
162100         IF XDCA-KDARTURS = SPACE                                         
162200           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
162300         END-IF                                                           
162400         IF XDCA-VKART-NTO = ZERO                                         
162500           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
162600         END-IF                                                           
162700         IF XDCA-VLARTNTO = ZERO                                          
162800           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
162900         END-IF                                                           
163000       END-IF                                                             
163100     END-IF                                                               
163200     .                                                                    
163300     EJECT                                                                
163400 ECGX-CHECK-DIFF SECTION.                                                 
163500                                                                          
163600     MOVE 'CHECK-DIFF       '              TO   PGM-POS                   
163700     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
163800     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
163900     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
164000     AND NDCA-ADGANG     = XDCA-ADGANG                                    
164100     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
164200     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
164300     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
164400     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
164500     AND NDCA-KDOI       = XDCA-KDOI                                      
164600     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
164700     AND NDCA-VKART      = XDCA-VKART-OUT                                 
164800     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
164900     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
165000     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
165100     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
165200     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
165300     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
165400         MOVE NEJ TO DIFF-FLSVAR                                          
165500     ELSE                                                                 
165600        MOVE JA           TO DIFF-FLSVAR                                  
165700     END-IF                                                               
165800                                                                          
165900* ORDER LOG INFO                                                          
166000     IF DIFF-FLSVAR = JA                                                  
166100       MOVE IDPGM         TO FIL-IDPGM                                    
166200       ACCEPT FIL-TIREGDAT FROM DATE                                      
166300       ACCEPT FIL-TIKLOCK FROM TIME                                       
166400       MOVE 1             TO FIL-IDSEKVNR                                 
166500       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
166600       MOVE 'A'           TO FIL-CT-IDVTYP                                
166700       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
166800                                                                          
166900*   ORDER LINE INFO                                                       
167000       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
167100       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
167200       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
167300       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
167400       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
167500       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
167600       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
167700       MOVE 'W016'         TO DIFF-IDSYSTEM                               
167800                                                                          
167900*   NDCA INFO                                                             
168000       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
168100       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
168200       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
168300       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
168400       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
168500       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
168600       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
168700       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
168800       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
168900       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
169000       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
169100       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
169200       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
169300       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
169400       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
169500       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
169600                                                                          
169700*   XDCA INFO                                                             
169800       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
169900       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
170000       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
170100       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
170200       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
170300       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
170400       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
170500       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
170600       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
170700       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
170800       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
170900       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
171000       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
171100       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
171200       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
171300       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
171400                                                                          
171500       PERFORM IMS-ISRT-WDR601                                            
171600       IF SEGMENT-FINNS-REDAN                                             
171700          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
171800             ADD 1 TO FIL-IDSEKVNR                                        
171900             PERFORM IMS-ISRT-WDR601                                      
172000          END-PERFORM                                                     
172100       END-IF                                                             
172200     END-IF                                                               
172300     .                                                                    
172400     EJECT                                                                
172500 ECK-KOLLA-SDC-RAD SECTION.                                               
172600     MOVE 'STA ECK-KOLLA-SDC-RAD          ' TO PGM-POS                    
172700                                                                          
172800     IF ALLT-OK AND (DCS-SDC OR DIST35-REFILL-NA-JAP)                     
172900                                                                          
173000       MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                          
173100       MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                          
173200       MOVE JA                   TO SDCA-FLORDSPE                         
173300       MOVE AREG-FLREFILL        TO SDCA-FLREFILL                         
173400       MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                          
173500       MOVE ORAD-IDDC            TO SDCA-IDDC                             
173600       MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                         
173700       MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                          
173800       MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                          
173900       MOVE ORAD-KDORDING        TO SDCA-KDORDING                         
174000       MOVE AREG-KDSORT          TO SDCA-KDSORT                           
174100       MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                         
174200       MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                        
174300       MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                        
174400       MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                         
174500       MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                         
174600       MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                         
174700       MOVE +0                   TO SDCA-TIREPDAT                         
174800       MOVE +0                   TO SDCA-KVOKS-PREL                       
174900       MOVE +1                   TO SDCA-KDCALL                           
175000       MOVE +1                   TO SDCA-IXDCCLEAR                        
175100                                                                          
175200       CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                    
175300                                         SDCA-WDB6-PCB                    
175400                                         SDCA-WDK9-PCB                    
175500                                         SDCA-WDR6-PCB                    
175600                                         SDCA-WDK6-PCB                    
175700                                         SDCA-WDQ4B-PCB                   
175800                                         SDCA-WDQ2-PCB                    
175900                                         SDCA-WDQ4-PCB                    
176000                                         SDCA-WDB6-2-PCB                  
176100                                         SDCA-WDK6-2-PCB                  
176200                                         SDCA-WDK7-2-PCB                  
176300                                         SDCA-WDK7-3-PCB                  
176400                                                                          
176500       IF SDCA-KDORDBEK > ZERO                                            
176600         MOVE JA                   TO OBKR-SW                             
176700         MOVE NEJ                  TO ALLT-SW                             
176800       ELSE                                                               
176900         MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                       
177000         MOVE SDCA-ADGANG          TO ORAD-ADGANG                         
177100         MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                        
177200       END-IF                                                             
177300       IF DIST35-REFILL-NA-JAP                                            
177400         MOVE 'RE'                 TO ORAD-KDOI                           
177500         MOVE SPACE                TO ORAD-CLEARGROUP                     
177600       ELSE                                                               
177700         MOVE SDCA-KDOI            TO ORAD-KDOI                           
177800         MOVE SDCA-CLEARGROUP      TO ORAD-CLEARGROUP                     
177900       END-IF                                                             
178000     END-IF                                                               
178100     MOVE 'END ECK-KOLLA-SDC-RAD          ' TO PGM-POS                    
178200     .                                                                    
178300     EJECT                                                                
178400 ECG-SKRIV-OBKR SECTION.                                                  
178500     MOVE 'STA ECG-SKRIV-OBKR             ' TO PGM-POS                    
178600                                                                          
178700     PERFORM ECGA-REDIGERA-OBKR-RAD                                       
178800                                                                          
178900     IF ORFK-KDORDBEK(WS-INDEX-RESP) > 0                                  
179000*----(KOD 58, 59)                                                         
179100        MOVE ORFK-KDORDBEK(WS-INDEX-RESP)                                 
179200                               TO OBKR-KDORDBEK                           
179300        MOVE '4232ORFK'        TO OBKR-IDPGM                              
179400        MOVE 'S'               TO OBKR-SW                                 
179500     END-IF                                                               
179600                                                                          
179700     IF SPAR-KDORDBEK > +0                                                
179800*----(KOD 58)                                                             
179900        IF OBKR-SKRIVEN                                                   
180000           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
180100           ADD +1              TO OBKR-IDSEKVNR                           
180200        END-IF                                                            
180300        MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                           
180400        MOVE '4232SPAR'        TO OBKR-IDPGM                              
180500        MOVE 'S'               TO OBKR-SW                                 
180600     END-IF                                                               
180700                                                                          
180800     IF XDCA-KDORDBEK > ZERO                                              
180900*----(KOD 53, 55, 80)                                                     
181000        IF OBKR-SKRIVEN                                                   
181100           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
181200           ADD +1              TO OBKR-IDSEKVNR                           
181300        END-IF                                                            
181400        IF XDCA-KDORDBEK = 80                                             
181500           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
181600        END-IF                                                            
181700        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
181800        MOVE '4232XDCA'        TO OBKR-IDPGM                              
181900        MOVE 'S'               TO OBKR-SW                                 
182000     END-IF                                                               
182100                                                                          
182200     IF SDCA-KDORDBEK > ZERO                                              
182300*----(KOD 53)                                                             
182400        IF OBKR-SKRIVEN                                                   
182500           PERFORM IMS-05-ISRT-WLORQM01-WDQ101                            
182600           ADD +1              TO OBKR-IDSEKVNR                           
182700        END-IF                                                            
182800        IF SDCA-KDORDBEK = 80                                             
182900           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
183000        END-IF                                                            
183100        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
183200        MOVE '4232SDCA'        TO OBKR-IDPGM                              
183300        MOVE 'S'               TO OBKR-SW                                 
183400     END-IF                                                               
183500     MOVE OBKR-KDORDBEK     TO 0167-REQU-KDORDBEK(WS-INDEX-RESP)          
183600     MOVE ORFK-IDARTNR(WS-INDEX-RESP)                                     
183700                            TO 0167-REQU-IDARTNR(WS-INDEX-RESP)           
183800     MOVE ORAD-IDDC         TO 0167-REQU-IDDC-RAD(WS-INDEX-RESP)          
183900     MOVE W-IDLOPNR-Q1-MIN     TO WS-AKT-IDLOPNR                          
184000     MOVE W-IDSEKVNR-Q1-MIN    TO WS-AKT-IDSEKVNR                         
184100     MOVE WS-AKT-KEYS          TO 0167-REQU-KEYS(WS-INDEX-RESP)           
184200                                                                          
184300     IF OBKR-SKRIVEN                                                      
184400        PERFORM IMS-05-ISRT-WLORQM01-WDQ101                               
184500     END-IF                                                               
184600     MOVE 'END ECG-SKRIV-OBKR             ' TO PGM-POS                    
184700     .                                                                    
184800     EJECT                                                                
184900 ECGA-REDIGERA-OBKR-RAD SECTION.                                          
185000     MOVE 'STA ECGA-REDIGERA-OBKR-RAD     ' TO PGM-POS                    
185100                                                                          
185200     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
185300     MOVE ORFK-IDARTNR(WS-INDEX-RESP)                                     
185400                               TO OBKR-IDARTNR                            
185500     MOVE OBKR-IDORDER         TO W-IDORDER-Q1-MIN                        
185600                                  W-IDORDER-Q1-MAX                        
185700     MOVE OBKR-IDARTNR         TO W-IDARTNR-Q1-MIN                        
185800                                  W-IDARTNR-Q1-MAX                        
185900     MOVE +1                   TO W-IDLOPNR-Q1-MIN                        
186000                                  W-IDLOPNR-Q1-MAX                        
186100                                  W-IDSEKVNR-Q1-MIN                       
186200                                  W-IDSEKVNR-Q1-MAX                       
186300     PERFORM IMS-04-GU-ORQM-WDQ101                                        
186400     PERFORM UNTIL SEGMENT-SAKNAS                                         
186500        ADD +1                 TO W-IDLOPNR-Q1-MIN                        
186600                                  W-IDLOPNR-Q1-MAX                        
186700        PERFORM IMS-04-GU-ORQM-WDQ101                                     
186800     END-PERFORM                                                          
186900     MOVE W-IDLOPNR-Q1-MIN     TO OBKR-IDLOPNR                            
187000     MOVE W-IDSEKVNR-Q1-MIN    TO OBKR-IDSEKVNR                           
187100     MOVE ORAD-IDDC            TO OBKR-IDDC                               
187200     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
187300     MOVE +0                   TO OBKR-KDORDBEK                           
187400     MOVE SPACE                TO OBKR-BEERS                              
187500     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
187600     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
187700     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
187800     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
187900     MOVE +0                   TO OBKR-DIERS-KVOT                         
188000     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
188100     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
188200     MOVE NEJ                  TO OBKR-FLOBOK                             
188300     EJECT                                                                
188400     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
188500     MOVE NEJ                  TO OBKR-FLOBPRT                            
188600     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
188700     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
188800     MOVE JA                   TO OBKR-FLSLATT                            
188900     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
189000     MOVE +0                   TO OBKR-IDARTNR-TILLK                      
189100                                  OBKR-REKSIFFR-TILLK                     
189200     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
189300     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
189400     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
189500     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
189600     MOVE 'IMS '               TO OBKR-IDSYSTEM                           
189700     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
189800     MOVE AREG-KDERS           TO OBKR-KDERS                              
189900     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
190000     MOVE ORAD-KDOI            TO OBKR-KDOI                               
190100     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
190200     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
190300     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
190400     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
190500     MOVE +0                   TO OBKR-KVANNANT                           
190600     MOVE +0                   TO OBKR-KVAVBART                           
190700     EJECT                                                                
190800     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
190900     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
191000     MOVE +0                   TO OBKR-KVBEART-TILLK                      
191100     MOVE +0                   TO OBKR-KVPREAVB                           
191200     MOVE +0                   TO OBKR-KVPRERO                            
191300     MOVE AREG-KVQPACK-1       TO OBKR-KVQPACK                            
191400     MOVE +0                   TO OBKR-KVRO                               
191500     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
191600     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
191700     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
191800     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
191900     MOVE ORAD-REKSIFFR        TO OBKR-REKSIFFR                           
192000     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
192100     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
192200     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
192300     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
192400     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
192500     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
192600     MOVE +0                   TO OBKR-TIRODAT                            
192700     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
192800     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
192900       MOVE 20                 TO WS-SEKEL-9KOMPL                         
193000     ELSE                                                                 
193100       MOVE 19                 TO WS-SEKEL-9KOMPL                         
193200     END-IF                                                               
193300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
193400     MOVE ORAD-TITPO           TO OBKR-TITPO                              
193500     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
193600     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
193700     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
193800     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
193900     MOVE SPACE                TO OBKR-IDBIL                              
194000                                                                          
194100     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
194200     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
194300     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
194400     MOVE ZERO                TO OBKR-TIDLEVDAT                           
194500     MOVE ORAD-PRAVCOST       TO OBKR-PRAVCOST                            
194600     MOVE ORAD-KDVALISO       TO OBKR-KDVALISO                            
194700     MOVE 'END ECGA-REDIGERA-OBKR-RAD     ' TO PGM-POS                    
194800     .                                                                    
194900     EJECT                                                                
195000 ECH-KONTROLLERA-ENHETSLAST SECTION.                                      
195100     MOVE 'STA ECH-KONTROLLERA-ENHETLAST  ' TO PGM-POS                    
195200                                                                          
195300     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
195400     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
195500     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
195600     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
195700     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
195800     MOVE ORAD-IDDC            TO LAST-IDDC                               
195900     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
196000     MOVE ORAD-KVBEART-Q       TO LAST-KVPREAVB                           
196100     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
196200     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
196300                                                                          
196400     CALL W411LAST USING LAST-W411LAST                                    
196500     MOVE 'END ECH-KONTROLLERA-ENHETLAST  ' TO PGM-POS                    
196600     .                                                                    
196700     EJECT                                                                
196800 ECI-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
196900     MOVE 'STA ECI-BERAKNA-WOPS-SKRIV-ORA ' TO PGM-POS                    
197000                                                                          
197100     IF LAST-ADLAGOMR-UT = +0 AND                                         
197200        LAST-KVANTAL-UT  = +0 AND                                         
197300        LAST-KVBEART-UT  = +0                                             
197400*------------------------------------------------------------*            
197500*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
197600*------------------------------------------------------------*            
197700        PERFORM ECIA-FIXA-LAGEROMR-PLATS                                  
197800        PERFORM ECIB-REDIGERA-WOPS-AREA                                   
197900        PERFORM IMS-06-ISRT-ORQF-WDQ401                                   
198000                                                                          
198100        PERFORM UNTIL SEGMENT-FINNS                                       
198200           ADD +1                    TO ORAD-IDLOPNR                      
198300           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
198400        END-PERFORM                                                       
198500     ELSE                                                                 
198600*------------------------------------------------------------*            
198700*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
198800*------------------------------------------------------------*            
198900                                                                          
199000        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
199100*------------------------------------------------------------*            
199200*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
199300*------------------------------------------------------------*            
199400                                                                          
199500           MOVE LAST-KVBEART-UT      TO ORAD-KVBEART-Q                    
199600           PERFORM ECIA-FIXA-LAGEROMR-PLATS                               
199700           PERFORM ECIB-REDIGERA-WOPS-AREA                                
199800           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
199900                                                                          
200000           PERFORM UNTIL SEGMENT-FINNS                                    
200100              ADD +1                 TO ORAD-IDLOPNR                      
200200              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
200300           END-PERFORM                                                    
200400     SKIP2                                                                
200500*------------------------------------------------------------*            
200600*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
200700*------------------------------------------------------------*            
200800                                                                          
200900           MOVE +0                   TO ORAD-KVBEART                      
201000           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
201100           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
201200           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
201300             CONTINUE                                                     
201400           ELSE                                                           
201500             IF LAST-ADGANG-UT > ZERO                                     
201600               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
201700             END-IF                                                       
201800           END-IF                                                         
201900           MOVE 1.0000               TO ORAD-RERF-RAD                     
202000           PERFORM ECIB-REDIGERA-WOPS-AREA                                
202100           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
202200                                                                          
202300           PERFORM UNTIL SEGMENT-FINNS                                    
202400              ADD +1                 TO ORAD-IDLOPNR                      
202500              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
202600           END-PERFORM                                                    
202700        ELSE                                                              
202800*------------------------------------------------------------*            
202900*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
203000*------------------------------------------------------------*            
203100           MOVE LAST-ADLAGOMR-UT    TO ORAD-ADLAGOMR                      
203200           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
203300             CONTINUE                                                     
203400           ELSE                                                           
203500             IF LAST-ADGANG-UT > ZERO                                     
203600               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
203700             END-IF                                                       
203800           END-IF                                                         
203900           MOVE 1.0000            TO ORAD-RERF-RAD                        
204000           PERFORM ECIA-FIXA-LAGEROMR-PLATS                               
204100           PERFORM ECIB-REDIGERA-WOPS-AREA                                
204200           PERFORM IMS-06-ISRT-ORQF-WDQ401                                
204300                                                                          
204400           PERFORM UNTIL SEGMENT-FINNS                                    
204500              ADD +1              TO ORAD-IDLOPNR                         
204600              PERFORM IMS-06-ISRT-ORQF-WDQ401                             
204700           END-PERFORM                                                    
204800        END-IF                                                            
204900     END-IF                                                               
205000     MOVE 'END ECI-BERAKNA-WOPS-SKRIV-ORA ' TO PGM-POS                    
205100     .                                                                    
205200     EJECT                                                                
205300 ECIA-FIXA-LAGEROMR-PLATS SECTION.                                        
205400     MOVE 'STA ECIA-FIXA-LAGEROMR-PLATS   ' TO PGM-POS                    
205500                                                                          
205600     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
205700     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
205800     MOVE ORAD-BERADREF        TO ADRS-BEVARREF-IN                        
205900     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
206000     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
206100     MOVE 1                    TO ADRS-KDCALL-IN                          
206200     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
206300     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
206400     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
206500     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
206600                                                                          
206700     CALL W413ADRS USING ADRS-W413ADRS                                    
206800                                                                          
206900     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
207000     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
207100     MOVE 'END ECIA-FIXA-LAGEROMR-PLATS   ' TO PGM-POS                    
207200     .                                                                    
207300     EJECT                                                                
207400 ECIB-REDIGERA-WOPS-AREA SECTION.                                         
207500     MOVE 'STA ECIB-REDIGERA-WOPS-AREA    ' TO PGM-POS                    
207600                                                                          
207700     MOVE +1                   TO AVSR-KDCALL                             
207800     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
207900     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
208000     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
208100     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
208200     MOVE DAGENS-DATUM                 TO AVSR-TIREGDAT                   
208300     MOVE DAGENS-TID                   TO AVSR-TIHHMM                     
208400                                                                          
208500     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
208600     MOVE SPACE                TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
208700     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
208800                                                                          
208900     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
209000     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
209100     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
209200     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
209300     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
209400     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
209500     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
209600     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
209700     MOVE SPACE                TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
209800     MOVE +0                   TO AVSR-KDVIA   (WS-INDEX-WOPS)            
209900     MOVE +0                   TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
210000     MOVE +0                   TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
210100     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
210200     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
210300                                                                          
210400     ADD +1                    TO WS-INDEX-WOPS                           
210500     MOVE 'END ECIB-REDIGERA-WOPS-AREA    ' TO PGM-POS                    
210600     .                                                                    
210700     EJECT                                                                
210800 F-HOPPA-TILL-SVARSBILD SECTION.                                          
210900     MOVE 'STA F-HOPPA-TILL-SVARSBILD     ' TO PGM-POS                    
211000                                                                          
211100                                                                          
211200     MOVE WS-IDDISTR             TO 0167-REQU-IDDISTR-KEY                 
211300                                                                          
211400     MOVE WS-IDKUNDNR            TO 0167-REQU-IDKUNDNR-KEY                
211500                                                                          
211600     MOVE WS-IDORDNR             TO 0167-REQU-IDORDNR-KEY                 
211700                                                                          
211800     MOVE OHUV-KDORDKL           TO 0167-REQU-KDORDKL-UT                  
211900     MOVE 'N'                    TO 0167-REQU-FLANNULL                    
212000     MOVE DCS-IDDC               TO 0167-REQU-IDDC-KEY                    
212100                                                                          
212200     MOVE 'END F-HOPPA-TILL-SVARSBILD     ' TO PGM-POS                    
212300     .                                                                    
212400     EJECT                                                                
212500 G-ANNULLERA-ORDER  SECTION.                                              
212600     MOVE 'STA G-ANNULLERA-ORDERA         ' TO PGM-POS                    
212700                                                                          
212800                                                                          
212900     MOVE '2'                  TO 4292-SPRAK                              
213000     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
213100     MOVE W-IDDISTR            TO 4292-IDDISTR                            
213200     MOVE W-IDKUNDNR           TO 4292-IDKUNDNR                           
213300     MOVE W-IDKUNDRF           TO 4292-IDKUNDRF                           
213400                                                                          
213500     PERFORM IMS-INSERT-4292-MSG                                          
213600                                                                          
213700     MOVE 'END G-ANNULLERA-ORDERABILD     ' TO PGM-POS                    
213800     .                                                                    
213900     EJECT                                                                
214000 H-SKICKA-PRISFRAGA SECTION.                                              
214100     MOVE 'STA H-SKICKA-PRISFRAGA         ' TO PGM-POS                    
214200                                                                          
214300     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
214400     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
214500     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
214600     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
214700     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
214800                                                                          
214900     PERFORM S04-SKICKA-OPEN                                              
215000     PERFORM S04-SKICKA-MEDDELANDE                                        
215100     PERFORM S04-SKICKA-CLOSE                                             
215200                                                                          
215300     MOVE 'END H-SKICKA-PRISFRAGA         ' TO PGM-POS                    
215400     .                                                                    
215500     EJECT                                                                
215600 S04-SKICKA-OPEN SECTION.                                                 
215700     MOVE 'STA S04-SKICKA-OPEN            ' TO PGM-POS                    
215800                                                                          
215900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
216000     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
216100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
216200                                                                          
216300     IF SEND-KDRC > 0                                                     
216400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
216500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
216600       DELIMITED BY SIZE INTO FELTEXT                                     
216700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
216800     END-IF                                                               
216900     MOVE 'END S04-SKICKA-OPEN            ' TO PGM-POS                    
217000     .                                                                    
217100     SKIP3                                                                
217200 S04-SKICKA-MEDDELANDE SECTION.                                           
217300     MOVE 'STA S04-SKICKA-MEDDELANDE      ' TO PGM-POS                    
217400                                                                          
217500     MOVE 'PUT'                      TO SEND-KDFUNC                       
217600     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
217700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
217800                                                                          
217900     IF SEND-KDRC > 0                                                     
218000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
218100       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
218200       DELIMITED BY SIZE INTO FELTEXT                                     
218300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
218400     END-IF                                                               
218500     MOVE 'END S04-SKICKA-MEDDELANDE      ' TO PGM-POS                    
218600     .                                                                    
218700     SKIP3                                                                
218800 S04-SKICKA-CLOSE SECTION.                                                
218900     MOVE 'STA S04-SKICKA-CLOSE           ' TO PGM-POS                    
219000                                                                          
219100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
219200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
219300                                                                          
219400     IF SEND-KDRC > 0                                                     
219500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
219600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
219700       DELIMITED BY SIZE INTO FELTEXT                                     
219800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
219900     END-IF                                                               
220000     MOVE 'END S04-SKICKA-CLOSE           ' TO PGM-POS                    
220100     .                                                                    
220200     EJECT                                                                
220300                                                                          
220400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
220500     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
220600                                                                          
220700     MOVE 'GETARG'               TO SUB-KDFUNC                            
220800     MOVE 'CARPARTS.LDC.SPECORDERLINE'     TO SUB-ADDISPABS               
220900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
221000                                                                          
221100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
221200                                                                          
221300     IF SUB-KDRC > 0                                                      
221400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
221500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
221600       DELIMITED BY SIZE INTO FELTEXT                                     
221700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
221800     END-IF                                                               
221900     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
222000     .                                                                    
222100     SKIP3                                                                
222200 S02-RETURN-RESPONSE SECTION.                                             
222300     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
222400                                                                          
222500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
222600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
222700                                                                          
222800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
222900                                                                          
223000     IF SUB-KDRC > 0                                                      
223100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
223200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
223300       DELIMITED BY SIZE INTO FELTEXT                                     
223400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
223500     END-IF                                                               
223600     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
223700     .                                                                    
223800     EJECT                                                                
223900                                                                          
224000* --- IMS SEKTIONER ---                                                   
224100                                                                          
224200                                                                          
224300 IMS-INSERT-4292-MSG SECTION.                                             
224400     MOVE 'STA IMS-INSERT-4292-MSG'  TO  PGM-POS                          
224500                                                                          
224600     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
224700     MOVE SPACE TO GODK-STATUSKODER                                       
224800     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
224900     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
225000     PERFORM IMS-STATUSKONTROLL                                           
225100     .                                                                    
225200                                                                          
225300                                                                          
225400 IMS-01-GU-ORQI-WDQ201 SECTION.                                           
225500     MOVE 'STA IMS-01-GU-ORQI-WDQ201      ' TO PGM-POS                    
225600                                                                          
225700     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     MOVE '  GE'               TO GODK-STATUSKODER                        
226000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
226100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     SKIP2                                                                
226500 IMS-03-GNP-ORQI-WDQ212 SECTION.                                          
226600     MOVE 'STA IMS-03-GNP-ORQI-WDQ212     ' TO PGM-POS                    
226700                                                                          
226800     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
226900          DELIMITED BY SIZE INTO SSA1                                     
227000     MOVE '    '               TO GODK-STATUSKODER                        
227100     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
227200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500     EJECT                                                                
227600 IMS-04-GU-ORQM-WDQ101 SECTION.                                           
227700     MOVE 'STA IMS-04-GU-ORQM-WDQ101      ' TO PGM-POS                    
227800                                                                          
227900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
228000                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
228100          DELIMITED BY SIZE INTO SSA1                                     
228200     MOVE '  GE'               TO GODK-STATUSKODER                        
228300     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
228400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP2                                                                
228800 IMS-05-ISRT-WLORQM01-WDQ101 SECTION.                                     
228900     MOVE 'STA IMS-05-ISRT-WLORQM01-WDQ101' TO PGM-POS                    
229000                                                                          
229100     MOVE 'WLORQM01 '          TO SSA1                                    
229200     MOVE '    '               TO GODK-STATUSKODER                        
229300     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
229400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
229500     PERFORM IMS-STATUSKONTROLL                                           
229600     .                                                                    
229700     SKIP2                                                                
229800 IMS-06-ISRT-ORQF-WDQ401 SECTION.                                         
229900     MOVE 'STA IMS-06-ISRT-ORQF-WDQ401    ' TO PGM-POS                    
230000                                                                          
230100     MOVE 'WLORQF01 '          TO SSA1                                    
230200     MOVE '  II'               TO GODK-STATUSKODER                        
230300     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
230400     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
230500     PERFORM IMS-STATUSKONTROLL                                           
230600     .                                                                    
230700     EJECT                                                                
230800 IMS-GHU-WDK601 SECTION.                                                  
230900     MOVE 'STA IMS-GHU-WDK601          ' TO PGM-POS                       
231000                                                                          
231100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
231200          DELIMITED BY SIZE INTO SSA1                                     
231300     MOVE '  GE'                 TO GODK-STATUSKODER                      
231400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK601 SSA1              
231500     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
231600     PERFORM IMS-STATUSKONTROLL                                           
231700     .                                                                    
231800     SKIP2                                                                
231900 IMS-GU-WDB201 SECTION.                                                   
232000     MOVE 'STA IMS-GU-WDB201              ' TO PGM-POS                    
232100                                                                          
232200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
232300          DELIMITED BY SIZE INTO SSA1                                     
232400     MOVE '  '                 TO GODK-STATUSKODER                        
232500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
232600     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900     SKIP2                                                                
233000 IMS-GU-WDB101 SECTION.                                                   
233100     MOVE 'STA IMS-GU-WDB101              ' TO PGM-POS                    
233200                                                                          
233300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
233400          DELIMITED BY SIZE INTO SSA1                                     
233500     MOVE '  '               TO GODK-STATUSKODER                          
233600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
233700     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
233800     PERFORM IMS-STATUSKONTROLL                                           
233900     .                                                                    
234000     SKIP2                                                                
234100 IMS-GU-WDB601    SECTION.                                                
234200     MOVE 'STA IMS-GU-WDB601              ' TO PGM-POS                    
234300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
234400          DELIMITED BY SIZE INTO SSA1                                     
234500     MOVE '  '   TO GODK-STATUSKODER                                      
234600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
234700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
234800     PERFORM IMS-STATUSKONTROLL                                           
234900     .                                                                    
235000     SKIP2                                                                
235100 IMS-ISRT-WDR601 SECTION.                                                 
235200                                                                          
235300     MOVE 'WDR601' TO SSA1                                                
235400     MOVE '  II' TO GODK-STATUSKODER                                      
235500     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
235600     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900 IMS-STATUSKONTROLL SECTION.                                              
236000                                                                          
236100     SET STATUS-IX TO 1                                                   
236200     SEARCH GODK-STATUS                                                   
236300       AT END CALL FELLOG                                                 
236400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
236500     END-SEARCH                                                           
236600     .                                                                    
