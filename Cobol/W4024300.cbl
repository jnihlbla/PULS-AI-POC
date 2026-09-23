000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4024300.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000400 DATE-WRITTEN.   APRIL -90.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*                                                                         
000900*        PROGRAMMET HANTERAR SVARSBILD TILL ORDERREGISTRERING             
001000*        4241/4242.                                                       
001100*        VISAR AVVIKELSER.                                                
001200*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001300*        ANNULLATION AV HEL ORDER MÖJLIG.                                 
001400*                                                                         
001500*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
001600*        4241.                                                            
001700*                                                                         
001800*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001900*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
002000*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002100*        PROGRAMMET LÄSER      WLORQA (WDQ4)  ORDERDELAR                  
002200*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002300*        PROGRAMMET LÄSER              WDD6   ARTIKELREGISTER             
002400*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
002500*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002600*        PROGRAMMET LÄSER      WLXXKK (WDR1)  TVÅNGSSTYRN.TAB             
002700*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002800*        PROGRAMMET LÄSER      WLXXKN (WDR1)  TEDTIDS.TAB                 
002900*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
003000*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
003100*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003200*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
003300*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003400*        PROGRAMMET LÄSER      WLXXKR (WDR4)  KAMPANJREGISTER             
003500*        PROGRAMMET LÄSER      WLXXKS (WDR4)  MARKN.REG KAMPANJ           
003600*        PROGRAMMET LÄSER      WLXXKT (WDR4)  ANTALSTAB KAMPANJ           
003700*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003800*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003900*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
004000*        PROGRAMMET LÄSER      WL4437 (RD??)  ARBTIDSKALENDER             
004100*                                                                         
004200*    INDATA.                                                              
004300*        TRANSAKTION: W4T243                                              
004400*                     W4T243U                                             
004500*                     W4T243V                                             
004600*        MID:         W4I24301                                            
004700*                                                                         
004800*    UTDATA.                                                              
004900*        MOD:         W4O24301                                            
005000*                                                                         
005100* CHANGE LOG:                                                             
005200*                                                                         
005300*    E'TRACKER: 5444132 DATED 2007-08-21                                  
005400*    E'TRACKER: 2218613 DATED 2008-03-11                                  
005500*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
005600*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
005700*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
005800*    E'TRACKER: 10263222 2015      FORCE TO END ORDER REG                 
005900*                                                                         
006000                                                                          
006100 ENVIRONMENT DIVISION.                                                    
006200                                                                          
006300 DATA DIVISION.                                                           
006400                                                                          
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700*    -COPY WY2000W1                                                       
006800                                                                          
006900 77  IDPGM                       PIC X(08)   VALUE 'W4024300'.            
007000                                                                          
007100 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
007200 77  WS-IDDISTR-NUM4             PIC 9(4)   VALUE ZERO.                   
007300 77  WS-IDKUNDNR-NUM6            PIC 9(6)   VALUE ZERO.                   
007400                                                                          
007500 77  HOPP                        PIC X(1)   VALUE 'N'.                    
007600 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
007700 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
007800 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
007900 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008100 77  4241-MOD-LAENGD             PIC S9(4)  VALUE +44   COMP SYNC.        
008200 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
008300 77  SPAR-BEHORIGHETS-KONTR      PIC X(1)   VALUE SPACE.                  
008400 77  WS-RADER                    PIC 9(2)   VALUE ZERO.                   
008500 77  4242-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
008600 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008700 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
008900 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
009000 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
009100 77  WS-INDEX-SATS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009200 77  WS-INDEX-SATS-MAX           PIC S9(9)   COMP SYNC VALUE +5.          
009300 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009400 77  WS-INDEX-WOPS-MAX           PIC S9(9)  COMP SYNC VALUE 100.          
009410 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
009500 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009600 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
009700 77  SPAR-IDARTNR-TILLK          PIC S9(9)   VALUE +0  COMP-3.            
009800 77  SPAR-KVBEART-TILLK          PIC S9(7)   VALUE +0  COMP-3.            
009900 77  SPAR-REKSIFFR-TILLK         PIC S9(1)   VALUE +0  COMP-3.            
010000 77  SPAR-DIERS-KVOT          PIC S9(4)V9(3) VALUE +0  COMP-3.            
010100 77  SPAR-IDARTNR-40             PIC S9(9)   VALUE +0  COMP-3.            
010200 77  SPAR-IDLOPNR-40             PIC S9(3)   VALUE +0  COMP-3.            
010300 77  SPAR-IDSEKVNR-40            PIC S9(3)   VALUE +0  COMP-3.            
010400 77  WS-KVSLASK                  PIC S9(7)   VALUE +0  COMP-3.            
010500 77  WS-IDDISTR                  PIC X(4).                                
010600 77  WS-IDKUNDNR                 PIC X(6).                                
010700 77  WS-IDORDNR                  PIC X(5).                                
010800 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010900 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
011000 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
011100 77  WS-IDANSK                   PIC  9(3)   VALUE ZERO.                  
011200                                                                          
011300     EJECT                                                                
011400*                                                                         
011500 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
011600                                                                          
011700*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
011800*    ----DIST79-DEALER-PRICE----                                          
011900     EJECT                                                                
012000                                                                          
012100*01  -COPY WWPRODSL                                                       
012200                                                                          
012300 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
012400 01  FILLER REDEFINES WS-TIHHMMSS.                                        
012500     03 WS-TIHHMM                PIC 9(4).                                
012600     03 FILLER                   PIC 9(2).                                
012700     EJECT                                                                
012800                                                                          
012900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013000     88  ALLT-OK                             VALUE 'J'.                   
013100 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
013200     88  NYCKEL-OK                           VALUE 'J'.                   
013300 77  AKT-SIDA-SW                 PIC X       VALUE 'N'.                   
013400     88  AKT-SIDA                            VALUE 'J'.                   
013500 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
013600     88  AVSLUTA                             VALUE 'J'.                   
013700 77  START-4242-SW               PIC X       VALUE 'N'.                   
013800     88  START-4242                          VALUE 'J'.                   
013900 77  NEXT-SATS-SW                PIC X       VALUE 'N'.                   
014000     88  NEXT-SATS                           VALUE 'J'.                   
014100                                                                          
014200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014300     88  EGEN-MID                            VALUE '4243'.                
014400     88  GODK-MID                            VALUE '4241' '4242'          
014500                                                   '4243' '4244'          
014600                                                   '4206'.                
014700                                                                          
014800 77  X-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014900 01  WS-ALFA-1.                                                           
015000     03  WS-NUM-1                PIC 9(1).                                
015100 01  WS-ALFA-5.                                                           
015200     03  WS-NUM-5                PIC 9(5).                                
015300 01  WS-ALFA-6.                                                           
015400     03  WS-NUM-6                PIC 9(6).                                
015500 01  WS-ALFA-9.                                                           
015600     03  WS-NUM-9                PIC 9(9).                                
015700 01  WS-ALFA-10.                                                          
015800     03  WS-NUM-10               PIC 9(10).                               
015900                                                                          
016000 01  WS-IDARTNR-REKSIFFR.                                                 
016100     03  WS-IDARTNR              PIC 9(9).                                
016200     03  FILLER                  PIC X(1)   VALUE '-'.                    
016300     03  WS-REKSIFFR             PIC 9(1).                                
016400     EJECT                                                                
016500 01  WS-AKTUELL-MID-RAD.                                                  
016600     03  WS-AKT-KDORDBEK         PIC 9(2).                                
016700     03  WS-AKT-KDBEHX           PIC X(1).                                
016800     03  WS-AKT-IDARTNR          PIC 9(9).                                
016900     03  WS-AKT-FILLER           PIC X(1).                                
017000     03  WS-AKT-REKSIFFR         PIC 9(1).                                
017100     03  WS-AKT-IDDC             PIC X(2).                                
017200     03  WS-AKT-IDKUNDRF-RO      PIC X(7).                                
017300     03  WS-AKT-KEYS.                                                     
017400         05 WS-AKT-IDLOPNR       PIC 9(3).                                
017500         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
017600         05 WS-AKT-IDARTNR-URS   PIC 9(9).                                
017700         05 WS-AKT-IDLOPNR-RO    PIC 9(3).                                
017800                                                                          
017900 01  WS-IDARTNR-SATS-TAB.                                                 
018000     05 WS-IDARTNR-SATS          PIC X(9)    OCCURS 5.                    
018100                                                                          
018200 01  WS-KEYS-SPAR.                                                        
018300     03 WS-IDLOPNR-SPAR          PIC 9(3).                                
018400     03 WS-IDSEKVNR-SPAR         PIC 9(3).                                
018500     03 WS-IDARTNR-URS-SPAR      PIC 9(9).                                
018600     03 WS-IDARTNR-SPAR          PIC 9(9).                                
018700     03 WS-IDDC-SPAR             PIC X(2).                                
018800     EJECT                                                                
018900                                                                          
019000 01  SPAR-ARBTAB-DATA.                                                    
019100   03  SPAR-ARB-KDFRAKT           PIC S9(3)       COMP-3.                 
019200   03  SPAR-ARB-KDFDKRAV          PIC S9(3)       COMP-3.                 
019300   03  SPAR-ARB-KDROPACK          PIC X.                                  
019400                                                                          
019500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019600 01  GENERELLA-SUBPROGRAM.                                                
019700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
019900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
020400*                                                                         
020500*                                                                         
020600 01  GEMENSAMMA-SUBPROGRAM.                                               
020700     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
020800*        LÄSNING ARTIKELREGISTER                                          
020900     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
021000*        KONTROLL DIREKTLEVERANS                                          
021100     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
021200*        KONTROLL ENHETSLAST                                              
021300     EJECT                                                                
021400     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
021500*        KONTROLL PRELIMINÄRAVBOKNING                                     
021600     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
021700*        BERÄKNA RANSONERING                                              
021800     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
021900*        KONTROLL/UPPDATERING TPO2                                        
022000     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
022100*        UPPDATERING TPO6                                                 
022200     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
022300*        JUSTERING LAGERPLATS OCH LAGEROMRÅDE                             
022400     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
022500*        WOPS PER RAD                                                     
022600     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
022700*        PRISFRÅGA                                                        
022800     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
022900*        PRISFRÅGA                                                        
023000     EJECT                                                                
023100*   -COPY W402W001                                                        
023200     EJECT                                                                
023300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023400 01  MESSAGE-CODES.                                                       
023500     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
023600     03  MED-UPPLYSN-UPPDAT-PF   PIC X(3)    VALUE '144'.                 
023700     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
023800     03  MED-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
023900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024000     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
024100     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
024200     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
024300     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
024400     03  ERR-EJ-ANNULLATION      PIC X(3)    VALUE '066'.                 
024500     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
024600     SKIP2                                                                
024700*   ---  PARAMETRAR TILL WSECURIT                                         
024800*   -COPY WSECAREA                                                        
024900     EJECT                                                                
025000*   ---  PARAMETRAR TILL W005INIT                                         
025100*   -COPY WMSGINIT                                                        
025200     EJECT                                                                
025300*   -COPY WMEDAREA                                                        
025400     EJECT                                                                
025500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
025600 01 FILLER                       PIC X(8)    VALUE 'W411AREG'.            
025700*   -COPY W411AREG                                                        
025800     EJECT                                                                
025900 01 FILLER                       PIC X(8)    VALUE 'W411DLEV'.            
026000*   -COPY W411DLEV                                                        
026100     EJECT                                                                
026200 01 FILLER                       PIC X(8)    VALUE 'W411LAST'.            
026300*   -COPY W411LAST                                                        
026400     EJECT                                                                
026500 01 FILLER                       PIC X(8)    VALUE 'W411CDCA'.            
026600*   -COPY W411CDCA                                                        
026700     EJECT                                                                
026800 01 FILLER                       PIC X(8)    VALUE 'W411RANS'.            
026900*   -COPY W411RANS                                                        
027000     EJECT                                                                
027100 01 FILLER                       PIC X(8)    VALUE 'W411TPO2'.            
027200*   -COPY W411TPO2                                                        
027300     EJECT                                                                
027400 01 FILLER                       PIC X(8)    VALUE 'W411TPO6'.            
027500*   -COPY W411TPO6                                                        
027600     EJECT                                                                
027700 01 FILLER                       PIC X(8)    VALUE 'W413AVSR'.            
027800*   -COPY W413AVSR                                                        
027900     EJECT                                                                
028000 01 FILLER                       PIC X(8)    VALUE 'W413ADRS'.            
028100*   -COPY W413ADRS                                                        
028200     EJECT                                                                
028300 01 FILLER                       PIC X(8)    VALUE 'W335PRNO'.            
028400*   -COPY W335PRNO                                                        
028500     EJECT                                                                
028600 01 FILLER                       PIC X(8)    VALUE 'W335PRQU'.            
028700*   -COPY W335PRQU                                                        
028800     EJECT                                                                
028900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029000*                                                                         
029100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029200     SKIP3                                                                
029300*01  MID -COPY W4I24301                                                   
029400     EJECT                                                                
029500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
029600     SKIP3                                                                
029700*01  -COPY WMSGAREA                                                       
029800     EJECT                                                                
029900*    03  MOD -COPY W4O24301   -RED MSG-AREA.                              
030000     EJECT                                                                
030100     EJECT                                                                
030200******************************************************************        
030300*    MID-AREA FÖR W2T191                                         *        
030400******************************************************************        
030500*01  -COPY  W2I19101  -PRE 2191-                                          
030600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030700     SKIP3                                                                
030800*01  -COPY WMFSAREA                                                       
030900     EJECT                                                                
031000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031200 01  NYCKLAR-TILL-DLI.                                                    
031300     03  W-WDQ101-KEY-UNIK.                                               
031400         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
031500         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
031600         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
031700         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
031800         05  W-Q1-IDDC-UNIK      PIC  X(2)   VALUE ZERO.                  
031900         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
032000                                                                          
032100     03  W-WDQ101-KEY-MIN.                                                
032200         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
032300         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
032400         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
032500         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
032600         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
032700         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
032800     03  W-WDQ101-KEY-MAX.                                                
032900         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
033000         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
033100         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
033200         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
033300         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
033400         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
033500                                                                          
033600     03  W-WDQ101-KEY-MIN1.                                               
033700         05  W-Q1-IDORDER-MIN1   PIC S9(7)   COMP-3.                      
033800         05  W-Q1-IDARTNR-MIN1   PIC S9(9)   COMP-3.                      
033900         05  W-Q1-IDLOPNR-MIN1   PIC S9(3)   COMP-3.                      
034000         05  W-Q1-IDSEKVNR-MIN1  PIC S9(3)   COMP-3.                      
034100         05  FILLER              PIC  X(4)   VALUE LOW-VALUE.             
034200     03  W-WDQ101-KEY-MAX1.                                               
034300         05  W-Q1-IDORDER-MAX1   PIC S9(7)   COMP-3.                      
034400         05  W-Q1-IDARTNR-MAX1   PIC S9(9)   COMP-3.                      
034500         05  W-Q1-IDLOPNR-MAX1   PIC S9(3)   COMP-3.                      
034600         05  W-Q1-IDSEKVNR-MAX1  PIC S9(3)   COMP-3.                      
034700         05  FILLER              PIC  X(4)   VALUE HIGH-VALUE.            
034800     EJECT                                                                
034900                                                                          
035000     03  W-WDJ1CSEQ-X.                                                    
035100         05  W-J1-IDLEVNR        PIC X(5)    VALUE SPACE.                 
035200         05  FILLER              PIC X(30)   VALUE SPACE.                 
035300         05  W-J1-IDARTNR        PIC S9(9)   VALUE +0  COMP-3.            
035400                                                                          
035500     03  W-IDLEVNR-X             PIC X(5)    VALUE '1002 '.               
035600                                                                          
035700     03  W-WDA5KEY-X.                                                     
035800         05  W-A5-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
035900         05  W-A5-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
036000         05  W-A5-IDKUNDRF       PIC X(10)   VALUE SPACE.                 
036100         05  W-A5-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
036200         05  W-A5-IDLOPNR        PIC S9(3)   VALUE ZERO COMP-3.           
036300     03  W-WDA5KEY-MIN-X.                                                 
036400         05  W-A5-IDDISTR-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
036500         05  W-A5-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
036600         05  W-A5-IDKUNDRF-MIN   PIC X(10)   VALUE SPACE.                 
036700         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
036800     03  W-WDA5KEY-MAX-X.                                                 
036900         05  W-A5-IDDISTR-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
037000         05  W-A5-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
037100         05  W-A5-IDKUNDRF-MAX   PIC X(10)   VALUE SPACE.                 
037200         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
037300                                                                          
037400     03  W-IDGMTREF-X.                                                    
037500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
037600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
037700         05  W-IDKUNDRF.                                                  
037800            07  W-IDORDNR        PIC 9(7)    VALUE ZERO.                  
037900            07  FILLER           PIC X(3)    VALUE SPACE.                 
038000     03  W-WDE801KY-X.                                                    
038100         05  W-E8-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
038200         05  W-E8-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
038300         05  W-E8-IDKUNDRF.                                               
038400            07  W-E8-IDORDNR     PIC 9(7)    VALUE ZERO.                  
038500            07  FILLER           PIC X(3)    VALUE SPACE.                 
038600     EJECT                                                                
038700                                                                          
038710     03  W-WDB201KEY-X.                                                   
038720         05  W-WDB2-IDDISTR      PIC S9(5) COMP-3 VALUE +0.               
038730         05  W-WDB2-IDKUNDNR     PIC S9(7) COMP-3 VALUE +0.               
038740                                                                          
038800     03  W-IDORDER-X.                                                     
038900         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
039000     03  W-IDDC-X.                                                        
039100         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
039200     03  W-IDLAND-X.                                                      
039300         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
039400     03  W-IDARTNR-X.                                                     
039500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
039600     03  W-IDSKYLT-X.                                                     
039700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
039800     03  W-WDGXKEY-4541-X.                                                
039900         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
040000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
040100     EJECT                                                                
040200     03  W-IDDC-B6-X.                                                     
040300         05 W-IDDC-B6                  PIC X(2).                          
040400*                                                                         
040500     SKIP3                                                                
040600*    --- STATUS-KOD FRÅN IMS                                              
040700 01  STATUS-WS                   PIC XX.                                  
040800     88  SEGMENT-FINNS                       VALUE '  '.                  
040900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041000     88  BASEN-SLUT                          VALUE 'GB'.                  
041100     SKIP2                                                                
041200 01  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
041300     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
041400     SKIP2                                                                
041500 01  GODK-STATUSKODER.                                                    
041600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041700     SKIP3                                                                
041800 01  SSA1                        PIC X(160).                              
041900 01  SSA2                        PIC X(96).                               
042000 01  SSA3                        PIC X(96).                               
042100                                                                          
042200     EJECT                                                                
042300*    --- IMS FUNKTIONSKODER                                               
042400*01  -COPY W0003                                                          
042500     EJECT                                                                
042600*    ---  DLI INPUT-OUTPUT AREA                                           
042700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
042800     SKIP3                                                                
042900 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
043000 01  DLI-IO-AREA-ORQM.                                                    
043100     03  WLORQM01.                                                        
043200*        05  -COPY WDQ101                                                 
043300     EJECT                                                                
043400 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
043500 01  DLI-IO-AREA-ORQI01.                                                  
043600     03  WLORQI01.                                                        
043700*        05  -COPY WDQ201                                                 
043800     EJECT                                                                
043900 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
044000 01  DLI-IO-AREA-ORQI12.                                                  
044100     03  WLORQI12.                                                        
044200*        05  -COPY WDQ212                                                 
044300     EJECT                                                                
044400 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
044500 01  DLI-IO-AREA-ORQF.                                                    
044600     03  WLORQF01.                                                        
044700*        05  -COPY WDQ401                                                 
044800     EJECT                                                                
044900 01  FILLER                      PIC X(16)   VALUE 'WDE801-AREA'.         
045000 01  DLI-IO-AREA-PROC.                                                    
045100     03  WLPROC01.                                                        
045200*        05  -COPY WDE801                                                 
045300     EJECT                                                                
045400 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
045500 01  DLI-IO-AREA-ARTM.                                                    
045600     03  WLARTM01.                                                        
045700*        05  -COPY WDK901                                                 
045800     EJECT                                                                
045900 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
046000 01  DLI-IO-AREA-WDK7.                                                    
046100     03  WDK711.                                                          
046200*        05  -COPY WDK711                                                 
046300 01  FILLER                      PIC X(16)   VALUE 'WDK712-AREA'.         
046400 01  DLI-IO-AREA-WDK712.                                                  
046500     03  WDK712.                                                          
046600*        05  -COPY WDK712                                                 
046700 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
046800 01  DLI-IO-WDK722.                                                       
046900     03  WDK722.                                                          
047000*        05  -COPY WDK722                                                 
047100     EJECT                                                                
047200 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
047300 01  DLI-IO-AREA-BENA.                                                    
047400     03  WLBENA11.                                                        
047500*        05  -COPY WDD311                                                 
047600     EJECT                                                                
047700 01  FILLER                      PIC X(16)   VALUE 'WDA501-AREA'.         
047800 01  DLI-IO-AREA-ORDP.                                                    
047900     03  WLORDP01.                                                        
048000*        05  -COPY WDA501                                                 
048100     EJECT                                                                
048200 01  FILLER                      PIC X(16)   VALUE 'WDJ1  -AREA'.         
048300 01  DLI-IO-AREA-SATB.                                                    
048400     03  WLSATB11.                                                        
048500*        05  -COPY WDJ111                                                 
048600     03  WLSATB01.                                                        
048700*        05  -COPY WDJ101                                                 
048800     EJECT                                                                
048900 01  FILLER                      PIC X(16)   VALUE 'VOR-KÖ-AREA'.         
049000 01  DLI-IO-AREA-4541.                                                    
049100     03  WL454111.                                                        
049200*        05  -COPY WDGX4542                                               
049300     EJECT                                                                
049400 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
049500 01  DLI-IO-AREA-WDK611.                                                  
049600*    03  -COPY WDK611                                                     
049700     EJECT                                                                
049800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
049900 01   DLI-IO-AREA-B601.                                                   
050000*     03  -COPY WDB601                                                    
050100     EJECT                                                                
050110 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
050120 01   DLI-IO-AREA-B201.                                                   
050130*     03  -COPY WDB201                                                    
050140     EJECT                                                                
050200 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
050300 01  4243-MSG-IO-AREA.                                                    
050400     03  4243-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
050500     03  4243-Z1               PIC X      VALUE LOW-VALUE.                
050600     03  4243-Z2               PIC X      VALUE LOW-VALUE.                
050700     03  4243-TRANSKOD         PIC X(8)   VALUE 'W4T243V '.               
050800     03  4243-IDTRANS          PIC X(4)   VALUE '4243'.                   
050900     03  4243-SPRAK            PIC X.                                     
051000     03  4243-IDDISTR-IN       PIC X(4).                                  
051100     03  4243-IDKUNDNR-IN      PIC X(6).                                  
051200     03  4243-IDORDNR-IN       PIC X(5).                                  
051300     03  4243-IDDISTR-UT       PIC X(4).                                  
051400     03  4243-IDKUNDNR-UT      PIC X(6).                                  
051500     03  4243-IDORDNR-UT       PIC X(5).                                  
051600     03  4243-KDORDKL-UT       PIC X      VALUE SPACE.                    
051700     03  4243-FLANNULL         PIC X      VALUE 'N'.                      
051800     03  FILLER                PIC X(20)  VALUE ZERO.                     
051900                                                                          
052000 01  4292-MSG-IO-AREA.                                                    
052100     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
052200     03  4292-Z1               PIC X.                                     
052300     03  4292-Z2               PIC X.                                     
052400     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
052500     03  4292-IDTRANS          PIC X(4)   VALUE '4243'.                   
052600     03  4292-SPRAK            PIC X.                                     
052700     03  4292-IDORDER          PIC X(7).                                  
052800     03  4292-IDDISTR          PIC X(4).                                  
052900     03  4292-IDKUNDNR         PIC X(6).                                  
053000     03  4292-IDKUNDRF         PIC X(7).                                  
053100     03  FILLER                PIC X(6)   VALUE SPACE.                    
053200     EJECT                                                                
053300                                                                          
053400 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
053500 01  4298-MSG-IO-AREA.                                                    
053600     03  4298-LL               PIC S9(4)  VALUE +0  COMP SYNC.            
053700     03  4298-Z1               PIC X.                                     
053800     03  4298-Z2               PIC X.                                     
053900     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
054000     03  4298-IDTRANS          PIC X(4)   VALUE '4243'.                   
054100     03  4298-SPRAK            PIC X      VALUE SPACE.                    
054200*    03  -COPY W4I29801  -PRE 4298-                                       
054300     EJECT                                                                
054400 01  FILLER                  PIC X(16)  VALUE '4242-MSG-IO-AREA'.         
054500 01  4242-MSG-IO-AREA.                                                    
054600     03  4242-LL               PIC S9(4)  VALUE +600  COMP SYNC.          
054700     03  4242-Z1               PIC X.                                     
054800     03  4242-Z2               PIC X.                                     
054900     03  4242-TRANSKOD         PIC X(8)   VALUE 'W4T242U '.               
055000     03  4242-IDTRANS          PIC X(4)   VALUE '4243'.                   
055100     03  4242-SPRAK            PIC X.                                     
055200     03  -COPY W4I24201  -PRE 4242-                                       
055300     EJECT                                                                
055400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
055500     SKIP3                                                                
055600 01  -COPY WZ01SEND                                                       
055700     EJECT                                                                
055800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
055900     SKIP3                                                                
056000 01  SEND-AREA.                                                           
056100*    03  -COPY WZ01REQU  -PRE 3039-                                       
056200*    03  -COPY W30391I1  -PRE 3039-                                       
056300     EJECT                                                                
056400 LINKAGE SECTION.                                                         
056500                                                                          
056600*01  -COPY W0009   -PRE MSG-                                              
056700                                                                          
056800 01  AVSR-ALT-PCB                PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0009   -PRE 4292-                                             
057100     EJECT                                                                
057200*01  -COPY W0009   -PRE 4298-                                             
057300     EJECT                                                                
057400*01  -COPY W0009   -PRE 4242-                                             
057500     -COPY W0009   -PRE 4243-                                             
057600     EJECT                                                                
057700*01  -COPY W0009   -PRE 2191-                                             
057800     EJECT                                                                
057900*01  -COPY W0008   -PRE USEA-                                             
058000     05  FILLER                  PIC X.                                   
058100     SKIP2                                                                
058200*01  -COPY W0009   -PRE PRQRY-                                            
058300     05  FILLER                  PIC X.                                   
058400     SKIP2                                                                
058500*01  -COPY W0008   -PRE SATB-                                             
058600     05  FILLER                  PIC X.                                   
058700     SKIP2                                                                
058800*01  -COPY W0008   -PRE ARTM-                                             
058900     05  FILLER                  PIC X.                                   
059000     EJECT                                                                
059100*01  -COPY W0008   -PRE WDK7-                                             
059200     05  FILLER                  PIC X.                                   
059300     EJECT                                                                
059400*01  -COPY W0008   -PRE BENA-                                             
059500     05  FILLER                  PIC X.                                   
059600     SKIP2                                                                
059700*01  -COPY W0008   -PRE ORDP-                                             
059800     05  FILLER                  PIC X.                                   
059900     EJECT                                                                
060000*01  -COPY W0008   -PRE ORQF-                                             
060100     05  FILLER                  PIC X.                                   
060200     SKIP2                                                                
060300*01  -COPY W0008   -PRE ORQI-                                             
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008   -PRE ORQM-                                             
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900*01  -COPY W0008   -PRE 4541-                                             
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01  -COPY W0008   -PRE WDK6-                                             
061300     05  FILLER                  PIC X.                                   
061400     SKIP2                                                                
061500*01  -COPY W0008   -PRE PROC-                                             
061600     05  FILLER                  PIC X.                                   
061700     EJECT                                                                
061800*01  -COPY W0008   -PRE WDB6-                                             
061900     05  FILLER                  PIC X.                                   
061910*01  -COPY W0008   -PRE WDB2-                                             
061920     05  FILLER                  PIC X.                                   
062000     EJECT                                                                
062100 01  AREG-WDK6-PCB               PIC X.                                   
062200 01  AREG-WDK7-PCB               PIC X.                                   
062300                                                                          
062400 01  DLEV-LEVF-PCB               PIC X.                                   
062500 01  DLEV-LEVG-PCB               PIC X.                                   
062600 01  DLEV-LEVA-PCB               PIC X.                                   
062700 01  DLEV-ARTS-PCB               PIC X.                                   
062800 01  DLEV-WDB6-PCB               PIC X.                                   
062900                                                                          
063000 01  CDCA-ARTM-PCB               PIC X.                                   
063100 01  CDCA-INLB-PCB               PIC X.                                   
063200 01  CDCA-WDB2-PCB               PIC X.                                   
063300 01  CDCA-WDC1-PCB               PIC X.                                   
063400                                                                          
063500 01  RANS-XXKM-PCB               PIC X.                                   
063600 01  RANS-ARTM-PCB               PIC X.                                   
063700 01  RANS-ARTS-PCB               PIC X.                                   
063800                                                                          
063900 01  TPO2-ORDP-PCB               PIC X.                                   
064000 01  TPO2-XXBU-PCB               PIC X.                                   
064100 01  TPO2-XXBV-PCB               PIC X.                                   
064200 01  TPO2-ARTM-PCB               PIC X.                                   
064300 01  TPO2-FILA-PCB               PIC X.                                   
064400 01  TPO2-XXBX-PCB               PIC X.                                   
064500                                                                          
064600 01  2109-PCB                    PIC X.                                   
064700 01  TPO6-ORDP-PCB               PIC X.                                   
064800 01  TPO6-XXBU-PCB               PIC X.                                   
064900 01  TPO6-XXBV-PCB               PIC X.                                   
065000 01  TPO6-XXBX-PCB               PIC X.                                   
065100 01  TPO6-ARTS-PCB               PIC X.                                   
065200                                                                          
065300 01  TIME-4437-PCB               PIC X.                                   
065400                                                                          
065500 01  AVSR-ORQI-PCB               PIC X.                                   
065600 01  AVSR-GMTB-PCB               PIC X.                                   
065700 01  AVSR-GMTC-PCB               PIC X.                                   
065800 01  AVSR-WDB2-PCB               PIC X.                                   
065900 01  AVSR-WDB6-PCB               PIC X.                                   
066000                                                                          
066100 01  TRAN-XXKB-PCB               PIC X.                                   
066200 01  KVAN-WDB2-PCB               PIC X.                                   
066300                                                                          
066400 01  PRNO-3107-PCB               PIC X.                                   
066500 01  PRQU-WDG2-PCB               PIC X.                                   
066600 01  PRQU-WDC7-PCB               PIC X.                                   
066700 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
066800                                                                          
066900     EJECT                                                                
067000 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
067100      4298-PCB 4242-PCB 4243-PCB 2109-PCB 2191-PCB PRQRY-PCB              
067200      USEA-PCB SATB-PCB                                                   
067300      ARTM-PCB WDK7-PCB BENA-PCB ORDP-PCB ORQF-PCB ORQI-PCB               
067400      ORQM-PCB 4541-PCB WDK6-PCB PROC-PCB                                 
067500      WDB6-PCB WDB2-PCB AREG-WDK6-PCB                                     
067600      AREG-WDK7-PCB                                                       
067700      DLEV-LEVF-PCB                                                       
067800      DLEV-LEVG-PCB                                                       
067900      DLEV-LEVA-PCB                                                       
068000      DLEV-ARTS-PCB                                                       
068100      DLEV-WDB6-PCB                                                       
068200      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
068300      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
068400      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
068500      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
068600      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
068700      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
068800      TIME-4437-PCB                                                       
068900      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
069000      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
069100      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
069200      PRNO-3107-PCB                                                       
069300      PRQU-WDG2-PCB                                                       
069400      PRQU-WDC7-PCB                                                       
069500      PRQU-SJKO-WDK6-PCB.                                                 
069600                                                                          
069700 MAIN SECTION.                                                            
069800                                                                          
069900     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
070000      4298-PCB 4242-PCB 4243-PCB 2109-PCB 2191-PCB PRQRY-PCB              
070100      USEA-PCB SATB-PCB                                                   
070200      ARTM-PCB WDK7-PCB BENA-PCB ORDP-PCB ORQF-PCB ORQI-PCB               
070300      ORQM-PCB 4541-PCB WDK6-PCB PROC-PCB                                 
070400      WDB6-PCB WDB2-PCB AREG-WDK6-PCB                                     
070500      AREG-WDK7-PCB                                                       
070600      DLEV-LEVF-PCB                                                       
070700      DLEV-LEVG-PCB                                                       
070800      DLEV-LEVA-PCB                                                       
070900      DLEV-ARTS-PCB                                                       
071000      DLEV-WDB6-PCB                                                       
071100      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
071200      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
071300      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
071400      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
071500      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
071600      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
071700      TIME-4437-PCB                                                       
071800      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
071900      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
072000      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
072100      PRNO-3107-PCB                                                       
072200      PRQU-WDG2-PCB                                                       
072300      PRQU-WDC7-PCB                                                       
072400      PRQU-SJKO-WDK6-PCB.                                                 
072500                                                                          
072600     EJECT                                                                
072700     PERFORM IMS-GET-MSG                                                  
072800     IF SEGMENT-FINNS                                                     
072900        PERFORM A-INIT                                                    
073000        PERFORM B-KOLLA-NYCKLAR                                           
073100        IF NYCKEL-OK                                                      
073200           PERFORM O-KONTROLLERA-BEHORIGHET                               
073300           IF ALLT-OK                                                     
073400              IF MFS-NEXT                                                 
073500                 PERFORM C-NAESTA-SIDA                                    
073600              ELSE                                                        
073700                 PERFORM D-FOERSTA-SIDA                                   
073800              END-IF                                                      
073900           END-IF                                                         
074000           IF ALLT-OK                                                     
074100              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
074200              IF ALLT-OK                                                  
074300                 PERFORM E-SKRIVSKYDDA-NYCKLAR                            
074400                 IF (MFS-KDTRTYP = 'U' OR 'V')  OR MFS-ENTER              
074500                    PERFORM G-KONTROLLERA-BILDEN                          
074600                 END-IF                                                   
074700                 IF ALLT-OK                                               
074800                    PERFORM H-BEHANDLA-RADER                              
074900                    IF  AVSLUTA                                           
075000                       PERFORM I-STARTA-ORDERAVSLUT                       
075100                       PERFORM M-HOPPA-TILL-ORDERHUVUD-4241               
075200                    END-IF                                                
075300                 END-IF                                                   
075400              END-IF                                                      
075500           END-IF                                                         
075600        END-IF                                                            
075700        IF HOPP = NEJ                                                     
075800           PERFORM Z-FINIT                                                
075900        END-IF                                                            
076000     END-IF                                                               
076100     MOVE +0 TO RETURN-CODE                                               
076200     GOBACK                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 A-INIT SECTION.                                                          
076600                                                                          
076700     MOVE SPACE                TO MED-IDMFSFEL                            
076800                                  MED-IDMFSINF                            
076900     MOVE JA                   TO ALLT-SW                                 
077000                                  NYCKEL-SW                               
077100     ACCEPT DAGENS-DATUM     FROM DATE                                    
077200                                                                          
077300     IF MSG-DUBBLA-TRANSKODER                                             
077400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I24301                 
077500       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
077600       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
077700     ELSE                                                                 
077800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I24301                  
077900       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
078000       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
078100     END-IF                                                               
078200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
078300     MOVE MSG-IDPFK            TO MFS-IDPFK                               
078400     MOVE MFS-IDTRANS          TO W-IDTRANS                               
078500     MOVE LOW-VALUE            TO MSG-AREA                                
078600     MOVE 'W4O24301'           TO MFS-IDMOD                               
078700     MOVE '4'                  TO MOD-IDTRANS1                            
078800     MOVE '2'                  TO MOD-IDTRANS2                            
078900     MOVE '4'                  TO MOD-IDTRANS3                            
079000     MOVE '3'                  TO MOD-IDTRANS4                            
079100     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
079200                                                                          
079300     IF NOT EGEN-MID                                                      
079400       IF W-IDTRANS = '4242' AND MFS-UPD-V                                
079500         CONTINUE                                                         
079600       ELSE                                                               
079700         MOVE SPACE            TO MFS-KDTRTYP                             
079800         MOVE '7'              TO MFS-IDPFK                               
079900         IF W-IDTRANS = '0813'                                            
080000            MOVE ALL '+'           TO MSGI-WMSGINIT                       
080100            MOVE '013'             TO MSGI-KDCALL                         
080200            MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                         
080300            MOVE '4243'            TO MSGI-IDTRANS                        
080400            MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                   
080500                                                                          
080600            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
080700         END-IF                                                           
080800       END-IF                                                             
080900     END-IF                                                               
081000     EJECT                                                                
081100     IF ENGLISH-TEXT                                                      
081200       MOVE 'GB '              TO MED-IDSKYLT                             
081300     ELSE                                                                 
081400       MOVE 'S  '              TO MED-IDSKYLT                             
081500     END-IF                                                               
081600                                                                          
081700     PERFORM AA-NOLLA-TABELLER                                            
081800                                                                          
081900     MOVE SPACE                TO 2191-MID-W2I19101                       
082000     .                                                                    
082100                                                                          
082200 AA-NOLLA-TABELLER SECTION.                                               
082300                                                                          
082400     MOVE +1                   TO WS-INDEX-WOPS                           
082500     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
082600        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
082700        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
082800        MOVE ZERO              TO AVSR-IDDC(WS-INDEX-WOPS)                
082900        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
083000        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
083100        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
083200        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
083300        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
083400        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
083500        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
083600        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
083700        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
083800        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
083900        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
084000        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
084100        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
084200                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
084300        ADD +1                 TO WS-INDEX-WOPS                           
084400     END-PERFORM                                                          
084500     MOVE +1                   TO WS-INDEX-WOPS                           
084600                                                                          
084700     MOVE +1                   TO WS-INDEX                                
084800     PERFORM UNTIL WS-INDEX > +14                                         
084900        MOVE ALL '+'           TO 4242-MID-RADER(WS-INDEX)                
085000        ADD +1                 TO WS-INDEX                                
085100     END-PERFORM                                                          
085200     .                                                                    
085300     EJECT                                                                
085400 B-KOLLA-NYCKLAR SECTION.                                                 
085500                                                                          
085600     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
085700                                  MOD-IDKUNDNR-IN                         
085800                                  MOD-IDORDNR-IN                          
085900     IF W-IDTRANS = '0813'                                                
086000        MOVE MSGI-IDDISTR       TO MID-IDDISTR-IN                         
086100        MOVE MSGI-IDKUNDNR      TO MID-IDKUNDNR-IN                        
086200        MOVE MSGI-IDKUNDRF(3:5) TO MID-IDORDNR-IN                         
086300     END-IF                                                               
086400                                                                          
086500     IF MID-IDDISTR-IN = ALL '+'                                          
086600        MOVE MID-IDDISTR-UT    TO WS-IDDISTR                              
086700        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
086800     ELSE                                                                 
086900        MOVE MID-IDDISTR-IN    TO WS-IDDISTR                              
087000        MOVE '7'               TO MFS-IDPFK                               
087100        MOVE SPACE             TO MFS-KDTRTYP                             
087200     END-IF                                                               
087300                                                                          
087400     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
087500        MOVE WS-IDDISTR        TO W-IDDISTR                               
087600     ELSE                                                                 
087700        MOVE NEJ               TO NYCKEL-SW                               
087800        MOVE ZERO              TO WS-IDDISTR                              
087900     END-IF                                                               
088000                                                                          
088100     MOVE WS-IDDISTR           TO TEST-IDDISTR                            
088200     IF DIST79-DEALER-PRICE                                               
088300        IF ENGLISH-TEXT                                                   
088400           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
088500        ELSE                                                              
088600           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
088700        END-IF                                                            
088800     ELSE                                                                 
088900        MOVE SPACES            TO MOD-TEDDI                               
089000     END-IF                                                               
089100                                                                          
089200     IF MID-IDKUNDNR-IN = ALL '+'                                         
089300        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
089400        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
089500     ELSE                                                                 
089600        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
089700        MOVE '7'               TO MFS-IDPFK                               
089800        MOVE SPACE             TO MFS-KDTRTYP                             
089900     END-IF                                                               
090000                                                                          
090100     IF WS-IDKUNDNR NUMERIC                                               
090200        MOVE WS-IDKUNDNR       TO W-IDKUNDNR                              
090300     ELSE                                                                 
090400        MOVE NEJ               TO NYCKEL-SW                               
090500     END-IF                                                               
090600     IF MID-IDORDNR-IN = ALL '+'                                          
090700        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
090800        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
090900     ELSE                                                                 
091000        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
091100        MOVE '7'               TO MFS-IDPFK                               
091200        MOVE SPACE             TO MFS-KDTRTYP                             
091300     END-IF                                                               
091400     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
091500        MOVE WS-IDORDNR        TO W-IDORDNR                               
091600     ELSE                                                                 
091700        MOVE NEJ               TO NYCKEL-SW                               
091800     END-IF                                                               
091900                                                                          
092000     IF GODK-MID OR NYCKEL-OK                                             
092100        MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                      
092200        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
092300        IF WS-IDKUNDNR = ZERO                                             
092400           MOVE '     0'          TO MOD-IDKUNDNR-UT                      
092500        END-IF                                                            
092600        MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                       
092700        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
092800        MOVE WS-IDORDNR           TO MOD-IDORDNR-UT                       
092900        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
093000     ELSE                                                                 
093100        MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                          
093200                                  MOD-IDKUNDNR-UT                         
093300                                  MOD-IDORDNR-UT                          
093400     END-IF                                                               
093500                                                                          
093600     IF NOT NYCKEL-OK                                                     
093700        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
093800        PERFORM MFS-RENSA-ALLA-FAELT                                      
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200                                                                          
094300 C-NAESTA-SIDA SECTION.                                                   
094400                                                                          
094500     IF MID-IDARTNR-NEXT NUMERIC AND                                      
094600        MID-IDARTNR-NEXT > ZERO                                           
094700        MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                        
094800        MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                        
094900        MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                       
095000        MOVE MID-IDDC-NEXT     TO W-Q1-IDDC-MIN                           
095100        MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                       
095200     ELSE                                                                 
095300        MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                            
095400        MOVE NEJ               TO ALLT-SW                                 
095500     END-IF                                                               
095600     .                                                                    
095700                                                                          
095800 D-FOERSTA-SIDA SECTION.                                                  
095900                                                                          
096000     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
096100                                  W-Q1-IDLOPNR-MIN                        
096200                                  W-Q1-IDSEKVNR-MIN                       
096300                                  W-Q1-IDDC-MIN                           
096400                                  W-Q1-KDORDBEK-MIN                       
096500                                                                          
096600     IF MID-IDARTNR-NEXT NUMERIC AND                                      
096700        MID-IDARTNR-NEXT > ZERO                                           
096800        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                        
096900        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                        
097000        MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                       
097100        MOVE MID-IDDC-NEXT     TO MOD-IDDC-NEXT                           
097200        MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                       
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600                                                                          
097700 E-SKRIVSKYDDA-NYCKLAR SECTION.                                           
097800                                                                          
097900     MOVE MFS-STAENG-FAELT     TO MOD-IDTRANS1-ATTR                       
098000                                  MOD-IDTRANS2-ATTR                       
098100                                  MOD-IDTRANS3-ATTR                       
098200                                  MOD-IDTRANS4-ATTR                       
098300                                  MOD-IDDISTR-IN-ATTR                     
098400                                  MOD-IDKUNDNR-IN-ATTR                    
098500                                  MOD-IDORDNR-IN-ATTR                     
098600     MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLANNULL-ATTR                       
098700     .                                                                    
098800     EJECT                                                                
098900                                                                          
099000 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
099100                                                                          
099200     PERFORM IMS-09-GU-ORQI-WDQ201                                        
099300     IF SEGMENT-FINNS                                                     
099400        IF OHUV-FLKLAR = JA                                               
099500           MOVE ERR-ORDER-AVSLUTAD                                        
099600                               TO MED-IDMFSFEL                            
099700           MOVE NEJ            TO ALLT-SW                                 
099800           MOVE NEJ            TO NYCKEL-SW                               
099900           PERFORM MFS-RENSA-ALLA-FAELT                                   
100000        ELSE                                                              
100100           IF OHUV-IDSYSTEM NOT  = '4241'  AND                            
100200              (MSG-SIGNON-USERID NOT = 'PC30174 ')                        
100300              MOVE ERR-FEL-BILDSERIE                                      
100400                                  TO MED-IDMFSFEL                         
100500              MOVE NEJ            TO ALLT-SW                              
100600              MOVE NEJ            TO NYCKEL-SW                            
100700              PERFORM MFS-RENSA-ALLA-FAELT                                
100800           ELSE                                                           
100900              MOVE OHUV-KDORDKL TO MOD-KDORDKL-UT                         
101000              MOVE OHUV-IDORDER TO W-Q1-IDORDER-UNIK                      
101100                                   W-Q1-IDORDER-MIN                       
101200                                   W-Q1-IDORDER-MAX                       
101300                                   W-Q1-IDORDER-MIN1                      
101400                                   W-Q1-IDORDER-MAX1                      
101500              PERFORM FA-FIXA-LOKAL-TID                                   
101600           END-IF                                                         
101700        END-IF                                                            
101800     ELSE                                                                 
101900        MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                            
102000        MOVE NEJ               TO ALLT-SW                                 
102100        MOVE NEJ               TO NYCKEL-SW                               
102200        PERFORM MFS-RENSA-ALLA-FAELT                                      
102300     END-IF                                                               
102400     .                                                                    
102500     EJECT                                                                
102600                                                                          
102700 FA-FIXA-LOKAL-TID SECTION.                                               
102800                                                                          
102900     MOVE ALL '+'              TO MSGI-WMSGINIT                           
103000     MOVE '013'                TO MSGI-KDCALL                             
103100     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
103200     IF OHUV-IDDC-TVS = SPACE                                             
103300       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
103400     ELSE                                                                 
103500       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
103600     END-IF                                                               
103700                                                                          
103800     MOVE '4243'               TO MSGI-IDTRANS                            
103900     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
104000                                                                          
104100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 G-KONTROLLERA-BILDEN SECTION.                                            
104600                                                                          
104700     MOVE JA                   TO ALLT-SW                                 
104800                                                                          
104900     IF MID-FLANNULL NOT = '+'                                            
105000        IF MID-FLANNULL = 'J' OR 'Y' OR 'N'                               
105100           IF MID-FLANNULL = 'J' OR 'Y'                                   
105200              IF OHUV-KDTPOTYP > +0 OR                                    
105300                 OHUV-KVORDTIL > +0                                       
105400                 MOVE NEJ                TO ALLT-SW                       
105500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR             
105600                 MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL                  
105700              ELSE                                                        
105800                 MOVE W-IDDISTR  TO W-A5-IDDISTR-MIN                      
105900                                    W-A5-IDDISTR-MAX                      
106000                 MOVE W-IDKUNDNR TO W-A5-IDKUNDNR-MIN                     
106100                                    W-A5-IDKUNDNR-MAX                     
106200                 MOVE WS-IDORDNR TO W-A5-IDKUNDRF-MIN                     
106300                                    W-A5-IDKUNDRF-MAX                     
106400                 PERFORM IMS-24-GU-ORDP-WDA501                            
106500                 IF SEGMENT-FINNS                                         
106600                    MOVE NEJ                TO ALLT-SW                    
106700                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR          
106800                    MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL               
106900                 END-IF                                                   
107000              END-IF                                                      
107100              IF MID-FLANNULL = 'Y'                                       
107200                 MOVE JA          TO MID-FLANNULL                         
107300              END-IF                                                      
107400           END-IF                                                         
107500        ELSE                                                              
107600           MOVE NEJ                TO ALLT-SW                             
107700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR                   
107800           MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                        
107900        END-IF                                                            
108000     ELSE                                                                 
108100        MOVE NEJ               TO MID-FLANNULL                            
108200     END-IF                                                               
108300     EJECT                                                                
108400                                                                          
108500     PERFORM GA-KONTROLLERA-KDBEHX                                        
108600     IF ALLT-OK                                                           
108700        PERFORM GB-KONTROLLERA-SAMBAND                                    
108800        IF ALLT-OK                                                        
108900           PERFORM GC-JUSTERA-KDBEHX                                      
109000        END-IF                                                            
109100     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400                                                                          
109500 GA-KONTROLLERA-KDBEHX SECTION.                                           
109600                                                                          
109700     MOVE +1                   TO WS-INDEX-MID                            
109800     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
109900                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
110000        IF MID-KDBEHX(WS-INDEX-MID) = '+'                                 
110100           MOVE SPACE          TO MID-KDBEHX(WS-INDEX-MID)                
110200        END-IF                                                            
110300        MOVE MID-RAD(WS-INDEX-MID)                                        
110400                               TO WS-AKTUELL-MID-RAD                      
110500        IF WS-AKT-KDBEHX = 'B' OR 'D' OR 'A' OR 'X'                       
110600                               OR '1' OR '2' OR ' '                       
110700           EVALUATE WS-AKT-KDBEHX                                         
110800              WHEN 'A'                                                    
110900                 IF WS-AKT-KDORDBEK = 61                                  
111000                    CONTINUE                                              
111100                 ELSE                                                     
111200                    MOVE MFS-ALFA-FAELT-FEL                               
111300                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
111400                    MOVE NEJ   TO ALLT-SW                                 
111500                 END-IF                                                   
111600              WHEN 'B'                                                    
111700                 IF WS-AKT-KDORDBEK = 21  OR                              
111800                        51  OR 52  OR 53  OR 54  OR 55 OR                 
111900                        57  OR 58  OR 59  OR 66 OR                        
112000                        67  OR 72  OR 73  OR 74  OR 75  OR                
112100                        76  OR 80  OR 81  OR 82  OR 85  OR                
112210                     ((WS-AKT-KDORDBEK = 41 OR 61) AND                    
112300                      (WS-AKT-IDARTNR  = WS-AKT-IDARTNR-URS))             
112400                    CONTINUE                                              
112500                 ELSE                                                     
112600                    MOVE MFS-ALFA-FAELT-FEL                               
112700                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
112800                    MOVE NEJ   TO ALLT-SW                                 
112900                 END-IF                                                   
113000              WHEN 'D'                                                    
113100                 IF WS-AKT-IDLOPNR-RO = +0                                
113200                    IF WS-AKT-KDORDBEK = 15  OR 16  OR 43                 
113300                               OR 44  OR 70  OR 71  OR 95  OR 98          
113400                               OR 99  OR 41                               
113500                       CONTINUE                                           
113600                    ELSE                                                  
113700                      MOVE MFS-ALFA-FAELT-FEL                             
113800                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
113900                      MOVE NEJ TO ALLT-SW                                 
114000                    END-IF                                                
114100                 ELSE                                                     
114200                    IF WS-AKT-KDORDBEK = 43  OR                           
114300                           44  OR 70  OR 98  OR 99  OR                    
114400                           41                                             
114500                       CONTINUE                                           
114600                    ELSE                                                  
114700                      MOVE MFS-ALFA-FAELT-FEL                             
114800                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
114900                      MOVE NEJ TO ALLT-SW                                 
115000                    END-IF                                                
115100                 END-IF                                                   
115200              WHEN 'X'                                                    
115300                 IF WS-AKT-KDORDBEK = 10                                  
115400                    CONTINUE                                              
115500                 ELSE                                                     
115600                    MOVE MFS-ALFA-FAELT-FEL                               
115700                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
115800                    MOVE NEJ   TO ALLT-SW                                 
115900                 END-IF                                                   
116000              WHEN '1'                                                    
116100                 IF WS-AKT-KDORDBEK = 43  OR 44                           
116200                    CONTINUE                                              
116300                 ELSE                                                     
116400                    MOVE MFS-ALFA-FAELT-FEL                               
116500                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
116600                    MOVE NEJ   TO ALLT-SW                                 
116700                 END-IF                                                   
116800              WHEN '2'                                                    
116900                 IF WS-AKT-KDORDBEK = 43  OR 44                           
117000                    CONTINUE                                              
117100                 ELSE                                                     
117200                    MOVE MFS-ALFA-FAELT-FEL                               
117300                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
117400                    MOVE NEJ   TO ALLT-SW                                 
117500                 END-IF                                                   
117600              WHEN OTHER                                                  
117700                 IF WS-AKT-KDORDBEK = 10  OR 15  OR 16  OR                
117800                        43  OR 44  OR 70  OR 71  OR 74  OR 95  OR         
117900                        98  OR 99  OR 41  OR 61  OR                       
118000                        92  OR 56  OR 26                                  
118100                    CONTINUE                                              
118200                 ELSE                                                     
118300                    MOVE MFS-ALFA-FAELT-FEL                               
118400                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
118500                    MOVE NEJ   TO ALLT-SW                                 
118600                 END-IF                                                   
118700           END-EVALUATE                                                   
118800        ELSE                                                              
118900           MOVE MFS-ALFA-FAELT-FEL                                        
119000                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
119100           MOVE NEJ            TO ALLT-SW                                 
119200        END-IF                                                            
119300                                                                          
119400        ADD +1                 TO WS-INDEX-MID                            
119500     END-PERFORM                                                          
119600     IF NOT ALLT-OK                                                       
119700        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
119800     END-IF                                                               
119900     .                                                                    
120000     EJECT                                                                
120100                                                                          
120200 GB-KONTROLLERA-SAMBAND SECTION.                                          
120300                                                                          
120400     MOVE +1                   TO WS-INDEX-MID                            
120500     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
120600                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
120700        MOVE MID-RAD(WS-INDEX-MID)                                        
120800                               TO WS-AKTUELL-MID-RAD                      
120900        IF WS-AKT-KDBEHX = 'D'                                            
121000           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
121100           ADD +1              TO WS-INDEX-MID                            
121200           IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                         
121300              MOVE MID-RAD(WS-INDEX-MID)                                  
121400                               TO WS-AKTUELL-MID-RAD                      
121500           END-IF                                                         
121600           PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR               
121700                         WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR          
121800                         WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR          
121900                     WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR         
122000              IF WS-AKT-KDBEHX = '1' OR '2'                               
122100                 MOVE NEJ      TO ALLT-SW                                 
122200                 MOVE MFS-ALFA-FAELT-FEL                                  
122300                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
122400                 MOVE ERR-UPPLYSTA-FEL                                    
122500                               TO MED-IDMFSFEL                            
122600              END-IF                                                      
122700              ADD +1           TO WS-INDEX-MID                            
122800              IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                      
122900                 MOVE MID-RAD(WS-INDEX-MID)                               
123000                               TO WS-AKTUELL-MID-RAD                      
123100              END-IF                                                      
123200           END-PERFORM                                                    
123300        ELSE                                                              
123400           ADD +1              TO WS-INDEX-MID                            
123500        END-IF                                                            
123600     END-PERFORM                                                          
123700     .                                                                    
123800     EJECT                                                                
123900                                                                          
124000 GC-JUSTERA-KDBEHX SECTION.                                               
124100                                                                          
124200*    JUSTERINGEN GÖRS FÖR ATT VARJE RAD SENARE I PROGRAMMET               
124300*    SKALL KUNNA BEHANDLAS VAR FÖR SIG.                                   
124400*    BORTTAG AV ORDERBEKRÄFTELSER SKULLE ANNARS BEHÖVA GÖRAS              
124500*    I MÅNGA SEKTIONER. PÅ DETTA SÄTT KOMMER ALLA BORTTAG                 
124600*    ATT GÖRAS I HEA-ANNULLERA-RAD.                                       
124700                                                                          
124800     MOVE +1                   TO WS-INDEX-MID                            
124900     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX   OR                   
125000                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
125100        MOVE MID-RAD(WS-INDEX-MID)                                        
125200                               TO WS-AKTUELL-MID-RAD                      
125300        IF WS-AKT-KDBEHX = '1' OR '2'                                     
125400           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
125500           PERFORM GCA-D-MARKERA-1-2-RADER                                
125600        ELSE                                                              
125700           IF WS-AKT-KDBEHX = 'X'                                         
125800              IF OHUV-KDTPOTYP > +0                                       
125900                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
126000                 PERFORM GCB-D-MARKERA-RADER                              
126100              ELSE                                                        
126200                 ADD +1        TO WS-INDEX-MID                            
126300              END-IF                                                      
126400           ELSE                                                           
126500              IF WS-AKT-KDBEHX = 'D'                                      
126600                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
126700                 PERFORM GCB-D-MARKERA-RADER                              
126800              ELSE                                                        
126900                 ADD +1        TO WS-INDEX-MID                            
127000              END-IF                                                      
127100           END-IF                                                         
127200        END-IF                                                            
127300     END-PERFORM                                                          
127400     .                                                                    
127500     EJECT                                                                
127600                                                                          
127700 GCA-D-MARKERA-1-2-RADER SECTION.                                         
127800                                                                          
127900     ADD +1                    TO WS-INDEX-MID                            
128000     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
128100        MOVE MID-RAD(WS-INDEX-MID)                                        
128200                               TO WS-AKTUELL-MID-RAD                      
128300     END-IF                                                               
128400     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
128500                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
128600                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
128700               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
128800                                                                          
128900        IF WS-AKT-KDBEHX = ' '                                            
129000           IF WS-AKT-KDORDBEK = 41                                        
129100              CONTINUE                                                    
129200           ELSE                                                           
129300              MOVE 'D'         TO MID-KDBEHX(WS-INDEX-MID)                
129400           END-IF                                                         
129500        END-IF                                                            
129600        ADD +1                 TO WS-INDEX-MID                            
129700        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
129800           MOVE MID-RAD(WS-INDEX-MID)                                     
129900                               TO WS-AKTUELL-MID-RAD                      
130000        END-IF                                                            
130100     END-PERFORM                                                          
130200     .                                                                    
130300     EJECT                                                                
130400                                                                          
130500 GCB-D-MARKERA-RADER SECTION.                                             
130600                                                                          
130700     ADD +1                    TO WS-INDEX-MID                            
130800     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
130900        MOVE MID-RAD(WS-INDEX-MID)                                        
131000                               TO WS-AKTUELL-MID-RAD                      
131100     END-IF                                                               
131200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
131300                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
131400                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
131500               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
131600                                                                          
131700        IF WS-AKT-KDBEHX = ' '  AND                                       
131800                 (WS-AKT-IDDC = WS-IDDC-SPAR)                             
131900                                                                          
132000           MOVE 'D'            TO MID-KDBEHX(WS-INDEX-MID)                
132100        END-IF                                                            
132200        ADD +1                 TO WS-INDEX-MID                            
132300        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
132400           MOVE MID-RAD(WS-INDEX-MID)                                     
132500                               TO WS-AKTUELL-MID-RAD                      
132600        END-IF                                                            
132700     END-PERFORM                                                          
132800     .                                                                    
132900     EJECT                                                                
133000 H-BEHANDLA-RADER SECTION.                                                
133100                                                                          
133200     PERFORM HA-SKAPA-SPAR-ARB-PER-CL                                     
133300                                                                          
133400     IF MFS-FIRST  OR  MFS-NEXT                                           
133500                                                                          
133600        PERFORM HB-LAS-IN-13-RADER                                        
133700        IF  WS-INDEX-MOD = +1  AND (W-IDTRANS = '4242' OR                 
133800                                                '4297' OR                 
133900                                                '4206')                   
134000           MOVE JA             TO AVSLUTA-SW                              
134100        END-IF                                                            
134200     ELSE                                                                 
134300        IF MID-FLANNULL NOT = JA                                          
134400                                                                          
134500           IF MFS-UPDATE  OR  MFS-QUERY                                   
134600              PERFORM HE-UPPDATERA-AKT-SIDA                               
134700              PERFORM HD-UPPDATERA-OBEH-RADER                             
134800              IF START-4242                                               
134900                MOVE 'U'         TO 4242-MID-KDTRTYP                      
135000                PERFORM S03-STARTA-RADBEHANDLINGEN                        
135100              ELSE                                                        
135200                PERFORM HB-LAS-IN-13-RADER                                
135300                IF  WS-INDEX-MOD = +1                                     
135400                   MOVE JA       TO AVSLUTA-SW                            
135500                END-IF                                                    
135600              END-IF                                                      
135700           ELSE                                                           
135800              IF MFS-UPD-V                                                
135900                 PERFORM HE-UPPDATERA-AKT-SIDA                            
136000                 PERFORM HG-UPPDATERA-RESTERANDE-RADER                    
136100              END-IF                                                      
136200           END-IF                                                         
136300        ELSE                                                              
136400           PERFORM HH-ANNULLERA-ORDER                                     
136500        END-IF                                                            
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900                                                                          
137000 HA-SKAPA-SPAR-ARB-PER-CL SECTION.                                        
137100                                                                          
137200     PERFORM IMS-14-GNP-ORQI-WDQ212                                       
137300                                                                          
137400     MOVE ARB-KDFRAKT  TO SPAR-ARB-KDFRAKT                                
137500     MOVE ARB-KDFDKRAV TO SPAR-ARB-KDFDKRAV                               
137600     MOVE ARB-KDROPACK TO SPAR-ARB-KDROPACK                               
137700     .                                                                    
137800     EJECT                                                                
137900 HB-LAS-IN-13-RADER SECTION.                                              
138000                                                                          
138100     PERFORM MFS-RENSA-ALLA-FAELT                                         
138200     MOVE +0                   TO WS-IDARTNR-SPAR                         
138300                                  WS-IDLOPNR-SPAR                         
138400                                                                          
138500     PERFORM HBA-VISA-OBKR-OCH-SATS-RADER                                 
138600                                                                          
138700     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
138800                                                                          
138900     IF WS-INDEX-MOD > WS-INDEX-MOD-MAX AND                               
139000           OBKR-SEGMENT-FINNS   AND                                       
139100          (OBKR-KDORDBEK = 41 OR 61)                                      
139200                                                                          
139300        PERFORM HBC-KONTROLLERA-SIDSLUT                                   
139400     END-IF                                                               
139500     .                                                                    
139600     EJECT                                                                
139700                                                                          
139800 HBA-VISA-OBKR-OCH-SATS-RADER SECTION.                                    
139900                                                                          
140000     MOVE +1                   TO WS-INDEX-MOD                            
140100                                                                          
140200     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
140300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
140400              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
140500                                                                          
140600        PERFORM HBAA-REDIGERA-ORDERBEKR-RAD                               
140700                                                                          
140800        IF OBKR-KDORDBEK = 57                                             
140900           PERFORM HBAB-VISA-SATS-ARTIKLAR                                
141000        END-IF                                                            
141100        MOVE OBKR-IDARTNR   TO WS-IDARTNR-SPAR                            
141200        MOVE OBKR-IDLOPNR   TO WS-IDLOPNR-SPAR                            
141300                                                                          
141400        ADD +1              TO WS-INDEX-MOD                               
141500                                                                          
141600        IF NOT NEXT-SATS                                                  
141700           PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                         
141800*----------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN              
141900           MOVE STATUS-WS      TO STATUS-OBKR-WS                          
142000        END-IF                                                            
142100     END-PERFORM                                                          
142200     .                                                                    
142300     EJECT                                                                
142400                                                                          
142500 HBAA-REDIGERA-ORDERBEKR-RAD SECTION.                                     
142600                                                                          
142700     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
142810     IF OBKR-KDORDBEK = 41 OR 61                                          
142900        MOVE '*'               TO MOD-ASTERIX(WS-INDEX-MOD)               
143000     END-IF                                                               
143100                                                                          
143200     MOVE SPACE                TO MOD-KDBEHX(WS-INDEX-MOD)                
143300     IF OBKR-KDORDBEK = 21 OR 51                                          
143400                     OR 52 OR 53 OR 54 OR 55 OR 57 OR 58 OR 59            
143500                     OR 66 OR 67 OR 72 OR 73 OR 74 OR 75 OR 76            
143600                     OR 80 OR 81 OR 82 OR 85                              
143700        MOVE 'B'               TO MOD-KDBEHX(WS-INDEX-MOD)                
143800        MOVE MFS-STAENG-FAELT  TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
143900     END-IF                                                               
144010     IF OBKR-KDORDBEK = 41 OR 61                                          
144100        IF OBKR-IDARTNR = WS-IDARTNR-SPAR AND                             
144200                 OBKR-IDLOPNR = WS-IDLOPNR-SPAR                           
144300           MOVE SPACE          TO MOD-KDBEHX(WS-INDEX-MOD)                
144400           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
144500                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
144600        ELSE                                                              
144700           MOVE 'B'              TO MOD-KDBEHX(WS-INDEX-MOD)              
144800           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)         
144900        END-IF                                                            
145000     END-IF                                                               
145100     IF (OBKR-KDORDBEK = 61 )  AND                                        
145200                         OBKR-IDARTNR-TILLK > +0                          
145300        MOVE 'A'               TO MOD-KDBEHX(WS-INDEX-MOD)                
145400     END-IF                                                               
145500     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
145600     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
145700     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
145800                                                                          
145900     PERFORM S22-HAMTA-BENAMNING                                          
146000                                                                          
146100     MOVE OBKR-IDDC            TO MOD-IDDC-RAD(WS-INDEX-MOD)              
146200                                                                          
146300     IF OBKR-KDORDBEK = 10 OR 15 OR 16 OR 43 OR 44 OR 70 OR               
146400                        71 OR 74 OR 95                                    
146500        MOVE OBKR-KVBEART-Q    TO MOD-KVANTAL(WS-INDEX-MOD)               
146600     ELSE                                                                 
146700        IF OBKR-KDORDBEK = 80 OR 85                                       
146800           MOVE OBKR-KVANNANT  TO MOD-KVANTAL(WS-INDEX-MOD)               
146900        ELSE                                                              
147000           IF OBKR-KDORDBEK = 92 OR 98 OR 99                              
147100              MOVE OBKR-KVPRERO TO MOD-KVANTAL(WS-INDEX-MOD)              
147200           ELSE                                                           
147300              MOVE OBKR-KVBEART TO MOD-KVANTAL(WS-INDEX-MOD)              
147400           END-IF                                                         
147500        END-IF                                                            
147600     END-IF                                                               
147700                                                                          
147800     IF OBKR-KDORDBEK = 43 OR 44                                          
147900        MOVE OBKR-KVQPACK      TO MOD-KVQPACK(WS-INDEX-MOD)               
148000     ELSE                                                                 
148100        MOVE +0                TO MOD-KVQPACK(WS-INDEX-MOD)               
148200     END-IF                                                               
148300     IF OBKR-KDORDBEK = 10                                                
148400        MOVE OBKR-IDKUNDRF-RO (1:7)                                       
148500                               TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
148600     ELSE                                                                 
148700        MOVE +0                TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
148800     END-IF                                                               
148900                                                                          
149000     IF OBKR-KDORDBEK = 15 OR 16 OR 21                                    
149100                     OR 43 OR 44 OR 51 OR 52 OR 53 OR 54                  
149200                     OR 55 OR 57 OR 67 OR 70 OR 72 OR 73 OR 74            
149300                     OR 75 OR 76 OR 80 OR 95 OR 99 OR 58 OR 92            
149400                     OR 66 OR 71 OR 98                                    
149500        IF OBKR-IDARTNR-TILLK > 0                                         
149600           MOVE OBKR-IDARTNR-TILLK                                        
149700                               TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
149800           MOVE '-'            TO MOD-STRAEK(WS-INDEX-MOD)                
149900           MOVE OBKR-REKSIFFR-TILLK                                       
150000                               TO MOD-REKSIFFR(WS-INDEX-MOD)              
150100        END-IF                                                            
150200     ELSE                                                                 
150310        IF OBKR-KDORDBEK = 41 OR 61                                       
150400*------ ERSATT ARTIKEL                                                    
150500                                                                          
150600           PERFORM HBAAA-FIXA-ERSATNING-RAD                               
150700        END-IF                                                            
150800     END-IF                                                               
150900                                                                          
151000     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
151100     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
151200     MOVE OBKR-IDARTNR         TO WS-AKT-IDARTNR-URS                      
151300     MOVE OBKR-IDLOPNR-RO      TO WS-AKT-IDLOPNR-RO                       
151400                                                                          
151500     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
151600     .                                                                    
151700     EJECT                                                                
151800 HBAAA-FIXA-ERSATNING-RAD SECTION.                                        
151900                                                                          
152000     IF (OBKR-IDARTNR NOT = WS-IDARTNR-SPAR)     OR                       
152100           (OBKR-IDARTNR = WS-IDARTNR-SPAR  AND                           
152200              OBKR-IDLOPNR NOT = WS-IDLOPNR-SPAR)                         
152300        PERFORM S22-HAMTA-BENAMNING                                       
152400        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
152500                      TO MOD-IDDC-ATTR(WS-INDEX-MOD)                      
152600                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
152700                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
152800                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
152900     ELSE                                                                 
153000        IF OBKR-IDARTNR-TILLK = +0                                        
153100*------ TILLKOMMANDE TEXT                                                 
153200           MOVE MFS-RENSA-FAELT                                           
153300                           TO MOD-IDARTNR(WS-INDEX-MOD)                   
153400           MOVE OBKR-BEERS TO MOD-BEART(WS-INDEX-MOD)                     
153500           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
153600                      TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)                    
153700                         MOD-IDDC-ATTR(WS-INDEX-MOD)                      
153800                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
153900                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
154000                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
154100        ELSE                                                              
154200*-------TILLKOMMANDE ARTIKEL                                              
154300           MOVE OBKR-IDARTNR-TILLK                                        
154400                         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)                
154500           MOVE '-'      TO MOD-STRAEK(WS-INDEX-MOD)                      
154600           MOVE OBKR-REKSIFFR-TILLK                                       
154700                         TO MOD-REKSIFFR(WS-INDEX-MOD)                    
154800           PERFORM S22-HAMTA-BENAMNING                                    
154900           MOVE OBKR-KVBEART-TILLK                                        
155000                         TO MOD-KVANTAL(WS-INDEX-MOD)                     
155100           MOVE OBKR-DIERS-KVOT                                           
155200                         TO MOD-KVQPACK(WS-INDEX-MOD)                     
155300        END-IF                                                            
155400     END-IF                                                               
155500     .                                                                    
155600     EJECT                                                                
155700 HBAB-VISA-SATS-ARTIKLAR SECTION.                                         
155800                                                                          
155900     MOVE NEJ                  TO NEXT-SATS-SW                            
156000                                                                          
156100     MOVE +1                   TO WS-INDEX-SATS                           
156200     PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
156300        MOVE SPACE             TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
156400        ADD +1                 TO WS-INDEX-SATS                           
156500     END-PERFORM                                                          
156600                                                                          
156700     MOVE +1                   TO WS-INDEX-SATS                           
156800                                  WS-INDEX                                
156900     MOVE OBKR-IDARTNR         TO W-J1-IDARTNR                            
157000                                                                          
157100     PERFORM IMS-18A-GU-SATB-WDJ111-01                                    
157200     PERFORM UNTIL SEGMENT-SAKNAS                                         
157300             OR    WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
157400             OR    WS-INDEX  > +10                                        
157500                                                                          
157600        MOVE RAD-TISTADAT   TO TMP1-YYMMDD                                
157700        MOVE RAD-TISTODAT   TO TMP2-YYMMDD                                
157800        MOVE DAGENS-DATUM   TO TMP3-YYMMDD                                
157900        PERFORM WY2000P1                                                  
158000        IF STR-IDARTNR < +100000000 AND STR-TIBORT = +0 AND               
158100           TMP1-YYMMDD  NOT > TMP3-YYMMDD   AND                           
158200           TMP2-YYMMDD  NOT < TMP3-YYMMDD                                 
158300                                                                          
158400           IF WS-INDEX-MOD = WS-INDEX-MOD-MAX                             
158500              MOVE JA             TO NEXT-SATS-SW                         
158600              MOVE +6             TO WS-INDEX-SATS                        
158700           ELSE                                                           
158800              MOVE STR-IDARTNR TO WS-NUM-9                                
158900              MOVE WS-ALFA-9   TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
159000              ADD +1           TO WS-INDEX-SATS                           
159100           END-IF                                                         
159200        END-IF                                                            
159300        IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                          
159400           PERFORM IMS-18-GN-SATB-WDJ111-01                               
159500           ADD  +1             TO WS-INDEX                                
159600        END-IF                                                            
159700     END-PERFORM                                                          
159800     IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                             
159900                                                                          
160000        COMPUTE WS-INDEX = WS-INDEX-MOD + WS-INDEX-SATS - 1               
160100        IF WS-INDEX NOT > WS-INDEX-MOD-MAX                                
160200                                                                          
160300           MOVE +1       TO WS-INDEX-SATS                                 
160400           PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                
160500                 OR WS-IDARTNR-SATS(WS-INDEX-SATS) = SPACE                
160600                                                                          
160700              ADD +1     TO WS-INDEX-MOD                                  
160800              PERFORM HBABA-REDIGERA-SATS-RAD                             
160900              ADD +1     TO WS-INDEX-SATS                                 
161000           END-PERFORM                                                    
161100        ELSE                                                              
161200           MOVE JA       TO NEXT-SATS-SW                                  
161300        END-IF                                                            
161400     END-IF                                                               
161500     IF NEXT-SATS                                                         
161600*-----OM EJ ALLA SATS-ART FÅR PLATS PÅ SIDAN RADERAS RAD MED              
161700*-----ORDBEK = 57 OCH DEN SPARAS FÖR NÄSTA SIDA(I HBB-SECTIONEN)          
161800        MOVE MFS-RENSA-FAELT   TO MOD-KDORDBEK(WS-INDEX-MOD)              
161900                                  MOD-KDBEHX(WS-INDEX-MOD)                
162000                                  MOD-IDARTNR(WS-INDEX-MOD)               
162100                                  MOD-BEART(WS-INDEX-MOD)                 
162200                                  MOD-IDDC-RAD(WS-INDEX-MOD)              
162300                                  MOD-KVANTAL(WS-INDEX-MOD)               
162400                                  MOD-KVQPACK(WS-INDEX-MOD)               
162500                                  MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
162600        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
162700        MOVE MED-UPPLYSN-UPPDAT-PF                                        
162800                               TO MED-IDMFSFEL                            
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200                                                                          
163300 HBABA-REDIGERA-SATS-RAD SECTION.                                         
163400                                                                          
163500     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
163600     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
163700                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
163800                                                                          
163900     MOVE SPACE                TO MOD-ASTERIX(WS-INDEX-MOD)               
164000     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
164100     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(WS-INDEX-MOD)               
164200                                                                          
164300     MOVE WS-IDARTNR-SATS(WS-INDEX-SATS)                                  
164400                               TO MOD-BEART(WS-INDEX-MOD)                 
164500     INSPECT MOD-BEART(WS-INDEX-MOD) REPLACING LEADING ZERO BY            
164600                                                         SPACE            
164700     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
164800                           TO MOD-IDDC-ATTR(WS-INDEX-MOD)                 
164900                              MOD-KVANTAL-ATTR(WS-INDEX-MOD)              
165000                              MOD-KVQPACK-ATTR(WS-INDEX-MOD)              
165100                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)          
165200                                                                          
165300     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
165400     .                                                                    
165500     EJECT                                                                
165600                                                                          
165700 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
165800                                                                          
165900     IF OBKR-SEGMENT-FINNS                                                
166000        MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                        
166100        MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                        
166200        MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                       
166300        MOVE OBKR-IDDC         TO MOD-IDDC-NEXT                           
166400        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                       
166500                                                                          
166600        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
166700        MOVE MED-UPPLYSN-UPPDAT-PF                                        
166800                               TO MED-IDMFSFEL                            
166900     ELSE                                                                 
167000        MOVE ZERO              TO MOD-IDARTNR-NEXT                        
167100                                  MOD-IDLOPNR-NEXT                        
167200                                  MOD-IDSEKVNR-NEXT                       
167300                                  MOD-IDDC-NEXT                           
167400                                  MOD-KDORDBEK-NEXT                       
167500     END-IF                                                               
167600     .                                                                    
167700     EJECT                                                                
167800                                                                          
167900 HBC-KONTROLLERA-SIDSLUT SECTION.                                         
168000                                                                          
168100     MOVE WS-INDEX-MOD-MAX     TO WS-INDEX-MOD                            
168200     MOVE MOD-KEYS(WS-INDEX-MOD)                                          
168300                               TO WS-AKT-KEYS                             
168400     IF OBKR-IDARTNR = WS-AKT-IDARTNR-URS AND                             
168500        OBKR-IDLOPNR = WS-AKT-IDLOPNR                                     
168600                                                                          
168700        PERFORM UNTIL WS-INDEX-MOD = +1 OR                                
168800                     (OBKR-IDARTNR NOT = WS-AKT-IDARTNR-URS OR            
168900                      OBKR-IDLOPNR NOT = WS-AKT-IDLOPNR)                  
169000                                                                          
169100           SUBTRACT 1 FROM WS-INDEX-MOD                                   
169200           MOVE MOD-KEYS(WS-INDEX-MOD)                                    
169300                               TO WS-AKT-KEYS                             
169400        END-PERFORM                                                       
169500                                                                          
169600        ADD +1 TO WS-INDEX-MOD                                            
169700                                                                          
169800*---- BLÄDDRINGSVÄRDENA MÅSTE JUSTERAS OM NÄR VI BACKAR RADER             
169900                                                                          
170000        MOVE MOD-KEYS(WS-INDEX-MOD)                                       
170100                               TO WS-AKT-KEYS                             
170200        MOVE WS-AKT-IDARTNR-URS TO MOD-IDARTNR-NEXT                       
170300        MOVE WS-AKT-IDLOPNR    TO MOD-IDLOPNR-NEXT                        
170400        MOVE WS-AKT-IDSEKVNR   TO MOD-IDSEKVNR-NEXT                       
170500        MOVE MOD-IDDC-RAD(WS-INDEX-MOD)                                   
170600                               TO MOD-IDDC-NEXT                           
170700        MOVE MOD-KDORDBEK(WS-INDEX-MOD)                                   
170800                               TO MOD-KDORDBEK-NEXT                       
170900        MOVE WS-INDEX-MOD      TO WS-INDEX                                
171000        PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                         
171100          MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
171200                                  MOD-ASTERIX(WS-INDEX)                   
171300                                  MOD-KDBEHX(WS-INDEX)                    
171400                                  MOD-IDARTNR(WS-INDEX)                   
171500                                  MOD-BEART(WS-INDEX)                     
171600                                  MOD-IDDC-RAD(WS-INDEX)                  
171700                                  MOD-KVANTAL(WS-INDEX)                   
171800                                  MOD-KVQPACK(WS-INDEX)                   
171900                                  MOD-IDKUNDRF-RO(WS-INDEX)               
172000                                  MOD-KEYS(WS-INDEX)                      
172100          ADD +1               TO WS-INDEX                                
172200        END-PERFORM                                                       
172300     ELSE                                                                 
172400        ADD +1                 TO WS-INDEX-MOD                            
172500     END-IF                                                               
172600                                                                          
172700     MOVE MED-FLER-SIDOR       TO MED-IDMFSINF                            
172800     MOVE MED-UPPLYSN-UPPDAT-PF                                           
172900                               TO MED-IDMFSFEL                            
173000     .                                                                    
173100     EJECT                                                                
173200 HD-UPPDATERA-OBEH-RADER SECTION.                                         
173300                                                                          
173400     MOVE NEJ                  TO AKT-SIDA-SW                             
173500                                                                          
173600     MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                      
173700     MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-MAX                        
173800     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
173900     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
174000     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
174100     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
174200                                                                          
174300     PERFORM IMS-01-GHU-ORQM-WDQ101-FOERE                                 
174400     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
174500                                                                          
174600        PERFORM S02-GODKANN-RAD                                           
174700                                                                          
174800        PERFORM IMS-02-GHN-ORQM-WDQ101-FOERE                              
174900     END-PERFORM                                                          
175000                                                                          
175100     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
175200                                  W-Q1-IDLOPNR-MAX                        
175300                                  W-Q1-IDSEKVNR-MAX                       
175400                                  W-Q1-IDDC-MAX                           
175500                                  W-Q1-KDORDBEK-MAX                       
175600     .                                                                    
175700     EJECT                                                                
175800 HE-UPPDATERA-AKT-SIDA SECTION.                                           
175900                                                                          
176000     MOVE JA                   TO AKT-SIDA-SW                             
176100     MOVE +1 TO WS-INDEX-MID                                              
176200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
176300                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
176400       MOVE MID-RAD(WS-INDEX-MID)                                         
176500                             TO WS-AKTUELL-MID-RAD                        
176600       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-UNIK                     
176700       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
176800       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
176900       MOVE WS-AKT-IDDC          TO W-Q1-IDDC-UNIK                        
177000       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
177100                                                                          
177200       PERFORM IMS-05-GHU-ORQM-WDQ101-UNIK                                
177300       IF SEGMENT-FINNS                                                   
177400         IF MID-IDARTNR(WS-INDEX-MID) = ZERO                              
177500           MOVE JA       TO OBKR-FLOBOK                                   
177600           PERFORM IMS-06-REPL-ORQM-WDQ101                                
177700         ELSE                                                             
177800           IF WS-AKT-KDBEHX = SPACE                                       
177900             PERFORM S02-GODKANN-RAD                                      
178000           ELSE                                                           
178100             IF WS-AKT-KDBEHX = 'A'                                       
178200               MOVE JA       TO OBKR-FLOBOK                               
178300               PERFORM IMS-06-REPL-ORQM-WDQ101                            
178400             ELSE                                                         
178500               IF WS-AKT-KDBEHX = 'D'                                     
178600                 PERFORM HEA-ANNULLERA-RAD                                
178700               ELSE                                                       
178800                 IF WS-AKT-KDBEHX = '1' OR '2'                            
178900                   PERFORM HEA-ANNULLERA-RAD                              
179000                   IF WS-AKT-KDBEHX = '1'                                 
179100                     MOVE +1       TO OBKR-KDKVBRYT                       
179200                   ELSE                                                   
179300                     MOVE +2       TO OBKR-KDKVBRYT                       
179400                   END-IF                                                 
179500                   MOVE +0         TO OBKR-KVPREAVB                       
179600                                      OBKR-KVPRERO                        
179700                   PERFORM S01-SKRIV-MID-TILL-4242                        
179800                 ELSE                                                     
179900                   IF WS-AKT-KDBEHX = 'X'                                 
180000                     PERFORM HEB-BACKA-WDA5-STATUS                        
180100                                                                          
180200                     PERFORM HEA-ANNULLERA-RAD                            
180300                   ELSE                                                   
180400                     IF WS-AKT-KDBEHX = 'B' AND OBKR-KDORDKL = +0         
180500                       AND (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54          
180600                                         OR 55 OR 57 OR 67)               
180700                       PERFORM S11-SKRIV-VOR-RAD                          
180800                       MOVE JA TO OBKR-FLOBOK                             
180900                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
181000                     ELSE                                                 
181100                       MOVE JA TO OBKR-FLOBOK                             
181200                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
181300                     END-IF                                               
181400                   END-IF                                                 
181500                 END-IF                                                   
181600               END-IF                                                     
181700             END-IF                                                       
181800           END-IF                                                         
181900         END-IF                                                           
182000       END-IF                                                             
182100       ADD +1                  TO WS-INDEX-MID                            
182200     END-PERFORM                                                          
182300     .                                                                    
182400     EJECT                                                                
182500                                                                          
182600 HEA-ANNULLERA-RAD SECTION.                                               
182700                                                                          
182800     IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                           
182900        IF OBKR-IDLEVNR = SPACE                                           
183000                                                                          
183100           IF OBKR-IDARTNR-TILLK > +0                                     
183200              MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                        
183300           ELSE                                                           
183400              MOVE OBKR-IDARTNR       TO W-IDARTNR                        
183500           END-IF                                                         
183600                                                                          
183700           MOVE OBKR-IDDC             TO WS-IDDC                          
183800           IF WS-IDDC NOT = W-IDDC-B6                                     
183900              MOVE WS-IDDC TO W-IDDC-B6                                   
184000              PERFORM IMS-GU-WDB601                                       
184100           END-IF                                                         
184200           IF DCS-CDC                                                     
184300             PERFORM IMS-21-GHU-ARTM-WDK901                               
184400                                                                          
184500             PERFORM HEAA-BACKA-WDK9-SALDON                               
184600                                                                          
184700             PERFORM IMS-22-REPL-ARTM-WDK901                              
184800           ELSE                                                           
184900             MOVE OBKR-IDDC TO W-IDDC                                     
185000             PERFORM IMS-11-GHU-WDK711                                    
185100             IF OBKR-KDORDKL = +0 OR +1                                   
185200               SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-DAG                
185300             ELSE                                                         
185400               IF OBKR-KDORDKL = +2 OR +3 OR +4                           
185500                 SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK             
185600               END-IF                                                     
185700             END-IF                                                       
185800             PERFORM IMS-12-REPL-WDK711                                   
185900           END-IF                                                         
186000        END-IF                                                            
186100     END-IF                                                               
186200     PERFORM IMS-08-DLET-ORQM-WDQ101                                      
186300     PERFORM S23-DELETE-PRICE-Q-LINE                                      
186400     .                                                                    
186500     EJECT                                                                
186600                                                                          
186700 HEAA-BACKA-WDK9-SALDON SECTION.                                          
186800                                                                          
186900     IF OBKR-KDORDKL = +0                                                 
187000        IF OBKR-KVPREAVB > +0                                             
187100           COMPUTE ART-KVPREAVB-VOR =                                     
187200           ART-KVPREAVB-VOR - OBKR-KVPREAVB                               
187300        END-IF                                                            
187400        IF OBKR-KVPRERO > +0                                              
187500           COMPUTE ART-KVPRERO-DAG =                                      
187600           ART-KVPRERO-DAG - OBKR-KVPRERO                                 
187700        END-IF                                                            
187800        IF OHUV-IDKAMPRF = +0 AND                                         
187900           OBKR-IDKUNDRF-RO = '0000000   '                                
188000           COMPUTE ART-KVOKS-VOR =                                        
188100           ART-KVOKS-VOR - OBKR-KVBEART-Q                                 
188200        END-IF                                                            
188300     ELSE                                                                 
188400        IF OBKR-KDORDKL = +1                                              
188500           IF OBKR-KVPREAVB > +0                                          
188600              COMPUTE ART-KVPREAVB-DAG =                                  
188700              ART-KVPREAVB-DAG - OBKR-KVPREAVB                            
188800           END-IF                                                         
188900           IF OBKR-KVPRERO > +0                                           
189000              COMPUTE ART-KVPRERO-DAG =                                   
189100              ART-KVPRERO-DAG - OBKR-KVPRERO                              
189200           END-IF                                                         
189300           IF OHUV-IDKAMPRF = +0 AND                                      
189400              OBKR-IDKUNDRF-RO = '0000000   '                             
189500              COMPUTE ART-KVOKS-DAG =                                     
189600              ART-KVOKS-DAG - OBKR-KVBEART-Q                              
189700           END-IF                                                         
189800        ELSE                                                              
189900           IF OBKR-KVPREAVB > +0                                          
190000              COMPUTE ART-KVPREAVB-BULK =                                 
190100              ART-KVPREAVB-BULK - OBKR-KVPREAVB                           
190200           END-IF                                                         
190300           IF OBKR-KVPRERO > +0                                           
190400              COMPUTE ART-KVPRERO-BULK =                                  
190500              ART-KVPRERO-BULK - OBKR-KVPRERO                             
190600           END-IF                                                         
190700           IF OHUV-IDKAMPRF = +0 AND                                      
190800              OBKR-IDKUNDRF-RO = '0000000   '                             
190900              COMPUTE ART-KVOKS-BULK =                                    
191000              ART-KVOKS-BULK - OBKR-KVBEART-Q                             
191100           END-IF                                                         
191200        END-IF                                                            
191300     END-IF                                                               
191400     .                                                                    
191500     EJECT                                                                
191600 HEB-BACKA-WDA5-STATUS SECTION.                                           
191700                                                                          
191800     MOVE OBKR-IDDISTR       TO W-A5-IDDISTR                              
191900     MOVE OBKR-IDKUNDNR      TO W-A5-IDKUNDNR                             
192000     MOVE WS-AKT-IDKUNDRF-RO (3:5)                                        
192100                             TO W-A5-IDKUNDRF                             
192200     INSPECT W-A5-IDKUNDRF REPLACING LEADING                              
192300                                 SPACE BY ZERO                            
192400     MOVE WS-AKT-IDARTNR     TO W-A5-IDARTNR                              
192500     MOVE WS-AKT-IDLOPNR-RO  TO W-A5-IDLOPNR                              
192600                                                                          
192700     PERFORM IMS-26-GHU-ORDP-WDA501                                       
192800                                                                          
192900     MOVE '3'             TO RAD-KDSTARAD                                 
193000     MOVE '00000     ' TO RAD-IDKUNDRF-LEV                                
193100     PERFORM IMS-27-REPL-ORDP-WDA501                                      
193200                                                                          
193300     IF SPAR-ARB-KDROPACK NOT = ZERO AND SPACE                            
193400                                                                          
193500        MOVE OBKR-IDDC         TO W-IDDC                                  
193600                                                                          
193700        PERFORM IMS-13-GHNP-ORQI-WDQ212-UNIK                              
193800        IF SEGMENT-FINNS                                                  
193900          MOVE ZERO            TO ARB-KDROPACK                            
194000          PERFORM IMS-15-REPL-ORQI-WDQ212                                 
194100        END-IF                                                            
194200     END-IF                                                               
194300     .                                                                    
194400     EJECT                                                                
194500                                                                          
194600 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
194700                                                                          
194800     MOVE NEJ                  TO AKT-SIDA-SW                             
194900*--- BEHANDLA RESTERANDE OBKR PÅ ORDERN                                   
195000                                                                          
195100     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
195200     MOVE +1                   TO WS-RADER                                
195300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WS-RADER > +13         
195400                                                                          
195500        PERFORM S02-GODKANN-RAD                                           
195600        ADD  +1               TO WS-RADER                                 
195700                                                                          
195800        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
195900     END-PERFORM                                                          
196000                                                                          
196100     IF START-4242                                                        
196200        MOVE 'V'              TO 4242-MID-KDTRTYP                         
196300        PERFORM S03-STARTA-RADBEHANDLINGEN                                
196400     ELSE                                                                 
196500        IF WS-RADER = 14 AND SEGMENT-FINNS                                
196600           PERFORM HGA-OMSKEDULERA                                        
196700        ELSE                                                              
196800           MOVE JA             TO AVSLUTA-SW                              
196900        END-IF                                                            
197000     END-IF                                                               
197100     .                                                                    
197200     EJECT                                                                
197300 HGA-OMSKEDULERA SECTION.                                                 
197400                                                                          
197500     MOVE MFS-KDMFSFOR         TO 4243-SPRAK                              
197600     MOVE ALL '+'              TO 4243-IDDISTR-IN                         
197700                                  4243-IDKUNDNR-IN                        
197800                                  4243-IDORDNR-IN                         
197900     MOVE WS-IDDISTR           TO 4243-IDDISTR-UT                         
198000     MOVE WS-IDKUNDNR          TO 4243-IDKUNDNR-UT                        
198100     MOVE WS-IDORDNR           TO 4243-IDORDNR-UT                         
198200                                                                          
198300     PERFORM IMS-INSERT-4243-MSG                                          
198400     MOVE JA                   TO HOPP                                    
198500     .                                                                    
198600     EJECT                                                                
198700                                                                          
198800 HH-ANNULLERA-ORDER SECTION.                                              
198900                                                                          
199000     MOVE MFS-KDMFSFOR         TO 4292-SPRAK                              
199100     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
199200     MOVE WS-IDDISTR           TO 4292-IDDISTR                            
199300     MOVE WS-IDKUNDNR          TO 4292-IDKUNDNR                           
199400     MOVE W-IDORDNR            TO 4292-IDKUNDRF                           
199500                                                                          
199600     PERFORM IMS-INSERT-4292-MSG                                          
199700                                                                          
199800     MOVE 'W4O24101'           TO MFS-IDMOD                               
199900                                                                          
200000     MOVE '4241'               TO MOD-W4O24301(1:4)                       
200100                                                                          
200200     MOVE MED-ORDER-ANNULLERAD TO MED-IDMFSFEL                            
200300     CALL WMEDKONV USING MED-WMEDAREA                                     
200400     MOVE MED-MFSFEL           TO MOD-W4O24301(5:40)                      
200500                                                                          
200600     MOVE 4241-MOD-LAENGD      TO MSG-KVLL                                
200700     PERFORM IMS-INSERT-MSG                                               
200800     MOVE JA                   TO HOPP                                    
200900     .                                                                    
201000     EJECT                                                                
201100                                                                          
201200 I-STARTA-ORDERAVSLUT SECTION.                                            
201300                                                                          
201400     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
201500     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
201600     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
201700     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
201800                                                                          
201900     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
202000     PERFORM IMS-INSERT-4298-MSG                                          
202100     .                                                                    
202200     EJECT                                                                
202300                                                                          
202400 M-HOPPA-TILL-ORDERHUVUD-4241 SECTION.                                    
202500                                                                          
202600     MOVE 'W4O24101'           TO MFS-IDMOD                               
202700                                                                          
202800     MOVE '4241'               TO MOD-W4O24301(1:4)                       
202900                                                                          
203000     MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                            
203100     CALL WMEDKONV USING MED-WMEDAREA                                     
203200     MOVE MED-MFSFEL           TO MOD-W4O24301(5:40)                      
203300                                                                          
203400     MOVE 4241-MOD-LAENGD      TO MSG-KVLL                                
203500     PERFORM IMS-INSERT-MSG                                               
203600     MOVE JA                   TO HOPP                                    
203700     .                                                                    
203800     EJECT                                                                
203900 O-KONTROLLERA-BEHORIGHET SECTION.                                        
204000     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
204100     MOVE '4243'            TO    SEC-IDTRANS                             
204200     MOVE WS-IDDISTR        TO    SEC-IDKEY                               
204300                                                                          
204400     CALL WSECURIT          USING SEC-IDUSER                              
204500                                  SEC-IDTRANS                             
204600                                  SEC-IDKEY                               
204700                                  SEC-KDSVAR                              
204800                                                                          
204900     IF SEC-KDSVAR = OBEHORIG                                             
205000        MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                 
205100        MOVE NEJ          TO ALLT-SW                                      
205200        MOVE OBEHORIG     TO SPAR-BEHORIGHETS-KONTR                       
205300     ELSE                                                                 
205400        CONTINUE                                                          
205500     END-IF                                                               
205600     .                                                                    
205700     EJECT                                                                
205800                                                                          
205900 Z-FINIT SECTION.                                                         
206000                                                                          
206100     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
206200         CALL WMEDKONV USING MED-WMEDAREA                                 
206300         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
206400         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
206500     END-IF                                                               
206600     IF NOT ALLT-OK AND NYCKEL-OK                                         
206700        PERFORM MFS-ROER-EJ-BILD                                          
206800     END-IF                                                               
206900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O24301 + 4                        
207000     PERFORM IMS-INSERT-MSG                                               
207100     .                                                                    
207200     EJECT                                                                
207300 S01-SKRIV-MID-TILL-4242 SECTION.                                         
207400                                                                          
207500     MOVE JA                   TO START-4242-SW                           
207600     ADD +1                    TO 4242-MID-IX                             
207700                                                                          
207800     IF OBKR-IDARTNR-TILLK > +0                                           
207900       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
208000      MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                             
208100     ELSE                                                                 
208200       MOVE OBKR-IDARTNR       TO WS-IDARTNR                              
208300       MOVE OBKR-REKSIFFR      TO WS-REKSIFFR                             
208400     END-IF                                                               
208500     MOVE WS-IDARTNR-REKSIFFR  TO 4242-MID-IDARTNR(4242-MID-IX)           
208600                                                                          
208700     IF OBKR-KVBEART-TILLK > +0                                           
208800       MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                                
208900       MOVE WS-ALFA-6          TO 4242-MID-KVBEART(4242-MID-IX)           
209000     ELSE                                                                 
209100       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
209200          COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                 
209300          MOVE WS-ALFA-6       TO 4242-MID-KVBEART(4242-MID-IX)           
209400       ELSE                                                               
209500          MOVE OBKR-KVBEART    TO WS-NUM-6                                
209600          MOVE WS-ALFA-6       TO 4242-MID-KVBEART(4242-MID-IX)           
209700       END-IF                                                             
209800     END-IF                                                               
209900                                                                          
210000     IF OBKR-TITPO > +0                                                   
210100        MOVE OBKR-TITPO        TO WS-NUM-6                                
210200        MOVE WS-ALFA-6         TO 4242-MID-TITPO(4242-MID-IX)             
210300     ELSE                                                                 
210400        MOVE ALL '+'           TO 4242-MID-TITPO(4242-MID-IX)             
210500     END-IF                                                               
210600     EJECT                                                                
210700     MOVE OBKR-FLRESTN         TO 4242-MID-FLRESTN(4242-MID-IX)           
210800     MOVE OBKR-FLSLATT         TO 4242-MID-FLSLATT(4242-MID-IX)           
210900     MOVE OBKR-KDKVBRYT        TO 4242-MID-KDKVBRYT(4242-MID-IX)          
211000     MOVE OBKR-BERADREF        TO 4242-MID-BERADREF(4242-MID-IX)          
211100     .                                                                    
211200     EJECT                                                                
211300 S02-GODKANN-RAD SECTION.                                                 
211400                                                                          
211500     MOVE OBKR-IDDC            TO WS-IDDC                                 
211600     MOVE JA                   TO OBKR-FLOBOK                             
211700     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
211800     IF OBKR-IDPGM(1:4) = '4206'                                          
211900       MOVE '4206'     TO X-IDTRANS                                       
212000     END-IF                                                               
212100                                                                          
212200     IF ((OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0)                        
212300            AND OBKR-KDORDBEK NOT = 92 AND 98) OR                         
212400               (OBKR-KDORDBEK = 92 AND OBKR-KVPREAVB > +0) OR             
212500               (OBKR-KDORDBEK = 98 AND OBKR-KVPREAVB > +0)                
212600                                                                          
212700        PERFORM S14-LAS-ARTIKELREG                                        
212800        PERFORM S12-BYGG-UPP-ORDERRAD                                     
212900                                                                          
213000        PERFORM S09-KONTROLLERA-ENHETSLAST                                
213100        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
213200     END-IF                                                               
213300                                                                          
213400     IF OBKR-KDORDBEK = 10                                                
213500                                                                          
213600       PERFORM S14-LAS-ARTIKELREG                                         
213700       PERFORM S02A-VALD-BIPACKNING                                       
213800     ELSE                                                                 
213900       IF (OBKR-KDORDBEK =  61) AND AKT-SIDA                              
214000                                                                          
214100         PERFORM S01-SKRIV-MID-TILL-4242                                  
214200         PERFORM S02B-SKRIV-ORDERBEKR-40                                  
214300       ELSE                                                               
214400         IF OBKR-KDORDBEK =  70 OR 71 OR 74                               
214500           IF OBKR-KDTPOTYP = +2                                          
214600                                                                          
214700             PERFORM S14-LAS-ARTIKELREG                                   
214800             PERFORM S02C-UPPDATERA-TPO2                                  
214900           ELSE                                                           
215000             IF OBKR-KDTPOTYP = +6                                        
215100                                                                          
215200                PERFORM S14-LAS-ARTIKELREG                                
215300                PERFORM S02D-UPPDATERA-TPO6                               
215400             END-IF                                                       
215500           END-IF                                                         
215600         ELSE                                                             
215700*DDGS      IF (OBKR-KDORDBEK = 21 OR 52 OR 53 OR 54 OR                    
215800           IF (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR                    
215900                               55 OR 57 OR 67 OR 92 OR 98)                
216000                          AND  OHUV-KDORDKL = +0                          
216100                                                                          
216200              PERFORM S11-SKRIV-VOR-RAD                                   
216300           END-IF                                                         
216400         END-IF                                                           
216500       END-IF                                                             
216600     END-IF                                                               
216700     .                                                                    
216800     EJECT                                                                
216900                                                                          
217000 S02A-VALD-BIPACKNING SECTION.                                            
217100                                                                          
217200     PERFORM S12-BYGG-UPP-ORDERRAD                                        
217300                                                                          
217400*** BIPACKNING RESTORDER ***                                              
217500     IF OBKR-TIRODAT > +0                                                 
217600        PERFORM S09-KONTROLLERA-ENHETSLAST                                
217700        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
217800        IF SPAR-ARB-KDROPACK = 'L'                                        
217900           PERFORM S02AB-UPPDATERA-WDE8                                   
218000        END-IF                                                            
218100     ELSE                                                                 
218200*** BIPACKNING TPO ***                                                    
218300       IF WS-IDDC NOT = W-IDDC-B6                                         
218400          MOVE WS-IDDC TO W-IDDC-B6                                       
218500          PERFORM IMS-GU-WDB601                                           
218600       END-IF                                                             
218700       IF DCS-CDC OR DCS-SDC                                              
218800         IF DCS-SDC                                                       
219000            PERFORM S02AA-KOLLA-DIREKTLEVERANS                            
219100         ELSE                                                             
219200            MOVE ZERO  TO DLEV-KDORDBEK-UT                                
219300         END-IF                                                           
219400                                                                          
219500         IF DLEV-KDORDBEK-UT = +0                                         
219600           PERFORM S02AC-KOMPLETTERA-RANSONERING                          
219700           PERFORM S02AE-PREL-AVBOKNING                                   
219800           PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                            
219900                                                                          
220000           IF SPAR-ARB-KDROPACK = 'L'                                     
220100              PERFORM S02AB-UPPDATERA-WDE8                                
220200           END-IF                                                         
220300         ELSE                                                             
220400           MOVE DLEV-KDORDBEK-UT TO OBKR-KDORDBEK                         
220500           MOVE '4243DLEV'       TO OBKR-IDPGM                            
220600           MOVE DLEV-IDLEVNR-UT  TO OBKR-IDLEVNR                          
220700           MOVE NEJ              TO OBKR-FLOBOK                           
220800                                                                          
220900           ADD +1                TO OBKR-IDSEKVNR                         
221000           PERFORM IMS-07-ISRT-ORQM-WDQ101                                
221100                                                                          
221200           IF DLEV-KDORDBEK-UT = 95                                       
221300              PERFORM S02AC-KOMPLETTERA-RANSONERING                       
221400              PERFORM S02AE-PREL-AVBOKNING                                
221500              PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                         
221600                                                                          
221700              IF SPAR-ARB-KDROPACK = 'L'                                  
221800                 PERFORM S02AB-UPPDATERA-WDE8                             
221900              END-IF                                                      
222000           ELSE                                                           
222100              MOVE OBKR-IDARTNR TO W-IDARTNR                              
222200              PERFORM IMS-21-GHU-ARTM-WDK901                              
222300              IF OBKR-KDORDKL = +0                                        
222400                 SUBTRACT OBKR-KVBEART-Q FROM                             
222500                          ART-KVOKS-VOR                                   
222600              ELSE                                                        
222700                IF OBKR-KDORDKL = +1                                      
222800                   SUBTRACT OBKR-KVBEART-Q FROM                           
222900                            ART-KVOKS-DAG                                 
223000                ELSE                                                      
223100                   SUBTRACT OBKR-KVBEART-Q FROM                           
223200                            ART-KVOKS-BULK                                
223300                END-IF                                                    
223400              END-IF                                                      
223500              PERFORM IMS-22-REPL-ARTM-WDK901                             
223600           END-IF                                                         
223700         END-IF                                                           
223800       END-IF                                                             
223900     END-IF                                                               
224000     .                                                                    
224100     EJECT                                                                
224200                                                                          
224300 S02AA-KOLLA-DIREKTLEVERANS SECTION.                                      
224400                                                                          
224500     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
224600     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
224700     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
224800     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
224900     MOVE ORAD-IDLEVNR         TO DLEV-IDLEVNR-IN                         
225000     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
225100     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
225200     MOVE ORAD-IDDC            TO DLEV-IDDC-IN                            
225300     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
225400     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
225500     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
225600     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
225700     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
225800     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
225900     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
226000     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
226100     MOVE ORAD-CLEARGROUP      TO DLEV-CLEARGROUP                         
226200     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
226300*    TO GET CLEARING DC LIST FROM WDB2                                    
226400     MOVE ORAD-IDDISTR         TO W-WDB2-IDDISTR                          
226500     MOVE ORAD-IDKUNDNR        TO W-WDB2-IDKUNDNR                         
226600     PERFORM IMS-GU-WDB201                                                
226700*                                                                         
226800     IF OHUV-KDORDKL > 1                                                  
226900                                                                          
226910        MOVE +1 TO WS-INDEX                                               
226920        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
226930           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
226940                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
226950           ADD +1 TO WS-INDEX                                             
226960        END-PERFORM                                                       
226970                                                                          
226980     ELSE                                                                 
226990       IF OHUV-KDORDKL = 1                                                
226991                                                                          
226992          MOVE +1 TO WS-INDEX                                             
226993          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
226994             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
226995                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
226996             ADD +1 TO WS-INDEX                                           
226997          END-PERFORM                                                     
226998                                                                          
226999       ELSE                                                               
227000         IF OHUV-KDORDKL = 0                                              
227001                                                                          
227002            MOVE +1 TO WS-INDEX                                           
227003            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
227004               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
227005                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
227006               ADD +1 TO WS-INDEX                                         
227007            END-PERFORM                                                   
227008                                                                          
227009         END-IF                                                           
227010       END-IF                                                             
227011     END-IF                                                               
227012                                                                          
227020     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
227100**   MOVE 1                    TO DLEV-KDCALL                             
227200**   THE ABOVE LINE IS COMMENTED AND ZERO IS MOVED TO KDCALL              
227300**   TO AVOID THE INSERT INTO WDR6 IN W411DLEV                            
227400     MOVE ZERO                 TO DLEV-KDCALL                             
227500                                                                          
227600     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
227700                                       DLEV-LEVG-PCB                      
227800                                       DLEV-LEVA-PCB                      
227900                                       DLEV-ARTS-PCB                      
228000                                       DLEV-WDB6-PCB                      
228100                                       TPO2-FILA-PCB                      
228200                                                                          
228300     IF DLEV-IDLEVNR-UT NOT = SPACE                                       
228400       IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                                 
228500         MOVE DLEV-IDDC-UT     TO ORAD-IDDC                               
228600       END-IF                                                             
228700     END-IF                                                               
228800                                                                          
228900     MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)           
229000     MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)           
229100     MOVE DLEV-KVDAGAR-DIFF-UT TO                                         
229200                               AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)           
229300     MOVE DLEV-TISKEPPN-DDC-UT TO                                         
229400                               AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)           
229500                                                                          
229600     MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                            
229700     MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                            
229800     MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                           
229900     IF DLEV-FLSDCLEV-UT = JA                                             
230000       MOVE DLEV-IDDC-UT       TO ORAD-IDDC                               
230100                                  WS-IDDC                                 
230200     ELSE                                                                 
230300       MOVE DLEV-KDOI-UT       TO ORAD-KDOI                               
230400       MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                         
230500     END-IF                                                               
230600     .                                                                    
230700     EJECT                                                                
230800 S02AB-UPPDATERA-WDE8 SECTION.                                            
230900                                                                          
231000     MOVE W-IDDISTR        TO W-E8-IDDISTR                                
231100     MOVE W-IDKUNDNR       TO W-E8-IDKUNDNR                               
231200     MOVE OBKR-IDKUNDRF-RO TO W-E8-IDKUNDRF                               
231300     PERFORM IMS-35-GHU-PROC-WDE801                                       
231400                                                                          
231500     IF SEGMENT-FINNS                                                     
231600     MOVE +1                   TO WS-INDEX                                
231700     PERFORM UNTIL WS-INDEX > 10                                          
231800        IF PHUV-IDKUNDRF-ING(WS-INDEX) = W-IDKUNDRF                       
231900           MOVE +10            TO WS-INDEX                                
232000        ELSE                                                              
232100           IF PHUV-IDKUNDRF-ING(WS-INDEX) = '0000000   '                  
232200              MOVE W-IDKUNDRF  TO PHUV-IDKUNDRF-ING(WS-INDEX)             
232300                                                                          
232400              PERFORM IMS-36-REPL-PROC-WDE801                             
232500              MOVE +10         TO WS-INDEX                                
232600           END-IF                                                         
232700        END-IF                                                            
232800        ADD  +1                TO WS-INDEX                                
232900     END-PERFORM                                                          
233000     END-IF                                                               
233100     .                                                                    
233200     EJECT                                                                
233300                                                                          
233400 S02AC-KOMPLETTERA-RANSONERING SECTION.                                   
233500                                                                          
233600     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
233700     MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                           
233800     MOVE OHUV-FLFORBI         TO RANS-FLFORBI                            
233900     MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                           
234000     MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                           
234100     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
234200     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
234300     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
234400     MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                            
234500     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
234600     MOVE +2                   TO RANS-KDORDBEH                           
234700     MOVE OHUV-KDORDKL         TO RANS-KDORDKL                            
234800     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
234900     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
235000     MOVE AREG-KDERS           TO RANS-KDERS                              
235100     MOVE AREG-KVLS            TO RANS-KVLS                               
235200     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
235300     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
235400     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
235500     MOVE AREG-KVRESS          TO RANS-KVRESS                             
235600     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
235700     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
235800     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
235900                                                                          
236000     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
236100     IF KDPRODSL-BIMA                                                     
236200       MOVE 1                  TO ORAD-RERF-RAD                           
236300                                  RANS-RERF-RAD-UT                        
236400       MOVE ZERO               TO RANS-SUTPO-PB-UT                        
236500                                  RANS-SUTPO-EJPB-UT                      
236600                                  RANS-RERF-ART-UT                        
236700     ELSE                                                                 
236800       CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                    
236900                           RANS-ARTM-PCB RANS-ARTS-PCB                    
237000                                                                          
237100       MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                             
237200     END-IF                                                               
237300     .                                                                    
237400     EJECT                                                                
237500                                                                          
237600                                                                          
237700 S02AE-PREL-AVBOKNING SECTION.                                            
237800                                                                          
237900     MOVE JA                   TO CDCA-FLFINLV-IN                         
238000     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
238100     MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                        
238200     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
238300     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
238400     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
238500     MOVE OBKR-FLSLATT         TO CDCA-FLSLATT-IN                         
238600     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
238700     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
238800     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
238900     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
239000     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
239100     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
239200     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
239300     MOVE +0                   TO CDCA-KDERS-IN                           
239400     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
239500     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
239600     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
239700     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
239800     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
239900     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
240000     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
240100     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
240200     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
240300     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
240400     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
240500     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
240600     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
240700     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
240800     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
240900     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
241000     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
241100     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
241200     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
241300     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
241400     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
241500     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
241600     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
241700     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
241800     MOVE +1                   TO CDCA-KDCALL                             
241900                                                                          
242000     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
242100                                       CDCA-INLB-PCB                      
242200                                       CDCA-WDB2-PCB                      
242300                                       CDCA-WDC1-PCB                      
242400     EJECT                                                                
242500     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
242600     MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                            
242700     MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                          
242800     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
242900     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
243000     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
243100     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
243200     .                                                                    
243300     EJECT                                                                
243400 S02AF-SKRIV-Q1-OCH-Q4-RADER SECTION.                                     
243500                                                                          
243600     MOVE AREG-ADLAGOMR TO ORAD-ADLAGOMR                                  
243700     MOVE AREG-ADGANG   TO ORAD-ADGANG                                    
243800     MOVE AREG-ADPLATS  TO ORAD-ADPLATS                                   
243900                                                                          
244000     IF ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0                           
244100       PERFORM S09-KONTROLLERA-ENHETSLAST                                 
244200       PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                                
244300     END-IF                                                               
244400                                                                          
244500     IF CDCA-KDORDBEK-UT >  0                                             
244600       PERFORM S02AFA-SKRIV-ORDERBEKR                                     
244700     END-IF                                                               
244800     .                                                                    
244900     EJECT                                                                
245000 S02AFA-SKRIV-ORDERBEKR SECTION.                                          
245100                                                                          
245200     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
245300     MOVE ORAD-IDDC            TO OBKR-IDDC                               
245400     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
245500     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
245600     MOVE ORAD-KVPREAVB        TO OBKR-KVPREAVB                           
245700     MOVE ORAD-KVPRERO         TO OBKR-KVPRERO                            
245800     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
245900                                                                          
246000     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
246100     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
246200     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
246300                                                                          
246400     IF CDCA-KDORDBEK-UT > +0                                             
246500*----(KOD 80, 92, 99)                                                     
246600        IF CDCA-KDORDBEK-UT =  80                                         
246700          MOVE CDCA-KVANNANT-UT TO OBKR-KVANNANT                          
246800        END-IF                                                            
246900                                                                          
247000        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
247100           MOVE ORAD-KVBEART-Q TO OBKR-KVPREAVB                           
247200        END-IF                                                            
247300        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
247400        MOVE '4243CDCA'        TO OBKR-IDPGM                              
247500        ADD +1                 TO OBKR-IDSEKVNR                           
247600        PERFORM  IMS-07-ISRT-ORQM-WDQ101                                  
247700     END-IF                                                               
247800     .                                                                    
247900     EJECT                                                                
248000                                                                          
248100 S02B-SKRIV-ORDERBEKR-40 SECTION.                                         
248200                                                                          
248300     MOVE OBKR-KVBEART-TILLK   TO SPAR-KVBEART-TILLK                      
248400     MOVE OBKR-IDARTNR-TILLK   TO SPAR-IDARTNR-TILLK                      
248500     MOVE OBKR-REKSIFFR-TILLK  TO SPAR-REKSIFFR-TILLK                     
248600     MOVE OBKR-DIERS-KVOT      TO SPAR-DIERS-KVOT                         
248700                                                                          
248800     IF OBKR-IDARTNR NOT = SPAR-IDARTNR-40  OR                            
248900         (OBKR-IDARTNR = SPAR-IDARTNR-40   AND                            
249000          OBKR-IDLOPNR NOT = SPAR-IDLOPNR-40)                             
249100                                                                          
249200        PERFORM S02BA-SKRIV-KOD40-ERSATT-ART                              
249300        COMPUTE SPAR-IDSEKVNR-40 = OBKR-IDSEKVNR + 1                      
249400     END-IF                                                               
249500                                                                          
249600     MOVE SPAR-KVBEART-TILLK   TO OBKR-KVBEART-TILLK                      
249700     MOVE SPAR-IDARTNR-TILLK   TO OBKR-IDARTNR-TILLK                      
249800     MOVE SPAR-REKSIFFR-TILLK  TO OBKR-REKSIFFR-TILLK                     
249900     MOVE SPAR-DIERS-KVOT      TO OBKR-DIERS-KVOT                         
250000                                                                          
250100     MOVE JA                   TO OBKR-FLOBOK                             
250200     MOVE 40                   TO OBKR-KDORDBEK                           
250300     MOVE IDPGM                TO OBKR-IDPGM                              
250400     MOVE SPAR-IDSEKVNR-40     TO OBKR-IDSEKVNR                           
250500                                                                          
250600     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
250700     ADD +1                 TO OBKR-IDSEKVNR                              
250800                               SPAR-IDSEKVNR-40                           
250900     MOVE OBKR-IDARTNR         TO SPAR-IDARTNR-40                         
251000     MOVE OBKR-IDLOPNR         TO SPAR-IDLOPNR-40                         
251100                                                                          
251200     PERFORM S02BB-UPPDAT-ERSATT-ART                                      
251300     .                                                                    
251400     EJECT                                                                
251500 S02BA-SKRIV-KOD40-ERSATT-ART SECTION.                                    
251600                                                                          
251700     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
251800                               W-Q1-IDARTNR-MAX1                          
251900     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
252000                               W-Q1-IDLOPNR-MAX1                          
252100     MOVE +3                TO W-Q1-IDSEKVNR-MIN1                         
252200     MOVE +999              TO W-Q1-IDSEKVNR-MAX1                         
252300                                                                          
252400     PERFORM IMS-33-GN-ORQM-WDQ101                                        
252500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
252600                                                                          
252700        PERFORM IMS-33-GN-ORQM-WDQ101                                     
252800     END-PERFORM                                                          
252900                                                                          
253000     MOVE +0                TO OBKR-IDARTNR-TILLK                         
253100                               OBKR-KVBEART-TILLK                         
253200                               OBKR-REKSIFFR-TILLK                        
253300                               OBKR-DIERS-KVOT                            
253400     MOVE JA                TO OBKR-FLOBOK                                
253500     MOVE 40                TO OBKR-KDORDBEK                              
253600     MOVE IDPGM             TO OBKR-IDPGM                                 
253700     ADD +1                 TO OBKR-IDSEKVNR                              
253800                                                                          
253900     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
254000     .                                                                    
254100     EJECT                                                                
254200 S02BB-UPPDAT-ERSATT-ART SECTION.                                         
254300                                                                          
254400*--VID VAL AV ERSÄTTNING SÄTTS FLOBTRAN TILL 'N' FÖR ATT FÖRHINDRA        
254500*--ATT ORDERBEKRÄFTELSETRANS SKICKAS FRÅN W4029300 TILL VR/VIPS           
254600     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
254700                               W-Q1-IDARTNR-MAX1                          
254800     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
254900                               W-Q1-IDLOPNR-MAX1                          
255000     MOVE +1                TO W-Q1-IDSEKVNR-MIN1                         
255100                               W-Q1-IDSEKVNR-MAX1                         
255200                                                                          
255300     PERFORM IMS-34-GHU-ORQM-WDQ101                                       
255400     MOVE NEJ               TO OBKR-FLOBTRAN                              
255500     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
255600     .                                                                    
255700     EJECT                                                                
255800 S02C-UPPDATERA-TPO2 SECTION.                                             
255900                                                                          
256000     IF WS-IDDC NOT = W-IDDC-B6                                           
256100        MOVE WS-IDDC TO W-IDDC-B6                                         
256200        PERFORM IMS-GU-WDB601                                             
256300     END-IF                                                               
256400                                                                          
256500     IF DCS-CDC OR DCS-SDC                                                
256700       PERFORM S12-BYGG-UPP-ORDERRAD                                      
256800                                                                          
256900       MOVE ORAD-IDDISTR       TO TPO2-IDDISTR                            
257000       MOVE ORAD-IDKUNDNR      TO TPO2-IDKUNDNR                           
257100       MOVE ORAD-IDKUNDRF      TO TPO2-IDKUNDRF                           
257200       MOVE ORAD-IDARTNR       TO TPO2-IDARTNR                            
257300       MOVE ORAD-BERADREF      TO TPO2-BERADREF                           
257400       MOVE AREG-IDANSK        TO TPO2-IDANSK                             
257500       MOVE OHUV-IDKONTO       TO TPO2-IDKONTO                            
257600       MOVE OHUV-IDKST         TO TPO2-IDKST                              
257700       MOVE OHUV-IDANALYS      TO TPO2-IDANALYS                           
257800       MOVE ORAD-KDDSP         TO TPO2-KDDSP                              
257900       MOVE OHUV-KDFAKTYP      TO TPO2-KDFAKTYP                           
258000       MOVE SPAR-ARB-KDFRAKT   TO TPO2-KDFRAKT                            
258100       IF OBKR-KDORDBEK = 71                                              
258200         MOVE OBKR-KDFRAKT     TO TPO2-KDFRAKT                            
258300       ELSE                                                               
258400         MOVE SPAR-ARB-KDFRAKT TO TPO2-KDFRAKT                            
258500       END-IF                                                             
258600       MOVE ORAD-KDKVBRYT      TO TPO2-KDKVBRYT                           
258700       EJECT                                                              
258800       MOVE ORAD-KDORDING      TO TPO2-KDORDING                           
258900       MOVE OHUV-KDORDKL       TO TPO2-KDORDKL                            
259000       MOVE ORAD-KDPRODSL      TO TPO2-KDPRODSL                           
259100       MOVE +2                 TO TPO2-KDTPOTYP                           
259200       MOVE ORAD-KDVRINFO      TO TPO2-KDVRINFO                           
259300       MOVE ORAD-KVBEART-Q     TO TPO2-KVBEART-Q                          
259400       MOVE ORAD-PRARTNTO      TO TPO2-PRARTNTO                           
259500       MOVE ORAD-DEAL-PR-LINE  TO TPO2-DEAL-PR-LINE                       
259600       MOVE ORAD-REKSIFFR      TO TPO2-REKSIFFR                           
259700       MOVE ORAD-TITPO         TO TPO2-TITPO                              
259800       MOVE ORAD-KDPRTYP       TO TPO2-KDPRTYP                            
259900       MOVE ORAD-BEVOLREF      TO TPO2-BEVOLREF                           
260000       MOVE ORAD-FLINVEST      TO TPO2-FLINVEST                           
260100       MOVE OHUV-FLORDSPE      TO TPO2-FLORDSPE                           
260200       MOVE OHUV-FLOVRLEV      TO TPO2-FLOVRLEV                           
260300       MOVE OHUV-FLFORBI       TO TPO2-FLFORBI                            
260400       MOVE ORAD-FLPRTILL      TO TPO2-FLPRTILL                           
260500       MOVE OHUV-BEKUNDRF      TO TPO2-BEKUNDRF                           
260600       MOVE ORAD-IDKAMPRF      TO TPO2-IDKAMPRF                           
260700       MOVE ORAD-IDLEVNR       TO TPO2-IDLEVNR                            
260800       MOVE ORAD-IDSYSTEM      TO TPO2-IDSYSTEM                           
260900       MOVE AREG-KDUART        TO TPO2-KDUART                             
261000       MOVE AREG-KVFRYSTI      TO TPO2-KVFRYSTI                           
261100       IF OBKR-KDORDBEK = 71                                              
261200          MOVE +3              TO TPO2-KDORDBEH                           
261300       ELSE                                                               
261400          MOVE +2              TO TPO2-KDORDBEH                           
261500       END-IF                                                             
261600       MOVE ORAD-FLTILLK       TO TPO2-FLTILLK                            
261700       MOVE AREG-TIDISPIN      TO TPO2-TIDISPIN                           
261800       MOVE OHUV-BEVARREF      TO TPO2-BEVARREF                           
261900       MOVE OBKR-KVQPACK       TO TPO2-KVQPACK-1                          
262000       MOVE ORAD-KVBEART       TO TPO2-KVBEART                            
262100                                                                          
262200       MOVE OHUV-KDORDTYP-LDC TO TPO2-KDORDTYP-LDC                        
262300       MOVE OHUV-TIREPDAT     TO TPO2-TIREPDAT                            
262400       MOVE ORAD-IDKUNDRF-WIP TO TPO2-IDKUNDRF-WIP                        
262500                                                                          
262600       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
262700                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
262800                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
262900                           TIME-4437-PCB                                  
263000     END-IF                                                               
263100     .                                                                    
263200     EJECT                                                                
263300                                                                          
263400 S02D-UPPDATERA-TPO6 SECTION.                                             
263500                                                                          
263600     IF WS-IDDC NOT = W-IDDC-B6                                           
263700        MOVE WS-IDDC TO W-IDDC-B6                                         
263800        PERFORM IMS-GU-WDB601                                             
263900     END-IF                                                               
264000                                                                          
264100     IF DCS-CDC OR DCS-SDC                                                
264300       PERFORM S12-BYGG-UPP-ORDERRAD                                      
264400                                                                          
264500       MOVE ORAD-IDDISTR       TO TPO6-IDDISTR                            
264600       MOVE ORAD-IDKUNDNR      TO TPO6-IDKUNDNR                           
264700       MOVE ORAD-IDKUNDRF      TO TPO6-IDKUNDRF                           
264800       MOVE ORAD-IDARTNR       TO TPO6-IDARTNR                            
264900       MOVE OHUV-IDDC-PRIM     TO TPO6-IDDC-DAY                           
265000       MOVE AREG-FLREFILL      TO TPO6-FLREFILL                           
265100       MOVE AREG-KDUART        TO TPO6-KDUART                             
265200       MOVE AREG-REDIRLEV      TO TPO6-REDIRLEV                           
265300       MOVE ORAD-BERADREF      TO TPO6-BERADREF                           
265400       MOVE AREG-IDANSK        TO TPO6-IDANSK                             
265500       MOVE OHUV-IDKONTO       TO TPO6-IDKONTO                            
265600       MOVE OHUV-IDKST         TO TPO6-IDKST                              
265700       MOVE OHUV-IDANALYS      TO TPO6-IDANALYS                           
265800       MOVE ORAD-KDDSP         TO TPO6-KDDSP                              
265900       MOVE OHUV-KDFAKTYP      TO TPO6-KDFAKTYP                           
266000       MOVE SPAR-ARB-KDFRAKT   TO TPO6-KDFRAKT                            
266100       MOVE ORAD-KDKVBRYT      TO TPO6-KDKVBRYT                           
266200       EJECT                                                              
266300       MOVE ORAD-KDORDING      TO TPO6-KDORDING                           
266400       MOVE OHUV-KDORDKL       TO TPO6-KDORDKL                            
266500       MOVE ORAD-KDPRODSL      TO TPO6-KDPRODSL                           
266600       MOVE +6                 TO TPO6-KDTPOTYP                           
266700       MOVE ORAD-KDVRINFO      TO TPO6-KDVRINFO                           
266800       MOVE ORAD-KVBEART-Q     TO TPO6-KVBEART-Q                          
266900       MOVE ORAD-PRARTNTO      TO TPO6-PRARTNTO                           
267000       MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                       
267100       MOVE AREG-REKSIFFR      TO TPO6-REKSIFFR                           
267200       MOVE ORAD-KDPRTYP       TO TPO6-KDPRTYP                            
267300       MOVE ORAD-BEVOLREF      TO TPO6-BEVOLREF                           
267400       MOVE ORAD-FLINVEST      TO TPO6-FLINVEST                           
267500       MOVE ORAD-FLPRTILL      TO TPO6-FLPRTILL                           
267600       MOVE OHUV-BEKUNDRF      TO TPO6-BEKUNDRF                           
267700       MOVE ORAD-IDKAMPRF      TO TPO6-IDKAMPRF                           
267800       MOVE ORAD-IDLEVNR       TO TPO6-IDLEVNR                            
267900       MOVE ORAD-IDSYSTEM      TO TPO6-IDSYSTEM                           
268000       MOVE OHUV-FLFORBI       TO TPO6-FLFORBI                            
268100                                                                          
268200       MOVE OHUV-KDORDTYP-LDC TO TPO6-KDORDTYP-LDC                        
268300       MOVE OHUV-TIREPDAT     TO TPO6-TIREPDAT                            
268400       MOVE ORAD-IDKUNDRF-WIP TO TPO6-IDKUNDRF-WIP                        
268500                                                                          
268600       MOVE ORAD-KDOI         TO TPO6-KDOI                                
268700       MOVE ORAD-CLEARGROUP   TO TPO6-CLEARGROUP                          
268800                                                                          
268900       CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB           
269000           TPO6-XXBU-PCB TPO6-XXBV-PCB TPO6-XXBX-PCB                      
269100             TPO6-ARTS-PCB TIME-4437-PCB                                  
269200     END-IF                                                               
269300     .                                                                    
269400     EJECT                                                                
269500 S03-STARTA-RADBEHANDLINGEN SECTION.                                      
269600                                                                          
269700     MOVE MFS-KDMFSFOR         TO 4242-SPRAK                              
269800                                                                          
269900     MOVE WS-IDDISTR           TO 4242-MID-IDDISTR                        
270000     MOVE WS-IDKUNDNR          TO 4242-MID-IDKUNDNR                       
270100     MOVE WS-IDORDNR           TO 4242-MID-IDORDNR5                       
270200                                                                          
270300     MOVE OBKR-BEVOLREF        TO 4242-MID-BEVOLREF                       
270400                                                                          
270500     PERFORM IMS-INSERT-4242-MSG                                          
270600     MOVE JA                   TO HOPP                                    
270700     .                                                                    
270800     EJECT                                                                
270900 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
271000                                                                          
271100     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
271200     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
271300     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
271400     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
271500     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
271600     MOVE ORAD-IDDC            TO LAST-IDDC                               
271700     MOVE SPAR-ARB-KDFDKRAV    TO LAST-KDFDKRAV                           
271800     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
271900     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
272000     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
272100                                                                          
272200     CALL W411LAST USING LAST-W411LAST                                    
272300     .                                                                    
272400     EJECT                                                                
272500 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
272600                                                                          
272700     IF LAST-ADLAGOMR-UT = +0 AND                                         
272800        LAST-KVANTAL-UT  = +0 AND                                         
272900        LAST-KVBEART-UT  = +0                                             
273000*------------------------------------------------------------*            
273100*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
273200*------------------------------------------------------------*            
273300        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
273400        PERFORM S10B-REDIGERA-WOPS-AREA                                   
273500        PERFORM IMS-17-ISRT-ORQF-WDQ401                                   
273600        PERFORM UNTIL SEGMENT-FINNS                                       
273700           ADD +1                    TO ORAD-IDLOPNR                      
273800           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
273900        END-PERFORM                                                       
274000     ELSE                                                                 
274100                                                                          
274200        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
274300*------------------------------------------------------------*            
274400*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
274500*------------------------------------------------------------*            
274600                                                                          
274700           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
274800           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
274900                                    ORAD-KVPRERO                          
275000           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
275100           PERFORM S10C-BERAEKNA-KVSLATT                                  
275200           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
275300           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
275400           MOVE ORAD-ADGANG          TO WS-ADGANG                         
275500           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
275600           PERFORM S10B-REDIGERA-WOPS-AREA                                
275700           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
275800           PERFORM UNTIL SEGMENT-FINNS                                    
275900              ADD +1                 TO ORAD-IDLOPNR                      
276000              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
276100           END-PERFORM                                                    
276200*------------------------------------------------------------*            
276300*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
276400*------------------------------------------------------------*            
276500           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
276600           MOVE WS-ADGANG            TO ORAD-ADGANG                       
276700           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
276800                                                                          
276900           MOVE +0                   TO ORAD-KVBEART                      
277000           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
277100                                        ORAD-KVPREAVB                     
277200           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
277300                                                                          
277400           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
277500             CONTINUE                                                     
277600           ELSE                                                           
277700             IF LAST-ADGANG-UT > ZERO                                     
277800               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
277900             END-IF                                                       
278000           END-IF                                                         
278100           MOVE +0                   TO ORAD-KVPRERO                      
278200           MOVE 1.0000               TO ORAD-RERF-RAD                     
278300           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
278400           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
278500           PERFORM S10B-REDIGERA-WOPS-AREA                                
278600           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
278700           PERFORM UNTIL SEGMENT-FINNS                                    
278800              ADD +1                 TO ORAD-IDLOPNR                      
278900              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
279000           END-PERFORM                                                    
279100        ELSE                                                              
279200*------------------------------------------------------------*            
279300*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
279400*------------------------------------------------------------*            
279500           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
279600           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
279700             CONTINUE                                                     
279800           ELSE                                                           
279900             IF LAST-ADGANG-UT > ZERO                                     
280000               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
280100             END-IF                                                       
280200           END-IF                                                         
280300           MOVE 1.0000            TO ORAD-RERF-RAD                        
280400           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
280500           PERFORM S10B-REDIGERA-WOPS-AREA                                
280600           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
280700           PERFORM UNTIL SEGMENT-FINNS                                    
280800              ADD +1              TO ORAD-IDLOPNR                         
280900              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
281000           END-PERFORM                                                    
281100        END-IF                                                            
281200     END-IF                                                               
281300     .                                                                    
281400     EJECT                                                                
281500 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
281600                                                                          
281700     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
281800     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
281900     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
282000     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
282100     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
282200     MOVE 1                    TO ADRS-KDCALL-IN                          
282300     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
282400     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
282500     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
282600     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
282700                                                                          
282800     CALL W413ADRS USING ADRS-W413ADRS                                    
282900                                                                          
283000*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
283100     IF ORAD-IDKAMPRF > 0                                                 
283200       MOVE 8                  TO ORAD-ADLAGOMR                           
283300     ELSE                                                                 
283400       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
283500     END-IF                                                               
283600                                                                          
283700*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
283800     IF WS-IDDC NOT = W-IDDC-B6                                           
283900        MOVE WS-IDDC TO W-IDDC-B6                                         
284000        PERFORM IMS-GU-WDB601                                             
284100     END-IF                                                               
284200     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
284300     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
284400                                                                          
284500     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
284600     .                                                                    
284700     EJECT                                                                
284800 S10B-REDIGERA-WOPS-AREA SECTION.                                         
284900                                                                          
285000     MOVE +1                   TO AVSR-KDCALL                             
285100     IF X-IDTRANS = '4206'                                                
285200       MOVE +8                 TO AVSR-KDCALL                             
285300     END-IF                                                               
285400     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
285500     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
285600     MOVE SPAR-ARB-KDFRAKT     TO AVSR-KDFRAKT                            
285700     MOVE SPAR-ARB-KDROPACK    TO AVSR-KDROPACK                           
285800     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
285900     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
286000                                                                          
286100     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
286200     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
286300     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
286400     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
286500     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
286600     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
286700     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
286800     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
286900     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
287000     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
287100     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
287200     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
287300     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
287400                                                                          
287500     IF WS-IDDC NOT = W-IDDC-B6                                           
287600        MOVE WS-IDDC TO W-IDDC-B6                                         
287700        PERFORM IMS-GU-WDB601                                             
287800     END-IF                                                               
287900     IF DCS-DDC                                                           
288000       PERFORM S02AA-KOLLA-DIREKTLEVERANS                                 
288100     END-IF                                                               
288200                                                                          
288300     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB AVSR-ORQI-PCB         
288400                         AVSR-GMTB-PCB AVSR-GMTC-PCB                      
288500                         AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB        
288600     .                                                                    
288700     EJECT                                                                
288800 S10C-BERAEKNA-KVSLATT SECTION.                                           
288900                                                                          
289000     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
289100                                                                          
289200        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
289300                                                                          
289400        COMPUTE ORAD-KVSLATT ROUNDED =                                    
289500               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
289600     END-IF                                                               
289700     .                                                                    
289800     EJECT                                                                
289900 S11-SKRIV-VOR-RAD SECTION.                                               
290000                                                                          
290100     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
290200     MOVE OBKR-IDDC            TO WS-IDDC                                 
290300                                                                          
290400     IF OBKR-IDARTNR-TILLK > +0                                           
290500       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                               
290600                                  4542-IDARTNR                            
290700     ELSE                                                                 
290800       MOVE OBKR-IDARTNR       TO W-IDARTNR                               
290900                                  4542-IDARTNR                            
291000     END-IF                                                               
291100     MOVE OBKR-IDDC            TO W-IDDC                                  
291200                                  4542-IDDC                               
291300                                                                          
291400     PERFORM IMS-GU-WDK722                                                
291500     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
291600        MOVE XLAG-IDANSK       TO WS-IDANSK                               
291700     ELSE                                                                 
291800        PERFORM IMS-10-GU-WDK611                                          
291900        MOVE CLAG-IDANSK       TO WS-IDANSK                               
292000     END-IF                                                               
292100     MOVE WS-IDANSK            TO 4542-IDANSK                             
292200     MOVE +1                   TO 4542-IDLOPNR                            
292300     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
292400     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
292500     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
292600     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
292700     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
292800     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
292900     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
293000     MOVE ZERO                 TO 4542-KDVORATG                           
293100     IF 4542-KDORDBEK = 92 OR 98                                          
293200       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
293300       MOVE 4542-KVBEART       TO 4542-KVBEART-Q                          
293400       MOVE OBKR-KVPREAVB      TO 4542-KVPREAVB                           
293500     ELSE                                                                 
293600       MOVE OBKR-KVBEART       TO 4542-KVBEART-Q                          
293700       MOVE +0                 TO 4542-KVPREAVB                           
293800     END-IF                                                               
293900     EJECT                                                                
294000     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
294100     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
294200     MOVE SPACE                TO 4542-TEVORMRK                           
294300     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
294400     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
294500     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
294600     MOVE +0                   TO 4542-TIUPPDAT                           
294700     MOVE +0                   TO 4542-TIUPPTID                           
294800     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
294900                                                                          
295000*---------------------------------------UPPLÄGG TILL NY VORKÖ             
295100*                                       SKER I W40293                     
295200     IF WS-IDDC NOT = W-IDDC-B6                                           
295300        MOVE WS-IDDC TO W-IDDC-B6                                         
295400        PERFORM IMS-GU-WDB601                                             
295500     END-IF                                                               
295700     IF DCS-NDC                                                           
295800         PERFORM IMS-19-ISRT-4541-WDR411                                  
295900         PERFORM UNTIL SEGMENT-FINNS                                      
296000                                                                          
296100            ADD +1             TO 4542-IDLOPNR                            
296200            PERFORM IMS-19-ISRT-4541-WDR411                               
296300         END-PERFORM                                                      
296400                                                                          
296500         PERFORM IMS-11-GHU-WDK711                                        
296600         IF  DCS-NDC-CN                                                   
296700         OR (DCS-NDC-NA AND DCS-USA)                                      
296800           IF SLAG-IDDC-REF = SPACE                                       
296900             PERFORM S11A-STARTA-W2T191X                                  
297000           END-IF                                                         
297100         END-IF                                                           
297200     END-IF                                                               
297300                                                                          
297400     IF (4542-KDORDBEK NOT = 92 AND 98) AND                               
297500         4542-IDLEVNR = SPACE                                             
297600       IF DCS-CDC                                                         
297700         MOVE 4542-IDARTNR     TO W-IDARTNR                               
297800         PERFORM IMS-21-GHU-ARTM-WDK901                                   
297900         COMPUTE ART-KVOKS-VOR =                                          
298000                 ART-KVOKS-VOR + (4542-KVBEART-Q - 4542-KVPREAVB)         
298100         PERFORM IMS-22-REPL-ARTM-WDK901                                  
298200       END-IF                                                             
298300     END-IF                                                               
298400     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
298500     .                                                                    
298600     EJECT                                                                
298700                                                                          
298800 S11A-STARTA-W2T191X   SECTION.                                           
298900                                                                          
299000     MOVE +1                    TO 2191-MID-KDCLAGER                      
299100     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
299200     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
299300                                   2191-MID-TISENBEK-KL                   
299400     MOVE SPACE                 TO 2191-MID-IDKR                          
299500     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
299600     MOVE '500'                 TO 2191-MID-KDLARM                        
299700     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
299800     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
299900     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
300000     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
300100     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
300200     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
300300     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
300400     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
300500                                                                          
300600     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
300700     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
300800     MOVE '4243'                TO MSG-IDTRANS-1                          
300900     MOVE '1'                   TO MSG-KDMFSFOR-1                         
301000     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
301100                                                                          
301200     PERFORM IMS-PURG-MSG-2191                                            
301300                                                                          
301400     MOVE SPACE                 TO 2191-MID-W2I19101                      
301500     .                                                                    
301600     EJECT                                                                
301700                                                                          
301800 S12-BYGG-UPP-ORDERRAD SECTION.                                           
301900                                                                          
302000     MOVE OBKR-IDORDER         TO ORAD-IDORDER                            
302100     MOVE OBKR-IDDC            TO ORAD-IDDC                               
302200                                  WS-IDDC                                 
302300     IF WS-IDDC NOT = W-IDDC-B6                                           
302400        MOVE WS-IDDC TO W-IDDC-B6                                         
302500        PERFORM IMS-GU-WDB601                                             
302600     END-IF                                                               
302700                                                                          
302800     IF DCS-SDC OR DCS-NDC                                                
302900       MOVE AREG-IDARTNR       TO W-IDARTNR                               
303000       MOVE ORAD-IDDC          TO W-IDDC                                  
303100       PERFORM IMS-11-GHU-WDK711                                          
303200       MOVE SLAG-ADLAGOMR      TO ORAD-ADLAGOMR                           
303300       MOVE SLAG-ADGANG        TO ORAD-ADGANG                             
303400       MOVE SLAG-ADPLATS       TO ORAD-ADPLATS                            
303500       MOVE DCS-IDLANDX2       TO W-IDLAND                                
303600       IF DCS-NDC                                                         
303700         PERFORM IMS-GU-WDK712                                            
303800         IF SEGMENT-FINNS                                                 
303900            IF LART-KDARTURS > SPACE                                      
304000               MOVE LART-KDARTURS TO ORAD-KDARTURS                        
304100            ELSE                                                          
304200               MOVE AREG-KDARTURS TO ORAD-KDARTURS                        
304300            END-IF                                                        
304400            IF LART-VKART > ZERO AND                                      
304500               LART-VKART NOT = AREG-VKART                                
304600               MOVE LART-VKART TO ORAD-VKART                              
304700                                  ORAD-VKART-NTO                          
304800            ELSE                                                          
304900               MOVE AREG-VKART     TO ORAD-VKART                          
305000               MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                      
305100            END-IF                                                        
305200            IF LART-VLARTNTO > 0                                          
305300               MOVE LART-VLARTNTO TO ORAD-VLARTNTO                        
305400            ELSE                                                          
305500               MOVE AREG-VLARTNTO TO ORAD-VLARTNTO                        
305600            END-IF                                                        
305700         ELSE                                                             
305800            MOVE AREG-KDARTURS  TO ORAD-KDARTURS                          
305900            MOVE AREG-VKART     TO ORAD-VKART                             
306000            MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                         
306100            MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                          
306200         END-IF                                                           
306300       ELSE                                                               
306400         MOVE AREG-KDARTURS  TO ORAD-KDARTURS                             
306500         MOVE AREG-VKART     TO ORAD-VKART                                
306600         MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                            
306700         MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                             
306800       END-IF                                                             
306900     ELSE                                                                 
307000       MOVE AREG-ADLAGOMR      TO ORAD-ADLAGOMR                           
307100       MOVE AREG-ADGANG        TO ORAD-ADGANG                             
307200       MOVE AREG-ADPLATS       TO ORAD-ADPLATS                            
307300       MOVE AREG-KDARTURS      TO ORAD-KDARTURS                           
307400       MOVE AREG-VKART         TO ORAD-VKART                              
307500       MOVE AREG-VKART-NTO     TO ORAD-VKART-NTO                          
307600       MOVE AREG-VLARTNTO      TO ORAD-VLARTNTO                           
307700     END-IF                                                               
307800     MOVE AREG-IDARTNR         TO ORAD-IDARTNR                            
307900     MOVE +1                   TO ORAD-IDLOPNR                            
308000     MOVE OBKR-BERADREF        TO ORAD-BERADREF                           
308100     MOVE OBKR-BEVOLREF        TO ORAD-BEVOLREF                           
308200     MOVE OBKR-FLAKPLOC        TO ORAD-FLAKPLOC                           
308300     MOVE OBKR-FLINVEST        TO ORAD-FLINVEST                           
308400     MOVE OBKR-FLOBTRAN        TO ORAD-FLOBTRAN                           
308500     MOVE OBKR-FLPRTILL        TO ORAD-FLPRTILL                           
308600     MOVE OBKR-FLRESTN         TO ORAD-FLRESTN                            
308700     MOVE OBKR-FLTILLK         TO ORAD-FLTILLK                            
308800     MOVE 'N'                  TO ORAD-FLSDCLEV                           
308900     MOVE OBKR-IDDC-RO         TO ORAD-IDDC-RO                            
309000     MOVE OBKR-IDDISTR         TO ORAD-IDDISTR                            
309100     MOVE OBKR-IDKUNDNR        TO ORAD-IDKUNDNR                           
309200     MOVE OBKR-IDKUNDRF        TO ORAD-IDKUNDRF                           
309300     MOVE OBKR-IDKAMPRF        TO ORAD-IDKAMPRF                           
309400     MOVE OBKR-IDLEVNR         TO ORAD-IDLEVNR                            
309500     EJECT                                                                
309600     MOVE OBKR-IDLOPNR-RO      TO ORAD-IDLOPNR-RO                         
309700     MOVE OBKR-IDKUNDRF-RO     TO ORAD-IDKUNDRF-RO                        
309800     MOVE +0                   TO ORAD-IDSPECEMB                          
309900     MOVE OBKR-IDSYSTEM        TO ORAD-IDSYSTEM                           
310000     MOVE OBKR-KDDSP           TO ORAD-KDDSP                              
310100     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
310200     MOVE OBKR-KDKVBRYT        TO ORAD-KDKVBRYT                           
310300     MOVE OBKR-KDOI            TO ORAD-KDOI                               
310400     MOVE OBKR-CLEARGROUP      TO ORAD-CLEARGROUP                         
310500     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
310600     MOVE JA                   TO ORAD-FLORDING                           
310700     MOVE OBKR-KDORDKL         TO ORAD-KDORDKL                            
310800     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
310900     MOVE OBKR-KDPRTYP         TO ORAD-KDPRTYP                            
311000     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
311100     MOVE OBKR-KDTPOTYP        TO ORAD-KDTPOTYP                           
311200     MOVE OBKR-KDVRINFO        TO ORAD-KDVRINFO                           
311300     IF OBKR-KDORDBEK = 10                                                
311400        MOVE OBKR-KVBEART      TO ORAD-KVPREAVB                           
311500     ELSE                                                                 
311600        MOVE OBKR-KVPREAVB     TO ORAD-KVPREAVB                           
311700     END-IF                                                               
311800     MOVE OBKR-KVPRERO         TO ORAD-KVPRERO                            
311900     MOVE +0                   TO ORAD-KVOKS-PREL                         
312000     MOVE OBKR-KVBEART         TO ORAD-KVBEART                            
312100     IF OBKR-KDORDBEK = 92 OR 98                                          
312200       COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO                 
312300       MOVE OBKR-KVPREAVB      TO ORAD-KVBEART-Q                          
312400       MOVE +0                 TO ORAD-KVPRERO                            
312500     ELSE                                                                 
312600       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
312700          COMPUTE ORAD-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO           
312800       ELSE                                                               
312900          MOVE OBKR-KVBEART-Q  TO ORAD-KVBEART-Q                          
313000       END-IF                                                             
313100     END-IF                                                               
313200     EJECT                                                                
313300     MOVE OBKR-KVSLATT         TO ORAD-KVSLATT                            
313400     MOVE OBKR-PRARTNTO        TO ORAD-PRARTNTO                           
313500     MOVE OBKR-DEAL-PR-LINE    TO ORAD-DEAL-PR-LINE                       
313600***VID T.EX ERSATTA BIPACKNINGSRADER, TPO6'OR ETC.                        
313700*** DE HAR INGEN PRISFRÅGA ÄNNU.                                          
313800     IF DIST79-DEALER-PRICE AND                                           
313900        (OBKR-PRARTNTO-LOC = +0 AND OBKR-PRARTNTO-LOCPREL = +0)           
314000        PERFORM S25-ADD-PRICE-Q-LINE                                      
314100     END-IF                                                               
314200     MOVE OBKR-PRBPRIS         TO ORAD-PRBPRIS                            
314300     MOVE AREG-REKSIFFR        TO ORAD-REKSIFFR                           
314400     MOVE OBKR-RERF-RAD        TO ORAD-RERF-RAD                           
314500     MOVE OBKR-TIPRIS          TO ORAD-TIPRIS                             
314600     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
314700     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
314800     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
314900     IF OBKR-KDORDBEK = 10                                                
315000        MOVE OBKR-TIRODAT      TO ORAD-TIRODAT                            
315100     ELSE                                                                 
315200        MOVE +0                TO ORAD-TIRODAT                            
315300     END-IF                                                               
315400     MOVE OBKR-TITPO           TO ORAD-TITPO                              
315500     MOVE SPACE                TO ORAD-IDBIL                              
315600                                  ORAD-IDKLIENT                           
315700                                  ORAD-IDARBREF                           
315800                                  ORAD-IDVIN                              
315900                                                                          
316000     MOVE OBKR-IDKUNDRF-WIP    TO ORAD-IDKUNDRF-WIP                       
316100     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
316200     MOVE OBKR-KDVALISO        TO ORAD-KDVALISO                           
316300     .                                                                    
316400     EJECT                                                                
316500                                                                          
316600 S13-HITTA-FORSTA-I-GRUPPEN SECTION.                                      
316700                                                                          
316800     MOVE WS-AKT-IDARTNR       TO WS-IDARTNR-SPAR                         
316900     MOVE WS-AKT-IDDC          TO WS-IDDC-SPAR                            
317000     MOVE WS-AKT-IDLOPNR       TO WS-IDLOPNR-SPAR                         
317100     MOVE WS-AKT-IDARTNR-URS   TO WS-IDARTNR-URS-SPAR                     
317200                                                                          
317300     SUBTRACT 1 FROM WS-INDEX-MID                                         
317400     IF WS-INDEX-MID NOT = +0                                             
317500        MOVE MID-RAD(WS-INDEX-MID)                                        
317600                               TO WS-AKTUELL-MID-RAD                      
317700        PERFORM UNTIL WS-INDEX-MID = +0 OR                                
317800                      WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR             
317900                      WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR             
318000                      WS-AKT-IDARTNR-URS NOT =                            
318100                      WS-IDARTNR-URS-SPAR                                 
318200           SUBTRACT 1        FROM WS-INDEX-MID                            
318300           IF WS-INDEX-MID NOT = +0                                       
318400              MOVE MID-RAD(WS-INDEX-MID)                                  
318500                               TO WS-AKTUELL-MID-RAD                      
318600           END-IF                                                         
318700        END-PERFORM                                                       
318800     END-IF                                                               
318900     .                                                                    
319000     EJECT                                                                
319100                                                                          
319200 S14-LAS-ARTIKELREG SECTION.                                              
319300                                                                          
319400     IF OBKR-IDARTNR-TILLK > +0                                           
319500       MOVE OBKR-IDARTNR-TILLK TO AREG-IDARTNR                            
319600     ELSE                                                                 
319700       MOVE OBKR-IDARTNR       TO AREG-IDARTNR                            
319800     END-IF                                                               
319900                                                                          
320000     CALL W411AREG USING AREG-W411AREG                                    
320100                         AREG-WDK6-PCB                                    
320200                         AREG-WDK7-PCB                                    
320300     .                                                                    
320400     EJECT                                                                
320500                                                                          
320600 S22-HAMTA-BENAMNING SECTION.                                             
320700                                                                          
320800     IF OBKR-IDARTNR-TILLK > +0                                           
320900        MOVE OBKR-IDARTNR-TILLK                                           
321000                               TO W-IDARTNR                               
321100     ELSE                                                                 
321200        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
321300     END-IF                                                               
321400                                                                          
321500     MOVE OHUV-IDSKYLT         TO W-IDSKYLT                               
321600                                                                          
321700     PERFORM IMS-23-GU-BENA-WDD311                                        
321800     IF SEGMENT-FINNS                                                     
321900        MOVE TEXT-BEART        TO MOD-BEART(WS-INDEX-MOD)                 
322000     ELSE                                                                 
322100        MOVE SPACE             TO MOD-BEART(WS-INDEX-MOD)                 
322200     END-IF                                                               
322300     .                                                                    
322400     EJECT                                                                
322500 S23-DELETE-PRICE-Q-LINE SECTION.                                         
322600                                                                          
322700     IF DIST79-DEALER-PRICE                                               
322800       IF OBKR-IDPRQUES > ZERO                                            
322900         INITIALIZE PRQU-W335PRQU                                         
323000         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
323100         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
323200         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
323300         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
323400         MOVE 4                  TO PRQU-KDCALL                           
323500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
323600                                            PRQU-WDC7-PCB                 
323700                                            PRQU-SJKO-WDK6-PCB            
323800       END-IF                                                             
323900     END-IF                                                               
324000     .                                                                    
324100     EJECT                                                                
324200                                                                          
324300 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
324400                                                                          
324500     IF DIST79-DEALER-PRICE                                               
324600       IF OBKR-IDPRQUES > ZERO                                            
324700         INITIALIZE PRQU-W335PRQU                                         
324800         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
324900         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
325000         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
325100         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
325200         MOVE OBKR-KVBEART-Q     TO PRQU-KVBEART-Q                        
325300         MOVE 5                  TO PRQU-KDCALL                           
325400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
325500                                            PRQU-WDC7-PCB                 
325600                                            PRQU-SJKO-WDK6-PCB            
325700       END-IF                                                             
325800     END-IF                                                               
325900     .                                                                    
326000     EJECT                                                                
326100                                                                          
326200                                                                          
326300 S25-ADD-PRICE-Q-LINE SECTION.                                            
326400                                                                          
326500     IF DIST79-DEALER-PRICE                                               
326600       IF OBKR-PRARTNTO-LOC = +0    AND                                   
326700          OBKR-PRARTNTO-LOCPREL = +0                                      
326800         IF OBKR-IDPRQUES       = +0                                      
326900********* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                 
327000           MOVE +0                    TO PRNO-IDPRQUES-IN                 
327100           MOVE +1                    TO PRNO-KDCALL                      
327200                                                                          
327300           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
327400                                                                          
327500**********UPPDATERAR WDC7 MED EN PRISFRÅGA                                
327600           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
327700           MOVE +1                    TO PRQU-KDCALL                      
327800                                                                          
327900           MOVE W-IDDISTR             TO PRQU-IDDISTR                     
328000           MOVE W-IDKUNDNR            TO PRQU-IDKUNDNR                    
328100           MOVE W-IDKUNDRF            TO PRQU-IDKUNDRF                    
328200           MOVE OBKR-IDORDER          TO PRQU-IDORDER                     
328300           MOVE OBKR-KDORDKL          TO PRQU-KDORDKL                     
328400           MOVE 'N'                   TO PRQU-KDPRSTA                     
328500           MOVE OBKR-IDARTNR          TO PRQU-IDARTNR                     
328600           MOVE OBKR-KVBEART-Q        TO PRQU-KVBEART-Q                   
328700           MOVE OBKR-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                
328800           MOVE +0                    TO PRQU-PRARTNTO-LOCPREL            
328900           MOVE OBKR-IDSYSTEM         TO PRQU-IDSYSTEM                    
329000                                                                          
329100           MOVE OBKR-KDVALISO         TO PRQU-KDVALISO                    
329200                                                                          
329300                                                                          
329400           CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                
329500                                           PRQU-WDC7-PCB                  
329600                                           PRQU-SJKO-WDK6-PCB             
329700                                                                          
329800           MOVE PRQU-IDPRQUES           TO  ORAD-IDPRQUES                 
329900           MOVE PRQU-FLPRTILL           TO  ORAD-FLPRTILL                 
330000                                                                          
330100           IF OBKR-PRARTNTO-LOC = +0                                      
330200             MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL         
330300           ELSE                                                           
330400             MOVE OBKR-PRARTNTO-LOC     TO  ORAD-PRARTNTO-LOC             
330500           END-IF                                                         
330600                                                                          
330700*** UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                    
330800                                                                          
330900           MOVE PRQU-IDPRQUES           TO PRNO-IDPRQUES-IN               
331000           MOVE +3                      TO PRNO-KDCALL                    
331100                                                                          
331200           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
331300***SKICKA PRISFRÅGA                                                       
331400           PERFORM S25C-SKICKA-PRISFRAGA                                  
331500         END-IF                                                           
331600       END-IF                                                             
331700     END-IF                                                               
331800     .                                                                    
331900                                                                          
332000 S25C-SKICKA-PRISFRAGA SECTION.                                           
332100                                                                          
332200     MOVE 1                      TO 3039-REQU-IDMSGVER                    
332300     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
332400     MOVE 'W4024300'             TO 3039-REQU-IDUSER                      
332500                                                                          
332600     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
332700     MOVE OBKR-IDDISTR           TO 3039-MID-IDDISTR                      
332800     MOVE OBKR-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
332900     MOVE OBKR-IDORDNR7          TO 3039-MID-IDBUNDLE                     
333000     MOVE PRQU-IDPRQUES          TO 3039-MID-IDPRQUES                     
333100                                                                          
333200     PERFORM S26-SKICKA-OPEN                                              
333300     PERFORM S26-SKICKA-MEDDELANDE                                        
333400     PERFORM S26-SKICKA-CLOSE                                             
333500     .                                                                    
333600                                                                          
333700 S26-SKICKA-OPEN SECTION.                                                 
333800                                                                          
333900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
334000     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
334100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
334200                                                                          
334300     IF SEND-KDRC > 0                                                     
334400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
334500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
334600       DELIMITED BY SIZE INTO FELTEXT                                     
334700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
334800     END-IF                                                               
334900     .                                                                    
335000     SKIP3                                                                
335100 S26-SKICKA-MEDDELANDE SECTION.                                           
335200                                                                          
335300     MOVE 'PUT'                      TO SEND-KDFUNC                       
335400     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
335500     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
335600                                                                          
335700     IF SEND-KDRC > 0                                                     
335800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
335900       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
336000       DELIMITED BY SIZE INTO FELTEXT                                     
336100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
336200     END-IF                                                               
336300     .                                                                    
336400     SKIP3                                                                
336500 S26-SKICKA-CLOSE SECTION.                                                
336600                                                                          
336700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
336800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
336900                                                                          
337000     IF SEND-KDRC > 0                                                     
337100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
337200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
337300       DELIMITED BY SIZE INTO FELTEXT                                     
337400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
337500     END-IF                                                               
337600     .                                                                    
337700     EJECT                                                                
337800                                                                          
337900                                                                          
338000 MFS-ROER-EJ-BILD SECTION.                                                
338100                                                                          
338200     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLANNULL                            
338300                                  MOD-KDORDKL-UT                          
338400                                                                          
338500     MOVE +1                   TO WS-INDEX                                
338600     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
338700                                                                          
338800        PERFORM MFS-SAETT-ATTRIBUT                                        
338900                                                                          
339000        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
339100                                  MOD-ASTERIX(WS-INDEX)                   
339200                                  MOD-KDBEHX(WS-INDEX)                    
339300                                  MOD-IDARTNR(WS-INDEX)                   
339400                                  MOD-BEART(WS-INDEX)                     
339500                                  MOD-IDDC-RAD(WS-INDEX)                  
339600                                  MOD-KVANTAL(WS-INDEX)                   
339700                                  MOD-KVQPACK(WS-INDEX)                   
339800                                  MOD-IDKUNDRF-RO(WS-INDEX)               
339900                                  MOD-KEYS(WS-INDEX)                      
340000        ADD +1                 TO WS-INDEX                                
340100     END-PERFORM                                                          
340200     .                                                                    
340300     EJECT                                                                
340400 MFS-SAETT-ATTRIBUT SECTION.                                              
340500                                                                          
340600     MOVE MID-RAD(WS-INDEX) TO WS-AKTUELL-MID-RAD                         
340700                                                                          
340800     IF WS-AKT-KDBEHX = 'B'                                               
340900       IF (WS-AKT-KDORDBEK = 41 OR 61) AND                                
341000                    (WS-AKT-IDARTNR NOT = WS-AKT-IDARTNR-URS)             
341100         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
341200                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
341300       ELSE                                                               
341400         IF WS-AKT-KDORDBEK      = 21 OR 51 OR                            
341500                                   41 OR 52 OR 53 OR 54 OR                
341600                                   55 OR 57 OR 58 OR 59 OR 66 OR          
341700                                   67 OR 72 OR 73 OR 74 OR 75 OR          
341800                                   76 OR 80 OR 81 OR 82 OR 85 OR          
341900                                   61                                     
342000           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX)             
342100         END-IF                                                           
342200                                                                          
342300         IF WS-AKT-KDORDBEK = 41 OR 61                                    
342400           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
342500                             TO MOD-IDDC-ATTR(WS-INDEX)                   
342600                                MOD-KVANTAL-ATTR(WS-INDEX)                
342700                                MOD-KVQPACK-ATTR(WS-INDEX)                
342800                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
342900         END-IF                                                           
343000         IF WS-AKT-KDORDBEK = 57 AND  WS-AKT-IDARTNR = ZERO               
343100            MOVE MFS-STAENG-FAELT-OSYNLIGT                                
343200                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
343300                                MOD-IDDC-ATTR(WS-INDEX)                   
343400                                MOD-KVANTAL-ATTR(WS-INDEX)                
343500                                MOD-KVQPACK-ATTR(WS-INDEX)                
343600                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
343700         END-IF                                                           
343800       END-IF                                                             
343900     EJECT                                                                
344000     ELSE                                                                 
344100       IF WS-AKT-KDORDBEK = 41 OR 61                                      
344200         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
344300                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
344400         IF (WS-AKT-KDORDBEK = 61) AND                                    
344500                  WS-AKT-IDARTNR = ZERO                                   
344600           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
344700                           TO MOD-KDBEHX-ATTR(WS-INDEX)                   
344800                              MOD-IDDC-ATTR(WS-INDEX)                     
344900                              MOD-KVANTAL-ATTR(WS-INDEX)                  
345000                              MOD-KVQPACK-ATTR(WS-INDEX)                  
345100                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX)              
345200         END-IF                                                           
345300       END-IF                                                             
345400     END-IF                                                               
345500     .                                                                    
345600     EJECT                                                                
345700                                                                          
345800 MFS-RENSA-MOD-RADER SECTION.                                             
345900                                                                          
346000     MOVE +1                 TO WS-INDEX                                  
346100     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
346200        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
346300                                MOD-ASTERIX(WS-INDEX)                     
346400                                MOD-KDBEHX(WS-INDEX)                      
346500                                MOD-IDARTNR(WS-INDEX)                     
346600                                MOD-BEART(WS-INDEX)                       
346700                                MOD-IDDC-RAD(WS-INDEX)                    
346800                                MOD-KVANTAL(WS-INDEX)                     
346900                                MOD-KVQPACK(WS-INDEX)                     
347000                                MOD-IDKUNDRF-RO(WS-INDEX)                 
347100                                MOD-KEYS(WS-INDEX)                        
347200        ADD 1                TO WS-INDEX                                  
347300     END-PERFORM                                                          
347400     .                                                                    
347500                                                                          
347600 MFS-RENSA-ALLA-FAELT SECTION.                                            
347700                                                                          
347800     MOVE MFS-RENSA-FAELT    TO MOD-FLANNULL                              
347900                                                                          
348000     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
348100                                MOD-IDLOPNR-NEXT                          
348200                                MOD-IDSEKVNR-NEXT                         
348300                                MOD-IDDC-NEXT                             
348400                                MOD-KDORDBEK-NEXT                         
348500                                                                          
348600     PERFORM MFS-RENSA-MOD-RADER                                          
348700     .                                                                    
348800     EJECT                                                                
348900                                                                          
349000 IMS-GET-MSG SECTION.                                                     
349100                                                                          
349200     MOVE '  QC' TO GODK-STATUSKODER                                      
349300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
349400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
349500     PERFORM IMS-STATUSKONTROLL                                           
349600     .                                                                    
349700     SKIP3                                                                
349800 IMS-INSERT-MSG SECTION.                                                  
349900                                                                          
350000     IF ENGLISH-TEXT                                                      
350100       MOVE 'N' TO MFS-KDHUVOMR                                           
350200     END-IF                                                               
350300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
350400     MOVE SPACE TO GODK-STATUSKODER                                       
350500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
350600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
350700     PERFORM IMS-STATUSKONTROLL                                           
350800     .                                                                    
350900                                                                          
351000 IMS-INSERT-4242-MSG SECTION.                                             
351100                                                                          
351200     IF ENGLISH-TEXT                                                      
351300       MOVE 'N' TO MFS-KDHUVOMR                                           
351400     END-IF                                                               
351500     MOVE LOW-VALUE TO 4242-Z1 4242-Z2                                    
351600     MOVE SPACE TO GODK-STATUSKODER                                       
351700     CALL CBLTDLI USING ISRT 4242-PCB 4242-MSG-IO-AREA                    
351800     MOVE 4242-STATUS-CODE TO STATUS-WS                                   
351900     PERFORM IMS-STATUSKONTROLL                                           
352000     .                                                                    
352100     EJECT                                                                
352200 IMS-INSERT-4292-MSG SECTION.                                             
352300                                                                          
352400     IF ENGLISH-TEXT                                                      
352500       MOVE 'N' TO MFS-KDHUVOMR                                           
352600     END-IF                                                               
352700     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
352800     MOVE SPACE TO GODK-STATUSKODER                                       
352900     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
353000     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
353100     PERFORM IMS-STATUSKONTROLL                                           
353200     .                                                                    
353300     EJECT                                                                
353400                                                                          
353500 IMS-INSERT-4298-MSG SECTION.                                             
353600                                                                          
353700     IF ENGLISH-TEXT                                                      
353800       MOVE 'N' TO MFS-KDHUVOMR                                           
353900     END-IF                                                               
354000     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
354100     MOVE SPACE TO GODK-STATUSKODER                                       
354200     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
354300     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
354400     PERFORM IMS-STATUSKONTROLL                                           
354500     .                                                                    
354600     SKIP2                                                                
354700 IMS-INSERT-4243-MSG SECTION.                                             
354800                                                                          
354900     IF ENGLISH-TEXT                                                      
355000       MOVE 'N' TO MFS-KDHUVOMR                                           
355100     END-IF                                                               
355200     MOVE SPACE TO GODK-STATUSKODER                                       
355300     CALL CBLTDLI USING ISRT 4243-PCB 4243-MSG-IO-AREA                    
355400     MOVE 4243-STATUS-CODE TO STATUS-WS                                   
355500     PERFORM IMS-STATUSKONTROLL                                           
355600     .                                                                    
355700     EJECT                                                                
355800                                                                          
355900 IMS-PURG-MSG-2191  SECTION.                                              
356000     MOVE SPACE TO GODK-STATUSKODER                                       
356100     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
356200     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
356300     PERFORM IMS-STATUSKONTROLL                                           
356400     .                                                                    
356500     EJECT                                                                
356600                                                                          
356700 IMS-01-GHU-ORQM-WDQ101-FOERE SECTION.                                    
356800                                                                          
356900     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
357000                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
357100                    '&FLOBOK   =' NEJ ')'                                 
357200          DELIMITED BY SIZE INTO SSA1                                     
357300     MOVE '  GE'               TO GODK-STATUSKODER                        
357400     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
357500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
357600     PERFORM IMS-STATUSKONTROLL                                           
357700     .                                                                    
357800     SKIP2                                                                
357900 IMS-02-GHN-ORQM-WDQ101-FOERE SECTION.                                    
358000                                                                          
358100     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
358200                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
358300                    '&FLOBOK   =' NEJ ')'                                 
358400          DELIMITED BY SIZE INTO SSA1                                     
358500     MOVE '  GEGB'             TO GODK-STATUSKODER                        
358600     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
358700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
358800     PERFORM IMS-STATUSKONTROLL                                           
358900     .                                                                    
359000     SKIP2                                                                
359100 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
359200                                                                          
359300     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
359400                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
359500                    '&FLOBOK   =' NEJ ')'                                 
359600          DELIMITED BY SIZE INTO SSA1                                     
359700     MOVE '  GE'               TO GODK-STATUSKODER                        
359800     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
359900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
360000     PERFORM IMS-STATUSKONTROLL                                           
360100     .                                                                    
360200     EJECT                                                                
360300 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
360400                                                                          
360500     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
360600                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
360700                    '&FLOBOK   =' NEJ ')'                                 
360800          DELIMITED BY SIZE INTO SSA1                                     
360900     MOVE '  GEGB'             TO GODK-STATUSKODER                        
361000     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
361100     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
361200     PERFORM IMS-STATUSKONTROLL                                           
361300     .                                                                    
361400     SKIP2                                                                
361500 IMS-05-GHU-ORQM-WDQ101-UNIK SECTION.                                     
361600                                                                          
361700     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
361800                    '&FLOBOK   =' NEJ ')'                                 
361900          DELIMITED BY SIZE  INTO SSA1                                    
362000     MOVE '  GE'               TO GODK-STATUSKODER                        
362100     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
362200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
362300     PERFORM IMS-STATUSKONTROLL                                           
362400     .                                                                    
362500     SKIP2                                                                
362600                                                                          
362700 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
362800                                                                          
362900     MOVE '    '               TO GODK-STATUSKODER                        
363000     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
363100     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
363200     PERFORM IMS-STATUSKONTROLL                                           
363300     .                                                                    
363400     EJECT                                                                
363500 IMS-07-ISRT-ORQM-WDQ101 SECTION.                                         
363600                                                                          
363700     MOVE 'WLORQM01 '          TO SSA1                                    
363800     MOVE '  '                 TO GODK-STATUSKODER                        
363900     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-ORQM SSA1               
364000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
364100     PERFORM IMS-STATUSKONTROLL                                           
364200     .                                                                    
364300     SKIP3                                                                
364400 IMS-08-DLET-ORQM-WDQ101 SECTION.                                         
364500                                                                          
364600     MOVE '    '               TO GODK-STATUSKODER                        
364700     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-ORQM                    
364800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
364900     PERFORM IMS-STATUSKONTROLL                                           
365000     .                                                                    
365100     EJECT                                                                
365200                                                                          
365300 IMS-09-GU-ORQI-WDQ201 SECTION.                                           
365400                                                                          
365500     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
365600          DELIMITED BY SIZE INTO SSA1                                     
365700     MOVE '  GE'               TO GODK-STATUSKODER                        
365800     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
365900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
366000     PERFORM IMS-STATUSKONTROLL                                           
366100     .                                                                    
366200     SKIP3                                                                
366300 IMS-10-GU-WDK611 SECTION.                                                
366400                                                                          
366500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
366600          DELIMITED BY SIZE INTO SSA1                                     
366700     MOVE   'WDK611  '        TO SSA2                                     
366800     MOVE '  '                 TO GODK-STATUSKODER                        
366900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
367000     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
367100     PERFORM IMS-STATUSKONTROLL                                           
367200     .                                                                    
367300     EJECT                                                                
367400 IMS-11-GHU-WDK711 SECTION.                                               
367500                                                                          
367600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
367700          DELIMITED BY SIZE  INTO SSA1                                    
367800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
367900          DELIMITED BY SIZE  INTO SSA2                                    
368000     MOVE '  GE'               TO GODK-STATUSKODER                        
368100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
368200     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
368300     PERFORM IMS-STATUSKONTROLL                                           
368400     .                                                                    
368500     SKIP3                                                                
368600 IMS-GU-WDK712           SECTION.                                         
368700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
368800            DELIMITED BY SIZE INTO SSA1                                   
368900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
369000            DELIMITED BY SIZE INTO SSA2                                   
369100     MOVE '  GE' TO GODK-STATUSKODER                                      
369200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
369300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
369400     PERFORM IMS-STATUSKONTROLL                                           
369500     .                                                                    
369600     SKIP3                                                                
369700 IMS-GU-WDK722 SECTION.                                                   
369800                                                                          
369900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
370000          DELIMITED BY SIZE  INTO SSA1                                    
370100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
370200          DELIMITED BY SIZE  INTO SSA2                                    
370300     MOVE 'WDK722 '            TO SSA3                                    
370400     MOVE '  GE'               TO GODK-STATUSKODER                        
370500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
370600     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
370700     PERFORM IMS-STATUSKONTROLL                                           
370800     .                                                                    
370900     SKIP3                                                                
371000 IMS-12-REPL-WDK711 SECTION.                                              
371100                                                                          
371200     MOVE '    '               TO GODK-STATUSKODER                        
371300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
371400     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
371500     PERFORM IMS-STATUSKONTROLL                                           
371600     .                                                                    
371700     EJECT                                                                
371800 IMS-13-GHNP-ORQI-WDQ212-UNIK SECTION.                                    
371900                                                                          
372000     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
372100          DELIMITED BY SIZE  INTO SSA1                                    
372200     MOVE '  GE'               TO GODK-STATUSKODER                        
372300     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1             
372400     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
372500     PERFORM IMS-STATUSKONTROLL                                           
372600     .                                                                    
372700     SKIP3                                                                
372800 IMS-14-GNP-ORQI-WDQ212 SECTION.                                          
372900                                                                          
373000     MOVE 'WLORQI12 '          TO SSA1                                    
373100     MOVE '  GE'               TO GODK-STATUSKODER                        
373200     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1              
373300     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
373400     PERFORM IMS-STATUSKONTROLL                                           
373500     .                                                                    
373600     EJECT                                                                
373700                                                                          
373800 IMS-15-REPL-ORQI-WDQ212 SECTION.                                         
373900                                                                          
374000     MOVE '    '               TO GODK-STATUSKODER                        
374100     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ORQI12                  
374200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
374300     PERFORM IMS-STATUSKONTROLL                                           
374400     .                                                                    
374500     SKIP3                                                                
374600 IMS-17-ISRT-ORQF-WDQ401 SECTION.                                         
374700                                                                          
374800     MOVE 'WLORQF01 '          TO SSA1                                    
374900     MOVE '  II'               TO GODK-STATUSKODER                        
375000     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORQF SSA1               
375100     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
375200     PERFORM IMS-STATUSKONTROLL                                           
375300     .                                                                    
375400     EJECT                                                                
375500                                                                          
375600 IMS-18A-GU-SATB-WDJ111-01 SECTION.                                       
375700                                                                          
375800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
375900          DELIMITED BY SIZE INTO SSA1                                     
376000     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
376100          DELIMITED BY SIZE INTO SSA2                                     
376200     MOVE '  GE'               TO GODK-STATUSKODER                        
376300     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
376400     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
376500     PERFORM IMS-STATUSKONTROLL                                           
376600     .                                                                    
376700     SKIP2                                                                
376800 IMS-18-GN-SATB-WDJ111-01 SECTION.                                        
376900                                                                          
377000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
377100          DELIMITED BY SIZE INTO SSA1                                     
377200     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
377300          DELIMITED BY SIZE INTO SSA2                                     
377400     MOVE '  GE'               TO GODK-STATUSKODER                        
377500     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
377600     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
377700     PERFORM IMS-STATUSKONTROLL                                           
377800     .                                                                    
377900     SKIP2                                                                
378000 IMS-19-ISRT-4541-WDR411 SECTION.                                         
378100                                                                          
378200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
378300          DELIMITED BY SIZE INTO SSA1                                     
378400     MOVE 'WDGX4542 '          TO SSA2                                    
378500     MOVE '  II'               TO GODK-STATUSKODER                        
378600     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
378700     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
378800     PERFORM IMS-STATUSKONTROLL                                           
378900     .                                                                    
379000     EJECT                                                                
379100                                                                          
379200 IMS-21-GHU-ARTM-WDK901 SECTION.                                          
379300                                                                          
379400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
379500          DELIMITED BY SIZE  INTO SSA1                                    
379600     MOVE '    '               TO GODK-STATUSKODER                        
379700     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
379800     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
379900     PERFORM IMS-STATUSKONTROLL                                           
380000     .                                                                    
380100                                                                          
380200 IMS-22-REPL-ARTM-WDK901 SECTION.                                         
380300                                                                          
380400     MOVE '    '               TO GODK-STATUSKODER                        
380500     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
380600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
380700     PERFORM IMS-STATUSKONTROLL                                           
380800     .                                                                    
380900     EJECT                                                                
381000 IMS-23-GU-BENA-WDD311 SECTION.                                           
381100                                                                          
381200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
381300          DELIMITED BY SIZE INTO SSA1                                     
381400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
381500          DELIMITED BY SIZE INTO SSA2                                     
381600     MOVE '  GE'               TO GODK-STATUSKODER                        
381700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
381800     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
381900     PERFORM IMS-STATUSKONTROLL                                           
382000     .                                                                    
382100     SKIP2                                                                
382200 IMS-24-GU-ORDP-WDA501 SECTION.                                           
382300                                                                          
382400     STRING 'WLORDP01(WDA501KY=>' W-WDA5KEY-MIN-X                         
382500                    '&WDA501KY=<' W-WDA5KEY-MAX-X ')'                     
382600          DELIMITED BY SIZE  INTO SSA1                                    
382700     MOVE '  GEGB'             TO GODK-STATUSKODER                        
382800     CALL CBLTDLI USING GU  ORDP-PCB DLI-IO-AREA-ORDP SSA1                
382900     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
383000     PERFORM IMS-STATUSKONTROLL                                           
383100     .                                                                    
383200     SKIP2                                                                
383300                                                                          
383400 IMS-26-GHU-ORDP-WDA501 SECTION.                                          
383500                                                                          
383600     STRING 'WLORDP01(WDA501KY =' W-WDA5KEY-X ')'                         
383700          DELIMITED BY SIZE  INTO SSA1                                    
383800     MOVE '    '               TO GODK-STATUSKODER                        
383900     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
384000     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
384100     PERFORM IMS-STATUSKONTROLL                                           
384200     .                                                                    
384300     SKIP3                                                                
384400 IMS-27-REPL-ORDP-WDA501 SECTION.                                         
384500                                                                          
384600     MOVE '    '               TO GODK-STATUSKODER                        
384700     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA-ORDP                    
384800     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
384900     PERFORM IMS-STATUSKONTROLL                                           
385000     .                                                                    
385100     SKIP3                                                                
385200 IMS-33-GN-ORQM-WDQ101 SECTION.                                           
385300                                                                          
385400     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
385500                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
385600          DELIMITED BY SIZE INTO SSA1                                     
385700     MOVE '  GEGB'             TO GODK-STATUSKODER                        
385800     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
385900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
386000     PERFORM IMS-STATUSKONTROLL                                           
386100     .                                                                    
386200     SKIP2                                                                
386300 IMS-34-GHU-ORQM-WDQ101 SECTION.                                          
386400                                                                          
386500     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
386600                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
386700          DELIMITED BY SIZE INTO SSA1                                     
386800     MOVE '  '                 TO GODK-STATUSKODER                        
386900     CALL CBLTDLI USING GHU  ORQM-PCB DLI-IO-AREA-ORQM SSA1               
387000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
387100     PERFORM IMS-STATUSKONTROLL                                           
387200     .                                                                    
387300 IMS-35-GHU-PROC-WDE801 SECTION.                                          
387400                                                                          
387500     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
387600          DELIMITED BY SIZE  INTO SSA1                                    
387700     MOVE '  GE'               TO GODK-STATUSKODER                        
387800     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-PROC SSA1                
387900     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
388000     PERFORM IMS-STATUSKONTROLL                                           
388100     .                                                                    
388200     EJECT                                                                
388300 IMS-36-REPL-PROC-WDE801 SECTION.                                         
388400                                                                          
388500     MOVE '    '               TO GODK-STATUSKODER                        
388600     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-PROC                    
388700     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
388800     PERFORM IMS-STATUSKONTROLL                                           
388900     .                                                                    
389000     SKIP2                                                                
389100                                                                          
389200 IMS-GU-WDB601    SECTION.                                                
389300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
389400          DELIMITED BY SIZE INTO SSA1                                     
389500     MOVE '  GE' TO GODK-STATUSKODER                                      
389600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
389700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
389800     PERFORM IMS-STATUSKONTROLL                                           
389900     IF SEGMENT-SAKNAS                                                    
390000        MOVE SPACE TO DCS-KDDC                                            
390100     END-IF                                                               
390200     .                                                                    
390300     SKIP2                                                                
390310 IMS-GU-WDB201 SECTION.                                                   
390320                                                                          
390330     STRING  'WDB201  (IDGMT    =' W-WDB201KEY-X ')'                      
390340            DELIMITED BY SIZE INTO SSA1                                   
390350     MOVE    '    '             TO GODK-STATUSKODER                       
390360     CALL    CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-B201 SSA1              
390370     MOVE    WDB2-STATUS-CODE   TO STATUS-WS                              
390380     PERFORM IMS-STATUSKONTROLL                                           
390390     .                                                                    
390391     EJECT                                                                
390400 IMS-STATUSKONTROLL SECTION.                                              
390500                                                                          
390600     SET STATUS-IX TO 1                                                   
390700     SEARCH GODK-STATUS                                                   
390800       AT END CALL FELLOG                                                 
390900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
391000     END-SEARCH                                                           
391100     .                                                                    
391200     EJECT                                                                
391300*    -COPY WY2000P1                                                       
