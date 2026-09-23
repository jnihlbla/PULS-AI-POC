000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4021300.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000400 DATE-WRITTEN.   APRIL -90.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        PROGRAMMET HANTERAR SVARSBILD TILL ORDERREGISTRERING             
000900*        4211/4212.                                                       
001000*        VISAR AVVIKELSER.                                                
001100*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001200*        ANNULLATION AV HEL ORDER MÖJLIG.                                 
001300*                                                                         
001400*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
001500*        4211.                                                            
001600*                                                                         
001700*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001800*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
001900*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002000*        PROGRAMMET LÄSER      WLORQA (WDQ4)  ORDERDELAR                  
002100*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002200*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
002300*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
002400*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002500*        PROGRAMMET LÄSER      WLXXKK (WDR1)  TVÅNGSSTYRN.TAB             
002600*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002700*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
002800*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
002900*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
003000*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
003100*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003200*        PROGRAMMET LÄSER      WLXXKR (WDR4)  KAMPANJREGISTER             
003300*        PROGRAMMET LÄSER      WLXXKS (WDR4)  MARKN.REG KAMPANJ           
003400*        PROGRAMMET LÄSER      WLXXKT (WDR4)  ANTALSTAB KAMPANJ           
003500*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003600*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003700*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
003800*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDSKALENDER          
003900*                                                                         
004000*    INDATA.                                                              
004100*        TRANSAKTION: W4T213                                              
004200*                     W4T213U                                             
004300*                     W4T213V                                             
004400*        MID:         W4I21301                                            
004500*                                                                         
004600*    UTDATA.                                                              
004700*        MOD:         W4O21301                                            
004800*                                                                         
004900*    E'TRACKER: 5444132 DATED 2007-08-21                                  
005000*    E'TRACKER: 2218613 DATED 2008-03-12 KAMPANJORDER SEPARAT PRC.        
005100*    E'TRACKER: 6605105 DATED 2008-04-01 CCC STEERING TO PRC.             
005200*    E'TRACKER: 6921638 2008-06  LAPS OC, RFS                             
005300*    E'TRACKER: 7450328 2008-HÖST  VOHF                                   
005400*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
005500*    E'TRACKER: 10254592 2015      DECOMISION VOHF                        
005600*    E'TRACKER: 10263222 2015      FORCE TO END ORDER REG                 
005700*                                                                         
005800 ENVIRONMENT DIVISION.                                                    
005900                                                                          
006000 DATA DIVISION.                                                           
006100                                                                          
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400*    -COPY WY2000W1                                                       
006500                                                                          
006600 77  IDPGM                       PIC X(08)   VALUE 'W4021300'.            
006700 77  HOPP                        PIC X(1)   VALUE 'N'.                    
006800 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
006900 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
007000                                                                          
007100 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
007200 77  WS-KDFRAKT                  PIC 9(2)   VALUE ZERO.                   
007300 77  WS-IDDISTR-NUM4             PIC 9(4)   VALUE ZERO.                   
007400 77  WS-IDKUNDNR-NUM6            PIC 9(6)   VALUE ZERO.                   
007500                                                                          
007600 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
007700 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
007900 77  4211-MOD-LAENGD             PIC S9(4)  VALUE +44   COMP SYNC.        
008000 77  WS-RADER                    PIC 9(2)   VALUE ZERO.                   
008100 77  4212-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
008200 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008300 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008400 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
008500 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
008600 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
008700 77  WS-INDEX-SATS               PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-SATS-MAX           PIC S9(9)   COMP SYNC VALUE +5.          
008900 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
009000 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE 100.         
009100 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
009200 77  WS-KVSLASK                  PIC S9(7)   VALUE +0  COMP-3.            
009300 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
009400 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
009500 77  SPAR-IDARTNR-TILLK          PIC S9(9)   VALUE +0  COMP-3.            
009600 77  SPAR-KVBEART-TILLK          PIC S9(7)   VALUE +0  COMP-3.            
009700 77  SPAR-REKSIFFR-TILLK         PIC S9(1)   VALUE +0  COMP-3.            
009800 77  SPAR-DIERS-KVOT          PIC S9(4)V9(3) VALUE +0  COMP-3.            
009900 77  SPAR-IDARTNR-40             PIC S9(9)   VALUE +0  COMP-3.            
010000 77  SPAR-IDLOPNR-40             PIC S9(3)   VALUE +0  COMP-3.            
010100 77  SPAR-IDSEKVNR-40            PIC S9(3)   VALUE +0  COMP-3.            
010200 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010300 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010400 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010500 77  WS-IDANSK                   PIC  9(3)   VALUE ZERO.                  
010600                                                                          
010700     EJECT                                                                
010800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010900     88  ALLT-OK                             VALUE 'J'.                   
011000 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
011100     88  NYCKEL-OK                           VALUE 'J'.                   
011200 77  AKT-SIDA-SW                 PIC X       VALUE 'N'.                   
011300     88  AKT-SIDA                            VALUE 'J'.                   
011400 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
011500     88  AVSLUTA                             VALUE 'J'.                   
011600 77  START-4212-SW               PIC X       VALUE 'N'.                   
011700     88  START-4212                          VALUE 'J'.                   
011800 77  NEXT-SATS-SW                PIC X       VALUE 'N'.                   
011900     88  NEXT-SATS                           VALUE 'J'.                   
012000                                                                          
012100 01  FILLER                      PIC X(8)   VALUE 'IDTRANS'.              
012200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012300     88  EGEN-MID                            VALUE '4213'.                
012400     88  GODK-MID                            VALUE '4211' '4212'          
012500                                                   '4213' '4214'          
012600                                                   '4297' '4206'.         
012700 01  X-IDTRANS                   PIC X(4)   VALUE SPACE.                  
012800 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
012900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
013000     03 WS-TIHHMM                PIC 9(4).                                
013100     03 FILLER                   PIC 9(2).                                
013200     EJECT                                                                
013300                                                                          
013400 01  WS-ALFA-1.                                                           
013500     03  WS-NUM-1                PIC 9(1).                                
013600 01  WS-ALFA-5.                                                           
013700     03  WS-NUM-5                PIC 9(5).                                
013800 01  WS-ALFA-6.                                                           
013900     03  WS-NUM-6                PIC 9(6).                                
014000 01  WS-ALFA-9.                                                           
014100     03  WS-NUM-9                PIC 9(9).                                
014200 01  WS-ALFA-10.                                                          
014300     03  WS-NUM-10               PIC 9(10).                               
014400                                                                          
014500 01  W-SPAR-IDKUNDRF.                                                     
014600     03  FILLER                  PIC X(2)    VALUE '00'.                  
014700     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
014800     03  FILLER                  PIC X(3)    VALUE '+++'.                 
014900                                                                          
015000 01  WS-IDARTNR-REKSIFFR.                                                 
015100     03  WS-IDARTNR              PIC 9(9).                                
015200     03  FILLER                  PIC X(1)   VALUE '-'.                    
015300     03  WS-REKSIFFR             PIC 9(1).                                
015400                                                                          
015500     EJECT                                                                
015600 01  WS-AKTUELL-MID-RAD.                                                  
015700     03  WS-AKT-KDORDBEK         PIC 9(2).                                
015800     03  WS-AKT-KDBEHX           PIC X(1).                                
015900     03  WS-AKT-IDARTNR          PIC 9(9).                                
016000     03  WS-AKT-FILLER           PIC X(1).                                
016100     03  WS-AKT-REKSIFFR         PIC 9(1).                                
016200     03  WS-AKT-IDDC             PIC X(2).                                
016300     03  WS-AKT-IDKUNDRF-RO      PIC X(7).                                
016400     03  WS-AKT-KEYS.                                                     
016500         05 WS-AKT-IDLOPNR       PIC 9(3).                                
016600         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
016700         05 WS-AKT-IDARTNR-URS   PIC 9(9).                                
016800         05 WS-AKT-IDLOPNR-RO    PIC 9(3).                                
016900                                                                          
017000 01  WS-IDARTNR-SATS-TAB.                                                 
017100     05 WS-IDARTNR-SATS          PIC X(9)    OCCURS 5.                    
017200                                                                          
017300 01  WS-KEYS-SPAR.                                                        
017400     03 WS-IDLOPNR-SPAR          PIC 9(3).                                
017500     03 WS-IDSEKVNR-SPAR         PIC 9(3).                                
017600     03 WS-IDARTNR-URS-SPAR      PIC 9(9).                                
017700     03 WS-IDARTNR-SPAR          PIC 9(9).                                
017800     03 WS-IDDC-SPAR             PIC X(2).                                
017900                                                                          
018000 01  SPAR-ARBTAB-DATA.                                                    
018100   03  SPAR-ARB-KDFRAKT           PIC S9(3)       COMP-3.                 
018200   03  SPAR-ARB-KDFDKRAV          PIC S9(3)       COMP-3.                 
018300   03  SPAR-ARB-KDROPACK          PIC X.                                  
018400     EJECT                                                                
018500                                                                          
018600 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
018700 01  FILLER    REDEFINES TEST-IDDISTR.                                    
018800*    03  -COPY WWDIST07                                                   
018900     EJECT                                                                
019000 01  FILLER    REDEFINES TEST-IDDISTR.                                    
019100*    ----DIST79-DEALER-PRICE-----                                         
019200*    03  -COPY  WWDIST79                                                  
019300     EJECT                                                                
019400                                                                          
019500*01  -COPY WWPRODSL                                                       
019600                                                                          
019700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019800 01  GENERELLA-SUBPROGRAM.                                                
019900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
020000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
020500     EJECT                                                                
020600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020700*01 -COPY WMSGINIT                                                        
020800     EJECT                                                                
020900*                                                                         
021000*                                                                         
021100*                                                                         
021200 01  GEMENSAMMA-SUBPROGRAM.                                               
021300     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
021400*        LÄSNING ARTIKELREGISTER                                          
021500     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
021600*        KONTROLL DIREKTLEVERANS                                          
021700     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
021800*        DATA TILL DELIVERYNOTE NDC                                       
021900     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
022000*        KONTROLL ENHETSLAST                                              
022100     EJECT                                                                
022200     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
022300*        KONTROLL PRELIMINÄRAVBOKNING                                     
022400     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
022500*        BERÄKNA RANSONERING                                              
022600     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
022700*        KONTROLL/UPPDATERING TPO2                                        
022800     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
022900*        UPPDATERING TPO6                                                 
023000     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
023100*        JUSTERING LAGERPLATS OCH LAGEROMRÅDE                             
023200     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
023300*        WOPS PER RAD                                                     
023400     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
023500*        PRISFRÅGA                                                        
023600     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
023700*        PRISFRÅGA                                                        
023800     EJECT                                                                
023900*   -COPY W402W001                                                        
024000     EJECT                                                                
024100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
024200 01  MESSAGE-CODES.                                                       
024300     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
024400     03  MED-UPPLYSN-UPPDAT-PF   PIC X(3)    VALUE '144'.                 
024500     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
024600     03  MED-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
024700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024800     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
024900     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
025000     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
025100     03  ERR-EJ-ANNULLATION      PIC X(3)    VALUE '066'.                 
025200     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
025300     SKIP2                                                                
025400*   -COPY WMEDAREA                                                        
025500     EJECT                                                                
025600*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
025700 01 FILLER                       PIC X(8)    VALUE 'W411AREG'.            
025800*   -COPY W411AREG                                                        
025900     EJECT                                                                
026000 01 FILLER                       PIC X(8)    VALUE 'W411DNOT'.            
026100*   -COPY W411DNOT                                                        
026200     EJECT                                                                
026300 01 FILLER                       PIC X(8)    VALUE 'W411DLEV'.            
026400*   -COPY W411DLEV                                                        
026500     EJECT                                                                
026600 01 FILLER                       PIC X(8)    VALUE 'W411LAST'.            
026700*   -COPY W411LAST                                                        
026800     EJECT                                                                
026900 01 FILLER                       PIC X(8)    VALUE 'W411CDCA'.            
027000*   -COPY W411CDCA                                                        
027100     EJECT                                                                
027200 01 FILLER                       PIC X(8)    VALUE 'W411RANS'.            
027300*   -COPY W411RANS                                                        
027400     EJECT                                                                
027500 01 FILLER                       PIC X(8)    VALUE 'W411TPO2'.            
027600*   -COPY W411TPO2                                                        
027700     EJECT                                                                
027800 01 FILLER                       PIC X(8)    VALUE 'W411TPO6'.            
027900*   -COPY W411TPO6                                                        
028000     EJECT                                                                
028100 01 FILLER                       PIC X(8)    VALUE 'W413AVSR'.            
028200*   -COPY W413AVSR                                                        
028300     EJECT                                                                
028400 01 FILLER                       PIC X(8)    VALUE 'W413ADRS'.            
028500*   -COPY W413ADRS                                                        
028600     EJECT                                                                
028700 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
028800*   -COPY W335PRNO                                                        
028900     EJECT                                                                
029000 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
029100*   -COPY W335PRQU                                                        
029200     EJECT                                                                
029300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029400*                                                                         
029500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029600     SKIP3                                                                
029700*01  MID -COPY W4I21301                                                   
029800     EJECT                                                                
029900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
030000     SKIP3                                                                
030100*01  -COPY WMSGAREA                                                       
030200     EJECT                                                                
030300*    03  MOD -COPY W4O21301   -RED MSG-AREA.                              
030400     EJECT                                                                
030500******************************************************************        
030600*    MID-AREA FÖR W2T191                                         *        
030700******************************************************************        
030800*01  -COPY  W2I19101  -PRE 2191-                                          
030900     EJECT                                                                
031000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
031100     SKIP3                                                                
031200*01  -COPY WMFSAREA                                                       
031300     EJECT                                                                
031400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031600 01  NYCKLAR-TILL-DLI.                                                    
031700                                                                          
031800     03  W-WDQ101-KEY-UNIK.                                               
031900         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
032000         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
032100         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
032200         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
032300         05  W-Q1-IDDC-UNIK      PIC  X(2)   VALUE ZERO.                  
032400         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
032500                                                                          
032600     03  W-WDQ101-KEY-MIN.                                                
032700         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
032800         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
032900         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
033000         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
033100         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
033200         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
033300     03  W-WDQ101-KEY-MAX.                                                
033400         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
033500         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
033600         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
033700         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
033800         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
033900         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
034000                                                                          
034100     03  W-WDQ101-KEY-MIN1.                                               
034200         05  W-Q1-IDORDER-MIN1   PIC S9(7)   COMP-3.                      
034300         05  W-Q1-IDARTNR-MIN1   PIC S9(9)   COMP-3.                      
034400         05  W-Q1-IDLOPNR-MIN1   PIC S9(3)   COMP-3.                      
034500         05  W-Q1-IDSEKVNR-MIN1  PIC S9(3)   COMP-3.                      
034600         05  FILLER              PIC X(4)    VALUE LOW-VALUE.             
034700     03  W-WDQ101-KEY-MAX1.                                               
034800         05  W-Q1-IDORDER-MAX1   PIC S9(7)   COMP-3.                      
034900         05  W-Q1-IDARTNR-MAX1   PIC S9(9)   COMP-3.                      
035000         05  W-Q1-IDLOPNR-MAX1   PIC S9(3)   COMP-3.                      
035100         05  W-Q1-IDSEKVNR-MAX1  PIC S9(3)   COMP-3.                      
035200         05  FILLER              PIC X(4)    VALUE HIGH-VALUE.            
035300     EJECT                                                                
035400                                                                          
035500     03  W-WDJ1CSEQ-X.                                                    
035600         05  W-J1-IDLEVNR        PIC X(5)    VALUE SPACE.                 
035700         05  FILLER              PIC X(30)   VALUE SPACE.                 
035800         05  W-J1-IDARTNR        PIC S9(9)   VALUE +0  COMP-3.            
035900                                                                          
036000     03  W-IDLEVNR-X             PIC X(5)    VALUE '1002 '.               
036100                                                                          
036200     03  W-WDA5KEY-X.                                                     
036300         05  W-A5-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
036400         05  W-A5-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
036500         05  W-A5-IDKUNDRF       PIC X(10)   VALUE SPACE.                 
036600         05  W-A5-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
036700         05  W-A5-IDLOPNR        PIC S9(3)   VALUE ZERO COMP-3.           
036800     03  W-WDA5KEY-MIN-X.                                                 
036900         05  W-A5-IDDISTR-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
037000         05  W-A5-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
037100         05  W-A5-IDKUNDRF-MIN   PIC X(10)   VALUE SPACE.                 
037200         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
037300     03  W-WDA5KEY-MAX-X.                                                 
037400         05  W-A5-IDDISTR-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
037500         05  W-A5-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
037600         05  W-A5-IDKUNDRF-MAX   PIC X(10)   VALUE SPACE.                 
037700         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
037800                                                                          
037900     03  W-IDGMTREF-X.                                                    
038000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
038100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
038200         05  W-IDKUNDRF.                                                  
038300            07  W-IDORDNR        PIC 9(7)    VALUE ZERO.                  
038400            07  FILLER           PIC X(3)    VALUE SPACE.                 
038500     EJECT                                                                
038600     03  W-WDE801KY-X.                                                    
038700         05  W-E8-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
038800         05  W-E8-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
038900         05  W-E8-IDKUNDRF.                                               
039000            07  W-E8-IDORDNR     PIC 9(7)    VALUE ZERO.                  
039100            07  FILLER           PIC X(3)    VALUE SPACE.                 
039200                                                                          
039300     03  W-WDB201KEY-X.                                                   
039400         05  W-WDB2-IDDISTR      PIC S9(5) COMP-3 VALUE +0.               
039500         05  W-WDB2-IDKUNDNR     PIC S9(7) COMP-3 VALUE +0.               
039600                                                                          
039700     03  W-IDORDER-X.                                                     
039800         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
039900     03  W-IDDC-X.                                                        
040000         05  W-IDDC              PIC  X(2)   VALUE ZERO.                  
040100     03  W-IDLAND-X.                                                      
040200         05  W-IDLAND            PIC  X(2)   VALUE ZERO.                  
040300     03  W-IDARTNR-X.                                                     
040400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
040500     03  W-IDSKYLT-X.                                                     
040600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
040700     03  W-WDGXKEY-4541-X.                                                
040800         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
040900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
041000     EJECT                                                                
041100     03  W-IDDC-B6-X.                                                     
041200         05 W-IDDC-B6                  PIC X(2).                          
041300     EJECT                                                                
041400*    --- STATUS-KOD FRÅN IMS                                              
041500 01  STATUS-WS                   PIC XX.                                  
041600     88  SEGMENT-FINNS                       VALUE '  '.                  
041700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041800     88  BASEN-SLUT                          VALUE 'GB'.                  
041900     SKIP2                                                                
042000 01  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
042100     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
042200     SKIP2                                                                
042300 01  GODK-STATUSKODER.                                                    
042400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042500     SKIP3                                                                
042600 01  SSA1                        PIC X(160).                              
042700 01  SSA2                        PIC X(96).                               
042800 01  SSA3                        PIC X(96).                               
042900                                                                          
043000     EJECT                                                                
043100*    --- IMS FUNKTIONSKODER                                               
043200*01  -COPY W0003                                                          
043300     EJECT                                                                
043400*    ---  DLI INPUT-OUTPUT AREA                                           
043500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
043600     SKIP3                                                                
043700 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
043800 01  DLI-IO-AREA-ORQM.                                                    
043900     03  WLORQM01.                                                        
044000*        05  -COPY WDQ101                                                 
044100     EJECT                                                                
044200 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
044300 01  DLI-IO-AREA-ORQI01.                                                  
044400     03  WLORQI01.                                                        
044500*        05  -COPY WDQ201                                                 
044600     EJECT                                                                
044700 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
044800 01  DLI-IO-AREA-ORQI12.                                                  
044900     03  WLORQI12.                                                        
045000*        05  -COPY WDQ212                                                 
045100     EJECT                                                                
045200 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
045300 01  DLI-IO-AREA-ORQF.                                                    
045400     03  WLORQF01.                                                        
045500*        05  -COPY WDQ401                                                 
045600     EJECT                                                                
045700 01  FILLER                      PIC X(16)   VALUE 'WDE801-AREA'.         
045800 01  DLI-IO-AREA-PROC.                                                    
045900     03  WLPROC01.                                                        
046000*        05  -COPY WDE801                                                 
046100     EJECT                                                                
046200 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
046300 01  DLI-IO-AREA-ARTM.                                                    
046400     03  WLARTM01.                                                        
046500*        05  -COPY WDK901                                                 
046600     EJECT                                                                
046700 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
046800 01  DLI-IO-AREA-WDK7.                                                    
046900     03  WDK711.                                                          
047000*        05  -COPY WDK711                                                 
047100 01  FILLER                      PIC X(16)   VALUE 'WDK712-AREA'.         
047200 01  DLI-IO-AREA-WDK712.                                                  
047300     03  WDK712.                                                          
047400*        05  -COPY WDK712                                                 
047500     EJECT                                                                
047600 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
047700 01  DLI-IO-WDK722.                                                       
047800     03  WDK722.                                                          
047900*        05  -COPY WDK722                                                 
048000     EJECT                                                                
048100 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
048200 01  DLI-IO-AREA-BENA.                                                    
048300     03  WLBENA11.                                                        
048400*        05  -COPY WDD311                                                 
048500     EJECT                                                                
048600 01  FILLER                      PIC X(16)   VALUE 'WDA501-AREA'.         
048700 01  DLI-IO-AREA-ORDP.                                                    
048800     03  WLORDP01.                                                        
048900*        05  -COPY WDA501                                                 
049000     EJECT                                                                
049100 01  FILLER                      PIC X(16)   VALUE 'WDJ1  -AREA'.         
049200 01  DLI-IO-AREA-SATB.                                                    
049300     03  WLSATB11.                                                        
049400*        05  -COPY WDJ111                                                 
049500     03  WLSATB01.                                                        
049600*        05  -COPY WDJ101                                                 
049700     EJECT                                                                
049800 01  FILLER                      PIC X(16)   VALUE 'VOR-KÖ-AREA'.         
049900 01  DLI-IO-AREA-4541.                                                    
050000     03  WL454111.                                                        
050100*        05  -COPY WDGX4542                                               
050200     EJECT                                                                
050300 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
050400 01  DLI-IO-AREA-WDK611.                                                  
050500*    03  -COPY WDK611                                                     
050600     EJECT                                                                
050700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050800 01   DLI-IO-AREA-B601.                                                   
050900*     03  -COPY WDB601                                                    
051000     EJECT                                                                
051100 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
051200 01   DLI-IO-AREA-B201.                                                   
051300*     03  -COPY WDB201                                                    
051400     EJECT                                                                
051500 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
051600 01  4213-MSG-IO-AREA.                                                    
051700     03  4213-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
051800     03  4213-Z1               PIC X      VALUE LOW-VALUE.                
051900     03  4213-Z2               PIC X      VALUE LOW-VALUE.                
052000     03  4213-TRANSKOD         PIC X(8)   VALUE 'W4T213V '.               
052100     03  4213-IDTRANS          PIC X(4)   VALUE '4213'.                   
052200     03  4213-SPRAK            PIC X.                                     
052300     03  4213-IDDISTR-IN       PIC X(4).                                  
052400     03  4213-IDKUNDNR-IN      PIC X(6).                                  
052500     03  4213-IDORDNR-IN       PIC X(5).                                  
052600     03  4213-IDDISTR-UT       PIC X(4).                                  
052700     03  4213-IDKUNDNR-UT      PIC X(6).                                  
052800     03  4213-IDORDNR-UT       PIC X(5).                                  
052900     03  4213-KDORDKL-UT       PIC X      VALUE SPACE.                    
053000     03  4213-FLANNULL         PIC X      VALUE 'N'.                      
053100     03  FILLER                PIC X(20)  VALUE ZERO.                     
053200                                                                          
053300 01  4292-MSG-IO-AREA.                                                    
053400     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
053500     03  4292-Z1               PIC X.                                     
053600     03  4292-Z2               PIC X.                                     
053700     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
053800     03  4292-IDTRANS          PIC X(4)   VALUE '4213'.                   
053900     03  4292-SPRAK            PIC X.                                     
054000     03  4292-IDORDER          PIC X(7).                                  
054100     03  4292-IDDISTR          PIC X(4).                                  
054200     03  4292-IDKUNDNR         PIC X(6).                                  
054300     03  4292-IDKUNDRF         PIC X(7).                                  
054400     03  FILLER                PIC X(6)   VALUE SPACE.                    
054500     EJECT                                                                
054600 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
054700 01  4298-MSG-IO-AREA.                                                    
054800     03  4298-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
054900     03  4298-Z1               PIC X.                                     
055000     03  4298-Z2               PIC X.                                     
055100     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
055200     03  4298-IDTRANS          PIC X(4)   VALUE '4213'.                   
055300     03  4298-SPRAK            PIC X      VALUE SPACE.                    
055400*    03  -COPY W4I29801  -PRE 4298-                                       
055500     EJECT                                                                
055600 01  FILLER                  PIC X(16)  VALUE '4212-MSG-IO-AREA'.         
055700 01  4212-MSG-IO-AREA.                                                    
055800     03  4212-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
055900     03  4212-Z1               PIC X.                                     
056000     03  4212-Z2               PIC X.                                     
056100     03  4212-TRANSKOD         PIC X(8)   VALUE 'W4T212U '.               
056200     03  4212-IDTRANS          PIC X(4)   VALUE '4213'.                   
056300     03  4212-SPRAK            PIC X.                                     
056400     03  -COPY W4I21201  -PRE 4212-                                       
056500     EJECT                                                                
056600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
056700     SKIP3                                                                
056800 01  -COPY WZ01SEND                                                       
056900     EJECT                                                                
057000 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
057100     SKIP3                                                                
057200 01  SEND-AREA.                                                           
057300*    03  -COPY WZ01REQU  -PRE 3039-                                       
057400*    03  -COPY W30391I1  -PRE 3039-                                       
057500     EJECT                                                                
057600 LINKAGE SECTION.                                                         
057700                                                                          
057800*01  -COPY W0009   -PRE MSG-                                              
057900                                                                          
058000 01  AVSR-ALT-PCB                PIC X.                                   
058100     EJECT                                                                
058200*01  -COPY W0009   -PRE 4292-                                             
058300     EJECT                                                                
058400*01  -COPY W0009   -PRE 4298-                                             
058500     EJECT                                                                
058600*01  -COPY W0009   -PRE 4212-                                             
058700     -COPY W0009   -PRE 4213-                                             
058800     EJECT                                                                
058900*01  -COPY W0009   -PRE 2191-                                             
059000     EJECT                                                                
059100*01  -COPY W0008   -PRE USEA-                                             
059200     05  FILLER                  PIC X.                                   
059300     EJECT                                                                
059400*01  -COPY W0008   -PRE SATB-                                             
059500     05  FILLER                  PIC X.                                   
059600     SKIP2                                                                
059700*01  -COPY W0009   -PRE PRQRY-                                            
059800     05  FILLER                  PIC X.                                   
059900     SKIP2                                                                
060000*01  -COPY W0008   -PRE ARTM-                                             
060100     05  FILLER                  PIC X.                                   
060200     EJECT                                                                
060300*01  -COPY W0008   -PRE WDK7-                                             
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008   -PRE BENA-                                             
060700     05  FILLER                  PIC X.                                   
060800     SKIP2                                                                
060900*01  -COPY W0008   -PRE ORDP-                                             
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01  -COPY W0008   -PRE ORQF-                                             
061300     05  FILLER                  PIC X.                                   
061400     SKIP2                                                                
061500*01  -COPY W0008   -PRE ORQI-                                             
061600     05  FILLER                  PIC X.                                   
061700     EJECT                                                                
061800*01  -COPY W0008   -PRE ORQM-                                             
061900     05  FILLER                  PIC X.                                   
062000     EJECT                                                                
062100*01  -COPY W0008   -PRE 4541-                                             
062200     05  FILLER                  PIC X.                                   
062300     EJECT                                                                
062400*01  -COPY W0008   -PRE WDK6-                                             
062500     05  FILLER                  PIC X.                                   
062600     SKIP2                                                                
062700*01  -COPY W0008   -PRE PROC-                                             
062800     05  FILLER                  PIC X.                                   
062900     EJECT                                                                
063000*01  -COPY W0008   -PRE WDB6-                                             
063100     05  FILLER                  PIC X.                                   
063200     EJECT                                                                
063300*01  -COPY W0008   -PRE WDB2-                                             
063400     05  FILLER                  PIC X.                                   
063500     EJECT                                                                
063600 01  AREG-WDK6-PCB               PIC X.                                   
063700 01  AREG-WDK7-PCB               PIC X.                                   
063800                                                                          
063900 01  DLEV-LEVF-PCB               PIC X.                                   
064000 01  DLEV-LEVG-PCB               PIC X.                                   
064100 01  DLEV-LEVA-PCB               PIC X.                                   
064200 01  DLEV-ARTS-PCB               PIC X.                                   
064300 01  DLEV-WDB6-PCB               PIC X.                                   
064400                                                                          
064500 01  DNOT-ORQP-PCB               PIC X.                                   
064600 01  DNOT-ORQP2-PCB              PIC X.                                   
064700 01  DNOT-ORQP3-PCB              PIC X.                                   
064800 01  DNOT-4013-PCB               PIC X.                                   
064900 01  DNOT-BENA-PCB               PIC X.                                   
065000                                                                          
065100 01  CDCA-ARTM-PCB               PIC X.                                   
065200 01  CDCA-INLB-PCB               PIC X.                                   
065300 01  CDCA-WDB2-PCB               PIC X.                                   
065400 01  CDCA-WDC1-PCB               PIC X.                                   
065500                                                                          
065600 01  RANS-XXKM-PCB               PIC X.                                   
065700 01  RANS-ARTM-PCB               PIC X.                                   
065800 01  RANS-ARTS-PCB               PIC X.                                   
065900                                                                          
066000 01  TPO2-ORDP-PCB               PIC X.                                   
066100 01  TPO2-XXBU-PCB               PIC X.                                   
066200 01  TPO2-XXBV-PCB               PIC X.                                   
066300 01  TPO2-ARTM-PCB               PIC X.                                   
066400 01  TPO2-FILA-PCB               PIC X.                                   
066500 01  TPO2-XXBX-PCB               PIC X.                                   
066600                                                                          
066700 01  2109-PCB                    PIC X.                                   
066800 01  TPO6-ORDP-PCB               PIC X.                                   
066900 01  TPO6-XXBU-PCB               PIC X.                                   
067000 01  TPO6-XXBV-PCB               PIC X.                                   
067100 01  TPO6-XXBX-PCB               PIC X.                                   
067200 01  TPO6-ARTS-PCB               PIC X.                                   
067300                                                                          
067400 01  TIME-4437-PCB               PIC X.                                   
067500                                                                          
067600 01  AVSR-ORQI-PCB               PIC X.                                   
067700 01  AVSR-GMTB-PCB               PIC X.                                   
067800 01  AVSR-GMTC-PCB               PIC X.                                   
067900 01  AVSR-WDB2-PCB               PIC X.                                   
068000 01  AVSR-WDB6-PCB               PIC X.                                   
068100                                                                          
068200 01  TRAN-XXKB-PCB               PIC X.                                   
068300 01  KVAN-WDB2-PCB               PIC X.                                   
068400 01  PRNO-3107-PCB               PIC X.                                   
068500 01  PRQU-WDG2-PCB               PIC X.                                   
068600 01  PRQU-WDC7-PCB               PIC X.                                   
068700 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
068800                                                                          
068900     EJECT                                                                
069000 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
069100      4298-PCB 4212-PCB 4213-PCB 2191-PCB 2109-PCB PRQRY-PCB              
069200      USEA-PCB SATB-PCB                                                   
069300      ARTM-PCB WDK7-PCB BENA-PCB ORDP-PCB ORQF-PCB ORQI-PCB               
069400      ORQM-PCB 4541-PCB WDK6-PCB PROC-PCB                                 
069500      WDB6-PCB WDB2-PCB                                                   
069600      AREG-WDK6-PCB                                                       
069700      AREG-WDK7-PCB                                                       
069800      DLEV-LEVF-PCB                                                       
069900      DLEV-LEVG-PCB                                                       
070000      DLEV-LEVA-PCB                                                       
070100      DLEV-ARTS-PCB                                                       
070200      DLEV-WDB6-PCB                                                       
070300      DNOT-ORQP-PCB                                                       
070400      DNOT-ORQP2-PCB                                                      
070500      DNOT-ORQP3-PCB                                                      
070600      DNOT-4013-PCB                                                       
070700      DNOT-BENA-PCB                                                       
070800      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
070900      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
071000      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
071100      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
071200      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
071300      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
071400      TIME-4437-PCB                                                       
071500      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
071600      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
071700      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
071800      PRNO-3107-PCB                                                       
071900      PRQU-WDG2-PCB                                                       
072000      PRQU-WDC7-PCB                                                       
072100      PRQU-SJKO-WDK6-PCB.                                                 
072200 MAIN SECTION.                                                            
072300                                                                          
072400     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
072500      4298-PCB 4212-PCB 4213-PCB 2191-PCB 2109-PCB PRQRY-PCB              
072600      USEA-PCB SATB-PCB                                                   
072700      ORQM-PCB 4541-PCB WDK6-PCB PROC-PCB                                 
072800      WDB6-PCB WDB2-PCB                                                   
072900      AREG-WDK6-PCB                                                       
073000      AREG-WDK7-PCB                                                       
073100      DLEV-LEVF-PCB                                                       
073200      DLEV-LEVG-PCB                                                       
073300      DLEV-LEVA-PCB                                                       
073400      DLEV-ARTS-PCB                                                       
073500      DLEV-WDB6-PCB                                                       
073600      DNOT-ORQP-PCB                                                       
073700      DNOT-ORQP2-PCB                                                      
073800      DNOT-ORQP3-PCB                                                      
073900      DNOT-4013-PCB                                                       
074000      DNOT-BENA-PCB                                                       
074100      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
074200      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
074300      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
074400      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
074500      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
074600      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
074700      TIME-4437-PCB                                                       
074800      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
074900      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
075000      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
075100      PRNO-3107-PCB                                                       
075200      PRQU-WDG2-PCB                                                       
075300      PRQU-WDC7-PCB                                                       
075400      PRQU-SJKO-WDK6-PCB.                                                 
075500                                                                          
075600     EJECT                                                                
075700     PERFORM IMS-GET-MSG                                                  
075800     IF SEGMENT-FINNS                                                     
075900                                                                          
076000        PERFORM A-INIT                                                    
076100        PERFORM B-KOLLA-NYCKLAR                                           
076200        IF NYCKEL-OK                                                      
076300           IF MFS-NEXT                                                    
076400              PERFORM C-NAESTA-SIDA                                       
076500           ELSE                                                           
076600              PERFORM D-FOERSTA-SIDA                                      
076700           END-IF                                                         
076800           IF ALLT-OK                                                     
076900              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
077000              IF ALLT-OK                                                  
077100                 PERFORM E-SKRIVSKYDDA-NYCKLAR                            
077200                 IF (MFS-KDTRTYP = 'U' OR 'V')  OR MFS-ENTER              
077300                    PERFORM G-KONTROLLERA-BILDEN                          
077400                 END-IF                                                   
077500                 IF ALLT-OK                                               
077600                    PERFORM H-BEHANDLA-RADER                              
077700                    IF  AVSLUTA                                           
077800                       PERFORM I-STARTA-ORDERAVSLUT                       
077900                       PERFORM M-HOPPA-TILL-ORDERHUVUD-4211               
078000                    END-IF                                                
078100                 END-IF                                                   
078200              END-IF                                                      
078300           END-IF                                                         
078400        END-IF                                                            
078500        IF HOPP = NEJ                                                     
078600           PERFORM Z-FINIT                                                
078700        END-IF                                                            
078800     END-IF                                                               
078900     MOVE +0 TO RETURN-CODE                                               
079000     GOBACK                                                               
079100     .                                                                    
079200     EJECT                                                                
079300 A-INIT SECTION.                                                          
079400                                                                          
079500     MOVE SPACE                TO MED-IDMFSFEL                            
079600                                  MED-IDMFSINF                            
079700     MOVE JA                   TO ALLT-SW                                 
079800                                  NYCKEL-SW                               
079900                                                                          
080000     IF MSG-DUBBLA-TRANSKODER                                             
080100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I21301                 
080200       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
080300       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
080400     ELSE                                                                 
080500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I21301                  
080600       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
080700       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
080800     END-IF                                                               
080900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
081000     MOVE MSG-IDPFK            TO MFS-IDPFK                               
081100     MOVE MFS-IDTRANS          TO W-IDTRANS                               
081200     MOVE LOW-VALUE            TO MSG-AREA                                
081300     MOVE 'W4O213N1'           TO MFS-IDMOD                               
081400     MOVE '4'                  TO MOD-IDTRANS1                            
081500     MOVE '2'                  TO MOD-IDTRANS2                            
081600     MOVE '1'                  TO MOD-IDTRANS3                            
081700     MOVE '3'                  TO MOD-IDTRANS4                            
081800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
081900                                                                          
082000     IF NOT EGEN-MID                                                      
082100       IF W-IDTRANS = '4212' AND MFS-UPD-V                                
082200         CONTINUE                                                         
082300       ELSE                                                               
082400         MOVE SPACE            TO MFS-KDTRTYP                             
082500         MOVE '7'              TO MFS-IDPFK                               
082600       END-IF                                                             
082700     END-IF                                                               
082800     EJECT                                                                
082900     IF ENGLISH-TEXT                                                      
083000       MOVE 'GB '              TO MED-IDSKYLT                             
083100     ELSE                                                                 
083200       MOVE 'S  '              TO MED-IDSKYLT                             
083300     END-IF                                                               
083400                                                                          
083500     PERFORM AA-NOLLA-TABELLER                                            
083600                                                                          
083700     MOVE SPACE                TO   2191-MID-W2I19101                     
083800     .                                                                    
083900     EJECT                                                                
084000 AA-NOLLA-TABELLER SECTION.                                               
084100                                                                          
084200     MOVE +1                   TO WS-INDEX-WOPS                           
084300     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
084400        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
084500        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
084600        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
084700        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
084800        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
084900        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
085000        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
085100        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
085200        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
085300        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
085400        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
085500        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
085600        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
085700        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
085800        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
085900        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
086000                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
086100                                                                          
086200        ADD +1                 TO WS-INDEX-WOPS                           
086300     END-PERFORM                                                          
086400                                                                          
086500     MOVE +1                   TO WS-INDEX-WOPS                           
086600                                                                          
086700     MOVE +1                   TO WS-INDEX                                
086800     PERFORM UNTIL WS-INDEX > +14                                         
086900        MOVE ALL '+'           TO 4212-MID-RADER(WS-INDEX)                
087000        ADD +1                 TO WS-INDEX                                
087100     END-PERFORM                                                          
087200     .                                                                    
087300     EJECT                                                                
087400 B-KOLLA-NYCKLAR SECTION.                                                 
087500                                                                          
087600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
087700     MOVE '001'             TO MSGI-KDCALL                                
087800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
087900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
088000     MOVE '4213'            TO MSGI-IDTRANS                               
088100     IF GODK-MID                                                          
088200        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
088300        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
088400        IF MID-IDORDNR-IN       NOT = ALL '+'                             
088500           MOVE MID-IDORDNR-IN  TO W-SPAR-IDORDNR5                        
088600           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
088700        END-IF                                                            
088800     END-IF                                                               
088900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
089000                                                                          
089100     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
089200                                  MOD-IDKUNDNR-IN                         
089300                                  MOD-IDORDNR-IN                          
089400                                                                          
089500     IF MID-IDDISTR-IN         NOT = ALL '+'                              
089600        IF MFS-KDTRTYP         =  'V'                                     
089700           CONTINUE                                                       
089800        ELSE                                                              
089900           MOVE '7'            TO MFS-IDPFK                               
090000           MOVE SPACE          TO MFS-KDTRTYP                             
090100        END-IF                                                            
090200     END-IF                                                               
090300                                                                          
090400     IF MSGI-IDDISTR NUMERIC  AND  MSGI-IDDISTR > ZERO                    
090500        MOVE MSGI-IDDISTR        TO W-IDDISTR                             
090600     ELSE                                                                 
090700        MOVE NEJ               TO NYCKEL-SW                               
090800     END-IF                                                               
090900                                                                          
091000     MOVE W-IDDISTR            TO TEST-IDDISTR                            
091100     IF DIST79-DEALER-PRICE                                               
091200        IF ENGLISH-TEXT                                                   
091300           MOVE 'DEALERPRICE'  TO MOD-TEDDI                               
091400        ELSE                                                              
091500           MOVE '    ÅF PRIS'  TO MOD-TEDDI                               
091600        END-IF                                                            
091700     ELSE                                                                 
091800        MOVE SPACES            TO MOD-TEDDI                               
091900     END-IF                                                               
092000                                                                          
092100     IF MID-IDKUNDNR-IN        NOT = ALL '+'                              
092200        IF MFS-KDTRTYP         =  'V'                                     
092300           CONTINUE                                                       
092400        ELSE                                                              
092500           MOVE '7'            TO MFS-IDPFK                               
092600           MOVE SPACE          TO MFS-KDTRTYP                             
092700        END-IF                                                            
092800     END-IF                                                               
092900                                                                          
093000     IF MSGI-IDKUNDNR NUMERIC                                             
093100        MOVE MSGI-IDKUNDNR     TO W-IDKUNDNR                              
093200     ELSE                                                                 
093300        MOVE NEJ               TO NYCKEL-SW                               
093400     END-IF                                                               
093500     EJECT                                                                
093600     IF MID-IDORDNR-IN         NOT   = ALL '+'                            
093700        IF MFS-KDTRTYP         =  'V'                                     
093800           CONTINUE                                                       
093900        ELSE                                                              
094000          MOVE '7'             TO MFS-IDPFK                               
094100          MOVE SPACE           TO MFS-KDTRTYP                             
094200        END-IF                                                            
094300     END-IF                                                               
094400                                                                          
094500     IF MSGI-IDKUNDRF (3:5)    NUMERIC  AND                               
094600        MSGI-IDKUNDRF (3:5)    > ZERO                                     
094700        MOVE MSGI-IDKUNDRF(3:5)  TO W-IDORDNR                             
094800     ELSE                                                                 
094900        MOVE NEJ               TO NYCKEL-SW                               
095000     END-IF                                                               
095100                                                                          
095200     IF GODK-MID OR NYCKEL-OK                                             
095300        MOVE MSGI-IDKUNDNR          TO MOD-IDKUNDNR-UT                    
095400        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
095500        IF MSGI-IDKUNDNR = ZERO                                           
095600           MOVE '     0'          TO MOD-IDKUNDNR-UT                      
095700        END-IF                                                            
095800        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                       
095900        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
096000        MOVE MSGI-IDKUNDRF(3:5)   TO MOD-IDORDNR-UT                       
096100        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
096200     ELSE                                                                 
096300        MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                          
096400                                  MOD-IDKUNDNR-UT                         
096500                                  MOD-IDORDNR-UT                          
096600     END-IF                                                               
096700                                                                          
096800     IF NOT NYCKEL-OK                                                     
096900        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
097000        PERFORM MFS-RENSA-ALLA-FAELT                                      
097100     END-IF                                                               
097200     .                                                                    
097300     EJECT                                                                
097400                                                                          
097500 C-NAESTA-SIDA SECTION.                                                   
097600                                                                          
097700     IF MID-IDARTNR-NEXT NUMERIC AND                                      
097800        MID-IDARTNR-NEXT > ZERO                                           
097900        MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                        
098000        MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                        
098100        MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                       
098200        MOVE MID-IDDC-NEXT     TO W-Q1-IDDC-MIN                           
098300        MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                       
098400     ELSE                                                                 
098500        MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                            
098600        MOVE NEJ               TO ALLT-SW                                 
098700     END-IF                                                               
098800     .                                                                    
098900     SKIP2                                                                
099000 D-FOERSTA-SIDA SECTION.                                                  
099100                                                                          
099200     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
099300                                  W-Q1-IDLOPNR-MIN                        
099400                                  W-Q1-IDSEKVNR-MIN                       
099500                                  W-Q1-KDORDBEK-MIN                       
099600     MOVE SPACE                TO W-Q1-IDDC-MIN                           
099700                                                                          
099800     IF MID-IDARTNR-NEXT NUMERIC AND                                      
099900        MID-IDARTNR-NEXT > ZERO                                           
100000        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                        
100100        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                        
100200        MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                       
100300        MOVE MID-IDDC-NEXT     TO MOD-IDDC-NEXT                           
100400        MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                       
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 E-SKRIVSKYDDA-NYCKLAR SECTION.                                           
100900                                                                          
101000     MOVE MFS-CLOSE-FIELD      TO MOD-IDTRANS1-ATTR                       
101100                                  MOD-IDTRANS2-ATTR                       
101200                                  MOD-IDTRANS3-ATTR                       
101300                                  MOD-IDTRANS4-ATTR                       
101400                                  MOD-IDDISTR-IN-ATTR                     
101500                                  MOD-IDKUNDNR-IN-ATTR                    
101600                                  MOD-IDORDNR-IN-ATTR                     
101700     MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLANNULL-ATTR                       
101800     .                                                                    
101900     EJECT                                                                
102000 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
102100                                                                          
102200     PERFORM IMS-09-GU-ORQI-WDQ201                                        
102300     IF SEGMENT-FINNS                                                     
102400        IF OHUV-FLKLAR = JA                                               
102500           MOVE ERR-ORDER-AVSLUTAD                                        
102600                               TO MED-IDMFSFEL                            
102700           MOVE NEJ            TO ALLT-SW                                 
102800           MOVE NEJ            TO NYCKEL-SW                               
102900           PERFORM MFS-RENSA-ALLA-FAELT                                   
103000        ELSE                                                              
103100*RC LYNK IDSYSTEM REMAIN SO LYNK ORDERS BE COMPLETED IN 4213!             
103200           IF OHUV-IDSYSTEM     = '4211'     OR                           
103300              OHUV-IDSYSTEM     = 'LYNK'     OR                           
103400              MSG-SIGNON-USERID = 'PCCG915 ' OR                           
103400              MSG-SIGNON-USERID = 'PCCG975 ' OR                           
103500              MSG-SIGNON-USERID = 'PCCV363 ' OR                           
103600              MSG-SIGNON-USERID = 'PCCQ305 ' OR                           
103700              MSG-SIGNON-USERID = 'PCCS032 ' OR                           
103900              MSG-SIGNON-USERID = 'PC65926 ' OR                           
104100              MSG-SIGNON-USERID = 'PC06879 ' OR                           
104300              MSG-SIGNON-USERID = 'PC35634 ' OR                           
104400              MSG-SIGNON-USERID = 'PC49022 ' OR                           
104600              MSG-SIGNON-USERID = 'PC46164 ' OR                           
104700              MSG-SIGNON-USERID = 'PC51848 ' OR                           
104800              MSG-SIGNON-USERID = 'PC33574 ' OR                           
104800              MSG-SIGNON-USERID = 'PCCW991 '                              
104900              MOVE OHUV-KDORDKL TO MOD-KDORDKL-UT                         
105000              MOVE OHUV-IDORDER TO W-Q1-IDORDER-UNIK                      
105100                                   W-Q1-IDORDER-MIN                       
105200                                   W-Q1-IDORDER-MAX                       
105300                                   W-Q1-IDORDER-MIN1                      
105400                                   W-Q1-IDORDER-MAX1                      
105500           ELSE                                                           
105600              MOVE ERR-FEL-BILDSERIE                                      
105700                                  TO MED-IDMFSFEL                         
105800              MOVE NEJ            TO ALLT-SW                              
105900              MOVE NEJ            TO NYCKEL-SW                            
106000              PERFORM MFS-RENSA-ALLA-FAELT                                
106100           STRING OHUV-IDSYSTEM                                           
106200           DELIMITED BY SIZE INTO MOD-TEMFSINF                            
106300           END-IF                                                         
106400        END-IF                                                            
106500     ELSE                                                                 
106600        MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                            
106700        MOVE NEJ               TO ALLT-SW                                 
106800        MOVE NEJ               TO NYCKEL-SW                               
106900        PERFORM MFS-RENSA-ALLA-FAELT                                      
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107300 G-KONTROLLERA-BILDEN SECTION.                                            
107400                                                                          
107500     MOVE JA                   TO ALLT-SW                                 
107600                                                                          
107700     IF MID-FLANNULL NOT = '+'                                            
107800        IF MID-FLANNULL = 'J' OR 'Y' OR 'N'                               
107900           IF MID-FLANNULL = 'J' OR 'Y'                                   
108000              IF OHUV-KDTPOTYP > +0 OR                                    
108100                 OHUV-KVORDTIL > +0                                       
108200                 MOVE NEJ                TO ALLT-SW                       
108300                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR             
108400                 MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL                  
108500              ELSE                                                        
108600                 MOVE W-IDDISTR  TO W-A5-IDDISTR-MIN                      
108700                                    W-A5-IDDISTR-MAX                      
108800                 MOVE W-IDKUNDNR TO W-A5-IDKUNDNR-MIN                     
108900                                    W-A5-IDKUNDNR-MAX                     
109000                 MOVE MSGI-IDKUNDRF(3:5) TO W-A5-IDKUNDRF-MIN             
109100                                            W-A5-IDKUNDRF-MAX             
109200                 PERFORM IMS-24-GU-ORDP-WDA501                            
109300                 IF SEGMENT-FINNS                                         
109400                    MOVE NEJ                TO ALLT-SW                    
109500                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR          
109600                    MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL               
109700                 END-IF                                                   
109800              END-IF                                                      
109900              IF MID-FLANNULL = 'Y'                                       
110000                 MOVE JA          TO MID-FLANNULL                         
110100              END-IF                                                      
110200           END-IF                                                         
110300        ELSE                                                              
110400           MOVE NEJ                TO ALLT-SW                             
110500           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR                   
110600           MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                        
110700        END-IF                                                            
110800     ELSE                                                                 
110900        MOVE NEJ               TO MID-FLANNULL                            
111000     END-IF                                                               
111100     EJECT                                                                
111200                                                                          
111300     PERFORM GA-KONTROLLERA-KDBEHX                                        
111400     IF ALLT-OK                                                           
111500        PERFORM GB-KONTROLLERA-SAMBAND                                    
111600        IF ALLT-OK                                                        
111700           PERFORM GC-JUSTERA-KDBEHX                                      
111800        END-IF                                                            
111900     END-IF                                                               
112000     .                                                                    
112100     EJECT                                                                
112200                                                                          
112300 GA-KONTROLLERA-KDBEHX SECTION.                                           
112400                                                                          
112500     MOVE +1                   TO WS-INDEX-MID                            
112600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
112700                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
112800        IF MID-KDBEHX(WS-INDEX-MID) = '+'                                 
112900           MOVE SPACE          TO MID-KDBEHX(WS-INDEX-MID)                
113000        END-IF                                                            
113100        MOVE MID-RAD(WS-INDEX-MID)                                        
113200                               TO WS-AKTUELL-MID-RAD                      
113300        IF WS-AKT-KDBEHX = 'B' OR 'D' OR 'A' OR 'X'                       
113400                               OR '1' OR '2' OR ' '                       
113500           EVALUATE WS-AKT-KDBEHX                                         
113600              WHEN 'A'                                                    
113700                 IF WS-AKT-KDORDBEK = 61                                  
113800                    CONTINUE                                              
113900                 ELSE                                                     
114000                    MOVE MFS-ALFA-FAELT-FEL                               
114100                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
114200                    MOVE NEJ   TO ALLT-SW                                 
114300                 END-IF                                                   
114400              WHEN 'B'                                                    
114500                 IF WS-AKT-KDORDBEK = 21  OR 51  OR 52  OR 53  OR         
114600                        54  OR 55  OR 57  OR 58  OR 59  OR 66  OR         
114700                        67  OR 72  OR 73  OR 74  OR 75  OR                
114800                        76  OR 80  OR 81  OR 82  OR 85  OR                
114900                     ((WS-AKT-KDORDBEK = 41 OR 61) AND                    
115000                      (WS-AKT-IDARTNR  = WS-AKT-IDARTNR-URS))             
115100                    CONTINUE                                              
115200                 ELSE                                                     
115300                    MOVE MFS-ALFA-FAELT-FEL                               
115400                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
115500                    MOVE NEJ   TO ALLT-SW                                 
115600                 END-IF                                                   
115700     EJECT                                                                
115800              WHEN 'D'                                                    
115900                 IF WS-AKT-IDLOPNR-RO = +0                                
116000                    IF WS-AKT-KDORDBEK = 15  OR 16  OR 41                 
116100                         OR 43 OR 44 OR 70  OR 71 OR 92  OR 95            
116200                         OR 98 OR 99                                      
116300                       CONTINUE                                           
116400                    ELSE                                                  
116500                      MOVE MFS-ALFA-FAELT-FEL                             
116600                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
116700                      MOVE NEJ TO ALLT-SW                                 
116800                    END-IF                                                
116900                 ELSE                                                     
117000                    IF WS-AKT-KDORDBEK = 41 OR 43 OR 44                   
117100                                            OR 70 OR 92 OR 98             
117200                                            OR 99                         
117300                       CONTINUE                                           
117400                    ELSE                                                  
117500                      MOVE MFS-ALFA-FAELT-FEL                             
117600                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
117700                      MOVE NEJ TO ALLT-SW                                 
117800                    END-IF                                                
117900                 END-IF                                                   
118000              WHEN 'X'                                                    
118100                 IF WS-AKT-KDORDBEK NOT = 10                              
118200                    MOVE MFS-ALFA-FAELT-FEL                               
118300                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
118400                    MOVE NEJ   TO ALLT-SW                                 
118500                 END-IF                                                   
118600              WHEN '1'                                                    
118700                 IF WS-AKT-KDORDBEK = 43  OR 44                           
118800                    CONTINUE                                              
118900                 ELSE                                                     
119000                    MOVE MFS-ALFA-FAELT-FEL                               
119100                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
119200                    MOVE NEJ   TO ALLT-SW                                 
119300                 END-IF                                                   
119400     EJECT                                                                
119500              WHEN '2'                                                    
119600                 IF WS-AKT-KDORDBEK = 43  OR 44                           
119700                    CONTINUE                                              
119800                 ELSE                                                     
119900                    MOVE MFS-ALFA-FAELT-FEL                               
120000                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
120100                    MOVE NEJ   TO ALLT-SW                                 
120200                 END-IF                                                   
120300              WHEN OTHER                                                  
120400                 IF WS-AKT-KDORDBEK = 10 OR 15 OR 16 OR 41                
120500                                   OR 43 OR 44 OR 56 OR 61                
120600                                   OR 70 OR 71 OR 26                      
120700                                   OR 74 OR 92 OR 95 OR 98 OR 99          
120800                    CONTINUE                                              
120900                 ELSE                                                     
121000                    MOVE MFS-ALFA-FAELT-FEL                               
121100                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
121200                    MOVE NEJ   TO ALLT-SW                                 
121300                 END-IF                                                   
121400           END-EVALUATE                                                   
121500        ELSE                                                              
121600           MOVE MFS-ALFA-FAELT-FEL                                        
121700                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
121800           MOVE NEJ            TO ALLT-SW                                 
121900        END-IF                                                            
122000                                                                          
122100        ADD +1                 TO WS-INDEX-MID                            
122200     END-PERFORM                                                          
122300     IF NOT ALLT-OK                                                       
122400        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800                                                                          
122900 GB-KONTROLLERA-SAMBAND SECTION.                                          
123000                                                                          
123100     MOVE +1                   TO WS-INDEX-MID                            
123200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
123300                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
123400        MOVE MID-RAD(WS-INDEX-MID)                                        
123500                               TO WS-AKTUELL-MID-RAD                      
123600        IF WS-AKT-KDBEHX = 'D'                                            
123700           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
123800           ADD +1              TO WS-INDEX-MID                            
123900           IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                         
124000              MOVE MID-RAD(WS-INDEX-MID)                                  
124100                               TO WS-AKTUELL-MID-RAD                      
124200           END-IF                                                         
124300           PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR               
124400                         WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR          
124500                         WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR          
124600                     WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR         
124700              IF WS-AKT-KDBEHX = '1' OR '2'                               
124800                 MOVE NEJ      TO ALLT-SW                                 
124900                 MOVE MFS-ALFA-FAELT-FEL                                  
125000                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
125100                 MOVE ERR-UPPLYSTA-FEL                                    
125200                               TO MED-IDMFSFEL                            
125300              END-IF                                                      
125400              ADD +1           TO WS-INDEX-MID                            
125500              IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                      
125600                 MOVE MID-RAD(WS-INDEX-MID)                               
125700                               TO WS-AKTUELL-MID-RAD                      
125800              END-IF                                                      
125900           END-PERFORM                                                    
126000        ELSE                                                              
126100           ADD +1              TO WS-INDEX-MID                            
126200        END-IF                                                            
126300     END-PERFORM                                                          
126400     .                                                                    
126500     EJECT                                                                
126600                                                                          
126700 GC-JUSTERA-KDBEHX SECTION.                                               
126800                                                                          
126900*    JUSTERINGEN GÖRS FÖR ATT VARJE RAD SENARE I PROGRAMMET               
127000*    SKALL KUNNA BEHANDLAS VAR FÖR SIG.                                   
127100*    BORTTAG AV ORDERBEKRÄFTELSER SKULLE ANNARS BEHÖVA GÖRAS              
127200*    I MÅNGA SEKTIONER. PÅ DETTA SÄTT KOMMER ALLA BORTTAG                 
127300*    ATT GÖRAS I HEA-ANNULLERA-RAD.                                       
127400                                                                          
127500     MOVE +1                   TO WS-INDEX-MID                            
127600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX   OR                   
127700                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
127800        MOVE MID-RAD(WS-INDEX-MID)                                        
127900                               TO WS-AKTUELL-MID-RAD                      
128000        IF WS-AKT-KDBEHX = '1' OR '2'                                     
128100           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
128200           PERFORM GCA-D-MARKERA-1-2-RADER                                
128300        ELSE                                                              
128400           IF WS-AKT-KDBEHX = 'X'                                         
128500              IF OHUV-KDTPOTYP > +0                                       
128600                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
128700                 PERFORM GCB-D-MARKERA-RADER                              
128800              ELSE                                                        
128900                 ADD +1        TO WS-INDEX-MID                            
129000              END-IF                                                      
129100           ELSE                                                           
129200              IF WS-AKT-KDBEHX = 'D'                                      
129300                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
129400                 PERFORM GCB-D-MARKERA-RADER                              
129500              ELSE                                                        
129600                 ADD +1        TO WS-INDEX-MID                            
129700              END-IF                                                      
129800           END-IF                                                         
129900        END-IF                                                            
130000     END-PERFORM                                                          
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 GCA-D-MARKERA-1-2-RADER SECTION.                                         
130500                                                                          
130600     ADD +1                    TO WS-INDEX-MID                            
130700     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
130800        MOVE MID-RAD(WS-INDEX-MID)                                        
130900                               TO WS-AKTUELL-MID-RAD                      
131000     END-IF                                                               
131100     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
131200                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
131300                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
131400               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
131500                                                                          
131600        IF WS-AKT-KDBEHX = ' '                                            
131700           IF WS-AKT-KDORDBEK = 41                                        
131800              CONTINUE                                                    
131900           ELSE                                                           
132000              MOVE 'D'         TO MID-KDBEHX(WS-INDEX-MID)                
132100           END-IF                                                         
132200        END-IF                                                            
132300        ADD +1                 TO WS-INDEX-MID                            
132400        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
132500           MOVE MID-RAD(WS-INDEX-MID)                                     
132600                               TO WS-AKTUELL-MID-RAD                      
132700        END-IF                                                            
132800     END-PERFORM                                                          
132900     .                                                                    
133000     EJECT                                                                
133100                                                                          
133200 GCB-D-MARKERA-RADER SECTION.                                             
133300                                                                          
133400     ADD +1                    TO WS-INDEX-MID                            
133500     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
133600        MOVE MID-RAD(WS-INDEX-MID)                                        
133700                               TO WS-AKTUELL-MID-RAD                      
133800     END-IF                                                               
133900     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
134000                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
134100                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
134200               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
134300                                                                          
134400        IF WS-AKT-KDBEHX = ' '  AND                                       
134500                 (WS-AKT-IDDC = WS-IDDC-SPAR)                             
134600                                                                          
134700           MOVE 'D'            TO MID-KDBEHX(WS-INDEX-MID)                
134800        END-IF                                                            
134900        ADD +1                 TO WS-INDEX-MID                            
135000        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
135100           MOVE MID-RAD(WS-INDEX-MID)                                     
135200                               TO WS-AKTUELL-MID-RAD                      
135300        END-IF                                                            
135400     END-PERFORM                                                          
135500     .                                                                    
135600     EJECT                                                                
135700 H-BEHANDLA-RADER SECTION.                                                
135800                                                                          
135900     IF MFS-FIRST  OR  MFS-NEXT                                           
136000                                                                          
136100        PERFORM HB-LAS-IN-13-RADER                                        
136200        IF  WS-INDEX-MOD = +1  AND (W-IDTRANS = '4212' OR                 
136300                                                '4297' OR                 
136400                                                '4206')                   
136500           MOVE JA             TO AVSLUTA-SW                              
136600        END-IF                                                            
136700     ELSE                                                                 
136800        IF MID-FLANNULL NOT = JA                                          
136900           PERFORM HA-SKAPA-SPAR-ARB                                      
137000                                                                          
137100           IF MFS-UPDATE  OR  MFS-QUERY                                   
137200              PERFORM HE-UPPDATERA-AKT-SIDA                               
137300              PERFORM HD-UPPDATERA-OBEH-RADER                             
137400              IF START-4212                                               
137500                MOVE 'U'         TO 4212-MID-KDTRTYP                      
137600                PERFORM S03-STARTA-RADBEHANDLINGEN                        
137700              ELSE                                                        
137800                PERFORM HB-LAS-IN-13-RADER                                
137900                IF  WS-INDEX-MOD = +1                                     
138000                   MOVE JA       TO AVSLUTA-SW                            
138100                END-IF                                                    
138200              END-IF                                                      
138300           ELSE                                                           
138400              IF MFS-UPD-V                                                
138500                 PERFORM HE-UPPDATERA-AKT-SIDA                            
138600                 PERFORM HG-UPPDATERA-RESTERANDE-RADER                    
138700              END-IF                                                      
138800           END-IF                                                         
138900        ELSE                                                              
139000           PERFORM HH-ANNULLERA-ORDER                                     
139100        END-IF                                                            
139200     END-IF                                                               
139300     .                                                                    
139400     EJECT                                                                
139500                                                                          
139600 HA-SKAPA-SPAR-ARB SECTION.                                               
139700                                                                          
139800     PERFORM IMS-14-GNP-ORQI-WDQ212                                       
139900                                                                          
140000     MOVE ARB-KDFRAKT  TO SPAR-ARB-KDFRAKT                                
140100     MOVE ARB-KDFDKRAV TO SPAR-ARB-KDFDKRAV                               
140200     MOVE ARB-KDROPACK TO SPAR-ARB-KDROPACK                               
140300     .                                                                    
140400     EJECT                                                                
140500 HB-LAS-IN-13-RADER SECTION.                                              
140600                                                                          
140700     PERFORM MFS-RENSA-ALLA-FAELT                                         
140800     MOVE +0                   TO WS-IDARTNR-SPAR                         
140900                                  WS-IDLOPNR-SPAR                         
141000                                                                          
141100     PERFORM HBA-VISA-OBKR-OCH-SATS-RADER                                 
141200                                                                          
141300     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
141400                                                                          
141500     IF WS-INDEX-MOD > WS-INDEX-MOD-MAX                                   
141600                  AND                                                     
141700           OBKR-SEGMENT-FINNS                                             
141800                  AND                                                     
141900          (OBKR-KDORDBEK = 41 OR 61)                                      
142000                                                                          
142100        PERFORM HBC-KONTROLLERA-SIDSLUT                                   
142200     END-IF                                                               
142300     .                                                                    
142400     EJECT                                                                
142500                                                                          
142600 HBA-VISA-OBKR-OCH-SATS-RADER SECTION.                                    
142700                                                                          
142800     MOVE +1                   TO WS-INDEX-MOD                            
142900                                                                          
143000     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
143100     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
143200              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
143300                                                                          
143400        PERFORM HBAA-REDIGERA-ORDERBEKR-RAD                               
143500                                                                          
143600        IF OBKR-KDORDBEK = 57                                             
143700           PERFORM HBAB-VISA-SATS-ARTIKLAR                                
143800        END-IF                                                            
143900        MOVE OBKR-IDARTNR   TO WS-IDARTNR-SPAR                            
144000        MOVE OBKR-IDLOPNR   TO WS-IDLOPNR-SPAR                            
144100                                                                          
144200        ADD +1              TO WS-INDEX-MOD                               
144300                                                                          
144400        IF NOT NEXT-SATS                                                  
144500           PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                         
144600*----------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN              
144700           MOVE STATUS-WS      TO STATUS-OBKR-WS                          
144800        END-IF                                                            
144900     END-PERFORM                                                          
145000     .                                                                    
145100     EJECT                                                                
145200                                                                          
145300 HBAA-REDIGERA-ORDERBEKR-RAD SECTION.                                     
145400                                                                          
145500     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
145600     IF OBKR-KDORDBEK = 41 OR 61                                          
145700        MOVE '*'               TO MOD-ASTERIX(WS-INDEX-MOD)               
145800     END-IF                                                               
145900                                                                          
146000     MOVE SPACE                TO MOD-KDBEHX(WS-INDEX-MOD)                
146100     IF OBKR-KDORDBEK = 21                                                
146200                     OR 51 OR 52 OR 53 OR 54 OR 55 OR 57 OR 58            
146300                     OR 59 OR 66 OR 67 OR 72 OR 73 OR 74 OR 75            
146400                     OR 76 OR 80 OR 81 OR 82 OR 85                        
146500        MOVE 'B'               TO MOD-KDBEHX(WS-INDEX-MOD)                
146600        MOVE MFS-STAENG-FAELT  TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
146700     END-IF                                                               
146800     IF OBKR-KDORDBEK = 41 OR 61                                          
146900        IF OBKR-IDARTNR = WS-IDARTNR-SPAR AND                             
147000                 OBKR-IDLOPNR = WS-IDLOPNR-SPAR                           
147100           MOVE SPACE          TO MOD-KDBEHX(WS-INDEX-MOD)                
147200           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
147300                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
147400        ELSE                                                              
147500           MOVE 'B'              TO MOD-KDBEHX(WS-INDEX-MOD)              
147600           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)         
147700        END-IF                                                            
147800     END-IF                                                               
147900     IF (OBKR-KDORDBEK = 61) AND                                          
148000                         OBKR-IDARTNR-TILLK > +0                          
148100        MOVE 'A'               TO MOD-KDBEHX(WS-INDEX-MOD)                
148200     END-IF                                                               
148300     EJECT                                                                
148400                                                                          
148500     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
148600     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
148700     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
148800                                                                          
148900     PERFORM S22-HAMTA-BENAMNING                                          
149000                                                                          
149100     MOVE OBKR-IDDC            TO MOD-IDDC-RAD(WS-INDEX-MOD)              
149200                                                                          
149300     IF OBKR-KDORDBEK = 10 OR 15 OR 16 OR 43 OR 44 OR 56 OR               
149400                        70 OR 71 OR 74 OR 95                              
149500        MOVE OBKR-KVBEART-Q    TO MOD-KVANTAL(WS-INDEX-MOD)               
149600     ELSE                                                                 
149700        IF OBKR-KDORDBEK = 80 OR 85                                       
149800           MOVE OBKR-KVANNANT  TO MOD-KVANTAL(WS-INDEX-MOD)               
149900        ELSE                                                              
150000           IF OBKR-KDORDBEK = 92 OR 98 OR 99                              
150100              MOVE OBKR-KVPRERO TO MOD-KVANTAL(WS-INDEX-MOD)              
150200           ELSE                                                           
150300              MOVE OBKR-KVBEART TO MOD-KVANTAL(WS-INDEX-MOD)              
150400           END-IF                                                         
150500        END-IF                                                            
150600     END-IF                                                               
150700                                                                          
150800     IF OBKR-KDORDBEK = 43 OR 44                                          
150900        MOVE OBKR-KVQPACK      TO MOD-KVQPACK(WS-INDEX-MOD)               
151000     ELSE                                                                 
151100        MOVE +0                TO MOD-KVQPACK(WS-INDEX-MOD)               
151200     END-IF                                                               
151300     EJECT                                                                
151400                                                                          
151500     IF OBKR-KDORDBEK = 10                                                
151600        MOVE OBKR-IDKUNDRF-RO (1:7)                                       
151700                               TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
151800     ELSE                                                                 
151900        MOVE +0                TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
152000     END-IF                                                               
152100                                                                          
152200     IF OBKR-KDORDBEK = 15 OR 16 OR 21                                    
152300                     OR 43 OR 44 OR 52 OR 53 OR 54 OR 55                  
152400                     OR 56 OR 57 OR 70 OR 72 OR 73 OR 74                  
152500                     OR 75 OR 76 OR 80 OR 95 OR 99 OR 58 OR 92            
152600                     OR 66 OR 71 OR 98                                    
152700        IF OBKR-IDARTNR-TILLK > 0                                         
152800           MOVE OBKR-IDARTNR-TILLK                                        
152900                               TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
153000           MOVE '-'            TO MOD-STRAEK(WS-INDEX-MOD)                
153100           MOVE OBKR-REKSIFFR-TILLK                                       
153200                               TO MOD-REKSIFFR(WS-INDEX-MOD)              
153300        END-IF                                                            
153400     ELSE                                                                 
153500        IF OBKR-KDORDBEK = 41 OR 61                                       
153600*------ ERSATT ARTIKEL                                                    
153700                                                                          
153800           PERFORM HBAAA-FIXA-ERSATNING-RAD                               
153900        END-IF                                                            
154000     END-IF                                                               
154100                                                                          
154200     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
154300     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
154400     MOVE OBKR-IDARTNR         TO WS-AKT-IDARTNR-URS                      
154500     MOVE OBKR-IDLOPNR-RO      TO WS-AKT-IDLOPNR-RO                       
154600                                                                          
154700     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
154800     .                                                                    
154900     EJECT                                                                
155000 HBAAA-FIXA-ERSATNING-RAD SECTION.                                        
155100                                                                          
155200     IF (OBKR-IDARTNR NOT = WS-IDARTNR-SPAR)     OR                       
155300           (OBKR-IDARTNR = WS-IDARTNR-SPAR  AND                           
155400              OBKR-IDLOPNR NOT = WS-IDLOPNR-SPAR)                         
155500        PERFORM S22-HAMTA-BENAMNING                                       
155600        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
155700                      TO MOD-IDDC-ATTR(WS-INDEX-MOD)                      
155800                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
155900                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
156000                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
156100     ELSE                                                                 
156200        IF OBKR-IDARTNR-TILLK = +0                                        
156300*------ TILLKOMMANDE TEXT                                                 
156400           MOVE MFS-RENSA-FAELT                                           
156500                           TO MOD-IDARTNR(WS-INDEX-MOD)                   
156600           MOVE OBKR-BEERS TO MOD-BEART(WS-INDEX-MOD)                     
156700           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
156800                      TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)                    
156900                         MOD-IDDC-ATTR(WS-INDEX-MOD)                      
157000                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
157100                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
157200                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
157300        ELSE                                                              
157400*-------TILLKOMMANDE ARTIKEL                                              
157500           MOVE OBKR-IDARTNR-TILLK                                        
157600                         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)                
157700           MOVE '-'      TO MOD-STRAEK(WS-INDEX-MOD)                      
157800           MOVE OBKR-REKSIFFR-TILLK                                       
157900                         TO MOD-REKSIFFR(WS-INDEX-MOD)                    
158000           PERFORM S22-HAMTA-BENAMNING                                    
158100           MOVE OBKR-KVBEART-TILLK                                        
158200                         TO MOD-KVANTAL(WS-INDEX-MOD)                     
158300           MOVE OBKR-DIERS-KVOT                                           
158400                         TO MOD-KVQPACK(WS-INDEX-MOD)                     
158500        END-IF                                                            
158600     END-IF                                                               
158700     .                                                                    
158800     EJECT                                                                
158900 HBAB-VISA-SATS-ARTIKLAR SECTION.                                         
159000                                                                          
159100     MOVE NEJ                  TO NEXT-SATS-SW                            
159200                                                                          
159300     MOVE +1                   TO WS-INDEX-SATS                           
159400     PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
159500        MOVE SPACE             TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
159600        ADD +1                 TO WS-INDEX-SATS                           
159700     END-PERFORM                                                          
159800                                                                          
159900     MOVE +1                   TO WS-INDEX-SATS                           
160000                                  WS-INDEX                                
160100     MOVE OBKR-IDARTNR         TO W-J1-IDARTNR                            
160200                                                                          
160300     PERFORM IMS-18A-GU-SATB-WDJ111-01                                    
160400     PERFORM UNTIL SEGMENT-SAKNAS                                         
160500             OR    WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
160600             OR    WS-INDEX  > +10                                        
160700                                                                          
160800        MOVE RAD-TISTADAT    TO TMP1-YYMMDD                               
160900        MOVE RAD-TISTODAT    TO TMP2-YYMMDD                               
161000        MOVE MSGI-TILOKDAT   TO TMP3-YYMMDD                               
161100        PERFORM WY2000Q1                                                  
161200        IF STR-IDARTNR < +100000000 AND STR-TIBORT = +0 AND               
161300           TMP1-YYMMDD NOT > TMP3-YYMMDD AND                              
161400           TMP2-YYMMDD NOT < TMP3-YYMMDD                                  
161500                                                                          
161600           IF WS-INDEX-MOD = WS-INDEX-MOD-MAX                             
161700              MOVE JA             TO NEXT-SATS-SW                         
161800              MOVE +6             TO WS-INDEX-SATS                        
161900           ELSE                                                           
162000              MOVE STR-IDARTNR TO WS-NUM-9                                
162100              MOVE WS-ALFA-9   TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
162200              ADD +1           TO WS-INDEX-SATS                           
162300           END-IF                                                         
162400        END-IF                                                            
162500        IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                          
162600           PERFORM IMS-18-GN-SATB-WDJ111-01                               
162700           ADD  +1             TO WS-INDEX                                
162800        END-IF                                                            
162900     END-PERFORM                                                          
163000     EJECT                                                                
163100     IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                             
163200                                                                          
163300        COMPUTE WS-INDEX = WS-INDEX-MOD + WS-INDEX-SATS - 1               
163400        IF WS-INDEX NOT > WS-INDEX-MOD-MAX                                
163500                                                                          
163600           MOVE +1       TO WS-INDEX-SATS                                 
163700           PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                
163800                 OR WS-IDARTNR-SATS(WS-INDEX-SATS) = SPACE                
163900                                                                          
164000              ADD +1     TO WS-INDEX-MOD                                  
164100              PERFORM HBABA-REDIGERA-SATS-RAD                             
164200              ADD +1     TO WS-INDEX-SATS                                 
164300           END-PERFORM                                                    
164400        ELSE                                                              
164500           MOVE JA       TO NEXT-SATS-SW                                  
164600        END-IF                                                            
164700     END-IF                                                               
164800     IF NEXT-SATS                                                         
164900*-----OM EJ ALLA SATS-ART FÅR PLATS PÅ SIDAN RADERAS RAD MED              
165000*-----ORDBEK = 57 OCH DEN SPARAS FÖR NÄSTA SIDA(I HBB-SECTIONEN)          
165100        MOVE MFS-RENSA-FAELT   TO MOD-KDORDBEK(WS-INDEX-MOD)              
165200                                  MOD-KDBEHX(WS-INDEX-MOD)                
165300                                  MOD-IDARTNR(WS-INDEX-MOD)               
165400                                  MOD-BEART(WS-INDEX-MOD)                 
165500                                  MOD-IDDC-RAD(WS-INDEX-MOD)              
165600                                  MOD-KVANTAL(WS-INDEX-MOD)               
165700                                  MOD-KVQPACK(WS-INDEX-MOD)               
165800                                  MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
165900        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
166000        MOVE MED-UPPLYSN-UPPDAT-PF                                        
166100                               TO MED-IDMFSFEL                            
166200     END-IF                                                               
166300     .                                                                    
166400     EJECT                                                                
166500                                                                          
166600 HBABA-REDIGERA-SATS-RAD SECTION.                                         
166700                                                                          
166800     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
166900     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
167000                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
167100                                                                          
167200     MOVE SPACE                TO MOD-ASTERIX(WS-INDEX-MOD)               
167300     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
167400     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(WS-INDEX-MOD)               
167500                                                                          
167600     MOVE WS-IDARTNR-SATS(WS-INDEX-SATS)                                  
167700                               TO MOD-BEART(WS-INDEX-MOD)                 
167800     INSPECT MOD-BEART(WS-INDEX-MOD) REPLACING LEADING ZERO BY            
167900                                                         SPACE            
168000     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
168100                           TO MOD-IDDC-ATTR(WS-INDEX-MOD)                 
168200                              MOD-KVANTAL-ATTR(WS-INDEX-MOD)              
168300                              MOD-KVQPACK-ATTR(WS-INDEX-MOD)              
168400                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)          
168500                                                                          
168600     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
168700     .                                                                    
168800     EJECT                                                                
168900                                                                          
169000 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
169100                                                                          
169200     IF OBKR-SEGMENT-FINNS                                                
169300        MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                        
169400        MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                        
169500        MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                       
169600        MOVE OBKR-IDDC         TO MOD-IDDC-NEXT                           
169700        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                       
169800                                                                          
169900        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
170000        MOVE MED-UPPLYSN-UPPDAT-PF                                        
170100                               TO MED-IDMFSFEL                            
170200     ELSE                                                                 
170300        MOVE ZERO              TO MOD-IDARTNR-NEXT                        
170400                                  MOD-IDLOPNR-NEXT                        
170500                                  MOD-IDSEKVNR-NEXT                       
170600                                  MOD-IDDC-NEXT                           
170700                                  MOD-KDORDBEK-NEXT                       
170800     END-IF                                                               
170900     .                                                                    
171000     EJECT                                                                
171100                                                                          
171200 HBC-KONTROLLERA-SIDSLUT SECTION.                                         
171300                                                                          
171400     MOVE WS-INDEX-MOD-MAX     TO WS-INDEX-MOD                            
171500     MOVE MOD-KEYS(WS-INDEX-MOD)                                          
171600                               TO WS-AKT-KEYS                             
171700     IF OBKR-IDARTNR = WS-AKT-IDARTNR-URS AND                             
171800        OBKR-IDLOPNR = WS-AKT-IDLOPNR                                     
171900                                                                          
172000        PERFORM UNTIL WS-INDEX-MOD = +1 OR                                
172100                     (OBKR-IDARTNR NOT = WS-AKT-IDARTNR-URS OR            
172200                      OBKR-IDLOPNR NOT = WS-AKT-IDLOPNR)                  
172300                                                                          
172400           SUBTRACT 1 FROM WS-INDEX-MOD                                   
172500           MOVE MOD-KEYS(WS-INDEX-MOD)                                    
172600                               TO WS-AKT-KEYS                             
172700        END-PERFORM                                                       
172800                                                                          
172900        ADD +1 TO WS-INDEX-MOD                                            
173000                                                                          
173100*---- BLÄDDRINGSVÄRDENA MÅSTE JUSTERAS OM NÄR VI BACKAR RADER             
173200                                                                          
173300        MOVE MOD-KEYS(WS-INDEX-MOD)                                       
173400                                TO WS-AKT-KEYS                            
173500        MOVE WS-AKT-IDARTNR-URS TO MOD-IDARTNR-NEXT                       
173600        MOVE WS-AKT-IDLOPNR     TO MOD-IDLOPNR-NEXT                       
173700        MOVE WS-AKT-IDSEKVNR    TO MOD-IDSEKVNR-NEXT                      
173800        MOVE MOD-IDDC-RAD(WS-INDEX-MOD)                                   
173900                                TO MOD-IDDC-NEXT                          
174000        MOVE MOD-KDORDBEK(WS-INDEX-MOD)                                   
174100                                TO MOD-KDORDBEK-NEXT                      
174200     EJECT                                                                
174300                                                                          
174400        MOVE WS-INDEX-MOD      TO WS-INDEX                                
174500        PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                         
174600          MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
174700                                  MOD-ASTERIX(WS-INDEX)                   
174800                                  MOD-KDBEHX(WS-INDEX)                    
174900                                  MOD-IDARTNR(WS-INDEX)                   
175000                                  MOD-BEART(WS-INDEX)                     
175100                                  MOD-IDDC-RAD(WS-INDEX)                  
175200                                  MOD-KVANTAL(WS-INDEX)                   
175300                                  MOD-KVQPACK(WS-INDEX)                   
175400                                  MOD-IDKUNDRF-RO(WS-INDEX)               
175500                                  MOD-KEYS(WS-INDEX)                      
175600          ADD +1               TO WS-INDEX                                
175700        END-PERFORM                                                       
175800     ELSE                                                                 
175900        ADD +1                 TO WS-INDEX-MOD                            
176000     END-IF                                                               
176100                                                                          
176200     MOVE MED-FLER-SIDOR       TO MED-IDMFSINF                            
176300     MOVE MED-UPPLYSN-UPPDAT-PF                                           
176400                               TO MED-IDMFSFEL                            
176500     .                                                                    
176600     EJECT                                                                
176700 HD-UPPDATERA-OBEH-RADER SECTION.                                         
176800                                                                          
176900     MOVE NEJ                  TO AKT-SIDA-SW                             
177000                                                                          
177100     MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                      
177200     MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-MAX                        
177300     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
177400     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
177500     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
177600     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
177700                                                                          
177800     PERFORM IMS-01-GHU-ORQM-WDQ101-FOERE                                 
177900     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
178000                                                                          
178100        PERFORM S02-GODKANN-RAD                                           
178200                                                                          
178300        PERFORM IMS-02-GHN-ORQM-WDQ101-FOERE                              
178400     END-PERFORM                                                          
178500     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
178600                                  W-Q1-IDLOPNR-MAX                        
178700                                  W-Q1-IDSEKVNR-MAX                       
178800                                  W-Q1-IDDC-MAX                           
178900                                  W-Q1-KDORDBEK-MAX                       
179000     .                                                                    
179100     EJECT                                                                
179200 HE-UPPDATERA-AKT-SIDA SECTION.                                           
179300                                                                          
179400     MOVE JA                   TO AKT-SIDA-SW                             
179500     MOVE +1 TO WS-INDEX-MID                                              
179600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
179700                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
179800       MOVE MID-RAD(WS-INDEX-MID)                                         
179900                             TO WS-AKTUELL-MID-RAD                        
180000       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-UNIK                     
180100       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
180200       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
180300       MOVE WS-AKT-IDDC          TO W-Q1-IDDC-UNIK                        
180400       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
180500                                                                          
180600       PERFORM IMS-05-GHU-ORQM-WDQ101-UNIK                                
180700       IF SEGMENT-FINNS                                                   
180800         IF MID-IDARTNR(WS-INDEX-MID) = ZERO                              
180900           MOVE JA       TO OBKR-FLOBOK                                   
181000           PERFORM IMS-06-REPL-ORQM-WDQ101                                
181100         ELSE                                                             
181200           IF WS-AKT-KDBEHX = SPACE                                       
181300             PERFORM S02-GODKANN-RAD                                      
181400           ELSE                                                           
181500             IF WS-AKT-KDBEHX = 'A'                                       
181600               MOVE JA       TO OBKR-FLOBOK                               
181700               PERFORM IMS-06-REPL-ORQM-WDQ101                            
181800             ELSE                                                         
181900               IF WS-AKT-KDBEHX = 'D'                                     
182000                 PERFORM HEA-ANNULLERA-RAD                                
182100               ELSE                                                       
182200     EJECT                                                                
182300                 IF WS-AKT-KDBEHX = '1' OR '2'                            
182400                   PERFORM HEA-ANNULLERA-RAD                              
182500                   IF WS-AKT-KDBEHX = '1'                                 
182600                     MOVE +1       TO OBKR-KDKVBRYT                       
182700                   ELSE                                                   
182800                     MOVE +2       TO OBKR-KDKVBRYT                       
182900                   END-IF                                                 
183000                   MOVE +0         TO OBKR-KVPREAVB                       
183100                                      OBKR-KVPRERO                        
183200                   PERFORM S01-SKRIV-MID-TILL-4212                        
183300                 ELSE                                                     
183400                   IF WS-AKT-KDBEHX = 'X'                                 
183500                     PERFORM HEB-BACKA-WDA5-STATUS                        
183600                                                                          
183700                     PERFORM HEA-ANNULLERA-RAD                            
183800                   ELSE                                                   
183900                     IF WS-AKT-KDBEHX = 'B' AND OBKR-KDORDKL = +0         
184000                       AND (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54          
184100                                               OR 55 OR 57 OR 67)         
184200                       PERFORM S11-SKRIV-VOR-RAD                          
184300                       MOVE JA TO OBKR-FLOBOK                             
184400                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
184500                     ELSE                                                 
184600                       MOVE JA TO OBKR-FLOBOK                             
184700                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
184800                     END-IF                                               
184900                   END-IF                                                 
185000                 END-IF                                                   
185100               END-IF                                                     
185200             END-IF                                                       
185300           END-IF                                                         
185400         END-IF                                                           
185500       END-IF                                                             
185600       ADD +1                  TO WS-INDEX-MID                            
185700     END-PERFORM                                                          
185800     .                                                                    
185900     EJECT                                                                
186000                                                                          
186100 HEA-ANNULLERA-RAD SECTION.                                               
186200                                                                          
186300     IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                           
186400        IF OBKR-IDLEVNR = SPACE                                           
186500                                                                          
186600           IF OBKR-IDARTNR-TILLK > +0                                     
186700              MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                        
186800           ELSE                                                           
186900              MOVE OBKR-IDARTNR       TO W-IDARTNR                        
187000           END-IF                                                         
187100                                                                          
187200           MOVE OBKR-IDDC       TO WS-IDDC                                
187300           IF WS-IDDC NOT = W-IDDC-B6                                     
187400              MOVE WS-IDDC TO W-IDDC-B6                                   
187500              PERFORM IMS-GU-WDB601                                       
187600           END-IF                                                         
187700           IF DCS-CDC                                                     
187800             PERFORM IMS-21-GHU-ARTM-WDK901                               
187900                                                                          
188000             PERFORM HEAA-BACKA-WDK9-SALDON                               
188100                                                                          
188200             PERFORM IMS-22-REPL-ARTM-WDK901                              
188300           ELSE                                                           
188400             MOVE OBKR-IDDC TO W-IDDC                                     
188500             PERFORM IMS-11-GHU-WDK711                                    
188600             IF OBKR-KDORDKL = +0 OR +1                                   
188700               SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-DAG                
188800             ELSE                                                         
188900               IF OBKR-KDORDKL = +2 OR +3 OR +4                           
189000                 SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK             
189100               END-IF                                                     
189200             END-IF                                                       
189300             PERFORM IMS-12-REPL-WDK711                                   
189400           END-IF                                                         
189500        END-IF                                                            
189600     END-IF                                                               
189700     PERFORM IMS-08-DLET-ORQM-WDQ101                                      
189800     PERFORM S25-DELETE-PRICE-Q-LINE                                      
189900     .                                                                    
190000     EJECT                                                                
190100                                                                          
190200 HEAA-BACKA-WDK9-SALDON SECTION.                                          
190300                                                                          
190400     IF OBKR-KDORDKL = +0                                                 
190500        IF OBKR-KVPREAVB > +0                                             
190600           COMPUTE ART-KVPREAVB-VOR =                                     
190700           ART-KVPREAVB-VOR - OBKR-KVPREAVB                               
190800        END-IF                                                            
190900        IF OBKR-KVPRERO > +0                                              
191000           COMPUTE ART-KVPRERO-DAG =                                      
191100           ART-KVPRERO-DAG - OBKR-KVPRERO                                 
191200        END-IF                                                            
191300        IF OHUV-IDKAMPRF = +0 AND                                         
191400           OBKR-IDKUNDRF-RO = '0000000   '                                
191500           COMPUTE ART-KVOKS-VOR =                                        
191600           ART-KVOKS-VOR - OBKR-KVBEART-Q                                 
191700        END-IF                                                            
191800     ELSE                                                                 
191900        IF OBKR-KDORDKL = +1                                              
192000           IF OBKR-KVPREAVB > +0                                          
192100              COMPUTE ART-KVPREAVB-DAG =                                  
192200              ART-KVPREAVB-DAG - OBKR-KVPREAVB                            
192300           END-IF                                                         
192400           IF OBKR-KVPRERO > +0                                           
192500              COMPUTE ART-KVPRERO-DAG =                                   
192600              ART-KVPRERO-DAG - OBKR-KVPRERO                              
192700           END-IF                                                         
192800           IF OHUV-IDKAMPRF = +0 AND                                      
192900              OBKR-IDKUNDRF-RO = '0000000   '                             
193000              COMPUTE ART-KVOKS-DAG =                                     
193100              ART-KVOKS-DAG - OBKR-KVBEART-Q                              
193200           END-IF                                                         
193300        ELSE                                                              
193400     EJECT                                                                
193500                                                                          
193600           IF OBKR-KVPREAVB > +0                                          
193700              COMPUTE ART-KVPREAVB-BULK =                                 
193800              ART-KVPREAVB-BULK - OBKR-KVPREAVB                           
193900           END-IF                                                         
194000           IF OBKR-KVPRERO > +0                                           
194100              COMPUTE ART-KVPRERO-BULK =                                  
194200              ART-KVPRERO-BULK - OBKR-KVPRERO                             
194300           END-IF                                                         
194400           IF OHUV-IDKAMPRF = +0 AND                                      
194500              OBKR-IDKUNDRF-RO = '0000000   '                             
194600              COMPUTE ART-KVOKS-BULK =                                    
194700              ART-KVOKS-BULK - OBKR-KVBEART-Q                             
194800           END-IF                                                         
194900        END-IF                                                            
195000     END-IF                                                               
195100     .                                                                    
195200     EJECT                                                                
195300 HEB-BACKA-WDA5-STATUS SECTION.                                           
195400                                                                          
195500     MOVE OBKR-IDDISTR       TO W-A5-IDDISTR                              
195600     MOVE OBKR-IDKUNDNR      TO W-A5-IDKUNDNR                             
195700     MOVE WS-AKT-IDKUNDRF-RO (3:5)                                        
195800                             TO W-A5-IDKUNDRF                             
195900     INSPECT W-A5-IDKUNDRF REPLACING LEADING                              
196000                                 SPACE BY ZERO                            
196100     MOVE WS-AKT-IDARTNR     TO W-A5-IDARTNR                              
196200     MOVE WS-AKT-IDLOPNR-RO  TO W-A5-IDLOPNR                              
196300                                                                          
196400     PERFORM IMS-26-GHU-ORDP-WDA501                                       
196500                                                                          
196600     MOVE '3'             TO RAD-KDSTARAD                                 
196700     MOVE '00000     ' TO RAD-IDKUNDRF-LEV                                
196800     PERFORM IMS-27-REPL-ORDP-WDA501                                      
196900                                                                          
197000     IF SPAR-ARB-KDROPACK NOT = ZERO AND SPACE                            
197100                                                                          
197200        MOVE OBKR-IDDC         TO W-IDDC                                  
197300                                                                          
197400        PERFORM IMS-13-GHNP-ORQI-WDQ212-UNIK                              
197500        IF SEGMENT-FINNS                                                  
197600          MOVE ZERO            TO ARB-KDROPACK                            
197700          PERFORM IMS-15-REPL-ORQI                                        
197800        END-IF                                                            
197900     END-IF                                                               
198000     .                                                                    
198100     EJECT                                                                
198200                                                                          
198300 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
198400                                                                          
198500     MOVE NEJ                  TO AKT-SIDA-SW                             
198600*--- BEHANDLA RESTERANDE OBKR PÅ ORDERN                                   
198700                                                                          
198800     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
198900     MOVE +1                   TO WS-RADER                                
199000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WS-RADER > +13         
199100                                                                          
199200        PERFORM S02-GODKANN-RAD                                           
199300        ADD  +1               TO WS-RADER                                 
199400                                                                          
199500        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
199600     END-PERFORM                                                          
199700                                                                          
199800     IF START-4212                                                        
199900        MOVE 'V'              TO 4212-MID-KDTRTYP                         
200000        PERFORM S03-STARTA-RADBEHANDLINGEN                                
200100     ELSE                                                                 
200200        IF WS-RADER = 14 AND SEGMENT-FINNS                                
200300           PERFORM HGA-OMSKEDULERA                                        
200400        ELSE                                                              
200500           MOVE JA             TO AVSLUTA-SW                              
200600        END-IF                                                            
200700     END-IF                                                               
200800     .                                                                    
200900     EJECT                                                                
201000 HGA-OMSKEDULERA SECTION.                                                 
201100                                                                          
201200     MOVE MFS-KDMFSFOR         TO 4213-SPRAK                              
201300     MOVE MSGI-IDDISTR         TO 4213-IDDISTR-IN                         
201400     MOVE MSGI-IDKUNDNR        TO 4213-IDKUNDNR-IN                        
201500     MOVE MSGI-IDKUNDRF(3:5)   TO 4213-IDORDNR-IN                         
201600                                                                          
201700     MOVE MFS-RENSA-FAELT      TO 4213-IDDISTR-UT                         
201800                                  4213-IDKUNDNR-UT                        
201900                                  4213-IDORDNR-UT                         
202000                                                                          
202100     PERFORM IMS-INSERT-4213-MSG                                          
202200     MOVE JA                   TO HOPP                                    
202300     .                                                                    
202400     EJECT                                                                
202500                                                                          
202600 HH-ANNULLERA-ORDER SECTION.                                              
202700                                                                          
202800     MOVE MFS-KDMFSFOR         TO 4292-SPRAK                              
202900     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
203000     MOVE MSGI-IDDISTR         TO 4292-IDDISTR                            
203100     MOVE MSGI-IDKUNDNR        TO 4292-IDKUNDNR                           
203200     MOVE W-IDORDNR            TO 4292-IDKUNDRF                           
203300                                                                          
203400     MOVE OHUV-IDDC-PRIM       TO WS-IDDC                                 
203500     IF WS-IDDC NOT = W-IDDC-B6                                           
203600        MOVE WS-IDDC TO W-IDDC-B6                                         
203700        PERFORM IMS-GU-WDB601                                             
203800     END-IF                                                               
203900     IF DCS-NDC-NA                                                        
204000        PERFORM S23-DATA-TILL-DEL-NOTE                                    
204100     END-IF                                                               
204200                                                                          
204300     PERFORM IMS-INSERT-4292-MSG                                          
204400                                                                          
204500     MOVE 'W4O211N1'           TO MFS-IDMOD                               
204600                                                                          
204700     MOVE '4211'               TO MOD-W4O21301(1:4)                       
204800                                                                          
204900     MOVE MED-ORDER-ANNULLERAD TO MED-IDMFSFEL                            
205000     CALL WMEDKONV USING MED-WMEDAREA                                     
205100     MOVE MED-MFSFEL           TO MOD-W4O21301(5:40)                      
205200                                                                          
205300     MOVE 4211-MOD-LAENGD      TO MSG-KVLL                                
205400     PERFORM IMS-INSERT-MSG                                               
205500     MOVE JA                   TO HOPP                                    
205600     .                                                                    
205700     EJECT                                                                
205800                                                                          
205900 I-STARTA-ORDERAVSLUT SECTION.                                            
206000                                                                          
206100     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
206200     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
206300     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
206400     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
206500                                                                          
206600     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
206700     PERFORM IMS-INSERT-4298-MSG                                          
206800     .                                                                    
206900     EJECT                                                                
207000                                                                          
207100 M-HOPPA-TILL-ORDERHUVUD-4211 SECTION.                                    
207200                                                                          
207300     MOVE 'W4O211N1'           TO MFS-IDMOD                               
207400                                                                          
207500     MOVE '4211'               TO MOD-W4O21301(1:4)                       
207600                                                                          
207700     MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                            
207800     CALL WMEDKONV USING MED-WMEDAREA                                     
207900     MOVE MED-MFSFEL           TO MOD-W4O21301(5:40)                      
208000                                                                          
208100     MOVE 4211-MOD-LAENGD      TO MSG-KVLL                                
208200     PERFORM IMS-INSERT-MSG                                               
208300     MOVE JA                   TO HOPP                                    
208400     .                                                                    
208500     EJECT                                                                
208600                                                                          
208700 Z-FINIT SECTION.                                                         
208800                                                                          
208900     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
209000         CALL WMEDKONV USING MED-WMEDAREA                                 
209100         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
209200         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
209300     END-IF                                                               
209400                                                                          
209500     IF NOT ALLT-OK AND NYCKEL-OK                                         
209600        PERFORM MFS-ROER-EJ-BILD                                          
209700     END-IF                                                               
209800                                                                          
209900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O21301 + 4                        
210000     PERFORM IMS-INSERT-MSG                                               
210100     .                                                                    
210200     EJECT                                                                
210300 S01-SKRIV-MID-TILL-4212 SECTION.                                         
210400                                                                          
210500     MOVE JA                   TO START-4212-SW                           
210600     ADD +1                    TO 4212-MID-IX                             
210700                                                                          
210800     IF OBKR-IDARTNR-TILLK > +0                                           
210900       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
211000      MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                             
211100     ELSE                                                                 
211200       MOVE OBKR-IDARTNR       TO WS-IDARTNR                              
211300       MOVE OBKR-REKSIFFR      TO WS-REKSIFFR                             
211400     END-IF                                                               
211500     MOVE WS-IDARTNR-REKSIFFR  TO 4212-MID-IDARTNR(4212-MID-IX)           
211600                                                                          
211700     IF OBKR-KVBEART-TILLK > +0                                           
211800       MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                                
211900       MOVE WS-ALFA-6          TO 4212-MID-KVBEART(4212-MID-IX)           
212000     ELSE                                                                 
212100       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
212200          COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                 
212300          MOVE WS-ALFA-6       TO 4212-MID-KVBEART(4212-MID-IX)           
212400       ELSE                                                               
212500          MOVE OBKR-KVBEART    TO WS-NUM-6                                
212600          MOVE WS-ALFA-6       TO 4212-MID-KVBEART(4212-MID-IX)           
212700       END-IF                                                             
212800     END-IF                                                               
212900                                                                          
213000     MOVE ALL '+'              TO 4212-MID-PRARTNTO(4212-MID-IX)          
213100     IF OBKR-TITPO > +0                                                   
213200        MOVE OBKR-TITPO        TO WS-NUM-6                                
213300        MOVE WS-ALFA-6         TO 4212-MID-TITPO(4212-MID-IX)             
213400     ELSE                                                                 
213500        MOVE ALL '+'           TO 4212-MID-TITPO(4212-MID-IX)             
213600     END-IF                                                               
213700     EJECT                                                                
213800     MOVE OBKR-FLRESTN         TO 4212-MID-FLRESTN(4212-MID-IX)           
213900     MOVE OBKR-FLSLATT         TO 4212-MID-FLSLATT(4212-MID-IX)           
214000     MOVE OBKR-KDKVBRYT        TO 4212-MID-KDKVBRYT(4212-MID-IX)          
214100     MOVE OBKR-FLINVEST        TO 4212-MID-FLINVEST(4212-MID-IX)          
214200     MOVE OBKR-KDVRINFO        TO 4212-MID-KDVRINFO(4212-MID-IX)          
214300     MOVE OBKR-BERADREF        TO 4212-MID-BERADREF(4212-MID-IX)          
214400     .                                                                    
214500     EJECT                                                                
214600 S02-GODKANN-RAD SECTION.                                                 
214700                                                                          
214800     MOVE OBKR-IDDC            TO WS-IDDC                                 
214900     MOVE JA                   TO OBKR-FLOBOK                             
215000     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
215100     IF OBKR-IDPGM(1:4) = '4206'                                          
215200       MOVE '4206'       TO X-IDTRANS                                     
215300     END-IF                                                               
215400                                                                          
215500     IF ((OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0)                        
215600            AND OBKR-KDORDBEK NOT = 92 AND 98) OR                         
215700               (OBKR-KDORDBEK = 92 AND OBKR-KVPREAVB > +0) OR             
215800               (OBKR-KDORDBEK = 98 AND OBKR-KVPREAVB > +0)                
215900                                                                          
216000        PERFORM S14-LAS-ARTIKELREG                                        
216100        PERFORM S12-BYGG-UPP-ORDERRAD                                     
216200                                                                          
216300        PERFORM S09-KONTROLLERA-ENHETSLAST                                
216400        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
216500     END-IF                                                               
216600                                                                          
216700     IF OBKR-KDORDBEK = 10                                                
216800                                                                          
216900       PERFORM S14-LAS-ARTIKELREG                                         
217000       PERFORM S02A-VALD-BIPACKNING                                       
217100     ELSE                                                                 
217200       IF (OBKR-KDORDBEK =  61) AND AKT-SIDA                              
217300                                                                          
217400         PERFORM S01-SKRIV-MID-TILL-4212                                  
217500         PERFORM S02B-SKRIV-ORDERBEKR-40                                  
217600       ELSE                                                               
217700     EJECT                                                                
217800                                                                          
217900         IF OBKR-KDORDBEK =  70  OR 71 OR 74                              
218000           IF OBKR-KDTPOTYP = +2                                          
218100                                                                          
218200             PERFORM S14-LAS-ARTIKELREG                                   
218300             PERFORM S02C-UPPDATERA-TPO2                                  
218400           ELSE                                                           
218500             IF OBKR-KDTPOTYP = +6                                        
218600                                                                          
218700                PERFORM S14-LAS-ARTIKELREG                                
218800                PERFORM S02D-UPPDATERA-TPO6                               
218900             END-IF                                                       
219000           END-IF                                                         
219100         ELSE                                                             
219200*DDGS      IF (OBKR-KDORDBEK = 21 OR 52 OR 53 OR 54 OR                    
219300           IF (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR                    
219400                               55 OR 57 OR 67 OR 92 OR 98)                
219500                          AND  OHUV-KDORDKL = +0                          
219600                                                                          
219700              PERFORM S11-SKRIV-VOR-RAD                                   
219800           END-IF                                                         
219900         END-IF                                                           
220000       END-IF                                                             
220100     END-IF                                                               
220200     .                                                                    
220300     EJECT                                                                
220400                                                                          
220500 S02A-VALD-BIPACKNING SECTION.                                            
220600                                                                          
220700     PERFORM S12-BYGG-UPP-ORDERRAD                                        
220800                                                                          
220900*** BIPACKNING RESTORDER ***                                              
221000     IF OBKR-TIRODAT > +0                                                 
221100        PERFORM S09-KONTROLLERA-ENHETSLAST                                
221200        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
221300        IF SPAR-ARB-KDROPACK = 'L'                                        
221400           PERFORM S02AB-UPPDATERA-WDE8                                   
221500        END-IF                                                            
221600     ELSE                                                                 
221700*** BIPACKNING TPO ***                                                    
221800       IF WS-IDDC NOT = W-IDDC-B6                                         
221900          MOVE WS-IDDC TO W-IDDC-B6                                       
222000          PERFORM IMS-GU-WDB601                                           
222100       END-IF                                                             
222200       IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                
222300         IF DCS-SDC                                                       
222400            PERFORM S02AA-KOLLA-DIREKTLEVERANS                            
222500         ELSE                                                             
222600            MOVE ZERO   TO DLEV-KDORDBEK-UT                               
222700         END-IF                                                           
222800                                                                          
222900         IF DLEV-KDORDBEK-UT = +0                                         
223000            PERFORM S02AC-KOMPLETTERA-RANSONERING                         
223100            PERFORM S02AE-PREL-AVBOKNING                                  
223200            PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                           
223300                                                                          
223400            IF SPAR-ARB-KDROPACK = 'L'                                    
223500               PERFORM S02AB-UPPDATERA-WDE8                               
223600            END-IF                                                        
223700         ELSE                                                             
223800            MOVE DLEV-KDORDBEK-UT TO OBKR-KDORDBEK                        
223900            MOVE '4213DLEV'      TO OBKR-IDPGM                            
224000            MOVE DLEV-IDLEVNR-UT TO OBKR-IDLEVNR                          
224100            MOVE NEJ             TO OBKR-FLOBOK                           
224200                                                                          
224300            ADD +1               TO OBKR-IDSEKVNR                         
224400            PERFORM IMS-07-ISRT-ORQM-WDQ101                               
224500                                                                          
224600            IF DLEV-KDORDBEK-UT = 95                                      
224700               PERFORM S02AC-KOMPLETTERA-RANSONERING                      
224800               PERFORM S02AE-PREL-AVBOKNING                               
224900               PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                        
225000                                                                          
225100               IF SPAR-ARB-KDROPACK = 'L'                                 
225200                  PERFORM S02AB-UPPDATERA-WDE8                            
225300               END-IF                                                     
225400            ELSE                                                          
225500               MOVE OBKR-IDARTNR TO W-IDARTNR                             
225600               PERFORM IMS-21-GHU-ARTM-WDK901                             
225700               IF OBKR-KDORDKL = +0                                       
225800                  SUBTRACT OBKR-KVBEART-Q FROM                            
225900                           ART-KVOKS-VOR                                  
226000               ELSE                                                       
226100                 IF OBKR-KDORDKL = +1                                     
226200                    SUBTRACT OBKR-KVBEART-Q FROM                          
226300                             ART-KVOKS-DAG                                
226400                 ELSE                                                     
226500                    SUBTRACT OBKR-KVBEART-Q FROM                          
226600                             ART-KVOKS-BULK                               
226700                 END-IF                                                   
226800               END-IF                                                     
226900               PERFORM IMS-22-REPL-ARTM-WDK901                            
227000            END-IF                                                        
227100         END-IF                                                           
227200       END-IF                                                             
227300     END-IF                                                               
227400     .                                                                    
227500     EJECT                                                                
227600                                                                          
227700 S02AA-KOLLA-DIREKTLEVERANS SECTION.                                      
227800                                                                          
227900     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
228000     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
228100     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
228200     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
228300     MOVE ORAD-IDLEVNR         TO DLEV-IDLEVNR-IN                         
228400     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
228500     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
228600     MOVE ORAD-IDDC            TO DLEV-IDDC-IN                            
228700     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
228800     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
228900     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
229000     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
229100     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
229200     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
229300     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
229400     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
229500     MOVE ORAD-CLEARGROUP      TO DLEV-CLEARGROUP                         
229600     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
229700*    TO GET CLEARING DC LIST FROM WDB2                                    
229800     MOVE ORAD-IDDISTR         TO W-WDB2-IDDISTR                          
229900     MOVE ORAD-IDKUNDNR        TO W-WDB2-IDKUNDNR                         
230000     PERFORM IMS-GU-WDB201                                                
230100*                                                                         
230200     IF OHUV-KDORDKL > 1                                                  
230300                                                                          
230400        MOVE +1 TO WS-INDEX                                               
230500        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
230600           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
230700                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
230800           ADD +1 TO WS-INDEX                                             
230900        END-PERFORM                                                       
231000                                                                          
231100     ELSE                                                                 
231200       IF OHUV-KDORDKL = 1                                                
231300                                                                          
231400          MOVE +1 TO WS-INDEX                                             
231500          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
231600             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
231700                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
231800             ADD +1 TO WS-INDEX                                           
231900          END-PERFORM                                                     
232000                                                                          
232100       ELSE                                                               
232200         IF OHUV-KDORDKL = 0                                              
232300                                                                          
232400            MOVE +1 TO WS-INDEX                                           
232500            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
232600               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
232700                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
232800               ADD +1 TO WS-INDEX                                         
232900            END-PERFORM                                                   
233000                                                                          
233100         END-IF                                                           
233200       END-IF                                                             
233300     END-IF                                                               
233400                                                                          
233500     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
233600**   MOVE 1                    TO DLEV-KDCALL                             
233700**   THE ABOVE LINE IS COMMENTED AND ZERO IS MOVED TO KDCALL              
233800**   TO AVOID THE INSERT INTO WDR6 IN W411DLEV                            
233900     MOVE ZERO                 TO DLEV-KDCALL                             
234000                                                                          
234100     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
234200                                       DLEV-LEVG-PCB                      
234300                                       DLEV-LEVA-PCB                      
234400                                       DLEV-ARTS-PCB                      
234500                                       DLEV-WDB6-PCB                      
234600                                       TPO2-FILA-PCB                      
234700                                                                          
234800     IF DLEV-IDLEVNR-UT NOT = SPACE                                       
234900       IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                                 
235000         MOVE DLEV-IDDC-UT  TO ORAD-IDDC                                  
235100       END-IF                                                             
235200     END-IF                                                               
235300                                                                          
235400     MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)           
235500     MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)           
235600     MOVE DLEV-KVDAGAR-DIFF-UT TO                                         
235700                               AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)           
235800     MOVE DLEV-TISKEPPN-DDC-UT TO                                         
235900                               AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)           
236000                                                                          
236100     MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                            
236200     MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                            
236300     MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                           
236400     IF DLEV-FLSDCLEV-UT = JA                                             
236500       MOVE DLEV-IDDC-UT       TO ORAD-IDDC                               
236600                                  WS-IDDC                                 
236700     ELSE                                                                 
236800       MOVE DLEV-KDOI-UT       TO ORAD-KDOI                               
236900       MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                         
237000     END-IF                                                               
237100     .                                                                    
237200     EJECT                                                                
237300 S02AB-UPPDATERA-WDE8 SECTION.                                            
237400                                                                          
237500     MOVE W-IDDISTR        TO W-E8-IDDISTR                                
237600     MOVE W-IDKUNDNR       TO W-E8-IDKUNDNR                               
237700     MOVE OBKR-IDKUNDRF-RO TO W-E8-IDKUNDRF                               
237800     PERFORM IMS-35-GHU-PROC-WDE801                                       
237900                                                                          
238000*--FIX FÖR FELAKTIGT RENSADE PROFHUVUD. TAS BORT 92/93                    
238100*--TA BORT 'GE' OCKSÅ I IMS-35                                            
238200     IF SEGMENT-FINNS                                                     
238300     MOVE +1                   TO WS-INDEX                                
238400     PERFORM UNTIL WS-INDEX > 10                                          
238500        IF PHUV-IDKUNDRF-ING(WS-INDEX) = W-IDKUNDRF                       
238600           MOVE +10            TO WS-INDEX                                
238700        ELSE                                                              
238800           IF PHUV-IDKUNDRF-ING(WS-INDEX) = '0000000   '                  
238900              MOVE W-IDKUNDRF  TO PHUV-IDKUNDRF-ING(WS-INDEX)             
239000                                                                          
239100              PERFORM IMS-36-REPL-PROC-WDE801                             
239200              MOVE +10         TO WS-INDEX                                
239300           END-IF                                                         
239400        END-IF                                                            
239500        ADD  +1                TO WS-INDEX                                
239600     END-PERFORM                                                          
239700     END-IF                                                               
239800     .                                                                    
239900     EJECT                                                                
240000                                                                          
240100 S02AC-KOMPLETTERA-RANSONERING SECTION.                                   
240200                                                                          
240300     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
240400     MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                           
240500     MOVE OHUV-FLFORBI         TO RANS-FLFORBI                            
240600     MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                           
240700     MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                           
240800     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
240900     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
241000     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
241100     MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                            
241200     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
241300     MOVE +2                   TO RANS-KDORDBEH                           
241400     MOVE OHUV-KDORDKL         TO RANS-KDORDKL                            
241500     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
241600     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
241700     MOVE AREG-KDERS           TO RANS-KDERS                              
241800     MOVE AREG-KVLS            TO RANS-KVLS                               
241900     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
242000     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
242100     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
242200     MOVE AREG-KVRESS          TO RANS-KVRESS                             
242300     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
242400     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
242500     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
242600                                                                          
242700     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
242800     IF KDPRODSL-BIMA                                                     
242900       MOVE 1                  TO ORAD-RERF-RAD                           
243000                                  RANS-RERF-RAD-UT                        
243100       MOVE ZERO               TO RANS-SUTPO-PB-UT                        
243200                                  RANS-SUTPO-EJPB-UT                      
243300                                  RANS-RERF-ART-UT                        
243400     ELSE                                                                 
243500       CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                    
243600                           RANS-ARTM-PCB RANS-ARTS-PCB                    
243700                                                                          
243800       MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                             
243900     END-IF                                                               
244000     .                                                                    
244100     EJECT                                                                
244200                                                                          
244300 S02AE-PREL-AVBOKNING SECTION.                                            
244400                                                                          
244500     MOVE JA                   TO CDCA-FLFINLV-IN                         
244600     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
244700     MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                        
244800     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
244900     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
245000     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
245100     MOVE OBKR-FLSLATT         TO CDCA-FLSLATT-IN                         
245200     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
245300     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
245400     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
245500     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
245600     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
245700     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
245800     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
245900     MOVE +0                   TO CDCA-KDERS-IN                           
246000     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
246100     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
246200     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
246300     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
246400     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
246500     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
246600     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
246700     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
246800     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
246900     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
247000     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
247100     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
247200     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
247300     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
247400     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
247500     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
247600     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
247700     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
247800     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
247900     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
248000     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
248100     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
248200     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
248300     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
248400     MOVE +1                   TO CDCA-KDCALL                             
248500                                                                          
248600     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
248700                                       CDCA-INLB-PCB                      
248800                                       CDCA-WDB2-PCB                      
248900                                       CDCA-WDC1-PCB                      
249000     EJECT                                                                
249100     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
249200     MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                            
249300     MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                          
249400     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
249500     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
249600     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
249700     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
249800     .                                                                    
249900     EJECT                                                                
250000 S02AF-SKRIV-Q1-OCH-Q4-RADER SECTION.                                     
250100                                                                          
250200     MOVE AREG-ADLAGOMR    TO ORAD-ADLAGOMR                               
250300     MOVE AREG-ADGANG      TO ORAD-ADGANG                                 
250400     MOVE AREG-ADPLATS     TO ORAD-ADPLATS                                
250500                                                                          
250600     IF ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0                           
250700       PERFORM S09-KONTROLLERA-ENHETSLAST                                 
250800       PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                                
250900     END-IF                                                               
251000                                                                          
251100     IF CDCA-KDORDBEK-UT >  0                                             
251200       PERFORM S02AFA-SKRIV-ORDERBEKR                                     
251300     END-IF                                                               
251400     .                                                                    
251500     EJECT                                                                
251600 S02AFA-SKRIV-ORDERBEKR SECTION.                                          
251700                                                                          
251800     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
251900     MOVE ORAD-IDDC            TO OBKR-IDDC                               
252000     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
252100     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
252200     MOVE ORAD-KVPREAVB        TO OBKR-KVPREAVB                           
252300     MOVE ORAD-KVPRERO         TO OBKR-KVPRERO                            
252400     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
252500                                                                          
252600     IF CDCA-KDORDBEK-UT > +0                                             
252700*----(KOD 80, 92, 99)                                                     
252800        IF CDCA-KDORDBEK-UT =  80                                         
252900          MOVE CDCA-KVANNANT-UT TO OBKR-KVANNANT                          
253000        END-IF                                                            
253100                                                                          
253200        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
253300           MOVE ORAD-KVBEART-Q TO OBKR-KVPREAVB                           
253400        END-IF                                                            
253500        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
253600        MOVE '4213CDCA'        TO OBKR-IDPGM                              
253700        ADD +1                 TO OBKR-IDSEKVNR                           
253800        PERFORM  IMS-07-ISRT-ORQM-WDQ101                                  
253900     END-IF                                                               
254000     .                                                                    
254100     EJECT                                                                
254200                                                                          
254300 S02B-SKRIV-ORDERBEKR-40 SECTION.                                         
254400                                                                          
254500     MOVE OBKR-KVBEART-TILLK   TO SPAR-KVBEART-TILLK                      
254600     MOVE OBKR-IDARTNR-TILLK   TO SPAR-IDARTNR-TILLK                      
254700     MOVE OBKR-REKSIFFR-TILLK  TO SPAR-REKSIFFR-TILLK                     
254800     MOVE OBKR-DIERS-KVOT      TO SPAR-DIERS-KVOT                         
254900                                                                          
255000     IF OBKR-IDARTNR NOT = SPAR-IDARTNR-40  OR                            
255100         (OBKR-IDARTNR = SPAR-IDARTNR-40   AND                            
255200          OBKR-IDLOPNR NOT = SPAR-IDLOPNR-40)                             
255300                                                                          
255400        PERFORM S02BA-SKRIV-KOD40-ERSATT-ART                              
255500        COMPUTE SPAR-IDSEKVNR-40 = OBKR-IDSEKVNR + 1                      
255600     END-IF                                                               
255700                                                                          
255800     MOVE SPAR-KVBEART-TILLK   TO OBKR-KVBEART-TILLK                      
255900     MOVE SPAR-IDARTNR-TILLK   TO OBKR-IDARTNR-TILLK                      
256000     MOVE SPAR-REKSIFFR-TILLK  TO OBKR-REKSIFFR-TILLK                     
256100     MOVE SPAR-DIERS-KVOT      TO OBKR-DIERS-KVOT                         
256200                                                                          
256300     MOVE JA                   TO OBKR-FLOBOK                             
256400     MOVE 40                   TO OBKR-KDORDBEK                           
256500     MOVE IDPGM                TO OBKR-IDPGM                              
256600     MOVE SPAR-IDSEKVNR-40     TO OBKR-IDSEKVNR                           
256700                                                                          
256800     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
256900     ADD +1                 TO OBKR-IDSEKVNR                              
257000                               SPAR-IDSEKVNR-40                           
257100     MOVE OBKR-IDARTNR         TO SPAR-IDARTNR-40                         
257200     MOVE OBKR-IDLOPNR         TO SPAR-IDLOPNR-40                         
257300                                                                          
257400     PERFORM S02BB-UPPDAT-ERSATT-ART                                      
257500     .                                                                    
257600     EJECT                                                                
257700 S02BA-SKRIV-KOD40-ERSATT-ART SECTION.                                    
257800                                                                          
257900     MOVE OBKR-IDARTNR         TO W-Q1-IDARTNR-MIN1                       
258000                                  W-Q1-IDARTNR-MAX1                       
258100     MOVE OBKR-IDLOPNR         TO W-Q1-IDLOPNR-MIN1                       
258200                                  W-Q1-IDLOPNR-MAX1                       
258300     MOVE +3                   TO W-Q1-IDSEKVNR-MIN1                      
258400     MOVE +999                 TO W-Q1-IDSEKVNR-MAX1                      
258500                                                                          
258600     PERFORM IMS-33-GN-ORQM-WDQ101                                        
258700     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
258800                                                                          
258900        PERFORM IMS-33-GN-ORQM-WDQ101                                     
259000     END-PERFORM                                                          
259100                                                                          
259200     MOVE +0                TO OBKR-IDARTNR-TILLK                         
259300                               OBKR-KVBEART-TILLK                         
259400                               OBKR-REKSIFFR-TILLK                        
259500                               OBKR-DIERS-KVOT                            
259600     MOVE JA                TO OBKR-FLOBOK                                
259700     MOVE 40                TO OBKR-KDORDBEK                              
259800     MOVE IDPGM             TO OBKR-IDPGM                                 
259900     ADD +1                 TO OBKR-IDSEKVNR                              
260000                                                                          
260100     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
260200     .                                                                    
260300     EJECT                                                                
260400                                                                          
260500 S02BB-UPPDAT-ERSATT-ART SECTION.                                         
260600                                                                          
260700*--VID VAL AV ERSÄTTNING SÄTTS FLOBTRAN TILL 'N' FÖR ATT FÖRHINDRA        
260800*--ATT ORDERBEKRÄFTELSETRANS SKICKAS FRÅN W4029300 TILL VR/VIPS           
260900                                                                          
261000     MOVE OBKR-IDARTNR         TO W-Q1-IDARTNR-MIN1                       
261100                                  W-Q1-IDARTNR-MAX1                       
261200     MOVE OBKR-IDLOPNR         TO W-Q1-IDLOPNR-MIN1                       
261300                                  W-Q1-IDLOPNR-MAX1                       
261400     MOVE +1                   TO W-Q1-IDSEKVNR-MIN1                      
261500                                  W-Q1-IDSEKVNR-MAX1                      
261600     PERFORM IMS-34-GHU-ORQM-WDQ101                                       
261700                                                                          
261800     MOVE NEJ                  TO OBKR-FLOBTRAN                           
261900     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
262000     .                                                                    
262100     EJECT                                                                
262200 S02C-UPPDATERA-TPO2 SECTION.                                             
262300                                                                          
262400     IF WS-IDDC NOT = W-IDDC-B6                                           
262500        MOVE WS-IDDC TO W-IDDC-B6                                         
262600        PERFORM IMS-GU-WDB601                                             
262700     END-IF                                                               
262800                                                                          
262900     IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                  
263000       PERFORM S12-BYGG-UPP-ORDERRAD                                      
263100                                                                          
263200       MOVE ORAD-IDDISTR       TO TPO2-IDDISTR                            
263300       MOVE ORAD-IDKUNDNR      TO TPO2-IDKUNDNR                           
263400       MOVE ORAD-IDKUNDRF      TO TPO2-IDKUNDRF                           
263500       MOVE ORAD-IDARTNR       TO TPO2-IDARTNR                            
263600       MOVE ORAD-BERADREF      TO TPO2-BERADREF                           
263700       MOVE AREG-IDANSK        TO TPO2-IDANSK                             
263800                                                                          
263900       MOVE OHUV-IDKONTO       TO TPO2-IDKONTO                            
264000       MOVE OHUV-IDKST         TO TPO2-IDKST                              
264100       MOVE OHUV-IDANALYS      TO TPO2-IDANALYS                           
264200       MOVE ORAD-KDDSP         TO TPO2-KDDSP                              
264300       MOVE OHUV-KDFAKTYP      TO TPO2-KDFAKTYP                           
264400       IF OBKR-KDORDBEK = 71                                              
264500          MOVE OBKR-KDFRAKT     TO TPO2-KDFRAKT                           
264600       ELSE                                                               
264700          MOVE SPAR-ARB-KDFRAKT TO TPO2-KDFRAKT                           
264800       END-IF                                                             
264900       MOVE ORAD-KDKVBRYT      TO TPO2-KDKVBRYT                           
265000       EJECT                                                              
265100       MOVE ORAD-KDORDING      TO TPO2-KDORDING                           
265200       MOVE OHUV-KDORDKL       TO TPO2-KDORDKL                            
265300       MOVE ORAD-KDPRODSL      TO TPO2-KDPRODSL                           
265400       MOVE +2                 TO TPO2-KDTPOTYP                           
265500       MOVE ORAD-KDVRINFO      TO TPO2-KDVRINFO                           
265600       MOVE ORAD-KVBEART-Q     TO TPO2-KVBEART-Q                          
265700       MOVE ORAD-PRARTNTO      TO TPO2-PRARTNTO                           
265800       MOVE ORAD-DEAL-PR-LINE  TO TPO2-DEAL-PR-LINE                       
265900       MOVE ORAD-REKSIFFR      TO TPO2-REKSIFFR                           
266000       MOVE ORAD-TITPO         TO TPO2-TITPO                              
266100       MOVE ORAD-KDPRTYP       TO TPO2-KDPRTYP                            
266200       MOVE ORAD-BEVOLREF      TO TPO2-BEVOLREF                           
266300       MOVE ORAD-FLINVEST      TO TPO2-FLINVEST                           
266400       MOVE OHUV-FLORDSPE      TO TPO2-FLORDSPE                           
266500       MOVE OHUV-FLOVRLEV      TO TPO2-FLOVRLEV                           
266600       MOVE OHUV-FLFORBI       TO TPO2-FLFORBI                            
266700       MOVE ORAD-FLPRTILL      TO TPO2-FLPRTILL                           
266800       MOVE OHUV-BEKUNDRF      TO TPO2-BEKUNDRF                           
266900       MOVE ORAD-IDKAMPRF      TO TPO2-IDKAMPRF                           
267000       MOVE ORAD-IDLEVNR       TO TPO2-IDLEVNR                            
267100       MOVE ORAD-IDSYSTEM      TO TPO2-IDSYSTEM                           
267200       MOVE AREG-KDUART        TO TPO2-KDUART                             
267300       MOVE AREG-KVFRYSTI      TO TPO2-KVFRYSTI                           
267400       IF OBKR-KDORDBEK = 71                                              
267500          MOVE +3              TO TPO2-KDORDBEH                           
267600       ELSE                                                               
267700          MOVE +2              TO TPO2-KDORDBEH                           
267800       END-IF                                                             
267900       MOVE ORAD-FLTILLK       TO TPO2-FLTILLK                            
268000       MOVE AREG-TIDISPIN      TO TPO2-TIDISPIN                           
268100       MOVE OHUV-BEVARREF      TO TPO2-BEVARREF                           
268200       MOVE OBKR-KVQPACK       TO TPO2-KVQPACK-1                          
268300       MOVE ORAD-KVBEART       TO TPO2-KVBEART                            
268400                                                                          
268500       MOVE OHUV-KDORDTYP-LDC  TO TPO2-KDORDTYP-LDC                       
268600       MOVE OHUV-TIREPDAT      TO TPO2-TIREPDAT                           
268700       MOVE ORAD-IDKUNDRF-WIP  TO TPO2-IDKUNDRF-WIP                       
268800                                                                          
268900       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
269000                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
269100                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
269200                           TIME-4437-PCB                                  
269300     END-IF                                                               
269400     .                                                                    
269500     EJECT                                                                
269600                                                                          
269700 S02D-UPPDATERA-TPO6 SECTION.                                             
269800                                                                          
269900     IF WS-IDDC NOT = W-IDDC-B6                                           
270000        MOVE WS-IDDC TO W-IDDC-B6                                         
270100        PERFORM IMS-GU-WDB601                                             
270200     END-IF                                                               
270300                                                                          
270400     IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                  
270500       PERFORM S12-BYGG-UPP-ORDERRAD                                      
270600                                                                          
270700       MOVE ORAD-IDDISTR       TO TPO6-IDDISTR                            
270800       MOVE ORAD-IDKUNDNR      TO TPO6-IDKUNDNR                           
270900       MOVE ORAD-IDKUNDRF      TO TPO6-IDKUNDRF                           
271000       MOVE ORAD-IDARTNR       TO TPO6-IDARTNR                            
271100       MOVE OHUV-IDDC-PRIM     TO TPO6-IDDC-DAY                           
271200       MOVE AREG-FLREFILL      TO TPO6-FLREFILL                           
271300       MOVE AREG-KDUART        TO TPO6-KDUART                             
271400       MOVE AREG-REDIRLEV      TO TPO6-REDIRLEV                           
271500       MOVE ORAD-BERADREF      TO TPO6-BERADREF                           
271600       MOVE AREG-IDANSK        TO TPO6-IDANSK                             
271700                                                                          
271800       MOVE OHUV-IDKONTO       TO TPO6-IDKONTO                            
271900       MOVE OHUV-IDKST         TO TPO6-IDKST                              
272000       MOVE OHUV-IDANALYS      TO TPO6-IDANALYS                           
272100       MOVE ORAD-KDDSP         TO TPO6-KDDSP                              
272200       MOVE OHUV-KDFAKTYP      TO TPO6-KDFAKTYP                           
272300       MOVE SPAR-ARB-KDFRAKT   TO TPO6-KDFRAKT                            
272400       MOVE ORAD-KDKVBRYT      TO TPO6-KDKVBRYT                           
272500       EJECT                                                              
272600                                                                          
272700       MOVE ORAD-KDORDING      TO TPO6-KDORDING                           
272800       MOVE OHUV-KDORDKL       TO TPO6-KDORDKL                            
272900       MOVE ORAD-KDPRODSL      TO TPO6-KDPRODSL                           
273000       MOVE +6                 TO TPO6-KDTPOTYP                           
273100       MOVE ORAD-KDVRINFO      TO TPO6-KDVRINFO                           
273200       MOVE ORAD-KVBEART-Q     TO TPO6-KVBEART-Q                          
273300       MOVE ORAD-PRARTNTO      TO TPO6-PRARTNTO                           
273400       MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                       
273500       MOVE AREG-REKSIFFR      TO TPO6-REKSIFFR                           
273600       MOVE ORAD-KDPRTYP       TO TPO6-KDPRTYP                            
273700       MOVE OHUV-BEVARREF      TO TPO6-BEVOLREF                           
273800       MOVE ORAD-FLINVEST      TO TPO6-FLINVEST                           
273900       MOVE ORAD-FLPRTILL      TO TPO6-FLPRTILL                           
274000       MOVE OHUV-BEKUNDRF      TO TPO6-BEKUNDRF                           
274100       MOVE ORAD-IDKAMPRF      TO TPO6-IDKAMPRF                           
274200       MOVE ORAD-IDLEVNR       TO TPO6-IDLEVNR                            
274300       MOVE ORAD-IDSYSTEM      TO TPO6-IDSYSTEM                           
274400       MOVE OHUV-FLFORBI       TO TPO6-FLFORBI                            
274500                                                                          
274600       MOVE OHUV-KDORDTYP-LDC  TO TPO6-KDORDTYP-LDC                       
274700       MOVE OHUV-TIREPDAT      TO TPO6-TIREPDAT                           
274800       MOVE ORAD-IDKUNDRF-WIP  TO TPO6-IDKUNDRF-WIP                       
274900                                                                          
275000       MOVE ORAD-KDOI          TO TPO6-KDOI                               
275100       MOVE ORAD-CLEARGROUP    TO TPO6-CLEARGROUP                         
275200                                                                          
275300       CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB           
275400           TPO6-XXBU-PCB TPO6-XXBV-PCB TPO6-XXBX-PCB                      
275500             TPO6-ARTS-PCB TIME-4437-PCB                                  
275600     END-IF                                                               
275700     .                                                                    
275800     EJECT                                                                
275900 S03-STARTA-RADBEHANDLINGEN SECTION.                                      
276000                                                                          
276100     MOVE MFS-KDMFSFOR         TO 4212-SPRAK                              
276200                                                                          
276300     MOVE MSGI-IDDISTR         TO 4212-MID-IDDISTR                        
276400     MOVE MSGI-IDKUNDNR        TO 4212-MID-IDKUNDNR                       
276500     MOVE MSGI-IDKUNDRF(3:5)   TO 4212-MID-IDORDNR5                       
276600                                                                          
276700     MOVE OBKR-BEVOLREF        TO 4212-MID-BEVOLREF                       
276800                                                                          
276900     COMPUTE 4212-LL = LENGTH OF 4212-MID-W4I21201 + 17                   
277000     PERFORM IMS-INSERT-4212-MSG                                          
277100     MOVE JA                   TO HOPP                                    
277200     .                                                                    
277300     EJECT                                                                
277400 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
277500                                                                          
277600     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
277700     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
277800     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
277900     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
278000     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
278100     MOVE ORAD-IDDC            TO LAST-IDDC                               
278200     MOVE SPAR-ARB-KDFDKRAV    TO LAST-KDFDKRAV                           
278300     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
278400     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
278500     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
278600                                                                          
278700     CALL W411LAST USING LAST-W411LAST                                    
278800     .                                                                    
278900     EJECT                                                                
279000 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
279100                                                                          
279200     IF LAST-ADLAGOMR-UT = +0 AND                                         
279300        LAST-KVANTAL-UT  = +0 AND                                         
279400        LAST-KVBEART-UT  = +0                                             
279500*------------------------------------------------------------*            
279600*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
279700*------------------------------------------------------------*            
279800        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
279900        PERFORM S10B-REDIGERA-WOPS-AREA                                   
280000        PERFORM IMS-17-ISRT-ORQF-WDQ401                                   
280100        PERFORM UNTIL SEGMENT-FINNS                                       
280200           ADD +1                    TO ORAD-IDLOPNR                      
280300           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
280400        END-PERFORM                                                       
280500     ELSE                                                                 
280600                                                                          
280700        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
280800*------------------------------------------------------------*            
280900*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
281000*------------------------------------------------------------*            
281100                                                                          
281200           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
281300                                                                          
281400           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
281500                                    ORAD-KVPRERO                          
281600           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
281700           PERFORM S10C-BERAEKNA-KVSLATT                                  
281800           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
281900           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
282000           MOVE ORAD-ADGANG          TO WS-ADGANG                         
282100           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
282200           PERFORM S10B-REDIGERA-WOPS-AREA                                
282300           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
282400           PERFORM UNTIL SEGMENT-FINNS                                    
282500              ADD +1                 TO ORAD-IDLOPNR                      
282600              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
282700           END-PERFORM                                                    
282800     EJECT                                                                
282900*------------------------------------------------------------*            
283000*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
283100*------------------------------------------------------------*            
283200                                                                          
283300           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
283400           MOVE WS-ADGANG            TO ORAD-ADGANG                       
283500           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
283600           MOVE +0                   TO ORAD-KVBEART                      
283700           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
283800                                        ORAD-KVPREAVB                     
283900           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
284000           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64 OR ZERO                   
284100             CONTINUE                                                     
284200           ELSE                                                           
284300             IF LAST-ADGANG-UT > ZERO                                     
284400               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
284500             END-IF                                                       
284600           END-IF                                                         
284700           MOVE +0                   TO ORAD-KVPRERO                      
284800           MOVE 1.0000               TO ORAD-RERF-RAD                     
284900           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
285000           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
285100           PERFORM S10B-REDIGERA-WOPS-AREA                                
285200           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
285300           PERFORM UNTIL SEGMENT-FINNS                                    
285400              ADD +1                 TO ORAD-IDLOPNR                      
285500              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
285600           END-PERFORM                                                    
285700        ELSE                                                              
285800*------------------------------------------------------------*            
285900*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
286000*------------------------------------------------------------*            
286100           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
286200           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64 OR ZERO                   
286300             CONTINUE                                                     
286400           ELSE                                                           
286500             IF LAST-ADGANG-UT > ZERO                                     
286600               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
286700             END-IF                                                       
286800           END-IF                                                         
286900           MOVE 1.0000            TO ORAD-RERF-RAD                        
287000           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
287100           PERFORM S10B-REDIGERA-WOPS-AREA                                
287200           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
287300           PERFORM UNTIL SEGMENT-FINNS                                    
287400              ADD +1              TO ORAD-IDLOPNR                         
287500              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
287600           END-PERFORM                                                    
287700        END-IF                                                            
287800     END-IF                                                               
287900     .                                                                    
288000     EJECT                                                                
288100 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
288200                                                                          
288300     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
288400     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
288500     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
288600     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
288700     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
288800     MOVE 1                    TO ADRS-KDCALL-IN                          
288900     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
289000     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
289100     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
289200     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
289300                                                                          
289400     CALL W413ADRS USING ADRS-W413ADRS                                    
289500                                                                          
289600*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
289700     IF ORAD-IDKAMPRF > 0                                                 
289800       MOVE 8                  TO ORAD-ADLAGOMR                           
289900     ELSE                                                                 
290000       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
290100     END-IF                                                               
290200     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
290300                                                                          
290400*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
290500     IF WS-IDDC NOT = W-IDDC-B6                                           
290600        MOVE WS-IDDC TO W-IDDC-B6                                         
290700        PERFORM IMS-GU-WDB601                                             
290800     END-IF                                                               
290900     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
291000     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
291100                                                                          
291200     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
291300     .                                                                    
291400     EJECT                                                                
291500 S10B-REDIGERA-WOPS-AREA SECTION.                                         
291600                                                                          
291700     MOVE +1                   TO AVSR-KDCALL                             
291800     IF X-IDTRANS = '4206'                                                
291900*       KOMMER FRÅN W4020600                                              
292000        MOVE +8                TO AVSR-KDCALL                             
292100     END-IF                                                               
292200     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
292300     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
292400     MOVE SPAR-ARB-KDFRAKT     TO AVSR-KDFRAKT                            
292500     MOVE SPAR-ARB-KDROPACK    TO AVSR-KDROPACK                           
292600     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
292700     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
292800                                                                          
292900     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
293000     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
293100     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
293200     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
293300     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
293400     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
293500     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
293600     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
293700     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
293800     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
293900     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
294000     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
294100     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
294200                                                                          
294300     IF WS-IDDC NOT = W-IDDC-B6                                           
294400        MOVE WS-IDDC TO W-IDDC-B6                                         
294500        PERFORM IMS-GU-WDB601                                             
294600     END-IF                                                               
294700     IF DCS-DDC                                                           
294800       PERFORM S02AA-KOLLA-DIREKTLEVERANS                                 
294900     END-IF                                                               
295000                                                                          
295100     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB AVSR-ORQI-PCB         
295200                         AVSR-GMTB-PCB AVSR-GMTC-PCB                      
295300                         AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB        
295400     .                                                                    
295500     EJECT                                                                
295600 S10C-BERAEKNA-KVSLATT SECTION.                                           
295700                                                                          
295800     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
295900                                                                          
296000        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
296100                                                                          
296200        COMPUTE ORAD-KVSLATT ROUNDED =                                    
296300               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
296400     END-IF                                                               
296500     .                                                                    
296600     EJECT                                                                
296700 S11-SKRIV-VOR-RAD SECTION.                                               
296800                                                                          
296900     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
297000     MOVE OBKR-IDDC            TO WS-IDDC                                 
297100                                                                          
297200     IF OBKR-IDARTNR-TILLK > +0                                           
297300       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                               
297400                                  4542-IDARTNR                            
297500     ELSE                                                                 
297600       MOVE OBKR-IDARTNR       TO W-IDARTNR                               
297700                                  4542-IDARTNR                            
297800     END-IF                                                               
297900     MOVE OBKR-IDDC            TO W-IDDC                                  
298000                                  4542-IDDC                               
298100     PERFORM IMS-GU-WDK722                                                
298200     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
298300        MOVE XLAG-IDANSK       TO WS-IDANSK                               
298400     ELSE                                                                 
298500        PERFORM IMS-10-GU-WDK611                                          
298600        MOVE CLAG-IDANSK       TO WS-IDANSK                               
298700     END-IF                                                               
298800     MOVE WS-IDANSK            TO 4542-IDANSK                             
298900     MOVE +1                   TO 4542-IDLOPNR                            
299000     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
299100     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
299200     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
299300     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
299400     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
299500     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
299600     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
299700     MOVE ZERO                 TO 4542-KDVORATG                           
299800     IF 4542-KDORDBEK = 92 OR 98                                          
299900       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
300000       MOVE 4542-KVBEART       TO 4542-KVBEART-Q                          
300100       MOVE OBKR-KVPREAVB      TO 4542-KVPREAVB                           
300200     ELSE                                                                 
300300       MOVE OBKR-KVBEART       TO 4542-KVBEART-Q                          
300400       MOVE +0                 TO 4542-KVPREAVB                           
300500     END-IF                                                               
300600     EJECT                                                                
300700     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
300800     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
300900     MOVE SPACE                TO 4542-TEVORMRK                           
301000     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
301100     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
301200     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
301300     MOVE +0                   TO 4542-TIUPPDAT                           
301400     MOVE +0                   TO 4542-TIUPPTID                           
301500     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
301600                                                                          
301700                                                                          
301800*---------------------------------------UPPLÄGG TILL NY VORKÖ             
301900*                                       SKER I W40293                     
302000     IF WS-IDDC NOT = W-IDDC-B6                                           
302100        MOVE WS-IDDC TO W-IDDC-B6                                         
302200        PERFORM IMS-GU-WDB601                                             
302300     END-IF                                                               
302400                                                                          
302500     IF  DCS-NDC                                                          
302600         PERFORM IMS-19-ISRT-4541-WDR411                                  
302700         PERFORM UNTIL SEGMENT-FINNS                                      
302800                                                                          
302900            ADD +1             TO 4542-IDLOPNR                            
303000            PERFORM IMS-19-ISRT-4541-WDR411                               
303100         END-PERFORM                                                      
303200                                                                          
303300         PERFORM IMS-11-GHU-WDK711                                        
303400         IF  DCS-NDC-CN                                                   
303500         OR (DCS-NDC-NA AND DCS-USA)                                      
303600           IF SLAG-IDDC-REF = SPACE                                       
303700             PERFORM S11A-STARTA-W2T191X                                  
303800           END-IF                                                         
303900         END-IF                                                           
304000     END-IF                                                               
304100                                                                          
304200     IF (4542-KDORDBEK NOT = 92 AND 98) AND                               
304300        4542-IDLEVNR = SPACE                                              
304400        IF DCS-CDC                                                        
304500          MOVE 4542-IDARTNR    TO W-IDARTNR                               
304600          PERFORM IMS-21-GHU-ARTM-WDK901                                  
304700          COMPUTE ART-KVOKS-VOR =                                         
304800            ART-KVOKS-VOR + (4542-KVBEART-Q - 4542-KVPREAVB)              
304900          PERFORM IMS-22-REPL-ARTM-WDK901                                 
305000        END-IF                                                            
305100     END-IF                                                               
305200     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
305300     .                                                                    
305400     EJECT                                                                
305500                                                                          
305600 S11A-STARTA-W2T191X   SECTION.                                           
305700                                                                          
305800     MOVE +1                    TO 2191-MID-KDCLAGER                      
305900     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
306000     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
306100                                   2191-MID-TISENBEK-KL                   
306200     MOVE SPACE                 TO 2191-MID-IDKR                          
306300     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
306400     MOVE '500'                 TO 2191-MID-KDLARM                        
306500     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
306600     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
306700     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
306800     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
306900     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
307000     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
307100     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
307200     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
307300                                                                          
307400     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
307500     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
307600     MOVE '4213'                TO MSG-IDTRANS-1                          
307700     MOVE '1'                   TO MSG-KDMFSFOR-1                         
307800     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
307900                                                                          
308000     PERFORM IMS-PURG-MSG-2191                                            
308100                                                                          
308200     MOVE SPACE                 TO 2191-MID-W2I19101                      
308300     .                                                                    
308400     EJECT                                                                
308500                                                                          
308600 S12-BYGG-UPP-ORDERRAD SECTION.                                           
308700                                                                          
308800     MOVE OBKR-IDORDER         TO ORAD-IDORDER                            
308900     MOVE OBKR-IDDC            TO ORAD-IDDC                               
309000                                  WS-IDDC                                 
309100     IF WS-IDDC NOT = W-IDDC-B6                                           
309200        MOVE WS-IDDC TO W-IDDC-B6                                         
309300        PERFORM IMS-GU-WDB601                                             
309400     END-IF                                                               
309500                                                                          
309600     IF DCS-SDC OR DCS-NDC                                                
309700       MOVE AREG-IDARTNR       TO W-IDARTNR                               
309800       MOVE ORAD-IDDC          TO W-IDDC                                  
309900       PERFORM IMS-11-GHU-WDK711                                          
310000       MOVE SLAG-ADLAGOMR      TO ORAD-ADLAGOMR                           
310100       MOVE SLAG-ADGANG        TO ORAD-ADGANG                             
310200       MOVE SLAG-ADPLATS       TO ORAD-ADPLATS                            
310300       MOVE DCS-IDLANDX2       TO W-IDLAND                                
310400       IF DCS-NDC                                                         
310500         PERFORM IMS-GU-WDK712                                            
310600         IF SEGMENT-FINNS                                                 
310700            IF LART-KDARTURS > SPACE                                      
310800              MOVE LART-KDARTURS TO ORAD-KDARTURS                         
310900            ELSE                                                          
311000              MOVE AREG-KDARTURS TO ORAD-KDARTURS                         
311100            END-IF                                                        
311200            IF LART-VKART > ZERO AND                                      
311300               LART-VKART NOT = AREG-VKART                                
311400               MOVE LART-VKART     TO ORAD-VKART                          
311500                                      ORAD-VKART-NTO                      
311600            ELSE                                                          
311700               MOVE AREG-VKART     TO ORAD-VKART                          
311800               MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                      
311900            END-IF                                                        
312000            IF LART-VLARTNTO > 0                                          
312100               MOVE LART-VLARTNTO TO ORAD-VLARTNTO                        
312200            ELSE                                                          
312300               MOVE AREG-VLARTNTO TO ORAD-VLARTNTO                        
312400            END-IF                                                        
312500         ELSE                                                             
312600            MOVE AREG-KDARTURS  TO ORAD-KDARTURS                          
312700            MOVE AREG-VKART     TO ORAD-VKART                             
312800            MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                         
312900            MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                          
313000         END-IF                                                           
313100       ELSE                                                               
313200          MOVE AREG-KDARTURS   TO ORAD-KDARTURS                           
313300          MOVE AREG-VKART      TO ORAD-VKART                              
313400          MOVE AREG-VKART-NTO  TO ORAD-VKART-NTO                          
313500          MOVE AREG-VLARTNTO   TO ORAD-VLARTNTO                           
313600       END-IF                                                             
313700     ELSE                                                                 
313800       MOVE AREG-ADLAGOMR      TO ORAD-ADLAGOMR                           
313900       MOVE AREG-ADGANG        TO ORAD-ADGANG                             
314000       MOVE AREG-ADPLATS       TO ORAD-ADPLATS                            
314100       MOVE AREG-KDARTURS      TO ORAD-KDARTURS                           
314200       MOVE AREG-VKART         TO ORAD-VKART                              
314300       MOVE AREG-VKART-NTO     TO ORAD-VKART-NTO                          
314400       MOVE AREG-VLARTNTO      TO ORAD-VLARTNTO                           
314500     END-IF                                                               
314600     MOVE AREG-IDARTNR         TO ORAD-IDARTNR                            
314700     MOVE +1                   TO ORAD-IDLOPNR                            
314800     MOVE OBKR-BERADREF        TO ORAD-BERADREF                           
314900     MOVE OBKR-BEVOLREF        TO ORAD-BEVOLREF                           
315000     MOVE OBKR-FLAKPLOC        TO ORAD-FLAKPLOC                           
315100     MOVE OBKR-FLINVEST        TO ORAD-FLINVEST                           
315200     MOVE OBKR-FLOBTRAN        TO ORAD-FLOBTRAN                           
315300     MOVE OBKR-FLPRTILL        TO ORAD-FLPRTILL                           
315400     MOVE OBKR-FLRESTN         TO ORAD-FLRESTN                            
315500     MOVE OBKR-FLTILLK         TO ORAD-FLTILLK                            
315600     MOVE 'N'                  TO ORAD-FLSDCLEV                           
315700     MOVE OBKR-IDDC-RO         TO ORAD-IDDC-RO                            
315800     MOVE OBKR-IDDISTR         TO ORAD-IDDISTR                            
315900     MOVE OBKR-IDKUNDNR        TO ORAD-IDKUNDNR                           
316000     MOVE OBKR-IDKUNDRF        TO ORAD-IDKUNDRF                           
316100     MOVE OBKR-IDKAMPRF        TO ORAD-IDKAMPRF                           
316200     MOVE OBKR-IDLEVNR         TO ORAD-IDLEVNR                            
316300     EJECT                                                                
316400     MOVE OBKR-IDLOPNR-RO      TO ORAD-IDLOPNR-RO                         
316500     MOVE OBKR-IDKUNDRF-RO     TO ORAD-IDKUNDRF-RO                        
316600     MOVE +0                   TO ORAD-IDSPECEMB                          
316700     MOVE OBKR-IDSYSTEM        TO ORAD-IDSYSTEM                           
316800     MOVE OBKR-KDDSP           TO ORAD-KDDSP                              
316900     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
317000     MOVE OBKR-KDKVBRYT        TO ORAD-KDKVBRYT                           
317100     MOVE OBKR-KDOI            TO ORAD-KDOI                               
317200     MOVE OBKR-CLEARGROUP      TO ORAD-CLEARGROUP                         
317300     MOVE JA                   TO ORAD-FLORDING                           
317400     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
317500     MOVE OBKR-KDORDKL         TO ORAD-KDORDKL                            
317600     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
317700     MOVE OBKR-KDPRTYP         TO ORAD-KDPRTYP                            
317800     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
317900     MOVE OBKR-KDTPOTYP        TO ORAD-KDTPOTYP                           
318000     MOVE OBKR-KDVRINFO        TO ORAD-KDVRINFO                           
318100     MOVE OBKR-KVPRERO         TO ORAD-KVPRERO                            
318200     MOVE +0                   TO ORAD-KVOKS-PREL                         
318300       MOVE OBKR-KVPREAVB      TO ORAD-KVPREAVB                           
318400     MOVE OBKR-KVBEART         TO ORAD-KVBEART                            
318500     IF OBKR-KDORDBEK = 92 OR 98                                          
318600       COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO                 
318700       MOVE OBKR-KVPREAVB      TO ORAD-KVBEART-Q                          
318800       MOVE +0                 TO ORAD-KVPRERO                            
318900     ELSE                                                                 
319000       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
319100          COMPUTE ORAD-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO           
319200       ELSE                                                               
319300          MOVE OBKR-KVBEART-Q  TO ORAD-KVBEART-Q                          
319400       END-IF                                                             
319500     END-IF                                                               
319600     EJECT                                                                
319700     MOVE OBKR-KVSLATT         TO ORAD-KVSLATT                            
319800     MOVE OBKR-PRARTNTO        TO ORAD-PRARTNTO                           
319900     MOVE OBKR-DEAL-PR-LINE    TO ORAD-DEAL-PR-LINE                       
320000***VID T.EX ERSATTA BIPACKNINGSRADER, TPO6'OR ETC.                        
320100*** DE HAR INGEN PRISFRÅGA ÄNNU.                                          
320200     IF DIST79-DEALER-PRICE AND                                           
320300        (OBKR-PRARTNTO-LOC = +0 AND OBKR-PRARTNTO-LOCPREL = +0)           
320400        PERFORM S26-ADD-PRICE-Q-LINE                                      
320500     END-IF                                                               
320600     MOVE OBKR-PRBPRIS         TO ORAD-PRBPRIS                            
320700     MOVE AREG-REKSIFFR        TO ORAD-REKSIFFR                           
320800     MOVE OBKR-RERF-RAD        TO ORAD-RERF-RAD                           
320900     MOVE OBKR-TIPRIS          TO ORAD-TIPRIS                             
321000     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
321100     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
321200     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
321300     IF OBKR-KDORDBEK = 10                                                
321400        MOVE OBKR-TIRODAT      TO ORAD-TIRODAT                            
321500     ELSE                                                                 
321600        MOVE +0                TO ORAD-TIRODAT                            
321700     END-IF                                                               
321800     MOVE OBKR-TITPO           TO ORAD-TITPO                              
321900     MOVE SPACE                TO ORAD-IDBIL                              
322000                                  ORAD-IDKLIENT                           
322100                                  ORAD-IDARBREF                           
322200                                  ORAD-IDVIN                              
322300                                                                          
322400     MOVE OBKR-IDKUNDRF-WIP    TO ORAD-IDKUNDRF-WIP                       
322500     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
322600     MOVE OBKR-KDVALISO        TO ORAD-KDVALISO                           
322700     .                                                                    
322800     EJECT                                                                
322900                                                                          
323000 S13-HITTA-FORSTA-I-GRUPPEN SECTION.                                      
323100                                                                          
323200     MOVE WS-AKT-IDARTNR       TO WS-IDARTNR-SPAR                         
323300     MOVE WS-AKT-IDDC          TO WS-IDDC-SPAR                            
323400     MOVE WS-AKT-IDLOPNR       TO WS-IDLOPNR-SPAR                         
323500     MOVE WS-AKT-IDARTNR-URS   TO WS-IDARTNR-URS-SPAR                     
323600                                                                          
323700     SUBTRACT 1 FROM WS-INDEX-MID                                         
323800     IF WS-INDEX-MID NOT = +0                                             
323900        MOVE MID-RAD(WS-INDEX-MID)                                        
324000                               TO WS-AKTUELL-MID-RAD                      
324100        PERFORM UNTIL WS-INDEX-MID = +0 OR                                
324200                      WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR             
324300                      WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR             
324400                      WS-AKT-IDARTNR-URS NOT =                            
324500                      WS-IDARTNR-URS-SPAR                                 
324600           SUBTRACT 1        FROM WS-INDEX-MID                            
324700           IF WS-INDEX-MID NOT = +0                                       
324800              MOVE MID-RAD(WS-INDEX-MID)                                  
324900                               TO WS-AKTUELL-MID-RAD                      
325000           END-IF                                                         
325100        END-PERFORM                                                       
325200     END-IF                                                               
325300     .                                                                    
325400     EJECT                                                                
325500                                                                          
325600 S14-LAS-ARTIKELREG SECTION.                                              
325700                                                                          
325800     IF OBKR-IDARTNR-TILLK > +0                                           
325900       MOVE OBKR-IDARTNR-TILLK TO AREG-IDARTNR                            
326000     ELSE                                                                 
326100       MOVE OBKR-IDARTNR       TO AREG-IDARTNR                            
326200     END-IF                                                               
326300                                                                          
326400     CALL W411AREG USING AREG-W411AREG                                    
326500                         AREG-WDK6-PCB                                    
326600                         AREG-WDK7-PCB                                    
326700     .                                                                    
326800     EJECT                                                                
326900                                                                          
327000 S22-HAMTA-BENAMNING SECTION.                                             
327100                                                                          
327200     IF OBKR-IDARTNR-TILLK > +0                                           
327300        MOVE OBKR-IDARTNR-TILLK                                           
327400                               TO W-IDARTNR                               
327500     ELSE                                                                 
327600        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
327700     END-IF                                                               
327800                                                                          
327900     MOVE OHUV-IDSKYLT         TO W-IDSKYLT                               
328000                                                                          
328100     PERFORM IMS-23-GU-BENA-WDD311                                        
328200     IF SEGMENT-FINNS                                                     
328300        MOVE TEXT-BEART        TO MOD-BEART(WS-INDEX-MOD)                 
328400     ELSE                                                                 
328500        MOVE SPACE             TO MOD-BEART(WS-INDEX-MOD)                 
328600     END-IF                                                               
328700     .                                                                    
328800     EJECT                                                                
328900 S23-DATA-TILL-DEL-NOTE SECTION.                                          
329000                                                                          
329100     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
329200     IF DIST07-USA-RETAILER-DNOTE                                         
329300     OR DIST07-CAN-RETAILER                                               
329400                                                                          
329500        INITIALIZE DNOT-ORDER-INFO                                        
329600                                                                          
329700        MOVE IDPGM                    TO DNOT-IDPGM                       
329800        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
329900                                                                          
330000        CALL W411DNOT USING DNOT-W411DNOT                                 
330100                            DNOT-ORQP-PCB                                 
330200                            DNOT-ORQP2-PCB                                
330300                            DNOT-ORQP3-PCB                                
330400                            DNOT-4013-PCB                                 
330500                            DNOT-BENA-PCB                                 
330600     END-IF                                                               
330700     .                                                                    
330800     EJECT                                                                
330900 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
331000                                                                          
331100     IF DIST79-DEALER-PRICE                                               
331200       IF OBKR-IDPRQUES > ZERO                                            
331300         INITIALIZE PRQU-W335PRQU                                         
331400         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
331500         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
331600         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
331700         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
331800         MOVE OBKR-KVBEART-Q     TO PRQU-KVBEART-Q                        
331900         MOVE 5                  TO PRQU-KDCALL                           
332000         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
332100                                            PRQU-WDC7-PCB                 
332200                                            PRQU-SJKO-WDK6-PCB            
332300       END-IF                                                             
332400     END-IF                                                               
332500     .                                                                    
332600     EJECT                                                                
332700 S25-DELETE-PRICE-Q-LINE SECTION.                                         
332800                                                                          
332900     IF DIST79-DEALER-PRICE                                               
333000       IF OBKR-IDPRQUES > ZERO                                            
333100         INITIALIZE PRQU-W335PRQU                                         
333200         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
333300         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
333400         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
333500         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
333600         MOVE 4                  TO PRQU-KDCALL                           
333700         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
333800                                            PRQU-WDC7-PCB                 
333900                                            PRQU-SJKO-WDK6-PCB            
334000       END-IF                                                             
334100     END-IF                                                               
334200     .                                                                    
334300     EJECT                                                                
334400 S26-ADD-PRICE-Q-LINE SECTION.                                            
334500                                                                          
334600     IF DIST79-DEALER-PRICE                                               
334700       IF OBKR-PRARTNTO-LOC = +0    AND                                   
334800          OBKR-PRARTNTO-LOCPREL = +0                                      
334900         IF OBKR-IDPRQUES       = +0                                      
335000********* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                 
335100           MOVE +0                    TO PRNO-IDPRQUES-IN                 
335200           MOVE +1                    TO PRNO-KDCALL                      
335300                                                                          
335400           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
335500                                                                          
335600**********UPPDATERAR WDC7 MED EN PRISFRÅGA                                
335700           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
335800           MOVE +1                    TO PRQU-KDCALL                      
335900                                                                          
336000           MOVE W-IDDISTR             TO PRQU-IDDISTR                     
336100           MOVE W-IDKUNDNR            TO PRQU-IDKUNDNR                    
336200           MOVE W-IDKUNDRF            TO PRQU-IDKUNDRF                    
336300           MOVE OBKR-IDORDER          TO PRQU-IDORDER                     
336400           MOVE OBKR-KDORDKL          TO PRQU-KDORDKL                     
336500           MOVE 'N'                   TO PRQU-KDPRSTA                     
336600           MOVE OBKR-IDARTNR          TO PRQU-IDARTNR                     
336700           MOVE OBKR-KVBEART-Q        TO PRQU-KVBEART-Q                   
336800           MOVE OBKR-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                
336900           MOVE +0                    TO PRQU-PRARTNTO-LOCPREL            
337000           MOVE OBKR-IDSYSTEM         TO PRQU-IDSYSTEM                    
337100                                                                          
337200                                                                          
337300           MOVE OBKR-KDVALISO          TO PRQU-KDVALISO                   
337400                                                                          
337500                                                                          
337600           CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                
337700                                           PRQU-WDC7-PCB                  
337800                                           PRQU-SJKO-WDK6-PCB             
337900                                                                          
338000           MOVE PRQU-IDPRQUES         TO  ORAD-IDPRQUES                   
338100           MOVE PRQU-FLPRTILL         TO  ORAD-FLPRTILL                   
338200                                                                          
338300           MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL           
338400           MOVE OBKR-PRARTNTO-LOC     TO  ORAD-PRARTNTO-LOC               
338500                                                                          
338600*** UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                    
338700                                                                          
338800           MOVE PRQU-IDPRQUES         TO  PRNO-IDPRQUES-IN                
338900           MOVE +3                    TO  PRNO-KDCALL                     
339000                                                                          
339100           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
339200***SKICKA PRISFRÅGA                                                       
339300*          PERFORM S26C-SKICKA-PRISFRAGA                                  
339400         END-IF                                                           
339500       END-IF                                                             
339600     END-IF                                                               
339700     .                                                                    
339800                                                                          
339900 S26C-SKICKA-PRISFRAGA SECTION.                                           
340000                                                                          
340100     MOVE 1                      TO 3039-REQU-IDMSGVER                    
340200     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
340300     MOVE 'W4021300'             TO 3039-REQU-IDUSER                      
340400                                                                          
340500     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
340600     MOVE OBKR-IDDISTR           TO 3039-MID-IDDISTR                      
340700     MOVE OBKR-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
340800     MOVE OBKR-IDORDNR7          TO 3039-MID-IDBUNDLE                     
340900     MOVE PRQU-IDPRQUES          TO 3039-MID-IDPRQUES                     
341000                                                                          
341100     PERFORM S27-SKICKA-OPEN                                              
341200     PERFORM S27-SKICKA-MEDDELANDE                                        
341300     PERFORM S27-SKICKA-CLOSE                                             
341400     .                                                                    
341500                                                                          
341600 S27-SKICKA-OPEN SECTION.                                                 
341700                                                                          
341800     MOVE 'OPEN'                     TO SEND-KDFUNC                       
341900     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
342000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
342100                                                                          
342200     IF SEND-KDRC > 0                                                     
342300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
342400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
342500       DELIMITED BY SIZE INTO FELTEXT                                     
342600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
342700     END-IF                                                               
342800     .                                                                    
342900     SKIP3                                                                
343000 S27-SKICKA-MEDDELANDE SECTION.                                           
343100                                                                          
343200     MOVE 'PUT'                      TO SEND-KDFUNC                       
343300     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
343400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
343500                                                                          
343600     IF SEND-KDRC > 0                                                     
343700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
343800       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
343900       DELIMITED BY SIZE INTO FELTEXT                                     
344000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
344100     END-IF                                                               
344200     .                                                                    
344300     SKIP3                                                                
344400 S27-SKICKA-CLOSE SECTION.                                                
344500                                                                          
344600     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
344700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
344800                                                                          
344900     IF SEND-KDRC > 0                                                     
345000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
345100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
345200       DELIMITED BY SIZE INTO FELTEXT                                     
345300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
345400     END-IF                                                               
345500     .                                                                    
345600     EJECT                                                                
345700 MFS-ROER-EJ-BILD SECTION.                                                
345800                                                                          
345900     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLANNULL                            
346000                                  MOD-KDORDKL-UT                          
346100                                                                          
346200     MOVE +1                   TO WS-INDEX                                
346300     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
346400                                                                          
346500        PERFORM MFS-SAETT-ATTRIBUT                                        
346600                                                                          
346700        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
346800                                  MOD-ASTERIX(WS-INDEX)                   
346900                                  MOD-KDBEHX(WS-INDEX)                    
347000                                  MOD-IDARTNR(WS-INDEX)                   
347100                                  MOD-BEART(WS-INDEX)                     
347200                                  MOD-IDDC-RAD(WS-INDEX)                  
347300                                  MOD-KVANTAL(WS-INDEX)                   
347400                                  MOD-KVQPACK(WS-INDEX)                   
347500                                  MOD-IDKUNDRF-RO(WS-INDEX)               
347600                                  MOD-KEYS(WS-INDEX)                      
347700        ADD +1                 TO WS-INDEX                                
347800     END-PERFORM                                                          
347900     .                                                                    
348000     EJECT                                                                
348100 MFS-SAETT-ATTRIBUT SECTION.                                              
348200                                                                          
348300     MOVE MID-RAD(WS-INDEX) TO WS-AKTUELL-MID-RAD                         
348400                                                                          
348500     IF WS-AKT-KDBEHX = 'B'                                               
348600       IF (WS-AKT-KDORDBEK = 41 OR 61) AND                                
348700                    (WS-AKT-IDARTNR NOT = WS-AKT-IDARTNR-URS)             
348800         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
348900                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
349000       ELSE                                                               
349100         IF WS-AKT-KDORDBEK      = 21 OR 51 OR                            
349200                                   41 OR 52 OR 53 OR 54 OR                
349300                                   55 OR 57 OR 58 OR 59 OR 66 OR          
349400                                   67 OR 72 OR 73 OR 74 OR 75 OR          
349500                                   76 OR 80 OR 81 OR 82 OR 85 OR          
349600                                   61                                     
349700           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX)             
349800         END-IF                                                           
349900                                                                          
350000         IF WS-AKT-KDORDBEK = 41 OR 61                                    
350100           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
350200                             TO MOD-IDDC-ATTR(WS-INDEX)                   
350300                                MOD-KVANTAL-ATTR(WS-INDEX)                
350400                                MOD-KVQPACK-ATTR(WS-INDEX)                
350500                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
350600         END-IF                                                           
350700         IF WS-AKT-KDORDBEK = 57 AND  WS-AKT-IDARTNR = ZERO               
350800            MOVE MFS-STAENG-FAELT-OSYNLIGT                                
350900                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
351000                                MOD-IDDC-ATTR(WS-INDEX)                   
351100                                MOD-KVANTAL-ATTR(WS-INDEX)                
351200                                MOD-KVQPACK-ATTR(WS-INDEX)                
351300                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
351400         END-IF                                                           
351500       END-IF                                                             
351600     EJECT                                                                
351700     ELSE                                                                 
351800       IF WS-AKT-KDORDBEK = 41 OR 61                                      
351900         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
352000                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
352100         IF (WS-AKT-KDORDBEK = 61) AND                                    
352200                  WS-AKT-IDARTNR = ZERO                                   
352300           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
352400                           TO MOD-KDBEHX-ATTR(WS-INDEX)                   
352500                              MOD-IDDC-ATTR(WS-INDEX)                     
352600                              MOD-KVANTAL-ATTR(WS-INDEX)                  
352700                              MOD-KVQPACK-ATTR(WS-INDEX)                  
352800                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX)              
352900         END-IF                                                           
353000       END-IF                                                             
353100     END-IF                                                               
353200     .                                                                    
353300     EJECT                                                                
353400                                                                          
353500 MFS-RENSA-MOD-RADER SECTION.                                             
353600                                                                          
353700     MOVE +1                 TO WS-INDEX                                  
353800     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
353900        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
354000                                MOD-ASTERIX(WS-INDEX)                     
354100                                MOD-KDBEHX(WS-INDEX)                      
354200                                MOD-IDARTNR(WS-INDEX)                     
354300                                MOD-BEART(WS-INDEX)                       
354400                                MOD-IDDC-RAD(WS-INDEX)                    
354500                                MOD-KVANTAL(WS-INDEX)                     
354600                                MOD-KVQPACK(WS-INDEX)                     
354700                                MOD-IDKUNDRF-RO(WS-INDEX)                 
354800                                MOD-KEYS(WS-INDEX)                        
354900        ADD 1                TO WS-INDEX                                  
355000     END-PERFORM                                                          
355100     .                                                                    
355200                                                                          
355300 MFS-RENSA-ALLA-FAELT SECTION.                                            
355400                                                                          
355500     MOVE MFS-RENSA-FAELT    TO MOD-FLANNULL                              
355600                                                                          
355700     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
355800                                MOD-IDLOPNR-NEXT                          
355900                                MOD-IDSEKVNR-NEXT                         
356000                                MOD-IDDC-NEXT                             
356100                                MOD-KDORDBEK-NEXT                         
356200                                                                          
356300     PERFORM MFS-RENSA-MOD-RADER                                          
356400     .                                                                    
356500     EJECT                                                                
356600                                                                          
356700 IMS-GET-MSG SECTION.                                                     
356800                                                                          
356900     MOVE '  QC' TO GODK-STATUSKODER                                      
357000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
357100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
357200     PERFORM IMS-STATUSKONTROLL                                           
357300     .                                                                    
357400     SKIP3                                                                
357500 IMS-INSERT-MSG SECTION.                                                  
357600                                                                          
357700     IF NOT ENGLISH-TEXT                                                  
357800       MOVE '0' TO MFS-KDHUVOMR                                           
357900     END-IF                                                               
358000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
358100     MOVE SPACE TO GODK-STATUSKODER                                       
358200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
358300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
358400     PERFORM IMS-STATUSKONTROLL                                           
358500     .                                                                    
358600                                                                          
358700 IMS-INSERT-4212-MSG SECTION.                                             
358800                                                                          
358900     IF NOT ENGLISH-TEXT                                                  
359000       MOVE '0' TO MFS-KDHUVOMR                                           
359100     END-IF                                                               
359200     MOVE LOW-VALUE TO 4212-Z1 4212-Z2                                    
359300     MOVE SPACE TO GODK-STATUSKODER                                       
359400     CALL CBLTDLI USING ISRT 4212-PCB 4212-MSG-IO-AREA                    
359500     MOVE 4212-STATUS-CODE TO STATUS-WS                                   
359600     PERFORM IMS-STATUSKONTROLL                                           
359700     .                                                                    
359800     EJECT                                                                
359900 IMS-INSERT-4292-MSG SECTION.                                             
360000                                                                          
360100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
360200       MOVE '0' TO MFS-KDHUVOMR                                           
360300     END-IF                                                               
360400     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
360500     MOVE SPACE TO GODK-STATUSKODER                                       
360600     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
360700     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
360800     PERFORM IMS-STATUSKONTROLL                                           
360900     .                                                                    
361000     SKIP2                                                                
361100                                                                          
361200 IMS-INSERT-4298-MSG SECTION.                                             
361300                                                                          
361400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
361500       MOVE '0' TO MFS-KDHUVOMR                                           
361600     END-IF                                                               
361700     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
361800     MOVE SPACE TO GODK-STATUSKODER                                       
361900     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
362000     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
362100     PERFORM IMS-STATUSKONTROLL                                           
362200     .                                                                    
362300     SKIP2                                                                
362400 IMS-INSERT-4213-MSG SECTION.                                             
362500                                                                          
362600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
362700       MOVE '0' TO MFS-KDHUVOMR                                           
362800     END-IF                                                               
362900     MOVE SPACE TO GODK-STATUSKODER                                       
363000     CALL CBLTDLI USING ISRT 4213-PCB 4213-MSG-IO-AREA                    
363100     MOVE 4213-STATUS-CODE TO STATUS-WS                                   
363200     PERFORM IMS-STATUSKONTROLL                                           
363300     .                                                                    
363400     EJECT                                                                
363500                                                                          
363600 IMS-PURG-MSG-2191  SECTION.                                              
363700     MOVE SPACE TO GODK-STATUSKODER                                       
363800     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
363900     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
364000     PERFORM IMS-STATUSKONTROLL                                           
364100     .                                                                    
364200     EJECT                                                                
364300                                                                          
364400 IMS-01-GHU-ORQM-WDQ101-FOERE SECTION.                                    
364500                                                                          
364600     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
364700                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
364800                    '&FLOBOK   =' NEJ ')'                                 
364900          DELIMITED BY SIZE INTO SSA1                                     
365000     MOVE '  GE'               TO GODK-STATUSKODER                        
365100     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
365200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
365300     PERFORM IMS-STATUSKONTROLL                                           
365400     .                                                                    
365500     SKIP2                                                                
365600 IMS-02-GHN-ORQM-WDQ101-FOERE SECTION.                                    
365700                                                                          
365800     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
365900                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
366000                    '&FLOBOK   =' NEJ ')'                                 
366100          DELIMITED BY SIZE INTO SSA1                                     
366200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
366300     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
366400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
366500     PERFORM IMS-STATUSKONTROLL                                           
366600     .                                                                    
366700     SKIP2                                                                
366800 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
366900                                                                          
367000     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
367100                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
367200                    '&FLOBOK   =' NEJ ')'                                 
367300          DELIMITED BY SIZE INTO SSA1                                     
367400     MOVE '  GE'               TO GODK-STATUSKODER                        
367500     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
367600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
367700     PERFORM IMS-STATUSKONTROLL                                           
367800     .                                                                    
367900     EJECT                                                                
368000 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
368100                                                                          
368200     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
368300                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
368400                    '&FLOBOK   =' NEJ ')'                                 
368500          DELIMITED BY SIZE INTO SSA1                                     
368600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
368700     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
368800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
368900     PERFORM IMS-STATUSKONTROLL                                           
369000     .                                                                    
369100     SKIP2                                                                
369200 IMS-05-GHU-ORQM-WDQ101-UNIK SECTION.                                     
369300                                                                          
369400     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
369500                    '&FLOBOK   =' NEJ ')'                                 
369600          DELIMITED BY SIZE  INTO SSA1                                    
369700     MOVE '  GE'               TO GODK-STATUSKODER                        
369800     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
369900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
370000     PERFORM IMS-STATUSKONTROLL                                           
370100     .                                                                    
370200     SKIP2                                                                
370300                                                                          
370400 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
370500                                                                          
370600     MOVE '    '               TO GODK-STATUSKODER                        
370700     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
370800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
370900     PERFORM IMS-STATUSKONTROLL                                           
371000     .                                                                    
371100     EJECT                                                                
371200 IMS-07-ISRT-ORQM-WDQ101 SECTION.                                         
371300                                                                          
371400     MOVE 'WLORQM01 '          TO SSA1                                    
371500     MOVE '  '                 TO GODK-STATUSKODER                        
371600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-ORQM SSA1               
371700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
371800     PERFORM IMS-STATUSKONTROLL                                           
371900     .                                                                    
372000     SKIP2                                                                
372100 IMS-08-DLET-ORQM-WDQ101 SECTION.                                         
372200                                                                          
372300     MOVE '    '               TO GODK-STATUSKODER                        
372400     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-ORQM                    
372500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
372600     PERFORM IMS-STATUSKONTROLL                                           
372700     .                                                                    
372800     SKIP2                                                                
372900 IMS-09-GU-ORQI-WDQ201 SECTION.                                           
373000                                                                          
373100     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
373200          DELIMITED BY SIZE INTO SSA1                                     
373300     MOVE '  GE'               TO GODK-STATUSKODER                        
373400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
373500     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
373600     PERFORM IMS-STATUSKONTROLL                                           
373700     .                                                                    
373800     EJECT                                                                
373900 IMS-10-GU-WDK611 SECTION.                                                
374000                                                                          
374100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
374200           DELIMITED BY SIZE INTO SSA1                                    
374300     MOVE   'WDK611  '         TO SSA2                                    
374400     MOVE '  '                 TO GODK-STATUSKODER                        
374500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
374600     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
374700     PERFORM IMS-STATUSKONTROLL                                           
374800     .                                                                    
374900     EJECT                                                                
375000 IMS-11-GHU-WDK711 SECTION.                                               
375100                                                                          
375200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
375300          DELIMITED BY SIZE  INTO SSA1                                    
375400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
375500          DELIMITED BY SIZE  INTO SSA2                                    
375600     MOVE '  GE'               TO GODK-STATUSKODER                        
375700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
375800     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
375900     PERFORM IMS-STATUSKONTROLL                                           
376000     .                                                                    
376100     SKIP3                                                                
376200 IMS-GU-WDK712           SECTION.                                         
376300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
376400            DELIMITED BY SIZE INTO SSA1                                   
376500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
376600            DELIMITED BY SIZE INTO SSA2                                   
376700     MOVE '  GE' TO GODK-STATUSKODER                                      
376800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
376900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
377000     PERFORM IMS-STATUSKONTROLL                                           
377100     .                                                                    
377200     SKIP3                                                                
377300 IMS-GU-WDK722 SECTION.                                                   
377400                                                                          
377500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
377600          DELIMITED BY SIZE  INTO SSA1                                    
377700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
377800          DELIMITED BY SIZE  INTO SSA2                                    
377900     MOVE 'WDK722 '            TO SSA3                                    
378000     MOVE '  GE'               TO GODK-STATUSKODER                        
378100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
378200     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
378300     PERFORM IMS-STATUSKONTROLL                                           
378400     .                                                                    
378500     SKIP3                                                                
378600 IMS-12-REPL-WDK711 SECTION.                                              
378700                                                                          
378800     MOVE '    '               TO GODK-STATUSKODER                        
378900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
379000     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
379100     PERFORM IMS-STATUSKONTROLL                                           
379200     .                                                                    
379300     EJECT                                                                
379400 IMS-13-GHNP-ORQI-WDQ212-UNIK SECTION.                                    
379500                                                                          
379600     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
379700          DELIMITED BY SIZE  INTO SSA1                                    
379800     MOVE '  GE'               TO GODK-STATUSKODER                        
379900     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1             
380000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
380100     PERFORM IMS-STATUSKONTROLL                                           
380200     .                                                                    
380300     SKIP2                                                                
380400 IMS-14-GNP-ORQI-WDQ212 SECTION.                                          
380500                                                                          
380600     MOVE 'WLORQI12 '          TO SSA1                                    
380700     MOVE '    '               TO GODK-STATUSKODER                        
380800     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1              
380900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
381000     PERFORM IMS-STATUSKONTROLL                                           
381100     .                                                                    
381200     SKIP2                                                                
381300 IMS-15-REPL-ORQI SECTION.                                                
381400                                                                          
381500     MOVE '    '               TO GODK-STATUSKODER                        
381600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ORQI12                  
381700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
381800     PERFORM IMS-STATUSKONTROLL                                           
381900     .                                                                    
382000     SKIP2                                                                
382100 IMS-17-ISRT-ORQF-WDQ401 SECTION.                                         
382200                                                                          
382300     MOVE 'WLORQF01 '          TO SSA1                                    
382400     MOVE '  II'               TO GODK-STATUSKODER                        
382500     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORQF SSA1               
382600     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
382700     PERFORM IMS-STATUSKONTROLL                                           
382800     .                                                                    
382900     EJECT                                                                
383000 IMS-18A-GU-SATB-WDJ111-01 SECTION.                                       
383100                                                                          
383200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
383300          DELIMITED BY SIZE INTO SSA1                                     
383400     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
383500          DELIMITED BY SIZE INTO SSA2                                     
383600     MOVE '  GE'               TO GODK-STATUSKODER                        
383700     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
383800     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
383900     PERFORM IMS-STATUSKONTROLL                                           
384000     .                                                                    
384100     SKIP2                                                                
384200 IMS-18-GN-SATB-WDJ111-01 SECTION.                                        
384300                                                                          
384400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
384500          DELIMITED BY SIZE INTO SSA1                                     
384600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
384700          DELIMITED BY SIZE INTO SSA2                                     
384800     MOVE '  GE'               TO GODK-STATUSKODER                        
384900     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
385000     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
385100     PERFORM IMS-STATUSKONTROLL                                           
385200     .                                                                    
385300     SKIP2                                                                
385400 IMS-19-ISRT-4541-WDR411 SECTION.                                         
385500                                                                          
385600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
385700          DELIMITED BY SIZE INTO SSA1                                     
385800     MOVE 'WDGX4542 '          TO SSA2                                    
385900     MOVE '  II'               TO GODK-STATUSKODER                        
386000     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
386100     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
386200     PERFORM IMS-STATUSKONTROLL                                           
386300     .                                                                    
386400     EJECT                                                                
386500 IMS-21-GHU-ARTM-WDK901 SECTION.                                          
386600                                                                          
386700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
386800          DELIMITED BY SIZE  INTO SSA1                                    
386900     MOVE '    '               TO GODK-STATUSKODER                        
387000     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
387100     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
387200     PERFORM IMS-STATUSKONTROLL                                           
387300     .                                                                    
387400     SKIP2                                                                
387500 IMS-22-REPL-ARTM-WDK901 SECTION.                                         
387600                                                                          
387700     MOVE '    '               TO GODK-STATUSKODER                        
387800     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
387900     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
388000     PERFORM IMS-STATUSKONTROLL                                           
388100     .                                                                    
388200     SKIP2                                                                
388300 IMS-23-GU-BENA-WDD311 SECTION.                                           
388400                                                                          
388500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
388600          DELIMITED BY SIZE INTO SSA1                                     
388700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
388800          DELIMITED BY SIZE INTO SSA2                                     
388900     MOVE '  GE'               TO GODK-STATUSKODER                        
389000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
389100     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
389200     PERFORM IMS-STATUSKONTROLL                                           
389300     .                                                                    
389400     EJECT                                                                
389500 IMS-24-GU-ORDP-WDA501 SECTION.                                           
389600                                                                          
389700     STRING 'WLORDP01(WDA501KY=>' W-WDA5KEY-MIN-X                         
389800                    '&WDA501KY=<' W-WDA5KEY-MAX-X ')'                     
389900          DELIMITED BY SIZE  INTO SSA1                                    
390000     MOVE '  GEGB'             TO GODK-STATUSKODER                        
390100     CALL CBLTDLI USING GU  ORDP-PCB DLI-IO-AREA-ORDP SSA1                
390200     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
390300     PERFORM IMS-STATUSKONTROLL                                           
390400     .                                                                    
390500     SKIP2                                                                
390600                                                                          
390700 IMS-26-GHU-ORDP-WDA501 SECTION.                                          
390800                                                                          
390900     STRING 'WLORDP01(WDA501KY =' W-WDA5KEY-X ')'                         
391000          DELIMITED BY SIZE  INTO SSA1                                    
391100     MOVE '    '               TO GODK-STATUSKODER                        
391200     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
391300     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
391400     PERFORM IMS-STATUSKONTROLL                                           
391500     .                                                                    
391600     SKIP2                                                                
391700 IMS-27-REPL-ORDP-WDA501 SECTION.                                         
391800                                                                          
391900     MOVE '    '               TO GODK-STATUSKODER                        
392000     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA-ORDP                    
392100     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
392200     PERFORM IMS-STATUSKONTROLL                                           
392300     .                                                                    
392400     EJECT                                                                
392500 IMS-33-GN-ORQM-WDQ101 SECTION.                                           
392600                                                                          
392700     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
392800                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
392900          DELIMITED BY SIZE INTO SSA1                                     
393000     MOVE '  GEGB'             TO GODK-STATUSKODER                        
393100     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
393200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
393300     PERFORM IMS-STATUSKONTROLL                                           
393400     .                                                                    
393500     EJECT                                                                
393600 IMS-34-GHU-ORQM-WDQ101 SECTION.                                          
393700                                                                          
393800     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
393900                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
394000          DELIMITED BY SIZE INTO SSA1                                     
394100     MOVE '  '                 TO GODK-STATUSKODER                        
394200     CALL CBLTDLI USING GHU  ORQM-PCB DLI-IO-AREA-ORQM SSA1               
394300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
394400     PERFORM IMS-STATUSKONTROLL                                           
394500     .                                                                    
394600     SKIP2                                                                
394700 IMS-35-GHU-PROC-WDE801 SECTION.                                          
394800                                                                          
394900     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
395000          DELIMITED BY SIZE  INTO SSA1                                    
395100     MOVE '  GE'               TO GODK-STATUSKODER                        
395200     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-PROC SSA1                
395300     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
395400     PERFORM IMS-STATUSKONTROLL                                           
395500     .                                                                    
395600     EJECT                                                                
395700 IMS-36-REPL-PROC-WDE801 SECTION.                                         
395800                                                                          
395900     MOVE '    '               TO GODK-STATUSKODER                        
396000     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-PROC                    
396100     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
396200     PERFORM IMS-STATUSKONTROLL                                           
396300     .                                                                    
396400     EJECT                                                                
396500 IMS-GU-WDB601    SECTION.                                                
396600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
396700          DELIMITED BY SIZE INTO SSA1                                     
396800     MOVE '  GE' TO GODK-STATUSKODER                                      
396900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
397000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
397100     PERFORM IMS-STATUSKONTROLL                                           
397200     .                                                                    
397300     SKIP2                                                                
397400 IMS-GU-WDB201 SECTION.                                                   
397500                                                                          
397600     STRING  'WDB201  (IDGMT    =' W-WDB201KEY-X ')'                      
397700            DELIMITED BY SIZE INTO SSA1                                   
397800     MOVE    '    '             TO GODK-STATUSKODER                       
397900     CALL    CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-B201 SSA1              
398000     MOVE    WDB2-STATUS-CODE   TO STATUS-WS                              
398100     PERFORM IMS-STATUSKONTROLL                                           
398200     .                                                                    
398300     EJECT                                                                
398400 IMS-STATUSKONTROLL SECTION.                                              
398500                                                                          
398600     SET STATUS-IX TO 1                                                   
398700     SEARCH GODK-STATUS                                                   
398800       AT END CALL FELLOG                                                 
398900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
399000     END-SEARCH                                                           
399100     .                                                                    
399200     EJECT                                                                
399300*    -COPY WY2000Q1                                                       
