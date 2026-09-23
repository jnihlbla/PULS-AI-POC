000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4020300.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN                                        
000400 DATE-WRITTEN.   DECEMBER 90.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        PROGRAMMET HANTERAR SVARSBILD TILL ORDERREGISTRERING             
000900*        4202                                                             
001000*        VISAR AVVIKELSER.                                                
001100*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001200*                                                                         
001300*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001400*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
001500*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
001600*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
001700*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
001800*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002000*        PROGRAMMET LÄSER      WLKNDG (WDB5)  KUNDREGISTER                
002100*        PROGRAMMET LÄSER      WLXXKK (WDR1)  TVÅNGSSTYRN.TAB             
002200*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002300*        PROGRAMMET LÄSER      WLXXKN (WDR1)  TEDTIDS.TAB                 
002400*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
002500*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
002600*        PROGRAMMET LÄSER      WLXXKR (WDR4)  KAMPANJREGISTER             
002700*        PROGRAMMET LÄSER      WLXXKS (WDR4)  MARKN.REG KAMPANJ           
002800*        PROGRAMMET LÄSER      WLXXKT (WDR4)  ANTALSTAB KAMPANJ           
002900*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003000*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003100*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
003200*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSKALENDER              
003300*        PROGRAMMET LÄSER             (WDB6)  DC REGISTER                 
003400*                                                                         
003500*    INDATA.                                                              
003600*        TRANSAKTION: W4T203                                              
003700*                     W4T203U                                             
003800*                     W4T203V                                             
003900*        MID:         W4I20301                                            
004000*                                                                         
004100*    UTDATA.                                                              
004200*        MOD:         W4O20301                                            
004300*                                                                         
004400*                                                                         
004500*    E'TRACKER: 5444132 DATED 2007-08-21                                  
004600*    E'TRACKER: 7450328 DATED 2008 HÖST  VOHF                             
004700*    E'TRACKER: 8081720 DATED 2009-04-09 ORDER STEERING TO VOHF           
004800*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
004900*                                                                         
005000 ENVIRONMENT DIVISION.                                                    
005100                                                                          
005200 DATA DIVISION.                                                           
005300                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600*    -COPY WY2000W1                                                       
005700     SKIP3                                                                
005800 77  IDPGM                       PIC X(08)   VALUE 'W4021300'.            
005900 77  HOPP                        PIC X(1)   VALUE 'N'.                    
006000 77  SPEC-FORBI                  PIC X(1)   VALUE 'S'.                    
006100 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
006200 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
006300 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
006400 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
006600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006700 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
006800 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
006900 77  4202-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
007000 77  4244-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
007100 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
007200 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
007300 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
007400 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
007500 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
007600 77  WS-INDEX-SATS               PIC S9(9)   COMP SYNC VALUE ZERO.        
007700 77  WS-INDEX-SATS-MAX           PIC S9(9)   COMP SYNC VALUE +5.          
007800 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
007900 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
008000 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
008100 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE 100.         
008200 77  WS-INDEX-CL                 PIC S9(3)   VALUE +1 COMP-3.             
008300 77  WS-INDEX-CL-MAX             PIC S9(3)   VALUE +9 COMP-3.             
008400 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
008500 77  WS-RADER                    PIC  9(2)   VALUE ZERO.                  
008600 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
008700 77  SPAR-IDARTNR-TILLK          PIC S9(9)   VALUE +0  COMP-3.            
008800 77  SPAR-KVBEART-TILLK          PIC S9(7)   VALUE +0  COMP-3.            
008900 77  SPAR-REKSIFFR-TILLK         PIC S9(1)   VALUE +0  COMP-3.            
009000 77  SPAR-DIERS-KVOT          PIC S9(4)V9(3) VALUE +0  COMP-3.            
009100 77  SPAR-IDARTNR-40             PIC S9(9)   VALUE +0  COMP-3.            
009200 77  SPAR-IDLOPNR-40             PIC S9(3)   VALUE +0  COMP-3.            
009300 77  SPAR-IDSEKVNR-40            PIC S9(3)   VALUE +0  COMP-3.            
009400 77  SPAR-KDFRAKT                PIC S9(3)   VALUE +0  COMP-3.            
009500 77  SPAR-KDFDKRAV               PIC S9(3)   VALUE +0  COMP-3.            
009600 77  WS-IDDISTR                  PIC X(4).                                
009700 77  WS-IDKUNDNR                 PIC X(6).                                
009800 77  WS-IDORDNR                  PIC X(5).                                
009900 77  WS-TRANSKOD                 PIC X(6).                                
010000 77  WS-KVSLASK                  PIC S9(7)   VALUE +0  COMP-3.            
010100 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
010200 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
010300 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
010400 77  WS-IDANSK                   PIC  9(3)   VALUE ZERO.                  
010500 77  WS-IDDISTR-NUM4             PIC 9(4).                                
010600 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
010700                                                                          
010800     SKIP2                                                                
010900                                                                          
011000 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
011100*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
011200*    ----DIST79-DEALER-PRICE----                                          
011300     EJECT                                                                
011400 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
011500 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011600     03 WS-TIHHMM                PIC 9(4).                                
011700     03 FILLER                   PIC 9(2).                                
011800     EJECT                                                                
011900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012000     88  ALLT-OK                             VALUE 'J'.                   
012100 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
012200     88  NYCKEL-OK                           VALUE 'J'.                   
012300 77  AKT-SIDA-SW                 PIC X       VALUE 'N'.                   
012400     88  AKT-SIDA                            VALUE 'J'.                   
012500 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
012600     88  AVSLUTA                             VALUE 'J'.                   
012700 77  START-4202-SW               PIC X       VALUE 'N'.                   
012800     88  START-4202                          VALUE 'J'.                   
012900 77  START-4244-SW               PIC X       VALUE 'N'.                   
013000     88  START-4244                          VALUE 'J'.                   
013100 77  NEXT-SATS-SW                PIC X       VALUE 'N'.                   
013200     88  NEXT-SATS                           VALUE 'J'.                   
013300                                                                          
013400 77  BILD-SW                     PIC X(4)    VALUE SPACE.                 
013500     88  GODK-BILD                           VALUE '4202' '4244'.         
013600                                                                          
013700                                                                          
013800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013900     88  EGEN-MID                            VALUE '4203' '4246'.         
014000     88  GODK-MID                            VALUE '4201' '4202'          
014100                                                   '4203' '4204'          
014200                                                   '4244' '4246'.         
014300                                                                          
014400 01  WS-ALFA-6.                                                           
014500     03  WS-NUM-6                PIC 9(6).                                
014600 01  WS-ALFA-9.                                                           
014700     03  WS-NUM-9                PIC 9(9).                                
014800                                                                          
014900 01  WS-IDARTNR-REKSIFFR.                                                 
015000     03  WS-IDARTNR              PIC 9(9).                                
015100     03  FILLER                  PIC X(1)   VALUE '-'.                    
015200     03  WS-REKSIFFR             PIC 9(1).                                
015300                                                                          
015400     EJECT                                                                
015500 01  WS-AKTUELL-MID-RAD.                                                  
015600     03  WS-AKT-KDORDBEK         PIC 9(2).                                
015700     03  WS-AKT-KDBEHX           PIC X(1).                                
015800     03  WS-AKT-IDARTNR          PIC 9(9).                                
015900     03  WS-AKT-FILLER           PIC X(1).                                
016000     03  WS-AKT-REKSIFFR         PIC 9(1).                                
016100     03  WS-AKT-IDDC             PIC X(2).                                
016200     03  WS-AKT-IDKUNDRF-RO      PIC X(7).                                
016300     03  WS-AKT-KEYS.                                                     
016400         05 WS-AKT-IDLOPNR       PIC 9(3).                                
016500         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
016600         05 WS-AKT-IDARTNR-URS   PIC 9(9).                                
016700         05 WS-AKT-IDLOPNR-RO    PIC 9(3).                                
016800                                                                          
016900 01  WS-IDARTNR-SATS-TAB.                                                 
017000     03 WS-IDARTNR-SATS          PIC X(9)  OCCURS 5.                      
017100                                                                          
017200 01  WS-KEYS-SPAR.                                                        
017300     03 WS-IDLOPNR-SPAR          PIC 9(3).                                
017400     03 WS-IDSEKVNR-SPAR         PIC 9(3).                                
017500     03 WS-IDARTNR-URS-SPAR      PIC 9(9).                                
017600     03 WS-IDARTNR-SPAR          PIC 9(9).                                
017700     03 WS-IDDC-SPAR             PIC X(2).                                
017800     EJECT                                                                
017900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018000 01  GENERELLA-SUBPROGRAM.                                                
018100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018500     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
018600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018800*                                                                         
018900*                                                                         
019000*                                                                         
019100 01  GEMENSAMMA-SUBPROGRAM.                                               
019200     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
019300*        LÄSNING ARTIKELREGISTER                                          
019400     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
019500*        KONTROLL ENHETSLAST                                              
019600     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
019700*        KONTROLL/UPPDATERING TPO2                                        
019800     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
019900*        UPPDATERING TPO6                                                 
020000     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
020100*        JUSTERING LAGERPLATS OCH LAGEROMRÅDE                             
020200     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
020300*        WOPS PER RAD                                                     
020400     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
020500*        PRISFRÅGA                                                        
020600     03   W335PRQU               PIC X(8)    VALUE 'W335PRQU'.            
020700*        PRISFRÅGA                                                        
020800     EJECT                                                                
020900*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
021000*   -COPY WSECAREA                                                        
021100     EJECT                                                                
021200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021300*   -COPY WMSGINIT                                                        
021400     EJECT                                                                
021500*   -COPY W402W001                                                        
021600     EJECT                                                                
021700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
021800 01  MESSAGE-CODES.                                                       
021900     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
022000     03  MED-UPPLYSN-UPPDAT-PF   PIC X(3)    VALUE '144'.                 
022100     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
022200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
022300     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
022400     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
022500     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
022600     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
022700     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
022800     SKIP2                                                                
022900*   -COPY WMEDAREA                                                        
023000     EJECT                                                                
023100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
023200*   -COPY W411AREG                                                        
023300*                                                                         
023400     EJECT                                                                
023500*   -COPY W411LAST                                                        
023600*                                                                         
023700     EJECT                                                                
023800*   -COPY W411TPO2                                                        
023900*                                                                         
024000     EJECT                                                                
024100*   -COPY W411TPO6                                                        
024200*                                                                         
024300     EJECT                                                                
024400*   -COPY W413AVSR                                                        
024500*                                                                         
024600     EJECT                                                                
024700*   -COPY W413ADRS                                                        
024800*                                                                         
024900     EJECT                                                                
025000 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
025100*   -COPY W335PRNO                                                        
025200     EJECT                                                                
025300 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
025400*   -COPY W335PRQU                                                        
025500     EJECT                                                                
025600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
025700*                                                                         
025800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025900     SKIP3                                                                
026000*01  MID -COPY W4I20301                                                   
026100     EJECT                                                                
026200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
026300     SKIP3                                                                
026400*01  -COPY WMSGAREA                                                       
026500     EJECT                                                                
026600*    03  MOD -COPY W4O20301   -RED MSG-AREA.                              
026700     EJECT                                                                
026800******************************************************************        
026900*    MID-AREA FÖR W2T191                                         *        
027000******************************************************************        
027100*01  -COPY  W2I19101  -PRE 2191-                                          
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027400     SKIP3                                                                
027500*01  -COPY WMFSAREA                                                       
027600     EJECT                                                                
027700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027900 01  NYCKLAR-TILL-DLI.                                                    
028000                                                                          
028100     03  W-WDQ101-KEY-UNIK.                                               
028200         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
028300         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
028400         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
028500         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
028600         05  W-Q1-IDDC-UNIK      PIC X(2)    VALUE ZERO.                  
028700         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
028800                                                                          
028900     03  W-WDQ101-KEY-MIN.                                                
029000         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
029100         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
029200         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
029300         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
029400         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
029500         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
029600     03  W-WDQ101-KEY-MAX.                                                
029700         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
029800         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
029900         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
030000         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
030100         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
030200         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
030300                                                                          
030400     03  W-WDQ101-KEY-MIN1.                                               
030500         05  W-Q1-IDORDER-MIN1   PIC S9(7)   COMP-3.                      
030600         05  W-Q1-IDARTNR-MIN1   PIC S9(9)   COMP-3.                      
030700         05  W-Q1-IDLOPNR-MIN1   PIC S9(3)   COMP-3.                      
030800         05  W-Q1-IDSEKVNR-MIN1  PIC S9(3)   COMP-3.                      
030900         05  FILLER              PIC X(4)    VALUE LOW-VALUE.             
031000     03  W-WDQ101-KEY-MAX1.                                               
031100         05  W-Q1-IDORDER-MAX1   PIC S9(7)   COMP-3.                      
031200         05  W-Q1-IDARTNR-MAX1   PIC S9(9)   COMP-3.                      
031300         05  W-Q1-IDLOPNR-MAX1   PIC S9(3)   COMP-3.                      
031400         05  W-Q1-IDSEKVNR-MAX1  PIC S9(3)   COMP-3.                      
031500         05  FILLER              PIC X(4)    VALUE HIGH-VALUE.            
031600     EJECT                                                                
031700     03  W-WDJ1CSEQ-X.                                                    
031800         05  W-J1-IDLEVNR        PIC X(5)  VALUE SPACE.                   
031900         05  FILLER              PIC X(30) VALUE SPACE.                   
032000         05  W-J1-IDARTNR        PIC S9(9) VALUE +0 COMP-3.               
032100                                                                          
032200     03  W-IDLEVNR-X             PIC X(5)    VALUE '1002 '.               
032300                                                                          
032400     03  W-WDA5KEY-X.                                                     
032500         05  W-A5-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
032600         05  W-A5-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
032700         05  W-A5-IDKUNDRF       PIC X(10)   VALUE SPACE.                 
032800         05  W-A5-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
032900         05  W-A5-IDLOPNR        PIC S9(3)   VALUE ZERO COMP-3.           
033000                                                                          
033100     03  W-IDGMTREF-X.                                                    
033200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
033300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
033400         05  W-IDKUNDRF.                                                  
033500            07  W-IDORDNR        PIC 9(7)    VALUE ZERO.                  
033600            07  FILLER           PIC X(3)    VALUE SPACE.                 
033700     EJECT                                                                
033800                                                                          
033900     03  W-KDFRAKT-X.                                                     
034000         05  W-KDFRAKT           PIC S9(3)   VALUE ZERO COMP-3.           
034100     03  W-IDARTNR-X.                                                     
034200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
034300     03  W-IDSKYLT-X.                                                     
034400         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
034500     03  W-IDDC-X.                                                        
034600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
034700     03  W-IDLAND-X.                                                      
034800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
034900     03  W-WDGXKEY-4541-X.                                                
035000         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
035100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
035200     SKIP3                                                                
035300     03  W-IDDC-B6-X.                                                     
035400         05 W-IDDC-B6                  PIC X(2).                          
035500     EJECT                                                                
035600*    --- STATUS-KOD FRÅN IMS                                              
035700 01  STATUS-WS                   PIC XX.                                  
035800     88  SEGMENT-FINNS                       VALUE '  '.                  
035900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
036000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036100     88  BASEN-SLUT                          VALUE 'GB'.                  
036200     SKIP2                                                                
036300 01  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
036400     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
036500     SKIP2                                                                
036600 01  GODK-STATUSKODER.                                                    
036700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036800     SKIP3                                                                
036900 01  SSA1                        PIC X(160).                              
037000 01  SSA2                        PIC X(96).                               
037100 01  SSA3                        PIC X(96).                               
037200     EJECT                                                                
037300*    --- IMS FUNKTIONSKODER                                               
037400*01  -COPY W0003                                                          
037500     EJECT                                                                
037600*    ---  DLI INPUT-OUTPUT AREA                                           
037700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
037800     SKIP3                                                                
037900 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQM01  '.         
038000 01  DLI-IO-AREA-ORQM.                                                    
038100     03  WLORQM01.                                                        
038200*        05  -COPY WDQ101                                                 
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQI01  '.         
038600 01  DLI-IO-AREA-ORQI01.                                                  
038700     03  WLORQI01.                                                        
038800*        05  -COPY WDQ201                                                 
038900     EJECT                                                                
039000                                                                          
039100 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQI12  '.         
039200 01  DLI-IO-AREA-ORQI12.                                                  
039300     03  WLORQI12.                                                        
039400*        05  -COPY WDQ212                                                 
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQF01  '.         
039800 01  DLI-IO-AREA-ORQF.                                                    
039900     03  WLORQF01.                                                        
040000*        05  -COPY WDQ401                                                 
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ARTM01  '.         
040400 01  DLI-IO-AREA-ARTM.                                                    
040500     03  WLARTM01.                                                        
040600*        05  -COPY WDK901                                                 
040700     EJECT                                                                
040800                                                                          
040900                                                                          
041000 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-WDK711  '.         
041100 01  DLI-IO-AREA-WDK7.                                                    
041200*    03  -COPY WDK711                                                     
041300     EJECT                                                                
041400                                                                          
041500 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-WDK712  '.         
041600 01  DLI-IO-AREA-WDK712.                                                  
041700*    03  -COPY WDK712                                                     
041800     EJECT                                                                
041900                                                                          
042000 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-WDK722  '.         
042100 01  DLI-IO-WDK722.                                                       
042200     03  WDK722.                                                          
042300*        05  -COPY WDK722                                                 
042400     EJECT                                                                
042500                                                                          
042600 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-BENA11  '.         
042700 01  DLI-IO-AREA-BENA.                                                    
042800     03  WLBENA11.                                                        
042900*        05  -COPY WDD311                                                 
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORDP01  '.         
043300 01  DLI-IO-AREA-ORDP.                                                    
043400     03  WLORDP01.                                                        
043500*        05  -COPY WDA501                                                 
043600     EJECT                                                                
043700                                                                          
043800 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-SATB1101'.         
043900 01  DLI-IO-AREA-SATB.                                                    
044000     03  WLSATB11.                                                        
044100*        05  -COPY WDJ111                                                 
044200     EJECT                                                                
044300     03  WLSATB01.                                                        
044400*        05  -COPY WDJ101                                                 
044500     EJECT                                                                
044600                                                                          
044700 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-454111  '.         
044800 01  DLI-IO-AREA-4541.                                                    
044900     03  WL454111.                                                        
045000*        05  -COPY WDGX4542                                               
045100     EJECT                                                                
045200                                                                          
045300 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-WDK611  '.         
045400 01  DLI-IO-AREA-WDK611.                                                  
045500*    03  -COPY WDK611                                                     
045600     EJECT                                                                
045700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045800 01   DLI-IO-AREA-B601.                                                   
045900*     03  -COPY WDB601                                                    
046000     EJECT                                                                
046100                                                                          
046200 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
046300 01  4298-MSG-IO-AREA.                                                    
046400     03  4298-LL               PIC S9(4)  VALUE +0  COMP SYNC.            
046500     03  4298-Z1               PIC X.                                     
046600     03  4298-Z2               PIC X.                                     
046700     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
046800     03  4298-IDTRANS          PIC X(4)   VALUE '4203'.                   
046900     03  4298-SPRAK            PIC X      VALUE SPACE.                    
047000*    03  -COPY W4I29801  -PRE 4298-                                       
047100     EJECT                                                                
047200                                                                          
047300 01  FILLER                  PIC X(16)  VALUE '4202-MSG-IO-AREA'.         
047400 01  4202-MSG-IO-AREA.                                                    
047500     03  4202-LL               PIC S9(4)  VALUE +954 COMP SYNC.           
047600     03  4202-Z1               PIC X.                                     
047700     03  4202-Z2               PIC X.                                     
047800     03  4202-TRANSKOD         PIC X(8)   VALUE 'W4T202U '.               
047900     03  4202-IDTRANS          PIC X(4)   VALUE '4203'.                   
048000     03  4202-SPRAK            PIC X.                                     
048100*    03  -COPY W4I20201  -PRE 4202-                                       
048200     EJECT                                                                
048300                                                                          
048400 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
048500 01  4203-MSG-IO-AREA.                                                    
048600     03  4203-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
048700     03  4203-Z1               PIC X      VALUE LOW-VALUE.                
048800     03  4203-Z2               PIC X      VALUE LOW-VALUE.                
048900     03  4203-TRANSKOD         PIC X(8)   VALUE 'W4T203V '.               
049000     03  4203-IDTRANS          PIC X(4)   VALUE '4203'.                   
049100     03  4203-SPRAK            PIC X.                                     
049200     03  4203-IDDISTR-IN       PIC X(4).                                  
049300     03  4203-IDKUNDNR-IN      PIC X(6).                                  
049400     03  4203-IDORDNR-IN       PIC X(5).                                  
049500     03  4203-IDDISTR-UT       PIC X(4).                                  
049600     03  4203-IDKUNDNR-UT      PIC X(6).                                  
049700     03  4203-IDORDNR-UT       PIC X(5).                                  
049800     03  4203-KDORDKL-UT       PIC X      VALUE SPACE.                    
049900     03  FILLER                PIC X(21)  VALUE ZERO.                     
050000                                                                          
050100 01  FILLER                  PIC X(16)  VALUE '4244-MSG-IO-AREA'.         
050200 01  4244-MSG-IO-AREA.                                                    
050300     03  4244-LL               PIC S9(4)  VALUE +577 COMP SYNC.           
050400     03  4244-Z1               PIC X.                                     
050500     03  4244-Z2               PIC X.                                     
050600     03  4244-TRANSKOD         PIC X(8)   VALUE 'W4T244U '.               
050700     03  4244-IDTRANS          PIC X(4)   VALUE '4203'.                   
050800     03  4244-SPRAK            PIC X.                                     
050900*    03  -COPY W4I24401  -PRE 4244-                                       
051000     EJECT                                                                
051100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
051200     SKIP3                                                                
051300 01  -COPY WZ01SEND                                                       
051400     EJECT                                                                
051500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
051600     SKIP3                                                                
051700 01  SEND-AREA.                                                           
051800*    03  -COPY WZ01REQU  -PRE 3039-                                       
051900*    03  -COPY W30391I1  -PRE 3039-                                       
052000     EJECT                                                                
052100 LINKAGE SECTION.                                                         
052200                                                                          
052300*01  -COPY W0009      -PRE MSG-                                           
052400     EJECT                                                                
052500 01  AVSR-ALT-PCB                PIC X.                                   
052600*01  -COPY W0009      -PRE 4202-                                          
052700     EJECT                                                                
052800*01  -COPY W0009      -PRE 4203-                                          
052900     EJECT                                                                
053000*01  -COPY W0009      -PRE 4244-                                          
053100     EJECT                                                                
053200*01  -COPY W0009      -PRE 4298-                                          
053300     EJECT                                                                
053400*01  -COPY W0009      -PRE 2191-                                          
053500     EJECT                                                                
053600                                                                          
053700*01  -COPY W0008      -PRE USEA-                                          
053800     05  FILLER                  PIC X.                                   
053900     SKIP2                                                                
054000*01  -COPY W0008      -PRE SATB-                                          
054100     05  FILLER                  PIC X.                                   
054200     SKIP2                                                                
054300*01  -COPY W0009      -PRE PRQRY-                                         
054400     05  FILLER                  PIC X.                                   
054500     SKIP2                                                                
054600*01  -COPY W0008      -PRE ARTM-                                          
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01  -COPY W0008      -PRE WDK7-                                          
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01  -COPY W0008      -PRE BENA-                                          
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01  -COPY W0008      -PRE ORQF-                                          
055600     05  FILLER                  PIC X.                                   
055700     SKIP2                                                                
055800*01  -COPY W0008      -PRE ORQI-                                          
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01  -COPY W0008      -PRE ORQM-                                          
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01  -COPY W0008      -PRE 4541-                                          
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01  -COPY W0008      -PRE WDK6-                                          
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0008      -PRE WDB6-                                          
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300     EJECT                                                                
057400 01  AREG-WDK6-PCB               PIC X.                                   
057500 01  AREG-WDK7-PCB               PIC X.                                   
057600                                                                          
057700 01  TPO2-ORDP-PCB               PIC X.                                   
057800 01  TPO2-XXBU-PCB               PIC X.                                   
057900 01  TPO2-XXBV-PCB               PIC X.                                   
058000 01  TPO2-ARTM-PCB               PIC X.                                   
058100 01  TPO2-FILA-PCB               PIC X.                                   
058200 01  TPO2-XXBX-PCB               PIC X.                                   
058300                                                                          
058400 01  2109-PCB                    PIC X.                                   
058500 01  TPO6-ORDP-PCB               PIC X.                                   
058600 01  TPO6-XXBU-PCB               PIC X.                                   
058700 01  TPO6-XXBV-PCB               PIC X.                                   
058800 01  TPO6-XXBX-PCB               PIC X.                                   
058900 01  TPO6-ARTS-PCB               PIC X.                                   
059000                                                                          
059100 01  TIME-4437-PCB               PIC X.                                   
059200                                                                          
059300 01  AVSR-ORQI-PCB               PIC X.                                   
059400 01  AVSR-GMTB-PCB               PIC X.                                   
059500 01  AVSR-GMTC-PCB               PIC X.                                   
059600 01  AVSR-WDB2-PCB               PIC X.                                   
059700 01  AVSR-WDB6-PCB               PIC X.                                   
059800                                                                          
059900 01  TRAN-XXKB-PCB               PIC X.                                   
060000 01  PRNO-3107-PCB               PIC X.                                   
060100 01  PRQU-WDG2-PCB               PIC X.                                   
060200 01  PRQU-WDC7-PCB               PIC X.                                   
060300 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
060400                                                                          
060500     EJECT                                                                
060600 PROCEDURE DIVISION  USING                                                
060700      MSG-PCB        AVSR-ALT-PCB   4202-PCB       4203-PCB               
060800      4244-PCB       4298-PCB       2109-PCB       2191-PCB               
060900      PRQRY-PCB      USEA-PCB                                             
061000      SATB-PCB       ARTM-PCB       WDK7-PCB                              
061100      BENA-PCB       ORQF-PCB       ORQI-PCB                              
061200      ORQM-PCB       4541-PCB       WDK6-PCB                              
061300      WDB6-PCB                                                            
061400      AREG-WDK6-PCB                                                       
061500      AREG-WDK7-PCB                                                       
061600      TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                         
061700      TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                         
061800      TPO6-ORDP-PCB  TPO6-XXBU-PCB  TPO6-XXBV-PCB                         
061900      TPO6-XXBX-PCB  TPO6-ARTS-PCB                                        
062000      TIME-4437-PCB                                                       
062100      AVSR-ORQI-PCB  AVSR-GMTB-PCB  AVSR-GMTC-PCB                         
062200      AVSR-WDB2-PCB  AVSR-WDB6-PCB                                        
062300      TRAN-XXKB-PCB                                                       
062400      PRQU-WDG2-PCB                                                       
062500      PRQU-WDC7-PCB                                                       
062600      PRQU-SJKO-WDK6-PCB                                                  
062700      PRNO-3107-PCB.                                                      
062800                                                                          
062900 MAIN SECTION.                                                            
063000     ENTRY 'DLITCBL' USING                                                
063100      MSG-PCB        AVSR-ALT-PCB   4202-PCB       4203-PCB               
063200      4244-PCB       4298-PCB       2109-PCB       2191-PCB               
063300      PRQRY-PCB      USEA-PCB                                             
063400      SATB-PCB       ARTM-PCB       WDK7-PCB                              
063500      BENA-PCB       ORQF-PCB       ORQI-PCB                              
063600      ORQM-PCB       4541-PCB       WDK6-PCB                              
063700      WDB6-PCB                                                            
063800      AREG-WDK6-PCB                                                       
063900      AREG-WDK7-PCB                                                       
064000      TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                         
064100      TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                         
064200      TPO6-ORDP-PCB  TPO6-XXBU-PCB  TPO6-XXBV-PCB                         
064300      TPO6-XXBX-PCB  TPO6-ARTS-PCB                                        
064400      TIME-4437-PCB                                                       
064500      AVSR-ORQI-PCB  AVSR-GMTB-PCB  AVSR-GMTC-PCB                         
064600      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
064700      TRAN-XXKB-PCB                                                       
064800      PRQU-WDG2-PCB                                                       
064900      PRQU-WDC7-PCB                                                       
065000      PRQU-SJKO-WDK6-PCB                                                  
065100      PRNO-3107-PCB.                                                      
065200                                                                          
065300     EJECT                                                                
065400     PERFORM IMS-GET-MSG                                                  
065500     IF SEGMENT-FINNS                                                     
065600        PERFORM A-INIT                                                    
065700        PERFORM B-KOLLA-NYCKLAR                                           
065800        IF NYCKEL-OK                                                      
065900           IF MFS-NEXT                                                    
066000              PERFORM C-NAESTA-SIDA                                       
066100           ELSE                                                           
066200              PERFORM D-FOERSTA-SIDA                                      
066300           END-IF                                                         
066400           IF ALLT-OK                                                     
066500              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
066600              IF ALLT-OK                                                  
066700                 IF MFS-KDTRTYP = 'U' OR 'V' OR MFS-ENTER                 
066800                    PERFORM G-KONTROLLERA-BILDEN                          
066900                 END-IF                                                   
067000                 IF ALLT-OK                                               
067100                    PERFORM H-BEHANDLA-RADER                              
067200                    IF  AVSLUTA                                           
067300                       PERFORM I-STARTA-ORDERAVSLUT                       
067400                    END-IF                                                
067500                 END-IF                                                   
067600              END-IF                                                      
067700           END-IF                                                         
067800        END-IF                                                            
067900        IF HOPP = NEJ                                                     
068000           PERFORM Z-FINIT                                                
068100        END-IF                                                            
068200     END-IF                                                               
068300     MOVE +0 TO RETURN-CODE                                               
068400     GOBACK                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 A-INIT SECTION.                                                          
068800                                                                          
068900     MOVE SPACE                TO MED-IDMFSFEL                            
069000                                  MED-IDMFSINF                            
069100     MOVE JA                   TO ALLT-SW                                 
069200                                  NYCKEL-SW                               
069300     ACCEPT DAGENS-DATUM     FROM DATE                                    
069400                                                                          
069500     IF MSG-DUBBLA-TRANSKODER                                             
069600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20301                 
069700       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
069800       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
069900     ELSE                                                                 
070000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I20301                  
070100       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
070200       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
070300     END-IF                                                               
070400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
070500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
070600     MOVE MSG-KDTRANS-1        TO WS-TRANSKOD                             
070700     MOVE LOW-VALUE            TO MSG-AREA                                
070800     MOVE MFS-IDTRANS          TO W-IDTRANS                               
070900     IF (W-IDTRANS = '4244' OR '4246') OR WS-TRANSKOD = 'W4T246'          
071000        MOVE 'W4O246N1'        TO MFS-IDMOD                               
071100        MOVE '4246'            TO MOD-IDTRANS                             
071200     ELSE                                                                 
071300        MOVE 'W4O203N1'        TO MFS-IDMOD                               
071400        MOVE '4203'            TO MOD-IDTRANS                             
071500     END-IF                                                               
071600     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
071700                                                                          
071800     IF NOT EGEN-MID                                                      
071900       MOVE SPACE              TO MFS-KDTRTYP                             
072000       MOVE '7'                TO MFS-IDPFK                               
072100     END-IF                                                               
072200     EJECT                                                                
072300     IF ENGLISH-TEXT                                                      
072400       MOVE +2                 TO SPRAK-IX                                
072500       MOVE 'GB '              TO MED-IDSKYLT                             
072600     ELSE                                                                 
072700       MOVE +1                 TO SPRAK-IX                                
072800       MOVE 'S  '              TO MED-IDSKYLT                             
072900     END-IF                                                               
073000     PERFORM AA-NOLLA-TABELLER                                            
073100                                                                          
073200     MOVE SPACE                TO   2191-MID-W2I19101                     
073300     .                                                                    
073400     SKIP2                                                                
073500 AA-NOLLA-TABELLER SECTION.                                               
073600                                                                          
073700     MOVE +1                   TO WS-INDEX-WOPS                           
073800     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
073900        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
074000        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
074100        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
074200        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
074300        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
074400        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
074500        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
074600                                  AVSR-PRAVCOST(WS-INDEX-WOPS)            
074700                                  AVSR-PRARTNTO-LOC(WS-INDEX-WOPS)        
074800                                  AVSR-PRARTNTO-LOCPREL                   
074900                                                   (WS-INDEX-WOPS)        
075000        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
075100        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
075200        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
075300        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
075400        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
075500        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
075600        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
075700                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
075800        ADD +1                 TO WS-INDEX-WOPS                           
075900     END-PERFORM                                                          
076000     MOVE +1                   TO WS-INDEX-WOPS                           
076100                                                                          
076200     MOVE +1                   TO WS-INDEX                                
076300     PERFORM UNTIL WS-INDEX > +14                                         
076400        MOVE ALL '+'           TO 4202-MID-RADER(WS-INDEX)                
076500                                  4244-MID-RADER(WS-INDEX)                
076600        ADD +1                 TO WS-INDEX                                
076700     END-PERFORM                                                          
076800     .                                                                    
076900     EJECT                                                                
077000 B-KOLLA-NYCKLAR SECTION.                                                 
077100                                                                          
077200     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
077300                                  MOD-IDKUNDNR-IN                         
077400                                  MOD-IDORDNR-IN                          
077500     IF MID-IDDISTR-IN = ALL '+'                                          
077600        MOVE MID-IDDISTR-UT    TO WS-IDDISTR                              
077700        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
077800     ELSE                                                                 
077900        MOVE MID-IDDISTR-IN    TO WS-IDDISTR                              
078000        MOVE '7'               TO MFS-IDPFK                               
078100        MOVE SPACE             TO MFS-KDTRTYP                             
078200     END-IF                                                               
078300                                                                          
078400     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
078500        MOVE WS-IDDISTR        TO W-IDDISTR                               
078600     ELSE                                                                 
078700        MOVE NEJ               TO NYCKEL-SW                               
078800        MOVE ZERO              TO W-IDDISTR                               
078900     END-IF                                                               
079000                                                                          
079100     MOVE W-IDDISTR TO TEST-IDDISTR                                       
079200     IF DIST79-DEALER-PRICE                                               
079300        IF ENGLISH-TEXT                                                   
079400           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
079500        ELSE                                                              
079600           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
079700        END-IF                                                            
079800     ELSE                                                                 
079900        MOVE SPACES             TO MOD-TEDDI                              
080000     END-IF                                                               
080100                                                                          
080200     IF MID-IDKUNDNR-IN = ALL '+'                                         
080300        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
080400        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
080500     ELSE                                                                 
080600        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
080700        MOVE '7'               TO MFS-IDPFK                               
080800        MOVE SPACE             TO MFS-KDTRTYP                             
080900     END-IF                                                               
081000                                                                          
081100     IF WS-IDKUNDNR NUMERIC                                               
081200        MOVE WS-IDKUNDNR       TO W-IDKUNDNR                              
081300     ELSE                                                                 
081400        MOVE NEJ               TO NYCKEL-SW                               
081500     END-IF                                                               
081600     IF MID-IDORDNR-IN = ALL '+'                                          
081700        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
081800        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
081900     ELSE                                                                 
082000        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
082100        MOVE '7'               TO MFS-IDPFK                               
082200        MOVE SPACE             TO MFS-KDTRTYP                             
082300     END-IF                                                               
082400     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
082500        MOVE WS-IDORDNR        TO W-IDORDNR                               
082600     ELSE                                                                 
082700        MOVE NEJ               TO NYCKEL-SW                               
082800     END-IF                                                               
082900                                                                          
083000     IF GODK-MID OR NYCKEL-OK                                             
083100        MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                      
083200        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
083300        IF WS-IDKUNDNR = ZERO                                             
083400           MOVE '     0'          TO MOD-IDKUNDNR-UT                      
083500        END-IF                                                            
083600        MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                       
083700        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
083800        MOVE WS-IDORDNR           TO MOD-IDORDNR-UT                       
083900        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
084000     ELSE                                                                 
084100        MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                          
084200                                  MOD-IDKUNDNR-UT                         
084300                                  MOD-IDORDNR-UT                          
084400     END-IF                                                               
084500                                                                          
084600     IF NOT NYCKEL-OK                                                     
084700        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
084800        PERFORM MFS-RENSA-ALLA-FAELT                                      
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 C-NAESTA-SIDA SECTION.                                                   
085400     IF MID-IDARTNR-NEXT NUMERIC AND                                      
085500        MID-IDARTNR-NEXT > ZERO                                           
085600        MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                        
085700        MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                        
085800        MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                       
085900        MOVE MID-IDDC-NEXT     TO W-Q1-IDDC-MIN                           
086000        MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                       
086100     ELSE                                                                 
086200        MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                            
086300        MOVE NEJ               TO ALLT-SW                                 
086400     END-IF                                                               
086500                                                                          
086600     .                                                                    
086700                                                                          
086800 D-FOERSTA-SIDA SECTION.                                                  
086900                                                                          
087000     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
087100                                  W-Q1-IDLOPNR-MIN                        
087200                                  W-Q1-IDSEKVNR-MIN                       
087300                                  W-Q1-KDORDBEK-MIN                       
087400     MOVE SPACE                TO W-Q1-IDDC-MIN                           
087500                                                                          
087600     IF MID-IDARTNR-NEXT NUMERIC AND                                      
087700        MID-IDARTNR-NEXT > ZERO                                           
087800        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                        
087900        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                        
088000        MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                       
088100        MOVE MID-IDDC-NEXT     TO MOD-IDDC-NEXT                           
088200        MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                       
088300     END-IF                                                               
088400     .                                                                    
088500     EJECT                                                                
088600 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
088700                                                                          
088800     PERFORM IMS-09-GU-ORQI-WDQ201                                        
088900     IF SEGMENT-FINNS                                                     
089000                                                                          
089100        IF OHUV-FLKLAR = JA                                               
089200           MOVE ERR-ORDER-AVSLUTAD                                        
089300                               TO MED-IDMFSFEL                            
089400           MOVE NEJ            TO ALLT-SW                                 
089500           MOVE NEJ            TO NYCKEL-SW                               
089600           PERFORM MFS-RENSA-ALLA-FAELT                                   
089700        ELSE                                                              
089800           MOVE OHUV-IDSYSTEM TO BILD-SW                                  
089900           IF NOT GODK-BILD                                               
090000              MOVE ERR-FEL-BILDSERIE TO MED-IDMFSFEL                      
090100              MOVE NEJ               TO ALLT-SW                           
090200              PERFORM MFS-RENSA-ALLA-FAELT                                
090300           ELSE                                                           
090400              IF OHUV-FLFORBI = JA OR                                     
090500                 OHUV-FLFORBI = SPEC-FORBI OR                             
090600                 OHUV-FLORDSPE = JA OR                                    
090700                 OHUV-KDTPOTYP = +3 OR OHUV-KDORDKL = +0                  
090800                 MOVE NEJ          TO ALLT-SW                             
090900                 MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                     
091000              ELSE                                                        
091100                 MOVE OHUV-KDORDKL TO MOD-KDORDKL-UT                      
091200                 MOVE OHUV-IDORDER TO W-Q1-IDORDER-UNIK                   
091300                                      W-Q1-IDORDER-MIN                    
091400                                      W-Q1-IDORDER-MAX                    
091500                                      W-Q1-IDORDER-MIN1                   
091600                                      W-Q1-IDORDER-MAX1                   
091700                                                                          
091800                PERFORM FA-FIXA-LOKAL-TID                                 
091900              END-IF                                                      
092000           END-IF                                                         
092100        END-IF                                                            
092200     ELSE                                                                 
092300        MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                            
092400        MOVE NEJ               TO ALLT-SW                                 
092500        MOVE NEJ               TO NYCKEL-SW                               
092600        PERFORM MFS-RENSA-ALLA-FAELT                                      
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000                                                                          
093100 FA-FIXA-LOKAL-TID SECTION.                                               
093200                                                                          
093300     MOVE ALL '+'              TO MSGI-WMSGINIT                           
093400     MOVE '013'                TO MSGI-KDCALL                             
093500     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
093600                                                                          
093700     MOVE '4203'               TO MSGI-IDTRANS                            
093800     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
093900                                                                          
094000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
094100     .                                                                    
094200     EJECT                                                                
094300                                                                          
094400 G-KONTROLLERA-BILDEN SECTION.                                            
094500                                                                          
094600     MOVE JA                   TO ALLT-SW                                 
094700                                                                          
094800     PERFORM GA-KONTROLLERA-KDBEHX                                        
094900     IF ALLT-OK                                                           
095000        PERFORM GB-KONTROLLERA-SAMBAND                                    
095100        IF ALLT-OK                                                        
095200           PERFORM GC-JUSTERA-KDBEHX                                      
095300        END-IF                                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700                                                                          
095800 GA-KONTROLLERA-KDBEHX SECTION.                                           
095900                                                                          
096000     MOVE +1                   TO WS-INDEX-MID                            
096100     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
096200                      MID-KDORDBEK(WS-INDEX-MID) = ZERO                   
096300        IF MID-KDBEHX(WS-INDEX-MID) = '+'                                 
096400           MOVE SPACE          TO MID-KDBEHX(WS-INDEX-MID)                
096500        END-IF                                                            
096600        MOVE MID-RAD(WS-INDEX-MID)                                        
096700                               TO WS-AKTUELL-MID-RAD                      
096800        IF WS-AKT-KDBEHX = 'B' OR 'D' OR 'A'                              
096900                               OR '1' OR '2' OR ' '                       
097000           EVALUATE WS-AKT-KDBEHX                                         
097100              WHEN 'A'                                                    
097200                 IF WS-AKT-KDORDBEK = 61                                  
097300                    CONTINUE                                              
097400                 ELSE                                                     
097500                    MOVE MFS-ALFA-FAELT-FEL                               
097600                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
097700                    MOVE NEJ   TO ALLT-SW                                 
097800                 END-IF                                                   
097900              WHEN 'B'                                                    
098000                 IF WS-AKT-KDORDBEK = 21  OR 51  OR 52  OR 53  OR         
098100                        54  OR 55  OR 57  OR 58  OR 59  OR 66  OR         
098200                        67  OR 72  OR 73  OR 74  OR 75  OR                
098300                        76  OR 80  OR 81  OR 82  OR 85  OR                
098400                     ((WS-AKT-KDORDBEK = 41 OR 61) AND                    
098500                      (WS-AKT-IDARTNR  = WS-AKT-IDARTNR-URS))             
098600                    CONTINUE                                              
098700                 ELSE                                                     
098800                    MOVE MFS-ALFA-FAELT-FEL                               
098900                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
099000                    MOVE NEJ   TO ALLT-SW                                 
099100                 END-IF                                                   
099200              WHEN 'D'                                                    
099300                 IF WS-AKT-IDLOPNR-RO = +0                                
099400                    IF WS-AKT-KDORDBEK = 15  OR 16  OR 43                 
099500                               OR 44  OR 70  OR 95  OR 98                 
099600                               OR 99  OR 41  OR 71                        
099700                       CONTINUE                                           
099800                    ELSE                                                  
099900                      MOVE MFS-ALFA-FAELT-FEL                             
100000                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
100100                      MOVE NEJ TO ALLT-SW                                 
100200                    END-IF                                                
100300                 ELSE                                                     
100400                    IF WS-AKT-KDORDBEK = 43  OR                           
100500                           44  OR 70  OR 98  OR 99  OR                    
100600                           41  OR 56                                      
100700                       CONTINUE                                           
100800                    ELSE                                                  
100900                      MOVE MFS-ALFA-FAELT-FEL                             
101000                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
101100                      MOVE NEJ TO ALLT-SW                                 
101200                    END-IF                                                
101300                 END-IF                                                   
101400              WHEN '1'                                                    
101500                 IF WS-AKT-KDORDBEK = 43  OR 44                           
101600                    CONTINUE                                              
101700                 ELSE                                                     
101800                    MOVE MFS-ALFA-FAELT-FEL                               
101900                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
102000                    MOVE NEJ   TO ALLT-SW                                 
102100                 END-IF                                                   
102200              WHEN '2'                                                    
102300                 IF WS-AKT-KDORDBEK = 43  OR 44                           
102400                    CONTINUE                                              
102500                 ELSE                                                     
102600                    MOVE MFS-ALFA-FAELT-FEL                               
102700                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
102800                    MOVE NEJ   TO ALLT-SW                                 
102900                 END-IF                                                   
103000              WHEN OTHER                                                  
103100                 IF WS-AKT-KDORDBEK = 15  OR 16  OR                       
103200                        43  OR 44  OR 70  OR 74  OR 95  OR                
103300                        98  OR 99  OR 41  OR 61  OR                       
103400                        92  OR 71  OR 56  OR 26                           
103500                    CONTINUE                                              
103600                 ELSE                                                     
103700                    MOVE MFS-ALFA-FAELT-FEL                               
103800                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
103900                    MOVE NEJ   TO ALLT-SW                                 
104000                 END-IF                                                   
104100           END-EVALUATE                                                   
104200        ELSE                                                              
104300           MOVE MFS-ALFA-FAELT-FEL                                        
104400                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
104500           MOVE NEJ            TO ALLT-SW                                 
104600        END-IF                                                            
104700                                                                          
104800        ADD +1                 TO WS-INDEX-MID                            
104900     END-PERFORM                                                          
105000     IF NOT ALLT-OK                                                       
105100        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
105200     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500                                                                          
105600 GB-KONTROLLERA-SAMBAND SECTION.                                          
105700                                                                          
105800     MOVE +1                   TO WS-INDEX-MID                            
105900     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
106000                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
106100        MOVE MID-RAD(WS-INDEX-MID)                                        
106200                               TO WS-AKTUELL-MID-RAD                      
106300        IF WS-AKT-KDBEHX = 'D'                                            
106400           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
106500           ADD +1              TO WS-INDEX-MID                            
106600           IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                         
106700              MOVE MID-RAD(WS-INDEX-MID)                                  
106800                               TO WS-AKTUELL-MID-RAD                      
106900           END-IF                                                         
107000           PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR               
107100                         WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR          
107200                         WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR          
107300                     WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR         
107400              IF WS-AKT-KDBEHX = '1' OR '2'                               
107500                 MOVE NEJ      TO ALLT-SW                                 
107600                 MOVE MFS-ALFA-FAELT-FEL                                  
107700                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
107800                 MOVE ERR-UPPLYSTA-FEL                                    
107900                               TO MED-IDMFSFEL                            
108000              END-IF                                                      
108100              ADD +1           TO WS-INDEX-MID                            
108200              IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                      
108300                 MOVE MID-RAD(WS-INDEX-MID)                               
108400                               TO WS-AKTUELL-MID-RAD                      
108500              END-IF                                                      
108600           END-PERFORM                                                    
108700        ELSE                                                              
108800           ADD +1              TO WS-INDEX-MID                            
108900        END-IF                                                            
109000     END-PERFORM                                                          
109100     .                                                                    
109200     EJECT                                                                
109300                                                                          
109400 GC-JUSTERA-KDBEHX SECTION.                                               
109500                                                                          
109600*    JUSTERINGEN GÖRS FÖR ATT VARJE RAD SENARE I PROGRAMMET               
109700*    SKALL KUNNA BEHANDLAS VAR FÖR SIG.                                   
109800*    BORTTAG AV ORDERBEKRÄFTELSER SKULLE ANNARS BEHÖVA GÖRAS              
109900*    I MÅNGA SEKTIONER. PÅ DETTA SÄTT KOMMER ALLA BORTTAG                 
110000*    ATT GÖRAS I HEA-ANNULLERA-RAD.                                       
110100                                                                          
110200     MOVE +1                   TO WS-INDEX-MID                            
110300     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX   OR                   
110400                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
110500        MOVE MID-RAD(WS-INDEX-MID)                                        
110600                               TO WS-AKTUELL-MID-RAD                      
110700        IF WS-AKT-KDBEHX = '1' OR '2'                                     
110800           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
110900           PERFORM GCA-D-MARKERA-1-2-RADER                                
111000        ELSE                                                              
111100          IF WS-AKT-KDBEHX = 'D'                                          
111200             PERFORM S13-HITTA-FORSTA-I-GRUPPEN                           
111300             PERFORM GCB-D-MARKERA-RADER                                  
111400          ELSE                                                            
111500             ADD +1            TO WS-INDEX-MID                            
111600          END-IF                                                          
111700        END-IF                                                            
111800     END-PERFORM                                                          
111900     .                                                                    
112000     EJECT                                                                
112100                                                                          
112200 GCA-D-MARKERA-1-2-RADER SECTION.                                         
112300                                                                          
112400     ADD +1                    TO WS-INDEX-MID                            
112500     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
112600        MOVE MID-RAD(WS-INDEX-MID)                                        
112700                               TO WS-AKTUELL-MID-RAD                      
112800     END-IF                                                               
112900     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
113000                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
113100                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
113200               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
113300                                                                          
113400        IF WS-AKT-KDBEHX = ' '                                            
113500           IF WS-AKT-KDORDBEK = 41                                        
113600              CONTINUE                                                    
113700           ELSE                                                           
113800              MOVE 'D'         TO MID-KDBEHX(WS-INDEX-MID)                
113900           END-IF                                                         
114000        END-IF                                                            
114100        ADD +1                 TO WS-INDEX-MID                            
114200        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
114300           MOVE MID-RAD(WS-INDEX-MID)                                     
114400                               TO WS-AKTUELL-MID-RAD                      
114500        END-IF                                                            
114600     END-PERFORM                                                          
114700     .                                                                    
114800     EJECT                                                                
114900                                                                          
115000 GCB-D-MARKERA-RADER SECTION.                                             
115100                                                                          
115200     ADD +1                    TO WS-INDEX-MID                            
115300     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
115400        MOVE MID-RAD(WS-INDEX-MID)                                        
115500                               TO WS-AKTUELL-MID-RAD                      
115600     END-IF                                                               
115700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
115800                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
115900                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
116000               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
116100                                                                          
116200        IF WS-AKT-KDBEHX = ' '  AND                                       
116300                 (WS-AKT-IDDC = WS-IDDC-SPAR)                             
116400                                                                          
116500           MOVE 'D'            TO MID-KDBEHX(WS-INDEX-MID)                
116600        END-IF                                                            
116700        ADD +1                 TO WS-INDEX-MID                            
116800        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
116900           MOVE MID-RAD(WS-INDEX-MID)                                     
117000                               TO WS-AKTUELL-MID-RAD                      
117100        END-IF                                                            
117200     END-PERFORM                                                          
117300     .                                                                    
117400     EJECT                                                                
117500                                                                          
117600 H-BEHANDLA-RADER SECTION.                                                
117700                                                                          
117800     PERFORM HA-SKAPA-SPAR-ARBTAB                                         
117900                                                                          
118000     IF MFS-FIRST  OR  MFS-NEXT                                           
118100                                                                          
118200        PERFORM HB-LAS-IN-13-RADER                                        
118300        IF  WS-INDEX-MOD = +1  AND                                        
118400              (W-IDTRANS = '4202' OR '4244' OR '4246' OR '4297')          
118500           MOVE JA             TO AVSLUTA-SW                              
118600        END-IF                                                            
118700     ELSE                                                                 
118800        IF MFS-UPDATE     OR  MFS-QUERY                                   
118900           PERFORM HE-UPPDATERA-AKT-SIDA                                  
119000           PERFORM HD-UPPDATERA-OBEH-RADER                                
119100           IF START-4202                                                  
119200             MOVE 'U'            TO 4202-MID-KDTRTYP                      
119300             PERFORM S03-STARTA-RADBEHANDLINGEN                           
119400           ELSE                                                           
119500              IF START-4244                                               
119600                 MOVE 'U'        TO 4244-MID-KDTRTYP                      
119700                 PERFORM S03-STARTA-RADBEHANDLINGEN                       
119800              ELSE                                                        
119900                 PERFORM HB-LAS-IN-13-RADER                               
120000                 IF WS-INDEX-MOD = +1                                     
120100                    MOVE JA      TO AVSLUTA-SW                            
120200                 END-IF                                                   
120300              END-IF                                                      
120400           END-IF                                                         
120500        ELSE                                                              
120600           IF MFS-UPD-V                                                   
120700              PERFORM HE-UPPDATERA-AKT-SIDA                               
120800              PERFORM HG-UPPDATERA-RESTERANDE-RADER                       
120900           END-IF                                                         
121000        END-IF                                                            
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
121500 HA-SKAPA-SPAR-ARBTAB SECTION.                                            
121600                                                                          
121700     PERFORM IMS-12-GNP-ORQI-WDQ212                                       
121800     IF ARB-IDDC NOT = W-IDDC-B6                                          
121900        MOVE ARB-IDDC TO W-IDDC-B6                                        
122000        PERFORM IMS-GU-WDB601                                             
122100     END-IF                                                               
122200     IF DCS-CDC                                                           
122300       MOVE ARB-KDFRAKT    TO SPAR-KDFRAKT                                
122400       MOVE ARB-KDFDKRAV   TO SPAR-KDFDKRAV                               
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800 HB-LAS-IN-13-RADER SECTION.                                              
122900                                                                          
123000     PERFORM MFS-RENSA-ALLA-FAELT                                         
123100     MOVE +0                   TO WS-IDARTNR-SPAR                         
123200                                  WS-IDLOPNR-SPAR                         
123300                                                                          
123400     PERFORM HBA-VISA-OBKR-OCH-SATS-RADER                                 
123500                                                                          
123600     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
123700                                                                          
123800     IF WS-INDEX-MOD > WS-INDEX-MOD-MAX AND                               
123900           OBKR-SEGMENT-FINNS   AND                                       
124000          (OBKR-KDORDBEK = 41 OR 61)                                      
124100                                                                          
124200        PERFORM HBC-KONTROLLERA-SIDSLUT                                   
124300     END-IF                                                               
124400     .                                                                    
124500     EJECT                                                                
124600                                                                          
124700 HBA-VISA-OBKR-OCH-SATS-RADER SECTION.                                    
124800                                                                          
124900     MOVE +1                   TO WS-INDEX-MOD                            
125000                                                                          
125100     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
125200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
125300              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
125400                                                                          
125500        PERFORM HBAA-REDIGERA-ORDERBEKR-RAD                               
125600                                                                          
125700        IF OBKR-KDORDBEK = '57 '                                          
125800           PERFORM HBAB-VISA-SATS-ARTIKLAR                                
125900        END-IF                                                            
126000        MOVE OBKR-IDARTNR   TO WS-IDARTNR-SPAR                            
126100        MOVE OBKR-IDLOPNR   TO WS-IDLOPNR-SPAR                            
126200                                                                          
126300        ADD +1              TO WS-INDEX-MOD                               
126400                                                                          
126500        IF NOT NEXT-SATS                                                  
126600           PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                         
126700*----------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN              
126800           MOVE STATUS-WS      TO STATUS-OBKR-WS                          
126900        END-IF                                                            
127000     END-PERFORM                                                          
127100     .                                                                    
127200     EJECT                                                                
127300                                                                          
127400 HBAA-REDIGERA-ORDERBEKR-RAD SECTION.                                     
127500                                                                          
127600     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
127700     IF OBKR-KDORDBEK = 41 OR 61                                          
127800        MOVE '*'               TO MOD-ASTERIX(WS-INDEX-MOD)               
127900     END-IF                                                               
128000                                                                          
128100     MOVE SPACE                TO MOD-KDBEHX(WS-INDEX-MOD)                
128200     IF OBKR-KDORDBEK = 52 OR 53 OR 54 OR 55 OR 57 OR 58 OR 59            
128300                     OR 66 OR 67 OR 72 OR 73 OR 74 OR 75 OR 76            
128400                     OR 80 OR 81 OR 82 OR 85 OR 21 OR 51                  
128500        MOVE 'B'               TO MOD-KDBEHX(WS-INDEX-MOD)                
128600        MOVE MFS-STAENG-FAELT  TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
128700     END-IF                                                               
128800     IF OBKR-KDORDBEK = 41 OR 61                                          
128900        IF OBKR-IDARTNR = WS-IDARTNR-SPAR AND                             
129000                 OBKR-IDLOPNR = WS-IDLOPNR-SPAR                           
129100           MOVE SPACE          TO MOD-KDBEHX(WS-INDEX-MOD)                
129200           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
129300                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
129400        ELSE                                                              
129500           MOVE 'B'            TO MOD-KDBEHX(WS-INDEX-MOD)                
129600         MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
129700        END-IF                                                            
129800     END-IF                                                               
129900     IF (OBKR-KDORDBEK = 61 )  AND                                        
130000                         OBKR-IDARTNR-TILLK > +0                          
130100        MOVE 'A'               TO MOD-KDBEHX(WS-INDEX-MOD)                
130200     END-IF                                                               
130300     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
130400     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
130500     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
130600                                                                          
130700     PERFORM S22-HAMTA-BENAMNING                                          
130800                                                                          
130900     MOVE OBKR-IDDC            TO MOD-IDDC-RAD(WS-INDEX-MOD)              
131000                                                                          
131100     IF OBKR-KDORDBEK = 15 OR 16 OR 43 OR 44 OR 70 OR                     
131200                        74 OR 95 OR 71                                    
131300        MOVE OBKR-KVBEART-Q    TO MOD-KVANTAL(WS-INDEX-MOD)               
131400     ELSE                                                                 
131500        IF OBKR-KDORDBEK = 80 OR 85                                       
131600           MOVE OBKR-KVANNANT  TO MOD-KVANTAL(WS-INDEX-MOD)               
131700        ELSE                                                              
131800           IF OBKR-KDORDBEK = 92 OR 98 OR 99                              
131900              MOVE OBKR-KVPRERO TO MOD-KVANTAL(WS-INDEX-MOD)              
132000           ELSE                                                           
132100              MOVE OBKR-KVBEART TO MOD-KVANTAL(WS-INDEX-MOD)              
132200           END-IF                                                         
132300        END-IF                                                            
132400     END-IF                                                               
132500                                                                          
132600     IF OBKR-KDORDBEK = 43 OR 44                                          
132700        MOVE OBKR-KVQPACK      TO MOD-KVQPACK(WS-INDEX-MOD)               
132800     ELSE                                                                 
132900        MOVE +0                TO MOD-KVQPACK(WS-INDEX-MOD)               
133000     END-IF                                                               
133100     MOVE +0                   TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
133200                                                                          
133300     IF OBKR-KDORDBEK = 15 OR 16 OR 43 OR 44 OR 52 OR 53 OR 54            
133400                     OR 55 OR 57 OR 70 OR 72 OR 73 OR 74                  
133500                     OR 75 OR 76 OR 80 OR 95 OR 99 OR 58 OR 92            
133600                     OR 66 OR 82 OR 71 OR 21 OR 98                        
133700        IF OBKR-IDARTNR-TILLK > 0                                         
133800           MOVE OBKR-IDARTNR-TILLK                                        
133900                               TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
134000           MOVE '-'            TO MOD-STRAEK(WS-INDEX-MOD)                
134100           MOVE OBKR-REKSIFFR-TILLK                                       
134200                               TO MOD-REKSIFFR(WS-INDEX-MOD)              
134300        END-IF                                                            
134400     ELSE                                                                 
134500        IF OBKR-KDORDBEK = 41 OR 61                                       
134600*------ ERSATT ARTIKEL                                                    
134700                                                                          
134800           PERFORM HBAAA-FIXA-ERSATNING-RAD                               
134900        END-IF                                                            
135000     END-IF                                                               
135100                                                                          
135200     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
135300     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
135400     MOVE OBKR-IDARTNR         TO WS-AKT-IDARTNR-URS                      
135500     MOVE OBKR-IDLOPNR-RO      TO WS-AKT-IDLOPNR-RO                       
135600                                                                          
135700     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
135800     .                                                                    
135900     EJECT                                                                
136000 HBAAA-FIXA-ERSATNING-RAD SECTION.                                        
136100                                                                          
136200     IF (OBKR-IDARTNR NOT = WS-IDARTNR-SPAR)     OR                       
136300           (OBKR-IDARTNR = WS-IDARTNR-SPAR  AND                           
136400              OBKR-IDLOPNR NOT = WS-IDLOPNR-SPAR)                         
136500        PERFORM S22-HAMTA-BENAMNING                                       
136600        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
136700                      TO MOD-IDDC-ATTR(WS-INDEX-MOD)                      
136800                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
136900                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
137000                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
137100     ELSE                                                                 
137200        IF OBKR-IDARTNR-TILLK = +0                                        
137300*------ TILLKOMMANDE TEXT                                                 
137400           MOVE MFS-RENSA-FAELT                                           
137500                           TO MOD-IDARTNR(WS-INDEX-MOD)                   
137600           MOVE OBKR-BEERS TO MOD-BEART(WS-INDEX-MOD)                     
137700           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
137800                      TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)                    
137900                         MOD-IDDC-ATTR(WS-INDEX-MOD)                      
138000                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
138100                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
138200                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
138300        ELSE                                                              
138400*-------TILLKOMMANDE ARTIKEL                                              
138500           MOVE OBKR-IDARTNR-TILLK                                        
138600                         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)                
138700           MOVE '-'      TO MOD-STRAEK(WS-INDEX-MOD)                      
138800           MOVE OBKR-REKSIFFR-TILLK                                       
138900                         TO MOD-REKSIFFR(WS-INDEX-MOD)                    
139000           PERFORM S22-HAMTA-BENAMNING                                    
139100           MOVE OBKR-KVBEART-TILLK                                        
139200                         TO MOD-KVANTAL(WS-INDEX-MOD)                     
139300           MOVE OBKR-DIERS-KVOT                                           
139400                         TO MOD-KVQPACK(WS-INDEX-MOD)                     
139500        END-IF                                                            
139600     END-IF                                                               
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 HBAB-VISA-SATS-ARTIKLAR SECTION.                                         
140100                                                                          
140200     MOVE NEJ                  TO NEXT-SATS-SW                            
140300                                                                          
140400     MOVE +1                   TO WS-INDEX-SATS                           
140500     PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
140600        MOVE SPACE             TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
140700        ADD +1                 TO WS-INDEX-SATS                           
140800     END-PERFORM                                                          
140900                                                                          
141000     MOVE +1                   TO WS-INDEX-SATS                           
141100                                  WS-INDEX                                
141200     MOVE OBKR-IDARTNR         TO W-J1-IDARTNR                            
141300                                                                          
141400     PERFORM IMS-18A-GU-SATB-WDJ111-01                                    
141500     PERFORM UNTIL SEGMENT-SAKNAS                                         
141600             OR    WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
141700             OR    WS-INDEX  > +10                                        
141800                                                                          
141900        MOVE RAD-TISTADAT   TO TMP1-YYMMDD                                
142000        MOVE RAD-TISTODAT   TO TMP2-YYMMDD                                
142100        MOVE DAGENS-DATUM   TO TMP3-YYMMDD                                
142200        PERFORM WY2000Q1                                                  
142300        IF STR-IDARTNR < +100000000 AND STR-TIBORT = +0 AND               
142400           TMP1-YYMMDD NOT > TMP3-YYMMDD AND                              
142500           TMP2-YYMMDD NOT < TMP3-YYMMDD                                  
142600                                                                          
142700           IF WS-INDEX-MOD = WS-INDEX-MOD-MAX                             
142800              MOVE JA             TO NEXT-SATS-SW                         
142900              MOVE +6             TO WS-INDEX-SATS                        
143000           ELSE                                                           
143100              MOVE STR-IDARTNR TO WS-NUM-9                                
143200              MOVE WS-ALFA-9   TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
143300              ADD +1           TO WS-INDEX-SATS                           
143400           END-IF                                                         
143500        END-IF                                                            
143600        IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                          
143700           PERFORM IMS-18-GN-SATB-WDJ111-01                               
143800           ADD  +1             TO WS-INDEX                                
143900        END-IF                                                            
144000     END-PERFORM                                                          
144100     IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                             
144200                                                                          
144300        COMPUTE WS-INDEX = WS-INDEX-MOD + WS-INDEX-SATS - 1               
144400        IF WS-INDEX NOT > WS-INDEX-MOD-MAX                                
144500                                                                          
144600           MOVE +1       TO WS-INDEX-SATS                                 
144700           PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                
144800                 OR WS-IDARTNR-SATS(WS-INDEX-SATS) = SPACE                
144900                                                                          
145000              ADD +1     TO WS-INDEX-MOD                                  
145100              PERFORM HBABA-REDIGERA-SATS-RAD                             
145200              ADD +1     TO WS-INDEX-SATS                                 
145300           END-PERFORM                                                    
145400        ELSE                                                              
145500           MOVE JA       TO NEXT-SATS-SW                                  
145600        END-IF                                                            
145700     END-IF                                                               
145800     IF NEXT-SATS                                                         
145900*-----OM EJ ALLA SATS-ART FÅR PLATS PÅ SIDAN RADERAS RAD MED              
146000*-----ORDBEK = 57 OCH DEN SPARAS FÖR NÄSTA SIDA(I HBB-SECTIONEN)          
146100        MOVE MFS-RENSA-FAELT   TO MOD-KDORDBEK(WS-INDEX-MOD)              
146200                                  MOD-KDBEHX(WS-INDEX-MOD)                
146300                                  MOD-IDARTNR(WS-INDEX-MOD)               
146400                                  MOD-BEART(WS-INDEX-MOD)                 
146500                                  MOD-IDDC-RAD(WS-INDEX-MOD)              
146600                                  MOD-KVANTAL(WS-INDEX-MOD)               
146700                                  MOD-KVQPACK(WS-INDEX-MOD)               
146800                                  MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
146900        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
147000        MOVE MED-UPPLYSN-UPPDAT-PF                                        
147100                               TO MED-IDMFSFEL                            
147200     END-IF                                                               
147300     .                                                                    
147400     EJECT                                                                
147500                                                                          
147600 HBABA-REDIGERA-SATS-RAD SECTION.                                         
147700                                                                          
147800     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
147900     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
148000                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
148100                                                                          
148200     MOVE SPACE                TO MOD-ASTERIX(WS-INDEX-MOD)               
148300     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
148400     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(WS-INDEX-MOD)               
148500                                                                          
148600     MOVE WS-IDARTNR-SATS(WS-INDEX-SATS)                                  
148700                               TO MOD-BEART(WS-INDEX-MOD)                 
148800     INSPECT MOD-BEART(WS-INDEX-MOD) REPLACING LEADING ZERO BY            
148900                                                         SPACE            
149000     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
149100                           TO MOD-IDDC-ATTR(WS-INDEX-MOD)                 
149200                              MOD-KVANTAL-ATTR(WS-INDEX-MOD)              
149300                              MOD-KVQPACK-ATTR(WS-INDEX-MOD)              
149400                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)          
149500                                                                          
149600     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
149700     .                                                                    
149800     EJECT                                                                
149900                                                                          
150000 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
150100                                                                          
150200     IF OBKR-SEGMENT-FINNS                                                
150300        MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                        
150400        MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                        
150500        MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                       
150600        MOVE OBKR-IDDC         TO MOD-IDDC-NEXT                           
150700        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                       
150800                                                                          
150900        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
151000        MOVE MED-UPPLYSN-UPPDAT-PF                                        
151100                               TO MED-IDMFSFEL                            
151200     ELSE                                                                 
151300        MOVE ZERO              TO MOD-IDARTNR-NEXT                        
151400                                  MOD-IDLOPNR-NEXT                        
151500                                  MOD-IDSEKVNR-NEXT                       
151600                                  MOD-IDDC-NEXT                           
151700                                  MOD-KDORDBEK-NEXT                       
151800     END-IF                                                               
151900                                                                          
152000     .                                                                    
152100     EJECT                                                                
152200                                                                          
152300 HBC-KONTROLLERA-SIDSLUT SECTION.                                         
152400                                                                          
152500     MOVE WS-INDEX-MOD-MAX     TO WS-INDEX-MOD                            
152600     MOVE MOD-KEYS(WS-INDEX-MOD)                                          
152700                               TO WS-AKT-KEYS                             
152800     IF OBKR-IDARTNR = WS-AKT-IDARTNR-URS AND                             
152900        OBKR-IDLOPNR = WS-AKT-IDLOPNR                                     
153000                                                                          
153100        PERFORM UNTIL WS-INDEX-MOD = +1 OR                                
153200                     (OBKR-IDARTNR NOT = WS-AKT-IDARTNR-URS OR            
153300                      OBKR-IDLOPNR NOT = WS-AKT-IDLOPNR)                  
153400                                                                          
153500           SUBTRACT 1 FROM WS-INDEX-MOD                                   
153600           MOVE MOD-KEYS(WS-INDEX-MOD)                                    
153700                               TO WS-AKT-KEYS                             
153800        END-PERFORM                                                       
153900                                                                          
154000        ADD +1 TO WS-INDEX-MOD                                            
154100                                                                          
154200*---- BLÄDDRINGSVÄRDENA MÅSTE JUSTERAS OM NÄR VI BACKAR RADER             
154300                                                                          
154400        MOVE MOD-KEYS(WS-INDEX-MOD)                                       
154500                               TO WS-AKT-KEYS                             
154600        MOVE WS-AKT-IDARTNR-URS TO MOD-IDARTNR-NEXT                       
154700        MOVE WS-AKT-IDLOPNR    TO MOD-IDLOPNR-NEXT                        
154800        MOVE WS-AKT-IDSEKVNR   TO MOD-IDSEKVNR-NEXT                       
154900        MOVE MOD-IDDC-RAD(WS-INDEX-MOD)                                   
155000                               TO MOD-IDDC-NEXT                           
155100        MOVE MOD-KDORDBEK(WS-INDEX-MOD)                                   
155200                               TO MOD-KDORDBEK-NEXT                       
155300        MOVE WS-INDEX-MOD      TO WS-INDEX                                
155400        PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                         
155500          MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
155600                                  MOD-ASTERIX(WS-INDEX)                   
155700                                  MOD-KDBEHX(WS-INDEX)                    
155800                                  MOD-IDARTNR(WS-INDEX)                   
155900                                  MOD-BEART(WS-INDEX)                     
156000                                  MOD-IDDC-RAD(WS-INDEX)                  
156100                                  MOD-KVANTAL(WS-INDEX)                   
156200                                  MOD-KVQPACK(WS-INDEX)                   
156300                                  MOD-IDKUNDRF-RO(WS-INDEX)               
156400          ADD +1               TO WS-INDEX                                
156500        END-PERFORM                                                       
156600     ELSE                                                                 
156700        ADD +1                 TO WS-INDEX-MOD                            
156800     END-IF                                                               
156900                                                                          
157000     MOVE MED-FLER-SIDOR       TO MED-IDMFSINF                            
157100     MOVE MED-UPPLYSN-UPPDAT-PF                                           
157200                               TO MED-IDMFSFEL                            
157300     .                                                                    
157400     EJECT                                                                
157500                                                                          
157600 HD-UPPDATERA-OBEH-RADER SECTION.                                         
157700                                                                          
157800     MOVE NEJ                  TO AKT-SIDA-SW                             
157900                                                                          
158000     MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                      
158100     MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-MAX                        
158200     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
158300     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
158400     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
158500     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
158600                                                                          
158700     PERFORM IMS-01-GHU-ORQM-WDQ101-FOERE                                 
158800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
158900        PERFORM S02-GODKANN-RAD                                           
159000        PERFORM IMS-02-GHN-ORQM-WDQ101-FOERE                              
159100     END-PERFORM                                                          
159200                                                                          
159300     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
159400                                  W-Q1-IDLOPNR-MAX                        
159500                                  W-Q1-IDSEKVNR-MAX                       
159600                                  W-Q1-IDDC-MAX                           
159700                                  W-Q1-KDORDBEK-MAX                       
159800     .                                                                    
159900     EJECT                                                                
160000 HE-UPPDATERA-AKT-SIDA SECTION.                                           
160100                                                                          
160200     MOVE JA                   TO AKT-SIDA-SW                             
160300     MOVE +1 TO WS-INDEX-MID                                              
160400     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
160500                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
160600       MOVE MID-RAD(WS-INDEX-MID)                                         
160700                             TO WS-AKTUELL-MID-RAD                        
160800       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-UNIK                     
160900       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
161000       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
161100       MOVE WS-AKT-IDDC          TO W-Q1-IDDC-UNIK                        
161200       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
161300                                                                          
161400       PERFORM IMS-05-GHU-ORQM-WDQ101-UNIK                                
161500       IF SEGMENT-FINNS                                                   
161600         IF MID-IDARTNR(WS-INDEX-MID) = ZERO                              
161700           MOVE JA TO OBKR-FLOBOK                                         
161800           PERFORM IMS-06-REPL-ORQM-WDQ101                                
161900         ELSE                                                             
162000           IF WS-AKT-KDBEHX = SPACE                                       
162100             PERFORM S02-GODKANN-RAD                                      
162200           ELSE                                                           
162300             IF WS-AKT-KDBEHX = 'A'                                       
162400               MOVE JA     TO OBKR-FLOBOK                                 
162500               PERFORM IMS-06-REPL-ORQM-WDQ101                            
162600             ELSE                                                         
162700               IF WS-AKT-KDBEHX = 'D'                                     
162800                 PERFORM HEA-ANNULLERA-RAD                                
162900               ELSE                                                       
163000                 IF WS-AKT-KDBEHX = '1' OR '2'                            
163100                   PERFORM HEA-ANNULLERA-RAD                              
163200                   IF WS-AKT-KDBEHX = '1'                                 
163300                     MOVE +1     TO OBKR-KDKVBRYT                         
163400                   ELSE                                                   
163500                     MOVE +2     TO OBKR-KDKVBRYT                         
163600                   END-IF                                                 
163700                   MOVE +0       TO OBKR-KVPREAVB                         
163800                                      OBKR-KVPRERO                        
163900                   PERFORM S01-SKRIV-MID-TILL-4202-4244                   
164000                 ELSE                                                     
164100                   IF WS-AKT-KDBEHX = 'B' AND OBKR-KDORDKL = +0           
164200*DDGS                AND (OBKR-KDORDBEK = 21 OR 52 OR 53 OR 54 OR         
164300                     AND (OBKR-KDORDBEK = 51 OR 52 OR 53 OR               
164400                                          54 OR 55 OR 57 OR 67)           
164500                     MOVE JA TO OBKR-FLOBOK                               
164600                     PERFORM IMS-06-REPL-ORQM-WDQ101                      
164700                     PERFORM S11-SKRIV-VOR-RAD                            
164800                   ELSE                                                   
164900                     MOVE JA TO OBKR-FLOBOK                               
165000                     PERFORM IMS-06-REPL-ORQM-WDQ101                      
165100                   END-IF                                                 
165200                 END-IF                                                   
165300               END-IF                                                     
165400             END-IF                                                       
165500           END-IF                                                         
165600         END-IF                                                           
165700       END-IF                                                             
165800       ADD +1                  TO WS-INDEX-MID                            
165900     END-PERFORM                                                          
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300 HEA-ANNULLERA-RAD SECTION.                                               
166400                                                                          
166500     IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                           
166600        IF OBKR-IDLEVNR = SPACE                                           
166700                                                                          
166800           IF OBKR-IDARTNR-TILLK > +0                                     
166900              MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                        
167000           ELSE                                                           
167100              MOVE OBKR-IDARTNR   TO W-IDARTNR                            
167200           END-IF                                                         
167300                                                                          
167400           IF OBKR-IDDC NOT = W-IDDC-B6                                   
167500              MOVE OBKR-IDDC TO W-IDDC-B6                                 
167600              PERFORM IMS-GU-WDB601                                       
167700           END-IF                                                         
167800           IF DCS-CDC                                                     
167900             PERFORM IMS-21-GHU-ARTM-WDK901                               
168000                                                                          
168100             PERFORM HEAA-BACKA-WDK9-SALDON                               
168200                                                                          
168300             PERFORM IMS-22-REPL-ARTM-WDK901                              
168400           ELSE                                                           
168500             MOVE OBKR-IDDC TO W-IDDC                                     
168600             PERFORM IMS-13-GHU-WDK711                                    
168700             IF OBKR-KDORDKL = +0 OR +1                                   
168800               SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-DAG                
168900             ELSE                                                         
169000               IF OBKR-KDORDKL = +2 OR +3 OR +4                           
169100                 SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK             
169200               END-IF                                                     
169300             END-IF                                                       
169400             PERFORM IMS-14-REPL-WDK711                                   
169500           END-IF                                                         
169600        END-IF                                                            
169700     END-IF                                                               
169800     PERFORM IMS-08-DLET-ORQM-WDQ101                                      
169900     PERFORM S23-DELETE-PRICE-Q-LINE                                      
170000     .                                                                    
170100     EJECT                                                                
170200                                                                          
170300 HEAA-BACKA-WDK9-SALDON SECTION.                                          
170400                                                                          
170500     IF OBKR-KDORDKL = +0                                                 
170600        IF OBKR-KVPREAVB > +0                                             
170700           COMPUTE ART-KVPREAVB-VOR =                                     
170800           ART-KVPREAVB-VOR - OBKR-KVPREAVB                               
170900        END-IF                                                            
171000        IF OBKR-KVPRERO > +0                                              
171100           COMPUTE ART-KVPRERO-DAG =                                      
171200           ART-KVPRERO-DAG - OBKR-KVPRERO                                 
171300        END-IF                                                            
171400        IF OHUV-IDKAMPRF = +0 AND                                         
171500           OBKR-IDKUNDRF-RO = '0000000   '                                
171600           COMPUTE ART-KVOKS-VOR =                                        
171700           ART-KVOKS-VOR - OBKR-KVBEART-Q                                 
171800        END-IF                                                            
171900     ELSE                                                                 
172000        IF OBKR-KDORDKL = +1                                              
172100           IF OBKR-KVPREAVB > +0                                          
172200              COMPUTE ART-KVPREAVB-DAG =                                  
172300              ART-KVPREAVB-DAG - OBKR-KVPREAVB                            
172400           END-IF                                                         
172500           IF OBKR-KVPRERO > +0                                           
172600              COMPUTE ART-KVPRERO-DAG =                                   
172700              ART-KVPRERO-DAG - OBKR-KVPRERO                              
172800           END-IF                                                         
172900           IF OHUV-IDKAMPRF = +0 AND                                      
173000              OBKR-IDKUNDRF-RO = '0000000   '                             
173100              COMPUTE ART-KVOKS-DAG =                                     
173200              ART-KVOKS-DAG - OBKR-KVBEART-Q                              
173300           END-IF                                                         
173400        ELSE                                                              
173500           IF OBKR-KVPREAVB > +0                                          
173600              COMPUTE ART-KVPREAVB-BULK =                                 
173700              ART-KVPREAVB-BULK - OBKR-KVPREAVB                           
173800           END-IF                                                         
173900           IF OBKR-KVPRERO > +0                                           
174000              COMPUTE ART-KVPRERO-BULK =                                  
174100              ART-KVPRERO-BULK - OBKR-KVPRERO                             
174200           END-IF                                                         
174300           IF OHUV-IDKAMPRF = +0 AND                                      
174400              OBKR-IDKUNDRF-RO = '0000000   '                             
174500              COMPUTE ART-KVOKS-BULK =                                    
174600              ART-KVOKS-BULK - OBKR-KVBEART-Q                             
174700           END-IF                                                         
174800        END-IF                                                            
174900     END-IF                                                               
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
175400                                                                          
175500     MOVE NEJ                  TO AKT-SIDA-SW                             
175600*--- BEHANDLA RESTERANDE OBKR PÅ ORDERN                                   
175700                                                                          
175800     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
175900     MOVE +1                   TO WS-RADER                                
176000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WS-RADER > +13         
176100                                                                          
176200        PERFORM S02-GODKANN-RAD                                           
176300        ADD  +1               TO WS-RADER                                 
176400                                                                          
176500        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
176600     END-PERFORM                                                          
176700                                                                          
176800     IF START-4202                                                        
176900        MOVE 'V'              TO 4202-MID-KDTRTYP                         
177000        PERFORM S03-STARTA-RADBEHANDLINGEN                                
177100     ELSE                                                                 
177200        IF START-4244                                                     
177300           MOVE 'V'           TO 4244-MID-KDTRTYP                         
177400           PERFORM S03-STARTA-RADBEHANDLINGEN                             
177500        ELSE                                                              
177600           IF WS-RADER = 14 AND SEGMENT-FINNS                             
177700              PERFORM HGA-OMSKEDULERA                                     
177800           ELSE                                                           
177900              MOVE JA          TO AVSLUTA-SW                              
178000           END-IF                                                         
178100        END-IF                                                            
178200     END-IF                                                               
178300     .                                                                    
178400     EJECT                                                                
178500 HGA-OMSKEDULERA SECTION.                                                 
178600                                                                          
178700     MOVE MFS-KDMFSFOR         TO 4203-SPRAK                              
178800     MOVE ALL '+'              TO 4203-IDDISTR-IN                         
178900                                  4203-IDKUNDNR-IN                        
179000                                  4203-IDORDNR-IN                         
179100     MOVE WS-IDDISTR           TO 4203-IDDISTR-UT                         
179200     MOVE WS-IDKUNDNR          TO 4203-IDKUNDNR-UT                        
179300     MOVE WS-IDORDNR           TO 4203-IDORDNR-UT                         
179400                                                                          
179500     PERFORM IMS-INSERT-4203-MSG                                          
179600     MOVE JA                   TO HOPP                                    
179700     .                                                                    
179800     EJECT                                                                
179900                                                                          
180000 I-STARTA-ORDERAVSLUT SECTION.                                            
180100                                                                          
180200     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
180300     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
180400     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
180500     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
180600                                                                          
180700     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
180800     PERFORM IMS-INSERT-4298-MSG                                          
180900     .                                                                    
181000     EJECT                                                                
181100                                                                          
181200 Z-FINIT SECTION.                                                         
181300                                                                          
181400     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
181500         CALL WMEDKONV USING MED-WMEDAREA                                 
181600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
181700         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
181800     END-IF                                                               
181900                                                                          
182000     IF NOT ALLT-OK AND NYCKEL-OK                                         
182100        PERFORM MFS-ROER-EJ-BILD                                          
182200     END-IF                                                               
182300                                                                          
182400     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O20301 + 4                        
182500     PERFORM IMS-INSERT-MSG                                               
182600     .                                                                    
182700     EJECT                                                                
182800                                                                          
182900 S01-SKRIV-MID-TILL-4202-4244 SECTION.                                    
183000                                                                          
183100     IF OHUV-IDSYSTEM = '4202'                                            
183200        MOVE JA                TO START-4202-SW                           
183300        ADD +1                 TO 4202-MID-IX                             
183400                                                                          
183500        IF OBKR-IDARTNR-TILLK > +0                                        
183600          MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                           
183700         MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                          
183800        ELSE                                                              
183900          MOVE OBKR-IDARTNR    TO WS-IDARTNR                              
184000          MOVE OBKR-REKSIFFR   TO WS-REKSIFFR                             
184100        END-IF                                                            
184200        MOVE WS-IDARTNR-REKSIFFR TO 4202-MID-IDARTNR(4202-MID-IX)         
184300                                                                          
184400        IF OBKR-KVBEART-TILLK > +0                                        
184500           MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                            
184600           MOVE WS-ALFA-6      TO 4202-MID-KVBEART(4202-MID-IX)           
184700        ELSE                                                              
184800          IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                      
184900           COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                
185000           MOVE WS-ALFA-6      TO 4202-MID-KVBEART(4202-MID-IX)           
185100          ELSE                                                            
185200             MOVE OBKR-KVBEART TO WS-NUM-6                                
185300             MOVE WS-ALFA-6    TO 4202-MID-KVBEART(4202-MID-IX)           
185400          END-IF                                                          
185500        END-IF                                                            
185600                                                                          
185700        MOVE ALL '+'           TO 4202-MID-PRARTNTO(4202-MID-IX)          
185800        IF OBKR-TITPO > +0                                                
185900           MOVE OBKR-TITPO     TO WS-NUM-6                                
186000           MOVE WS-ALFA-6      TO 4202-MID-TITPO(4202-MID-IX)             
186100        ELSE                                                              
186200           MOVE ALL '+'        TO 4202-MID-TITPO(4202-MID-IX)             
186300        END-IF                                                            
186400        EJECT                                                             
186500        MOVE OBKR-FLRESTN      TO 4202-MID-FLRESTN(4202-MID-IX)           
186600        MOVE OBKR-FLSLATT      TO 4202-MID-FLSLATT(4202-MID-IX)           
186700        MOVE OBKR-KDKVBRYT     TO 4202-MID-KDKVBRYT(4202-MID-IX)          
186800        MOVE OBKR-FLINVEST     TO 4202-MID-FLINVEST(4202-MID-IX)          
186900        MOVE OBKR-KDVRINFO     TO 4202-MID-KDVRINFO(4202-MID-IX)          
187000        MOVE OBKR-BERADREF     TO 4202-MID-BERADREF(4202-MID-IX)          
187100     ELSE                                                                 
187200        MOVE JA                TO START-4244-SW                           
187300        ADD +1                 TO 4244-MID-IX                             
187400                                                                          
187500        IF OBKR-IDARTNR-TILLK > +0                                        
187600          MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                           
187700         MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                          
187800        ELSE                                                              
187900          MOVE OBKR-IDARTNR    TO WS-IDARTNR                              
188000          MOVE OBKR-REKSIFFR   TO WS-REKSIFFR                             
188100        END-IF                                                            
188200        MOVE WS-IDARTNR-REKSIFFR TO 4244-MID-IDARTNR(4244-MID-IX)         
188300                                                                          
188400        IF OBKR-KVBEART-TILLK > +0                                        
188500           MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                            
188600           MOVE WS-ALFA-6      TO 4244-MID-KVBEART(4244-MID-IX)           
188700        ELSE                                                              
188800          IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                      
188900           COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                
189000           MOVE WS-ALFA-6      TO 4244-MID-KVBEART(4244-MID-IX)           
189100          ELSE                                                            
189200             MOVE OBKR-KVBEART TO WS-NUM-6                                
189300             MOVE WS-ALFA-6    TO 4244-MID-KVBEART(4244-MID-IX)           
189400          END-IF                                                          
189500        END-IF                                                            
189600                                                                          
189700        IF OBKR-TITPO > +0                                                
189800           MOVE OBKR-TITPO     TO WS-NUM-6                                
189900           MOVE WS-ALFA-6      TO 4244-MID-TITPO(4244-MID-IX)             
190000        ELSE                                                              
190100           MOVE ALL '+'        TO 4244-MID-TITPO(4244-MID-IX)             
190200        END-IF                                                            
190300        EJECT                                                             
190400        MOVE OBKR-FLRESTN      TO 4244-MID-FLRESTN(4244-MID-IX)           
190500        MOVE OBKR-FLSLATT      TO 4244-MID-FLSLATT(4244-MID-IX)           
190600        MOVE OBKR-KDKVBRYT     TO 4244-MID-KDKVBRYT(4244-MID-IX)          
190700        MOVE OBKR-BERADREF     TO 4244-MID-BERADREF(4244-MID-IX)          
190800     END-IF                                                               
190900     .                                                                    
191000     EJECT                                                                
191100                                                                          
191200 S02-GODKANN-RAD SECTION.                                                 
191300                                                                          
191400     IF OBKR-IDDC NOT = W-IDDC-B6                                         
191500        MOVE OBKR-IDDC TO W-IDDC-B6                                       
191600        PERFORM IMS-GU-WDB601                                             
191700     END-IF                                                               
191800                                                                          
191900     MOVE JA                   TO OBKR-FLOBOK                             
192000     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
192100                                                                          
192200     IF ((OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0)                        
192300            AND OBKR-KDORDBEK NOT = 92 AND 98) OR                         
192400               (OBKR-KDORDBEK = 92 OR 98 AND OBKR-KVPREAVB > +0)          
192500                                                                          
192600        PERFORM S14-LAS-ARTIKELREG                                        
192700        PERFORM S12-BYGG-UPP-ORDERRAD                                     
192800                                                                          
192900        PERFORM S09-KONTROLLERA-ENHETSLAST                                
193000        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
193100     END-IF                                                               
193200                                                                          
193300     IF (OBKR-KDORDBEK =    61) AND AKT-SIDA                              
193400       PERFORM S01-SKRIV-MID-TILL-4202-4244                               
193500       PERFORM S02B-SKRIV-ORDERBEKR-40                                    
193600     ELSE                                                                 
193700       IF OBKR-KDORDBEK =    70 OR 74 OR 71                               
193800         IF OBKR-KDTPOTYP = +2                                            
193900           PERFORM S14-LAS-ARTIKELREG                                     
194000           PERFORM S02C-UPPDATERA-TPO2                                    
194100         ELSE                                                             
194200           IF OBKR-KDTPOTYP = +6                                          
194300              PERFORM S14-LAS-ARTIKELREG                                  
194400              PERFORM S02D-UPPDATERA-TPO6                                 
194500           END-IF                                                         
194600         END-IF                                                           
194700       ELSE                                                               
194800         IF (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR                      
194900                             55 OR 57 OR 67 OR 92 OR 98)                  
195000                        AND    OHUV-KDORDKL = +0                          
195100                                                                          
195200            PERFORM S11-SKRIV-VOR-RAD                                     
195300         END-IF                                                           
195400       END-IF                                                             
195500     END-IF                                                               
195600     .                                                                    
195700     EJECT                                                                
195800 S02B-SKRIV-ORDERBEKR-40 SECTION.                                         
195900                                                                          
196000     MOVE OBKR-KVBEART-TILLK   TO SPAR-KVBEART-TILLK                      
196100     MOVE OBKR-IDARTNR-TILLK   TO SPAR-IDARTNR-TILLK                      
196200     MOVE OBKR-REKSIFFR-TILLK  TO SPAR-REKSIFFR-TILLK                     
196300     MOVE OBKR-DIERS-KVOT      TO SPAR-DIERS-KVOT                         
196400                                                                          
196500     IF OBKR-IDARTNR NOT = SPAR-IDARTNR-40  OR                            
196600         (OBKR-IDARTNR = SPAR-IDARTNR-40   AND                            
196700          OBKR-IDLOPNR NOT = SPAR-IDLOPNR-40)                             
196800                                                                          
196900        PERFORM S02BA-SKRIV-KOD40-ERSATT-ART                              
197000        COMPUTE SPAR-IDSEKVNR-40 = OBKR-IDSEKVNR + 1                      
197100     END-IF                                                               
197200                                                                          
197300     MOVE SPAR-KVBEART-TILLK   TO OBKR-KVBEART-TILLK                      
197400     MOVE SPAR-IDARTNR-TILLK   TO OBKR-IDARTNR-TILLK                      
197500     MOVE SPAR-REKSIFFR-TILLK  TO OBKR-REKSIFFR-TILLK                     
197600     MOVE SPAR-DIERS-KVOT      TO OBKR-DIERS-KVOT                         
197700                                                                          
197800     MOVE JA                   TO OBKR-FLOBOK                             
197900     MOVE 40                   TO OBKR-KDORDBEK                           
198000     MOVE IDPGM                TO OBKR-IDPGM                              
198100     MOVE SPAR-IDSEKVNR-40     TO OBKR-IDSEKVNR                           
198200                                                                          
198300     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
198400     ADD +1                 TO OBKR-IDSEKVNR                              
198500                               SPAR-IDSEKVNR-40                           
198600     MOVE OBKR-IDARTNR         TO SPAR-IDARTNR-40                         
198700     MOVE OBKR-IDLOPNR         TO SPAR-IDLOPNR-40                         
198800                                                                          
198900     PERFORM S02BB-UPPDAT-ERSATT-ART                                      
199000     .                                                                    
199100     EJECT                                                                
199200 S02BA-SKRIV-KOD40-ERSATT-ART SECTION.                                    
199300                                                                          
199400     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
199500                               W-Q1-IDARTNR-MAX1                          
199600     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
199700                               W-Q1-IDLOPNR-MAX1                          
199800     MOVE +3                TO W-Q1-IDSEKVNR-MIN1                         
199900     MOVE +999              TO W-Q1-IDSEKVNR-MAX1                         
200000                                                                          
200100     PERFORM IMS-34-GN-ORQM-WDQ101                                        
200200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
200300                                                                          
200400        PERFORM IMS-34-GN-ORQM-WDQ101                                     
200500     END-PERFORM                                                          
200600                                                                          
200700     MOVE +0                TO OBKR-IDARTNR-TILLK                         
200800                               OBKR-KVBEART-TILLK                         
200900                               OBKR-REKSIFFR-TILLK                        
201000                               OBKR-DIERS-KVOT                            
201100     MOVE JA                TO OBKR-FLOBOK                                
201200     MOVE 40                TO OBKR-KDORDBEK                              
201300     MOVE IDPGM             TO OBKR-IDPGM                                 
201400     ADD +1                 TO OBKR-IDSEKVNR                              
201500                                                                          
201600     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
201700     .                                                                    
201800     EJECT                                                                
201900 S02BB-UPPDAT-ERSATT-ART SECTION.                                         
202000                                                                          
202100*--VID VAL AV ERSÄTTNING SÄTTS FLOBTRAN TILL 'N' FÖR ATT FÖRHINDRA        
202200*--ATT ORDERBEKRÄFTELSETRANS SKICKAS FRÅN W4029300 TILL VR/VIPS           
202300     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
202400                               W-Q1-IDARTNR-MAX1                          
202500     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
202600                               W-Q1-IDLOPNR-MAX1                          
202700     MOVE +1                TO W-Q1-IDSEKVNR-MIN1                         
202800                               W-Q1-IDSEKVNR-MAX1                         
202900     PERFORM IMS-36-GHU-ORQM-WDQ101                                       
203000     MOVE NEJ                TO OBKR-FLOBTRAN                             
203100     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
203200     .                                                                    
203300     EJECT                                                                
203400 S02C-UPPDATERA-TPO2 SECTION.                                             
203500                                                                          
203600     IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                  
203700       PERFORM S12-BYGG-UPP-ORDERRAD                                      
203800                                                                          
203900       MOVE ORAD-IDDISTR       TO TPO2-IDDISTR                            
204000       MOVE ORAD-IDKUNDNR      TO TPO2-IDKUNDNR                           
204100       MOVE ORAD-IDKUNDRF      TO TPO2-IDKUNDRF                           
204200       MOVE ORAD-IDARTNR       TO TPO2-IDARTNR                            
204300       MOVE ORAD-BERADREF      TO TPO2-BERADREF                           
204400       MOVE AREG-IDANSK        TO TPO2-IDANSK                             
204500       MOVE OHUV-IDKONTO       TO TPO2-IDKONTO                            
204600       MOVE OHUV-IDKST         TO TPO2-IDKST                              
204700       MOVE OHUV-IDANALYS      TO TPO2-IDANALYS                           
204800       MOVE ORAD-KDDSP         TO TPO2-KDDSP                              
204900       MOVE OHUV-KDFAKTYP      TO TPO2-KDFAKTYP                           
205000       MOVE SPAR-KDFRAKT       TO TPO2-KDFRAKT                            
205100       MOVE ORAD-KDKVBRYT      TO TPO2-KDKVBRYT                           
205200       MOVE ORAD-KDORDING      TO TPO2-KDORDING                           
205300       MOVE OHUV-KDORDKL       TO TPO2-KDORDKL                            
205400       MOVE ORAD-KDPRODSL      TO TPO2-KDPRODSL                           
205500       MOVE +2                 TO TPO2-KDTPOTYP                           
205600       MOVE ORAD-KDVRINFO      TO TPO2-KDVRINFO                           
205700       MOVE ORAD-KVBEART-Q     TO TPO2-KVBEART-Q                          
205800       MOVE ORAD-PRARTNTO      TO TPO2-PRARTNTO                           
205900       MOVE ORAD-DEAL-PR-LINE  TO TPO2-DEAL-PR-LINE                       
206000       MOVE ORAD-REKSIFFR      TO TPO2-REKSIFFR                           
206100       MOVE ORAD-TITPO         TO TPO2-TITPO                              
206200       MOVE ORAD-KDPRTYP       TO TPO2-KDPRTYP                            
206300       MOVE ORAD-BEVOLREF      TO TPO2-BEVOLREF                           
206400       MOVE ORAD-FLINVEST      TO TPO2-FLINVEST                           
206500       MOVE OHUV-FLORDSPE      TO TPO2-FLORDSPE                           
206600       MOVE OHUV-FLOVRLEV      TO TPO2-FLOVRLEV                           
206700       MOVE ORAD-FLPRTILL      TO TPO2-FLPRTILL                           
206800       MOVE OHUV-BEKUNDRF      TO TPO2-BEKUNDRF                           
206900       MOVE ORAD-IDKAMPRF      TO TPO2-IDKAMPRF                           
207000       MOVE OHUV-FLFORBI       TO TPO2-FLFORBI                            
207100       MOVE ORAD-IDLEVNR       TO TPO2-IDLEVNR                            
207200       MOVE ORAD-IDSYSTEM      TO TPO2-IDSYSTEM                           
207300       MOVE AREG-KDUART        TO TPO2-KDUART                             
207400       MOVE AREG-KVFRYSTI      TO TPO2-KVFRYSTI                           
207500       IF OBKR-KDORDBEK = 71                                              
207600          MOVE +3              TO TPO2-KDORDBEH                           
207700       ELSE                                                               
207800          MOVE +2              TO TPO2-KDORDBEH                           
207900       END-IF                                                             
208000       MOVE ORAD-FLTILLK       TO TPO2-FLTILLK                            
208100       MOVE AREG-TIDISPIN      TO TPO2-TIDISPIN                           
208200       MOVE OHUV-BEVARREF      TO TPO2-BEVARREF                           
208300       MOVE OBKR-KVQPACK       TO TPO2-KVQPACK-1                          
208400       MOVE ORAD-KVBEART       TO TPO2-KVBEART                            
208500                                                                          
208600       MOVE OHUV-KDORDTYP-LDC  TO TPO2-KDORDTYP-LDC                       
208700       MOVE OHUV-TIREPDAT      TO TPO2-TIREPDAT                           
208800       MOVE ORAD-IDKUNDRF-WIP  TO TPO2-IDKUNDRF-WIP                       
208900                                                                          
209000       CALL W411TPO2 USING TPO2-W411TPO2                                  
209100                         TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB        
209200                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
209300                           TIME-4437-PCB                                  
209400     END-IF                                                               
209500     .                                                                    
209600     EJECT                                                                
209700                                                                          
209800 S02D-UPPDATERA-TPO6 SECTION.                                             
209900                                                                          
210000     IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                  
210100       PERFORM S12-BYGG-UPP-ORDERRAD                                      
210200                                                                          
210300       MOVE ORAD-IDDISTR       TO TPO6-IDDISTR                            
210400       MOVE ORAD-IDKUNDNR      TO TPO6-IDKUNDNR                           
210500       MOVE ORAD-IDKUNDRF      TO TPO6-IDKUNDRF                           
210600       MOVE ORAD-IDARTNR       TO TPO6-IDARTNR                            
210700       MOVE OHUV-IDDC-PRIM     TO TPO6-IDDC-DAY                           
210800       MOVE AREG-FLREFILL      TO TPO6-FLREFILL                           
210900       MOVE AREG-KDUART        TO TPO6-KDUART                             
211000       MOVE AREG-REDIRLEV      TO TPO6-REDIRLEV                           
211100       MOVE ORAD-BERADREF      TO TPO6-BERADREF                           
211200       MOVE AREG-IDANSK        TO TPO6-IDANSK                             
211300       MOVE OHUV-IDKONTO       TO TPO6-IDKONTO                            
211400       MOVE OHUV-IDKST         TO TPO6-IDKST                              
211500       MOVE ORAD-KDDSP         TO TPO6-KDDSP                              
211600       MOVE OHUV-KDFAKTYP      TO TPO6-KDFAKTYP                           
211700       MOVE SPAR-KDFRAKT       TO TPO6-KDFRAKT                            
211800       MOVE ORAD-KDKVBRYT      TO TPO6-KDKVBRYT                           
211900       MOVE ORAD-KDORDING      TO TPO6-KDORDING                           
212000       MOVE OHUV-KDORDKL       TO TPO6-KDORDKL                            
212100       MOVE ORAD-KDPRODSL      TO TPO6-KDPRODSL                           
212200       MOVE +6                 TO TPO6-KDTPOTYP                           
212300       MOVE ORAD-KDVRINFO      TO TPO6-KDVRINFO                           
212400       MOVE ORAD-KVBEART-Q     TO TPO6-KVBEART-Q                          
212500       MOVE ORAD-PRARTNTO      TO TPO6-PRARTNTO                           
212600       MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                       
212700       MOVE AREG-REKSIFFR      TO TPO6-REKSIFFR                           
212800       MOVE ORAD-KDPRTYP       TO TPO6-KDPRTYP                            
212900       MOVE ORAD-BEVOLREF      TO TPO6-BEVOLREF                           
213000       MOVE ORAD-FLINVEST      TO TPO6-FLINVEST                           
213100       MOVE ORAD-FLPRTILL      TO TPO6-FLPRTILL                           
213200       MOVE OHUV-BEKUNDRF      TO TPO6-BEKUNDRF                           
213300       MOVE ORAD-IDKAMPRF      TO TPO6-IDKAMPRF                           
213400       MOVE ORAD-IDLEVNR       TO TPO6-IDLEVNR                            
213500       MOVE ORAD-IDSYSTEM      TO TPO6-IDSYSTEM                           
213600       MOVE OHUV-FLFORBI       TO TPO6-FLFORBI                            
213700                                                                          
213800       MOVE OHUV-KDORDTYP-LDC  TO TPO6-KDORDTYP-LDC                       
213900       MOVE OHUV-TIREPDAT      TO TPO6-TIREPDAT                           
214000       MOVE ORAD-IDKUNDRF-WIP  TO TPO6-IDKUNDRF-WIP                       
214100                                                                          
214200       MOVE ORAD-KDOI          TO TPO6-KDOI                               
214300       MOVE ORAD-CLEARGROUP    TO TPO6-CLEARGROUP                         
214400                                                                          
214500       CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB           
214600           TPO6-XXBU-PCB TPO6-XXBV-PCB TPO6-XXBX-PCB                      
214700             TPO6-ARTS-PCB TIME-4437-PCB                                  
214800     END-IF                                                               
214900     .                                                                    
215000     EJECT                                                                
215100                                                                          
215200 S03-STARTA-RADBEHANDLINGEN SECTION.                                      
215300                                                                          
215400     IF OHUV-IDSYSTEM = '4202'                                            
215500        MOVE MFS-KDMFSFOR      TO 4202-SPRAK                              
215600                                                                          
215700        MOVE ALL '+'           TO 4202-MID-IDDISTR-IN                     
215800                                     4202-MID-IDKUNDNR-IN                 
215900                                     4202-MID-IDORDNR-IN                  
216000        MOVE WS-IDDISTR        TO 4202-MID-IDDISTR-UT                     
216100        MOVE WS-IDKUNDNR       TO 4202-MID-IDKUNDNR-UT                    
216200        MOVE WS-IDORDNR        TO 4202-MID-IDORDNR-UT                     
216300                                                                          
216400        PERFORM IMS-INSERT-4202-MSG                                       
216500        MOVE JA                TO HOPP                                    
216600     ELSE                                                                 
216700        MOVE MFS-KDMFSFOR      TO 4244-SPRAK                              
216800                                                                          
216900        MOVE ALL '+'           TO 4244-MID-IDDISTR-IN                     
217000                                     4244-MID-IDKUNDNR-IN                 
217100                                     4244-MID-IDORDNR-IN                  
217200        MOVE WS-IDDISTR        TO 4244-MID-IDDISTR-UT                     
217300        MOVE WS-IDKUNDNR       TO 4244-MID-IDKUNDNR-UT                    
217400        MOVE WS-IDORDNR        TO 4244-MID-IDORDNR-UT                     
217500                                                                          
217600        PERFORM IMS-INSERT-4244-MSG                                       
217700        MOVE JA                TO HOPP                                    
217800     END-IF                                                               
217900                                                                          
218000     .                                                                    
218100     EJECT                                                                
218200                                                                          
218300 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
218400                                                                          
218500                                                                          
218600     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
218700     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
218800     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
218900     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
219000     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
219100     MOVE ORAD-IDDC            TO LAST-IDDC                               
219200     MOVE SPAR-KDFDKRAV        TO LAST-KDFDKRAV                           
219300     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
219400     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
219500     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
219600                                                                          
219700     CALL W411LAST USING LAST-W411LAST                                    
219800     .                                                                    
219900     EJECT                                                                
220000 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
220100                                                                          
220200     IF LAST-ADLAGOMR-UT = +0 AND                                         
220300        LAST-KVANTAL-UT  = +0 AND                                         
220400        LAST-KVBEART-UT  = +0                                             
220500*------------------------------------------------------------*            
220600*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
220700*------------------------------------------------------------*            
220800        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
220900        PERFORM S10B-REDIGERA-WOPS-AREA                                   
221000        PERFORM IMS-17-ISRT-ORQF-WDQ401                                   
221100        PERFORM UNTIL SEGMENT-FINNS                                       
221200           ADD +1                    TO ORAD-IDLOPNR                      
221300           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
221400        END-PERFORM                                                       
221500     ELSE                                                                 
221600                                                                          
221700        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
221800*------------------------------------------------------------*            
221900*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
222000*------------------------------------------------------------*            
222100                                                                          
222200           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
222300           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
222400                                    ORAD-KVPRERO                          
222500           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
222600           PERFORM S10C-BERAEKNA-KVSLATT                                  
222700           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
222800           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
222900           MOVE ORAD-ADGANG          TO WS-ADGANG                         
223000           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
223100           PERFORM S10B-REDIGERA-WOPS-AREA                                
223200           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
223300           PERFORM UNTIL SEGMENT-FINNS                                    
223400              ADD +1                 TO ORAD-IDLOPNR                      
223500              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
223600           END-PERFORM                                                    
223700*------------------------------------------------------------*            
223800*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
223900*------------------------------------------------------------*            
224000                                                                          
224100           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
224200           MOVE WS-ADGANG            TO ORAD-ADGANG                       
224300           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
224400                                                                          
224500           MOVE +0                   TO ORAD-KVBEART                      
224600           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
224700           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
224800           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
224900             CONTINUE                                                     
225000           ELSE                                                           
225100             IF LAST-ADGANG-UT > ZERO                                     
225200               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
225300             END-IF                                                       
225400           END-IF                                                         
225500           MOVE +0                   TO ORAD-KVPRERO                      
225600           MOVE 1.0000               TO ORAD-RERF-RAD                     
225700           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
225800           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
225900           PERFORM S10B-REDIGERA-WOPS-AREA                                
226000           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
226100           PERFORM UNTIL SEGMENT-FINNS                                    
226200              ADD +1                 TO ORAD-IDLOPNR                      
226300              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
226400           END-PERFORM                                                    
226500        ELSE                                                              
226600*------------------------------------------------------------*            
226700*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
226800*------------------------------------------------------------*            
226900           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
227000           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
227100             CONTINUE                                                     
227200           ELSE                                                           
227300             IF LAST-ADGANG-UT > ZERO                                     
227400               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
227500             END-IF                                                       
227600           END-IF                                                         
227700           MOVE 1.0000            TO ORAD-RERF-RAD                        
227800           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
227900           PERFORM S10B-REDIGERA-WOPS-AREA                                
228000           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
228100           PERFORM UNTIL SEGMENT-FINNS                                    
228200              ADD +1              TO ORAD-IDLOPNR                         
228300              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
228400           END-PERFORM                                                    
228500        END-IF                                                            
228600     END-IF                                                               
228700     .                                                                    
228800     EJECT                                                                
228900 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
229000                                                                          
229100     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
229200     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
229300     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
229400     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
229500     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
229600     MOVE 1                    TO ADRS-KDCALL-IN                          
229700     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
229800     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
229900     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
230000     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
230100                                                                          
230200     CALL W413ADRS USING ADRS-W413ADRS                                    
230300                                                                          
230400*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
230500     IF ORAD-IDDC NOT = W-IDDC-B6                                         
230600        MOVE ORAD-IDDC TO W-IDDC-B6                                       
230700        PERFORM IMS-GU-WDB601                                             
230800     END-IF                                                               
230900     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
231000     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
231100                                                                          
231200     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
231300     .                                                                    
231400     EJECT                                                                
231500 S10B-REDIGERA-WOPS-AREA SECTION.                                         
231600                                                                          
231700     MOVE +1                   TO AVSR-KDCALL                             
231800     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
231900     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
232000     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
232100     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
232200     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
232300     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
232400                                                                          
232500     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
232600     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
232700     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
232800     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
232900     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
233000     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
233100     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
233200     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
233300     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE                       
233400                                               (WS-INDEX-WOPS)            
233500     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
233600     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
233700     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
233800     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
233900                                                                          
234000     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                       
234100                         AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB        
234200                         AVSR-WDB2-PCB AVSR-WDB6-PCB                      
234300                         TRAN-XXKB-PCB                                    
234400     .                                                                    
234500     EJECT                                                                
234600 S10C-BERAEKNA-KVSLATT SECTION.                                           
234700                                                                          
234800     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
234900                                                                          
235000        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
235100                                                                          
235200        COMPUTE ORAD-KVSLATT ROUNDED =                                    
235300               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
235400     END-IF                                                               
235500     .                                                                    
235600     EJECT                                                                
235700 S11-SKRIV-VOR-RAD SECTION.                                               
235800                                                                          
235900     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
236000     IF OBKR-IDDC NOT = W-IDDC-B6                                         
236100        MOVE OBKR-IDDC TO W-IDDC-B6                                       
236200        PERFORM IMS-GU-WDB601                                             
236300     END-IF                                                               
236400                                                                          
236500     IF OBKR-IDARTNR-TILLK > +0                                           
236600        MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                              
236700                                   4542-IDARTNR                           
236800     ELSE                                                                 
236900        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
237000                                  4542-IDARTNR                            
237100     END-IF                                                               
237200     MOVE OBKR-IDDC            TO W-IDDC                                  
237300                                  4542-IDDC                               
237400     PERFORM IMS-GU-WDK722                                                
237500     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
237600        MOVE XLAG-IDANSK       TO WS-IDANSK                               
237700     ELSE                                                                 
237800        PERFORM IMS-11-GU-WDK611                                          
237900        MOVE CLAG-IDANSK       TO WS-IDANSK                               
238000     END-IF                                                               
238100     MOVE WS-IDANSK            TO 4542-IDANSK                             
238200                                                                          
238300     MOVE +1                   TO 4542-IDLOPNR                            
238400     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
238500     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
238600     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
238700     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
238800     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
238900     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
239000     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
239100     MOVE ZERO                 TO 4542-KDVORATG                           
239200     IF 4542-KDORDBEK = 92 OR 98                                          
239300       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
239400       MOVE 4542-KVBEART       TO 4542-KVBEART-Q                          
239500       MOVE OBKR-KVPREAVB      TO 4542-KVPREAVB                           
239600     ELSE                                                                 
239700       MOVE OBKR-KVBEART       TO 4542-KVBEART-Q                          
239800       MOVE +0                 TO 4542-KVPREAVB                           
239900     END-IF                                                               
240000     EJECT                                                                
240100     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
240200     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
240300     MOVE SPACE                TO 4542-TEVORMRK                           
240400     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
240500     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
240600     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
240700     MOVE +0                   TO 4542-TIUPPDAT                           
240800     MOVE +0                   TO 4542-TIUPPTID                           
240900     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
241000                                                                          
241100*---------------------------------------UPPLÄGG TILL NY VORKÖ             
241200*                                       SKER I W40293                     
241300     IF DCS-NDC                                                           
241400         PERFORM IMS-19-ISRT-4541-WDR411                                  
241500         PERFORM UNTIL SEGMENT-FINNS                                      
241600                                                                          
241700            ADD +1             TO 4542-IDLOPNR                            
241800            PERFORM IMS-19-ISRT-4541-WDR411                               
241900         END-PERFORM                                                      
242000                                                                          
242100         PERFORM IMS-13-GHU-WDK711                                        
242200         IF  DCS-NDC-CN                                                   
242300         OR (DCS-NDC-NA AND DCS-USA)                                      
242400           IF SLAG-IDDC-REF = SPACE                                       
242500             PERFORM S11A-STARTA-W2T191X                                  
242600           END-IF                                                         
242700         END-IF                                                           
242800     END-IF                                                               
242900                                                                          
243000     IF 4542-KDORDBEK NOT = 92 AND                                        
243100        4542-KDORDBEK NOT = 98 AND                                        
243200        4542-IDLEVNR = SPACE                                              
243300        IF DCS-CDC                                                        
243400          MOVE 4542-IDARTNR    TO W-IDARTNR                               
243500          PERFORM IMS-21-GHU-ARTM-WDK901                                  
243600          COMPUTE ART-KVOKS-VOR =                                         
243700            ART-KVOKS-VOR + (4542-KVBEART-Q - 4542-KVPREAVB)              
243800          PERFORM IMS-22-REPL-ARTM-WDK901                                 
243900        END-IF                                                            
244000     END-IF                                                               
244100     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
244200     .                                                                    
244300     EJECT                                                                
244400                                                                          
244500 S11A-STARTA-W2T191X   SECTION.                                           
244600                                                                          
244700     MOVE +1                    TO 2191-MID-KDCLAGER                      
244800     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
244900     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
245000                                   2191-MID-TISENBEK-KL                   
245100     MOVE SPACE                 TO 2191-MID-IDKR                          
245200     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
245300     MOVE '500'                 TO 2191-MID-KDLARM                        
245400     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
245500     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
245600     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
245700     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
245800     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
245900     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
246000     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
246100     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
246200                                                                          
246300     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
246400     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
246500     MOVE '4203'                TO MSG-IDTRANS-1                          
246600     MOVE '1'                   TO MSG-KDMFSFOR-1                         
246700     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
246800                                                                          
246900     PERFORM IMS-PURG-MSG-2191                                            
247000                                                                          
247100     MOVE SPACE                 TO 2191-MID-W2I19101                      
247200     .                                                                    
247300     EJECT                                                                
247400                                                                          
247500 S12-BYGG-UPP-ORDERRAD SECTION.                                           
247600                                                                          
247700     MOVE OBKR-IDORDER         TO ORAD-IDORDER                            
247800     MOVE OBKR-IDDC            TO ORAD-IDDC                               
247900     IF OBKR-IDDC NOT = W-IDDC-B6                                         
248000        MOVE OBKR-IDDC TO W-IDDC-B6                                       
248100        PERFORM IMS-GU-WDB601                                             
248200     END-IF                                                               
248300                                                                          
248400     IF DCS-CDC OR DCS-DDC                                                
248500       MOVE AREG-ADLAGOMR      TO ORAD-ADLAGOMR                           
248600       MOVE AREG-ADGANG        TO ORAD-ADGANG                             
248700       MOVE AREG-ADPLATS       TO ORAD-ADPLATS                            
248800       MOVE AREG-KDARTURS      TO ORAD-KDARTURS                           
248900       MOVE AREG-VKART         TO ORAD-VKART                              
249000       MOVE AREG-VKART-NTO     TO ORAD-VKART-NTO                          
249100       MOVE AREG-VLARTNTO      TO ORAD-VLARTNTO                           
249200     ELSE                                                                 
249300       MOVE AREG-IDARTNR       TO W-IDARTNR                               
249400       MOVE ORAD-IDDC          TO W-IDDC                                  
249500       PERFORM IMS-13-GHU-WDK711                                          
249600       MOVE SLAG-ADLAGOMR      TO ORAD-ADLAGOMR                           
249700       MOVE SLAG-ADGANG        TO ORAD-ADGANG                             
249800       MOVE SLAG-ADPLATS       TO ORAD-ADPLATS                            
249900       MOVE DCS-IDLANDX2       TO W-IDLAND                                
250000       IF DCS-NDC                                                         
250100         PERFORM IMS-GU-WDK712                                            
250200         IF SEGMENT-FINNS                                                 
250300            IF LART-KDARTURS > SPACE                                      
250400              MOVE LART-KDARTURS TO ORAD-KDARTURS                         
250500            ELSE                                                          
250600              MOVE AREG-KDARTURS TO ORAD-KDARTURS                         
250700            END-IF                                                        
250800            IF LART-VKART> ZERO AND                                       
250900               LART-VKART NOT = AREG-VKART                                
251000               MOVE LART-VKART     TO ORAD-VKART                          
251100                                      ORAD-VKART-NTO                      
251200            ELSE                                                          
251300               MOVE AREG-VKART     TO ORAD-VKART                          
251400               MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                      
251500            END-IF                                                        
251600            IF LART-VLARTNTO > 0                                          
251700               MOVE LART-VLARTNTO TO ORAD-VLARTNTO                        
251800            ELSE                                                          
251900               MOVE AREG-VLARTNTO TO ORAD-VLARTNTO                        
252000            END-IF                                                        
252100         ELSE                                                             
252200            MOVE AREG-KDARTURS  TO ORAD-KDARTURS                          
252300            MOVE AREG-VKART     TO ORAD-VKART                             
252400            MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                         
252500            MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                          
252600         END-IF                                                           
252700       ELSE                                                               
252800          MOVE AREG-KDARTURS   TO ORAD-KDARTURS                           
252900          MOVE AREG-VKART      TO ORAD-VKART                              
253000          MOVE AREG-VKART-NTO  TO ORAD-VKART-NTO                          
253100          MOVE AREG-VLARTNTO   TO ORAD-VLARTNTO                           
253200       END-IF                                                             
253300     END-IF                                                               
253400     MOVE AREG-IDARTNR         TO ORAD-IDARTNR                            
253500     MOVE +1                   TO ORAD-IDLOPNR                            
253600     MOVE OBKR-BERADREF        TO ORAD-BERADREF                           
253700     MOVE OBKR-BEVOLREF        TO ORAD-BEVOLREF                           
253800     MOVE OBKR-FLAKPLOC        TO ORAD-FLAKPLOC                           
253900     MOVE OBKR-FLINVEST        TO ORAD-FLINVEST                           
254000     MOVE OBKR-FLOBTRAN        TO ORAD-FLOBTRAN                           
254100     MOVE OBKR-FLPRTILL        TO ORAD-FLPRTILL                           
254200     MOVE OBKR-FLRESTN         TO ORAD-FLRESTN                            
254300     MOVE OBKR-FLTILLK         TO ORAD-FLTILLK                            
254400     MOVE 'N'                  TO ORAD-FLSDCLEV                           
254500     MOVE OBKR-IDDC-RO         TO ORAD-IDDC-RO                            
254600     MOVE OBKR-IDDISTR         TO ORAD-IDDISTR                            
254700     MOVE OBKR-IDKUNDNR        TO ORAD-IDKUNDNR                           
254800     MOVE OBKR-IDKUNDRF        TO ORAD-IDKUNDRF                           
254900     MOVE OBKR-IDKAMPRF        TO ORAD-IDKAMPRF                           
255000     MOVE OBKR-IDLEVNR         TO ORAD-IDLEVNR                            
255100     MOVE OBKR-IDLOPNR-RO      TO ORAD-IDLOPNR-RO                         
255200     MOVE OBKR-IDKUNDRF-RO     TO ORAD-IDKUNDRF-RO                        
255300     MOVE +0                   TO ORAD-IDSPECEMB                          
255400     MOVE SPACE                TO ORAD-IDBIL                              
255500                                  ORAD-IDKLIENT                           
255600                                  ORAD-IDARBREF                           
255700                                  ORAD-IDVIN                              
255800     MOVE OBKR-IDSYSTEM        TO ORAD-IDSYSTEM                           
255900     MOVE OBKR-KDDSP           TO ORAD-KDDSP                              
256000     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
256100     MOVE OBKR-KDKVBRYT        TO ORAD-KDKVBRYT                           
256200     MOVE OBKR-KDOI            TO ORAD-KDOI                               
256300     MOVE OBKR-CLEARGROUP      TO ORAD-CLEARGROUP                         
256400     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
256500     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
256600     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
256700     MOVE OBKR-KDPRTYP         TO ORAD-KDPRTYP                            
256800     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
256900     MOVE OBKR-KDTPOTYP        TO ORAD-KDTPOTYP                           
257000     MOVE OBKR-KDVRINFO        TO ORAD-KDVRINFO                           
257100     MOVE OBKR-KVPREAVB        TO ORAD-KVPREAVB                           
257200     MOVE OBKR-KVPRERO         TO ORAD-KVPRERO                            
257300     MOVE +0                   TO ORAD-KVOKS-PREL                         
257400     MOVE OBKR-KVBEART         TO ORAD-KVBEART                            
257500     MOVE JA                   TO ORAD-FLORDING                           
257600     IF OBKR-KDORDBEK = 92 OR 98                                          
257700       COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO                 
257800       MOVE OBKR-KVPREAVB      TO ORAD-KVBEART-Q                          
257900       MOVE +0                 TO ORAD-KVPRERO                            
258000     ELSE                                                                 
258100       IF OBKR-KVPREAVB > +0     OR  OBKR-KVPRERO > +0                    
258200          COMPUTE ORAD-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO           
258300       ELSE                                                               
258400          MOVE OBKR-KVBEART-Q  TO ORAD-KVBEART-Q                          
258500       END-IF                                                             
258600     END-IF                                                               
258700     MOVE OBKR-KVSLATT         TO ORAD-KVSLATT                            
258800     MOVE OBKR-PRARTNTO        TO ORAD-PRARTNTO                           
258900     MOVE OBKR-DEAL-PR-LINE    TO ORAD-DEAL-PR-LINE                       
259000***VID T.EX ERSATTA BIPACKNINGSRADER, TPO6'OR ETC.                        
259100*** DE HAR INGEN PRISFRÅGA ÄNNU.                                          
259200     IF DIST79-DEALER-PRICE AND                                           
259300        (OBKR-PRARTNTO-LOC = +0 AND OBKR-PRARTNTO-LOCPREL = +0)           
259400        PERFORM S26-ADD-PRICE-Q-LINE                                      
259500     END-IF                                                               
259600     MOVE OBKR-PRBPRIS         TO ORAD-PRBPRIS                            
259700     MOVE AREG-REKSIFFR        TO ORAD-REKSIFFR                           
259800     MOVE OBKR-RERF-RAD        TO ORAD-RERF-RAD                           
259900     MOVE OBKR-TIPRIS          TO ORAD-TIPRIS                             
260000     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
260100     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
260200     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
260300     MOVE +0                   TO ORAD-TIRODAT                            
260400     MOVE OBKR-TITPO           TO ORAD-TITPO                              
260500                                                                          
260600     MOVE OBKR-IDKUNDRF-WIP    TO ORAD-IDKUNDRF-WIP                       
260700     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
260800     MOVE OBKR-KDVALISO        TO ORAD-KDVALISO                           
260900                                                                          
261000     .                                                                    
261100     EJECT                                                                
261200                                                                          
261300 S13-HITTA-FORSTA-I-GRUPPEN SECTION.                                      
261400                                                                          
261500     MOVE WS-AKT-IDARTNR       TO WS-IDARTNR-SPAR                         
261600     MOVE WS-AKT-IDDC          TO WS-IDDC-SPAR                            
261700     MOVE WS-AKT-IDLOPNR       TO WS-IDLOPNR-SPAR                         
261800     MOVE WS-AKT-IDARTNR-URS   TO WS-IDARTNR-URS-SPAR                     
261900                                                                          
262000     SUBTRACT 1 FROM WS-INDEX-MID                                         
262100     IF WS-INDEX-MID NOT = +0                                             
262200        MOVE MID-RAD(WS-INDEX-MID)                                        
262300                               TO WS-AKTUELL-MID-RAD                      
262400        PERFORM UNTIL WS-INDEX-MID = +0 OR                                
262500                      WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR             
262600                      WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR             
262700                      WS-AKT-IDARTNR-URS NOT =                            
262800                      WS-IDARTNR-URS-SPAR                                 
262900           SUBTRACT 1        FROM WS-INDEX-MID                            
263000           IF WS-INDEX-MID NOT = +0                                       
263100              MOVE MID-RAD(WS-INDEX-MID)                                  
263200                               TO WS-AKTUELL-MID-RAD                      
263300           END-IF                                                         
263400        END-PERFORM                                                       
263500     END-IF                                                               
263600     .                                                                    
263700     EJECT                                                                
263800                                                                          
263900 S14-LAS-ARTIKELREG SECTION.                                              
264000                                                                          
264100     IF OBKR-IDARTNR-TILLK > +0                                           
264200       MOVE OBKR-IDARTNR-TILLK TO AREG-IDARTNR                            
264300     ELSE                                                                 
264400       MOVE OBKR-IDARTNR       TO AREG-IDARTNR                            
264500     END-IF                                                               
264600                                                                          
264700     CALL W411AREG USING AREG-W411AREG                                    
264800                         AREG-WDK6-PCB                                    
264900                         AREG-WDK7-PCB                                    
265000     .                                                                    
265100     EJECT                                                                
265200                                                                          
265300 S22-HAMTA-BENAMNING SECTION.                                             
265400                                                                          
265500     IF OBKR-IDARTNR-TILLK > +0                                           
265600        MOVE OBKR-IDARTNR-TILLK                                           
265700                               TO W-IDARTNR                               
265800     ELSE                                                                 
265900        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
266000     END-IF                                                               
266100                                                                          
266200     MOVE OHUV-IDSKYLT         TO W-IDSKYLT                               
266300                                                                          
266400     PERFORM IMS-23-GU-BENA-WDD311                                        
266500     IF SEGMENT-FINNS                                                     
266600        MOVE TEXT-BEART        TO MOD-BEART(WS-INDEX-MOD)                 
266700     ELSE                                                                 
266800        MOVE SPACE             TO MOD-BEART(WS-INDEX-MOD)                 
266900     END-IF                                                               
267000     .                                                                    
267100     EJECT                                                                
267200 S23-DELETE-PRICE-Q-LINE SECTION.                                         
267300                                                                          
267400     IF DIST79-DEALER-PRICE                                               
267500       IF OBKR-IDPRQUES > ZERO                                            
267600         INITIALIZE PRQU-W335PRQU                                         
267700         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
267800         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
267900         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
268000         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
268100         MOVE 4                  TO PRQU-KDCALL                           
268200         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
268300                                            PRQU-WDC7-PCB                 
268400                                            PRQU-SJKO-WDK6-PCB            
268500       END-IF                                                             
268600     END-IF                                                               
268700     .                                                                    
268800     EJECT                                                                
268900 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
269000                                                                          
269100     IF DIST79-DEALER-PRICE                                               
269200       IF OBKR-IDPRQUES > ZERO                                            
269300         INITIALIZE PRQU-W335PRQU                                         
269400         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
269500         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
269600         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
269700         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
269800         MOVE OBKR-KVBEART-Q     TO PRQU-KVBEART-Q                        
269900         MOVE 5                  TO PRQU-KDCALL                           
270000         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
270100                                            PRQU-WDC7-PCB                 
270200                                            PRQU-SJKO-WDK6-PCB            
270300       END-IF                                                             
270400     END-IF                                                               
270500     .                                                                    
270600     EJECT                                                                
270700 S26-ADD-PRICE-Q-LINE SECTION.                                            
270800                                                                          
270900     IF DIST79-DEALER-PRICE                                               
271000       IF OBKR-PRARTNTO-LOC = +0    AND                                   
271100          OBKR-PRARTNTO-LOCPREL = +0                                      
271200         IF OBKR-IDPRQUES       = +0                                      
271300********* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                 
271400           MOVE +0                    TO PRNO-IDPRQUES-IN                 
271500           MOVE +1                    TO PRNO-KDCALL                      
271600                                                                          
271700           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
271800                                                                          
271900**********UPPDATERAR WDC7 MED EN PRISFRÅGA                                
272000           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
272100           MOVE +1                    TO PRQU-KDCALL                      
272200                                                                          
272300           MOVE W-IDDISTR             TO PRQU-IDDISTR                     
272400           MOVE W-IDKUNDNR            TO PRQU-IDKUNDNR                    
272500           MOVE W-IDKUNDRF            TO PRQU-IDKUNDRF                    
272600           MOVE OBKR-IDORDER          TO PRQU-IDORDER                     
272700           MOVE OBKR-KDORDKL          TO PRQU-KDORDKL                     
272800           MOVE 'N'                   TO PRQU-KDPRSTA                     
272900           MOVE OBKR-IDARTNR          TO PRQU-IDARTNR                     
273000           MOVE OBKR-KVBEART-Q        TO PRQU-KVBEART-Q                   
273100           MOVE OBKR-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                
273200           MOVE +0                    TO PRQU-PRARTNTO-LOCPREL            
273300           MOVE OBKR-IDSYSTEM         TO PRQU-IDSYSTEM                    
273400           MOVE OBKR-KDVALISO         TO PRQU-KDVALISO                    
273500                                                                          
273600                                                                          
273700           CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                
273800                                           PRQU-WDC7-PCB                  
273900                                           PRQU-SJKO-WDK6-PCB             
274000                                                                          
274100           MOVE PRQU-IDPRQUES           TO  ORAD-IDPRQUES                 
274200           MOVE PRQU-FLPRTILL           TO  ORAD-FLPRTILL                 
274300                                                                          
274400           IF OBKR-PRARTNTO-LOC = +0                                      
274500             MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL         
274600           ELSE                                                           
274700             MOVE OBKR-PRARTNTO-LOC     TO  ORAD-PRARTNTO-LOC             
274800           END-IF                                                         
274900                                                                          
275000*** UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                    
275100                                                                          
275200           MOVE PRQU-IDPRQUES           TO PRNO-IDPRQUES-IN               
275300           MOVE +3                      TO PRNO-KDCALL                    
275400                                                                          
275500           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
275600***SKICKA PRISFRÅGA                                                       
275700           PERFORM S26C-SKICKA-PRISFRAGA                                  
275800         END-IF                                                           
275900       END-IF                                                             
276000     END-IF                                                               
276100     .                                                                    
276200 S26C-SKICKA-PRISFRAGA SECTION.                                           
276300                                                                          
276400     MOVE 1                      TO 3039-REQU-IDMSGVER                    
276500     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
276600     MOVE 'W4020300'             TO 3039-REQU-IDUSER                      
276700                                                                          
276800     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
276900     MOVE OBKR-IDDISTR           TO 3039-MID-IDDISTR                      
277000     MOVE OBKR-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
277100     MOVE OBKR-IDORDNR7          TO 3039-MID-IDBUNDLE                     
277200     MOVE PRQU-IDPRQUES          TO 3039-MID-IDPRQUES                     
277300                                                                          
277400     PERFORM S27-SKICKA-OPEN                                              
277500     PERFORM S27-SKICKA-MEDDELANDE                                        
277600     PERFORM S27-SKICKA-CLOSE                                             
277700     .                                                                    
277800                                                                          
277900 S27-SKICKA-OPEN SECTION.                                                 
278000                                                                          
278100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
278200     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
278300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
278400                                                                          
278500     IF SEND-KDRC > 0                                                     
278600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
278700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
278800       DELIMITED BY SIZE INTO FELTEXT                                     
278900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
279000     END-IF                                                               
279100     .                                                                    
279200     SKIP3                                                                
279300 S27-SKICKA-MEDDELANDE SECTION.                                           
279400                                                                          
279500     MOVE 'PUT'                      TO SEND-KDFUNC                       
279600     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
279700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
279800                                                                          
279900     IF SEND-KDRC > 0                                                     
280000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
280100       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
280200       DELIMITED BY SIZE INTO FELTEXT                                     
280300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
280400     END-IF                                                               
280500     .                                                                    
280600     SKIP3                                                                
280700 S27-SKICKA-CLOSE SECTION.                                                
280800                                                                          
280900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
281000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
281100                                                                          
281200     IF SEND-KDRC > 0                                                     
281300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
281400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
281500       DELIMITED BY SIZE INTO FELTEXT                                     
281600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
281700     END-IF                                                               
281800     .                                                                    
281900     EJECT                                                                
282000                                                                          
282100 MFS-ROER-EJ-BILD SECTION.                                                
282200                                                                          
282300     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDORDKL-UT                          
282400                                                                          
282500     MOVE +1                   TO WS-INDEX                                
282600     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
282700                                                                          
282800        PERFORM MFS-SAETT-ATTRIBUT                                        
282900                                                                          
283000        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
283100                                  MOD-ASTERIX(WS-INDEX)                   
283200                                  MOD-KDBEHX(WS-INDEX)                    
283300                                  MOD-IDARTNR(WS-INDEX)                   
283400                                  MOD-BEART(WS-INDEX)                     
283500                                  MOD-IDDC-RAD(WS-INDEX)                  
283600                                  MOD-KVANTAL(WS-INDEX)                   
283700                                  MOD-KVQPACK(WS-INDEX)                   
283800                                  MOD-IDKUNDRF-RO(WS-INDEX)               
283900                                  MOD-KEYS(WS-INDEX)                      
284000        ADD +1                 TO WS-INDEX                                
284100     END-PERFORM                                                          
284200     .                                                                    
284300     EJECT                                                                
284400 MFS-SAETT-ATTRIBUT SECTION.                                              
284500                                                                          
284600     MOVE MID-RAD(WS-INDEX) TO WS-AKTUELL-MID-RAD                         
284700                                                                          
284800     IF WS-AKT-KDBEHX = 'B'                                               
284900       IF (WS-AKT-KDORDBEK = 41 OR 61) AND                                
285000                    (WS-AKT-IDARTNR NOT = WS-AKT-IDARTNR-URS)             
285100         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
285200                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
285300       ELSE                                                               
285400         IF WS-AKT-KDORDBEK      = 21 OR 51 OR                            
285500                                   41 OR 52 OR 53 OR 54 OR                
285600                                   55 OR 57 OR 58 OR 59 OR 66 OR          
285700                                   67 OR 72 OR 73 OR 74 OR 75 OR          
285800                                   76 OR 80 OR 81 OR 82 OR 85 OR          
285900                                   61                                     
286000           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX)             
286100         END-IF                                                           
286200                                                                          
286300         IF WS-AKT-KDORDBEK = 41 OR 61                                    
286400           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
286500                             TO MOD-IDDC-ATTR(WS-INDEX)                   
286600                                MOD-KVANTAL-ATTR(WS-INDEX)                
286700                                MOD-KVQPACK-ATTR(WS-INDEX)                
286800                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
286900         END-IF                                                           
287000         IF WS-AKT-KDORDBEK = 57 AND  WS-AKT-IDARTNR = ZERO               
287100            MOVE MFS-STAENG-FAELT-OSYNLIGT                                
287200                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
287300                                MOD-IDDC-ATTR(WS-INDEX)                   
287400                                MOD-KVANTAL-ATTR(WS-INDEX)                
287500                                MOD-KVQPACK-ATTR(WS-INDEX)                
287600                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
287700         END-IF                                                           
287800       END-IF                                                             
287900     EJECT                                                                
288000     ELSE                                                                 
288100       IF WS-AKT-KDORDBEK = 41 OR 61                                      
288200         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
288300                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
288400         IF (WS-AKT-KDORDBEK = 61) AND                                    
288500                  WS-AKT-IDARTNR = ZERO                                   
288600           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
288700                           TO MOD-KDBEHX-ATTR(WS-INDEX)                   
288800                              MOD-IDDC-ATTR(WS-INDEX)                     
288900                              MOD-KVANTAL-ATTR(WS-INDEX)                  
289000                              MOD-KVQPACK-ATTR(WS-INDEX)                  
289100                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX)              
289200         END-IF                                                           
289300       END-IF                                                             
289400     END-IF                                                               
289500     .                                                                    
289600     EJECT                                                                
289700                                                                          
289800 MFS-RENSA-MOD-RADER SECTION.                                             
289900                                                                          
290000     MOVE +1                 TO WS-INDEX                                  
290100     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
290200        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
290300                                MOD-ASTERIX(WS-INDEX)                     
290400                                MOD-KDBEHX(WS-INDEX)                      
290500                                MOD-IDARTNR(WS-INDEX)                     
290600                                MOD-BEART(WS-INDEX)                       
290700                                MOD-IDDC-RAD(WS-INDEX)                    
290800                                MOD-KVANTAL(WS-INDEX)                     
290900                                MOD-KVQPACK(WS-INDEX)                     
291000                                MOD-IDKUNDRF-RO(WS-INDEX)                 
291100                                MOD-KEYS(WS-INDEX)                        
291200        ADD 1                TO WS-INDEX                                  
291300     END-PERFORM                                                          
291400     .                                                                    
291500                                                                          
291600 MFS-RENSA-ALLA-FAELT SECTION.                                            
291700                                                                          
291800     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
291900                                MOD-IDLOPNR-NEXT                          
292000                                MOD-IDSEKVNR-NEXT                         
292100                                MOD-IDDC-NEXT                             
292200                                MOD-KDORDBEK-NEXT                         
292300                                                                          
292400     PERFORM MFS-RENSA-MOD-RADER                                          
292500     .                                                                    
292600     EJECT                                                                
292700                                                                          
292800 IMS-GET-MSG SECTION.                                                     
292900                                                                          
293000     MOVE '  QC' TO GODK-STATUSKODER                                      
293100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
293200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
293300     PERFORM IMS-STATUSKONTROLL                                           
293400     .                                                                    
293500     SKIP3                                                                
293600 IMS-INSERT-MSG SECTION.                                                  
293700                                                                          
293800     IF NOT ENGLISH-TEXT                                                  
293900       MOVE '0' TO MFS-KDHUVOMR                                           
294000     END-IF                                                               
294100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
294200     MOVE SPACE TO GODK-STATUSKODER                                       
294300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
294400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
294500     PERFORM IMS-STATUSKONTROLL                                           
294600     .                                                                    
294700     SKIP3                                                                
294800 IMS-INSERT-4202-MSG SECTION.                                             
294900                                                                          
295000     IF NOT ENGLISH-TEXT                                                  
295100       MOVE '0' TO MFS-KDHUVOMR                                           
295200     END-IF                                                               
295300     MOVE LOW-VALUE TO 4202-Z1 4202-Z2                                    
295400     MOVE SPACE TO GODK-STATUSKODER                                       
295500     CALL CBLTDLI USING ISRT 4202-PCB 4202-MSG-IO-AREA                    
295600     MOVE 4202-STATUS-CODE TO STATUS-WS                                   
295700     PERFORM IMS-STATUSKONTROLL                                           
295800     .                                                                    
295900     EJECT                                                                
296000 IMS-INSERT-4203-MSG SECTION.                                             
296100                                                                          
296200     IF NOT ENGLISH-TEXT                                                  
296300       MOVE '0' TO MFS-KDHUVOMR                                           
296400     END-IF                                                               
296500     MOVE LOW-VALUE TO 4203-Z1 4203-Z2                                    
296600     MOVE SPACE TO GODK-STATUSKODER                                       
296700     CALL CBLTDLI USING ISRT 4203-PCB 4203-MSG-IO-AREA                    
296800     MOVE 4203-STATUS-CODE TO STATUS-WS                                   
296900     PERFORM IMS-STATUSKONTROLL                                           
297000     .                                                                    
297100     SKIP2                                                                
297200 IMS-INSERT-4244-MSG SECTION.                                             
297300                                                                          
297400     IF NOT ENGLISH-TEXT                                                  
297500       MOVE '0' TO MFS-KDHUVOMR                                           
297600     END-IF                                                               
297700     MOVE LOW-VALUE TO 4244-Z1 4244-Z2                                    
297800     MOVE SPACE TO GODK-STATUSKODER                                       
297900     CALL CBLTDLI USING ISRT 4244-PCB 4244-MSG-IO-AREA                    
298000     MOVE 4244-STATUS-CODE TO STATUS-WS                                   
298100     PERFORM IMS-STATUSKONTROLL                                           
298200     .                                                                    
298300     EJECT                                                                
298400                                                                          
298500 IMS-INSERT-4298-MSG SECTION.                                             
298600                                                                          
298700     IF NOT ENGLISH-TEXT                                                  
298800       MOVE '0' TO MFS-KDHUVOMR                                           
298900     END-IF                                                               
299000     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
299100     MOVE SPACE TO GODK-STATUSKODER                                       
299200     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
299300     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
299400     PERFORM IMS-STATUSKONTROLL                                           
299500     .                                                                    
299600     SKIP2                                                                
299700                                                                          
299800 IMS-PURG-MSG-2191  SECTION.                                              
299900     MOVE SPACE TO GODK-STATUSKODER                                       
300000     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
300100     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
300200     PERFORM IMS-STATUSKONTROLL                                           
300300     .                                                                    
300400     EJECT                                                                
300500                                                                          
300600 IMS-01-GHU-ORQM-WDQ101-FOERE SECTION.                                    
300700                                                                          
300800     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
300900                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
301000                    '&FLOBOK   =' NEJ ')'                                 
301100          DELIMITED BY SIZE INTO SSA1                                     
301200     MOVE '  GE'               TO GODK-STATUSKODER                        
301300     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
301400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
301500     PERFORM IMS-STATUSKONTROLL                                           
301600     .                                                                    
301700     SKIP2                                                                
301800 IMS-02-GHN-ORQM-WDQ101-FOERE SECTION.                                    
301900                                                                          
302000     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
302100                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
302200                    '&FLOBOK   =' NEJ ')'                                 
302300          DELIMITED BY SIZE INTO SSA1                                     
302400     MOVE '  GEGB'             TO GODK-STATUSKODER                        
302500     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
302600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
302700     PERFORM IMS-STATUSKONTROLL                                           
302800     .                                                                    
302900     EJECT                                                                
303000 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
303100                                                                          
303200     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
303300                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
303400                    '&FLOBOK   =' NEJ ')'                                 
303500          DELIMITED BY SIZE INTO SSA1                                     
303600     MOVE '  GE'               TO GODK-STATUSKODER                        
303700     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
303800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
303900     PERFORM IMS-STATUSKONTROLL                                           
304000     .                                                                    
304100     SKIP2                                                                
304200 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
304300                                                                          
304400     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
304500                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
304600                    '&FLOBOK   =' NEJ ')'                                 
304700          DELIMITED BY SIZE INTO SSA1                                     
304800     MOVE '  GEGB'             TO GODK-STATUSKODER                        
304900     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
305000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
305100     PERFORM IMS-STATUSKONTROLL                                           
305200     .                                                                    
305300     SKIP2                                                                
305400 IMS-05-GHU-ORQM-WDQ101-UNIK SECTION.                                     
305500                                                                          
305600     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
305700                    '&FLOBOK   =' NEJ ')'                                 
305800          DELIMITED BY SIZE  INTO SSA1                                    
305900     MOVE '  GE'               TO GODK-STATUSKODER                        
306000     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
306100     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
306200     PERFORM IMS-STATUSKONTROLL                                           
306300     .                                                                    
306400     EJECT                                                                
306500                                                                          
306600 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
306700                                                                          
306800     MOVE '    '               TO GODK-STATUSKODER                        
306900     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
307000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
307100     PERFORM IMS-STATUSKONTROLL                                           
307200     .                                                                    
307300     SKIP2                                                                
307400                                                                          
307500 IMS-07-ISRT-ORQM-WDQ101 SECTION.                                         
307600                                                                          
307700     MOVE 'WLORQM01 '          TO SSA1                                    
307800     MOVE '  '                 TO GODK-STATUSKODER                        
307900     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-ORQM SSA1               
308000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
308100     PERFORM IMS-STATUSKONTROLL                                           
308200     .                                                                    
308300     SKIP2                                                                
308400 IMS-08-DLET-ORQM-WDQ101 SECTION.                                         
308500                                                                          
308600     MOVE '    '               TO GODK-STATUSKODER                        
308700     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-ORQM                    
308800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
308900     PERFORM IMS-STATUSKONTROLL                                           
309000     .                                                                    
309100     EJECT                                                                
309200                                                                          
309300 IMS-09-GU-ORQI-WDQ201 SECTION.                                           
309400                                                                          
309500     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
309600          DELIMITED BY SIZE INTO SSA1                                     
309700     MOVE '  GE'               TO GODK-STATUSKODER                        
309800     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
309900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
310000     PERFORM IMS-STATUSKONTROLL                                           
310100     .                                                                    
310200     SKIP3                                                                
310300 IMS-11-GU-WDK611 SECTION.                                                
310400                                                                          
310500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
310600          DELIMITED BY SIZE INTO    SSA1                                  
310700     MOVE 'WDK611 '           TO    SSA2                                  
310800     MOVE '  '                TO    GODK-STATUSKODER                      
310900     CALL CBLTDLI             USING GU                 WDK6-PCB           
311000                                    DLI-IO-AREA-WDK611                    
311100                                    SSA1               SSA2               
311200     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
311300     PERFORM IMS-STATUSKONTROLL                                           
311400     .                                                                    
311500     EJECT                                                                
311600 IMS-12-GNP-ORQI-WDQ212 SECTION.                                          
311700                                                                          
311800     MOVE 'WLORQI12  '         TO SSA1                                    
311900     MOVE '  GE'               TO GODK-STATUSKODER                        
312000     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1              
312100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
312200     PERFORM IMS-STATUSKONTROLL                                           
312300     .                                                                    
312400     EJECT                                                                
312500 IMS-13-GHU-WDK711 SECTION.                                               
312600                                                                          
312700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
312800          DELIMITED BY SIZE  INTO SSA1                                    
312900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
313000          DELIMITED BY SIZE  INTO SSA2                                    
313100     MOVE '    '               TO GODK-STATUSKODER                        
313200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
313300     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
313400     PERFORM IMS-STATUSKONTROLL                                           
313500     .                                                                    
313600     SKIP3                                                                
313700                                                                          
313800 IMS-GU-WDK712           SECTION.                                         
313900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
314000            DELIMITED BY SIZE INTO SSA1                                   
314100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
314200            DELIMITED BY SIZE INTO SSA2                                   
314300     MOVE '  GE' TO GODK-STATUSKODER                                      
314400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
314500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
314600     PERFORM IMS-STATUSKONTROLL                                           
314700     .                                                                    
314800 IMS-GU-WDK722 SECTION.                                                   
314900                                                                          
315000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
315100          DELIMITED BY SIZE  INTO SSA1                                    
315200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
315300          DELIMITED BY SIZE  INTO SSA2                                    
315400     MOVE 'WDK722 '            TO SSA3                                    
315500     MOVE '  GE'               TO GODK-STATUSKODER                        
315600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
315700     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
315800     PERFORM IMS-STATUSKONTROLL                                           
315900     .                                                                    
316000     SKIP3                                                                
316100 IMS-14-REPL-WDK711 SECTION.                                              
316200                                                                          
316300     MOVE '    '               TO GODK-STATUSKODER                        
316400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
316500     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
316600     PERFORM IMS-STATUSKONTROLL                                           
316700     .                                                                    
316800     SKIP2                                                                
316900 IMS-17-ISRT-ORQF-WDQ401 SECTION.                                         
317000                                                                          
317100     MOVE 'WDQ401   '          TO SSA1                                    
317200     MOVE '  II'               TO GODK-STATUSKODER                        
317300     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORQF SSA1               
317400     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
317500     PERFORM IMS-STATUSKONTROLL                                           
317600     .                                                                    
317700     EJECT                                                                
317800                                                                          
317900 IMS-18A-GU-SATB-WDJ111-01 SECTION.                                       
318000                                                                          
318100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
318200          DELIMITED BY SIZE INTO SSA1                                     
318300     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
318400          DELIMITED BY SIZE INTO SSA2                                     
318500     MOVE '  GE'               TO GODK-STATUSKODER                        
318600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
318700     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
318800     PERFORM IMS-STATUSKONTROLL                                           
318900     .                                                                    
319000     SKIP2                                                                
319100                                                                          
319200 IMS-18-GN-SATB-WDJ111-01 SECTION.                                        
319300                                                                          
319400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
319500          DELIMITED BY SIZE INTO SSA1                                     
319600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
319700          DELIMITED BY SIZE INTO SSA2                                     
319800     MOVE '  GE'               TO GODK-STATUSKODER                        
319900     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
320000     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
320100     PERFORM IMS-STATUSKONTROLL                                           
320200     .                                                                    
320300     SKIP2                                                                
320400                                                                          
320500 IMS-19-ISRT-4541-WDR411 SECTION.                                         
320600                                                                          
320700     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
320800          DELIMITED BY SIZE INTO SSA1                                     
320900     MOVE 'WDGX4542 '          TO SSA2                                    
321000     MOVE '  II'               TO GODK-STATUSKODER                        
321100     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
321200     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
321300     PERFORM IMS-STATUSKONTROLL                                           
321400     .                                                                    
321500     EJECT                                                                
321600 IMS-21-GHU-ARTM-WDK901 SECTION.                                          
321700                                                                          
321800     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
321900          DELIMITED BY SIZE  INTO SSA1                                    
322000     MOVE '    '               TO GODK-STATUSKODER                        
322100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
322200     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
322300     PERFORM IMS-STATUSKONTROLL                                           
322400     .                                                                    
322500     SKIP3                                                                
322600 IMS-22-REPL-ARTM-WDK901 SECTION.                                         
322700                                                                          
322800     MOVE '    '               TO GODK-STATUSKODER                        
322900     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
323000     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
323100     PERFORM IMS-STATUSKONTROLL                                           
323200     .                                                                    
323300     SKIP2                                                                
323400 IMS-23-GU-BENA-WDD311 SECTION.                                           
323500                                                                          
323600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
323700          DELIMITED BY SIZE INTO SSA1                                     
323800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
323900          DELIMITED BY SIZE INTO SSA2                                     
324000     MOVE '  GE'               TO GODK-STATUSKODER                        
324100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
324200     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
324300     PERFORM IMS-STATUSKONTROLL                                           
324400     .                                                                    
324500     EJECT                                                                
324600 IMS-34-GN-ORQM-WDQ101 SECTION.                                           
324700                                                                          
324800     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
324900                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
325000          DELIMITED BY SIZE INTO SSA1                                     
325100     MOVE '  GEGB'             TO GODK-STATUSKODER                        
325200     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
325300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
325400     PERFORM IMS-STATUSKONTROLL                                           
325500     .                                                                    
325600     EJECT                                                                
325700 IMS-36-GHU-ORQM-WDQ101 SECTION.                                          
325800                                                                          
325900     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
326000                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
326100          DELIMITED BY SIZE INTO SSA1                                     
326200     MOVE '  '                 TO GODK-STATUSKODER                        
326300     CALL CBLTDLI USING GHU  ORQM-PCB DLI-IO-AREA-ORQM SSA1               
326400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
326500     PERFORM IMS-STATUSKONTROLL                                           
326600     .                                                                    
326700 IMS-GU-WDB601    SECTION.                                                
326800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
326900          DELIMITED BY SIZE INTO SSA1                                     
327000     MOVE '  '   TO GODK-STATUSKODER                                      
327100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
327200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
327300     PERFORM IMS-STATUSKONTROLL                                           
327400     .                                                                    
327500                                                                          
327600 IMS-STATUSKONTROLL SECTION.                                              
327700                                                                          
327800     SET STATUS-IX TO 1                                                   
327900     SEARCH GODK-STATUS                                                   
328000       AT END CALL FELLOG                                                 
328100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
328200     END-SEARCH                                                           
328300     .                                                                    
328400     EJECT                                                                
328500*    -COPY WY2000Q1                                                       
