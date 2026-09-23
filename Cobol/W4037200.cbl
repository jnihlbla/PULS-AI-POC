000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4037200.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   90/12/04.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        LÄSER ANGIVEN SATSORDER (WLSATG01), KONTROLLERAR ATT             
001100*        STATUS = 2 ELLER 5.                                              
001200*        OM STATUS = 2 KOMMER MAN FRÅN UTTAG AV PLOCKSATS.                
001300*        OM STATUS = 5 KOMMER MAN FRÅN AVVIKELSERAPPORTERINGEN.           
001400*        I BÅDA FALLEN SKALL ETIKETTER & PLOCKLISTA UT.                   
001500*                                                                         
001600*        ENDAST RADER SOM HAR KDSATLI = 1 SKALL SKRIVAS.                  
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T371X                                             
002000*                                                                         
002200*    UTDATA.                                                              
002300*        PLOCKETIKETTER                                                   
002400*        PACKUNDERLAG                                                     
002500*        TRANSAKTION: W4T373X (ÖVERFLYTTNING TIL WDE4, WDE6)              
002600*        TRANSAKTION: W4T374X (INLÄGGNINGSLISTA)                          
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W4037200'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
004100 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
004200 77  FILLER                      PIC X(08)   VALUE 'IMS-POS:'.            
004300 77  WS-CURRENT-IMS-SECTION      PIC X(32)   VALUE SPACE.                 
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  IDDC-SATS                   PIC X(02)   VALUE '11'.                  
004800                                                                          
004900 77  IDPURAD                     PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  PU-RAD-IX                   PIC S9(9)  VALUE +99   COMP SYNC.        
005100 77  LASER-RAD-IX                PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  SID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  ETIKETT-IX                  PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  MAX-ETIKETT                 PIC S9(9)  VALUE +6    COMP SYNC.        
005500 77  WS-IDAFPRCD                 PIC X(10)  VALUE SPACE.                  
005600                                                                          
005800 01  WS-ADPLATS-ORD              PIC X(5).                                
005900 01  FILLER  REDEFINES WS-ADPLATS-ORD.                                    
006000     03 WS-ADPLATSNR             PIC X(3).                                
006100     03 WS-ADPLNIV               PIC X(2).                                
006200                                                                          
006300 01      WS-KLOCKAN.                                                      
006400   03    WS-TIHHMMSS             PIC 9(6).                                
006500   03    FILLER                  PIC X(2).                                
006600*                                                                         
006700 01  IX-TAB                     PIC 9(9).                                 
006800 01  FILLER                     PIC X(08)   VALUE 'IDAFPRCD'.             
006900 01  WS-IDAFPRCD-TAB.                                                     
007000     05 WS-INGANG  OCCURS 30.                                             
007100        10 WS-TAB-IDAFPRCD      PIC X(3).                                 
007200        10 WS-TAB-RADSKIP       PIC 9(3).                                 
007300                                                                          
007400 77  SPAR-ADLAGOMR               PIC S9(3)  VALUE +0    COMP-3.           
007500                                                                          
007600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007700     88  ALLT-OK                             VALUE 'J'.                   
007800     88  ALLT-FEL                            VALUE 'N'.                   
007900                                                                          
008000 77  PLOCK-SW                    PIC X       VALUE 'N'.                   
008100     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
008200                                                                          
008300 77  PAGE-SW                     PIC X       VALUE 'N'.                   
008400     88  SKIP-PAGE                           VALUE 'Y'.                   
008500                                                                          
008600 77  PRINT-SW                    PIC X       VALUE 'N'.                   
008700     88  PRINT-LINE                          VALUE 'Y'.                   
008800                                                                          
008900 77  SECOND-COPY-LIST-SW         PIC X       VALUE 'N'.                   
009000     88  SECOND-COPY-LIST                    VALUE 'J'.                   
009100                                                                          
009200 77  PLE-SKRIVARTYP-SW               PIC X.                               
009300     88  PLE-LASER-SKRIVARE                      VALUE 'J'.               
009400     88  PLE-MATRIS-SKRIVARE                     VALUE 'N'.               
009500                                                                          
009600 77  PU-SKRIVARTYP-SW               PIC X.                                
009700     88  PU-LASER-SKRIVARE                      VALUE 'J'.                
009800     88  PU-MATRIS-SKRIVARE                     VALUE 'N'.                
009900                                                                          
010000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010100     88 GODK-MID                             VALUE '4304' '4371'.         
010200                                                                          
010300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010400 01  GENERELLA-SUBPROGRAM.                                                
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010700     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
010800     03  W006PRR2                PIC X(8)    VALUE 'W006PRR2'.            
010900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011100*    --- PARAMETERS TO ABEND                                              
011200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011500                                                                          
011600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011700     EJECT                                                                
011800     EJECT                                                                
011900*    --- AREA FÖR SUBPROGRAM W006PRR1 W006PRR2                            
012000*                                                                         
012100 01  FILLER                      PIC X(08)  VALUE 'PRINTPGM'.             
012200                                                                          
012300*01  -COPY W006PRAR                                                       
012400                                                                          
012500 01  FILLER                      PIC X(08)  VALUE 'W006PRT '.             
012600*   -COPY W006PRT                                                         
012700*                                                                         
012800     SKIP3                                                                
012900 01  FILLER                      PIC X(08)  VALUE 'PLEAREA '.             
013000 01  WS-PLE-AREA.                                                         
013100     03 WS-IDPRTLST-PLE.                                                  
013200        05 WS-SYSTDEL-PLE        PIC X(1).                                
013300        05 WS-LISTTYP-PLE        PIC X(2).                                
013400        05 WS-KDPRT-PLE          PIC X(3).                                
013500        05 FILLER-PLE            PIC X(2)    VALUE SPACE.                 
013600     03 WS-PLE-LISTID.                                                    
013700        05 FILLER                PIC X(5)  VALUE 'SATS'.                  
013800        05 WS-PLE-IDORDNSB       PIC 9(4).                                
013900        05 WS-PLE-IDORDNSS       PIC 9(1).                                
014000     03 WS-PLE-LISTRAD.                                                   
014100        05 WS-PLE-RAD            PIC X(132).                              
014200     03 WS-PLE-DUMMY             PIC X(1).                                
014300                                                                          
014400 01  FILLER                      PIC X(08)  VALUE 'ARB-RAD:'.             
014500 01  ARB-RAD-AREA.                                                        
014600     03 ARB-RAD        OCCURS 2.                                          
014700        05 ARB-RAD-V             PIC X(50).                               
014800        05 FILLER                PIC X(03)  VALUE SPACE.                  
014900        05 ARB-RAD-H             PIC X(50).                               
015000        05 FILLER                PIC X(29)  VALUE SPACE.                  
015100                                                                          
015200 01  FILLER                      PIC X(08)  VALUE 'PU-AREA '.             
015300 01  WS-PU-AREA.                                                          
015400     03 WS-IDPRTLST-PU.                                                   
015500        05 WS-SYSTDEL-PU         PIC X(1).                                
015600        05 WS-LISTTYP-PU         PIC X(2).                                
015700        05 WS-KDPRT-PU           PIC X(3).                                
015800        05 FILLER-PU             PIC X(2)    VALUE SPACE.                 
015900     03 WS-PU-LISTID.                                                     
016000        05 FILLER                PIC X(5)  VALUE 'SATS'.                  
016100        05 WS-PU-IDORDNSB        PIC 9(4).                                
016200        05 WS-PU-IDORDNSS        PIC 9(1).                                
016300     03 WS-PU-LISTRAD.                                                    
016400        05 FILLER                PIC X(2)  VALUE SPACE.                   
016500        05 WS-PU-RAD             PIC X(78).                               
016600     03 WS-PU-DUMMY              PIC X(1).                                
016700                                                                          
016800 01  WS-MSG-AREA.                                                         
016900     03 WS-IDORDNSB              PIC X(5).                                
017000     03 WS-IDORDNSS              PIC X(1).                                
017100     EJECT                                                                
017200                                                                          
017300*    --- AREOR FÖR LASERBLANKETT                                          
017400*                                                                         
017500 01  FILLER                      PIC X(16)  VALUE 'W4037601'.             
017600                                                                          
017700*01  -COPY W4037601                                                       
017800     EJECT                                                                
017900*                                                                         
018000 01  FILLER                      PIC X(16)  VALUE 'W4037602'.             
018100                                                                          
018200*01  -COPY W4037602                                                       
018300     EJECT                                                                
018400*    --- AREOR FÖR MSG-HANTERING                                          
018500*                                                                         
018600 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
018700                                                                          
018800*01  -COPY WMSGAREA                                                       
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
019100 01  P-TO-P-SW1.                                                          
019200     03  PTOP1-LL                PIC S9(4)   VALUE 23 COMP SYNC.          
019300     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
019400     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
019500     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T373X'.             
019600     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
019700     03  FILLER                  PIC  X(4)   VALUE '4372'.                
019800     03  PTOP1-KDMFSFOR          PIC  X(1).                               
019900     03  PTOP1-IDORDNSB          PIC  X(5).                               
020000     03  PTOP1-IDORDNSS          PIC  X(1).                               
020100                                                                          
020200 01  P-TO-P-SW2.                                                          
020300     03  PTOP2-LL                PIC S9(4)   VALUE 23 COMP SYNC.          
020400     03  PTOP2-Z1                PIC  X(1)   VALUE LOW-VALUE.             
020500     03  PTOP2-Z2                PIC  X(1)   VALUE LOW-VALUE.             
020600     03  PTOP2-TRANSKOD          PIC  X(7)   VALUE 'W4T374X'.             
020700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
020800     03  FILLER                  PIC  X(4)   VALUE '4372'.                
020900     03  PTOP2-KDMFSFOR          PIC  X(1).                               
021000     03  PTOP2-IDORDNSB          PIC  X(5).                               
021100     03  PTOP2-IDORDNSS          PIC  X(1).                               
021200     EJECT                                                                
021300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021400*                                                                         
021500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021600                                                                          
021700 01  NYCKLAR-TILL-DLI.                                                    
021800                                                                          
021900*----> ARTIKELREG.                                                        
022000                                                                          
022100     03  W-IDARTNR-X.                                                     
022200         05 W-IDARTNR                     PIC S9(9) COMP-3.               
022300                                                                          
022400     03  W-KDSEGKEY                       PIC X(1)  VALUE '1'.            
022500                                                                          
022600*----> SATSORDERKÖN.                                                      
022700                                                                          
022800     03  W-WDJ2-IDORDNST-X.                                               
022900         05 W-WDJ2-IDORDNSB               PIC S9(5) COMP-3.               
023000         05 W-WDJ2-IDORDNSS               PIC S9(1) COMP-3.               
023100                                                                          
023200     03 W-WDJ2-KDSATLI-X                  PIC X(1)  VALUE '1'.            
023300                                                                          
023400*----> BENÄMNINGSREGISTER WDD3.                                           
023500                                                                          
023600     03  W-WDD3BSEQ-X.                                                    
023700         05 W-WDD3BSEQ-IDARTNR            PIC S9(9) COMP-3.               
023800                                                                          
023900     03  W-WDD3-IDSKYLT-X                 PIC X(3).                       
024000                                                                          
024100*----> PRC-PRINTERTABELL WDR1.                                            
024200                                                                          
024300     03  W-WDR1-WDGXKEY-4453-X.                                           
024400         05  W-4453-IDHTYP       PIC  X(04) VALUE '4453'.                 
024500         05  W-4453-IDDC         PIC  X(02) VALUE '11'.                   
024600         05  W-4453-IDPRC        PIC  X(04).                              
024700         05  W-4447-LOW-VALUE    PIC  X(20) VALUE LOW-VALUE.              
024800                                                                          
024900     03  W-WDR1-KDSEGKEY-4454-X  PIC  X(01) VALUE '1'.                    
025000                                                                          
025100*    --- STATUS-KOD FRÅN IMS                                              
025200                                                                          
025300 01  STATUS-WS                   PIC  X(02).                              
025400     88  SEGMENT-FINNS                       VALUE '  '.                  
025500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025600     88  END-OF-DATA                         VALUE 'GB'.                  
025700     SKIP2                                                                
025800 01  GODK-STATUSKODER.                                                    
025900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026000                                                                          
026100 01  SSA1                        PIC X(96).                               
026200 01  SSA2                        PIC X(96).                               
026300     EJECT                                                                
026400*********************************                                         
026500*  PRINTRADER FÖR PLOCKETIKETT  *                                         
026600*********************************                                         
026700 01  PLE-RAD1.                                                            
026800     03   PLE-RAD1-ADLAGOMR       PIC Z9.                                 
026900     03   PLE-RAD1-ADGANG         PIC Z9.                                 
027000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027100     03   PLE-RAD1-ADPLATS        PIC Z(4)9.                              
027200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
027300     03   PLE-RAD1-IDARTNR        PIC Z(7)9.                              
027400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027500     03   PLE-RAD1-BEART          PIC X(15).                              
027600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027700     03   PLE-RAD1-REBEART        PIC Z(6)9.                              
027800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027900     03   PLE-RAD1-KDSORT         PIC X(2).                               
028000     03   PLE-RAD1-KDARTURS       PIC X(2).                               
028100                                                                          
028200 01  PLE-RAD2.                                                            
028300     03   PLE-RAD2-IDDISTR        PIC Z(3)9.                              
028400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028500     03   PLE-RAD2-IDKUNDNR       PIC Z(6).                               
028600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028700     03   PLE-RAD2-IDPRODNR       PIC Z(6)9.                              
028800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
028900     03   PLE-RAD2-IDORDNSB       PIC 9(4).                               
029000     03   PLE-RAD2-IDORDNSS       PIC 9(1).                               
029100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
029200     03   PLE-RAD2-IDRADNR        PIC Z(3)9.                              
029300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029400     03   PLE-RAD2-KDORDKL        PIC 9.                                  
029500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029600     03   PLE-RAD2-IDPRC          PIC X(4).                               
029700     EJECT                                                                
029800***********************************                                       
029900*  PRINTRADER FÖR PACKUNDERLAGET  *                                       
030000***********************************                                       
030100 01  PU-HRAD1.                                                            
030200     03   FILLER                  PIC X(74) VALUE SPACE.                  
030300     03   PU-HRAD1-IDSID          PIC ZZ9.                                
030400                                                                          
030500 01  PU-HRAD2.                                                            
030600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030700     03   PU-HRAD2-IDDISTR        PIC Z(4)9.                              
030800     03   FILLER                  PIC X(06) VALUE SPACE.                  
030900     03   PU-HRAD2-COPY           PIC X(06) VALUE SPACE.                  
031000     03   FILLER                  PIC X(06) VALUE SPACE.                  
031100     03   PU-HRAD2-IDORDNSB       PIC Z(4).                               
031200     03   PU-HRAD2-IDORDNSS       PIC 9(1).                               
031300     03   FILLER                  PIC X(7)  VALUE SPACE.                  
031400     03   PU-HRAD2-KDORDKL        PIC X(1).                               
031500     03   FILLER                  PIC X(9)  VALUE SPACE.                  
031600     03   PU-HRAD2-IDUSER         PIC X(8).                               
031700     03   FILLER                  PIC X(8)  VALUE SPACE.                  
031800     03   PU-HRAD2-IDPRODNR       PIC Z(6)9.                              
031900                                                                          
032000 01  PU-HRAD3.                                                            
032100     03   FILLER                  PIC X(12) VALUE 'SATSART.NR: '.         
032200     03   PU-HRAD3-IDARTNR        PIC Z(8)9.                              
032300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
032400     03   PU-HRAD3-BEART          PIC X(32).                              
032500     03   FILLER                  PIC X(10) VALUE SPACE.                  
032600     03   FILLER                  PIC X(9)  VALUE 'REG.DATUM'.            
032700                                                                          
032800 01  PU-HRAD4.                                                            
032900     03   FILLER                  PIC X(11) VALUE 'PRIORITET: '.          
033000     03   PU-HRAD4-PRIO           PIC X(12).                              
033100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
033200     03   FILLER                  PIC X(7)  VALUE 'ANTAL: '.              
033300     03   PU-HRAD4-KVBYGGB        PIC Z(6)9.                              
033400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
033500     03   FILLER                  PIC X(9)  VALUE 'FÖRPTYP: '.            
033600     03   PU-HRAD4-BEFT           PIC Z(3).                               
033700     03   FILLER                  PIC X(12) VALUE SPACE.                  
033800     03   PU-HRAD4-TIREGDAT       PIC 9(6).                               
033900                                                                          
034000 01  PU-HRAD5.                                                            
034100     03   FILLER                  PIC X(6)  VALUE 'ANSK: '.               
034200     03   PU-HRAD5-IDANSK         PIC Z(3).                               
034300     03   FILLER                  PIC X(8)  VALUE SPACE.                  
034400     03   FILLER                  PIC X(16)                               
034500                                  VALUE 'BEGÄRD PACKAD:  '.               
034600     03   PU-HRAD5-TIBEGPAC       PIC 9(6).                               
034700                                                                          
034800 01  PU-HRAD6.                                                            
034900     03   FILLER                  PIC X(65) VALUE SPACE.                  
035000     03   FILLER                  PIC X(11) VALUE 'UTSKR.DATUM'.          
035100                                                                          
035200 01  PU-HRAD7.                                                            
035300     03   FILLER                  PIC X(10) VALUE 'PACKAD AV:'.           
035400     03   FILLER                  PIC X(10) VALUE SPACE.                  
035500     03   FILLER                  PIC X(14)                               
035600                                  VALUE 'BUFFERT PLATS:'.                 
035700     03   FILLER                  PIC X(10) VALUE SPACE.                  
035800     03   FILLER                  PIC X(13)                               
035900                                  VALUE 'ANTAL PALLAR:'.                  
036000     03   FILLER                  PIC X(8)  VALUE SPACE.                  
036100     03   PU-HRAD7-TIUTSKR        PIC 9(6).                               
036200                                                                          
036300 01  PU-HRAD8.                                                            
036400     03   FILLER                  PIC X(3)  VALUE 'BRI'.                  
036500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
036600     03   FILLER                  PIC X(6)  VALUE 'ADRESS'.               
036700     03   FILLER                  PIC X(7)  VALUE SPACE.                  
036800     03   FILLER                  PIC X(3)  VALUE 'RAD'.                  
036900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
037000     03   FILLER                  PIC X(6)  VALUE 'ART.NR'.               
037100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
037200     03   FILLER                  PIC X(9)  VALUE 'BENÄMNING'.            
037300     03   FILLER                  PIC X(12) VALUE SPACE.                  
037400     03   FILLER                  PIC X(8)  VALUE 'ANT/SATS'.             
037500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
037600     03   FILLER                  PIC X(3)  VALUE 'ENH'.                  
037700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
037800     03   FILLER                  PIC X(4)  VALUE 'BEST'.                 
037900                                                                          
038000 01  PU-RAD.                                                              
038100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
038200     03   PU-RAD-BRIST            PIC X(1).                               
038300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038400     03   PU-RAD-KDSATKMB         PIC X(1).                               
038500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
038600     03   PU-RAD-ADLAGOMR         PIC Z(2)9.                              
038700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
038800     03   PU-RAD-ADGANG           PIC Z(1)9.                              
038900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
039000     03   PU-RAD-ADPLATS          PIC Z(5).                               
039100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
039200     03   PU-RAD-IDPURAD          PIC Z(4).                               
039300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
039400     03   PU-RAD-IDARTNR          PIC Z(7)9.                              
039500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
039600     03   PU-RAD-BEART            PIC X(20).                              
039700     03   FILLER                  PIC X(3)  VALUE SPACE.                  
039800     03   PU-RAD-REANTPSA         PIC Z9.999.                             
039900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
040000     03   PU-RAD-KDSORT           PIC X(2).                               
040100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
040200     03   PU-RAD-REBEART          PIC Z(6)9.                              
040300                                                                          
040400 01  PU-TOTRAD1.                                                          
040500     03   FILLER                  PIC X(11) VALUE 'TOTALVIKT: '.          
040600     03   PU-TOTRAD1-VKORDNTO     PIC Z(5)9.9(1).                         
040700     03   FILLER                  PIC X(6)  VALUE ' KG   '.               
040800     03   FILLER                  PIC X(7)  VALUE 'VOLYM: '.              
040900     03   PU-TOTRAD1-VLORDNTO     PIC Z(3)9.9(3).                         
041000     03   FILLER                  PIC X(5)  VALUE ' M3  '.                
041100     03   FILLER                  PIC X(16)                               
041200                                  VALUE 'ANTAL ARTIKLAR: '.               
041300     03   PU-TOTRAD1-KVRADER      PIC Z(7)9.                              
041400     03   FILLER                  PIC X(3)  VALUE ' ST'.                  
041500     EJECT                                                                
041600********************************************************                  
041700 01  LASER-TOTRAD.                                                        
041800     03   ANTAL-TEXT              PIC X(6) VALUE 'ANTAL'.                 
041900     03   LASER-TOTRAD-LAGOMRADE.                                         
042000       05 FILLER                  OCCURS 8.                               
042100          07 LASER-TOTRAD-ADLAGOMR      PIC ZZB.                          
042200          07 LASER-TOTRAD-ANTAL         PIC ZZZ.                          
042300          07 LASER-TOTRAD-KOMMA         PIC X(2) VALUE SPACE.             
042400**END LASER TOTAL LINES*********************************                  
042500*    --- IMS FUNKTIONSKODER                                               
042600*01  -COPY W0003                                                          
042700     EJECT                                                                
042800*    ---  DLI INPUT-OUTPUT AREA                                           
042900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
043000                                                                          
043100 01  DLI-IO-AREA1.                                                        
043200     03  WLSATG01.                                                        
043300*        05  -COPY WDJ201                                                 
043400     EJECT                                                                
043500 01  DLI-IO-AREA2.                                                        
043600     03  WLSATG12.                                                        
043700*        05  -COPY WDJ212                                                 
043800     EJECT                                                                
043900 01  DLI-IO-AREA3.                                                        
044000     03  WLBENA11.                                                        
044100*        05  -COPY WDD311                                                 
044200     EJECT                                                                
044300 01  DLI-IO-AREA4.                                                        
044400     03  WLXXKL11.                                                        
044500*        05  -COPY WDGX4454                                               
044600     EJECT                                                                
044700 01  DLI-IO-AREA5.                                                        
044800     03  WLARTC11.                                                        
044900*        05  -COPY WDK611                                                 
045000     EJECT                                                                
045100 LINKAGE SECTION.                                                         
045200                                                                          
045300*01  -COPY W0009      -PRE MSG-                                           
045400     EJECT                                                                
045500*01  -COPY W0009      -PRE ALT1-                                          
045600     EJECT                                                                
045700*01  -COPY W0009      -PRE ALT2-                                          
045800     EJECT                                                                
045900*01  -COPY W0009      -PRE ALT3-                                          
046000     EJECT                                                                
046100*01  -COPY W0009      -PRE ALT4-                                          
046200     EJECT                                                                
046300*01  -COPY W0008      -PRE LISBPE-                                        
046400     05  FILLER                  PIC X.                                   
046500     EJECT                                                                
046600*01  -COPY W0008      -PRE LISBA4S-                                       
046700     05  FILLER                  PIC X.                                   
046800     EJECT                                                                
046900*01  -COPY W0008      -PRE SATG-                                          
047000     05  FILLER                  PIC X.                                   
047100     EJECT                                                                
047200*01  -COPY W0008      -PRE BENA-                                          
047300     05  FILLER                  PIC X.                                   
047400     EJECT                                                                
047500*01  -COPY W0008      -PRE XXKL-                                          
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
047800*01  -COPY W0008      -PRE ARTC-                                          
047900     05  FILLER                  PIC X.                                   
048000     EJECT                                                                
048100 PROCEDURE DIVISION  USING MSG-PCB                                        
048200                           ALT1-PCB                                       
048300                           ALT2-PCB                                       
048400                           ALT3-PCB                                       
048500                           ALT4-PCB                                       
048600                           LISBPE-PCB                                     
048700                           LISBA4S-PCB                                    
048800                           SATG-PCB                                       
048900                           BENA-PCB                                       
049000                           XXKL-PCB                                       
049100                           ARTC-PCB.                                      
049200                                                                          
049300     ENTRY 'DLITCBL' USING MSG-PCB                                        
049400                           ALT1-PCB                                       
049500                           ALT2-PCB                                       
049600                           ALT3-PCB                                       
049700                           ALT4-PCB                                       
049800                           LISBPE-PCB                                     
049900                           LISBA4S-PCB                                    
050000                           SATG-PCB                                       
050100                           BENA-PCB                                       
050200                           XXKL-PCB                                       
050300                           ARTC-PCB.                                      
050400                                                                          
050500     PERFORM IMS-GU-MSG                                                   
050600     IF SEGMENT-FINNS                                                     
050700        PERFORM A-INIT                                                    
050800        IF ALLT-OK                                                        
050900           PERFORM B-LAES-SATSORDER                                       
051000           IF ALLT-OK                                                     
051100              PERFORM C-LAES-PRC                                          
051200              IF ALLT-OK                                                  
051300                 PERFORM D-SKRIV-PLE-PU                                   
051400                 PERFORM E-UPPDAT-SATSORDER                               
051500                 PERFORM F-SKICKA-IMSTRANS                                
051600              END-IF                                                      
051700           END-IF                                                         
051800        END-IF                                                            
051900     END-IF                                                               
052000                                                                          
052100     MOVE ZERO TO RETURN-CODE                                             
052200     GOBACK                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 A-INIT SECTION.                                                          
052600                                                                          
052700     IF MSG-KDTRANS-1  NOT = 'W4T372X '                                   
052800        MOVE NEJ TO ALLT-SW                                               
052900     END-IF                                                               
053000                                                                          
053100     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
053200     IF NOT GODK-MID                                                      
053300        MOVE NEJ TO ALLT-SW                                               
053400     END-IF                                                               
053500                                                                          
053600     IF ALLT-OK                                                           
053700        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO WS-MSG-AREA                   
053800        MOVE WS-IDORDNSB                 TO W-WDJ2-IDORDNSB               
053900        MOVE WS-IDORDNSS                 TO W-WDJ2-IDORDNSS               
054000     END-IF                                                               
054100     MOVE SPACE                          TO FILLER-PLE                    
054200                                            FILLER-PU                     
054300     MOVE NEJ TO PLE-SKRIVARTYP-SW                                        
054400     MOVE NEJ TO PU-SKRIVARTYP-SW                                         
054500     MOVE +0                             TO LASER-RAD-IX                  
054600     .                                                                    
054700     EJECT                                                                
054800 B-LAES-SATSORDER SECTION.                                                
054900                                                                          
055000     PERFORM IMS-GU-WDJ2-WLSATG01                                         
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300        AND                                                               
055400       (SHUV-KDSATPLK = 2 OR 5)                                           
055500        MOVE SHUV-IDORDNSB TO WS-PLE-IDORDNSB                             
055600                              WS-PU-IDORDNSB                              
055700        MOVE SHUV-IDORDNSS TO WS-PLE-IDORDNSS                             
055800                              WS-PU-IDORDNSS                              
055900        PERFORM BA-HAMTA-BENAMNING                                        
056000        PERFORM BB-REDIGERA-PUHUVUD                                       
056100     ELSE                                                                 
056200        MOVE NEJ TO ALLT-SW                                               
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 BA-HAMTA-BENAMNING SECTION.                                              
056700                                                                          
056800     MOVE SHUV-IDARTNR TO W-WDD3BSEQ-IDARTNR                              
056900                                                                          
057000     MOVE 'S  ' TO W-WDD3-IDSKYLT-X                                       
057100                                                                          
057200     PERFORM IMS-GU-WDD3-WLBENA11                                         
057300     IF SEGMENT-FINNS                                                     
057400        MOVE TEXT-BEART       TO PU-HRAD3-BEART                           
057500     ELSE                                                                 
057600        MOVE 'BENÄMN. SAKNAS' TO PU-HRAD3-BEART                           
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 BB-REDIGERA-PUHUVUD SECTION.                                             
058100                                                                          
058200     MOVE SHUV-IDDISTR         TO PU-HRAD2-IDDISTR                        
058300     MOVE '*COPY*'             TO PU-HRAD2-COPY                           
058400     MOVE SHUV-IDORDNSB        TO PU-HRAD2-IDORDNSB                       
058500     MOVE SHUV-IDORDNSS        TO PU-HRAD2-IDORDNSS                       
058600     MOVE SHUV-KDORDKL         TO PU-HRAD2-KDORDKL                        
058700     MOVE SHUV-IDUSER          TO PU-HRAD2-IDUSER                         
058800     INSPECT PU-HRAD2-IDUSER REPLACING LEADING ZERO BY SPACE              
058900     MOVE SHUV-IDPRODNR        TO PU-HRAD2-IDPRODNR                       
059000                                                                          
059100     MOVE SHUV-IDARTNR         TO PU-HRAD3-IDARTNR                        
059200                                                                          
059300     IF SHUV-FLSATPRI = JA                                                
059400        MOVE 'PRIO'            TO PU-HRAD4-PRIO                           
059500     ELSE                                                                 
059600        MOVE SPACE             TO PU-HRAD4-PRIO                           
059700     END-IF                                                               
059800     MOVE SHUV-KVBYGGB         TO PU-HRAD4-KVBYGGB                        
059900     MOVE SHUV-BEFT            TO PU-HRAD4-BEFT                           
060000     MOVE SHUV-DAREGDAT (3:6)  TO PU-HRAD4-TIREGDAT                       
060100                                                                          
060200     MOVE SHUV-IDANSK          TO PU-HRAD5-IDANSK                         
060300     MOVE SHUV-TIBEGPAC        TO PU-HRAD5-TIBEGPAC                       
060400                                                                          
060500     ACCEPT PU-HRAD7-TIUTSKR FROM DATE                                    
060600     ACCEPT WS-KLOCKAN       FROM TIME                                    
060700     .                                                                    
060800     EJECT                                                                
060900 C-LAES-PRC SECTION.                                                      
061000                                                                          
061100     MOVE IDDC-SATS     TO W-4453-IDDC                                    
061200     MOVE SHUV-IDPRC    TO W-4453-IDPRC                                   
061300                                                                          
061400     PERFORM IMS-GU-WDR1-WLXXKL11                                         
061500                                                                          
061600     IF SEGMENT-SAKNAS                                                    
061700        MOVE NEJ TO ALLT-SW                                               
061800     END-IF                                                               
061900                                                                          
062000     IF ALLT-OK                                                           
062100       MOVE '4'               TO WS-SYSTDEL-PU                            
062200                                 WS-SYSTDEL-PLE                           
062300                                                                          
062400       MOVE 'PU'              TO WS-LISTTYP-PU                            
062500       MOVE 'PE'              TO WS-LISTTYP-PLE                           
062600                                                                          
062700       MOVE 4454-KDPRTGEN-PU  TO WS-KDPRT-PU                              
062800       MOVE 4454-KDPRTGEN-PLE TO WS-KDPRT-PLE                             
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 D-SKRIV-PLE-PU SECTION.                                                  
063300*******************************                                           
063400**   INIT PRINTING PROCESS   **                                           
063500*******************************                                           
063600     PERFORM DA-OPEN-PRINTRAR                                             
063700                                                                          
063800*    GU IS READ IN B-LAES-SATSORDER                                       
063900     PERFORM DB-LAES-SATSORDERRAD                                         
064000                                                                          
064100     MOVE 'Y'               TO PAGE-SW                                    
064200     MOVE 'N'               TO PRINT-SW                                   
064300*******************************                                           
064400**   PRINTING PROCESS        **                                           
064500*******************************                                           
064600                                                                          
064700     PERFORM S08-KOLLA-OM-LASER-PU                                        
064800     PERFORM S09-KOLLA-OM-LASER-PLE                                       
064900                                                                          
065000     IF PU-LASER-SKRIVARE                                                 
065100     OR PLE-LASER-SKRIVARE                                                
065200                                                                          
065300*PLE - PRINT LABELS                                                       
065400                                                                          
065500       PERFORM UNTIL PLOCKSATS-KLAR                                       
065600          ADD 1                TO IDPURAD                                 
065700          PERFORM DH-SKRIV-LASER-ETIKETT                                  
065800          MOVE URAD-ADLAGOMR   TO SPAR-ADLAGOMR                           
065900          PERFORM DB-LAES-SATSORDERRAD                                    
066000       END-PERFORM                                                        
066100       MOVE NEJ  TO PLOCK-SW                                              
066200                                                                          
066300*PU  - PRINT PACKING SPECIFICATION                                        
066400                                                                          
066500       PERFORM IMS-GU-WDJ2-WLSATG01                                       
066600       MOVE 0                  TO IDPURAD                                 
066700       MOVE NEJ                TO SECOND-COPY-LIST-SW                     
066800                                                                          
066900       IF SEGMENT-FINNS                                                   
067000         PERFORM DB-LAES-SATSORDERRAD                                     
067100                                                                          
067200         PERFORM UNTIL PLOCKSATS-KLAR                                     
067300            ADD 1              TO IDPURAD                                 
067400            PERFORM DI-SKRIV-LASER-PU                                     
067500            MOVE URAD-ADLAGOMR TO SPAR-ADLAGOMR                           
067600            PERFORM DB-LAES-SATSORDERRAD                                  
067700         END-PERFORM                                                      
067800       END-IF                                                             
067900     ELSE                                                                 
068000       PERFORM UNTIL PLOCKSATS-KLAR                                       
068100          ADD 1                TO IDPURAD                                 
068200*PLE - PRINT LABELS                                                       
068300          PERFORM DC-SKRIV-PLE                                            
068400                                                                          
068500*PU  - PRINT PACKING SPECIFICATION                                        
068600          PERFORM DD-SKRIV-PU                                             
068700                                                                          
068800          MOVE URAD-ADLAGOMR   TO SPAR-ADLAGOMR                           
068900          PERFORM DB-LAES-SATSORDERRAD                                    
069000       END-PERFORM                                                        
069100     END-IF                                                               
069200*******************************                                           
069300**   CLOSE PRINTING PROCESS  **                                           
069400*******************************                                           
069500                                                                          
069600     IF PLE-MATRIS-SKRIVARE                                               
069700       PERFORM DE-CLOSE-PLE-LINES                                         
069800     END-IF                                                               
069900                                                                          
070000     PERFORM DG-SKRIV-TOTAL-PU                                            
070100     PERFORM DF-CLOSE-PRINTRAR                                            
070200     .                                                                    
070300     EJECT                                                                
070400 DA-OPEN-PRINTRAR SECTION.                                                
070500                                                                          
070600     CALL W006PRR2 USING PRT-SPOOL-OVR                                    
070700                         PRT-OPEN                                         
070800                         WS-IDPRTLST-PLE                                  
070900                         ALT3-PCB                                         
071000                         LISBPE-PCB                                       
071100                         WS-PLE-LISTID                                    
071200                         WS-PLE-DUMMY                                     
071300                         WS-PLE-DUMMY                                     
071400                                                                          
071500     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
071600                         PRT-OPEN                                         
071700                         WS-IDPRTLST-PU                                   
071800                         ALT4-PCB                                         
071900                         LISBA4S-PCB                                      
072000                         WS-PU-LISTID                                     
072100                         WS-PU-DUMMY                                      
072200                         WS-PU-DUMMY                                      
072300     .                                                                    
072400     EJECT                                                                
072500 DB-LAES-SATSORDERRAD SECTION.                                            
072600                                                                          
072700     PERFORM IMS-GNP-WDJ2-WLSATG12                                        
072800                                                                          
072900     IF SEGMENT-SAKNAS                                                    
073000        MOVE JA TO PLOCK-SW                                               
073100     ELSE                                                                 
073200        MOVE URAD-ADLAGOMR  TO SPAR-ADLAGOMR                              
073300     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
073600 DC-SKRIV-PLE SECTION.                                                    
073700                                                                          
073800*MATRIX PRINT                                                             
073900                                                                          
074000     MOVE URAD-IDARTNR           TO W-IDARTNR                             
074100     PERFORM IMS-GU-WDK6-WLARTC11                                         
074200                                                                          
074300     PERFORM DCA-REDIGERA-PLERADER                                        
074400                                                                          
074500     PERFORM DCB-SAETT-PRINT                                              
074600     .                                                                    
074700     EJECT                                                                
074800 DCA-REDIGERA-PLERADER SECTION.                                           
074900                                                                          
075000     MOVE URAD-ADLAGOMR          TO PLE-RAD1-ADLAGOMR                     
075100     MOVE URAD-ADGANG            TO PLE-RAD1-ADGANG                       
075200     MOVE URAD-ADPLATS           TO PLE-RAD1-ADPLATS                      
075300     MOVE URAD-IDARTNR           TO PLE-RAD1-IDARTNR                      
075400                                    W-IDARTNR                             
075500     MOVE URAD-BEART             TO PLE-RAD1-BEART                        
075600     MOVE URAD-REBEART           TO PLE-RAD1-REBEART                      
075700     MOVE URAD-KDSORT            TO PLE-RAD1-KDSORT                       
075800     MOVE CLAG-KDARTURS          TO PLE-RAD1-KDARTURS                     
075900                                                                          
076000     MOVE SHUV-IDDISTR           TO PLE-RAD2-IDDISTR                      
076100     MOVE SHUV-IDKUNDNR          TO PLE-RAD2-IDKUNDNR                     
076200     MOVE SHUV-IDPRODNR          TO PLE-RAD2-IDPRODNR                     
076300     MOVE SHUV-IDORDNSB          TO PLE-RAD2-IDORDNSB                     
076400     MOVE SHUV-IDORDNSS          TO PLE-RAD2-IDORDNSS                     
076500     MOVE IDPURAD                TO PLE-RAD2-IDRADNR                      
076600     MOVE SHUV-KDORDKL           TO PLE-RAD2-KDORDKL                      
076700     MOVE SHUV-IDPRC             TO PLE-RAD2-IDPRC                        
076800                                                                          
076900     .                                                                    
077000     EJECT                                                                
077100 DCB-SAETT-PRINT SECTION.                                                 
077200                                                                          
077300*MATRIX PRINT                                                             
077400                                                                          
077500     IF URAD-ADLAGOMR = SPAR-ADLAGOMR                                     
077600        IF PRINT-LINE                                                     
077700           MOVE PLE-RAD1         TO ARB-RAD-H(1)                          
077800           MOVE PLE-RAD2         TO ARB-RAD-H(2)                          
077900           IF SKIP-PAGE                                                   
078000              MOVE PRT-NYSIDA-RAD1         TO PRT-RADSKIP                 
078100              PERFORM DCBA-PRINT-PLE-LINES                                
078200              MOVE 'N'                     TO PAGE-SW                     
078300           ELSE                                                           
078400              MOVE PRT-AFTER-3             TO PRT-RADSKIP                 
078500              PERFORM DCBA-PRINT-PLE-LINES                                
078600           END-IF                                                         
078700           MOVE SPACES           TO ARB-RAD(1)                            
078800           MOVE SPACES           TO ARB-RAD(2)                            
078900           MOVE 'N'              TO PRINT-SW                              
079000        ELSE                                                              
079100           MOVE PLE-RAD1         TO ARB-RAD-V(1)                          
079200           MOVE PLE-RAD2         TO ARB-RAD-V(2)                          
079300           MOVE 'Y'              TO PRINT-SW                              
079400        END-IF                                                            
079500     ELSE                                                                 
079600        IF PRINT-LINE                                                     
079700           IF SKIP-PAGE                                                   
079800              MOVE PRT-NYSIDA-RAD1         TO PRT-RADSKIP                 
079900              PERFORM DCBA-PRINT-PLE-LINES                                
080000           ELSE                                                           
080100              MOVE PRT-AFTER-3             TO PRT-RADSKIP                 
080200              PERFORM DCBA-PRINT-PLE-LINES                                
080300           END-IF                                                         
080400           MOVE SPACES           TO ARB-RAD(1)                            
080500           MOVE SPACES           TO ARB-RAD(2)                            
080600        END-IF                                                            
080700        MOVE PLE-RAD1         TO ARB-RAD-V(1)                             
080800        MOVE PLE-RAD2         TO ARB-RAD-V(2)                             
080900        MOVE 'Y'              TO PAGE-SW                                  
081000        MOVE 'Y'              TO PRINT-SW                                 
081100        MOVE 0                TO ETIKETT-IX                               
081200     END-IF                                                               
081300                                                                          
081400     ADD 1            TO ETIKETT-IX                                       
081500                                                                          
081600     IF ETIKETT-IX = MAX-ETIKETT                                          
081700        MOVE 'Y'         TO PAGE-SW                                       
081800        MOVE 0           TO ETIKETT-IX                                    
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200                                                                          
082300 DCBA-PRINT-PLE-LINES SECTION.                                            
082400                                                                          
082500     MOVE ARB-RAD(1)  TO WS-PLE-LISTRAD                                   
082600     PERFORM S03-SKRIV-PLERAD                                             
082700     MOVE PRT-AFTER-5 TO PRT-RADSKIP                                      
082800     MOVE ARB-RAD(2)  TO WS-PLE-LISTRAD                                   
082900     PERFORM S03-SKRIV-PLERAD                                             
083000                                                                          
083100     .                                                                    
083200     EJECT                                                                
083300                                                                          
083400 DH-SKRIV-LASER-ETIKETT SECTION.                                          
083500     MOVE 'DH-SKRIV-LASER-ETIKETT '   TO WS-CURRENT-SECTION               
083600                                                                          
083700*LASER HERE STARTS THE CODE FOR PRINTING LASER LABELS                     
083800                                                                          
083900     MOVE URAD-IDARTNR           TO W-IDARTNR                             
084000     PERFORM IMS-GU-WDK6-WLARTC11                                         
084100                                                                          
084200     PERFORM DHA-REDIGERA-LASERRADER                                      
084300                                                                          
084400     PERFORM DHB-SAETT-RADSKIP                                            
084500                                                                          
084600     PERFORM S07-SKRIV-LASERRAD-PLE                                       
084700                                                                          
084800     .                                                                    
084900     EJECT                                                                
085000 DHA-REDIGERA-LASERRADER SECTION.                                         
085100     MOVE 'DHA-REDIGERA-LASERRADER'   TO WS-CURRENT-SECTION               
085200*                                                                         
085300     ADD    +1                   TO LASER-RAD-IX                          
085400     IF LASER-RAD-IX = 15                                                 
085500       MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                           
085600       MOVE +1                   TO LASER-RAD-IX                          
085700       MOVE '          '         TO LINE-IDAFPRCD                         
085800       MOVE '          '         TO WS-IDAFPRCD                           
085900     END-IF                                                               
086000*                                                                         
086100     EVALUATE WS-IDAFPRCD                                                 
086200       WHEN '          '                                                  
086300         MOVE '0         '       TO LINE-IDAFPRCD                         
086400         MOVE '0         '       TO WS-IDAFPRCD                           
086500       WHEN '0         '                                                  
086600         MOVE '2         '       TO LINE-IDAFPRCD                         
086700         MOVE '2         '       TO WS-IDAFPRCD                           
086800       WHEN '1         '                                                  
086900         MOVE '2         '       TO LINE-IDAFPRCD                         
087000         MOVE '2         '       TO WS-IDAFPRCD                           
087100       WHEN '2         '                                                  
087200         MOVE '1         '       TO LINE-IDAFPRCD                         
087300         MOVE '1         '       TO WS-IDAFPRCD                           
087400     END-EVALUATE                                                         
087500                                                                          
087600     ADD    +1                   TO IX-TAB                                
087700     MOVE  WS-IDAFPRCD           TO WS-TAB-IDAFPRCD(IX-TAB)               
087800     MOVE  PRT-RADSKIP           TO WS-TAB-RADSKIP(IX-TAB)                
088000                                                                          
088100     MOVE URAD-ADLAGOMR          TO LINE-ADLAGOMR                         
088200     MOVE URAD-ADGANG            TO LINE-ADGANG                           
088400                                                                          
089000     MOVE URAD-ADPLATS           TO WS-ADPLATS-ORD                        
089100     MOVE WS-ADPLATSNR           TO LINE-ADPLATSNR                        
089200     MOVE WS-ADPLNIV(1:1)        TO LINE-ADPLNIV-LEFT                     
089300     MOVE WS-ADPLNIV(2:1)        TO LINE-ADPLNIV-RIGHT                    
089400                                                                          
089500     MOVE URAD-IDARTNR           TO LINE-IDARTNR                          
089600                                    W-IDARTNR                             
089700     MOVE URAD-BEART             TO LINE-BEART                            
089800                                                                          
089900*    LINE-KVAVBART IS USED BECAUSE REBEART DONT EXIST ON THE LASER        
090000*    LABEL (W40376)                                                       
090100     MOVE URAD-REBEART           TO LINE-KVAVBART                         
090200                                                                          
090300     MOVE URAD-KDSORT            TO LINE-KDSORT                           
090400     MOVE CLAG-KDARTURS          TO LINE-KDARTURS                         
090500                                                                          
090600     MOVE '*COPY*'               TO LINE-COPY                             
090700                                                                          
090800     MOVE SHUV-IDDISTR           TO LINE-IDDISTR                          
090900     MOVE SHUV-IDKUNDNR          TO LINE-IDKUNDNR                         
091000     MOVE SHUV-IDPRODNR          TO LINE-IDPRODNR                         
091100     MOVE WS-PLE-IDORDNSB        TO LINE-IDORDNR5(1:4)                    
091200     MOVE WS-PLE-IDORDNSS        TO LINE-IDORDNR5(5:1)                    
091300                                                                          
091400     INSPECT LINE-IDORDNR5 REPLACING LEADING ZERO BY SPACE                
091410                                                                          
091500     MOVE IDPURAD                TO LINE-IDRADNR                          
091600     MOVE SHUV-KDORDKL           TO LINE-KDORDKL                          
091700     MOVE SHUV-IDPRC             TO LINE-IDPRC                            
091800                                                                          
091900     .                                                                    
092000     EJECT                                                                
092100                                                                          
092200 DHB-SAETT-RADSKIP SECTION.                                               
092300     MOVE 'DHB-SAETT-RADSKIP '    TO WS-CURRENT-SECTION                   
092400                                                                          
092500     IF (LASER-RAD-IX = 15)                                               
092600                                                                          
092700       MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                           
092800       MOVE +1                   TO LASER-RAD-IX                          
092900       MOVE '0         '         TO LINE-IDAFPRCD                         
093000       MOVE '0         '         TO WS-IDAFPRCD                           
093100     ELSE                                                                 
093200        IF URAD-ADLAGOMR NOT = SPAR-ADLAGOMR                              
093300                                                                          
093400          MOVE PRT-NYSIDA-RAD1   TO PRT-RADSKIP                           
093500          MOVE +1                TO LASER-RAD-IX                          
093600          MOVE '0         '      TO LINE-IDAFPRCD                         
093700          MOVE '0         '      TO WS-IDAFPRCD                           
093800        END-IF                                                            
093900     END-IF                                                               
094000                                                                          
094100     MOVE URAD-ADLAGOMR TO SPAR-ADLAGOMR                                  
094200     .                                                                    
094300     EJECT                                                                
094400                                                                          
094500*LASER - HERE STARTS LASER PRINT FOR PACKING SPEC                         
094600                                                                          
094700 DI-SKRIV-LASER-PU   SECTION.                                             
094800     MOVE 'DI-SKRIV-LASER-PU '    TO WS-CURRENT-SECTION                   
094900*                                                                         
095000     IF WS-IDPRTLST-PU = '4PUSVS  '                                       
095100     OR WS-IDPRTLST-PU = '4PUPUM  '                                       
096000       IF SECOND-COPY-LIST-SW = JA                                        
097000         MOVE '-COPY-'          TO PU-HRAD2-COPY                          
098000       ELSE                                                               
098100         MOVE '*COPY*'          TO PU-HRAD2-COPY                          
098200       END-IF                                                             
098300     END-IF                                                               
098400                                                                          
098500     IF PU-RAD-IX > 51                                                    
098600        PERFORM S01-SKRIV-PUHUVUD                                         
098700     END-IF                                                               
098800                                                                          
098900     PERFORM DDA-REDIGERA-PURAD                                           
099000                                                                          
099100     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
099200     MOVE PU-RAD      TO WS-PU-RAD                                        
099300     PERFORM S02-SKRIV-PURAD                                              
099400                                                                          
099500     ADD 2       TO PU-RAD-IX                                             
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900*MATRIX - START PRINT MATRIX PACKING SPEC                                 
100000                                                                          
100100 DD-SKRIV-PU SECTION.                                                     
100200     MOVE 'DD-SKRIV-PU       '    TO WS-CURRENT-SECTION                   
100300                                                                          
100400     IF PU-RAD-IX > 51                                                    
100500        PERFORM S01-SKRIV-PUHUVUD                                         
100600     END-IF                                                               
100700                                                                          
100800     PERFORM DDA-REDIGERA-PURAD                                           
100900                                                                          
101000     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
101100     MOVE PU-RAD      TO WS-PU-RAD                                        
101200     PERFORM S02-SKRIV-PURAD                                              
101300                                                                          
101400     ADD 2       TO PU-RAD-IX                                             
101500     .                                                                    
101600     EJECT                                                                
101700 DDA-REDIGERA-PURAD SECTION.                                              
101800     MOVE 'DDA-REDIGERA-PURAD'    TO WS-CURRENT-SECTION                   
101900                                                                          
102000     IF URAD-FLSATBRI = JA                                                
102100        MOVE '*'                TO PU-RAD-BRIST                           
102200     ELSE                                                                 
102300        MOVE SPACE              TO PU-RAD-BRIST                           
102400     END-IF                                                               
102500     MOVE URAD-KDSATKMB         TO PU-RAD-KDSATKMB                        
102600     MOVE URAD-ADLAGOMR         TO PU-RAD-ADLAGOMR                        
102700     MOVE URAD-ADGANG           TO PU-RAD-ADGANG                          
102800     MOVE URAD-ADPLATS          TO PU-RAD-ADPLATS                         
102900     MOVE IDPURAD               TO PU-RAD-IDPURAD                         
103000     MOVE URAD-IDARTNR          TO PU-RAD-IDARTNR                         
103100     MOVE URAD-BEART            TO PU-RAD-BEART                           
103200     MOVE URAD-REANTPSA         TO PU-RAD-REANTPSA                        
103300     MOVE URAD-KDSORT           TO PU-RAD-KDSORT                          
103400     MOVE URAD-REBEART          TO PU-RAD-REBEART                         
103500     .                                                                    
103600     EJECT                                                                
103700 DE-CLOSE-PLE-LINES SECTION.                                              
103800     MOVE ' DE-CLOSE-PLE-LINES'   TO WS-CURRENT-SECTION                   
103900                                                                          
104000     IF PRINT-LINE                                                        
104100       IF SKIP-PAGE                                                       
104200          MOVE PRT-NYSIDA-RAD1             TO PRT-RADSKIP                 
104300          PERFORM DCBA-PRINT-PLE-LINES                                    
104400          MOVE SPACES               TO ARB-RAD(1)                         
104500          MOVE SPACES               TO ARB-RAD(2)                         
104600          MOVE 'N'                  TO PAGE-SW                            
104700       ELSE                                                               
104800          MOVE PRT-AFTER-3                 TO PRT-RADSKIP                 
104900          PERFORM DCBA-PRINT-PLE-LINES                                    
105000          MOVE SPACES               TO ARB-RAD(1)                         
105100          MOVE SPACES               TO ARB-RAD(2)                         
105200       END-IF                                                             
105300       MOVE 'N'                  TO PRINT-SW                              
105400     END-IF                                                               
105500                                                                          
105600     .                                                                    
105700     EJECT                                                                
105800 DG-SKRIV-TOTAL-PU SECTION.                                               
105900     MOVE 'DG-SKRIV-TOTAL-PU  '   TO WS-CURRENT-SECTION                   
106000                                                                          
106200     MOVE PRT-AFTER-2            TO PRT-RADSKIP                           
106300     MOVE SHUV-VKORDNTO          TO PU-TOTRAD1-VKORDNTO                   
106400     MOVE SHUV-VLORDNTO          TO PU-TOTRAD1-VLORDNTO                   
106500     MOVE IDPURAD                TO PU-TOTRAD1-KVRADER                    
106600     MOVE PU-TOTRAD1             TO WS-PU-RAD                             
106700     PERFORM S02-SKRIV-PURAD                                              
106800     .                                                                    
106900     EJECT                                                                
107000 DF-CLOSE-PRINTRAR SECTION.                                               
107100     MOVE 'DF-CLOSE-PRINTRAR  '   TO WS-CURRENT-SECTION                   
107200                                                                          
107400     CALL W006PRR2 USING PRT-SPOOL-OVR                                    
107500                         PRT-CLOSE                                        
107600                         WS-IDPRTLST-PLE                                  
107700                         ALT3-PCB                                         
107800                         LISBPE-PCB                                       
107900                         WS-PLE-LISTID                                    
108000                         WS-PLE-DUMMY                                     
108100                         WS-PLE-DUMMY                                     
108200                                                                          
108300     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
108400                         PRT-CLOSE                                        
108500                         WS-IDPRTLST-PU                                   
108600                         ALT4-PCB                                         
108700                         LISBA4S-PCB                                      
108800                         WS-PU-LISTID                                     
108900                         WS-PU-DUMMY                                      
109000                         WS-PU-DUMMY                                      
109100     .                                                                    
109200     EJECT                                                                
109300 E-UPPDAT-SATSORDER SECTION.                                              
109400     MOVE 'E-UPPDAT-SATSORDER '   TO WS-CURRENT-SECTION                   
109500                                                                          
109600     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
109700                                                                          
109800     IF SEGMENT-FINNS                                                     
109900        IF SHUV-KDSATPLK = '2'                                            
110000           MOVE '3' TO SHUV-KDSATPLK                                      
110100        ELSE                                                              
110200           IF SHUV-KDSATPLK = '5'                                         
110300              MOVE '6' TO SHUV-KDSATPLK                                   
110400           END-IF                                                         
110500        END-IF                                                            
110600        PERFORM IMS-REPL-WDJ2-WLSATG01                                    
110700     END-IF                                                               
110800     .                                                                    
110900     EJECT                                                                
111000 F-SKICKA-IMSTRANS SECTION.                                               
111100     MOVE 'F-SKICKA-IMSTRANS  '   TO WS-CURRENT-SECTION                   
111200                                                                          
111300     IF SHUV-KDSATPLK = '3'                                               
111400        MOVE SHUV-IDORDNSB TO PTOP1-IDORDNSB                              
111500        MOVE SHUV-IDORDNSS TO PTOP1-IDORDNSS                              
111600        MOVE '1'           TO PTOP1-KDMFSFOR                              
111700        PERFORM IMS-ISRT-MSG-ALT1                                         
111800     ELSE                                                                 
111900        IF SHUV-KDSATPLK = '6'                                            
112000           MOVE SHUV-IDORDNSB TO PTOP2-IDORDNSB                           
112100           MOVE SHUV-IDORDNSS TO PTOP2-IDORDNSS                           
112200           MOVE '1'           TO PTOP2-KDMFSFOR                           
112300           PERFORM IMS-ISRT-MSG-ALT2                                      
112400        END-IF                                                            
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 S01-SKRIV-PUHUVUD SECTION.                                               
112900     MOVE 'S01-SKRIV-PUHUVUD  '   TO WS-CURRENT-SECTION                   
113000                                                                          
113100     ADD 1                TO SID-IX                                       
113200     MOVE SID-IX          TO PU-HRAD1-IDSID                               
113300     MOVE PRT-NYSIDA-RAD4 TO PRT-RADSKIP                                  
113400     MOVE PU-HRAD1        TO WS-PU-RAD                                    
113500     PERFORM S02-SKRIV-PURAD                                              
113600                                                                          
113700     MOVE PRT-AFTER-3     TO PRT-RADSKIP                                  
113800     MOVE PU-HRAD2        TO WS-PU-RAD                                    
113900     PERFORM S02-SKRIV-PURAD                                              
114000                                                                          
114100     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
114200     MOVE PU-HRAD3        TO WS-PU-RAD                                    
114300     PERFORM S02-SKRIV-PURAD                                              
114400                                                                          
114500     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
114600     MOVE PU-HRAD4        TO WS-PU-RAD                                    
114700     PERFORM S02-SKRIV-PURAD                                              
114800                                                                          
114900     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
115000     MOVE PU-HRAD5        TO WS-PU-RAD                                    
115100     PERFORM S02-SKRIV-PURAD                                              
115200                                                                          
115300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
115400     MOVE PU-HRAD6        TO WS-PU-RAD                                    
115500     PERFORM S02-SKRIV-PURAD                                              
115600                                                                          
115700     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
115800     MOVE PU-HRAD7        TO WS-PU-RAD                                    
115900     PERFORM S02-SKRIV-PURAD                                              
116000                                                                          
116100     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
116200     MOVE PU-HRAD8        TO WS-PU-RAD                                    
116300     PERFORM S02-SKRIV-PURAD                                              
116400                                                                          
116500     MOVE 15              TO PU-RAD-IX                                    
116600     .                                                                    
116700     EJECT                                                                
116800 S02-SKRIV-PURAD SECTION.                                                 
116900     MOVE 'S02-SKRIV-PURAD    '   TO WS-CURRENT-SECTION                   
117000                                                                          
117100     IF WS-IDPRTLST-PU = '4PUSVS  '                                       
117200     OR WS-IDPRTLST-PU = '4PUPUM  '                                       
117400*        -- SATSORDER VILL HA TVÅ EX                                      
117500         MOVE '2' TO PRT-COPIES-A4S                                       
117600     END-IF                                                               
117700     MOVE WS-IDPRTLST-PU TO PRT-IDLIST                                    
117800                                                                          
117900     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
118000                         PRT-WRITE                                        
118100                         PRT-IDLIST                                       
118200                         ALT4-PCB                                         
118300                         LISBA4S-PCB                                      
118400                         WS-PU-LISTID                                     
118500                         PRT-RADSKIP                                      
118600                         WS-PU-LISTRAD                                    
118700     .                                                                    
118800     EJECT                                                                
118900 S03-SKRIV-PLERAD SECTION.                                                
119000     MOVE 'S03-SKRIV-PLERAD   '   TO WS-CURRENT-SECTION                   
119100                                                                          
119200     MOVE WS-IDPRTLST-PLE   TO PRT-IDLIST                                 
119300     CALL W006PRR2 USING PRT-SPOOL-OVR                                    
119400                         PRT-WRITE                                        
119500                         PRT-IDLIST                                       
119600                         ALT3-PCB                                         
119700                         LISBPE-PCB                                       
119800                         WS-PLE-LISTID                                    
119900                         PRT-RADSKIP                                      
120000                         WS-PLE-LISTRAD                                   
120100     .                                                                    
120200     EJECT                                                                
120300                                                                          
120400 S07-SKRIV-LASERRAD-PLE   SECTION.                                        
120500     MOVE 'S07-SKRIV-LASERRAD-PLE'   TO WS-CURRENT-SECTION                
120700                                                                          
120800     IF PRT-RADSKIP = PRT-NYSIDA-RAD1                                     
120900       CONTINUE                                                           
121000     ELSE                                                                 
121100       MOVE PRT-AFTER-1          TO PRT-RADSKIP                           
121200     END-IF                                                               
121300                                                                          
121400     CALL W006PRR2 USING PRT-SPOOL-OVR                                    
121500                         PRT-WRITE                                        
121600                         WS-IDPRTLST-PLE                                  
121700                         ALT3-PCB                                         
121800                         LISBPE-PCB                                       
121900                         WS-PLE-LISTID                                    
122000                         PRT-RADSKIP                                      
122100                         LINE-W40376                                      
122200                                                                          
122400     IF PRT-RADSKIP = PRT-NYSIDA-RAD1                                     
122500       MOVE PRT-AFTER-1          TO PRT-RADSKIP                           
122600     END-IF                                                               
122700     .                                                                    
122800     EJECT                                                                
122900 S08-KOLLA-OM-LASER-PU  SECTION.                                          
123000     MOVE 'S08-KOLLA-OM-LASER-PU'     TO WS-CURRENT-SECTION               
123100                                                                          
123200*    -- CHECK OF IDPRTLST                                                 
123300     MOVE SPACE                 TO PRT-IDPRTLST                           
123400     MOVE '4'                   TO WS-SYSTDEL-PU                          
123500     MOVE 'PU'                  TO WS-LISTTYP-PU                          
123600                                                                          
123700     MOVE WS-IDPRTLST-PU        TO PRT-IDPRTLST                           
123800     MOVE 001                   TO PRT-KDCALL                             
123900                                                                          
124000     CALL W006PRT USING PRT-W006PRT                                       
124100                                                                          
124200     IF PRT-BEPRTLST(1:3) = 'IBM'                                         
124300       MOVE 'W40377'    TO PRT-PFDEF-A4S                                  
124400       MOVE JA          TO PU-SKRIVARTYP-SW                               
124500     ELSE                                                                 
124600       MOVE '      '    TO PRT-PFDEF-A4S                                  
124700       MOVE NEJ         TO PU-SKRIVARTYP-SW                               
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100                                                                          
125200 S09-KOLLA-OM-LASER-PLE   SECTION.                                        
125300     MOVE 'S09-KOLLA-OM-LASER-PLE'   TO WS-CURRENT-SECTION                
125400                                                                          
125500*    -- CHECK OF IDPRTLST                                                 
125600     MOVE SPACE                 TO PRT-IDPRTLST                           
125700     MOVE '4'                   TO WS-SYSTDEL-PLE                         
125800     MOVE 'PE'                  TO WS-LISTTYP-PLE                         
125900*    WS-KDPRT-PLE IS MOVED IN C-SECTION                                   
126000                                                                          
126100     MOVE WS-IDPRTLST-PLE       TO PRT-IDPRTLST                           
126200     MOVE 001                   TO PRT-KDCALL                             
126300                                                                          
126400     CALL W006PRT USING PRT-W006PRT                                       
126500     IF PRT-KDSVAR = 'R'                                                  
126600        IF PRT-BEPRTLST(1:5) = 'LASER'                                    
126800          MOVE 'W40378' TO PRT-PFDEF-OVR                                  
126900          MOVE JA       TO PLE-SKRIVARTYP-SW                              
127000        ELSE                                                              
127100          MOVE '      ' TO PRT-PFDEF-OVR                                  
127200          MOVE NEJ      TO PLE-SKRIVARTYP-SW                              
127300        END-IF                                                            
127400     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
127700* --- IMS SEKTIONER ---                                                   
127800                                                                          
127900 IMS-GU-MSG SECTION.                                                      
128000                                                                          
128100     MOVE '  QC' TO GODK-STATUSKODER                                      
128200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
128300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128400     PERFORM IMS-STATUSKONTROLL                                           
128500     .                                                                    
128600                                                                          
128700 IMS-ISRT-MSG-ALT1 SECTION.                                               
128800                                                                          
128900     MOVE SPACE TO GODK-STATUSKODER                                       
129000     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
129100     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400                                                                          
129500 IMS-ISRT-MSG-ALT2 SECTION.                                               
129600                                                                          
129700     MOVE SPACE TO GODK-STATUSKODER                                       
129800     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
129900     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     EJECT                                                                
130300 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
130400                                                                          
130500     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
130600          DELIMITED BY SIZE INTO SSA1                                     
130700     MOVE '  GE' TO GODK-STATUSKODER                                      
130800     CALL CBLTDLI USING GHU SATG-PCB DLI-IO-AREA1 SSA1                    
130900     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200                                                                          
131300 IMS-GU-WDJ2-WLSATG01 SECTION.                                            
131400                                                                          
131500     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
131600          DELIMITED BY SIZE INTO SSA1                                     
131700     MOVE '  GE' TO GODK-STATUSKODER                                      
131800     CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA1 SSA1                     
131900     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
132000     PERFORM IMS-STATUSKONTROLL                                           
132100     .                                                                    
132200                                                                          
132300                                                                          
132400 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
132500                                                                          
132600     MOVE '    ' TO GODK-STATUSKODER                                      
132700     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA1                        
132800     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
132900     PERFORM IMS-STATUSKONTROLL                                           
133000     .                                                                    
133100     EJECT                                                                
133200 IMS-GNP-WDJ2-WLSATG12 SECTION.                                           
133300                                                                          
133400     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
133500          DELIMITED BY SIZE INTO SSA1                                     
133600     STRING 'WLSATG12(KDSATLI  =' W-WDJ2-KDSATLI-X ')'                    
133700          DELIMITED BY SIZE INTO SSA2                                     
133800     MOVE '  GE' TO GODK-STATUSKODER                                      
133900     CALL CBLTDLI USING GNP SATG-PCB DLI-IO-AREA2 SSA1 SSA2               
134000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
134100     PERFORM IMS-STATUSKONTROLL                                           
134200     .                                                                    
134300     EJECT                                                                
134400 IMS-GU-WDR1-WLXXKL11 SECTION.                                            
134500                                                                          
134600     STRING 'WLXXKL01(WDGXKEY  =' W-WDR1-WDGXKEY-4453-X ')'               
134700          DELIMITED BY SIZE INTO SSA1                                     
134800     STRING 'WLXXKL11(KDSEGKEY =' W-WDR1-KDSEGKEY-4454-X ')'              
134900          DELIMITED BY SIZE INTO SSA2                                     
135000     MOVE '  GE' TO GODK-STATUSKODER                                      
135100     CALL CBLTDLI USING GU XXKL-PCB DLI-IO-AREA4 SSA1 SSA2                
135200     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500     EJECT                                                                
135600 IMS-GU-WDD3-WLBENA11 SECTION.                                            
135700                                                                          
135800     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
135900          DELIMITED BY SIZE INTO SSA1                                     
136000     STRING 'WLBENA11(IDSKYLT  =' W-WDD3-IDSKYLT-X ')'                    
136100          DELIMITED BY SIZE INTO SSA2                                     
136200     MOVE '  GE' TO GODK-STATUSKODER                                      
136300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
136400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
136500     PERFORM IMS-STATUSKONTROLL                                           
136600     .                                                                    
136700     EJECT                                                                
136800 IMS-GU-WDK6-WLARTC11 SECTION.                                            
136900                                                                          
137000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
137100          DELIMITED BY SIZE INTO SSA1                                     
137200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY ')'                          
137300          DELIMITED BY SIZE INTO SSA2                                     
137400     MOVE '    ' TO GODK-STATUSKODER                                      
137500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA5 SSA1 SSA2                
137600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
137700     PERFORM IMS-STATUSKONTROLL                                           
137800     .                                                                    
137900     EJECT                                                                
138000 IMS-STATUSKONTROLL SECTION.                                              
138100                                                                          
138200     SET STATUS-IX TO 1                                                   
138300     SEARCH GODK-STATUS                                                   
138400       AT END                                                             
138500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
138600              DELIMITED BY SIZE INTO FELTEXT                              
138700         CALL FELLOG                                                      
138800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
138900     END-SEARCH                                                           
139000     .                                                                    
