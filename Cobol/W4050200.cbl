000100******************************************************************        
000200*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0141      *        
000300******************************************************************        
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W4050200.                                                
000700 AUTHOR.         GERRY CARMICHAEL.                                        
000800 DATE-WRITTEN.   90/05/28.                                                
000900                                                                          
001000     REMARKS.                                                             
001100*                                                                         
001200*    FUNKTION.                                                            
001300*        PROGRAMMET VISAR ARTIKEL INFORMATION OM ANGIVET                  
001400*        ORDER.BEROENDE PÅ ORDERRADENS STATUS VISAS OLIKA                 
001500*        INFORMATION.                                                     
001600*                                                                         
001700*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001800*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001900*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
002000*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
002100*        PROGRAMMET LÄSER              WDE4 WDE6 WDB6                     
002200*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T502                                              
002600*        MID:         W4I50201                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O50201                                            
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500*    -- CHECKED BY WY2000                                                 
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W4050200'.            
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004200                                                                          
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600 77  WS-IDDISTR                  PIC X(4)  VALUE SPACE.                   
004700 77  WS-IDDISTR-NUM              PIC 9(5)  VALUE ZERO.                    
004800 77  WS-IDKUNDNR                 PIC X(6)  VALUE SPACE.                   
004900 77  WS-IDKUNDNR-NUM             PIC 9(7)  VALUE ZERO.                    
005000 77  WS-IDKUNDRF                 PIC X(7)  VALUE SPACE.                   
005100 77  WS-IDKUNDRF-NUM             PIC 9(7)  VALUE ZERO.                    
005200 77  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
005300 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
005400 77  WS-IDKOLLI                  PIC X(5)  VALUE SPACE.                   
005500 77  WS-IDKOLLI-NUM              PIC 9(5)  VALUE ZERO.                    
005600 77  WS-IDPRODNR                 PIC X(7)  VALUE SPACE.                   
005700 77  WS-IDPRODNR-NUM             PIC 9(7)  VALUE ZERO.                    
005710 77  WS-PRAVCOST-NUM             PIC Z(6)9.9(2) VALUE ZERO.               
005800 77  WS-PRARTNTO-NUM             PIC Z(6)9.9(2) VALUE ZERO.               
005810 77  WS-PRAVCOST                 PIC 9(7)V9(2)  VALUE ZERO.               
005820 77  WS-PRARTNTO                 PIC 9(7)V9(2)  VALUE ZERO.               
005900 77  WS-PRARTNTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006000 77  WS-PRARTNTO-LOCPREL         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006110 77  HELP-PRARTNTO               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006200 77  WS-IDKUNDRF-KOLL            PIC X(10) VALUE SPACE.                   
006300 77  WS-IDKUNDRF-CONT            PIC X(10) VALUE SPACE.                   
006310 77  WS-SEK                      PIC X(03) VALUE 'SEK'.                   
006400                                                                          
006500*01 -COPY WWDC99                                                          
006600                                                                          
006700 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
006800                                                                          
006900 01  W-SPAR-IDKUNDRF.                                                     
007000     03  W-SPAR-IDORDNR7         PIC X(7)  VALUE ZERO.                    
007100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
007200                                                                          
007300 01  WS-IDKUNDRF-WIP             PIC X(10).                               
007400                                                                          
007500 01  IDKUNDRF-WS                 PIC X(10).                               
007600 01  IDKUNDRF5-WS  REDEFINES  IDKUNDRF-WS.                                
007700     03 IDORDNR5-WS              PIC 9(5).                                
007800     03 FILLER                   PIC X(5).                                
007900 01  IDKUNDRF7-WS  REDEFINES  IDKUNDRF-WS.                                
008000     03 IDORDNR7-WS              PIC 9(7).                                
008100     03 FILLER                   PIC X(3).                                
008200 01  FILLER        REDEFINES  IDKUNDRF-WS.                                
008300     03 FILLER                   PIC X(5).                                
008400     03 IDKUNDRF-WS-POS6-7       PIC X(2).                                
008500     03 FILLER                   PIC X(3).                                
008600                                                                          
008700 01  WS-MOD-PRIS.                                                         
008800     03 WS-MOD-PRIS-NUM          PIC Z(8)9   VALUE ZERO.                  
008900     03 FILLER                   PIC X(2)    VALUE ' *'.                  
008901                                                                          
008910 01  WS-TEDDI.                                                            
008920     03 FILLER                   PIC X(8) VALUE SPACE.                    
008930     03 WS-KDVALISO              PIC X(3).                                
009000                                                                          
009100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009200     88  NYCKLAR-OK                          VALUE 'J'.                   
009300     88  NYCKLAR-FEL                         VALUE 'N'.                   
009400                                                                          
009500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009600     88  ALLT-OK                             VALUE 'J'.                   
009700                                                                          
009800 77  IDARTNR-SW                  PIC X       VALUE 'N'.                   
009900     88  IDARTNR-IFYLLT                      VALUE 'J'.                   
010000                                                                          
010100 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
010200     88  IFYLLT-OK                           VALUE 'J'.                   
010300                                                                          
010400 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
010500     88  FIRST-TIME                          VALUE 'J'.                   
010600                                                                          
010700 77  SCROLL-SW                   PIC X       VALUE 'J'.                   
010800     88  SCROLL                              VALUE 'J'.                   
010900                                                                          
011000 77  WDQ4-SW                     PIC X       VALUE 'J'.                   
011100     88  WDQ4-FIRST                          VALUE 'J'.                   
011200                                                                          
011300 77  IDKOLLI-SW                  PIC X       VALUE 'N'.                   
011400     88  IDKOLLI-IFYLLT                      VALUE 'J'.                   
011500                                                                          
011600 77  FLER-KOLLI-SW               PIC X       VALUE 'J'.                   
011700     88  FLER-KOLLI                          VALUE 'J'.                   
011800                                                                          
011900 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
012000     88  KOLLI-FINNS                         VALUE 'J'.                   
012100     88  KOLLI-SAKNAS                        VALUE 'N'.                   
012200                                                                          
012300 77  IDPRODNR-SW                 PIC X       VALUE 'N'.                   
012400     88  IDPRODNR-IFYLLT                     VALUE 'J'.                   
012500                                                                          
012600 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
012700     88  ARTIKEL-FINNS                       VALUE 'J'.                   
012800     88  ARTIKEL-SAKNAS                      VALUE 'N'.                   
013000                                                                          
013100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013200     88  EGEN-MID                            VALUE '4502'.                
013300     88  GODK-MID                            VALUE '4501' '4502'          
013400                                                   '4503' '4504'          
013500                                                   '4505' '4506'          
013600                                                   '4507' '4508'          
013700                                                   '4509'.                
013800     EJECT                                                                
013900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014000 01  GENERELLA-SUBPROGRAM.                                                
014100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014200     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014700     EJECT                                                                
014800 01  SPAR-AREOR.                                                          
014900     03  SPAR-STATUS             PIC X(2)    VALUE SPACE.                 
015000     03  SPAR-IDARTNR            PIC S9(9)   COMP-3 VALUE ZERO.           
015100     03  SPAR-IDKOLLI            PIC S9(5)   COMP-3 VALUE ZERO.           
015200     03  SPAR-IDPRODNR           PIC S9(7)   COMP-3 VALUE ZERO.           
015300     03  SPAR-KDODELSTA          PIC X       VALUE SPACE.                 
015410     03  SPAR-IDDC-MIN           PIC X(3)    VALUE SPACE.                 
015420     03  SPAR-IDDC-MAX           PIC X(3)    VALUE SPACE.                 
015500                                                                          
015600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015700*01 -COPY WMSGINIT                                                        
015800     EJECT                                                                
015900*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
016000*   -COPY WSECAREA                                                        
016100     SKIP3                                                                
016200*   -COPY W402W001                                                        
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016500*   -COPY WMEDAREA                                                        
016900     SKIP3                                                                
017000 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
017100*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
017200*    ----DISTR-DEALER-PRICE-----                                          
017300     EJECT                                                                
017360*      --- VALID IDDC CODES                                               
017380*01    -COPY WWDCKONS                                                     
017390       EJECT                                                              
017400 01  MESSAGE-CODES.                                                       
017500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
017700     03  INF-CASE-MISSING        PIC X(3)    VALUE '758'.                 
017800     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
017900     03  INF-ORDER-ANNULLED      PIC X(3)    VALUE '052'.                 
018000     03  INF-ORDER-JOINED        PIC X(3)    VALUE '030'.                 
018100     03  INF-ORDERLINES-MISSING  PIC X(3)    VALUE '029'.                 
018200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018300     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
018400     03  INF-ORDERINFO-BORTTAGEN PIC X(3)    VALUE '415'.                 
018500     03  ERR-ORDERHEAD-MISSING   PIC X(3)    VALUE '417'.                 
018600     03  INF-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
018700     EJECT                                                                
018800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018900*                                                                         
019000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019100     SKIP3                                                                
019200*01  MID -COPY W4I50201                                                   
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019500     SKIP3                                                                
019600*01  -COPY WMSGAREA                                                       
019700     EJECT                                                                
019800*    03  MOD -COPY W4O50201   -RED MSG-AREA.                              
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020100     SKIP3                                                                
020200*01  -COPY WMFSAREA                                                       
020300     EJECT                                                                
020400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020700     SKIP3                                                                
020800 01  NYCKLAR-TILL-DLI.                                                    
020900*    ---------TILL WDQ201                                                 
021000     03  W-IDORDER-X.                                                     
021100         05  W-OHUV-IDORDER       PIC S9(7)   VALUE ZERO COMP-3.          
021200*    ---------TILL WDQ201 VIA WDQ2C1(SEK. INDX)                           
021300     03  W-WDQ2CSEQ-X.                                                    
021400         05  W-Q2CSEQ-IDDISTR     PIC S9(5)    VALUE ZERO COMP-3.         
021500         05  W-Q2CSEQ-IDKUNDNR    PIC S9(7)    VALUE ZERO COMP-3.         
021600         05  W-Q2CSEQ-IDKUNDRF    PIC  X(10)   VALUE SPACE.               
021700*    --- -----TILL WDQ301                                                 
021800     03  W-WDQ301KY-MIN-X.                                                
021900         05  W-ODEL-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
022000         05  W-ODEL-IDDC-MIN      PIC X(2)     VALUE SPACE.               
022100         05  W-ODEL-IDPRODNR-MIN  PIC S9(7)    VALUE ZERO COMP-3.         
022200         05  W-ODEL-IDPLKLST-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
022300     03  W-WDQ301KY-MAX-X.                                                
022400         05  W-ODEL-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
022500         05  W-ODEL-IDDC-MAX      PIC X(2)     VALUE SPACE.               
022600         05  W-ODEL-IDPRODNR-MAX  PIC S9(7)    VALUE ZERO COMP-3.         
022700         05  W-ODEL-IDPLKLST-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
022800*    ---------TILL WDQ401                                                 
022900     03  W-WDQ401KY-MIN-X.                                                
023000         05  W-ORAD-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
023100         05  W-ORAD-IDDC-MIN      PIC X(2)     VALUE SPACE.               
023200         05  W-ORAD-ADLAGOMR-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
023300         05  W-ORAD-ADGANG-MIN    PIC S9(3)    VALUE ZERO COMP-3.         
023400         05  W-ORAD-ADPLATS-MIN   PIC S9(5)    VALUE ZERO COMP-3.         
023500         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
023600         05  W-ORAD-IDLOPNR-MIN   PIC S9(3)    VALUE ZERO COMP-3.         
023700     03  W-ORAD-IDARTNR-X.                                                
023800         05  W-ORAD-IDARTNR       PIC S9(9)    VALUE ZERO COMP-3.         
023900     03  W-WDQ401KY-MAX-X.                                                
024000         05  W-ORAD-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
024100         05  W-ORAD-IDDC-MAX      PIC X(2)     VALUE SPACE.               
024200         05  W-ORAD-ADLAGOMR-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
024300         05  W-ORAD-ADGANG-MAX    PIC S9(3)    VALUE ZERO COMP-3.         
024400         05  W-ORAD-ADPLATS-MAX   PIC S9(5)    VALUE ZERO COMP-3.         
024500         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
024600         05  W-ORAD-IDLOPNR-MAX   PIC S9(3)    VALUE ZERO COMP-3.         
024700     03  W-WDQ401KY-MIN-MIN-X.                                            
024800         05  W-ORAD-IDORDER-MIN-MIN  PIC S9(7)  VALUE ZERO COMP-3.        
024900         05  W-ORAD-IDDC-MIN-MIN     PIC X(2)   VALUE SPACE.              
025000         05  W-ORAD-ADLAGOMR-MIN-MIN PIC S9(3)  VALUE ZERO COMP-3.        
025100         05  W-ORAD-ADGANG-MIN-MIN   PIC S9(3)  VALUE ZERO COMP-3.        
025200         05  W-ORAD-ADPLATS-MIN-MIN  PIC S9(5)  VALUE ZERO COMP-3.        
025300         05  W-ORAD-IDARTNR-MIN-MIN  PIC S9(9)  VALUE ZERO COMP-3.        
025400         05  W-ORAD-IDLOPNR-MIN-MIN  PIC S9(3)  VALUE ZERO COMP-3.        
025500     03  W-WDQ401KY-MAX-MAX-X.                                            
025600         05  W-ORAD-IDORDER-MAX-MAX  PIC S9(7)  VALUE ZERO COMP-3.        
025700         05  W-ORAD-IDDC-MAX-MAX     PIC X(2)   VALUE SPACE.              
025800         05  W-ORAD-ADLAGOMR-MAX-MAX PIC S9(3)  VALUE ZERO COMP-3.        
025900         05  W-ORAD-ADGANG-MAX-MAX   PIC S9(3)  VALUE ZERO COMP-3.        
026000         05  W-ORAD-ADPLATS-MAX-MAX  PIC S9(5)  VALUE ZERO COMP-3.        
026100         05  W-ORAD-IDARTNR-MAX-MAX  PIC S9(9)  VALUE ZERO COMP-3.        
026200         05  W-ORAD-IDLOPNR-MAX-MAX  PIC S9(3)  VALUE ZERO COMP-3.        
026300*    ---------TILL WDE401                                                 
026400     03  W-KORD-WDE4KEY-X.                                                
026500         05  W-KORD-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.        
026600         05  W-KORD-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.        
026700         05  W-KORD-IDKUNDRF         PIC X(10)  VALUE SPACE.              
026800         05  W-KORD-IDPRODNR         PIC S9(7)  VALUE ZERO COMP-3.        
026900         05  W-KORD-IDPLKLST         PIC S9(3)  VALUE ZERO COMP-3.        
027000     03  W-KORD-WDE4KEY-MIN-X.                                            
027100         05  W-KORD-IDDISTR-MIN      PIC S9(5)  VALUE ZERO COMP-3.        
027200         05  W-KORD-IDKUNDNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.        
027300         05  W-KORD-IDKUNDRF-MIN     PIC X(10)  VALUE SPACE.              
027400         05  FILLER                  PIC S9(7)  VALUE ZERO COMP-3.        
027500         05  W-KORD-IDPLKLST-MIN     PIC S9(3)  VALUE ZERO COMP-3.        
027600     03  W-KORD-IDPRODNR-MIN-X.                                           
027700         05  W-KORD-IDPRODNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.        
027800*    ---------TILL WDE411                                                 
027900     03  W-IDPURAD-X.                                                     
028000         05  W-ORAD-IDPURAD          PIC S9(5)  VALUE ZERO COMP-3.        
028100*    ---------TILL WDE411 VIA B-KEY                                       
028200     03  W-WDE4BSEQ-MIN-X.                                                
028300         05  W-SEK-IDPRODNR-MIN     PIC S9(7)  VALUE ZERO COMP-3.         
028400         05  W-SEK-IDPURAD-MIN      PIC S9(5)  VALUE ZERO COMP-3.         
028500     03  W-WDE4BSEQ-MAX-X.                                                
028600         05  W-SEK-IDPRODNR-MAX     PIC S9(7)  VALUE ZERO COMP-3.         
028700         05  W-SEK-IDPURAD-MAX      PIC S9(5)  VALUE ZERO COMP-3.         
028800*    ---------TILL WDE401 VIA A-KEY                                       
028900     03  W-WDE4ASEQ-X.                                                    
029000         05  W-SEQA-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.        
029100         05  W-SEQA-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.        
029200         05  W-SEQA-IDKUNDRF         PIC X(10)  VALUE SPACE.              
029300*    ---------TILL WDE421                                                 
029400     03  W-WDE4KEY-X.                                                     
029500         05  W-KKOLLI-IDPRODNR      PIC S9(7)  VALUE ZERO COMP-3.         
029600         05  W-KKOLLI-IDKOLLI       PIC S9(5)  VALUE ZERO COMP-3.         
029700                                                                          
029800     03  W-IDPRODNR-X.                                                    
029900         05  W-IDPRODNR-WDE6        PIC S9(7)  VALUE ZERO COMP-3.         
030000                                                                          
030100     03  W-IDPRODNR-F6-X.                                                 
030200         05  W-IDPRODNR-WDF6        PIC S9(7)  VALUE ZERO COMP-3.         
030300                                                                          
030400     03  W-IDKOLLI-X.                                                     
030500         05  W-IDKOLLI-WDE6         PIC S9(5)  VALUE ZERO COMP-3.         
030600                                                                          
032800     03  W-IDDC-B6-X.                                                     
032900         05 W-IDDC-B6                  PIC X(2).                          
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
034000     SKIP3                                                                
034100 01  SSA1                        PIC X(160).                              
034200 01  SSA2                        PIC X(64).                               
034300 01  SSA3                        PIC X(64).                               
034400     EJECT                                                                
034500*    --- IMS FUNKTIONSKODER                                               
034600*01  -COPY W0003                                                          
034700     EJECT                                                                
034800******************************************************************        
034900*                                                                *        
035000*        ARBETS-AREOR TILL IO-AREORNA                                     
035100*                                                                *        
035200*        DLI INPUT-OUTPUT AREA                                   *        
035300*                                                                *        
035400******************************************************************        
035500*    ---  DLI INPUT-OUTPUT                                                
035600*    ---  DLI-IO-AREA                                                     
035700*                                                                         
035800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
035900 01  DLI-IO-AREA-OHUV.                                                    
036000*    03  WLORQI01   -COPY WDQ201                                          
036100     EJECT                                                                
036200                                                                          
036300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARB '.             
036400 01  DLI-IO-AREA-ARB.                                                     
036500*    03  WLORQI01   -COPY WDQ212                                          
036600     EJECT                                                                
036700                                                                          
036800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ODEL'.             
036900 01  DLI-IO-AREA-ODEL.                                                    
037000*    03  WLORQA01   -COPY WDQ301                                          
037100     EJECT                                                                
037200                                                                          
037300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORAD'.             
037400 01  DLI-IO-AREA-ORAD.                                                    
037500*    03  WLORQF01   -COPY WDQ401     -PRE QF01-                           
037600     EJECT                                                                
037700                                                                          
037800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-KORD'.             
037900 01  DLI-IO-AREA-KORD.                                                    
038000*    03  WDE401     -COPY WDE401                                          
038100     EJECT                                                                
038200                                                                          
038300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E411'.             
038400 01  DLI-IO-AREA-WDE411.                                                  
038500*    03  WDE411     -COPY WDE411     -PRE 411-                            
038600     EJECT                                                                
038700                                                                          
038800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE421'.           
038900 01  DLI-IO-AREA-WDE421.                                                  
039000*    03  WDE421-1   -COPY WDE421                                          
039100     EJECT                                                                
039110 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE601'.           
039120 01  DLI-IOAREA-WDE601.                                                   
039130*    03  WDE601   -COPY WDE601                                            
039140     EJECT                                                                
039200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE611'.           
039300 01  DLI-IOAREA-WDE611.                                                   
039400*    03  WDE611   -COPY WDE611                                            
039500     EJECT                                                                
040800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040900 01   DLI-IO-AREA-B601.                                                   
041000*     03  -COPY WDB601                                                    
041100                                                                          
041200 01  FILLER               PIC X(16)   VALUE 'WDF601 AREA'.                
041300 01   DLI-IO-AREA-F601.                                                   
041400*     03  -COPY WDF601                                                    
041500                                                                          
041600     EJECT                                                                
041700 LINKAGE SECTION.                                                         
041800                                                                          
041900*01  -COPY W0009      -PRE MSG-                                           
042000     EJECT                                                                
042100*01  -COPY W0008      -PRE USEA-                                          
042200     05  FILLER                  PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008      -PRE ORQI-                                          
042500     05  FILLER                  PIC X.                                   
042600     EJECT                                                                
042700*01  -COPY W0008      -PRE ORQA-                                          
042800     05  FILLER                  PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008      -PRE WDE4A-                                         
043100     05  FILLER                  PIC X.                                   
043200     EJECT                                                                
043300*01  -COPY W0008      -PRE WDE4B-                                         
043400     05  FILLER                  PIC X.                                   
043500     EJECT                                                                
043600*01  -COPY W0008      -PRE WDE6-                                          
043700     05  FILLER                  PIC X.                                   
043800     EJECT                                                                
043900*01  -COPY W0008      -PRE ORQF-                                          
044000     05  FILLER                  PIC X.                                   
044100     EJECT                                                                
045100*01  -COPY W0008      -PRE WDB6-                                          
045200     05  FILLER                  PIC X.                                   
045300     EJECT                                                                
045400*01  -COPY W0008      -PRE WDF6-                                          
045500     05  FILLER                  PIC X.                                   
045600     EJECT                                                                
045700 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB ORQI-PCB  ORQA-PCB           
045800                           WDE4A-PCB WDE4B-PCB                            
045900                           WDE6-PCB ORQF-PCB                              
046000                           WDB6-PCB WDF6-PCB.                             
046100 MAIN SECTION.                                                            
046200     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB ORQI-PCB  ORQA-PCB           
046300                           WDE4A-PCB WDE4B-PCB                            
046400                           WDE6-PCB ORQF-PCB                              
046500                           WDB6-PCB WDF6-PCB.                             
046600                                                                          
046700     PERFORM IMS-GET-MSG                                                  
046800     IF SEGMENT-FINNS                                                     
046900       PERFORM A-INIT                                                     
047000       PERFORM C-KOLLA-NYCKLAR                                            
047100       IF NYCKLAR-OK                                                      
047200         PERFORM H-KOLLA-BEHOERIGHET                                      
047300         IF ALLT-OK                                                       
047400           IF MFS-FIRST                                                   
047500             PERFORM D-FOERSTA-SIDA                                       
047600           ELSE                                                           
047700             IF MFS-NEXT                                                  
047800               PERFORM E-NAESTA-SIDA                                      
047900             ELSE                                                         
048000               PERFORM F-SAMMA-SIDA                                       
048100             END-IF                                                       
048200           END-IF                                                         
048300           PERFORM G-LAES-VISA-INFO                                       
048400         ELSE                                                             
048500           MOVE ERR-OBEHORIG TO MED-IDMFSFEL                              
048600           CALL WMEDKONV USING MED-WMEDAREA                               
048700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
048800         END-IF                                                           
048900       END-IF                                                             
049000       PERFORM IMS-INSERT-MSG                                             
049100     END-IF                                                               
049200                                                                          
049300     MOVE ZERO TO RETURN-CODE                                             
049400     GOBACK                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 A-INIT SECTION.                                                          
049800                                                                          
049900     MOVE JA TO ALLT-SW                                                   
050000                                                                          
050100     IF MSG-DUBBLA-TRANSKODER                                             
050200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50201                 
050300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
050400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
050500     ELSE                                                                 
050600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I50201                  
050700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
050800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
050900     END-IF                                                               
051000                                                                          
051100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
051200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
051300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
051400                                                                          
051500     MOVE LOW-VALUE TO MSG-AREA                                           
051600     MOVE 'W4O502N1' TO MFS-IDMOD                                         
051700     MOVE '4502' TO MOD-IDTRANS                                           
051800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
051900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O50201 + 4                        
052000                                                                          
052100     IF NOT EGEN-MID                                                      
052200       MOVE SPACE TO MFS-KDTRTYP                                          
052300       MOVE '7' TO MFS-IDPFK                                              
052400     END-IF                                                               
052500                                                                          
052600     IF ENGLISH-TEXT                                                      
052700       MOVE +2 TO SPRAK-IX                                                
052800       MOVE ' NET PRICE' TO MOD-VARHEAD                                   
052900     ELSE                                                                 
053000       MOVE +1 TO SPRAK-IX                                                
053100       MOVE '   RADPRIS' TO MOD-VARHEAD                                   
053200     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 C-KOLLA-NYCKLAR SECTION.                                                 
054000                                                                          
054100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
054200     MOVE '001'             TO MSGI-KDCALL                                
054300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
054400     MOVE '4502'            TO MSGI-IDTRANS                               
054500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
054600     IF EGEN-MID                                                          
054700        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
054800        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
054900        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
055000        IF MID-IDKUNDRF-IN      NOT = ALL '+'                             
055100           MOVE MID-IDKUNDRF-IN TO W-SPAR-IDORDNR7                        
055200           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
055300        END-IF                                                            
055400        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
055500        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
055600     END-IF                                                               
055700                                                                          
055800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
055900     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
056000                                                                          
056100     MOVE JA TO NYCKLAR-SW                                                
056200     MOVE LOW-VALUE TO         W-IDORDER-X                                
056300                             W-WDQ2CSEQ-X                                 
056400                             W-WDQ301KY-MIN-X                             
056500                             W-WDQ401KY-MIN-X                             
056600                             W-WDQ401KY-MIN-MIN-X                         
056700                             W-WDE4ASEQ-X                                 
056800                             W-WDE4BSEQ-MIN-X                             
056900                             W-WDE4KEY-X                                  
057000                             W-IDPURAD-X                                  
057100                             W-KORD-WDE4KEY-X                             
057200                             W-KORD-WDE4KEY-MIN-X                         
057300                                                                          
057400     MOVE HIGH-VALUE TO        W-WDQ301KY-MAX-X                           
057500                             W-WDE4BSEQ-MAX-X                             
057600                             W-WDQ401KY-MAX-X                             
057700                             W-WDQ401KY-MAX-MAX-X                         
057800                                                                          
057900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
058000                             MOD-IDKUNDNR-IN                              
058100                             MOD-IDKUNDRF-IN                              
058200                             MOD-IDARTNR-IN                               
058300                             MOD-IDKOLLI-IN                               
058400                             MOD-IDPRODNR-IN                              
058500                             MOD-IDDC-IN                                  
058600                                                                          
058700     PERFORM CA-KOLLA-DISTRIKT                                            
058800     PERFORM CB-KOLLA-KUNDNUMMER                                          
058900     PERFORM CC-KOLLA-ORDERNUMMER                                         
059000                                                                          
059100     IF IFYLLT-OK AND GODK-MID                                            
059200       PERFORM CD-KOLLA-ARTIKELNUMMER                                     
059300       PERFORM CE-KOLLA-KOLLINUMMER                                       
059400       PERFORM CF-KOLLA-PRODNUMMER                                        
059500     END-IF                                                               
059600                                                                          
059700     IF NYCKLAR-FEL                                                       
059800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059900       CALL WMEDKONV USING MED-WMEDAREA                                   
060000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
060100       PERFORM MFS-RENSA-FAELT-UT                                         
060200     END-IF                                                               
060300                                                                          
060400     PERFORM CG-KOLLA-IDDC                                                
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 CA-KOLLA-DISTRIKT SECTION.                                               
060900                                                                          
061000     IF MID-IDDISTR-IN = ALL '+' AND                                      
061100       MID-IDDISTR-UT = ALL SPACE                                         
061200       MOVE NEJ TO IFYLLT-SW                                              
061300     END-IF                                                               
061400                                                                          
061500     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
061600       MOVE '7'         TO MFS-IDPFK                                      
061700       MOVE SPACE       TO MFS-KDTRTYP                                    
061800     END-IF                                                               
061900                                                                          
062000     IF MSGI-IDDISTR NUMERIC                                              
062100       MOVE MSGI-IDDISTR TO WS-IDDISTR-NUM                                
062200       MOVE WS-IDDISTR-NUM TO W-Q2CSEQ-IDDISTR                            
062300                              W-SEQA-IDDISTR                              
062400                              W-KORD-IDDISTR                              
062500                              W-KORD-IDDISTR-MIN                          
062900     ELSE                                                                 
063000       MOVE NEJ TO NYCKLAR-SW                                             
063100     END-IF                                                               
063200                                                                          
063300     IF WS-IDDISTR-NUM = ZERO                                             
063400       MOVE '   0' TO MOD-IDDISTR-UT                                      
063500     ELSE                                                                 
063600       MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                            
063700       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
063800     END-IF                                                               
063900                                                                          
063910     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > 0                         
064000        MOVE MSGI-IDDISTR  TO TEST-IDDISTR                                
064010     ELSE                                                                 
064020        MOVE ZERO          TO TEST-IDDISTR                                
064100     END-IF                                                               
064200                                                                          
065800     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
065810     MOVE SPACES          TO MOD-TEDDI                                    
065900     .                                                                    
066000     EJECT                                                                
066100                                                                          
066200 CB-KOLLA-KUNDNUMMER SECTION.                                             
066300                                                                          
066400     IF MID-IDKUNDNR-IN     NOT = ALL '+'                                 
066500       MOVE '7'         TO MFS-IDPFK                                      
066600       MOVE SPACE       TO MFS-KDTRTYP                                    
066700     END-IF                                                               
066800                                                                          
066900     IF MSGI-IDKUNDNR NUMERIC                                             
067000       MOVE MSGI-IDKUNDNR   TO WS-IDKUNDNR-NUM                            
067100       MOVE WS-IDKUNDNR-NUM TO W-Q2CSEQ-IDKUNDNR                          
067200                               W-SEQA-IDKUNDNR                            
067300                               W-KORD-IDKUNDNR                            
067400                               W-KORD-IDKUNDNR-MIN                        
067600     ELSE                                                                 
067700       MOVE NEJ TO NYCKLAR-SW                                             
067800     END-IF                                                               
067900                                                                          
068000     MOVE MSGI-IDKUNDNR      TO MOD-IDKUNDNR-UT                           
068100     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
068200     IF MOD-IDKUNDNR-UT = ALL SPACE                                       
068300       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
068400     END-IF                                                               
068500     .                                                                    
068600     EJECT                                                                
068700                                                                          
068800 CC-KOLLA-ORDERNUMMER SECTION.                                            
068900                                                                          
069000     IF MID-IDKUNDRF-IN = ALL '+' AND                                     
069100       MID-IDKUNDRF-UT = ALL SPACE                                        
069200       MOVE NEJ TO IFYLLT-SW                                              
069300     END-IF                                                               
069400                                                                          
069500     IF MID-IDKUNDRF-IN     NOT = ALL '+'                                 
069600       MOVE '7'         TO MFS-IDPFK                                      
069700       MOVE SPACE       TO MFS-KDTRTYP                                    
069800     END-IF                                                               
069900                                                                          
070000     IF MSGI-IDKUNDRF(1:7) NUMERIC                                        
070100       MOVE MSGI-IDKUNDRF(1:7) TO WS-IDKUNDRF-NUM                         
070200                                  WS-IDKUNDRF-KOLL                        
070300       MOVE MSGI-IDKUNDRF (3:5) TO WS-IDKUNDRF-CONT                       
070400       MOVE WS-IDKUNDRF-NUM TO W-Q2CSEQ-IDKUNDRF                          
070500       MOVE WS-IDKUNDRF-NUM (3:5) TO W-SEQA-IDKUNDRF                      
070600                                     W-KORD-IDKUNDRF                      
070700                                     W-KORD-IDKUNDRF-MIN                  
070800     ELSE                                                                 
070900       MOVE NEJ TO NYCKLAR-SW                                             
071000     END-IF                                                               
071100                                                                          
071200     MOVE MSGI-IDKUNDRF(1:7) TO MOD-IDKUNDRF-UT                           
071300     INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE              
071400     IF MOD-IDKUNDRF-UT = SPACE                                           
071500       MOVE '      0' TO MOD-IDKUNDRF-UT                                  
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 CD-KOLLA-ARTIKELNUMMER SECTION.                                          
072000                                                                          
072100     IF MID-IDARTNR-IN = ALL '+'                                          
072200       MOVE MID-IDARTNR-UT TO WS-IDARTNR                                  
072300       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
072400     ELSE                                                                 
072500       MOVE MID-IDARTNR-IN TO WS-IDARTNR                                  
072600       MOVE '7'         TO MFS-IDPFK                                      
072700       MOVE SPACE       TO MFS-KDTRTYP                                    
072800       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
072900     END-IF                                                               
073000                                                                          
073100     IF WS-IDARTNR NUMERIC                                                
073200       IF WS-IDARTNR = ZERO                                               
073300         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
073400         MOVE NEJ TO IDARTNR-SW                                           
073500       ELSE                                                               
073600         MOVE WS-IDARTNR TO WS-IDARTNR-NUM                                
073700         MOVE WS-IDARTNR-NUM TO W-ORAD-IDARTNR                            
073800                                SPAR-IDARTNR                              
073900         MOVE JA TO IDARTNR-SW                                            
074000       END-IF                                                             
074100     ELSE                                                                 
074200       MOVE NEJ TO NYCKLAR-SW                                             
074300       MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                              
074400     END-IF                                                               
074500                                                                          
074600     IF GODK-MID AND WS-IDARTNR NOT = ZERO                                
074700       MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                              
074800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
074900     ELSE                                                                 
075000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 CE-KOLLA-KOLLINUMMER SECTION.                                            
075500                                                                          
075600     IF MID-IDKOLLI-IN = ALL '+'                                          
075700       MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                                  
075800       INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
075900     ELSE                                                                 
076000       MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                                  
076100       MOVE '7'         TO MFS-IDPFK                                      
076200       MOVE SPACE       TO MFS-KDTRTYP                                    
076300       INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
076400     END-IF                                                               
076500                                                                          
076600     IF WS-IDKOLLI NUMERIC                                                
076700       IF WS-IDKOLLI = ZERO                                               
076800         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-UT                           
076900         MOVE NEJ TO IDKOLLI-SW                                           
077000       ELSE                                                               
077100         MOVE WS-IDKOLLI TO WS-IDKOLLI-NUM                                
077200         MOVE WS-IDKOLLI-NUM TO SPAR-IDKOLLI                              
077300                                W-KKOLLI-IDKOLLI                          
077400         MOVE JA TO IDKOLLI-SW                                            
077500       END-IF                                                             
077600     ELSE                                                                 
077700       MOVE NEJ TO NYCKLAR-SW                                             
077800       MOVE WS-IDKOLLI     TO MOD-IDKOLLI-UT                              
077900     END-IF                                                               
078000                                                                          
078100     IF GODK-MID AND WS-IDKOLLI NOT = ZERO                                
078200       MOVE WS-IDKOLLI     TO MOD-IDKOLLI-UT                              
078300       INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE             
078400     ELSE                                                                 
078500       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-UT                             
078600     END-IF                                                               
078700     .                                                                    
078800     EJECT                                                                
078900 CF-KOLLA-PRODNUMMER SECTION.                                             
079000                                                                          
079100     IF MID-IDPRODNR-IN = ALL '+'                                         
079200       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
079300       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
079400     ELSE                                                                 
079500       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
079600       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
079700       MOVE '7'         TO MFS-IDPFK                                      
079800       MOVE SPACE       TO MFS-KDTRTYP                                    
079900     END-IF                                                               
080000                                                                          
080100     IF WS-IDPRODNR NUMERIC                                               
080200       IF WS-IDPRODNR = ZERO                                              
080300         MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                          
080400         MOVE NEJ TO IDPRODNR-SW                                          
080500       ELSE                                                               
080600         MOVE WS-IDPRODNR TO WS-IDPRODNR-NUM                              
080700         MOVE WS-IDPRODNR-NUM TO W-ODEL-IDPRODNR-MIN                      
080800                                 W-ODEL-IDPRODNR-MAX                      
080900                                 W-SEK-IDPRODNR-MIN                       
081000                                 W-SEK-IDPRODNR-MAX                       
081100         MOVE JA TO IDPRODNR-SW                                           
081200       END-IF                                                             
081300     ELSE                                                                 
081400       MOVE NEJ TO NYCKLAR-SW                                             
081500       MOVE WS-IDPRODNR    TO MOD-IDPRODNR-UT                             
081600     END-IF                                                               
081700                                                                          
081800     IF GODK-MID AND WS-IDPRODNR NOT = ZERO                               
081900       MOVE WS-IDPRODNR    TO MOD-IDPRODNR-UT                             
082000       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
082100     ELSE                                                                 
082200       MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                            
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700 CG-KOLLA-IDDC SECTION.                                                   
082800                                                                          
082900     IF MID-IDDC-IN       = ALL '+'                                       
083000       MOVE MID-IDDC-UT   TO W-IDDC-B6                                    
083100     ELSE                                                                 
083200       MOVE MID-IDDC-IN   TO W-IDDC-B6                                    
083300       MOVE '7'           TO MFS-IDPFK                                    
083400       MOVE SPACE         TO MFS-KDTRTYP                                  
083500     END-IF                                                               
083600     PERFORM IMS-GU-WDB601                                                
083700                                                                          
083800     MOVE W-IDDC-B6 TO MOD-IDDC-UT                                        
083900     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
084000       MOVE MSGI-IDDC TO W-ORAD-IDDC-MIN                                  
084100                         W-ORAD-IDDC-MAX                                  
084200                         W-ORAD-IDDC-MIN-MIN                              
084300                         W-ORAD-IDDC-MAX-MAX                              
084400                         W-ODEL-IDDC-MIN                                  
084500                         W-ODEL-IDDC-MAX                                  
084600                         MOD-IDDC-UT                                      
084700                         W-IDDC-B6                                        
084800       PERFORM IMS-GU-WDB601                                              
084900     ELSE                                                                 
085000       MOVE DCS-IDDC TO W-ORAD-IDDC-MIN                                   
085100                        W-ORAD-IDDC-MAX                                   
085200                        W-ORAD-IDDC-MIN-MIN                               
085300                        W-ORAD-IDDC-MAX-MAX                               
085400                        W-ODEL-IDDC-MIN                                   
085500                        W-ODEL-IDDC-MAX                                   
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900 D-FOERSTA-SIDA SECTION.                                                  
086000                                                                          
086100*    MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
086200*    CALL WMEDKONV USING MED-WMEDAREA                                     
086300*    MOVE MED-MFSINF TO MOD-TEMFSINF                                      
086400                                                                          
086500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
086600     MOVE ZERO       TO MOD-IDORDER-ENTER                                 
086700                        MOD-IDORDER-NEXT                                  
086800                        MOD-IDPURAD-ENTER                                 
086900                        MOD-IDPURAD-NEXT                                  
087000                        MOD-IDARTNR-ENTER                                 
087100                        MOD-IDARTNR-NEXT                                  
087200                        MOD-IDPRODNR-ENTER                                
087300                        MOD-IDPRODNR-NEXT                                 
087400                        MOD-IDDC-ENTER                                    
087500                        MOD-IDDC-NEXT                                     
087600                        MOD-ADLAGOMR-ENTER                                
087700                        MOD-ADLAGOMR-NEXT                                 
087800                        MOD-ADGANG-ENTER                                  
087900                        MOD-ADGANG-NEXT                                   
088000                        MOD-ADPLATS-ENTER                                 
088100                        MOD-ADPLATS-NEXT                                  
088200                        MOD-IDLOPNR-ENTER                                 
088300                        MOD-IDLOPNR-NEXT                                  
088400                        MOD-IDPLKLST-ENTER                                
088500                        MOD-IDPLKLST-NEXT                                 
088600                        MOD-IDKOLLI-ENTER                                 
088700                        MOD-IDKOLLI-NEXT                                  
088800     .                                                                    
088900     EJECT                                                                
089000 E-NAESTA-SIDA SECTION.                                                   
089100                                                                          
089200     MOVE  MID-IDARTNR-NEXT   TO  W-ORAD-IDARTNR                          
089300                                  W-ORAD-IDARTNR-MIN-MIN                  
089400     MOVE  MID-IDORDER-NEXT   TO  W-ORAD-IDORDER-MIN                      
089500                                  W-ORAD-IDORDER-MIN-MIN                  
089600                                  W-ORAD-IDORDER-MAX                      
089700                                  W-ORAD-IDORDER-MAX-MAX                  
089800                                  W-ODEL-IDORDER-MIN                      
089900                                  W-ODEL-IDORDER-MAX                      
090000     MOVE  MID-IDDC-NEXT      TO  W-ORAD-IDDC-MIN                         
090100                                  W-ORAD-IDDC-MAX                         
090200                                  W-ORAD-IDDC-MIN-MIN                     
090300                                  W-ORAD-IDDC-MAX-MAX                     
090400                                  W-ODEL-IDDC-MIN                         
090500                                  W-ODEL-IDDC-MAX                         
090600     MOVE  MID-ADLAGOMR-NEXT  TO  W-ORAD-ADLAGOMR-MIN                     
090700                                  W-ORAD-ADLAGOMR-MIN-MIN                 
090800     MOVE  MID-ADGANG-NEXT    TO  W-ORAD-ADGANG-MIN                       
090900                                  W-ORAD-ADGANG-MIN-MIN                   
091000     MOVE  MID-ADPLATS-NEXT   TO  W-ORAD-ADPLATS-MIN                      
091100                                  W-ORAD-ADPLATS-MIN-MIN                  
091200     MOVE  MID-IDLOPNR-NEXT   TO  W-ORAD-IDLOPNR-MIN                      
091300                                  W-ORAD-IDLOPNR-MIN-MIN                  
091400     MOVE  MID-IDPURAD-NEXT   TO  W-SEK-IDPURAD-MIN                       
091500                                  W-ORAD-IDPURAD                          
091600     MOVE  MID-IDKOLLI-NEXT   TO  W-KKOLLI-IDKOLLI                        
091700     MOVE  MID-IDPRODNR-NEXT  TO  W-ODEL-IDPRODNR-MIN                     
091800                                  W-KORD-IDPRODNR                         
091900                                  W-KORD-IDPRODNR-MIN                     
092000     MOVE  MID-IDPLKLST-NEXT  TO  W-ODEL-IDPLKLST-MIN                     
092100                                  W-KORD-IDPLKLST                         
092200                                  W-KORD-IDPLKLST-MIN                     
092300     .                                                                    
092400     EJECT                                                                
092500 F-SAMMA-SIDA SECTION.                                                    
092600                                                                          
092700     MOVE MID-IDARTNR-ENTER   TO  W-ORAD-IDARTNR                          
092800                                  W-ORAD-IDARTNR-MIN-MIN                  
092900     MOVE MID-IDORDER-ENTER   TO  W-ORAD-IDORDER-MIN                      
093000                                  W-ORAD-IDORDER-MIN-MIN                  
093100                                  W-ORAD-IDORDER-MAX                      
093200                                  W-ORAD-IDORDER-MAX-MAX                  
093300                                  W-ODEL-IDORDER-MIN                      
093400                                  W-ODEL-IDORDER-MAX                      
093500     MOVE MID-IDDC-ENTER      TO  W-ORAD-IDDC-MIN                         
093600                                  W-ORAD-IDDC-MAX                         
093700                                  W-ORAD-IDDC-MIN-MIN                     
093800                                  W-ORAD-IDDC-MAX-MAX                     
093900                                  W-ODEL-IDDC-MIN                         
094000                                  W-ODEL-IDDC-MAX                         
094100     MOVE MID-ADLAGOMR-ENTER  TO  W-ORAD-ADLAGOMR-MIN                     
094200                                  W-ORAD-ADLAGOMR-MIN-MIN                 
094300     MOVE MID-ADGANG-ENTER    TO  W-ORAD-ADGANG-MIN                       
094400                                  W-ORAD-ADGANG-MIN-MIN                   
094500     MOVE MID-ADPLATS-ENTER   TO  W-ORAD-ADPLATS-MIN                      
094600                                  W-ORAD-ADPLATS-MIN-MIN                  
094700     MOVE MID-IDLOPNR-ENTER   TO  W-ORAD-IDLOPNR-MIN                      
094800                                  W-ORAD-IDLOPNR-MIN-MIN                  
094900     MOVE MID-IDPURAD-ENTER   TO  W-SEK-IDPURAD-MIN                       
095000                                  W-ORAD-IDPURAD                          
095100     MOVE MID-IDKOLLI-ENTER   TO  W-KKOLLI-IDKOLLI                        
095200     MOVE MID-IDPRODNR-ENTER  TO  W-ODEL-IDPRODNR-MIN                     
095300                                  W-KORD-IDPRODNR                         
095400                                  W-KORD-IDPRODNR-MIN                     
095500     MOVE MID-IDPLKLST-ENTER  TO  W-ODEL-IDPLKLST-MIN                     
095600                                  W-KORD-IDPLKLST                         
095700                                  W-KORD-IDPLKLST-MIN                     
095800     IF ENGLISH-TEXT                                                      
095900       IF MFS-SPLIT                                                       
096000          MOVE 'VERKST-ONR' TO MOD-VARHEAD                                
096100       ELSE                                                               
096200          MOVE '   RADPRIS' TO MOD-VARHEAD                                
096300       END-IF                                                             
096400     ELSE                                                                 
096500       IF MFS-SPLIT                                                       
096600          MOVE 'WORK-ORDER' TO MOD-VARHEAD                                
096700       ELSE                                                               
096800          MOVE ' NET PRICE' TO MOD-VARHEAD                                
096900       END-IF                                                             
097000     END-IF                                                               
097100     .                                                                    
097200     EJECT                                                                
097300 G-LAES-VISA-INFO SECTION.                                                
097400                                                                          
097410     PERFORM S10-CHECK-EXPORT-ORDER                                       
097420                                                                          
097500     IF IDKOLLI-IFYLLT                                                    
097600       PERFORM GA-REDIGERA-WDE6                                           
097700     ELSE                                                                 
097800       PERFORM GB-LAES-WDQ2                                               
097801       IF ODEL-IDDC-EXP = WC-CDC-SE OR                                    
097802          ODEL-IDDC-EXP = SPACE                                           
097803         IF ODEL-IDDC-EXP NOT = DCS-IDDC                                  
097805           MOVE ODEL-KDVALISO TO WS-KDVALISO                              
097806           MOVE WS-TEDDI      TO MOD-TEDDI                                
097807         END-IF                                                           
097808       END-IF                                                             
097809       IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                               
097810          ODEL-IDDC-EXP NOT = SPACE                                       
097811          IF ODEL-IDDC-EXP NOT = DCS-IDDC AND                             
097812             ODEL-KDVALISO NOT = SPACE                                    
097813            MOVE WS-SEK         TO WS-KDVALISO                            
097814            MOVE WS-TEDDI       TO MOD-TEDDI                              
097815          END-IF                                                          
097816       END-IF                                                             
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200 GA-REDIGERA-WDE6 SECTION.                                                
098300                                                                          
098400     MOVE +1  TO INDX                                                     
098500     MOVE NEJ TO KOLLI-SW                                                 
098600                 ARTIKEL-SW                                               
098700     MOVE JA  TO FIRST-TIME-SW                                            
098800                 SCROLL-SW                                                
098900     PERFORM IMS-GU-WDE401-ASEQ                                           
099000     IF SEGMENT-FINNS                                                     
099100       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
099200         INDX > MAX-INDX                                                  
099300         PERFORM GAB-REDIGERA-BILDEN                                      
099400       END-PERFORM                                                        
099500       IF KOLLI-SAKNAS                                                    
099600         MOVE INF-CASE-MISSING TO MED-IDMFSINF                            
099700         CALL WMEDKONV USING MED-WMEDAREA                                 
099800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
099900       END-IF                                                             
100000       IF IDARTNR-IFYLLT                                                  
100100         IF ARTIKEL-SAKNAS                                                
100200           MOVE INF-PART-MISSING TO MED-IDMFSINF                          
100300           CALL WMEDKONV USING MED-WMEDAREA                               
100400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
100500         END-IF                                                           
100600       END-IF                                                             
100700     ELSE                                                                 
100800       MOVE INF-ORDER-MISSING TO MED-IDMFSINF                             
100900       CALL WMEDKONV USING MED-WMEDAREA                                   
101000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400 GAB-REDIGERA-BILDEN SECTION.                                             
101500                                                                          
101600     IF KORD-IDDC         = DCS-IDDC       AND                            
101700        KORD-IDPRODNR NOT = SPAR-IDPRODNR                                 
101800       MOVE KORD-IDPRODNR TO W-KORD-IDPRODNR                              
101900                             W-KORD-IDPRODNR-MIN                          
102000                             W-SEK-IDPRODNR-MIN                           
102100                             W-SEK-IDPRODNR-MAX                           
102200                             SPAR-IDPRODNR                                
102300                             MOD-IDPRODNR-ENTER                           
102400                                                                          
102500       PERFORM IMS-GU-WDE411-BSEQ                                         
102600       IF SEGMENT-FINNS                                                   
102700         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
102800           INDX > MAX-INDX                                                
102900           IF FIRST-TIME                                                  
103000            MOVE 411-ORAD-IDPURAD TO MOD-IDPURAD-ENTER                    
103100            MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                   
103200            MOVE NEJ TO FIRST-TIME-SW                                     
103300           END-IF                                                         
103400           PERFORM GABA-LAES-KOLLI-SEGMENT                                
103500         END-PERFORM                                                      
103600         IF SEGMENT-FINNS                                                 
103700           MOVE 411-ORAD-IDPRODNR TO MOD-IDPRODNR-NEXT                    
103800           MOVE 411-ORAD-IDPURAD TO MOD-IDPURAD-NEXT                      
103900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
104000           CALL WMEDKONV USING MED-WMEDAREA                               
104100           MOVE MED-MFSINF TO MOD-TEMFSINF                                
104200         END-IF                                                           
104300       END-IF                                                             
104400     END-IF                                                               
104500     PERFORM IMS-GN-WDE401-ASEQ                                           
104600     .                                                                    
104700     EJECT                                                                
104800 GABA-LAES-KOLLI-SEGMENT SECTION.                                         
104900     IF IDARTNR-IFYLLT                                                    
105000       IF 411-ORAD-IDARTNR = SPAR-IDARTNR                                 
105100         MOVE JA TO ARTIKEL-SW                                            
105200         PERFORM GABAA-FLYTTA-INFO-TILL-MOD                               
105300         PERFORM IMS-GN-WDE411-BSEQ                                       
105400       ELSE                                                               
105500         PERFORM IMS-GN-WDE411-BSEQ                                       
105600       END-IF                                                             
105700     ELSE                                                                 
105800       PERFORM GABAA-FLYTTA-INFO-TILL-MOD                                 
105900       PERFORM IMS-GN-WDE411-BSEQ                                         
106000     END-IF                                                               
106100     .                                                                    
106200     EJECT                                                                
106300 GABAA-FLYTTA-INFO-TILL-MOD SECTION.                                      
106400                                                                          
106500     MOVE 411-ORAD-IDPURAD TO W-ORAD-IDPURAD                              
106600     MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                          
106700     PERFORM IMS-GNP-WDE421                                               
106800     IF SEGMENT-FINNS                                                     
106900       IF INDX = +1                                                       
107000         MOVE 411-ORAD-IDPURAD TO MOD-IDPURAD-ENTER                       
107100                                  MOD-IDPURAD-NEXT                        
107200         MOVE KKOLLI-IDKOLLI   TO MOD-IDKOLLI-ENTER                       
107300                                  MOD-IDKOLLI-NEXT                        
107400         MOVE KKOLLI-IDKOLLI   TO W-IDKOLLI-WDE6                          
107500         MOVE W-KKOLLI-IDPRODNR  TO W-IDPRODNR-WDE6                       
107600       PERFORM IMS-GU-WDE611                                              
107700       END-IF                                                             
107800       MOVE JA TO KOLLI-SW                                                
107900       PERFORM S01-FLYTTA-WDE411-TILL-MOD                                 
108000       PERFORM S05-FLYTTA-WDE611-TILL-MOD                                 
108100       ADD +1 TO INDX                                                     
108200     END-IF                                                               
108300     .                                                                    
108400     EJECT                                                                
108500 GB-LAES-WDQ2 SECTION.                                                    
108600                                                                          
108700     PERFORM IMS-GU-ORQI-ORQI01-M-Q2CSEQ                                  
108800     IF SEGMENT-FINNS                                                     
108900       IF OHUV-KDTPOTYP = ZERO                                            
109000         MOVE OHUV-IDORDER TO W-OHUV-IDORDER                              
109100                              W-ORAD-IDORDER-MIN                          
109200                              W-ORAD-IDORDER-MAX                          
109300                              W-ORAD-IDORDER-MIN-MIN                      
109400                              W-ORAD-IDORDER-MAX-MAX                      
109500                              W-ODEL-IDORDER-MIN                          
109600                              W-ODEL-IDORDER-MAX                          
109700         IF OHUV-FLKLAR = 'N'                                             
109800           MOVE 'E' TO SPAR-STATUS                                        
109900         ELSE                                                             
110000           MOVE 'R' TO SPAR-STATUS                                        
110100         END-IF                                                           
110200         IF OHUV-FLBORT = 'J'                                             
110300           PERFORM IMS-GNP-ORQI-ORQI12                                    
110400           IF SEGMENT-SAKNAS                                              
110410             MOVE SPACE TO ODEL-KDVALISO                                  
110500             MOVE INF-ORDERINFO-BORTTAGEN TO MED-IDMFSINF                 
110600             CALL WMEDKONV USING MED-WMEDAREA                             
110700             MOVE MED-MFSINF TO MOD-TEMFSINF                              
110800           ELSE                                                           
110810             MOVE SPACE TO ODEL-KDVALISO                                  
110900             MOVE INF-ORDER-ANNULLED TO MED-IDMFSINF                      
111000             CALL WMEDKONV USING MED-WMEDAREA                             
111100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
111200           END-IF                                                         
111300         ELSE                                                             
111400           IF SEGMENT-FINNS                                               
111500             PERFORM GBA-LAES-WDQ3                                        
111600           END-IF                                                         
111700         END-IF                                                           
111800       ELSE                                                               
111810         MOVE SPACE TO ODEL-KDVALISO                                      
111900         MOVE INF-ORDER-MISSING TO MED-IDMFSINF                           
112000         CALL WMEDKONV USING MED-WMEDAREA                                 
112100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
112200       END-IF                                                             
112300     ELSE                                                                 
112310       MOVE SPACE TO ODEL-KDVALISO                                        
112400       MOVE INF-ORDER-MISSING TO MED-IDMFSINF                             
112500       CALL WMEDKONV USING MED-WMEDAREA                                   
112600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
112700     END-IF                                                               
112800     .                                                                    
112900     EJECT                                                                
113000 GBA-LAES-WDQ3 SECTION.                                                   
113100                                                                          
113200     MOVE JA TO WDQ4-SW                                                   
113300     MOVE NEJ TO ARTIKEL-SW                                               
113400     MOVE +1 TO INDX                                                      
113500     PERFORM IMS-GU-ORQA-ORQA01                                           
113600     IF SEGMENT-FINNS                                                     
113700       PERFORM GBAA-SPAR-WDQ301KY-ENTER                                   
113800       PERFORM S09-SPAR-WDQ301KY-NEXT                                     
113900       MOVE LOW-VALUE         TO W-WDQ301KY-MIN-X                         
114000       MOVE HIGH-VALUE        TO W-WDQ301KY-MAX-X                         
114100       MOVE OHUV-IDORDER      TO W-ODEL-IDORDER-MIN                       
114200                                 W-ODEL-IDORDER-MAX                       
114210       IF DCS-IDDC = ODEL-IDDC-EXP                                        
114212         MOVE ODEL-IDPRODNR TO W-IDPRODNR-WDE6                            
114213         PERFORM IMS-GU-WDE601                                            
114214         IF SEGMENT-FINNS AND VORD-KVKOLLI-FAKT > 0                       
114220            MOVE ODEL-IDDC       TO W-ODEL-IDDC-MIN                       
114230                                    W-ODEL-IDDC-MAX                       
114231         ELSE                                                             
114232            MOVE DCS-IDDC        TO W-ODEL-IDDC-MIN                       
114233                                    W-ODEL-IDDC-MAX                       
114234         END-IF                                                           
114240       ELSE                                                               
114250         MOVE DCS-IDDC        TO W-ODEL-IDDC-MIN                          
114260                                 W-ODEL-IDDC-MAX                          
114270       END-IF                                                             
114500       IF MFS-NEXT                                                        
114600          MOVE MID-IDPRODNR-NEXT TO W-ODEL-IDPRODNR-MIN                   
114700          MOVE MID-IDPLKLST-NEXT TO W-ODEL-IDPLKLST-MIN                   
114800       END-IF                                                             
114900       PERFORM IMS-GU-ORQA-ORQA01                                         
115000       IF SEGMENT-FINNS                                                   
115100         MOVE NEJ TO FIRST-TIME-SW                                        
115500         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
115600           INDX > MAX-INDX                                                
115700           PERFORM GBAD-LAES-VIDARE                                       
115800         END-PERFORM                                                      
115900       END-IF                                                             
116000     ELSE                                                                 
116100       MOVE +1 TO INDX                                                    
116200       PERFORM S07-LAES-WDQ401                                            
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600 GBAA-SPAR-WDQ301KY-ENTER SECTION.                                        
116700     MOVE ODEL-IDORDER      TO MOD-IDORDER-ENTER                          
116800     MOVE ODEL-IDDC         TO MOD-IDDC-ENTER                             
116900     MOVE ODEL-IDPRODNR     TO MOD-IDPRODNR-ENTER                         
117000     MOVE ODEL-IDPLKLST     TO MOD-IDPLKLST-ENTER                         
117100     .                                                                    
117200     EJECT                                                                
117300 GBAD-LAES-VIDARE SECTION.                                                
117400     IF ODEL-KDODELSTA = 'R'                                              
117500       IF WDQ4-FIRST                                                      
117600         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
117700         PERFORM S07-LAES-WDQ401                                          
117800         MOVE NEJ TO WDQ4-SW                                              
117900       END-IF                                                             
118000     ELSE                                                                 
118100       IF ODEL-IDPRODNR NOT = SPAR-IDPRODNR                               
118200         MOVE ODEL-IDPRODNR TO SPAR-IDPRODNR                              
118300         MOVE ODEL-IDPRODNR TO W-SEK-IDPRODNR-MIN                         
118400                               W-SEK-IDPRODNR-MAX                         
118500         PERFORM GBADA-REDIGERA-WDE4                                      
118600       END-IF                                                             
118700     END-IF                                                               
118800     PERFORM IMS-GN-ORQA-ORQA01                                           
118900     .                                                                    
119000     EJECT                                                                
119100 GBADA-REDIGERA-WDE4 SECTION.                                             
119200     PERFORM IMS-GU-WDE411-BSEQ                                           
119300     IF SEGMENT-FINNS                                                     
119400       MOVE JA TO FIRST-TIME-SW                                           
119500       MOVE 411-ORAD-IDPURAD  TO  MOD-IDPURAD-ENTER                       
119600       MOVE 411-ORAD-IDPURAD  TO  MOD-IDPURAD-NEXT                        
119700       PERFORM GBADAA-REDIGERA-BILDEN                                     
119800     ELSE                                                                 
119900       IF WDQ4-FIRST                                                      
120000         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
120100         PERFORM S07-LAES-WDQ401                                          
120200         MOVE NEJ TO WDQ4-SW                                              
120300       END-IF                                                             
120400     END-IF                                                               
120500     .                                                                    
120600     EJECT                                                                
120700 GBADAA-REDIGERA-BILDEN SECTION.                                          
120800     MOVE NEJ TO FLER-KOLLI-SW                                            
120900     MOVE JA TO FIRST-TIME-SW                                             
121000     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                      
121100       IF IDARTNR-IFYLLT                                                  
121200         IF 411-ORAD-IDARTNR = SPAR-IDARTNR                               
121300           MOVE JA TO ARTIKEL-SW                                          
121400           PERFORM GBADAAA-FLYTTA-TILL-MOD                                
121500         ELSE                                                             
121600           PERFORM IMS-GN-WDE411-BSEQ                                     
121700           IF SEGMENT-SAKNAS                                              
121800              MOVE LOW-VALUE TO W-WDE4BSEQ-MIN-X                          
121900              MOVE HIGH-VALUE TO W-WDE4BSEQ-MAX-X                         
122000           END-IF                                                         
122100         END-IF                                                           
122200       ELSE                                                               
122300         PERFORM GBADAAA-FLYTTA-TILL-MOD                                  
122400       END-IF                                                             
122500     END-PERFORM                                                          
122600     IF IDARTNR-IFYLLT                                                    
122700       IF ARTIKEL-SAKNAS                                                  
122800         MOVE INF-PART-MISSING TO MED-IDMFSINF                            
122900         CALL WMEDKONV USING MED-WMEDAREA                                 
123000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
123100       END-IF                                                             
123200     END-IF                                                               
123300     .                                                                    
123400     EJECT                                                                
123500 GBADAAA-FLYTTA-TILL-MOD SECTION.                                         
123600     IF FIRST-TIME                                                        
123700       PERFORM S01-FLYTTA-WDE411-TILL-MOD                                 
123800       MOVE NEJ TO FIRST-TIME-SW                                          
123900     END-IF                                                               
124000     MOVE 411-ORAD-IDPRODNR TO W-KKOLLI-IDPRODNR                          
124100                                                                          
124200     IF MFS-NEXT AND MID-IDKOLLI-NEXT > ZERO AND SCROLL AND               
124300       MID-IDKOLLI-NEXT NOT = MID-IDKOLLI-ENTER                           
124400       PERFORM IMS-GNP-WDE421                                             
124500       MOVE NEJ TO SCROLL-SW                                              
124600     ELSE                                                                 
124700       PERFORM IMS-GNP-WDE421-BKEY                                        
124800     END-IF                                                               
124900                                                                          
125000     IF SEGMENT-FINNS                                                     
125100       IF INDX = +1                                                       
125200         MOVE KKOLLI-IDKOLLI TO MOD-IDKOLLI-ENTER                         
125300                                MOD-IDKOLLI-NEXT                          
125400       END-IF                                                             
125500*         MOVE KKOLLI-IDKOLLI   TO W-IDKOLLI-WDE6                         
125600*         MOVE W-KKOLLI-IDPRODNR  TO W-IDPRODNR-WDE6                      
125700*      PERFORM IMS-GU-WDE611                                              
125800       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
125900         MOVE KKOLLI-IDKOLLI   TO W-IDKOLLI-WDE6                          
126000         MOVE W-KKOLLI-IDPRODNR  TO W-IDPRODNR-WDE6                       
126100         PERFORM IMS-GU-WDE611                                            
126200         MOVE 411-ORAD-IDARTNR TO MOD-IDARTNR(INDX)                       
126300                                                                          
126400         IF 411-ORAD-KDFARLIG = +4                                        
126500         OR 411-ORAD-KDFARLIG = +7                                        
126600           MOVE 'J'             TO MOD-KDFARLIG(INDX)                     
126700         ELSE                                                             
126800           MOVE SPACE           TO MOD-KDFARLIG(INDX)                     
126900         END-IF                                                           
127000                                                                          
127100         PERFORM S05-FLYTTA-WDE611-TILL-MOD                               
127200         PERFORM IMS-GNP-WDE421-BKEY                                      
127300         IF SEGMENT-FINNS                                                 
127400           ADD +1 TO INDX                                                 
127500         END-IF                                                           
127600       END-PERFORM                                                        
127700       IF SEGMENT-FINNS                                                   
127800         MOVE JA TO FLER-KOLLI-SW                                         
127900         MOVE KKOLLI-IDKOLLI TO MOD-IDKOLLI-NEXT                          
128000       END-IF                                                             
128100     END-IF                                                               
128200     IF NOT FLER-KOLLI                                                    
128300       ADD +1 TO INDX                                                     
128400       PERFORM IMS-GN-WDE411-BSEQ                                         
128500       MOVE JA TO FIRST-TIME-SW                                           
128600     END-IF                                                               
128700                                                                          
128800     IF SEGMENT-FINNS AND INDX > MAX-INDX                                 
128900       MOVE 411-ORAD-IDPURAD TO MOD-IDPURAD-NEXT                          
129000       PERFORM S09-SPAR-WDQ301KY-NEXT                                     
129100       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
129200       CALL WMEDKONV USING MED-WMEDAREA                                   
129300       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
129400     ELSE                                                                 
129500       IF INDX > MAX-INDX                                                 
129600          PERFORM IMS-GN-ORQA-ORQA01                                      
129700          IF SEGMENT-FINNS                                                
129800             PERFORM S09-SPAR-WDQ301KY-NEXT                               
129900             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
130000             CALL WMEDKONV USING MED-WMEDAREA                             
130100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
130200          END-IF                                                          
130300       END-IF                                                             
130400     END-IF                                                               
130500                                                                          
130600     IF SEGMENT-SAKNAS                                                    
130700        MOVE LOW-VALUE   TO W-WDE4BSEQ-MIN-X                              
130800        MOVE HIGH-VALUE  TO W-WDE4BSEQ-MAX-X                              
130900     END-IF                                                               
131000     .                                                                    
131100     EJECT                                                                
131200 H-KOLLA-BEHOERIGHET SECTION.                                             
131300                                                                          
131400     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
131500     MOVE '4502'            TO    SEC-IDTRANS                             
131600     MOVE MSGI-IDDISTR      TO    SEC-IDKEY                               
131700                                                                          
131800     CALL WSECURIT          USING SEC-IDUSER                              
131900                                  SEC-IDTRANS                             
132000                                  SEC-IDKEY                               
132100                                  SEC-KDSVAR                              
132200                                                                          
132300     IF SEC-KDSVAR = OBEHORIG                                             
132400        MOVE NEJ TO ALLT-SW                                               
132500     ELSE                                                                 
132600        CONTINUE                                                          
132700     END-IF                                                               
132800     .                                                                    
132900     EJECT                                                                
133000 S01-FLYTTA-WDE411-TILL-MOD SECTION.                                      
133100                                                                          
133200     IF IDARTNR-IFYLLT                                                    
133300       MOVE JA              TO ARTIKEL-SW                                 
133400       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
133500     END-IF                                                               
133600                                                                          
133700     MOVE +0                TO WS-PRARTNTO-NUM                            
133710     MOVE +0                TO WS-PRAVCOST-NUM                            
133800     MOVE +0                TO WS-PRARTNTO-LOC                            
133900     MOVE +0                TO WS-PRARTNTO-LOCPREL                        
134000     MOVE 411-ORAD-IDARTNR  TO MOD-IDARTNR(INDX)                          
134100     MOVE 411-ORAD-KVBEART  TO MOD-KVBEART-Q(INDX)                        
134200     MOVE 411-ORAD-KVAVBART TO MOD-KVAVBART(INDX)                         
134300     MOVE 411-ORAD-KVLEVART TO MOD-KVLEVART(INDX)                         
134400     MOVE 411-ORAD-IDKUNDRF-WIP TO WS-IDKUNDRF-WIP                        
134500                                                                          
134600     IF 411-ORAD-KDFARLIG = +4                                            
134700     OR 411-ORAD-KDFARLIG = +7                                            
134800       MOVE 'J'             TO MOD-KDFARLIG(INDX)                         
134900     ELSE                                                                 
135000       MOVE SPACE           TO MOD-KDFARLIG(INDX)                         
135100     END-IF                                                               
135200                                                                          
135300     IF 411-ORAD-IDKUNDRF-RO NOT = '00000     '                           
135400      IF 411-ORAD-IDKUNDRF-RO NOT = WS-IDKUNDRF-CONT                      
135500         MOVE 411-ORAD-IDKUNDRF-RO TO IDKUNDRF-WS                         
135600         IF IDKUNDRF-WS-POS6-7 = SPACE                                    
135700           MOVE IDORDNR5-WS TO MOD-IDKUNDRF-URS(INDX)                     
135800         ELSE                                                             
135900           MOVE IDORDNR7-WS TO MOD-IDKUNDRF-URS(INDX)                     
136000         END-IF                                                           
136100         INSPECT MOD-IDKUNDRF-URS(INDX) REPLACING LEADING                 
136200                                        ZERO BY SPACE                     
136300      END-IF                                                              
136400     END-IF                                                               
136500                                                                          
136600     IF 411-ORAD-KDRADSTA < +4                                            
136700       IF DIST79-DEALER-PRICE OR                                          
136720          DIST79-ECOM-PRICE                                               
136800                                                                          
137000         PERFORM S01A-RAKNA-KVAVBART-DIST79                               
137100                                                                          
137200       ELSE                                                               
137201         IF ODEL-IDDC-EXP = WC-CDC-SE OR ODEL-IDDC-EXP = SPACE            
137202            PERFORM S01F-ADD-PRICE                                        
137203         END-IF                                                           
137204         IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                             
137205            ODEL-IDDC-EXP NOT = SPACE                                     
137206            PERFORM S01G-ADD-PRICE-VOR                                    
137207         END-IF                                                           
137500       END-IF                                                             
137600       IF SEC-KDSVAR = 2 OR 6                                             
137700         MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO(INDX)                       
137800       ELSE                                                               
137900         IF DIST79-DEALER-PRICE OR                                        
137920            DIST79-ECOM-PRICE                                             
138000           IF 411-ORAD-PRARTNTO-LOC > 0                                   
138100                                                                          
138600             PERFORM S01C-FLYTTA-NTO-LOC                                  
138700                                                                          
138800           ELSE                                                           
138900             IF 411-ORAD-PRARTNTO-LOCPREL > 0                             
139000                                                                          
139500               PERFORM S01DA-FLYTTA-NTO-LOCPREL                           
139600                                                                          
139700             END-IF                                                       
139800           END-IF                                                         
139900         ELSE                                                             
140000           IF MFS-SPLIT                                                   
140100             MOVE WS-IDKUNDRF-WIP TO MOD-PRARTNTO-FIX(INDX)               
140200           ELSE                                                           
140201             IF ODEL-IDDC-EXP = WC-CDC-SE OR                              
140202                ODEL-IDDC-EXP = SPACE                                     
140203                PERFORM S01H-PRICE-TO-MOD                                 
140204             END-IF                                                       
140205             IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                         
140206                ODEL-IDDC-EXP NOT = SPACE                                 
140207                PERFORM S01I-PRICE-TO-MOD-VOR                             
140208             END-IF                                                       
140400           END-IF                                                         
140500         END-IF                                                           
140600       END-IF                                                             
140700                                                                          
140800       MOVE 'U'            TO  MOD-KDKOLSTA(INDX)                         
140900       PERFORM S01E-KOLLA-DDGS                                            
141000       IF 411-ORAD-KVANNANT > +0                                          
141100         MOVE 'D' TO MOD-KDAVVIK(INDX)                                    
141200       ELSE                                                               
141300         MOVE SPACE TO MOD-KDAVVIK(INDX)                                  
141400       END-IF                                                             
141500     ELSE                                                                 
141600       IF 411-ORAD-KVAVBART = ZERO                                        
141700* * * INNEBÄR ATT RADEN ÄR NOLLAD VID PACKNINGSRAPPORTERINGEN,            
141800* * * 'N' SOM STATUS SKALL DÅ STÅ FÖR NOLLNINGEN.                         
141900         MOVE 'N' TO  MOD-KDKOLSTA(INDX)                                  
142000       ELSE                                                               
142100         MOVE 'P' TO  MOD-KDKOLSTA(INDX)                                  
142200       END-IF                                                             
142300       IF 411-ORAD-FLFYSAVV = JA                                          
142400           MOVE 'A' TO MOD-KDAVVIK(INDX)                                  
142500       ELSE                                                               
142600         IF 411-ORAD-KVANNANT > +0                                        
142700            MOVE 'D' TO MOD-KDAVVIK(INDX)                                 
142800         ELSE                                                             
142900            MOVE SPACE TO MOD-KDAVVIK(INDX)                               
143000         END-IF                                                           
143100       END-IF                                                             
143200       IF DIST79-DEALER-PRICE OR                                          
143220          DIST79-ECOM-PRICE                                               
143300                                                                          
143400         PERFORM S01B-RAKNA-KVLEVART-DIST79                               
143500                                                                          
143600       ELSE                                                               
143601         IF ODEL-IDDC-EXP = WC-CDC-SE OR                                  
143602            ODEL-IDDC-EXP = SPACE                                         
143603            PERFORM S01J-ADD-PRICE                                        
143604         END-IF                                                           
143605         IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                             
143606            ODEL-IDDC-EXP NOT = SPACE                                     
143607            PERFORM S01K-ADD-PRICE-VOR                                    
143608         END-IF                                                           
143900       END-IF                                                             
144000       IF SEC-KDSVAR = 2 OR 6                                             
144100           MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO(INDX)                     
144200       ELSE                                                               
144300          IF DIST79-DEALER-PRICE OR                                       
144320             DIST79-ECOM-PRICE                                            
144400             IF 411-ORAD-PRARTNTO-LOC > 0                                 
144500                                                                          
145000               PERFORM S01C-FLYTTA-NTO-LOC                                
145100                                                                          
145200             ELSE                                                         
145300               IF 411-ORAD-PRARTNTO-LOCPREL > 0                           
145400                                                                          
145900                 PERFORM S01DB-FLYTTA-NTO-LOCPREL                         
146000                                                                          
146100               END-IF                                                     
146200             END-IF                                                       
146300          ELSE                                                            
146400           IF MFS-SPLIT                                                   
146500             MOVE WS-IDKUNDRF-WIP TO MOD-PRARTNTO-FIX(INDX)               
146600           ELSE                                                           
146601             IF ODEL-IDDC-EXP = WC-CDC-SE OR                              
146602                ODEL-IDDC-EXP = SPACE                                     
146603                PERFORM S01H-PRICE-TO-MOD                                 
146604             END-IF                                                       
146605             IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                         
146606                ODEL-IDDC-EXP NOT = SPACE                                 
146607                PERFORM S01I-PRICE-TO-MOD-VOR                             
146608             END-IF                                                       
146800           END-IF                                                         
146900          END-IF                                                          
147000                                                                          
147100       END-IF                                                             
147200     END-IF                                                               
147300                                                                          
147400                                                                          
147500     IF 411-ORAD-IDLEVNR NOT = SPACE                                      
147600       MOVE 411-ORAD-IDLEVNR TO  MOD-IDLEVNR(INDX)                        
147700     END-IF                                                               
147800     .                                                                    
147900     EJECT                                                                
148000 S01A-RAKNA-KVAVBART-DIST79 SECTION.                                      
148100                                                                          
148200     COMPUTE WS-PRARTNTO-LOC = 411-ORAD-KVAVBART *                        
148300                               411-ORAD-PRARTNTO-LOC                      
148400     COMPUTE WS-PRARTNTO-LOCPREL = 411-ORAD-KVAVBART *                    
148500                                   411-ORAD-PRARTNTO-LOCPREL              
148600     .                                                                    
148700     EJECT                                                                
148800 S01B-RAKNA-KVLEVART-DIST79 SECTION.                                      
148900                                                                          
149000     COMPUTE WS-PRARTNTO-LOC = 411-ORAD-KVLEVART *                        
149100                               411-ORAD-PRARTNTO-LOC                      
149200     COMPUTE WS-PRARTNTO-LOCPREL = 411-ORAD-KVLEVART *                    
149300                                   411-ORAD-PRARTNTO-LOCPREL              
149400     .                                                                    
149500     EJECT                                                                
149600 S01C-FLYTTA-NTO-LOC SECTION.                                             
149700                                                                          
149800     MOVE WS-PRARTNTO-LOC TO HELP-PRARTNTO                                
150000     IF MFS-SPLIT                                                         
150100       MOVE WS-IDKUNDRF-WIP  TO MOD-PRARTNTO-FIX(INDX)                    
150200     ELSE                                                                 
150310       MOVE HELP-PRARTNTO    TO MOD-PRARTNTO(INDX)                        
150400     END-IF                                                               
150600     .                                                                    
150700     EJECT                                                                
150800 S01DA-FLYTTA-NTO-LOCPREL SECTION.                                        
150900                                                                          
151000     MOVE WS-PRARTNTO-LOCPREL TO HELP-PRARTNTO                            
151310     IF HELP-PRARTNTO    < 1                                              
151410       MOVE 1    TO HELP-PRARTNTO                                         
151500     END-IF                                                               
151600     IF MFS-SPLIT                                                         
151700       MOVE WS-IDKUNDRF-WIP  TO MOD-PRARTNTO-FIX(INDX)                    
151800     ELSE                                                                 
151910       MOVE HELP-PRARTNTO    TO MOD-PRARTNTO(INDX)                        
152000       MOVE '. *' TO MOD-PRARTNTO-FIX(INDX)(8:3)                          
152100     END-IF                                                               
152300     .                                                                    
152400     EJECT                                                                
152500 S01DB-FLYTTA-NTO-LOCPREL SECTION.                                        
152600                                                                          
152700     MOVE WS-PRARTNTO-LOCPREL TO HELP-PRARTNTO                            
152910     IF HELP-PRARTNTO < 1                                                 
153010       MOVE 1      TO HELP-PRARTNTO                                       
153100     END-IF                                                               
153200     IF MFS-SPLIT                                                         
153300       MOVE WS-IDKUNDRF-WIP  TO MOD-PRARTNTO-FIX(INDX)                    
153400     ELSE                                                                 
153510       MOVE HELP-PRARTNTO    TO MOD-PRARTNTO(INDX)                        
153600       MOVE '. *'        TO MOD-PRARTNTO-FIX(INDX)(8:3)                   
153700     END-IF                                                               
153800     .                                                                    
153900     EJECT                                                                
154000 S01E-KOLLA-DDGS SECTION.                                                 
154100                                                                          
154200     MOVE ODEL-IDDC  TO WS-IDDC                                           
154300                                                                          
154400     IF GOOD-DDC                                                          
154500        MOVE ODEL-IDPRODNR TO W-IDPRODNR-WDF6                             
154600        PERFORM IMS-GU-WDF601                                             
154700        IF SEGMENT-FINNS                                                  
154800           MOVE 'RD'       TO MOD-KDKOLSTA(INDX)                          
154900        END-IF                                                            
155000     END-IF                                                               
155100     .                                                                    
155200     EJECT                                                                
155210 S01F-ADD-PRICE SECTION.                                                  
155221*    *NORMAL ORDER OR BOUNCE ORDER (NOT VOR)                              
155222                                                                          
155230     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155240        COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVAVBART *                     
155250                                  411-ORAD-PRARTNTO                       
155260     ELSE                                                                 
155270       IF 411-ORAD-PRAVCOST > 0                                           
155280         COMPUTE WS-PRAVCOST-NUM = 411-ORAD-KVAVBART *                    
155290                                   411-ORAD-PRAVCOST                      
155291       ELSE                                                               
155292         COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVAVBART *                    
155293                                   411-ORAD-PRARTNTO                      
155294       END-IF                                                             
155295     END-IF                                                               
155296     .                                                                    
155297     EJECT                                                                
155298 S01G-ADD-PRICE-VOR SECTION.                                              
155299*    *VOR ORDER BOUNCE                                                    
155300                                                                          
155301     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155302        COMPUTE WS-PRAVCOST-NUM = 411-ORAD-KVAVBART *                     
155303                                  411-ORAD-PRAVCOST                       
155304     ELSE                                                                 
155308        COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVAVBART *                     
155309                                  411-ORAD-PRARTNTO                       
155311     END-IF                                                               
155312     .                                                                    
155313     EJECT                                                                
155314 S01H-PRICE-TO-MOD SECTION.                                               
155315*    *NORMAL ORDER OR BOUNCE ORDER (NOT VOR)                              
155316                                                                          
155336     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155337        MOVE WS-PRARTNTO-NUM   TO MOD-PRARTNTO(INDX)                      
155338        MOVE 411-ORAD-KDVALISO TO WS-KDVALISO                             
155339        MOVE WS-TEDDI          TO MOD-TEDDI                               
155340     ELSE                                                                 
155341       MOVE WS-PRAVCOST-NUM    TO WS-PRAVCOST                             
155342       IF WS-PRAVCOST > 0                                                 
155343          MOVE WS-PRAVCOST-NUM                                            
155344                      TO MOD-PRARTNTO(INDX)                               
155345          MOVE 411-ORAD-KDVALISO-EXP                                      
155346                               TO WS-KDVALISO                             
155347          MOVE WS-TEDDI        TO MOD-TEDDI                               
155348       ELSE                                                               
155349          MOVE WS-PRARTNTO-NUM  TO MOD-PRARTNTO(INDX)                     
155350          MOVE 411-ORAD-KDVALISO                                          
155351                               TO WS-KDVALISO                             
155352          MOVE WS-TEDDI        TO MOD-TEDDI                               
155353       END-IF                                                             
155354     END-IF                                                               
155355     .                                                                    
155356     EJECT                                                                
155357 S01I-PRICE-TO-MOD-VOR SECTION.                                           
155358*    *BOUNCE ORDER VOR                                                    
155359                                                                          
155360     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155361        MOVE WS-PRAVCOST-NUM       TO MOD-PRARTNTO(INDX)                  
155362        MOVE 411-ORAD-KDVALISO-EXP TO WS-KDVALISO                         
155363        MOVE WS-TEDDI              TO MOD-TEDDI                           
155364     ELSE                                                                 
155365        MOVE WS-PRARTNTO-NUM       TO MOD-PRARTNTO(INDX)                  
155366        MOVE 411-ORAD-KDVALISO                                            
155367                                   TO WS-KDVALISO                         
155368        MOVE WS-TEDDI              TO MOD-TEDDI                           
155369     END-IF                                                               
155370     .                                                                    
155371     EJECT                                                                
155372 S01J-ADD-PRICE SECTION.                                                  
155373*    *NORMAL ORDER AND BOUNCE NOT VOR                                     
155374                                                                          
155387     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155388        COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVLEVART *                     
155389                                   411-ORAD-PRARTNTO                      
155390     ELSE                                                                 
155391       IF 411-ORAD-PRAVCOST > 0                                           
155392         COMPUTE WS-PRAVCOST-NUM = 411-ORAD-KVLEVART *                    
155393                                   411-ORAD-PRAVCOST                      
155394       ELSE                                                               
155395         COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVLEVART *                    
155396                                   411-ORAD-PRARTNTO                      
155397       END-IF                                                             
155398     END-IF                                                               
155399     .                                                                    
155400     EJECT                                                                
155401 S01K-ADD-PRICE-VOR SECTION.                                              
155402*    *BOUNCE ORDER VOR                                                    
155403                                                                          
155404     IF ODEL-IDDC-EXP  = DCS-IDDC                                         
155405        COMPUTE WS-PRAVCOST-NUM = 411-ORAD-KVLEVART *                     
155406                                  411-ORAD-PRAVCOST                       
155407     ELSE                                                                 
155408        COMPUTE WS-PRARTNTO-NUM = 411-ORAD-KVLEVART *                     
155409                                  411-ORAD-PRARTNTO                       
155410     END-IF                                                               
155411     .                                                                    
155412     EJECT                                                                
155413 S03-FLYTTA-WDQ401-TILL-MOD SECTION.                                      
155420                                                                          
155500     IF IDARTNR-IFYLLT                                                    
155600       MOVE JA               TO ARTIKEL-SW                                
155700     END-IF                                                               
155800                                                                          
155900     MOVE +0                 TO WS-PRAVCOST-NUM                           
155910     MOVE +0                 TO WS-PRARTNTO-NUM                           
156000     MOVE +0                 TO WS-PRARTNTO-LOC                           
156100     MOVE +0                 TO WS-PRARTNTO-LOCPREL                       
156200     MOVE QF01-ORAD-IDARTNR  TO  MOD-IDARTNR(INDX)                        
156300                                                                          
156400     IF QF01-ORAD-KDFARLIG = +4                                           
156500     OR QF01-ORAD-KDFARLIG = +7                                           
156600       MOVE 'J'                 TO MOD-KDFARLIG(INDX)                     
156700     ELSE                                                                 
156800       MOVE SPACE               TO MOD-KDFARLIG(INDX)                     
156900     END-IF                                                               
157000                                                                          
157100     MOVE QF01-ORAD-KVBEART-Q    TO   MOD-KVBEART-Q(INDX)                 
157200     MOVE QF01-ORAD-IDKUNDRF-WIP TO   WS-IDKUNDRF-WIP                     
157300     IF DIST79-DEALER-PRICE OR                                            
157320        DIST79-ECOM-PRICE                                                 
157400                                                                          
157800        PERFORM S03A-RAKNA-KVBEART-DIST79                                 
157900                                                                          
158000     ELSE                                                                 
158001       IF ODEL-IDDC-EXP = WC-CDC-SE OR                                    
158002          ODEL-IDDC-EXP = SPACE                                           
158003          PERFORM S03D-ADD-Q4-PRICE                                       
158004       END-IF                                                             
158005       IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                               
158006          ODEL-IDDC-EXP NOT = SPACE                                       
158007          PERFORM S03E-ADD-Q4-PRICE-VOR                                   
158008       END-IF                                                             
158300     END-IF                                                               
158400     IF SEC-KDSVAR = 2 OR 6                                               
158500       MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO(INDX)                         
158600     ELSE                                                                 
158700       IF DIST79-DEALER-PRICE OR                                          
158720          DIST79-ECOM-PRICE                                               
158800         IF QF01-ORAD-PRARTNTO-LOC > 0                                    
158900                                                                          
159400           PERFORM S03B-FLYTTA-NTO-LOC                                    
159500                                                                          
159600         ELSE                                                             
159700           IF QF01-ORAD-PRARTNTO-LOCPREL > 0                              
159800                                                                          
160200             PERFORM S03C-FLYTTA-NTO-LOCPREL                              
160300                                                                          
160400           END-IF                                                         
160500         END-IF                                                           
160600       ELSE                                                               
160700         IF MFS-SPLIT                                                     
160800           MOVE WS-IDKUNDRF-WIP TO MOD-PRARTNTO-FIX(INDX)                 
160900         ELSE                                                             
160901           IF ODEL-IDDC-EXP = WC-CDC-SE OR                                
160902              ODEL-IDDC-EXP = SPACE                                       
160903              PERFORM S03F-PRICE-TO-MOD                                   
160904           END-IF                                                         
160905           IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                           
160906              ODEL-IDDC-EXP NOT= SPACE                                    
160907              PERFORM S03G-PRICE-TO-MOD-VOR                               
160908           END-IF                                                         
161100         END-IF                                                           
161200*        CALL FELLOG                                                      
161300       END-IF                                                             
161400     END-IF                                                               
161500                                                                          
161600     MOVE SPAR-STATUS            TO   MOD-KDKOLSTA(INDX)                  
161700     IF QF01-ORAD-IDKUNDRF-RO NOT = '0000000   '                          
161800       IF QF01-ORAD-IDKUNDRF-RO NOT = WS-IDKUNDRF-KOLL                    
161900         MOVE QF01-ORAD-IDKUNDRF-RO TO IDKUNDRF-WS                        
162000         IF IDKUNDRF-WS-POS6-7 = SPACE                                    
162100           MOVE IDORDNR5-WS TO MOD-IDKUNDRF-URS(INDX)                     
162200         ELSE                                                             
162300           MOVE IDORDNR7-WS TO MOD-IDKUNDRF-URS(INDX)                     
162400         END-IF                                                           
162500         INSPECT MOD-IDKUNDRF-URS(INDX) REPLACING LEADING                 
162600                                        ZERO BY SPACE                     
162700       END-IF                                                             
162800     END-IF                                                               
162900                                                                          
163000                                                                          
163100     IF QF01-ORAD-IDLEVNR NOT = SPACE                                     
163200       MOVE QF01-ORAD-IDLEVNR    TO   MOD-IDLEVNR(INDX)                   
163300     END-IF                                                               
163400     .                                                                    
163500     EJECT                                                                
163600 S03A-RAKNA-KVBEART-DIST79 SECTION.                                       
163700                                                                          
163800     COMPUTE WS-PRARTNTO-LOC = QF01-ORAD-PRARTNTO-LOC                     
163900                             * QF01-ORAD-KVBEART-Q                        
164000     COMPUTE WS-PRARTNTO-LOCPREL = QF01-ORAD-PRARTNTO-LOCPREL             
164100                                 * QF01-ORAD-KVBEART-Q                    
164200     .                                                                    
164300     EJECT                                                                
164400 S03B-FLYTTA-NTO-LOC SECTION.                                             
164500                                                                          
164600     MOVE WS-PRARTNTO-LOC TO HELP-PRARTNTO                                
164800     IF MFS-SPLIT                                                         
164900       MOVE WS-IDKUNDRF-WIP TO MOD-PRARTNTO-FIX(INDX)                     
165000     ELSE                                                                 
165110       MOVE HELP-PRARTNTO    TO MOD-PRARTNTO(INDX)                        
165200     END-IF                                                               
165400     .                                                                    
165500     EJECT                                                                
165600 S03C-FLYTTA-NTO-LOCPREL SECTION.                                         
165700                                                                          
165800     MOVE WS-PRARTNTO-LOCPREL TO HELP-PRARTNTO                            
166010     IF HELP-PRARTNTO    < 1                                              
166110       MOVE 1       TO HELP-PRARTNTO                                      
166200     END-IF                                                               
166300     IF MFS-SPLIT                                                         
166400       MOVE WS-IDKUNDRF-WIP TO MOD-PRARTNTO-FIX(INDX)                     
166500     ELSE                                                                 
166610       MOVE HELP-PRARTNTO     TO MOD-PRARTNTO(INDX)                       
166700       MOVE '. *' TO MOD-PRARTNTO-FIX(INDX)(8:3)                          
166800     END-IF                                                               
166900**GS MOVE WS-MOD-PRIS     TO MOD-PRARTNTO(INDX)                           
167000     .                                                                    
167100     EJECT                                                                
167110 S03D-ADD-Q4-PRICE   SECTION.                                             
167111*    *NORMAL FLOW OR BOUNCE FLOW (NOT VOR)                                
167112                                                                          
167113     IF ODEL-IDDC-EXP    = DCS-IDDC                                       
167114         COMPUTE WS-PRARTNTO-NUM = QF01-ORAD-KVBEART-Q *                  
167115                                   QF01-ORAD-PRARTNTO                     
167116     ELSE                                                                 
167117       IF QF01-ORAD-PRAVCOST > 0                                          
167118         COMPUTE WS-PRAVCOST-NUM = QF01-ORAD-KVBEART-Q *                  
167119                                   QF01-ORAD-PRAVCOST                     
167120       ELSE                                                               
167121         COMPUTE WS-PRARTNTO-NUM = QF01-ORAD-KVBEART-Q *                  
167122                                   QF01-ORAD-PRARTNTO                     
167123       END-IF                                                             
167124     END-IF                                                               
167125     .                                                                    
167126     EJECT                                                                
167130                                                                          
167140 S03E-ADD-Q4-PRICE-VOR   SECTION.                                         
167150*    *BOUNCE FLOW VOR                                                     
167160                                                                          
167170     IF ODEL-IDDC-EXP    = DCS-IDDC                                       
167171         COMPUTE WS-PRAVCOST-NUM = QF01-ORAD-KVBEART-Q *                  
167172                                   QF01-ORAD-PRAVCOST                     
167191     ELSE                                                                 
167196         COMPUTE WS-PRARTNTO-NUM = QF01-ORAD-KVBEART-Q *                  
167197                                   QF01-ORAD-PRARTNTO                     
167199     END-IF                                                               
167200     .                                                                    
167201     EJECT                                                                
167202                                                                          
167203 S03F-PRICE-TO-MOD    SECTION.                                            
167204*    *NORMAL FLOW OR BOUNCE FLOW (NOT VOR)                                
167206                                                                          
167207     IF ODEL-IDDC-EXP    = DCS-IDDC                                       
167208        MOVE WS-PRARTNTO-NUM      TO MOD-PRARTNTO(INDX)                   
167209        MOVE QF01-ORAD-KDVALISO TO WS-KDVALISO                            
167210        MOVE WS-TEDDI             TO MOD-TEDDI                            
167211     ELSE                                                                 
167212       MOVE WS-PRAVCOST-NUM      TO WS-PRAVCOST                           
167213       IF WS-PRAVCOST > 0                                                 
167214          MOVE WS-PRAVCOST-NUM                                            
167215                      TO MOD-PRARTNTO(INDX)                               
167216          MOVE QF01-ORAD-KDVALISO TO WS-KDVALISO                          
167217          MOVE WS-TEDDI             TO MOD-TEDDI                          
167218       ELSE                                                               
167219          MOVE WS-PRARTNTO-NUM    TO MOD-PRARTNTO(INDX)                   
167221          MOVE WS-SEK             TO WS-KDVALISO                          
167222          MOVE WS-TEDDI           TO MOD-TEDDI                            
167223       END-IF                                                             
167224     END-IF                                                               
167225     .                                                                    
167226     EJECT                                                                
167227                                                                          
167228 S03G-PRICE-TO-MOD-VOR SECTION.                                           
167229*    *BOUNCE FLOW VOR                                                     
167230                                                                          
167231     IF ODEL-IDDC-EXP    = DCS-IDDC                                       
167232        MOVE WS-PRAVCOST-NUM      TO MOD-PRARTNTO(INDX)                   
167233        MOVE QF01-ORAD-KDVALISO TO WS-KDVALISO                            
167234        MOVE WS-TEDDI             TO MOD-TEDDI                            
167235     ELSE                                                                 
167242        MOVE WS-PRARTNTO-NUM    TO MOD-PRARTNTO(INDX)                     
167244        MOVE WS-SEK             TO WS-KDVALISO                            
167245        MOVE WS-TEDDI           TO MOD-TEDDI                              
167246     END-IF                                                               
167247     .                                                                    
167248     EJECT                                                                
167249                                                                          
167250 S05-FLYTTA-WDE611-TILL-MOD SECTION.                                      
167300                                                                          
167400     MOVE KKOLLI-KVLEVART TO  MOD-KVLEVART(INDX)                          
167500     MOVE KOLLI-IDKOLLI TO  MOD-IDKOLLI(INDX)                             
167600                                                                          
167700     IF KOLLI-KDKOLSTA = +0                                               
167800       MOVE 'U'         TO MOD-KDKOLSTA(INDX)                             
167900     ELSE                                                                 
168000       IF KOLLI-KDKOLSTA = +1                                             
168100         MOVE 'P'       TO MOD-KDKOLSTA(INDX)                             
168200       ELSE                                                               
168300         IF KOLLI-KDKOLSTA = +2 OR +3                                     
168400           MOVE 'L'     TO MOD-KDKOLSTA(INDX)                             
168500         ELSE                                                             
168600           IF KOLLI-KDKOLSTA = +4                                         
168700             MOVE 'LF'  TO MOD-KDKOLSTA(INDX)                             
168800           ELSE                                                           
168900             IF KOLLI-KDKOLSTA = +6 OR +7                                 
169000               MOVE 'F' TO MOD-KDKOLSTA(INDX)                             
169100             ELSE                                                         
169200               IF KOLLI-KDKOLSTA = +8 OR +9                               
169300                 MOVE 'FL' TO MOD-KDKOLSTA(INDX)                          
169400               END-IF                                                     
169500             END-IF                                                       
169600           END-IF                                                         
169700           IF KOLLI-KDKOLSTA = +6                                         
169800             MOVE 'SC' TO MOD-KDKOLSTA(INDX)                              
169900           ELSE                                                           
170000             IF KOLLI-KDKOLSTA = +7                                       
170100               MOVE 'S' TO MOD-KDKOLSTA(INDX)                             
170200             ELSE                                                         
170300               IF KOLLI-KDKOLSTA = +9                                     
170400                 MOVE 'SF' TO MOD-KDKOLSTA(INDX)                          
170500               END-IF                                                     
170600             END-IF                                                       
170700           END-IF                                                         
170701           IF ODEL-IDDC-EXP = WC-CDC-SE OR                                
170702              ODEL-IDDC-EXP = SPACE                                       
170710              PERFORM S05A-INVOICE                                        
170711           END-IF                                                         
170712           IF ODEL-IDDC-EXP NOT = WC-CDC-SE AND                           
170713              ODEL-IDDC-EXP NOT = SPACE                                   
170714              PERFORM S05B-INVOICE-VOR                                    
170715           END-IF                                                         
170900         END-IF                                                           
171000       END-IF                                                             
171100     END-IF                                                               
171200     .                                                                    
171300     EJECT                                                                
171310 S05A-INVOICE SECTION.                                                    
171311*   *NORMAL ORDER OR BOUNCE ORDER WITH BOUNCE DC=11                       
171312                                                                          
171313     IF ODEL-IDDC-EXP = DCS-IDDC                                          
171314       MOVE KOLLI-IDFAKT      TO MOD-IDFAKT(INDX)                         
171315     ELSE                                                                 
171316        IF KOLLI-IDFAKT-EXP > 0                                           
171317          MOVE KOLLI-IDFAKT-EXP TO MOD-IDFAKT(INDX)                       
171318          MOVE KOLLI-KDVALISO-EXP  TO WS-KDVALISO                         
171319          MOVE WS-TEDDI        TO MOD-TEDDI                               
171320        ELSE                                                              
171321          MOVE KOLLI-IDFAKT    TO MOD-IDFAKT(INDX)                        
171322          MOVE KOLLI-KDVALISO  TO WS-KDVALISO                             
171323          MOVE WS-TEDDI        TO MOD-TEDDI                               
171324        END-IF                                                            
171325     END-IF                                                               
171326     .                                                                    
171330     EJECT                                                                
171331 S05B-INVOICE-VOR SECTION.                                                
171332*   *BOUNCE ORDER WITH BOUNCE DC NOT 11                                   
171333                                                                          
171334     IF ODEL-IDDC-EXP = DCS-IDDC                                          
171336       MOVE KOLLI-IDFAKT-EXP    TO MOD-IDFAKT(INDX)                       
171337       MOVE KOLLI-KDVALISO-EXP  TO WS-KDVALISO                            
171338       MOVE WS-TEDDI            TO MOD-TEDDI                              
171340     ELSE                                                                 
171346       MOVE KOLLI-IDFAKT        TO MOD-IDFAKT(INDX)                       
171347       MOVE KOLLI-KDVALISO      TO WS-KDVALISO                            
171348       MOVE WS-TEDDI            TO MOD-TEDDI                              
171350     END-IF                                                               
171351     .                                                                    
171352     EJECT                                                                
171353 S04B-INVOICE-VOR SECTION.                                                
171354*   *BOUNCE ORDER WITH BOUNCE DC OTHER THAN 11                            
171360                                                                          
171370     IF ODEL-IDDC-EXP = DCS-IDDC                                          
171380       MOVE KOLLI-IDFAKT      TO MOD-IDFAKT(INDX)                         
171390     ELSE                                                                 
171391        IF KOLLI-IDFAKT-EXP > 0                                           
171392          MOVE KOLLI-IDFAKT-EXP TO MOD-IDFAKT(INDX)                       
171393          MOVE KOLLI-KDVALISO-EXP  TO WS-KDVALISO                         
171394          MOVE WS-TEDDI        TO MOD-TEDDI                               
171395        ELSE                                                              
171396          MOVE KOLLI-IDFAKT    TO MOD-IDFAKT(INDX)                        
171397          MOVE KOLLI-KDVALISO  TO WS-KDVALISO                             
171398          MOVE WS-TEDDI        TO MOD-TEDDI                               
171399        END-IF                                                            
171400     END-IF                                                               
171401     .                                                                    
171402     EJECT                                                                
171410 S07-LAES-WDQ401 SECTION.                                                 
171500     IF IDARTNR-IFYLLT                                                    
171600       PERFORM IMS-GU-ORQF-ORQF01-M-IDARTNR                               
171700     ELSE                                                                 
171800       PERFORM IMS-GU-ORQF-ORQF01                                         
171900     END-IF                                                               
172000     IF SEGMENT-FINNS                                                     
172100       PERFORM S07A-SPAR-WDQ401KY-ENTER                                   
172200       PERFORM S07B-SPAR-WDQ401KY-NEXT                                    
172300                                                                          
172400       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
172500         PERFORM S03-FLYTTA-WDQ401-TILL-MOD                               
172600         ADD +1 TO INDX                                                   
172700         IF IDARTNR-IFYLLT                                                
172800           PERFORM IMS-GN-ORQF-ORQF01-M-IDARTNR                           
172900         ELSE                                                             
173000           PERFORM IMS-GN-ORQF-ORQF01                                     
173100         END-IF                                                           
173200       END-PERFORM                                                        
173300                                                                          
173400       IF SEGMENT-FINNS                                                   
173420         IF OHUV-FLKLAR = 'J'                                             
173500           PERFORM S09-SPAR-WDQ301KY-NEXT                                 
173501         END-IF                                                           
173600         PERFORM S07B-SPAR-WDQ401KY-NEXT                                  
173700         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
173800         CALL WMEDKONV USING MED-WMEDAREA                                 
173900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
174000       END-IF                                                             
174100     ELSE                                                                 
174200       IF IDARTNR-IFYLLT                                                  
174300        IF ARTIKEL-SAKNAS                                                 
174400         MOVE INF-PART-MISSING TO MED-IDMFSINF                            
174500         CALL WMEDKONV USING MED-WMEDAREA                                 
174600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
174700        END-IF                                                            
174800       ELSE                                                               
174810         MOVE SPACE                  TO ODEL-KDVALISO                     
174900         MOVE INF-ORDERLINES-MISSING TO MED-IDMFSINF                      
175000         CALL WMEDKONV USING MED-WMEDAREA                                 
175100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
175200       END-IF                                                             
175300     END-IF                                                               
175400     .                                                                    
175500     EJECT                                                                
175600 S07A-SPAR-WDQ401KY-ENTER SECTION.                                        
175700                                                                          
175800     MOVE QF01-ORAD-IDORDER   TO  MOD-IDORDER-ENTER                       
175900     MOVE QF01-ORAD-IDDC      TO  MOD-IDDC-ENTER                          
176000     MOVE QF01-ORAD-ADLAGOMR  TO  MOD-ADLAGOMR-ENTER                      
176100     MOVE QF01-ORAD-ADGANG    TO  MOD-ADGANG-ENTER                        
176200     MOVE QF01-ORAD-ADPLATS   TO  MOD-ADPLATS-ENTER                       
176300     MOVE QF01-ORAD-IDARTNR   TO  MOD-IDARTNR-ENTER                       
176400     MOVE QF01-ORAD-IDLOPNR   TO  MOD-IDLOPNR-ENTER                       
176500     .                                                                    
176600     EJECT                                                                
176700 S07B-SPAR-WDQ401KY-NEXT SECTION.                                         
176800                                                                          
176900     MOVE QF01-ORAD-IDORDER   TO  MOD-IDORDER-NEXT                        
177000     MOVE QF01-ORAD-IDDC      TO  MOD-IDDC-NEXT                           
177100     MOVE QF01-ORAD-ADLAGOMR  TO  MOD-ADLAGOMR-NEXT                       
177200     MOVE QF01-ORAD-ADGANG    TO  MOD-ADGANG-NEXT                         
177300     MOVE QF01-ORAD-ADPLATS   TO  MOD-ADPLATS-NEXT                        
177400     MOVE QF01-ORAD-IDARTNR   TO  MOD-IDARTNR-NEXT                        
177500     MOVE QF01-ORAD-IDLOPNR   TO  MOD-IDLOPNR-NEXT                        
177600     .                                                                    
177700     EJECT                                                                
177800 S09-SPAR-WDQ301KY-NEXT SECTION.                                          
177900                                                                          
178000     MOVE ODEL-IDORDER      TO MOD-IDORDER-NEXT                           
178100     MOVE ODEL-IDDC         TO MOD-IDDC-NEXT                              
178200     MOVE ODEL-IDPRODNR     TO MOD-IDPRODNR-NEXT                          
178300     MOVE ODEL-IDPLKLST     TO MOD-IDPLKLST-NEXT                          
178400     .                                                                    
178500     EJECT                                                                
178600                                                                          
178700 S10-CHECK-EXPORT-ORDER SECTION.                                          
178800                                                                          
178810     PERFORM IMS-GU-ORQI-ORQI01-M-Q2CSEQ                                  
178820     IF SEGMENT-FINNS                                                     
178900       MOVE LOW-VALUE  TO W-ODEL-IDDC-MIN                                 
179000       MOVE HIGH-VALUE TO W-ODEL-IDDC-MAX                                 
179001       MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                            
179002                            W-ODEL-IDORDER-MAX                            
179100                                                                          
179200       PERFORM IMS-GU-ORQA-ORQA01                                         
179280                                                                          
179290       IF DCS-IDDC = ODEL-IDDC-EXP                                        
179291*        *EXPORT-ORDER WITH 2 INVOICES                                    
179294         MOVE ODEL-IDDC TO W-ORAD-IDDC-MIN                                
179295                           W-ORAD-IDDC-MAX                                
179296                           W-ORAD-IDDC-MIN-MIN                            
179297                           W-ORAD-IDDC-MAX-MAX                            
179298                           W-ODEL-IDDC-MIN                                
179299                           W-ODEL-IDDC-MAX                                
179300       ELSE                                                               
179301         MOVE DCS-IDDC  TO W-ODEL-IDDC-MIN                                
179302                           W-ODEL-IDDC-MAX                                
179303       END-IF                                                             
179304     END-IF                                                               
179305                                                                          
179310     .                                                                    
179400     EJECT                                                                
179500                                                                          
182500 MFS-RENSA-FAELT-UT SECTION.                                              
182600                                                                          
182700*    --- ALLA UTDATA-FÄLT                                                 
182800     MOVE +1 TO INDX                                                      
182900     PERFORM UNTIL INDX > MAX-INDX                                        
183000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                          
183100                                 MOD-KVBEART-Q(INDX)                      
183200                                 MOD-KVAVBART(INDX)                       
183300                                 MOD-KVLEVART(INDX)                       
183400                                 MOD-KDAVVIK(INDX)                        
183500                                 MOD-PRARTNTO(INDX)                       
183600                                 MOD-KDKOLSTA(INDX)                       
183700                                 MOD-IDKOLLI(INDX)                        
183800                                 MOD-KDFARLIG(INDX)                       
183900                                 MOD-IDFAKT(INDX)                         
184000                                 MOD-IDLEVNR(INDX)                        
184100                                 MOD-IDKUNDRF-URS(INDX)                   
184200       ADD +1 TO INDX                                                     
184300     END-PERFORM                                                          
184400     .                                                                    
184500     EJECT                                                                
184600* --- IMS SEKTIONER ---                                                   
184700     SKIP3                                                                
184800 IMS-GET-MSG SECTION.                                                     
184900                                                                          
185000     MOVE '  QC' TO GODK-STATUSKODER                                      
185100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
185200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
185300     PERFORM IMS-STATUSKONTROLL                                           
185400     .                                                                    
185500     SKIP3                                                                
185600 IMS-INSERT-MSG SECTION.                                                  
185700                                                                          
185800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
185900       MOVE '0' TO MFS-KDHUVOMR                                           
186000     END-IF                                                               
186100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
186200     MOVE SPACE TO GODK-STATUSKODER                                       
186300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
186400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
186500     PERFORM IMS-STATUSKONTROLL                                           
186600     .                                                                    
186700     EJECT                                                                
186800                                                                          
186900 IMS-GU-WDE401-ASEQ SECTION.                                              
187000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
187100          DELIMITED BY SIZE INTO SSA1                                     
187200     MOVE '  GE' TO GODK-STATUSKODER                                      
187300     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA-KORD SSA1                
187400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
187500     PERFORM IMS-STATUSKONTROLL                                           
187600     .                                                                    
187700     EJECT                                                                
187800 IMS-GN-WDE401-ASEQ SECTION.                                              
187900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
188000          DELIMITED BY SIZE INTO SSA1                                     
188100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
188200     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA-KORD SSA1                
188300     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
188400     PERFORM IMS-STATUSKONTROLL                                           
188500     .                                                                    
188600     EJECT                                                                
188700 IMS-GU-WDE411-BSEQ SECTION.                                              
188800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
188900                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
189000          DELIMITED BY SIZE INTO SSA1                                     
189100     MOVE '  GE' TO GODK-STATUSKODER                                      
189200     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA-WDE411 SSA1              
189300     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
189400     PERFORM IMS-STATUSKONTROLL                                           
189500     .                                                                    
189600     EJECT                                                                
189700 IMS-GN-WDE411-BSEQ SECTION.                                              
189800     STRING 'WDE411  (WDE4BSEQ >' W-WDE4BSEQ-MIN-X                        
189900                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
190000          DELIMITED BY SIZE INTO SSA1                                     
190100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
190200     CALL CBLTDLI USING GN WDE4B-PCB DLI-IO-AREA-WDE411 SSA1              
190300     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
190400     PERFORM IMS-STATUSKONTROLL                                           
190500     .                                                                    
190600     EJECT                                                                
190700 IMS-GNP-WDE421 SECTION.                                                  
190800     STRING 'WDE421  (WDE421KY =' W-WDE4KEY-X ')'                         
190900          DELIMITED BY SIZE INTO SSA1                                     
191000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
191100     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA-WDE421 SSA1             
191200     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
191300     PERFORM IMS-STATUSKONTROLL                                           
191400     .                                                                    
191500     EJECT                                                                
191600 IMS-GNP-WDE421-BKEY SECTION.                                             
191700     MOVE 'WDE421   ' TO SSA1                                             
191800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
191900     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA-WDE421 SSA1             
192000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
192100     PERFORM IMS-STATUSKONTROLL                                           
192200     .                                                                    
192300     EJECT                                                                
192400 IMS-GU-ORQA-ORQA01 SECTION.                                              
192500                                                                          
192600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
192700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
192800          DELIMITED BY SIZE INTO SSA1                                     
192900     MOVE '    GE' TO GODK-STATUSKODER                                    
193000     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
193100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     EJECT                                                                
193500 IMS-GN-ORQA-ORQA01 SECTION.                                              
193600                                                                          
193700     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
193800                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
194100     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
194200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     EJECT                                                                
194600 IMS-GU-ORQF-ORQF01 SECTION.                                              
194700     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-MIN-X                    
194800                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
195100     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
195200     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     EJECT                                                                
195600 IMS-GU-ORQF-ORQF01-M-IDARTNR SECTION.                                    
195700     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
195800                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
195900                    '&IDARTNR  =' W-ORAD-IDARTNR-X ')'                    
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
196200     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
196300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     EJECT                                                                
196700 IMS-GN-ORQF-ORQF01 SECTION.                                              
196800     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-MIN-X                    
196900                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
197000          DELIMITED BY SIZE INTO SSA1                                     
197100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
197200     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
197300     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
197400     PERFORM IMS-STATUSKONTROLL                                           
197500     .                                                                    
197600     EJECT                                                                
197700 IMS-GN-ORQF-ORQF01-M-IDARTNR SECTION.                                    
197800     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-X                        
197900                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
198000                    '&IDARTNR  =' W-ORAD-IDARTNR-X ')'                    
198100          DELIMITED BY SIZE INTO SSA1                                     
198200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
198300     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
198400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
198500     PERFORM IMS-STATUSKONTROLL                                           
198600     .                                                                    
198700     EJECT                                                                
198800 IMS-GU-ORQI-ORQI01-M-Q2CSEQ SECTION.                                     
198900                                                                          
199000     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
199100          DELIMITED BY SIZE INTO SSA1                                     
199200     MOVE '  GE' TO GODK-STATUSKODER                                      
199300     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
199400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
199500     PERFORM IMS-STATUSKONTROLL                                           
199600     .                                                                    
199700                                                                          
199800 IMS-GNP-ORQI-ORQI12 SECTION.                                             
199900     MOVE 'WLORQI12 ' TO SSA1                                             
200000     MOVE '  GE' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
200200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500     EJECT                                                                
200510 IMS-GU-WDE601 SECTION.                                                   
200520                                                                          
200530     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
200540          DELIMITED BY SIZE INTO    SSA1                                  
200550     MOVE    '  GE'           TO    GODK-STATUSKODER                      
200560     CALL    CBLTDLI          USING GU  WDE6-PCB DLI-IOAREA-WDE601        
200570                                         SSA1                             
200580     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
200590     PERFORM IMS-STATUSKONTROLL                                           
200591     .                                                                    
200592                                                                          
200600 IMS-GU-WDE611 SECTION.                                                   
200700                                                                          
200800       STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                      
200900            DELIMITED BY SIZE INTO SSA1                                   
201000       STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                       
201100            DELIMITED BY SIZE INTO SSA2                                   
201200       MOVE '  ' TO GODK-STATUSKODER                                      
201300       CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE611 SSA1 SSA2         
201400       MOVE WDE6-STATUS-CODE TO STATUS-WS                                 
201500       PERFORM IMS-STATUSKONTROLL                                         
201600         .                                                                
201700     EJECT                                                                
206500 IMS-GU-WDB601    SECTION.                                                
206600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
206700          DELIMITED BY SIZE INTO SSA1                                     
206800     MOVE '  GE' TO GODK-STATUSKODER                                      
206900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
207000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
207100     PERFORM IMS-STATUSKONTROLL                                           
207200     IF SEGMENT-SAKNAS                                                    
207300         MOVE SPACE TO DCS-KDDC                                           
207400     END-IF                                                               
207500     .                                                                    
207600 IMS-GU-WDF601    SECTION.                                                
207700     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-F6-X ')'                     
207800          DELIMITED BY SIZE INTO SSA1                                     
207900     MOVE '  GE' TO GODK-STATUSKODER                                      
208000     CALL CBLTDLI USING GU WDF6-PCB DLI-IO-AREA-F601 SSA1                 
208100     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208310 IMS-GNP-WDQ301  SECTION.                                                 
208320                                                                          
208321     MOVE 'WDQ301   ' TO SSA1                                             
208360     MOVE '    GE' TO GODK-STATUSKODER                                    
208370     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
208380     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
208390     PERFORM IMS-STATUSKONTROLL                                           
208391     .                                                                    
208392     EJECT                                                                
208400 IMS-STATUSKONTROLL SECTION.                                              
208500                                                                          
208600     SET STATUS-IX TO 1                                                   
208700     SEARCH GODK-STATUS                                                   
208800       AT END CALL FELLOG                                                 
208900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
209000     END-SEARCH                                                           
209100     .                                                                    
