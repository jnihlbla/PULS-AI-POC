000100******************************************************************        
000200*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0142      *        
000300******************************************************************        
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W4050300.                                                
000700 AUTHOR.         STEFANO GIOBBI.                                          
000800 DATE-WRITTEN.   90/11/19.                                                
000900                                                                          
001000     REMARKS.                                                             
001100*                                                                         
001200*    FUNKTION:                                                            
001300*      4503 ÄR ETT FRÅGE-MPP.                                             
001400*      PROGRAMMET VISAR, PER DISTRIKT, KUND, ORDER OCH CLAGER,            
001500*      ORDERVÄRDE OCH ORDERRADER FÖR ORDERN. FAKTURANR OCH FAK-           
001600*      TURADATUM VISAS OM SÅDANA FINNS, MAX ÄR 12 ST OCH DE LÄGGS         
001700*      UT I KRONOLOGISK ORDNING PÅ SKÄRMEN (DEN ÄLDSTA FÖRST).            
001800*      FÖR ORDERVÄRDE VISAS EJ STATUS R UTAN DENNA BAKAS IN I             
001900*      TOTALEN, FÖR ORDERRADERNA BAKAS STATUS R OCH U IN I TOT-           
002000*      ALEN. FÖR ANTAL RADER FÖR STATUS P, F OCH L ÄR DET ANTAL           
002100*      ORDERRADER I KOLLISEGMENTET (WDE611) SOM SUMMERAS. DESSA           
002200*      'KOLLIRADER' KAN VARA FLER ÄN TOTALT REGISTRERAT ANTAL             
002300*      ORDERRADER FÖR ORDERN.                                             
002400*                                                                         
002500*        REGLER FÖR SUMMERING AV ORDERRADER PÅ KOLLISEGMENTEN:            
002600*                                                                         
002700*        KOLLISTATUS  INNEBÖRD                SUMMERAS TILL STATUS        
002800*                                                                         
002900*          1          FÄRDIGPACKAT KOLLI        P                         
003000*          6          FAKTURABEORDRAT           P                         
003100*                                                                         
003200*          2          LASTBEORDRAT              L                         
003300*          3          LASTAT                    L                         
003400*          4          LASTAT O. FAKT.BEORDRAT   L                         
003500*                                                                         
003600*          7          FAKTURERAT                F                         
003700*          8          FAKTURERAT O. LASTBEORD.  F                         
003800*          9          FAKTURERAT OCH LASTAT     F                         
003900*                     (LASTAT OCH FAKTURERAT)                             
004000*                                                                         
004100*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
004200*                              WLORQA (WDQ3)                              
004300*                              WDE6                                       
004400*                                                                         
004500*    INDATA.                                                              
004600*        TRANSAKTION: W4T503                                              
004700*        MID:         W4I50301                                            
004800*                                                                         
004900*    UTDATA.                                                              
005000*        MOD:         W4O50301                                            
005100                                                                          
005200     SKIP3                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600 WORKING-STORAGE SECTION.                                                 
005700*    -COPY WY2000W1                                                       
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(08)   VALUE 'W4050300'.            
006000                                                                          
006100 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
006200                                                                          
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77  HJALP-MOD-IX                PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77  HJALP-MOD-IX-MAX-12         PIC S9(9)   VALUE +12  COMP SYNC.        
006900 77  Y2K-IX                      PIC S9(2)   VALUE +0   COMP SYNC.        
007000 77  RKOD-ABEND-MED-DUMP         PIC S9(5)   VALUE +1000                  
007100                                                        COMP SYNC.        
007200                                                                          
007300 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
007400 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
007500 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
007600                                                                          
007700 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
007800 77  WS-SEK                      PIC X(3)    VALUE 'SEK'.                 
007900                                                                          
008000 77  SW-SKALL-VARDEN-LAGGAS-UT   PIC X       VALUE 'N'.                   
008100 77  SW-ORDER-KLAR               PIC X       VALUE 'J'.                   
008200 77  SW-FAKT-INFO-TAB-SORTERAD   PIC X       VALUE 'J'.                   
008300                                                                          
008400 01  W-SPAR-IDKUNDRF.                                                     
008500     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
008600     03  FILLER                  PIC X(3)    VALUE '+++'.                 
008700                                                                          
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDCKONS                                                     
009100       EJECT                                                              
009200*                                                                         
009300 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
009400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
009500*    ----DISTR-DEALER-PRICE-----                                          
009600     EJECT                                                                
009700                                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000     88  NYCKLAR-FEL                         VALUE 'N'.                   
010100                                                                          
010200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010300     88  ALLT-OK                             VALUE 'J'.                   
010400                                                                          
010500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010600     88  EGEN-MID                            VALUE '4503'.                
010700     88  GODK-MID                            VALUE '4501' '4502'          
010800                                            '4503' '4504' '4505'          
010900                                            '4506' '4507' '4508'          
011000                                            '4509'.                       
011100                                                                          
011200 77  AVERAGECOST-SW             PIC X       VALUE 'J'.                    
011300     88  AVERAGECOST                         VALUE 'J'.                   
011400                                                                          
011500 77  BOUNCE-SW                  PIC X       VALUE 'J'.                    
011600     88  BOUNCEORDER                         VALUE 'J'.                   
011700     EJECT                                                                
011800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011900 01  GENERELLA-SUBPROGRAM.                                                
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
012600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012900*01 -COPY WMSGINIT                                                        
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
013200*   -COPY WSECAREA                                                        
013300     EJECT                                                                
013400*                                                                         
013500 01  FELMEDDELANDE-AREA.                                                  
013600     03  FILLER                  PIC X(16)   VALUE 'FELMEDD AREA'.        
013700     03  FELM-ORADER-SAKNAS-F-DC-028                                      
013800                                 PIC X(3)           VALUE '028'.          
013900     03  FELM-ORDER-ANNULLERAD-052                                        
014000                                 PIC X(3)           VALUE '052'.          
014100     03  FELM-ORDER-EJ-AVSLUTAD-053                                       
014200                                 PIC X(3)           VALUE '053'.          
014300     03  FELM-ORDER-KLAR-082     PIC X(3)           VALUE '082'.          
014400     03  FELM-NYCKEL-FEL-401     PIC X(3)           VALUE '401'.          
014500     03  FELM-UPPLYSTA-FALT-FEL-409                                       
014600                                 PIC X(3)           VALUE '409'.          
014700     03  FELM-ORDERINFO-BORTTAGEN-415                                     
014800                                 PIC X(3)           VALUE '415'.          
014900     03  FELM-ORDER-SAKNAS-701   PIC X(3)           VALUE '701'.          
015000     SKIP2                                                                
015100*                                                                         
015200 01  ABEND-TEXT-AREA.                                                     
015300     03  FILLER                  PIC X(16)   VALUE                        
015400                                                 'ABENDTEXT AREA'.        
015500     03  ABTXT-E611-SEG-SAKNAS   PIC X(64)   VALUE                        
015600         'WDE611-SEG (KOLLI-SEG) SAKNAS FÖR LASTAD ELLER FAKTURERA        
015700-        'D ORDER.'.                                                      
015800     EJECT                                                                
015900*                                                                         
016000 01  KONSTANT-AREA.                                                       
016100     03  FILLER                  PIC  X(16) VALUE 'KONSTANT AREA'.        
016200     03  K-KDODELSTA-R           PIC  X(1)         VALUE 'R'.             
016300     03  K-KDODELSTA-U           PIC  X(1)         VALUE 'U'.             
016400     03  K-KDODELSTA-P           PIC  X(1)         VALUE 'P'.             
016500     03  K-KDORDSTA-2-PACKRAPP-STARTAD                                    
016600                                 PIC S9(1)         VALUE +2.              
016700     03  K-KDORDSTA-3-VORD-FARDIGPACKAD                                   
016800                                 PIC S9(1)         VALUE +3.              
016900     03  K-KDKOLSTA-1-FARDIG-PACKAT                                       
017000                                 PIC S9(1)  COMP-3 VALUE +1.              
017100     03  K-KDKOLSTA-2-LASTREL    PIC S9(1)  COMP-3 VALUE +2.              
017200     03  K-KDKOLSTA-3-LASTAT     PIC S9(1)  COMP-3 VALUE +3.              
017300     03  K-KDKOLSTA-4-LASTAT-FAKTREL                                      
017400                                 PIC S9(1)  COMP-3 VALUE +4.              
017500     03  K-KDKOLSTA-6-FAKTREL    PIC S9(1)  COMP-3 VALUE +6.              
017600     03  K-KDKOLSTA-7-FAKTURERAT PIC S9(1)  COMP-3 VALUE +7.              
017700     03  K-KDKOLSTA-8-FAKT-LASTREL                                        
017800                                 PIC S9(1)  COMP-3 VALUE +8.              
017900     03  K-KDKOLSTA-9-FAKT-LASTAT                                         
018000                                 PIC S9(1)  COMP-3 VALUE +9.              
018100     03  K-KDTRPKAT-A            PIC  X(1)         VALUE 'A'.             
018200     03  K-IDKUNDNR-UT-NOLL      PIC  X(6)         VALUE '     0'.        
018400     EJECT                                                                
018500*                                                                         
018600 01  SPAR-AREA.                                                           
018700     03  FILLER                  PIC X(16)  VALUE 'SPAR AREA'.            
018800     03  SPAR-KVRADER-TOT        PIC S9(5)   COMP-3 VALUE +0.             
018900     03  SPAR-KVRADER-P          PIC S9(5)   COMP-3 VALUE +0.             
019000     03  SPAR-KVRADER-F          PIC S9(5)   COMP-3 VALUE +0.             
019100     03  SPAR-KVRADER-L          PIC S9(5)   COMP-3 VALUE +0.             
019200     03  SPAR-SUORDV-TOT         PIC S9(9)V9(2)                           
019300                                             COMP-3 VALUE +0.             
019400     03  SPAR-SUORDV-TOT-LOC     PIC S9(9)V9(2)                           
019500                                             COMP-3 VALUE +0.             
019600     03  SPAR-SUORDV-TOT-LOCPREL PIC S9(9)V9(2)                           
019700                                             COMP-3 VALUE +0.             
019800     03  SPAR-SUORDV-U           PIC S9(9)V9(2)                           
019900                                             COMP-3 VALUE +0.             
020000     03  SPAR-SUORDV-U-LOC       PIC S9(9)V9(2)                           
020100                                             COMP-3 VALUE +0.             
020200     03  SPAR-SUORDV-U-LOCPREL   PIC S9(9)V9(2)                           
020300                                             COMP-3 VALUE +0.             
020400     03  SPAR-SUORDV-P           PIC S9(9)V9(2)                           
020500                                             COMP-3 VALUE +0.             
020600     03  SPAR-SUORDV-P-LOC       PIC S9(9)V9(2)                           
020700                                             COMP-3 VALUE +0.             
020800     03  SPAR-SUORDV-P-LOCPREL   PIC S9(9)V9(2)                           
020900                                             COMP-3 VALUE +0.             
021000     03  SPAR-SUORDV-F           PIC S9(9)V9(2)                           
021100                                             COMP-3 VALUE +0.             
021200     03  SPAR-SUORDV-F-LOC      PIC S9(9)V9(2)                            
021300                                             COMP-3 VALUE +0.             
021400     03  SPAR-SUORDV-F-LOCPREL   PIC S9(9)V9(2)                           
021500                                             COMP-3 VALUE +0.             
021600     03  SPAR-SUORDV-L           PIC S9(9)V9(2)                           
021700                                             COMP-3 VALUE +0.             
021800     03  SPAR-SUORDV-L-LOC       PIC S9(9)V9(2)                           
021900                                             COMP-3 VALUE +0.             
022000     03  SPAR-SUORDV-L-LOCPREL   PIC S9(9)V9(2)                           
022100                                             COMP-3 VALUE +0.             
022200*                                                                         
022300     03  SPAR-ARB-IDDC           PIC X(2)           VALUE SPACE.          
022400     03  SPAR-ARB-TIRFS          PIC S9(11)  COMP-3 VALUE +0.             
022500     03  SPAR-ARB-KDTRPKAT       PIC S9(11)  COMP-3 VALUE +0.             
022600     03  SPAR-ODEL-IDPRODNR      PIC S9(7)   COMP-3 VALUE +0.             
022700     03  SPAR-KDVALISO           PIC X(3)    VALUE SPACE.                 
022800     EJECT                                                                
022900*                                                                         
023000 01  HELP-AREA.                                                           
023100     03  FILLER                  PIC X(16)      VALUE 'HELP AREA'.        
023200     03  HELP-KVRADER-OPACK      PIC  9(5)          VALUE ZERO.           
023300     03  HELP-SUORDV-OPACK       PIC  9(9)V9(2)     VALUE ZERO.           
023400     03  HELP-SUMMA              PIC S9(9)V9(2) COMP-3 VALUE +0.          
023500     03  HELP-IDKAMPRF           PIC  9(7)          VALUE ZERO.           
023600     03  HELP-TIREGDAT           PIC  9(7)          VALUE ZERO.           
023700     03  HELP-TIRFS              PIC  9(11)         VALUE ZERO.           
023800     03  FILLER             REDEFINES HELP-TIRFS.                         
023900         05  FILLER              PIC  9(1).                               
024000         05  HELP-TIRFS-AAMMDD   PIC  9(6).                               
024100         05  HELP-TIRFS-HHMM     PIC  9(4).                               
024200     03  HELP-TIAAMMDD-TRP       PIC  9(6)          VALUE ZERO.           
024300     03  HELP-TIHHMM-TRP         PIC  9(4)          VALUE ZERO.           
024400     03  HELP-DIRLEV-TIRFS       PIC  9(7)          VALUE ZERO.           
024500     03  FILLER             REDEFINES HELP-DIRLEV-TIRFS.                  
024600         05  HELP-FILLER         PIC  9(1).                               
024700         05  HELP-DIRLEV-RFS     PIC  9(6).                               
024800                                                                          
024900 01  WS-TID-X.                                                            
025000     03  WS-TID-N                PIC 9(5).                                
025100 01  WS-TID-RED-X.                                                        
025200     03  WS-TID-HH               PIC X(2).                                
025300     03  WS-TID-PKT              PIC X(1).                                
025400     03  WS-TID-MM               PIC X(2).                                
025500                                                                          
025600* ARBETSVARIABLER FÖR REG.TID START                                       
025700 01  WS-RTID-X.                                                           
025800     03  WS-RTID-N                PIC 9(7).                               
025900 01  WS-RTID-RED-X.                                                       
026000     03  WS-RTID-HH               PIC X(2).                               
026100     03  WS-RTID-PKT              PIC X(1).                               
026200     03  WS-RTID-MM               PIC X(2).                               
026300*                                                                         
026400* ARBETSVARIABLER FÖR REG.TID STOPP                                       
026500 01  WS-RTIDS-X.                                                          
026600     03  WS-RTIDS-N                PIC 9(7).                              
026700 01  WS-RTIDS-RED-X.                                                      
026800     03  WS-RTIDS-HH               PIC X(2).                              
026900     03  WS-RTIDS-PKT              PIC X(1).                              
027000     03  WS-RTIDS-MM               PIC X(2).                              
027100*                                                                         
027200 01  TAB-FAKT-HJALP-AREA.                                                 
027300     03  TAB-FAKT-IX-MAX-12      PIC S9(9)   COMP   VALUE +12.            
027400     03  TAB-FAKT-IX-MIN-1       PIC S9(9)   COMP   VALUE +1.             
027500     03  TAB-FAKT-VERKLIGT-ANTAL PIC S9(9)   COMP   VALUE +0.             
027600     03  TAB-FAKT-WINTSOR-STEG-LANGD                                      
027700                                 PIC S9(9)   COMP   VALUE +13.            
027800     03  TAB-FAKT-WINTSOR-ANTAL  PIC S9(9)   COMP   VALUE +12.            
027900     03  TAB-FAKT-WINTSOR-SORT-LANGD                                      
028000                                 PIC S9(9)   COMP   VALUE +13.            
028100*                                                                         
028200*                       ASCENDING KEY IS TAB-FAKT-TIFAKT                  
028300*                                                                         
028400 01  TABELL-FAKTURA-INFO.                                                 
028500     03  TAB-FAKT-ELEM  OCCURS           12 TIMES                         
028600                        INDEXED BY       TAB-FAKT-IX.                     
028700                                                                          
028800         05  TAB-FAKT-TIFAKT     PIC  9(6).                               
028900         05  TAB-FAKT-IDFAKT     PIC  9(7).                               
029000     EJECT                                                                
029100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029200     SKIP3                                                                
029300*   -COPY WMEDAREA                                                        
029400     EJECT                                                                
029500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029600*                                                                         
029700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029800     SKIP3                                                                
029900*01  MID -COPY W4I50301                                                   
030000     EJECT                                                                
030100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
030200     SKIP3                                                                
030300*01  -COPY WMSGAREA                                                       
030400     EJECT                                                                
030500     03  MOD REDEFINES MSG-AREA.                                          
030600*      05  -COPY W4O50301                                                 
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030900     SKIP3                                                                
031000*01  -COPY WMFSAREA                                                       
031100     EJECT                                                                
031200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031300*                                                                         
031400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031500     SKIP3                                                                
031600 01  NYCKLAR-TILL-DLI.                                                    
031700     03  W-WDQ2CSEQ-MIN-X.                                                
031800         05  W-WDQ2C-IDDISTR-MIN  PIC S9(5)   COMP-3 VALUE +0.            
031900         05  W-WDQ2C-IDKUNDNR-MIN PIC S9(7)   COMP-3 VALUE +0.            
032000         05  W-WDQ2C-IDKUNDRF-MIN.                                        
032100             07  W-WDQ2C-IDORDNR7-MIN                                     
032200                                  PIC  9(7)          VALUE ZERO.          
032300             07  FILLER           PIC  X(3)          VALUE SPACE.         
032400*                                                                         
032500     03  W-WDQ2CSEQ-MAX-X.                                                
032600         05  W-WDQ2C-IDDISTR-MAX  PIC S9(5)   COMP-3 VALUE +0.            
032700         05  W-WDQ2C-IDKUNDNR-MAX PIC S9(7)   COMP-3 VALUE +0.            
032800         05  W-WDQ2C-IDKUNDRF-MAX.                                        
032900             07  W-WDQ2C-IDORDNR7-MAX                                     
033000                                  PIC  9(7)          VALUE ZERO.          
033100             07  FILLER           PIC  X(3)          VALUE SPACE.         
033200*                                                                         
033300     03  W-WDQ211KY-MIN-X.                                                
033400         05  W-WDQ211-IDDC-MIN    PIC  X(2)          VALUE SPACES.        
033500         05  W-WDQ211-IDLEVNR-MIN PIC  X(5)          VALUE SPACES.        
033600*                                                                         
033700     03  W-WDQ211KY-MAX-X.                                                
033800         05  W-WDQ211-IDDC-MAX    PIC  X(2)          VALUE SPACES.        
033900         05  W-WDQ211-IDLEVNR-MAX PIC  X(5)          VALUE SPACES.        
034000*                                                                         
034100     03  W-IDDC-X.                                                        
034200         05  W-WDQ2-IDDC          PIC X(2)           VALUE SPACE.         
034300*                                                                         
034400     03  W-WDQ301KY-MIN-X.                                                
034500         05  W-WDQ3-MIN-IDORDER   PIC S9(7)   COMP-3 VALUE +0.            
034600         05  W-WDQ3-MIN-IDDC      PIC X(2)           VALUE SPACE.         
034700         05  W-WDQ3-MIN-IDPRODNR  PIC S9(7)   COMP-3 VALUE +0.            
034800         05  W-WDQ3-MIN-IDPLKLST  PIC S9(3)   COMP-3 VALUE +0.            
034900*                                                                         
035000     03  W-WDQ301KY-MAX-X.                                                
035100         05  W-WDQ3-MAX-IDORDER   PIC S9(7)   COMP-3 VALUE +0.            
035200         05  W-WDQ3-MAX-IDDC      PIC X(2)           VALUE SPACE.         
035300         05  W-WDQ3-MAX-IDPRODNR  PIC S9(7)   COMP-3 VALUE +0.            
035400         05  W-WDQ3-MAX-IDPLKLST  PIC S9(3)   COMP-3 VALUE +0.            
035500*                                                                         
035600     03  W-IDPRODNR-X.                                                    
035700         05  W-WDE6-IDPRODNR      PIC S9(7)   COMP-3 VALUE +0.            
035800*                                                                         
035900     03  W-IDDC-B6-X.                                                     
036000         05 W-IDDC-B6                  PIC X(2).                          
037000                                                                          
038000     EJECT                                                                
038100*    --- STATUS-KOD FRÅN IMS                                              
038200 01  STATUS-WS                    PIC XX.                                 
038300     88  SEGMENT-FINNS                       VALUE '  '.                  
038400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
038500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038600     88  ANNAT-SEGMENT                       VALUE 'GK'.                  
038700     SKIP2                                                                
038800 01  GODK-STATUSKODER.                                                    
038900     03  GODK-STATUS OCCURS  5  TIMES                                     
039000                     INDEXED BY STATUS-IX                                 
039100                                  PIC X(2).                               
039200     SKIP3                                                                
039300 01  SSA1                         PIC X(128).                             
039400 01  SSA2                         PIC X(128).                             
039500     EJECT                                                                
039600*    --- IMS FUNKTIONSKODER                                               
039700*01  -COPY W0003                                                          
039800     EJECT                                                                
039900******************************************************************        
040000*                                                                *        
040100*        ARBETS-AREOR TILL IO-AREORNA                            *        
040200*                                                                *        
040300*        DLI INPUT-OUTPUT AREA                                   *        
040400*                                                                *        
040500******************************************************************        
040600*    ---  DLI INPUT-OUTPUT                                                
040700*    ---  DLI-IO-AREA                                                     
040800*                                                                         
040900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI01'.           
041000 01  DLI-IO-WLORQI01.                                                     
041100*    03  WLORQI01   -COPY WDQ201                                          
041200     EJECT                                                                
041300                                                                          
041400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI11'.           
041500 01  DLI-IO-WLORQI11.                                                     
041600*    03  WLORQI11   -COPY WDQ211                                          
041700     EJECT                                                                
041800                                                                          
041900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI12'.           
042000 01  DLI-IO-WLORQI12.                                                     
042100*    03  WLORQI12   -COPY WDQ212                                          
042200     EJECT                                                                
042300                                                                          
042400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI21'.           
042500 01  DLI-IO-WLORQI21.                                                     
042600*    03  WLORQI21   -COPY WDQ221                                          
042700     EJECT                                                                
042800                                                                          
042900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQA01'.           
043000 01  DLI-IO-WLORQA01.                                                     
043100*    03  WLORQA01   -COPY WDQ301                                          
043200     EJECT                                                                
043300                                                                          
043400 01  FILLER                  PIC X(16)   VALUE 'MLI-IO-WDE601'.           
043500 01  DLI-IO-WDE601.                                                       
043600*    03             -COPY WDE601                                          
043700     EJECT                                                                
043800                                                                          
043900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE611'.           
044000 01  DLI-IO-WDE611.                                                       
044100*    03             -COPY WDE611                                          
044200     EJECT                                                                
044300                                                                          
044400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
044500 01   DLI-IO-AREA-B601.                                                   
045000*     03  -COPY WDB601                                                    
046000                                                                          
046100     EJECT                                                                
046200 LINKAGE SECTION.                                                         
046300                                                                          
046400*01  -COPY W0009      -PRE MSG-                                           
046500     EJECT                                                                
046600*01  -COPY W0008      -PRE USEA-                                          
046700     05  FILLER                  PIC X.                                   
046800     EJECT                                                                
046900*01  -COPY W0008      -PRE ORQI-                                          
047000     05  FILLER                  PIC X.                                   
047100     EJECT                                                                
047200*01  -COPY W0008      -PRE ORQA-                                          
047300     05  FILLER                  PIC X.                                   
047400     EJECT                                                                
047500*01  -COPY W0008      -PRE WDE6-                                          
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
047800*01  -COPY W0008      -PRE WDB6-                                          
047900     05  FILLER                  PIC X.                                   
048000     EJECT                                                                
049000 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
049100                           ORQI-PCB                                       
049200                           ORQA-PCB  WDE6-PCB                             
049300                           WDB6-PCB.                                      
049400     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
049500                           ORQI-PCB                                       
049600                           ORQA-PCB  WDE6-PCB                             
049700                           WDB6-PCB.                                      
049800                                                                          
049900     PERFORM IMS-GET-MSG                                                  
050000     IF SEGMENT-FINNS                                                     
050100       PERFORM A-INIT                                                     
050200       PERFORM B-KOLLA-NYCKLAR                                            
050300       IF NYCKLAR-OK                                                      
050400         PERFORM C-KOLLA-BEHORIGHET                                       
050500         PERFORM F-LAES-VISA-INFO                                         
050600       END-IF                                                             
050700       PERFORM IMS-INSERT-MSG                                             
050800     END-IF                                                               
050900                                                                          
051000     MOVE ZERO TO RETURN-CODE                                             
051100     GOBACK                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 A-INIT SECTION.                                                          
051500                                                                          
051600     IF MSG-DUBBLA-TRANSKODER                                             
051700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50301                 
051800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
051900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
052000     ELSE                                                                 
052100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I50301                 
052200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
052300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
052400     END-IF                                                               
052500                                                                          
052600     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
052700     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
052800     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
052900                                                                          
053000     MOVE LOW-VALUE                       TO MSG-AREA                     
053100     MOVE 'W4O503N1'                      TO MFS-IDMOD                    
053200     MOVE '4503'                          TO MOD-IDTRANS                  
053300     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
053400                                             MOD-TEMFSINF                 
053500     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O50301 + 4                        
053600                                                                          
053700     IF NOT EGEN-MID                                                      
053800       MOVE SPACE                         TO MFS-KDTRTYP                  
053900       MOVE '7'                           TO MFS-IDPFK                    
054000     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 B-KOLLA-NYCKLAR SECTION.                                                 
055400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
055500     MOVE '001'             TO MSGI-KDCALL                                
055600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
055700     MOVE '4503'            TO MSGI-IDTRANS                               
055800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
055900     IF EGEN-MID                                                          
056000        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
056100        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
056200        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
056300        IF MID-IDORDNR7-IN      NOT = ALL '+'                             
056400           MOVE MID-IDORDNR7-IN TO W-SPAR-IDORDNR7                        
056500           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
056600        END-IF                                                            
056700        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
056800        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
056900     END-IF                                                               
057000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
057100     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
057200                                                                          
057300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
057400                                                                          
057500     MOVE    JA               TO    NYCKLAR-SW                            
057600     MOVE    LOW-VALUE        TO    W-WDQ301KY-MIN-X                      
057700                                    W-WDQ2CSEQ-MIN-X                      
057800                                    W-WDQ211KY-MIN-X                      
057900     MOVE    HIGH-VALUE       TO    W-WDQ301KY-MAX-X                      
058000                                    W-WDQ2CSEQ-MAX-X                      
058100                                    W-WDQ211KY-MAX-X                      
058200                                                                          
058300     MOVE    MFS-RENSA-FAELT  TO    MOD-IDDISTR-IN                        
058400                                    MOD-IDKUNDNR-IN                       
058500                                    MOD-IDORDNR7-IN                       
058600                                    MOD-IDDC-IN                           
058700                                    MOD-IDARTNR-IN                        
058800                                    MOD-IDKOLLI-IN                        
058900                                    MOD-IDPRODNR-IN                       
059000                                                                          
059100     PERFORM BA-KONTROLLERA-IDDISTR                                       
059200     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
059300     PERFORM BC-KONTROLLERA-IDKUNDRF                                      
059400     PERFORM BD-KONTROLLERA-IDDC                                          
059500     PERFORM BE-KONTROLLERA-IDARTNR                                       
059600     PERFORM BF-KONTROLLERA-IDKOLLI                                       
059700     PERFORM BG-KONTROLLERA-IDPRODNR                                      
059800                                                                          
059900     IF NYCKLAR-FEL                                                       
060000       MOVE    FELM-NYCKEL-FEL-401 TO    MED-IDMFSFEL                     
060100       CALL    WMEDKONV            USING MED-WMEDAREA                     
060200       MOVE    MED-MFSFEL          TO    MOD-TEMFSFEL                     
060300       PERFORM MFS-RENSA-FAELT-UT                                         
060400                                                                          
060500       IF NOT GODK-MID                                                    
060600         MOVE MFS-RENSA-FAELT      TO    MOD-IDDISTR-UT                   
060700                                         MOD-IDKUNDNR-UT                  
060800                                         MOD-IDORDNR7-UT                  
060900                                         MOD-IDARTNR-UT                   
061000                                         MOD-IDKOLLI-UT                   
061100                                         MOD-IDPRODNR-UT                  
061200       END-IF                                                             
061300     END-IF                                                               
061400                                                                          
061500     IF GODK-MID                                                          
061600        CONTINUE                                                          
061700     ELSE                                                                 
061800       MOVE MFS-RENSA-FAELT      TO        MOD-IDARTNR-UT                 
061900                                           MOD-IDKOLLI-UT                 
062000                                           MOD-IDPRODNR-UT                
062100     END-IF                                                               
062200                                                                          
062300     .                                                                    
062400     EJECT                                                                
062500 BA-KONTROLLERA-IDDISTR SECTION.                                          
062600                                                                          
062700     IF MID-IDDISTR-IN         NOT  = ALL '+'                             
062800       MOVE    '7'             TO        MFS-IDPFK                        
062900       MOVE    SPACE           TO        MFS-KDTRTYP                      
063000     END-IF                                                               
063100                                                                          
063200     MOVE      MSGI-IDDISTR      TO        MOD-IDDISTR-UT                 
063300     INSPECT   MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
063400                                                                          
063500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
063600       MOVE    MSGI-IDDISTR      TO        W-WDQ2C-IDDISTR-MIN            
063700                                           W-WDQ2C-IDDISTR-MAX            
063800                                           TEST-IDDISTR                   
063900     ELSE                                                                 
064000       MOVE    NEJ               TO        NYCKLAR-SW                     
064100     END-IF                                                               
064200                                                                          
064300     MOVE SPACE                  TO        MOD-TEDDI                      
064400                                                                          
064500     .                                                                    
064600     EJECT                                                                
064700 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
064800                                                                          
064900     IF MID-IDKUNDNR-IN           NOT = ALL '+'                           
065000       MOVE    '7'                TO        MFS-IDPFK                     
066000       MOVE    SPACE              TO        MFS-KDTRTYP                   
066100     END-IF                                                               
066200                                                                          
066300     IF MSGI-IDKUNDNR = ZERO                                              
066400       MOVE    K-IDKUNDNR-UT-NOLL TO        MOD-IDKUNDNR-UT               
066500     ELSE                                                                 
066600       MOVE    MSGI-IDKUNDNR        TO        MOD-IDKUNDNR-UT             
066700       INSPECT MOD-IDKUNDNR-UT    REPLACING LEADING ZERO BY SPACE         
066800     END-IF                                                               
066900                                                                          
067000     IF MSGI-IDKUNDNR NUMERIC                                             
067100       MOVE    MSGI-IDKUNDNR      TO        W-WDQ2C-IDKUNDNR-MIN          
067200                                            W-WDQ2C-IDKUNDNR-MAX          
067300     ELSE                                                                 
067400       MOVE    NEJ                TO        NYCKLAR-SW                    
067500     END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 BC-KONTROLLERA-IDKUNDRF SECTION.                                         
067900                                                                          
068000     IF MID-IDORDNR7-IN         NOT = ALL '+'                             
068100       MOVE    '7'              TO        MFS-IDPFK                       
068200       MOVE    SPACE            TO        MFS-KDTRTYP                     
068300     END-IF                                                               
068400                                                                          
068500     MOVE      MSGI-IDKUNDRF(1:7) TO        MOD-IDORDNR7-UT               
068600     INSPECT   MOD-IDORDNR7-UT  REPLACING LEADING ZERO BY SPACE           
068700                                                                          
068800     IF MSGI-IDKUNDRF(1:7)      NUMERIC AND                               
068900        MSGI-IDKUNDRF(1:7)      > ZERO                                    
069000                                                                          
069100       MOVE    SPACE            TO        W-WDQ2C-IDKUNDRF-MIN            
069200                                          W-WDQ2C-IDKUNDRF-MAX            
069300       MOVE    MSGI-IDKUNDRF(1:7) TO     W-WDQ2C-IDORDNR7-MIN             
069400                                          W-WDQ2C-IDORDNR7-MAX            
069500     ELSE                                                                 
069600       MOVE    NEJ              TO        NYCKLAR-SW                      
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 BD-KONTROLLERA-IDDC SECTION.                                             
070100                                                                          
070200     IF EGEN-MID                                                          
070300        IF MID-IDDC-IN     = ALL '+'                                      
070400          MOVE MID-IDDC-UT TO W-IDDC-B6                                   
070500        ELSE                                                              
070600          MOVE MID-IDDC-IN TO W-IDDC-B6                                   
070700          MOVE '7'        TO MFS-IDPFK                                    
070800          MOVE SPACE      TO MFS-KDTRTYP                                  
070900        END-IF                                                            
071000     ELSE                                                                 
071100        MOVE MSGI-IDDC       TO W-IDDC-B6                                 
071200     END-IF                                                               
071300     PERFORM IMS-GU-WDB601                                                
071400                                                                          
071500     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
071600       MOVE   NEJ         TO NYCKLAR-SW                                   
071700     ELSE                                                                 
071800       IF DCS-DDC                                                         
071900         MOVE WC-CDC-SE   TO W-WDQ2-IDDC                                  
072000         MOVE DCS-IDDC    TO W-WDQ211-IDDC-MIN                            
072100                             W-WDQ211-IDDC-MAX                            
072200       ELSE                                                               
072300         MOVE DCS-IDDC    TO W-WDQ2-IDDC                                  
072400       END-IF                                                             
072500       MOVE DCS-IDDC      TO W-WDQ3-MIN-IDDC                              
072600                             W-WDQ3-MAX-IDDC                              
072700     END-IF                                                               
072800                                                                          
072900     MOVE W-IDDC-B6       TO MOD-IDDC-UT                                  
073000     .                                                                    
073100     EJECT                                                                
073200 BE-KONTROLLERA-IDARTNR SECTION.                                          
073300                                                                          
073400     IF MID-IDARTNR-IN = ALL '+'                                          
073500       MOVE MID-IDARTNR-UT TO MOD-IDARTNR-UT                              
073600     ELSE                                                                 
073700       MOVE MID-IDARTNR-IN TO MOD-IDARTNR-UT                              
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 BF-KONTROLLERA-IDKOLLI SECTION.                                          
074200                                                                          
074300     IF MID-IDKOLLI-IN = ALL '+'                                          
074400       MOVE MID-IDKOLLI-UT TO MOD-IDKOLLI-UT                              
074500     ELSE                                                                 
074600       MOVE MID-IDKOLLI-IN TO MOD-IDKOLLI-UT                              
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000 BG-KONTROLLERA-IDPRODNR SECTION.                                         
075100                                                                          
075200     IF MID-IDPRODNR-IN = ALL '+'                                         
075300       MOVE MID-IDPRODNR-UT TO MOD-IDPRODNR-UT                            
075400     ELSE                                                                 
075500       MOVE MID-IDPRODNR-IN TO MOD-IDPRODNR-UT                            
075600     END-IF                                                               
075700     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
075800     .                                                                    
075900     EJECT                                                                
076000 C-KOLLA-BEHORIGHET SECTION.                                              
076100                                                                          
076200     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
076300     MOVE '4503'            TO    SEC-IDTRANS                             
076400     MOVE MSGI-IDDISTR      TO    SEC-IDKEY                               
076500                                                                          
076600     CALL WSECURIT          USING SEC-IDUSER                              
076700                                  SEC-IDTRANS                             
076800                                  SEC-IDKEY                               
076900                                  SEC-KDSVAR                              
077000                                                                          
077100     .                                                                    
077200     EJECT                                                                
077300 F-LAES-VISA-INFO SECTION.                                                
077400                                                                          
077500     PERFORM FA-INITIERA-TABELLER                                         
077600     PERFORM FB-LAS-ORDERHUVUD                                            
077700                                                                          
077800     IF ALLT-OK                                                           
077900       PERFORM FC-LAS-ARBTAB                                              
078000                                                                          
078100       IF ALLT-OK           AND                                           
078200          SPAR-ARB-TIRFS = ZERO                                           
078300         PERFORM FD-LAS-DIRLEV                                            
078400         PERFORM FE-FYLL-MOD-UTAN-WDQ3-E6-INFO                            
078500       ELSE                                                               
078600                                                                          
078700         IF ALLT-OK           AND                                         
078800            SPAR-ARB-TIRFS > ZERO                                         
078900                                                                          
079000           PERFORM FF-LAS-ORDERDELAR                                      
079100                                                                          
079200         END-IF                                                           
079300       END-IF                                                             
079400       IF ALLT-OK                                                         
079500         MOVE SPAR-KDVALISO TO MOD-KDVALISO                               
079600       END-IF                                                             
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000 FA-INITIERA-TABELLER SECTION.                                            
080100                                                                          
080200     SET TAB-FAKT-IX TO +1                                                
080300                                                                          
080400     PERFORM UNTIL TAB-FAKT-IX > TAB-FAKT-IX-MAX-12                       
080500       MOVE ALL '9'     TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)                   
080600                           TAB-FAKT-TIFAKT(TAB-FAKT-IX)                   
080700       SET  TAB-FAKT-IX UP BY +1                                          
080800     END-PERFORM                                                          
080900     .                                                                    
081000     EJECT                                                                
081100 FB-LAS-ORDERHUVUD SECTION.                                               
081200                                                                          
081300     PERFORM IMS-GET-WDQ201-CSEQ                                          
081400                                                                          
081500     IF SEGMENT-SAKNAS                                                    
081600        MOVE NEJ                   TO ALLT-SW                             
081700        MOVE FELM-ORDER-SAKNAS-701 TO MED-IDMFSFEL                        
081800     ELSE                                                                 
081900       IF OHUV-FLBORT = JA                                                
082000         PERFORM IMS-GNP-WDQ212-OKVAL                                     
082100         IF SEGMENT-SAKNAS                                                
082200            MOVE NEJ                       TO ALLT-SW                     
082300            MOVE FELM-ORDERINFO-BORTTAGEN-415  TO MED-IDMFSFEL            
082400         ELSE                                                             
082500            MOVE NEJ                       TO ALLT-SW                     
082600            MOVE FELM-ORDER-ANNULLERAD-052 TO MED-IDMFSFEL                
082700         END-IF                                                           
082800       ELSE                                                               
082900         IF OHUV-KDTPOTYP > ZERO                                          
083000           MOVE NEJ                   TO ALLT-SW                          
083100           MOVE FELM-ORDER-SAKNAS-701 TO MED-IDMFSFEL                     
083200         END-IF                                                           
083300       END-IF                                                             
083400     END-IF                                                               
083500                                                                          
083600     IF ALLT-OK                                                           
083700       IF OHUV-FLKLAR = NEJ                                               
083800         MOVE FELM-ORDER-EJ-AVSLUTAD-053 TO    MED-IDMFSINF               
083900         CALL WMEDKONV                   USING MED-WMEDAREA               
084000         MOVE MED-MFSINF                 TO    MOD-TEMFSINF               
084100       END-IF                                                             
084200       PERFORM FBA-FYLL-MOD-MED-OHUV-INFO                                 
084300     ELSE                                                                 
084400       CALL    WMEDKONV   USING MED-WMEDAREA                              
084500       MOVE    MED-MFSFEL TO    MOD-TEMFSFEL                              
084600       PERFORM MFS-RENSA-FAELT-UT                                         
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 FBA-FYLL-MOD-MED-OHUV-INFO SECTION.                                      
085100                                                                          
085200     MOVE OHUV-BEKUNDRF     TO MOD-BEKUNDRF                               
085300     MOVE OHUV-IDKAMPRF     TO HELP-IDKAMPRF                              
085400     MOVE HELP-IDKAMPRF     TO MOD-IDKAMPRF                               
085500     MOVE OHUV-TIREGDAT     TO HELP-TIREGDAT                              
085600     MOVE HELP-TIREGDAT     TO MOD-TIREGDAT                               
085700     MOVE OHUV-TIREGDAT-STO TO MOD-TIREGDAT-STO                           
085800     MOVE OHUV-TIREPDAT     TO MOD-TIREPDAT                               
085900     MOVE OHUV-KDORDTYP-LDC TO MOD-KDORDTYP-LDC                           
086000                                                                          
086100     MOVE OHUV-TIREGTID  TO WS-RTID-N                                     
086200     MOVE WS-RTID-X(2:2) TO WS-RTID-HH                                    
086300     MOVE WS-RTID-X(4:2) TO WS-RTID-MM                                    
086400     MOVE '.'            TO WS-RTID-PKT                                   
086500     MOVE WS-RTID-RED-X  TO MOD-TIHHMM-REG                                
086600                                                                          
086700     MOVE OHUV-TIREGTID-STO   TO WS-RTIDS-N                               
086800     MOVE WS-RTIDS-X(2:2)     TO WS-RTIDS-HH                              
086900     MOVE WS-RTIDS-X(4:2)     TO WS-RTIDS-MM                              
087000     MOVE '.'                 TO WS-RTIDS-PKT                             
088000     MOVE WS-RTIDS-RED-X      TO MOD-TIHHMM-REG-STO                       
088100     .                                                                    
088200     EJECT                                                                
088300 FC-LAS-ARBTAB SECTION.                                                   
088400                                                                          
088500     MOVE OHUV-IDORDER     TO W-WDQ3-MIN-IDORDER                          
088600                              W-WDQ3-MAX-IDORDER                          
088700     MOVE LOW-VALUE        TO W-WDQ3-MIN-IDDC                             
088800     MOVE HIGH-VALUE       TO W-WDQ3-MAX-IDDC                             
088900     PERFORM IMS-GN-WDQ301-INTERV                                         
089000     IF ODEL-IDDC-EXP = DCS-IDDC                                          
089100        MOVE ODEL-IDPRODNR TO W-WDE6-IDPRODNR                             
089200        PERFORM IMS-GET-WDE601                                            
089300        IF SEGMENT-FINNS AND                                              
089400           VORD-KVKOLLI-FAKT > 0                                          
089500*          *BOUNCE-DC WILL NOT SEE ORDERLINES BEFORE                      
089600*          *SOME LINES ARE INVOICED                                       
089700           MOVE ODEL-IDDC  TO W-WDQ2-IDDC                                 
089800        ELSE                                                              
089900           MOVE DCS-IDDC   TO W-WDQ3-MIN-IDDC                             
090000                              W-WDQ3-MAX-IDDC                             
090100        END-IF                                                            
090200     ELSE                                                                 
090300        MOVE DCS-IDDC      TO W-WDQ3-MIN-IDDC                             
090400                              W-WDQ3-MAX-IDDC                             
090500     END-IF                                                               
090600                                                                          
090700     PERFORM IMS-GNP-WDQ212                                               
090800     IF SEGMENT-FINNS                                                     
090900       PERFORM FCA-FYLL-MOD-MED-ARB-TAB-INFO                              
091000       MOVE    ARB-KDTRPKAT TO SPAR-ARB-KDTRPKAT                          
091100       MOVE    ARB-TIRFS    TO SPAR-ARB-TIRFS                             
091200       MOVE    ARB-IDDC     TO SPAR-ARB-IDDC                              
091300       IF ARB-TIRFS = ZERO                                                
091400         PERFORM FCB-SUMMERA-FRAN-WDQ221                                  
091500       END-IF                                                             
091600     ELSE                                                                 
091700       MOVE    NEJ                         TO    ALLT-SW                  
091800       MOVE    FELM-ORADER-SAKNAS-F-DC-028 TO    MED-IDMFSFEL             
091900       CALL    WMEDKONV                    USING MED-WMEDAREA             
092000       MOVE    MED-MFSFEL                  TO    MOD-TEMFSFEL             
092100       PERFORM MFS-RENSA-FAELT-UT                                         
092200       MOVE    SPACE                       TO    SPAR-KDVALISO            
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092600 FCA-FYLL-MOD-MED-ARB-TAB-INFO SECTION.                                   
092700                                                                          
092800*    MOVE ARB-TIRFS             TO MOD-TIAAMMDD-RFS                       
092900*    MOVE ARB-IDTRP             TO MOD-IDTRP                              
093000*    MOVE ARB-TIAAMMDD          TO MOD-TIAAMMDD-TRP                       
093100     MOVE ARB-TIRFS             TO HELP-TIRFS                             
093200     MOVE HELP-TIRFS-AAMMDD     TO MOD-TIAAMMDD-RFS                       
093300     MOVE ARB-IDTRP             TO MOD-IDTRP                              
093400     MOVE ARB-DATRPAVD (3:6)    TO HELP-TIAAMMDD-TRP                      
093500     MOVE HELP-TIAAMMDD-TRP     TO MOD-TIAAMMDD-TRP                       
093600                                                                          
093700     MOVE ARB-TIHHMM            TO WS-TID-N                               
093800     MOVE WS-TID-X(2:2)         TO WS-TID-HH                              
093900     MOVE WS-TID-X(4:2)         TO WS-TID-MM                              
094000     MOVE '.'                   TO WS-TID-PKT                             
094100     MOVE WS-TID-RED-X          TO MOD-TIHHMM-TRP                         
094200                                                                          
094300     IF DCS-DDC                                                           
094400        PERFORM FCAA-GET-DATE-DIR-LEV                                     
094500     END-IF                                                               
094600     .                                                                    
094700     EJECT                                                                
094800 FCAA-GET-DATE-DIR-LEV SECTION.                                           
094900                                                                          
095000     PERFORM IMS-GNPF-WDQ211-QUAL                                         
095100                                                                          
095200     IF SEGMENT-FINNS                                                     
095300        MOVE DIRL-TISKEPPN-DDC     TO HELP-DIRLEV-TIRFS                   
095400        MOVE HELP-DIRLEV-RFS       TO MOD-TIAAMMDD-RFS                    
095500                                      MOD-TIAAMMDD-TRP                    
095600        MOVE DIRL-IDLEVNR          TO MOD-IDLEVNR                         
095700        MOVE SPACES                TO MOD-IDTRP                           
095800                                                                          
095900        MOVE '18'                  TO WS-TID-HH                           
096000        MOVE '00'                  TO WS-TID-MM                           
096100        MOVE '.'                   TO WS-TID-PKT                          
096200        MOVE WS-TID-RED-X          TO MOD-TIHHMM-TRP                      
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 FCB-SUMMERA-FRAN-WDQ221 SECTION.                                         
096700                                                                          
096901       PERFORM IMS-GNP-WDQ221                                             
097000                                                                          
097100       PERFORM UNTIL SEGMENT-SAKNAS                                       
098000                                                                          
201000          IF LOR-SUORDV > ZERO                                            
202000            COMPUTE SPAR-SUORDV-TOT  =   SPAR-SUORDV-TOT                  
203000                                + LOR-SUORDV                              
204000            MOVE LOR-KDVALISO   TO SPAR-KDVALISO                          
205000          END-IF                                                          
206000          IF LOR-SUORDV-LOC > ZERO                                        
207000            COMPUTE SPAR-SUORDV-TOT-LOC  =   SPAR-SUORDV-TOT-LOC          
208000                                + LOR-SUORDV-LOC                          
209000          END-IF                                                          
209100          IF LOR-SUORDV-LOCPREL > ZERO                                    
209200            COMPUTE SPAR-SUORDV-TOT-LOCPREL =                             
209300                                  SPAR-SUORDV-TOT-LOCPREL                 
209400                                + LOR-SUORDV-LOCPREL                      
209500          END-IF                                                          
209600          IF LOR-KVRADER        > ZERO                                    
209700            COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                 
209800                                + LOR-KVRADER                             
209900          END-IF                                                          
210000          PERFORM IMS-GNP-WDQ221                                          
220000                                                                          
220100       END-PERFORM                                                        
222700     .                                                                    
222800     EJECT                                                                
222900 FD-LAS-DIRLEV SECTION.                                                   
223000                                                                          
223100     PERFORM IMS-GNP-FIRST-WDQ211                                         
223200     PERFORM UNTIL SEGMENT-SAKNAS                                         
223300                                                                          
223400       IF DIRL-SUORDV  > ZERO                                             
223500         COMPUTE SPAR-SUORDV-TOT  = SPAR-SUORDV-TOT                       
223600                                  + DIRL-SUORDV                           
223700       END-IF                                                             
223800       IF DIRL-SUORDV-LOC  > ZERO                                         
223900         COMPUTE SPAR-SUORDV-TOT-LOC  = SPAR-SUORDV-TOT-LOC               
224000                                  + DIRL-SUORDV-LOC                       
224100       END-IF                                                             
224200       IF DIRL-SUORDV-LOCPREL  > ZERO                                     
224300         COMPUTE SPAR-SUORDV-TOT-LOCPREL                                  
224400                                      = SPAR-SUORDV-TOT-LOCPREL           
224500                                  + DIRL-SUORDV-LOCPREL                   
224600       END-IF                                                             
224700       IF DIRL-KVRADER > ZERO                                             
224800         COMPUTE SPAR-KVRADER-TOT = SPAR-KVRADER-TOT                      
224900                                  + DIRL-KVRADER                          
225000       END-IF                                                             
225100       PERFORM IMS-GNP-WDQ211                                             
225200                                                                          
225300     END-PERFORM                                                          
225400     .                                                                    
225500     EJECT                                                                
225600 FE-FYLL-MOD-UTAN-WDQ3-E6-INFO SECTION.                                   
225700                                                                          
225800     IF DIST79-DEALER-PRICE OR                                            
226000        DIST79-ECOM-PRICE                                                 
226100       COMPUTE HELP-SUMMA = SPAR-SUORDV-TOT-LOC +                         
226200                            SPAR-SUORDV-TOT-LOCPREL                       
226300       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
226400       IF SPAR-SUORDV-TOT-LOCPREL = +0                                    
226500         MOVE ' '                TO MOD-ASTERIX1                          
226600       ELSE                                                               
226700         MOVE '*'                TO MOD-ASTERIX1                          
226800       END-IF                                                             
226900     ELSE                                                                 
227000       MOVE SPAR-SUORDV-TOT      TO HELP-SUORDV-OPACK                     
227100       MOVE ' '                  TO MOD-ASTERIX1                          
227200     END-IF                                                               
227300     MOVE HELP-SUORDV-OPACK      TO MOD-SUORDV-TOT                        
227400     MOVE SPAR-KVRADER-TOT       TO HELP-KVRADER-OPACK                    
227500     MOVE HELP-KVRADER-OPACK     TO MOD-KVRADER-TOT                       
227600     MOVE ZERO                   TO MOD-SUORDV-U                          
227700                                    MOD-SUORDV-P                          
227800                                    MOD-SUORDV-F                          
227900                                    MOD-SUORDV-L                          
228000                                    MOD-KVRADER-P                         
228100                                    MOD-KVRADER-F                         
228200                                    MOD-KVRADER-L                         
228300     MOVE ' '                    TO MOD-ASTERIX2                          
228400                                    MOD-ASTERIX3                          
228500                                    MOD-ASTERIX4                          
228600                                    MOD-ASTERIX5                          
228700     .                                                                    
228800     EJECT                                                                
228900 FF-LAS-ORDERDELAR SECTION.                                               
229000                                                                          
229100     MOVE    OHUV-IDORDER TO W-WDQ3-MIN-IDORDER                           
229200                             W-WDQ3-MAX-IDORDER                           
229300                                                                          
229400     PERFORM IMS-GU-WDQ301-FIRST                                          
229500     MOVE    NEJ          TO SW-FAKT-INFO-TAB-SORTERAD                    
229600                                                                          
229700     PERFORM UNTIL SEGMENT-SAKNAS                                         
229800       IF ODEL-IDDC-EXP NOT = SPACE                                       
229900          MOVE JA         TO BOUNCE-SW                                    
230000       ELSE                                                               
230100          MOVE NEJ        TO BOUNCE-SW                                    
230200       END-IF                                                             
230300       IF ODEL-KDODELSTA = K-KDODELSTA-R                                  
230400         PERFORM FFA-SUM-VARDE-RADER-STATUS-R                             
230500         MOVE    NEJ TO SW-ORDER-KLAR                                     
230600         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
230700       ELSE                                                               
230800         IF ODEL-KDODELSTA = K-KDODELSTA-U                                
230900           MOVE    NEJ TO SW-ORDER-KLAR                                   
231000           PERFORM FFB-SUMMERA-STATUS-U-P-L-F                             
231100         ELSE                                                             
231200           IF ODEL-KDODELSTA = K-KDODELSTA-P                              
231300             PERFORM FFC-SUMMERA-STATUS-P-L-F                             
231400           END-IF                                                         
231500         END-IF                                                           
231600       END-IF                                                             
231700                                                                          
231800       PERFORM IMS-GN-WDQ301-INTERV                                       
231900     END-PERFORM                                                          
232000                                                                          
232100     IF SW-SKALL-VARDEN-LAGGAS-UT = JA                                    
232200       PERFORM FFE-FYLL-MOD-MED-TOTALER                                   
232300       PERFORM FFF-FYLL-MOD-MED-FAKT-INFO                                 
232400                                                                          
232500       IF SW-ORDER-KLAR = JA                                              
232600         MOVE FELM-ORDER-KLAR-082 TO    MED-IDMFSINF                      
232700         CALL WMEDKONV            USING MED-WMEDAREA                      
232800         MOVE MED-MFSINF          TO    MOD-TEMFSINF                      
232900       END-IF                                                             
233000     ELSE                                                                 
233100       MOVE    FELM-ORADER-SAKNAS-F-DC-028 TO    MED-IDMFSFEL             
233200       CALL    WMEDKONV                    USING MED-WMEDAREA             
233300       MOVE    MED-MFSFEL                  TO    MOD-TEMFSFEL             
233400       PERFORM MFS-RENSA-FAELT-UT                                         
233500     END-IF                                                               
233600     .                                                                    
233700     EJECT                                                                
233800 FFA-SUM-VARDE-RADER-STATUS-R SECTION.                                    
233900                                                                          
234000     IF ODEL-SUORDV  > ZERO                                               
234100       COMPUTE SPAR-SUORDV-TOT  =   SPAR-SUORDV-TOT                       
234200                                  + ODEL-SUORDV                           
234300     END-IF                                                               
234400     IF ODEL-SUORDV-LOC  > ZERO                                           
234500       COMPUTE SPAR-SUORDV-TOT-LOC = SPAR-SUORDV-TOT-LOC                  
234600                                   + ODEL-SUORDV-LOC                      
234700     END-IF                                                               
234800     IF ODEL-SUORDV-LOCPREL  > ZERO                                       
234900       COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL          
235000                                   + ODEL-SUORDV-LOCPREL                  
235100     END-IF                                                               
235200     IF ODEL-IDDC-EXP = SPACE                                             
235300        MOVE ODEL-KDVALISO          TO SPAR-KDVALISO                      
235400     ELSE                                                                 
235500        IF ODEL-IDDC-EXP = WC-CDC-SE                                      
235600           MOVE ODEL-KDVALISO       TO SPAR-KDVALISO                      
235700        ELSE                                                              
235800           MOVE WS-SEK              TO SPAR-KDVALISO                      
235900        END-IF                                                            
236000     END-IF                                                               
236100     IF ODEL-KVRADER > ZERO                                               
236200       COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                      
236300                                  + ODEL-KVRADER                          
236400     END-IF                                                               
236500     .                                                                    
236600     EJECT                                                                
236700 FFB-SUMMERA-STATUS-U-P-L-F SECTION.                                      
236800                                                                          
236900     IF ODEL-IDPRODNR NOT = SPAR-ODEL-IDPRODNR                            
237000       MOVE    ODEL-IDPRODNR TO W-WDE6-IDPRODNR                           
237100                                SPAR-ODEL-IDPRODNR                        
237200       PERFORM IMS-GET-WDE601                                             
237300       IF VORD-IDDC-EXP NOT = SPACE                                       
237400          IF VORD-IDDC-EXP = WC-CDC-SE                                    
237500*           *BOUNCE DC = 11                                               
237600            IF VORD-IDDC = W-IDDC-B6                                      
237700               MOVE JA  TO AVERAGECOST-SW                                 
237800            ELSE                                                          
237900               MOVE NEJ TO AVERAGECOST-SW                                 
238000            END-IF                                                        
238100          ELSE                                                            
238200*           *BOUNCE DC NOT 11                                             
238300            IF VORD-IDDC = W-IDDC-B6                                      
238400               MOVE NEJ TO AVERAGECOST-SW                                 
238500            ELSE                                                          
238600               MOVE JA  TO AVERAGECOST-SW                                 
238700            END-IF                                                        
238800          END-IF                                                          
238900       ELSE                                                               
239000          IF VORD-SUORDV-EXP > 0                                          
239100             MOVE JA  TO AVERAGECOST-SW                                   
239200          ELSE                                                            
239300             MOVE NEJ TO AVERAGECOST-SW                                   
239400          END-IF                                                          
239500       END-IF                                                             
239600                                                                          
239700       IF SEGMENT-FINNS                                                   
239800         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
239900         PERFORM FFBA-SUMMERA-VARDEN-FRAN-E601                            
240000         IF VORD-KVORDRAD-PACK > ZERO                                     
240100           PERFORM IMS-GNP-WDE611                                         
240200           PERFORM UNTIL NOT SEGMENT-FINNS                                
240300                                                                          
240400             IF KOLLI-KDKOLSTA = K-KDKOLSTA-1-FARDIG-PACKAT OR            
240500                                 K-KDKOLSTA-6-FAKTREL                     
240600               PERFORM FFBB-SUM-VARDE-RADER-STAT-P                        
240700               IF KOLLI-KDKOLSTA = K-KDKOLSTA-6-FAKTREL                   
240800                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
240900               END-IF                                                     
241000             ELSE                                                         
241100               IF KOLLI-KDKOLSTA = K-KDKOLSTA-2-LASTREL     OR            
241200                                   K-KDKOLSTA-3-LASTAT      OR            
241300                                   K-KDKOLSTA-4-LASTAT-FAKTREL            
241400                 PERFORM FFBC-SUM-VARDE-RADER-STATUS-L                    
241500                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-4-LASTAT-FAKTREL          
241600                   PERFORM S02-SPARA-FAKT-INFO-I-TABELL                   
241700                 END-IF                                                   
241800               ELSE                                                       
241900                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT   OR         
242000                                     K-KDKOLSTA-8-FAKT-LASTREL OR         
242100                                     K-KDKOLSTA-9-FAKT-LASTAT             
242200                   PERFORM FFBD-SUM-VARDE-RADER-STATUS-F                  
242300                   PERFORM S02-SPARA-FAKT-INFO-I-TABELL                   
242400                 END-IF                                                   
242500               END-IF                                                     
242600             END-IF                                                       
242700             PERFORM IMS-GNP-WDE611                                       
242800           END-PERFORM                                                    
242900         END-IF                                                           
243000       END-IF                                                             
243100     END-IF                                                               
243200     .                                                                    
243300     EJECT                                                                
243400 FFBA-SUMMERA-VARDEN-FRAN-E601 SECTION.                                   
243500                                                                          
243600     COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                        
243700                                + VORD-KVORDRAD                           
243800     IF AVERAGECOST                                                       
243900        COMPUTE SPAR-SUORDV-U =   SPAR-SUORDV-U                           
244000                                + VORD-SUORDV-EXP                         
244100     ELSE                                                                 
244200        COMPUTE SPAR-SUORDV-U =   SPAR-SUORDV-U                           
244300                                + VORD-SUORDV                             
244400     END-IF                                                               
244500     COMPUTE SPAR-SUORDV-U-LOC =   SPAR-SUORDV-U-LOC                      
244600                                + VORD-SUORDV-LOC                         
244700     COMPUTE SPAR-SUORDV-U-LOCPREL  =  SPAR-SUORDV-U-LOCPREL              
244800                                + VORD-SUORDV-LOCPREL                     
244900     IF AVERAGECOST                                                       
245000        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
245100                                + VORD-SUORDV-EXP                         
245200     ELSE                                                                 
245300        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
245400                                + VORD-SUORDV                             
245500     END-IF                                                               
245600     COMPUTE SPAR-SUORDV-TOT-LOC =   SPAR-SUORDV-TOT-LOC                  
245700                                + VORD-SUORDV-LOC                         
245800     COMPUTE SPAR-SUORDV-TOT-LOCPREL = SPAR-SUORDV-TOT-LOCPREL            
245900                                + VORD-SUORDV-LOCPREL                     
246000     IF AVERAGECOST                                                       
246100        IF VORD-KDVALISO-EXP NOT = SPACE                                  
246200          MOVE VORD-KDVALISO-EXP TO SPAR-KDVALISO                         
246300        END-IF                                                            
246400     ELSE                                                                 
246500        IF VORD-KDVALISO NOT = SPACE                                      
246600          MOVE VORD-KDVALISO   TO SPAR-KDVALISO                           
246700        END-IF                                                            
246800     END-IF                                                               
246900     .                                                                    
247000     EJECT                                                                
247100 FFBB-SUM-VARDE-RADER-STAT-P SECTION.                                     
247200                                                                          
247300     IF KOLLI-KVORDRAD     > ZERO                                         
247400       COMPUTE SPAR-KVRADER-P =   SPAR-KVRADER-P                          
247500                                + KOLLI-KVORDRAD                          
247600     END-IF                                                               
247700     IF AVERAGECOST                                                       
247800        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
247900          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
248000                                   + KOLLI-SUORDV-KLI-EXP                 
248100          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
248200                                   - KOLLI-SUORDV-KLI-EXP                 
248300        END-IF                                                            
248400     ELSE                                                                 
248500        IF KOLLI-SUORDV-KOLLI > ZERO                                      
248600          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
248700                                   + KOLLI-SUORDV-KOLLI                   
248800          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
248900                                   - KOLLI-SUORDV-KOLLI                   
249000        END-IF                                                            
249100     END-IF                                                               
249200     IF KOLLI-SUORDV-LOC     > ZERO                                       
249300       COMPUTE SPAR-SUORDV-P-LOC  =  SPAR-SUORDV-P-LOC                    
249400                                + KOLLI-SUORDV-LOC                        
249500       COMPUTE SPAR-SUORDV-U-LOC  =  SPAR-SUORDV-U-LOC                    
249600                                - KOLLI-SUORDV-LOC                        
249700     END-IF                                                               
249800     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
249900       COMPUTE SPAR-SUORDV-P-LOCPREL = SPAR-SUORDV-P-LOCPREL              
250000                                + KOLLI-SUORDV-LOCPREL                    
250100       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
250200                                - KOLLI-SUORDV-LOCPREL                    
250300     END-IF                                                               
250400     IF AVERAGECOST                                                       
250500        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
250600          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
250700        END-IF                                                            
250800     ELSE                                                                 
250900        IF KOLLI-KDVALISO NOT = SPACE                                     
251000          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
251100        END-IF                                                            
251200     END-IF                                                               
251300     .                                                                    
251400     EJECT                                                                
251500                                                                          
251600 FFBC-SUM-VARDE-RADER-STATUS-L SECTION.                                   
251700                                                                          
251800     IF KOLLI-KVORDRAD     > ZERO                                         
251900       COMPUTE SPAR-KVRADER-L =   SPAR-KVRADER-L                          
252000                                + KOLLI-KVORDRAD                          
252100     END-IF                                                               
252200     IF AVERAGECOST                                                       
252300        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
252400          COMPUTE SPAR-SUORDV-L = SPAR-SUORDV-L                           
252500                                   + KOLLI-SUORDV-KLI-EXP                 
252600          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
252700                                   - KOLLI-SUORDV-KLI-EXP                 
252800        END-IF                                                            
252900     ELSE                                                                 
253000        IF KOLLI-SUORDV-KOLLI > ZERO                                      
253100          COMPUTE SPAR-SUORDV-L = SPAR-SUORDV-L                           
253200                                   + KOLLI-SUORDV-KOLLI                   
253300          COMPUTE SPAR-SUORDV-U = SPAR-SUORDV-U                           
253400                                   - KOLLI-SUORDV-KOLLI                   
253500        END-IF                                                            
253600     END-IF                                                               
253700     IF KOLLI-SUORDV-LOC      > ZERO                                      
253800       COMPUTE SPAR-SUORDV-L-LOC  =   SPAR-SUORDV-L-LOC                   
253900                                + KOLLI-SUORDV-LOC                        
254000       COMPUTE SPAR-SUORDV-U-LOC  =   SPAR-SUORDV-U-LOC                   
254100                                - KOLLI-SUORDV-LOC                        
254200     END-IF                                                               
254300     IF KOLLI-SUORDV-LOCPREL  > ZERO                                      
254400       COMPUTE SPAR-SUORDV-L-LOCPREL = SPAR-SUORDV-L-LOCPREL              
254500                                + KOLLI-SUORDV-LOCPREL                    
254600       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
254700                                - KOLLI-SUORDV-LOCPREL                    
254800     END-IF                                                               
254900     IF AVERAGECOST                                                       
255000        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
255100          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
255200        END-IF                                                            
255300     ELSE                                                                 
255400        IF KOLLI-KDVALISO NOT = SPACE                                     
255500          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
255600        END-IF                                                            
255700     END-IF                                                               
255800     .                                                                    
255900     EJECT                                                                
256000 FFBD-SUM-VARDE-RADER-STATUS-F SECTION.                                   
256100                                                                          
256200     IF KOLLI-KVORDRAD     > ZERO                                         
256300       COMPUTE SPAR-KVRADER-F =   SPAR-KVRADER-F                          
256400                                + KOLLI-KVORDRAD                          
256500     END-IF                                                               
256600     IF AVERAGECOST                                                       
256700       IF KOLLI-SUORDV-KLI-EXP > ZERO                                     
256800         COMPUTE SPAR-SUORDV-F =  SPAR-SUORDV-F                           
256900                                  + KOLLI-SUORDV-KLI-EXP                  
257000         COMPUTE SPAR-SUORDV-U =  SPAR-SUORDV-U                           
257100                                  - KOLLI-SUORDV-KLI-EXP                  
257200       END-IF                                                             
257300     ELSE                                                                 
257400       IF KOLLI-SUORDV-KOLLI   > ZERO                                     
257500         COMPUTE SPAR-SUORDV-F =  SPAR-SUORDV-F                           
257600                                  + KOLLI-SUORDV-KOLLI                    
257700         COMPUTE SPAR-SUORDV-U =  SPAR-SUORDV-U                           
257800                                  - KOLLI-SUORDV-KOLLI                    
257900       END-IF                                                             
258000     END-IF                                                               
258100     IF KOLLI-SUORDV-LOC       > ZERO                                     
258200       COMPUTE SPAR-SUORDV-F-LOC = SPAR-SUORDV-F-LOC                      
258300                                + KOLLI-SUORDV-LOC                        
258400       COMPUTE SPAR-SUORDV-U-LOC = SPAR-SUORDV-U-LOC                      
258500                                - KOLLI-SUORDV-LOC                        
258600     END-IF                                                               
258700     IF KOLLI-SUORDV-LOCPREL   > ZERO                                     
258800       COMPUTE SPAR-SUORDV-F-LOCPREL = SPAR-SUORDV-F-LOCPREL              
258900                                + KOLLI-SUORDV-LOCPREL                    
259000       COMPUTE SPAR-SUORDV-U-LOCPREL = SPAR-SUORDV-U-LOCPREL              
259100                                - KOLLI-SUORDV-LOCPREL                    
259200     END-IF                                                               
259300     IF AVERAGECOST                                                       
259400        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
259500          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
259600        END-IF                                                            
259700     ELSE                                                                 
259800        IF KOLLI-KDVALISO NOT = SPACE                                     
259900          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
260000        END-IF                                                            
260100     END-IF                                                               
260200     .                                                                    
260300     EJECT                                                                
260400 FFC-SUMMERA-STATUS-P-L-F SECTION.                                        
260500                                                                          
260600     IF ODEL-IDPRODNR NOT = SPAR-ODEL-IDPRODNR                            
260700       MOVE    ODEL-IDPRODNR TO W-WDE6-IDPRODNR                           
260800                                SPAR-ODEL-IDPRODNR                        
260900       PERFORM IMS-GET-WDE601                                             
261000       IF VORD-IDDC-EXP NOT = SPACE                                       
261100          IF VORD-IDDC-EXP = WC-CDC-SE                                    
261200*           *BOUNCE-DC = 11                                               
261300            IF VORD-IDDC = W-IDDC-B6                                      
261400               MOVE JA  TO AVERAGECOST-SW                                 
261500            ELSE                                                          
261600               MOVE NEJ TO AVERAGECOST-SW                                 
261700            END-IF                                                        
261800          ELSE                                                            
261900*           *BOUNCE-DC NOT 11                                             
262000            IF VORD-IDDC = W-IDDC-B6                                      
262100               MOVE NEJ TO AVERAGECOST-SW                                 
262200            ELSE                                                          
262300               MOVE JA  TO AVERAGECOST-SW                                 
262400            END-IF                                                        
262500          END-IF                                                          
262600       ELSE                                                               
262700          IF VORD-SUORDV-EXP > 0                                          
262800             MOVE JA  TO AVERAGECOST-SW                                   
262900          ELSE                                                            
263000             MOVE NEJ TO AVERAGECOST-SW                                   
263100          END-IF                                                          
263200       END-IF                                                             
263300                                                                          
263400       IF SEGMENT-FINNS                                                   
263500         MOVE    JA  TO SW-SKALL-VARDEN-LAGGAS-UT                         
263600         COMPUTE SPAR-KVRADER-TOT =   SPAR-KVRADER-TOT                    
263700                                    + VORD-KVORDRAD                       
263800     IF AVERAGECOST                                                       
263900        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
264000                                    + VORD-SUORDV-EXP                     
264100        MOVE VORD-KDVALISO-EXP  TO SPAR-KDVALISO                          
264200     ELSE                                                                 
264300        COMPUTE SPAR-SUORDV-TOT =   SPAR-SUORDV-TOT                       
264400                                    + VORD-SUORDV                         
264500        MOVE VORD-KDVALISO      TO SPAR-KDVALISO                          
264600     END-IF                                                               
264700     COMPUTE SPAR-SUORDV-TOT-LOC =   SPAR-SUORDV-TOT-LOC                  
264800                                    + VORD-SUORDV-LOC                     
264900     COMPUTE SPAR-SUORDV-TOT-LOCPREL  =   SPAR-SUORDV-TOT-LOCPREL         
265000                                    + VORD-SUORDV-LOCPREL                 
265100         PERFORM IMS-GNP-WDE611                                           
265200         PERFORM UNTIL NOT SEGMENT-FINNS                                  
265300                                                                          
265400           IF KOLLI-KDKOLSTA = K-KDKOLSTA-1-FARDIG-PACKAT OR              
265500                               K-KDKOLSTA-6-FAKTREL                       
265600             MOVE    NEJ TO SW-ORDER-KLAR                                 
265700             PERFORM FFCA-SUM-VARDE-RADER-STAT-P                          
265800             IF KOLLI-KDKOLSTA = K-KDKOLSTA-6-FAKTREL                     
265900               PERFORM S02-SPARA-FAKT-INFO-I-TABELL                       
266000             END-IF                                                       
266100           ELSE                                                           
266200             IF KOLLI-KDKOLSTA = K-KDKOLSTA-2-LASTREL     OR              
266300                                 K-KDKOLSTA-3-LASTAT      OR              
266400                                 K-KDKOLSTA-4-LASTAT-FAKTREL              
266500               MOVE    NEJ TO SW-ORDER-KLAR                               
266600               PERFORM FFCB-SUM-VARDE-RADER-STATUS-L                      
266700               IF KOLLI-KDKOLSTA = K-KDKOLSTA-4-LASTAT-FAKTREL            
266800                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
266900               END-IF                                                     
267000             ELSE                                                         
267100               IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT   OR           
267200                                   K-KDKOLSTA-8-FAKT-LASTREL OR           
267300                                   K-KDKOLSTA-9-FAKT-LASTAT               
267400                 IF KOLLI-KDKOLSTA = K-KDKOLSTA-7-FAKTURERAT  OR          
267500                                     K-KDKOLSTA-8-FAKT-LASTREL            
267600                   MOVE NEJ TO SW-ORDER-KLAR                              
267700                 END-IF                                                   
267800                 PERFORM FFCC-SUM-VARDE-RADER-STATUS-F                    
267900                 PERFORM S02-SPARA-FAKT-INFO-I-TABELL                     
268000               END-IF                                                     
268100             END-IF                                                       
268200           END-IF                                                         
268300           PERFORM IMS-GNP-WDE611                                         
268400         END-PERFORM                                                      
268500       END-IF                                                             
268600     END-IF                                                               
268700     .                                                                    
268800     EJECT                                                                
268900 FFCA-SUM-VARDE-RADER-STAT-P SECTION.                                     
269000                                                                          
269100     IF KOLLI-KVORDRAD     > ZERO                                         
269200       COMPUTE SPAR-KVRADER-P =   SPAR-KVRADER-P                          
269300                                + KOLLI-KVORDRAD                          
269400     END-IF                                                               
269500     IF AVERAGECOST                                                       
269600        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
269700          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
269800                                   + KOLLI-SUORDV-KLI-EXP                 
269900        END-IF                                                            
270000     ELSE                                                                 
270100        IF KOLLI-SUORDV-KOLLI > ZERO                                      
270200          COMPUTE SPAR-SUORDV-P = SPAR-SUORDV-P                           
270300                                   + KOLLI-SUORDV-KOLLI                   
270400        END-IF                                                            
270500     END-IF                                                               
270600     IF KOLLI-SUORDV-LOC     > ZERO                                       
270700       COMPUTE SPAR-SUORDV-P-LOC = SPAR-SUORDV-P-LOC                      
270800                                + KOLLI-SUORDV-LOC                        
270900     END-IF                                                               
271000     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
271100       COMPUTE SPAR-SUORDV-P-LOCPREL = SPAR-SUORDV-P-LOCPREL              
271200                                + KOLLI-SUORDV-LOCPREL                    
271300     END-IF                                                               
271400     IF AVERAGECOST                                                       
271500        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
271600          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
271700        END-IF                                                            
271800     ELSE                                                                 
271900        IF KOLLI-KDVALISO NOT = SPACE                                     
272000          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
272100        END-IF                                                            
272200     END-IF                                                               
272300     .                                                                    
272400     EJECT                                                                
272500                                                                          
272600 FFCB-SUM-VARDE-RADER-STATUS-L SECTION.                                   
272700                                                                          
272800     IF KOLLI-KVORDRAD     > ZERO                                         
272900       COMPUTE SPAR-KVRADER-L =   SPAR-KVRADER-L                          
273000                                + KOLLI-KVORDRAD                          
273100     END-IF                                                               
273200     IF AVERAGECOST                                                       
273300       IF KOLLI-SUORDV-KLI-EXP > ZERO                                     
273400         COMPUTE SPAR-SUORDV-L =  SPAR-SUORDV-L                           
273500                                  + KOLLI-SUORDV-KLI-EXP                  
273600       END-IF                                                             
273700     ELSE                                                                 
273800       IF KOLLI-SUORDV-KOLLI > ZERO                                       
273900         COMPUTE SPAR-SUORDV-L =  SPAR-SUORDV-L                           
274000                                  + KOLLI-SUORDV-KOLLI                    
274100       END-IF                                                             
274200     END-IF                                                               
274300     IF KOLLI-SUORDV-LOC     > ZERO                                       
274400       COMPUTE SPAR-SUORDV-L-LOC = SPAR-SUORDV-L-LOC                      
274500                                + KOLLI-SUORDV-LOC                        
274600     END-IF                                                               
274700     IF KOLLI-SUORDV-LOCPREL > ZERO                                       
274800       COMPUTE SPAR-SUORDV-L-LOCPREL = SPAR-SUORDV-L-LOCPREL              
274900                                + KOLLI-SUORDV-LOCPREL                    
275000     END-IF                                                               
275100     IF AVERAGECOST                                                       
275200        IF KOLLI-KDVALISO-EXP NOT = SPACE                                 
275300          MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                        
275400        END-IF                                                            
275500     ELSE                                                                 
275600        IF KOLLI-KDVALISO NOT = SPACE                                     
275700          MOVE KOLLI-KDVALISO TO SPAR-KDVALISO                            
275800        END-IF                                                            
275900     END-IF                                                               
276000     .                                                                    
276100     EJECT                                                                
276200 FFCC-SUM-VARDE-RADER-STATUS-F SECTION.                                   
276300                                                                          
276400     IF KOLLI-KVORDRAD     > ZERO                                         
276500       COMPUTE SPAR-KVRADER-F =   SPAR-KVRADER-F                          
276600                                + KOLLI-KVORDRAD                          
276700     END-IF                                                               
276800     IF AVERAGECOST                                                       
276900        IF KOLLI-SUORDV-KLI-EXP > ZERO                                    
277000          COMPUTE SPAR-SUORDV-F = SPAR-SUORDV-F                           
277100                                   + KOLLI-SUORDV-KLI-EXP                 
277200        END-IF                                                            
277300     ELSE                                                                 
277400        IF KOLLI-SUORDV-KOLLI > ZERO                                      
277500          COMPUTE SPAR-SUORDV-F = SPAR-SUORDV-F                           
277600                                   + KOLLI-SUORDV-KOLLI                   
277700        END-IF                                                            
277800     END-IF                                                               
277900     IF KOLLI-SUORDV-LOC      > ZERO                                      
278000       COMPUTE SPAR-SUORDV-F-LOC = SPAR-SUORDV-F-LOC                      
278100                                + KOLLI-SUORDV-LOC                        
278200     END-IF                                                               
278300     IF KOLLI-SUORDV-LOCPREL  > ZERO                                      
278400       COMPUTE SPAR-SUORDV-F-LOCPREL = SPAR-SUORDV-F-LOCPREL              
278500                                + KOLLI-SUORDV-LOCPREL                    
278600     END-IF                                                               
278700     IF AVERAGECOST                                                       
278800        MOVE KOLLI-KDVALISO-EXP TO SPAR-KDVALISO                          
278900     ELSE                                                                 
279000        MOVE KOLLI-KDVALISO     TO SPAR-KDVALISO                          
279100     END-IF                                                               
279200     .                                                                    
279300     EJECT                                                                
279400 FFE-FYLL-MOD-MED-TOTALER SECTION.                                        
279500                                                                          
279600     IF DIST79-DEALER-PRICE OR                                            
279800        DIST79-ECOM-PRICE                                                 
279900       COMPUTE HELP-SUMMA = SPAR-SUORDV-TOT-LOC +                         
280000                            SPAR-SUORDV-TOT-LOCPREL                       
280100       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
280200       IF SPAR-SUORDV-TOT-LOCPREL = +0                                    
280300         MOVE ' '                TO MOD-ASTERIX1                          
280400       ELSE                                                               
280500         MOVE '*'                TO MOD-ASTERIX1                          
280600       END-IF                                                             
280700     ELSE                                                                 
280800       MOVE SPAR-SUORDV-TOT      TO HELP-SUORDV-OPACK                     
280900       MOVE ' '                  TO MOD-ASTERIX1                          
281000     END-IF                                                               
281100     IF SEC-KDSVAR = 2 OR 6                                               
281200       MOVE MFS-RENSA-FAELT      TO MOD-SUORDV-TOT                        
281300     ELSE                                                                 
281400       MOVE HELP-SUORDV-OPACK    TO MOD-SUORDV-TOT                        
281500     END-IF                                                               
281600                                                                          
281700     MOVE SPAR-KVRADER-TOT       TO HELP-KVRADER-OPACK                    
281800     MOVE HELP-KVRADER-OPACK     TO MOD-KVRADER-TOT                       
281900                                                                          
282000     IF DIST79-DEALER-PRICE OR                                            
282200        DIST79-ECOM-PRICE                                                 
282300       COMPUTE HELP-SUMMA = SPAR-SUORDV-U-LOC +                           
282400                            SPAR-SUORDV-U-LOCPREL                         
282500       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
282600       IF SPAR-SUORDV-U-LOCPREL = +0                                      
282700         MOVE ' '                TO MOD-ASTERIX2                          
282800       ELSE                                                               
282900         MOVE '*'                TO MOD-ASTERIX2                          
283000       END-IF                                                             
283100     ELSE                                                                 
283200       MOVE SPAR-SUORDV-U        TO HELP-SUORDV-OPACK                     
283300       MOVE ' '                  TO MOD-ASTERIX2                          
283400     END-IF                                                               
283500     IF SEC-KDSVAR = 2 OR 6                                               
283600       MOVE MFS-RENSA-FAELT      TO MOD-SUORDV-U                          
283700     ELSE                                                                 
283800       MOVE HELP-SUORDV-OPACK    TO MOD-SUORDV-U                          
283900     END-IF                                                               
284000                                                                          
284100     IF DIST79-DEALER-PRICE OR                                            
284300        DIST79-ECOM-PRICE                                                 
284400       COMPUTE HELP-SUMMA = SPAR-SUORDV-P-LOC +                           
284500                            SPAR-SUORDV-P-LOCPREL                         
284600       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
284700       IF SPAR-SUORDV-P-LOCPREL = +0                                      
284800         MOVE ' '                TO MOD-ASTERIX3                          
284900       ELSE                                                               
285000         MOVE '*'                TO MOD-ASTERIX3                          
285100       END-IF                                                             
285200     ELSE                                                                 
285300       MOVE SPAR-SUORDV-P        TO HELP-SUORDV-OPACK                     
285400       MOVE ' '                  TO MOD-ASTERIX3                          
285500     END-IF                                                               
285600     IF SEC-KDSVAR = 2 OR 6                                               
285700       MOVE MFS-RENSA-FAELT      TO MOD-SUORDV-P                          
285800     ELSE                                                                 
285900       MOVE HELP-SUORDV-OPACK    TO MOD-SUORDV-P                          
286000     END-IF                                                               
286100                                                                          
286200     MOVE SPAR-KVRADER-P         TO HELP-KVRADER-OPACK                    
286300     MOVE HELP-KVRADER-OPACK     TO MOD-KVRADER-P                         
286400                                                                          
286500     IF DIST79-DEALER-PRICE OR                                            
286700        DIST79-ECOM-PRICE                                                 
286800       COMPUTE HELP-SUMMA = SPAR-SUORDV-F-LOC +                           
286900                            SPAR-SUORDV-F-LOCPREL                         
287000       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
287100       IF SPAR-SUORDV-F-LOCPREL = +0                                      
287200         MOVE ' '                TO MOD-ASTERIX4                          
287300       ELSE                                                               
287400         MOVE '*'                TO MOD-ASTERIX4                          
287500       END-IF                                                             
287600     ELSE                                                                 
287700       MOVE SPAR-SUORDV-F        TO HELP-SUORDV-OPACK                     
287800       MOVE ' '                  TO MOD-ASTERIX4                          
287900     END-IF                                                               
288000     IF SEC-KDSVAR = 2 OR 6                                               
288100       MOVE MFS-RENSA-FAELT      TO MOD-SUORDV-F                          
288200     ELSE                                                                 
288300       MOVE HELP-SUORDV-OPACK    TO MOD-SUORDV-F                          
288400     END-IF                                                               
288500                                                                          
288600     MOVE SPAR-KVRADER-F         TO HELP-KVRADER-OPACK                    
288700     MOVE HELP-KVRADER-OPACK     TO MOD-KVRADER-F                         
288800                                                                          
288900     IF DIST79-DEALER-PRICE OR                                            
289100        DIST79-ECOM-PRICE                                                 
289200       COMPUTE HELP-SUMMA = SPAR-SUORDV-L-LOC +                           
289300                            SPAR-SUORDV-L-LOCPREL                         
289400       MOVE HELP-SUMMA           TO HELP-SUORDV-OPACK                     
289500       IF SPAR-SUORDV-L-LOCPREL = +0                                      
289600         MOVE ' '                TO MOD-ASTERIX5                          
289700       ELSE                                                               
289800         MOVE '*'                TO MOD-ASTERIX5                          
289900       END-IF                                                             
290000     ELSE                                                                 
290100       MOVE SPAR-SUORDV-L        TO HELP-SUORDV-OPACK                     
290200       MOVE ' '                  TO MOD-ASTERIX5                          
290300     END-IF                                                               
290400     IF SEC-KDSVAR = 2 OR 6                                               
290500       MOVE MFS-RENSA-FAELT      TO MOD-SUORDV-L                          
290600     ELSE                                                                 
290700       MOVE HELP-SUORDV-OPACK    TO MOD-SUORDV-L                          
290800     END-IF                                                               
290900                                                                          
291000     MOVE SPAR-KVRADER-L         TO HELP-KVRADER-OPACK                    
291100     MOVE HELP-KVRADER-OPACK     TO MOD-KVRADER-L                         
291200     .                                                                    
291300     EJECT                                                                
291400 FFF-FYLL-MOD-MED-FAKT-INFO SECTION.                                      
291500                                                                          
291600     PERFORM S01-SORTERA-FAKT-INFO-TABELL                                 
291700                                                                          
291800     IF TAB-FAKT-IDFAKT(1) = ALL '9'                                      
291900       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(1)                           
292000                                  MOD-TIFAKT(1)                           
292100     ELSE                                                                 
292200       MOVE TAB-FAKT-IDFAKT(1) TO MOD-IDFAKT(1)                           
292300       MOVE TAB-FAKT-TIFAKT(1) TO MOD-TIFAKT(1)                           
292400     END-IF                                                               
292500                                                                          
292600     IF TAB-FAKT-IDFAKT(2) = ALL '9'                                      
292700       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(5)                           
292800                                  MOD-TIFAKT(5)                           
292900     ELSE                                                                 
293000       MOVE TAB-FAKT-IDFAKT(2) TO MOD-IDFAKT(5)                           
293100       MOVE TAB-FAKT-TIFAKT(2) TO MOD-TIFAKT(5)                           
293200     END-IF                                                               
293300                                                                          
293400     IF TAB-FAKT-IDFAKT(3) = ALL '9'                                      
293500       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(9)                           
293600                                  MOD-TIFAKT(9)                           
293700     ELSE                                                                 
293800       MOVE TAB-FAKT-IDFAKT(3) TO MOD-IDFAKT(9)                           
293900       MOVE TAB-FAKT-TIFAKT(3) TO MOD-TIFAKT(9)                           
294000     END-IF                                                               
294100                                                                          
294200     IF TAB-FAKT-IDFAKT(4) = ALL '9'                                      
294300       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(2)                           
294400                                  MOD-TIFAKT(2)                           
294500     ELSE                                                                 
294600       MOVE TAB-FAKT-IDFAKT(4) TO MOD-IDFAKT(2)                           
294700       MOVE TAB-FAKT-TIFAKT(4) TO MOD-TIFAKT(2)                           
294800     END-IF                                                               
294900                                                                          
295000     IF TAB-FAKT-IDFAKT(5) = ALL '9'                                      
295100       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(6)                           
295200                                  MOD-TIFAKT(6)                           
295300     ELSE                                                                 
295400       MOVE TAB-FAKT-IDFAKT(5) TO MOD-IDFAKT(6)                           
295500       MOVE TAB-FAKT-TIFAKT(5) TO MOD-TIFAKT(6)                           
295600     END-IF                                                               
295700                                                                          
295800     IF TAB-FAKT-IDFAKT(6) = ALL '9'                                      
295900       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(10)                          
296000                                  MOD-TIFAKT(10)                          
296100     ELSE                                                                 
296200       MOVE TAB-FAKT-IDFAKT(6) TO MOD-IDFAKT(10)                          
296300       MOVE TAB-FAKT-TIFAKT(6) TO MOD-TIFAKT(10)                          
296400     END-IF                                                               
296500                                                                          
296600     IF TAB-FAKT-IDFAKT(7) = ALL '9'                                      
296700       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(3)                           
296800                                  MOD-TIFAKT(3)                           
296900     ELSE                                                                 
297000       MOVE TAB-FAKT-IDFAKT(7) TO MOD-IDFAKT(3)                           
297100       MOVE TAB-FAKT-TIFAKT(7) TO MOD-TIFAKT(3)                           
297200     END-IF                                                               
297300                                                                          
297400     IF TAB-FAKT-IDFAKT(8) = ALL '9'                                      
297500       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(7)                           
297600                                  MOD-TIFAKT(7)                           
297700     ELSE                                                                 
297800       MOVE TAB-FAKT-IDFAKT(8) TO MOD-IDFAKT(7)                           
297900       MOVE TAB-FAKT-TIFAKT(8) TO MOD-TIFAKT(7)                           
298000     END-IF                                                               
298100                                                                          
298200     IF TAB-FAKT-IDFAKT(9) = ALL '9'                                      
298300       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(11)                          
298400                                  MOD-TIFAKT(11)                          
298500     ELSE                                                                 
298600       MOVE TAB-FAKT-IDFAKT(9) TO MOD-IDFAKT(11)                          
298700       MOVE TAB-FAKT-TIFAKT(9) TO MOD-TIFAKT(11)                          
298800     END-IF                                                               
298900                                                                          
299000     IF TAB-FAKT-IDFAKT(10) = ALL '9'                                     
299100       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(4)                           
299200                                  MOD-TIFAKT(4)                           
299300     ELSE                                                                 
299400       MOVE TAB-FAKT-IDFAKT(10) TO MOD-IDFAKT(4)                          
299500       MOVE TAB-FAKT-TIFAKT(10) TO MOD-TIFAKT(4)                          
299600     END-IF                                                               
299700                                                                          
299800     IF TAB-FAKT-IDFAKT(11) = ALL '9'                                     
299900       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(8)                           
300000                                  MOD-TIFAKT(8)                           
300100     ELSE                                                                 
300200       MOVE TAB-FAKT-IDFAKT(11) TO MOD-IDFAKT(8)                          
300300       MOVE TAB-FAKT-TIFAKT(11) TO MOD-TIFAKT(8)                          
300400     END-IF                                                               
300500                                                                          
300600     IF TAB-FAKT-IDFAKT(12) = ALL '9'                                     
300700       MOVE MFS-RENSA-FAELT    TO MOD-IDFAKT(12)                          
300800                                  MOD-TIFAKT(12)                          
300900     ELSE                                                                 
301000       MOVE TAB-FAKT-IDFAKT(12) TO MOD-IDFAKT(12)                         
301100       MOVE TAB-FAKT-TIFAKT(12) TO MOD-TIFAKT(12)                         
301200     END-IF                                                               
301300     .                                                                    
301400     EJECT                                                                
301500 S01-SORTERA-FAKT-INFO-TABELL SECTION.                                    
301600                                                                          
301700************* FIX FÖR ATT KLARA SEKELSKIFTET ****************             
301800************* ÅR < 50 BLIR ÅR + 50           ****************             
301900************* ÅR > 50 BLIR ÅR - 50           ****************             
302000     MOVE 1             TO Y2K-IX                                         
302100     PERFORM UNTIL Y2K-IX > 12                                            
302200       MOVE TAB-FAKT-TIFAKT (Y2K-IX) TO TMP1-YYMMDD                       
302300       PERFORM WY2000P1                                                   
302400       MOVE TMP1-YYMMDD     TO TAB-FAKT-TIFAKT (Y2K-IX)                   
302500       ADD 1                TO Y2K-IX                                     
302600     END-PERFORM                                                          
302700                                                                          
302800     CALL WINTSOR USING TABELL-FAKTURA-INFO                               
302900                        TAB-FAKT-WINTSOR-STEG-LANGD                       
303000                        TAB-FAKT-WINTSOR-ANTAL                            
303100                        TAB-FAKT-ELEM(1)                                  
303200                        TAB-FAKT-WINTSOR-SORT-LANGD                       
303300                                                                          
303400************* FIX FÖR ATT KLARA SEKELSKIFTET ****************             
303500************* ÅTERSTÄLLER DATUMEN            ****************             
303600     MOVE 1             TO Y2K-IX                                         
303700     PERFORM UNTIL Y2K-IX > 12                                            
303800       MOVE TAB-FAKT-TIFAKT (Y2K-IX) TO TMP1-YYMMDD                       
303900       PERFORM WY2000P1                                                   
304000       MOVE TMP1-YYMMDD     TO TAB-FAKT-TIFAKT (Y2K-IX)                   
304100       ADD 1                TO Y2K-IX                                     
304200     END-PERFORM                                                          
304300                                                                          
304400     .                                                                    
304500     EJECT                                                                
304600 S02-SPARA-FAKT-INFO-I-TABELL SECTION.                                    
304700                                                                          
304800     IF AVERAGECOST AND BOUNCEORDER                                       
304900*       *IDFAKT-EXP AND TIFAKT-EXP IS NOT FILLED IN                       
305000*       *FOR NOT BOUNCE-ORDER.                                            
305100        PERFORM S02A-FAKT-INFO-AVERAGECOST                                
305200     ELSE                                                                 
305300        PERFORM S02B-FAKT-INFO-NORMALORDER                                
305400     END-IF                                                               
305500     .                                                                    
305600     EJECT                                                                
305700 S02A-FAKT-INFO-AVERAGECOST   SECTION.                                    
305800                                                                          
305900     IF KOLLI-IDFAKT-EXP > ZERO                                           
306000       IF TAB-FAKT-VERKLIGT-ANTAL < TAB-FAKT-IX-MAX-12                    
306100         SET TAB-FAKT-IX TO +1                                            
306200         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
306300                 KOLLI-IDFAKT-EXP = TAB-FAKT-IDFAKT(TAB-FAKT-IX)          
306400           IF TAB-FAKT-IDFAKT(TAB-FAKT-IX) = ALL '9'                      
306500             MOVE KOLLI-IDFAKT-EXP TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
306600             MOVE KOLLI-TIFAKT-EXP TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
306700             SET  TAB-FAKT-VERKLIGT-ANTAL TO TAB-FAKT-IX                  
306800           ELSE                                                           
306900             SET  TAB-FAKT-IX UP BY +1                                    
307000           END-IF                                                         
307100         END-PERFORM                                                      
307200       ELSE                                                               
307300         IF SW-FAKT-INFO-TAB-SORTERAD = NEJ                               
307400           PERFORM S01-SORTERA-FAKT-INFO-TABELL                           
307500           MOVE    JA TO SW-FAKT-INFO-TAB-SORTERAD                        
307600         END-IF                                                           
307700         SET TAB-FAKT-IX TO +1                                            
307800         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
307900                   KOLLI-IDFAKT-EXP = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
308000           MOVE KOLLI-TIFAKT-EXP               TO TMP1-YYMMDD             
308100           MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX)   TO TMP2-YYMMDD             
308200           PERFORM WY2000P1                                               
308300           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
308400             SET     TAB-FAKT-IX TO TAB-FAKT-IX-MAX-12                    
308500             MOVE KOLLI-TIFAKT-EXP             TO TMP1-YYMMDD             
308600             MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD             
308700             PERFORM WY2000P1                                             
308800             PERFORM UNTIL TMP1-YYMMDD >= TMP2-YYMMDD           OR        
308900                           TAB-FAKT-IX  = TAB-FAKT-IX-MIN-1               
309000               MOVE TAB-FAKT-IDFAKT(TAB-FAKT-IX - 1) TO                   
309100                                      TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
309200               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX - 1) TO                   
309300                                      TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
309400               SET  TAB-FAKT-IX DOWN BY +1                                
309500               MOVE KOLLI-TIFAKT-EXP             TO TMP1-YYMMDD           
309600               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD           
309700               PERFORM WY2000P1                                           
309800             END-PERFORM                                                  
309900             IF TAB-FAKT-IX = 1                                           
310000               MOVE KOLLI-IDFAKT-EXP                                      
310100                                   TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
310200               MOVE KOLLI-TIFAKT-EXP                                      
310300                                   TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
310400               SET  TAB-FAKT-IX DOWN BY +1                                
310500             ELSE                                                         
310600               MOVE KOLLI-IDFAKT-EXP TO TAB-FAKT-IDFAKT                   
310700                                                 (TAB-FAKT-IX + 1)        
310800               MOVE KOLLI-TIFAKT-EXP TO TAB-FAKT-TIFAKT                   
310900                                                 (TAB-FAKT-IX + 1)        
311000             END-IF                                                       
311100           END-IF                                                         
311200           SET TAB-FAKT-IX UP BY +1                                       
311300         END-PERFORM                                                      
311400       END-IF                                                             
311500     END-IF                                                               
311600     .                                                                    
311700     EJECT                                                                
311800 S02B-FAKT-INFO-NORMALORDER   SECTION.                                    
311900                                                                          
312000     IF KOLLI-IDFAKT > ZERO                                               
312100       IF TAB-FAKT-VERKLIGT-ANTAL < TAB-FAKT-IX-MAX-12                    
312200         SET TAB-FAKT-IX TO +1                                            
312300         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
312400                       KOLLI-IDFAKT = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
312500           IF TAB-FAKT-IDFAKT(TAB-FAKT-IX) = ALL '9'                      
312600             MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)            
312700             MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)            
312800             SET  TAB-FAKT-VERKLIGT-ANTAL TO TAB-FAKT-IX                  
312900           ELSE                                                           
313000             SET  TAB-FAKT-IX UP BY +1                                    
313100           END-IF                                                         
313200         END-PERFORM                                                      
313300       ELSE                                                               
313400         IF SW-FAKT-INFO-TAB-SORTERAD = NEJ                               
313500           PERFORM S01-SORTERA-FAKT-INFO-TABELL                           
313600           MOVE    JA TO SW-FAKT-INFO-TAB-SORTERAD                        
313700         END-IF                                                           
313800         SET TAB-FAKT-IX TO +1                                            
313900         PERFORM UNTIL TAB-FAKT-IX  > TAB-FAKT-IX-MAX-12        OR        
314000                       KOLLI-IDFAKT = TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
314100           MOVE KOLLI-TIFAKT                   TO TMP1-YYMMDD             
314200           MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX)   TO TMP2-YYMMDD             
314300           PERFORM WY2000P1                                               
314400           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
314500             SET     TAB-FAKT-IX TO TAB-FAKT-IX-MAX-12                    
314600             MOVE KOLLI-TIFAKT                 TO TMP1-YYMMDD             
314700             MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD             
314800             PERFORM WY2000P1                                             
314900             PERFORM UNTIL TMP1-YYMMDD >= TMP2-YYMMDD           OR        
315000                           TAB-FAKT-IX  = TAB-FAKT-IX-MIN-1               
315100               MOVE TAB-FAKT-IDFAKT(TAB-FAKT-IX - 1) TO                   
315200                                      TAB-FAKT-IDFAKT(TAB-FAKT-IX)        
315300               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX - 1) TO                   
315400                                      TAB-FAKT-TIFAKT(TAB-FAKT-IX)        
315500               SET  TAB-FAKT-IX DOWN BY +1                                
315600               MOVE KOLLI-TIFAKT                 TO TMP1-YYMMDD           
315700               MOVE TAB-FAKT-TIFAKT(TAB-FAKT-IX) TO TMP2-YYMMDD           
315800               PERFORM WY2000P1                                           
315900             END-PERFORM                                                  
316000             IF TAB-FAKT-IX = 1                                           
316100               MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT(TAB-FAKT-IX)          
316200               MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT(TAB-FAKT-IX)          
316300               SET  TAB-FAKT-IX DOWN BY +1                                
316400             ELSE                                                         
316500               MOVE KOLLI-IDFAKT TO TAB-FAKT-IDFAKT                       
316600                                                 (TAB-FAKT-IX + 1)        
316700               MOVE KOLLI-TIFAKT TO TAB-FAKT-TIFAKT                       
316800                                                 (TAB-FAKT-IX + 1)        
316900             END-IF                                                       
317000           END-IF                                                         
317100           SET TAB-FAKT-IX UP BY +1                                       
317200         END-PERFORM                                                      
317300       END-IF                                                             
317400     END-IF                                                               
317500     .                                                                    
317600     EJECT                                                                
317700 MFS-RENSA-FAELT-UT SECTION.                                              
317800                                                                          
317900     MOVE MFS-RENSA-FAELT TO                                              
318000                             MOD-BEKUNDRF                                 
318100                             MOD-IDKAMPRF                                 
318200                             MOD-TIREGDAT                                 
318300                             MOD-TIHHMM-REG                               
318400                             MOD-TIREGDAT-STO                             
318500                             MOD-TIHHMM-REG-STO                           
318600                             MOD-TIAAMMDD-RFS                             
318700                             MOD-TIREPDAT                                 
318800                             MOD-IDLEVNR                                  
318900                             MOD-KDORDTYP-LDC                             
319000                             MOD-IDTRPLOS                                 
319100                             MOD-IDTRPVAR                                 
319200                             MOD-TIAAMMDD-TRP                             
319300                             MOD-TIHHMM-TRP                               
319400                             MOD-SUORDV-TOT                               
319500                             MOD-KVRADER-TOT                              
319600                             MOD-SUORDV-U                                 
319700                             MOD-SUORDV-P                                 
319800                             MOD-KVRADER-P                                
319900                             MOD-SUORDV-F                                 
320000                             MOD-KVRADER-F                                
320100                             MOD-SUORDV-L                                 
320200                             MOD-KVRADER-L                                
320300     MOVE +1 TO HJALP-MOD-IX                                              
320400                                                                          
320500     PERFORM UNTIL HJALP-MOD-IX > HJALP-MOD-IX-MAX-12                     
320600       MOVE MFS-RENSA-FAELT TO MOD-IDFAKT(HJALP-MOD-IX)                   
320700                               MOD-TIFAKT(HJALP-MOD-IX)                   
320800       ADD  +1              TO HJALP-MOD-IX                               
320900     END-PERFORM                                                          
321000     .                                                                    
321100     EJECT                                                                
321200*                                                                         
321300*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
321400*                 III     III MM MMMMM MM SSSS   SSSS                     
321500*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
321600*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
321700*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
321800*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
321900*                 III     III MM MMMMM MM SSSS   SSSS                     
322000*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
322100*                                                                         
322200*                                                                         
322300     SKIP3                                                                
322400 IMS-GET-MSG SECTION.                                                     
322500                                                                          
322600     MOVE    '  QC'          TO    GODK-STATUSKODER                       
322700     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
322800     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
322900     PERFORM IMS-STATUSKONTROLL                                           
323000     .                                                                    
323100                                                                          
323200 IMS-INSERT-MSG SECTION.                                                  
323300                                                                          
323400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
323500       MOVE '0' TO MFS-KDHUVOMR                                           
323600     END-IF                                                               
323700     MOVE    LOW-VALUE       TO    MSG-KDZ1 MSG-KDZ2                      
323800     MOVE    SPACE           TO    GODK-STATUSKODER                       
323900     CALL    CBLTDLI         USING ISRT MSG-PCB                           
324000                                   MSG-IO-AREA MFS-IDMOD                  
324100     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
324200     PERFORM IMS-STATUSKONTROLL                                           
324300     .                                                                    
324400     EJECT                                                                
324500 IMS-GET-WDQ201-CSEQ SECTION.                                             
324600                                                                          
324700     STRING 'WLORQI01(WDQ2CSEQ>=' W-WDQ2CSEQ-MIN-X                        
324800                    '&WDQ2CSEQ<=' W-WDQ2CSEQ-MAX-X ')'                    
324900          DELIMITED BY SIZE INTO    SSA1                                  
325000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
325100     CALL    CBLTDLI          USING GU   ORQI-PCB DLI-IO-WLORQI01         
325200                                         SSA1                             
325300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
325400     PERFORM IMS-STATUSKONTROLL                                           
325500     .                                                                    
325600     EJECT                                                                
325700 IMS-GNPF-WDQ211-QUAL SECTION.                                            
325800                                                                          
325900     STRING 'WLORQI11*F(WDQ211KY>=' W-WDQ211KY-MIN-X                      
326000                      '&WDQ211KY<=' W-WDQ211KY-MAX-X ')'                  
326100          DELIMITED BY SIZE INTO    SSA1                                  
326200     MOVE    '  GE'           TO    GODK-STATUSKODER                      
326300     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI11         
326400                                         SSA1                             
326500     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
326600     PERFORM IMS-STATUSKONTROLL                                           
326700     .                                                                    
326800     EJECT                                                                
326900 IMS-GNP-FIRST-WDQ211 SECTION.                                            
327000                                                                          
327100     MOVE   'WLORQI11*F'      TO    SSA1                                  
327200     MOVE    '  GE'           TO    GODK-STATUSKODER                      
327300     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI11         
327400                                         SSA1                             
327500     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
327600     PERFORM IMS-STATUSKONTROLL                                           
327700     .                                                                    
327800                                                                          
327900 IMS-GNP-WDQ211 SECTION.                                                  
328000                                                                          
328100     MOVE   'WLORQI11'        TO    SSA1                                  
328200     MOVE    '  GE'           TO    GODK-STATUSKODER                      
328300     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI11         
328400                                         SSA1                             
328500     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
328600     PERFORM IMS-STATUSKONTROLL                                           
328700     .                                                                    
328800     EJECT                                                                
328900 IMS-GNP-WDQ212 SECTION.                                                  
329000                                                                          
329100     STRING 'WLORQI12(IDDC     =' W-IDDC-X     ')'                        
329200          DELIMITED BY SIZE INTO    SSA1                                  
329300     MOVE    '  GE'           TO    GODK-STATUSKODER                      
329400     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI12         
329500                                         SSA1                             
329600     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
329700     PERFORM IMS-STATUSKONTROLL                                           
329800     .                                                                    
329900     EJECT                                                                
330000 IMS-GNP-WDQ212-OKVAL SECTION.                                            
330100                                                                          
330200     MOVE    'WLORQI12'       TO    SSA1                                  
330300     MOVE    '  GE'           TO    GODK-STATUSKODER                      
330400     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI12         
330500                                         SSA1                             
330600     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
330700     PERFORM IMS-STATUSKONTROLL                                           
330800     .                                                                    
330900     EJECT                                                                
332000 IMS-GNP-WDQ221 SECTION.                                                  
332100                                                                          
332202     STRING 'WLORQI12(IDDC     =' W-IDDC-X     ')'                        
332302          DELIMITED BY SIZE INTO    SSA1                                  
332402     MOVE   'WLORQI21'        TO    SSA2                                  
332500     MOVE    '  GE'           TO    GODK-STATUSKODER                      
332600     CALL    CBLTDLI          USING GNP  ORQI-PCB DLI-IO-WLORQI21         
332702                                         SSA1 SSA2                        
332800     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
332900     PERFORM IMS-STATUSKONTROLL                                           
333000     .                                                                    
333100     EJECT                                                                
333200 IMS-GU-WDQ301-FIRST SECTION.                                             
333300     STRING 'WLORQA01*F(WDQ301KY>=' W-WDQ301KY-MIN-X                      
333400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
333500          DELIMITED BY SIZE INTO    SSA1                                  
333600     MOVE    '  GE'           TO    GODK-STATUSKODER                      
333700     CALL    CBLTDLI          USING GU   ORQA-PCB DLI-IO-WLORQA01         
333800                                         SSA1                             
333900     MOVE    ORQA-STATUS-CODE TO    STATUS-WS                             
334000     PERFORM IMS-STATUSKONTROLL                                           
334100     .                                                                    
334200     EJECT                                                                
334300 IMS-GN-WDQ301-INTERV SECTION.                                            
334400     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
334500                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
334600          DELIMITED BY SIZE INTO    SSA1                                  
334700     MOVE    '  GE'           TO    GODK-STATUSKODER                      
334800     CALL    CBLTDLI          USING GN   ORQA-PCB DLI-IO-WLORQA01         
334900                                         SSA1                             
335000     MOVE    ORQA-STATUS-CODE TO    STATUS-WS                             
335100     PERFORM IMS-STATUSKONTROLL                                           
335200     .                                                                    
335300     EJECT                                                                
335400 IMS-GET-WDE601 SECTION.                                                  
335500                                                                          
335600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
335700          DELIMITED BY SIZE INTO    SSA1                                  
335800     MOVE    '  GE'           TO    GODK-STATUSKODER                      
335900     CALL    CBLTDLI          USING GU   WDE6-PCB DLI-IO-WDE601           
336000                                         SSA1                             
336100     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
336200     PERFORM IMS-STATUSKONTROLL                                           
336300     .                                                                    
336400                                                                          
336500 IMS-GNP-WDE611 SECTION.                                                  
336600                                                                          
336700     MOVE   'WDE611  '        TO    SSA1                                  
336800     MOVE    '  GE'           TO    GODK-STATUSKODER                      
336900     CALL    CBLTDLI          USING GNP  WDE6-PCB DLI-IO-WDE611           
337000                                         SSA1                             
337100     MOVE    WDE6-STATUS-CODE TO    STATUS-WS                             
337200     PERFORM IMS-STATUSKONTROLL                                           
337300     .                                                                    
337400     EJECT                                                                
337500 IMS-GU-WDB601    SECTION.                                                
337600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
337700          DELIMITED BY SIZE INTO SSA1                                     
337800     MOVE '  GE' TO GODK-STATUSKODER                                      
337900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
338000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     IF SEGMENT-SAKNAS                                                    
338300         MOVE SPACE TO DCS-KDDC                                           
338400     END-IF                                                               
338500     .                                                                    
338600 IMS-STATUSKONTROLL SECTION.                                              
338700                                                                          
338800     SET    STATUS-IX TO +1                                               
338900     SEARCH GODK-STATUS                                                   
339000       AT END                                                             
339100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
339200              DELIMITED BY SIZE INTO FELTEXT                              
339300         CALL FELLOG                                                      
339400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
339500         CONTINUE                                                         
339600     END-SEARCH                                                           
339700     .                                                                    
339800     EJECT                                                                
340000*    -COPY WY2000P1                                                       
