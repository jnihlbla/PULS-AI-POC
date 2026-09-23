000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4030100.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500     DATE-WRITTEN.   MARS 1986.                                           
000600                                                                          
000700     REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        BILD 4301.                                                       
001000*        PACKNINGSRAPPORTERING ORDERVIS                                   
001100*        GRUND-BILD SVERIGE.                                              
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T301                                              
001600*        MID:         W4I30101-MID.                                       
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O30101-MOD.                                       
002000*    SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77   PROGRAM-NAMN           VALUE 'W4030100'                             
002900                                 PIC X(8).                                
003000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003100 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77    RADIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77    JMF-IND                   PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77    MAX-RADIND-PLUS-1         PIC S9(9)   VALUE +14  COMP SYNC.        
003500 77    4316-IND                  PIC S9(9)   VALUE +0   COMP SYNC.        
003600 77    MAX-4316-IND              PIC S9(9)   VALUE +12  COMP SYNC.        
003700 77    MAX-4316-IND-PLUS-1       PIC S9(9)   VALUE +13  COMP SYNC.        
003800 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +500 COMP SYNC.        
003900 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
004000 77    M4391-MOD-LAENGD          PIC S9(4)   VALUE +84  COMP SYNC.        
004100 77    M4392-MOD-LAENGD          PIC S9(4)   VALUE +84  COMP SYNC.        
004200 77    FILLER                    PIC X(8)    VALUE 'QQQQQQQQ'.            
004300 77    MOTSV-IDPRODNR-FINNS      PIC X(1).                                
004400 77    ANTAL-SAMPACKADE-ORDER    PIC 9(2).                                
004500 77    RAETT                     PIC X       VALUE 'R'.                   
004600 77    FEL                       PIC X       VALUE 'F'.                   
004700 77    FEL-NR1                   PIC X       VALUE '1'.                   
004800 77    FEL-NR2                   PIC X       VALUE '2'.                   
004900 77    FEL-NR3                   PIC X       VALUE '3'.                   
005000 77    FEL-NR4                   PIC X       VALUE '4'.                   
005100 77    FEL-NR5                   PIC X       VALUE '5'.                   
005200 77    FEL-NR6                   PIC X       VALUE '6'.                   
005300 77    FEL-NR8                   PIC X       VALUE '8'.                   
005400 77    JA                        PIC X       VALUE 'J'.                   
005500 77    NEJ                       PIC X       VALUE 'N'.                   
005600 77    TRAFF                     PIC X       VALUE 'N'.                   
005700     SKIP2                                                                
005800 01  WS-MSG-CALL-GRP.                                                     
005900   03  WS-MSG-CALL               PIC X.                                   
006000     88  VISA-NAESTA-BILD                    VALUE '0'.                   
006100     88  STARTA-4301                         VALUE '1'.                   
006200     88  STARTA-4302                         VALUE '2'.                   
006300     88  STARTA-4303                         VALUE '3'.                   
006400     88  STARTA-4391                         VALUE '4'.                   
006500     88  STARTA-4392                         VALUE '5'.                   
006600     88  STARTA-4393                         VALUE '6'.                   
006700     88  STARTA-4394                         VALUE '7'.                   
006800     88  STARTA-4395                         VALUE '8'.                   
006900     88  STARTA-4396                         VALUE '9'.                   
007000   03  WS-VISA-NAESTA-BILD       PIC X       VALUE '0'.                   
007100   03  WS-STARTA-4301            PIC X       VALUE '1'.                   
007200   03  WS-STARTA-4302            PIC X       VALUE '2'.                   
007300   03  WS-STARTA-4303            PIC X       VALUE '3'.                   
007400   03  WS-STARTA-4391            PIC X       VALUE '4'.                   
007500   03  WS-STARTA-4392            PIC X       VALUE '5'.                   
007600   03  WS-STARTA-4393            PIC X       VALUE '6'.                   
007700   03  WS-STARTA-4394            PIC X       VALUE '7'.                   
007800   03  WS-STARTA-4395            PIC X       VALUE '8'.                   
007900   03  WS-STARTA-4396            PIC X       VALUE '9'.                   
008000     SKIP2                                                                
008100 01    DYNAMISKA-SUBPROGRAM.                                              
008200   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008500   03    W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008600     SKIP2                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800*01 -COPY WMSGINIT                                                        
008900     SKIP2                                                                
009000 01    FILLER                    PIC X(16)   VALUE 'WS-MODNAMN'.          
009100 01    WS-MODNAMN.                                                        
009200   03    FILLER                  PIC X       VALUE 'W'.                   
009300   03    WS-MOD-IDTRANS-POS-1    PIC X.                                   
009400   03    FILLER                  PIC X       VALUE 'O'.                   
009500   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3).                                
009600   03    FILLER                  PIC X(2)    VALUE '01'.                  
009700                                                                          
009800 01    FILLER                    PIC X(16)                                
009900                                 VALUE 'WS-IDTRANS-MOD'.                  
010000 01    WS-IDTRANS-MOD.                                                    
010100   03    WS-IDTRANS-POS-1-MOD    PIC X.                                   
010200   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3).                                
010300     EJECT                                                                
010400***WS-AREA************************************                            
010500 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
010600 01    DIVERSE.                                                           
010700                                                                          
010800   03    WS-INDATA-TEST          PIC X       VALUE SPACE.                 
010900         88  WS-INDATA-FEL-NR1               VALUE '1'.                   
011000         88  WS-INDATA-FEL-NR2               VALUE '2'.                   
011100         88  WS-INDATA-FEL-NR3               VALUE '3'.                   
011200         88  WS-INDATA-FEL-NR4               VALUE '4'.                   
011300         88  WS-INDATA-FEL-NR5               VALUE '5'.                   
011400         88  WS-INDATA-FEL-NR6               VALUE '6'.                   
011500         88  WS-INDATA-FEL-NR8               VALUE '8'.                   
011600         88  WS-INDATA-RAETT                 VALUE 'R'.                   
011700                                                                          
011800   03    SPARADE-NYCKLAR-FRAN-WDE601.                                     
011900         05  SPAR-IDDISTR        PIC 9(4).                                
012000         05  SPAR-IDKUNDNR       PIC 9(6).                                
012100         05  SPAR-KDFRAKT        PIC 9(2).                                
012200                                                                          
012300   03    MAX-ANT-RAD             PIC S9(9)   VALUE +100.                  
012400   03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
012500   03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
012600   03    WS-TOM-SISTA            PIC 9(4)    VALUE ZERO.                  
012700                                                                          
012800   03    WS-IDPRODNR             PIC 9(7)    VALUE ZERO.                  
012900   03    WS-IDDISTR              PIC 9(4)    VALUE ZERO.                  
013000   03    WS-IDKUNDNR             PIC 9(6)    VALUE ZERO.                  
013100   03    WS-KDFRAKT              PIC 9(2)    VALUE ZERO.                  
013200   03    WS-KDORDKL              PIC 9       VALUE ZERO.                  
013300   03    WS-KVORDRAD             PIC 9(4)    VALUE ZERO.                  
013400                                                                          
013500   03    WS-FROM                 PIC 9(4)    VALUE ZERO.                  
013600   03    WS-TOM                  PIC 9(4)    VALUE ZERO.                  
013700                                                                          
013800   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
013900         88  WS-EGEN-BILD        VALUE '4301'.                            
014000*                                                                         
016500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
016600     88  NYCKLAR-OK                          VALUE 'J'.                   
016700     88  NYCKLAR-FEL                         VALUE 'N'.                   
016800                                                                          
016900 01  FILLER                    PIC X(16)   VALUE 'WS-TAB'.                
017000 01  WS-TAB.                                                              
017100   03  WS-TABSTEG OCCURS 13.                                              
017200     05  WSWDE6-IDDISTR          PIC S9(5)        COMP-3.                 
017300     05  WSWDE6-IDKUNDNR         PIC S9(7)        COMP-3.                 
017400     05  WSWDE6-KDORDKL          PIC S9           COMP-3.                 
017500     05  WSWDE6-KVORDRAD         PIC S9(5)        COMP-3.                 
017600     05  WSWDE6-KDFRAKT          PIC S9(3)        COMP-3.                 
017700     05  WSWDE6-IDKUNDRF         PIC X(10).                               
017800     EJECT                                                                
017900 01  FILLER                    PIC X(16)   VALUE 'VIKT  '.                
018000     SKIP3                                                                
018100                                                                          
018200 01  WS-VKORDBTO-X.                                                       
018300   03  WS-VKORDBTO-RED           PIC  9(5).9 VALUE ZERO.                  
018400     SKIP3                                                                
018500                                                                          
018600 01  WS-IDEDITDATA               PIC 9(5)V9  VALUE ZERO.                  
018700 01  WS-VKORDBTO REDEFINES WS-IDEDITDATA.                                 
018800   03  FILLER                    PIC  9(5).                               
018900   03  WS-VKORDBTO-DEC           PIC  9.                                  
019000     SKIP3                                                                
019100                                                                          
019200 01  FILLER.                                                              
019300   03  WS-TABSTEG OCCURS 13.                                              
019400     05  WSEMB-KDEMBTYP          PIC 9.                                   
019500     05  WSTAB-VKORDBTO-X.                                                
019600       07  WSTAB-VKORDBTO          PIC 9(7).                              
019700                                                                          
019800 01  FILLER.                                                              
019900   03  TABELL-MED-PRODNR OCCURS 13 INDEXED BY T1-IX.                      
020000     05  T1-GRUPP.                                                        
020100       07  T1-IDPRODNR           PIC 9(7).                                
020200       07  T1-IDPRODNR-SAMP      PIC 9(7).                                
020300                                                                          
020400 01  T1-GRUPP-X.                                                          
020500   03  T1-IDPRODNR-X             PIC X(7).                                
020600   03  T1-IDPRODNR-SAMP-X        PIC X(7).                                
020700     EJECT                                                                
020800 01    FILLER                    PIC X(16)   VALUE 'WDECAREA'.            
020900*01    -COPY WDECAREA                                                     
021000     EJECT                                                                
021100****************************                                              
021200 01    FILLER                    PIC X(16)   VALUE 'WWDIST  '.            
021300 01    TEST-IDDISTR              PIC  9(5)   VALUE ZERO COMP-3.           
021400 01    FILLER REDEFINES TEST-IDDISTR.                                     
021500*  03  -COPY WWDIST03.                                                    
021600     SKIP2                                                                
021700 01    FILLER REDEFINES TEST-IDDISTR.                                     
021800*  03  -COPY WWDIST19.                                                    
021900****************************                                              
022000     EJECT                                                                
022100 01    FILLER                    PIC X(16)                                
022200                                 VALUE 'NYCKLAR-TILL-DLI'.                
022300 01    NYCKLAR-TILL-DLI.                                                  
022400   03    W-WDGXKEY-4305-X.                                                
022500     05    FILLER                PIC X(4)    VALUE '4305'.                
022600     05    W-IDDC-4305           PIC X(2).                                
022700     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
022800                                                                          
022900   03    W-WDGXKEY-4306-X.                                                
023000     05    W-IDPRODNR-4306       PIC S9(7)   VALUE ZERO  COMP-3.          
023100     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
023200                                                                          
023300   03    W-WDGXKEY-4311-X.                                                
023400     05    FILLER                PIC X(4)    VALUE '4311'.                
023500     05    W-IDDC-4311           PIC X(2).                                
023600     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
023700                                                                          
023800   03    W-WDGXKEY-4312-X.                                                
023900     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
024000     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
024100                                                                          
024200   03    W-WDGXKEY-IDUSER-4312   PIC X(8)    VALUE SPACE.                 
024300                                                                          
024400   03    W-WDGXKEY-4315-X.                                                
024500     05    FILLER                PIC X(4)    VALUE '4315'.                
024600     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
024700                                                                          
024800   03    W-WDGXKEY-4316-X.                                                
024900     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
025000     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
025100     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
025200     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
025300                                                                          
025400   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
025500                                                                          
025600   03    W-WDE6-X.                                                        
025700     05    W-IDPRODNR-WDE6       PIC S9(7)   VALUE ZERO  COMP-3.          
025800                                                                          
025900   03    W-WDQ3D-X.                                                       
026000     05    W-IDPRODNR-WDQ3D      PIC S9(7)   VALUE ZERO  COMP-3.          
026100     05    W-IDPLKLST-WDQ3D      PIC S9(3)   VALUE ZERO  COMP-3.          
026200                                                                          
026210   03    W-WDE4E1KY-MIN-X.                                                
026220     05  W-IDPRODNR-MIN          PIC S9(7)   VALUE ZERO  COMP-3.          
026230     05 FILLER-MIN               PIC X(19)   VALUE LOW-VALUE.             
026240                                                                          
026250   03    W-WDE4E1KY-MAX-X.                                                
026260     05  W-IDPRODNR-MAX          PIC S9(7)   VALUE ZERO  COMP-3.          
026270     05 FILLER-MAX               PIC X(19)   VALUE HIGH-VALUE.            
026271                                                                          
026272   03    W-IDDC-B6-X.                                                     
026273     05  W-IDDC-B6               PIC  X(2).                               
026280                                                                          
026300     EJECT                                                                
026400 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
026500 01    MEDDELANDE.                                                        
026600                                                                          
026700   03    FEL1.                                                            
026800      05    FILLER               PIC X(40)   VALUE                        
026900           '748. UPPLYSTA FÄLT FEL'.                                      
027000      05    FILLER               PIC X(40)   VALUE                        
027100           '748. VERLICHTE ZONE FOUTIEF'.                                 
027200   03    FILLER  REDEFINES FEL1.                                          
027300      05    FEL-1                PIC X(40)   OCCURS 2.                    
027400                                                                          
027500   03    FEL2.                                                            
027600      05    FILLER               PIC X(40)   VALUE                        
027700           '750. INGÅNG VIA ANNAN MENY'.                                  
027800      05    FILLER               PIC X(40)   VALUE                        
027900           '750. TOEGANGELIJK VIA EEN ANDERE MENU'.                       
028000   03    FILLER  REDEFINES FEL2.                                          
028100      05    FEL-2                PIC X(40)   OCCURS 2.                    
028200                                                                          
028300   03    FEL3.                                                            
028400      05    FILLER               PIC X(40)   VALUE                        
028500           'SAMPACKAD MED EN ANNAN ORDER'.                                
028600      05    FILLER               PIC X(40)   VALUE                        
028700           'PACKED TOGETHER WITH ANOTHER ORDER'.                          
028800   03    FILLER  REDEFINES FEL3.                                          
028900      05    FEL-3                PIC X(40)   OCCURS 2.                    
029000                                                                          
029100   03    FEL4.                                                            
029200      05    FILLER               PIC X(40)   VALUE                        
029300           'FRAKTSEDEL REDAN UTSKRIVEN FÖR ORDER    '.                    
029400      05    FILLER               PIC X(40)   VALUE                        
029500           'FREIGHT BILL ALREADY PRINTED FOR ORDER  '.                    
029600   03    FILLER  REDEFINES FEL4.                                          
029700      05    FEL-4                PIC X(40)   OCCURS 2.                    
029800                                                                          
029900   03    FEL5.                                                            
030000      05    FILLER               PIC X(40)   VALUE                        
030100           '782 ORDERN DELAD                        '.                    
030200      05    FILLER               PIC X(40)   VALUE                        
030300           '782 ORDER UITGEGEVEN                    '.                    
030400   03    FILLER  REDEFINES FEL5.                                          
030500      05    FEL-5                PIC X(40)   OCCURS 2.                    
030600                                                                          
030700   03    FEL6.                                                            
030800      05    FILLER               PIC X(40)   VALUE                        
030900           'ANVÄND KOLLIVIS > 100 RADER             '.                    
031000      05    FILLER               PIC X(40)   VALUE                        
031100           'USE CASE BY CASE > 100 LINES            '.                    
031200   03    FILLER  REDEFINES FEL6.                                          
031300      05    FEL-6                PIC X(40)   OCCURS 2.                    
031400                                                                          
031500   03    FEL7.                                                            
031600      05    FILLER               PIC X(40)   VALUE                        
031700           'NYCKLAR SAKNAS                          '.                    
031800      05    FILLER               PIC X(40)   VALUE                        
031900           'KEYS ARE MISSING                        '.                    
032000   03    FILLER  REDEFINES FEL7.                                          
032100      05    FEL-7                PIC X(40)   OCCURS 2.                    
032200                                                                          
032300   03    FEL8.                                                            
032400      05    FILLER               PIC X(40)   VALUE                        
032500           'SOFTWARE ORDER                          '.                    
032600      05    FILLER               PIC X(40)   VALUE                        
032700           'SOFTWARE ORDER                          '.                    
032800   03    FILLER  REDEFINES FEL8.                                          
032900      05    FEL-8                PIC X(40)   OCCURS 2.                    
033000                                                                          
033100   03    INF1.                                                            
033200      05    FILLER               PIC X(40)   VALUE                        
033300           '807. RAPPORTERING PÅBÖRJAD KOLLIVIS'.                         
033400      05    FILLER               PIC X(40)   VALUE                        
033500           '807. RAPPPORTERING PER KIST BEGONNEN'.                        
033600   03    FILLER  REDEFINES INF1.                                          
033700      05    INF-1                PIC X(40)   OCCURS 2.                    
033800                                                                          
033900   03    INF3.                                                            
034000      05    FILLER               PIC X(40)   VALUE                        
034100           '702. ORDERVIS PACKNING PÅGÅR'.                                
034200      05    FILLER               PIC X(40)   VALUE                        
034300           '702. REEDS BEGONNEN ORD/ORD RAPP'.                            
034400   03    FILLER  REDEFINES INF3.                                          
034500      05    INF-3                PIC X(40)   OCCURS 2.                    
034600                                                                          
034700   03    INF4.                                                            
034800      05    FILLER               PIC X(40)   VALUE                        
034900           '710. ORDERN FÄRDIGRAPPORTERAD'.                               
035000      05    FILLER               PIC X(40)   VALUE                        
035100           '710. ORDER VOLLEDIG GERAPPORTEERD'.                           
035200   03    FILLER  REDEFINES INF4.                                          
035300      05    INF-4                PIC X(40)   OCCURS 2.                    
035400                                                                          
035500   03    INF5.                                                            
035600      05    FILLER               PIC X(40)   VALUE                        
035700           '755. PACKNING GÅR EJ - LÅSNINGSKOD FEL'.                      
035800      05    FILLER               PIC X(40)   VALUE                        
035900           '755. FOUTIEVE LAADKODE'.                                      
036000   03    FILLER  REDEFINES INF5.                                          
036100      05    INF-5                PIC X(40)   OCCURS 2.                    
036200                                                                          
036300   03    INF6.                                                            
036400      05    FILLER               PIC X(40)   VALUE                        
036500           ' UPPDATERING UTFÖRD                   '.                      
036600      05    FILLER               PIC X(40)   VALUE                        
036700           ' UPDATING UITGEVORDERD.               '.                      
036800   03    FILLER  REDEFINES INF6.                                          
036900      05    INF-6                PIC X(40)   OCCURS 2.                    
037000                                                                          
037100   03    INF7.                                                            
037200      05    FILLER               PIC X(40)   VALUE                        
037300           ' ONLY FOR SWEDISH DC                  '.                      
037400      05    FILLER               PIC X(40)   VALUE                        
037500           ' ONLY FOR SWEDISH DC.                 '.                      
037600   03    FILLER  REDEFINES INF7.                                          
037700      05    INF-7                PIC X(40)   OCCURS 2.                    
037800                                                                          
037900     EJECT                                                                
038000******************************************************************        
038100*                                                                         
038200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
038300*                                                                         
038400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
038500     SKIP3                                                                
038600 01    FILLER                    PIC X(16)                                
038700                                 VALUE 'MID W4I30101 MID'.                
038800     SKIP3                                                                
038900*01    -COPY W4I30101                                                     
039000     EJECT                                                                
039100*01    -COPY WMSGAREA                                                     
039200     EJECT                                                                
039300*  03  MOD -COPY W4O30101   -RED MSG-AREA                                 
039400     EJECT                                                                
039500*  03  MOD -COPY W4O39101   -RED MSG-AREA -PRE M4391-                     
039600     EJECT                                                                
039700*  03  MOD -COPY W4O39201   -RED MSG-AREA -PRE M4392-                     
039800     EJECT                                                                
039900 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW'.           
040000 01    P-TO-P-SW.                                                         
040100       03  PTOP-LL               PIC S9(4)   VALUE +17 COMP SYNC.         
040200       03  PTOP-Z1               PIC X       VALUE LOW-VALUE.             
040300       03  PTOP-Z2               PIC X       VALUE LOW-VALUE.             
040400       03  PTOP-TRANSKOD         PIC X(7)    VALUE 'W0T605U'.             
040500       03  FILLER                PIC X       VALUE SPACE.                 
040600       03  FILLER                PIC X(4)    VALUE '4301'.                
040700       03  PTOP-KDMFSFOR         PIC X.                                   
040800****** 03  MID -COPY W0I60501   -PRE MOD-                                 
040900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
041000 01  P-TO-P-SW2.                                                          
041100       03  PTOP2-LL              PIC S9(4)   VALUE +89 COMP SYNC.         
041200       03  PTOP2-Z1              PIC X       VALUE LOW-VALUE.             
041300       03  PTOP2-Z2              PIC X       VALUE LOW-VALUE.             
041400       03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W4T392U'.             
041500       03  FILLER                PIC X       VALUE SPACE.                 
041600       03  FILLER                PIC X(4)    VALUE '4301'.                
041700       03  PTOP2-KDMFSFOR        PIC X.                                   
041800       03  PTOP2-IDPRODNR-IN     PIC X(7).                                
041900       03  PTOP2-IDPRODNR-UT     PIC X(7).                                
042000       03  PTOP2-IDDISTR-UT      PIC X(4).                                
042100       03  PTOP2-IDKUNDNR-UT     PIC X(6).                                
042200       03  PTOP2-KDFRAKT-UT      PIC X(2).                                
042300       03  PTOP2-IDORDNR-UT      PIC X(5).                                
042400       03  PTOP2-KDORDKL-UT      PIC X(1).                                
042500       03  PTOP2-IDDC-UT         PIC X(2).                                
042600     EJECT                                                                
042700 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
042800 01  P-TO-P-SW1.                                                          
042900       03  PTOP1-LL              PIC S9(4)   VALUE +189 COMP SYNC.        
043000       03  PTOP1-Z1              PIC X       VALUE LOW-VALUE.             
043100       03  PTOP1-Z2              PIC X       VALUE LOW-VALUE.             
043200       03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W4T391U'.             
043300       03  FILLER                PIC X       VALUE SPACE.                 
043400       03  FILLER                PIC X(4)    VALUE '4301'.                
043500       03  PTOP1-KDMFSFOR        PIC X.                                   
043600       03  PTOP1-IDPRODNR-IN     PIC X(7).                                
043700       03  PTOP1-IDPRODNR-UT     PIC X(7).                                
043800       03  PTOP1-IDDISTR-UT      PIC X(4).                                
043900       03  PTOP1-IDKUNDNR-UT     PIC X(6).                                
044000       03  PTOP1-KDFRAKT-UT      PIC X(2).                                
044100       03  PTOP1-IDORDNR-UT      PIC X(5).                                
044200       03  PTOP1-KDORDKL-UT      PIC X(1).                                
044300       03  PTOP1-IDDC-UT         PIC X(2).                                
044310       03  PTOP1-PRTVAL-ADRESSFL PIC X(2).                                
044400       03  FILLER                PIC X(134)  VALUE ALL '+'.               
044500     EJECT                                                                
044600*01    -COPY WMFSAREA                                                     
044700     EJECT                                                                
044800******************************************************************        
044900*                                                                         
045000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045100*                                                                         
045200 01    IMS-WS.                                                            
045300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
045400     SKIP3                                                                
045500*                        **** STATUS-KOD FRÅN IMS                         
045600   03    STATUS-WS               PIC XX.                                  
045700     88    SEGMENT-FINNS                     VALUE '  '.                  
045800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
045900     SKIP3                                                                
046000   03    GODK-STATUSKODER.                                                
046100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
046200     SKIP3                                                                
046300 01    SSA1                      PIC X(128).                              
046400 01    SSA2                      PIC X(64).                               
046500     EJECT                                                                
046600*                            IMS FUNKTIONSKODER                           
046700*01    -COPY W0003                                                        
046800     EJECT                                                                
046900*                            DLI INPUT-OUTPUT AREA                        
047000 01    FILLER                    PIC X(16)   VALUE ALL 'G'.               
047100 01    DLI-IO-AREA-1.                                                     
047200   03    IO-AREA-1               PIC X(370)  VALUE SPACE.                 
047300     SKIP3                                                                
047400*  03    EMBB01 -COPY WDK501      -RED IO-AREA-1                          
047500     EJECT                                                                
047600*  03    XXDL11 -COPY WDGX4316    -RED IO-AREA-1                          
047700     EJECT                                                                
047800*     08 AREA -COPY W4I31501    -RED 4316-FILLER -PRE 4316-               
047900     EJECT                                                                
048000*     08 AREA -COPY W4I31401    -RED 4316-FILLER -PRE 4316-B-             
048100     EJECT                                                                
048200*     08 AREA -COPY W4I39801    -RED 4316-FILLER -PRE 4316-C-             
048300     EJECT                                                                
048400 01    FILLER                    PIC X(16)   VALUE ALL 'H'.               
048500 01    DLI-IO-AREA-2.                                                     
048600   03    IO-AREA-2               PIC X(60)  VALUE SPACE.                  
048700     SKIP3                                                                
048800*  03    XXDJ01 -COPY WDGX4305    -RED IO-AREA-2                          
048900     EJECT                                                                
049000*  03    XXDJ11 -COPY WDGX4306    -RED IO-AREA-2                          
049100     EJECT                                                                
049200*  03    XXDJ11 -COPY WDGX4308    -RED IO-AREA-2                          
049300     EJECT                                                                
049400 01    FILLER                    PIC X(16)   VALUE ALL 'I'.               
049500 01    DLI-IO-AREA-3.                                                     
049600   03    IO-AREA-3               PIC X(100)  VALUE SPACE.                 
049700     SKIP3                                                                
049800*  03    XXDK11 -COPY WDGX4312    -RED IO-AREA-3                          
049900     EJECT                                                                
050600 01    FILLER                    PIC X(16)   VALUE 'WDQ301'.              
050800*01           -COPY WDQ301                                                
050900     EJECT                                                                
050910 01    FILLER                    PIC X(16)   VALUE 'WDE601'.              
050920 01    DLI-IO-E601.                                                       
050950*  03  -COPY WDE601                                                       
050960     EJECT                                                                
050970 01    FILLER                    PIC X(16)   VALUE 'WDE4E1'.              
050980 01    DLI-IO-E4E1.                                                       
050990*  03  -COPY WDE4E1                                                       
050991     EJECT                                                                
050992 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
050993 01  DLI-IO-AREA-B601.                                                    
050994*    03  -COPY WDB601                                                     
050995     EJECT                                                                
051000 LINKAGE SECTION.                                                         
051100*01    -COPY W0009     -PRE MSG-                                          
051200     EJECT                                                                
051300*01    -COPY W0009     -PRE ALT-                                          
051400     EJECT                                                                
051500*01    -COPY W0009     -PRE ALT1-                                         
051600     EJECT                                                                
051700*01    -COPY W0009     -PRE ALT2-                                         
051800     EJECT                                                                
051900*01    -COPY W0008     -PRE USEA-                                         
052000        05 FILLER                PIC X.                                   
052100     EJECT                                                                
052200*01    -COPY W0008     -PRE XXDJ-                                         
052300        05 FILLER                PIC X.                                   
052400     EJECT                                                                
052500*01    -COPY W0008     -PRE XXDK-                                         
052600        05 FILLER                PIC X.                                   
052700     EJECT                                                                
052800*01    -COPY W0008     -PRE XXDL-                                         
052900        05 FILLER                PIC X.                                   
053000     EJECT                                                                
053100*01    -COPY W0008     -PRE EMBB-                                         
053200        05 FILLER                PIC X.                                   
053300     EJECT                                                                
053400*01    -COPY W0008     -PRE WDE6-                                         
053500        05 FILLER                PIC X.                                   
053600     EJECT                                                                
053700*01    -COPY W0008     -PRE ORQA-                                         
053800        05 FILLER                PIC X.                                   
053900     EJECT                                                                
053910*01    -COPY W0008     -PRE E4E1-                                         
053920        05 FILLER                PIC X.                                   
053930     EJECT                                                                
053940*01    -COPY W0008     -PRE WDB6-                                         
053950        05 FILLER                PIC X.                                   
053960     EJECT                                                                
054000 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB              
054100                           USEA-PCB                                       
054200                           XXDJ-PCB XXDK-PCB XXDL-PCB EMBB-PCB            
054300                           WDE6-PCB ORQA-PCB E4E1-PCB WDB6-PCB.           
054400                                                                          
054500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB              
054600                           USEA-PCB                                       
054700                           XXDJ-PCB XXDK-PCB XXDL-PCB EMBB-PCB            
054710                           WDE6-PCB ORQA-PCB E4E1-PCB WDB6-PCB.           
054900                                                                          
055000     PERFORM IMS-GET-MSG                                                  
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300       PERFORM A-INIT-SPARA-INPUT                                         
055400       MOVE MFS-IDTRANS  TO WS-IDTRANS                                    
055500       IF WS-EGEN-BILD                                                    
055600         IF MID-W4I30101 NOT = ALL '+'                                    
055700              PERFORM C-INDATA-KOLL                                       
055800            IF  DCS-CDC                                                   
055810            OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                          
055900              IF WS-INDATA-RAETT                                          
056000                 PERFORM D-REL-KOLL                                       
056100                 IF WS-INDATA-RAETT                                       
056200                    PERFORM E-BEARBETA                                    
056300                    PERFORM S99-NAESTA-TRANS                              
056400                 END-IF                                                   
056500              ELSE                                                        
056600                 MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                        
056700                 PERFORM S02-ROER-EJ-FAELT                                
056800                 MOVE MAX-MOD-LAENGD TO MSG-KVLL                          
056900              END-IF                                                      
057000            ELSE                                                          
057100              MOVE INF-7 (INDX) TO MOD-TEMFSINF                           
057200              PERFORM S02-ROER-EJ-FAELT                                   
057300              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
057400            END-IF                                                        
057500         ELSE                                                             
057600            PERFORM S99-NAESTA-TRANS                                      
057700         END-IF                                                           
057800       ELSE                                                               
057900          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
058000       END-IF                                                             
058100                                                                          
058200       EVALUATE TRUE                                                      
058300         WHEN VISA-NAESTA-BILD PERFORM IMS-INSERT-MSG                     
058400         WHEN STARTA-4391      PERFORM IMS-INSERT-MSG-ALT1-PCB            
058500         WHEN STARTA-4392      PERFORM IMS-INSERT-MSG-ALT2-PCB            
058600       END-EVALUATE                                                       
058700     END-IF                                                               
058800                                                                          
058900     MOVE ZERO TO RETURN-CODE                                             
059000     GOBACK                                                               
059100     .                                                                    
059200     EJECT                                                                
059300 A-INIT-SPARA-INPUT SECTION.                                              
059400                                                                          
059500     IF MSG-DUBBLA-TRANSKODER                                             
059600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I30101                 
059700       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
059800       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
059900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
060000       MOVE MSG-IDPFK            TO MFS-IDPFK                             
060100     ELSE                                                                 
060200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I30101                  
060300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
060400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
060500       MOVE ' ' TO MFS-KDTRTYP                                            
060600                   MFS-IDPFK                                              
060700     END-IF                                                               
060800                                                                          
060900     MOVE LOW-VALUE  TO MSG-AREA                                          
060910                        FILLER-MIN                                        
060920     MOVE HIGH-VALUE TO FILLER-MAX                                        
061000     MOVE 'W4O301N1' TO MFS-IDMOD                                         
061100     MOVE '4301' TO MOD-IDTRANS                                           
061200                                                                          
061300     IF SWEDISH-TEXT                                                      
061400       MOVE +1 TO INDX                                                    
061500     ELSE                                                                 
061600       MOVE +2 TO INDX                                                    
061700     END-IF                                                               
061800                                                                          
061900     PERFORM AA-RENSA-FAELT                                               
062000     PERFORM AB-FLYTTA-IDDC                                               
062100     MOVE WS-VISA-NAESTA-BILD            TO WS-MSG-CALL                   
062200     SKIP2                                                                
062300     .                                                                    
062400     EJECT                                                                
062500 AA-RENSA-FAELT           SECTION.                                        
062600                                                                          
062700     MOVE +1 TO RADIND                                                    
062800     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
062900       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR        (RADIND)              
063000                                 MOD-IDPRODNR       (RADIND)              
063100                                 MOD-FLAVVPACK      (RADIND)              
063200                                 MOD-KDKOLLI        (RADIND)              
063300                                 MOD-VKORDBTO       (RADIND)              
063400                                 MOD-KDEMBTYP       (RADIND)              
063500                                 MOD-IDPRODNR-SAMP  (RADIND)              
063600       ADD +1 TO RADIND                                                   
063700     END-PERFORM                                                          
063800                                                                          
063900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
064000                             MOD-TEMFSINF                                 
064100     .                                                                    
064200     SKIP2                                                                
064300 AB-FLYTTA-IDDC           SECTION.                                        
064400                                                                          
064500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
064600     MOVE '001'             TO MSGI-KDCALL                                
064700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
064800     MOVE '4301'            TO MSGI-IDTRANS                               
064900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
065000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065100                                                                          
065200     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
065300                                                                          
065600     IF MSGI-IDLAND-SPR = 'GB'                                            
065700       MOVE +2 TO INDX                                                    
065800     ELSE                                                                 
065900       MOVE +1 TO INDX                                                    
066000     END-IF                                                               
066100                                                                          
066200     IF MSGI-IDDC IS > SPACE                                              
066300       MOVE MSGI-IDDC           TO MOD-IDDC-UT                            
066400     ELSE                                                                 
066500       MOVE NEJ                 TO NYCKLAR-SW                             
066600     END-IF                                                               
066700                                                                          
066800     IF NYCKLAR-OK                                                        
066810        MOVE MSGI-IDDC TO W-IDDC-B6                                       
066820        PERFORM IMS-GU-WDB601                                             
066830     ELSE                                                                 
066900        MOVE FEL-7 (INDX) TO MOD-TEMFSFEL                                 
067000        PERFORM MFS-ROER-EJ-FAELT-MOD-INFAELT                             
067100     END-IF                                                               
067200     .                                                                    
067300     SKIP2                                                                
067400 C-INDATA-KOLL SECTION.                                                   
067500                                                                          
067600     MOVE +1 TO RADIND                                                    
067700                                                                          
067800     MOVE RAETT TO WS-INDATA-TEST                                         
067900                                                                          
068000     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
068100                                                                          
068200       IF MID-RAD (RADIND) NOT = ALL '+'                                  
068300                                                                          
068400         IF MID-IDDISTR (RADIND) = ALL '+'                                
068500           MOVE FEL TO WS-INDATA-TEST                                     
068600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)            
068700         ELSE                                                             
068800                                                                          
068900           IF MID-IDDISTR (RADIND) NUMERIC                                
069000             MOVE MID-IDDISTR (RADIND) TO TEST-IDDISTR                    
069100                                                                          
069200               IF DIST19-SATS                                             
069300                 MOVE FEL TO WS-INDATA-TEST                               
069400                 MOVE MFS-NUM-FAELT-FEL TO                                
069500                          MOD-IDDISTR-ATTR(RADIND)                        
069600               ELSE                                                       
069700                                                                          
069800                 IF DIST03-SVERIGE                                        
069900                   MOVE MFS-NUM-FAELT-RAETT TO                            
070000                           MOD-IDDISTR-ATTR (RADIND)                      
070100                 ELSE                                                     
070200                   MOVE FEL TO WS-INDATA-TEST                             
070300                   MOVE MFS-NUM-FAELT-FEL                                 
070400                        TO MOD-IDDISTR-ATTR(RADIND)                       
070500                 END-IF                                                   
070600               END-IF                                                     
070700           ELSE                                                           
070800             MOVE FEL TO WS-INDATA-TEST                                   
070900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)          
071000           END-IF                                                         
071100         END-IF                                                           
071200                                                                          
071300         IF MID-IDPRODNR (RADIND) = ALL '+'                               
071400           MOVE FEL TO WS-INDATA-TEST                                     
071500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIND)         
071600         ELSE                                                             
071700                                                                          
071800           IF MID-IDPRODNR (RADIND) NUMERIC                               
071900             MOVE MFS-NUM-FAELT-RAETT TO                                  
072000                               MOD-IDPRODNR-ATTR (RADIND)                 
072100           ELSE                                                           
072200             MOVE FEL TO WS-INDATA-TEST                                   
072300             MOVE MFS-NUM-FAELT-FEL                                       
072400                   TO MOD-IDPRODNR-ATTR(RADIND)                           
072500           END-IF                                                         
072600         END-IF                                                           
072700                                                                          
072800         IF MID-FLAVVPACK (RADIND) = ALL '+'                              
072900         CONTINUE                                                         
073000         ELSE                                                             
073100                                                                          
073200           IF MID-FLAVVPACK (RADIND) = 'J' OR 'Y'                         
073300              MOVE MFS-ALFA-FAELT-RAETT TO                                
073400                                MOD-FLAVVPACK-ATTR(RADIND)                
073500           ELSE                                                           
073600             MOVE FEL                TO WS-INDATA-TEST                    
073700             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAVVPACK-ATTR(RADIND)        
073800           END-IF                                                         
073900         END-IF                                                           
074000                                                                          
074100                                                                          
074200         IF MID-KDEMBTYP (RADIND) = ALL '+'                               
074300         CONTINUE                                                         
074400         ELSE                                                             
074500          IF (MID-KDEMBTYP (RADIND) NUMERIC)                              
074600             AND (MID-KDEMBTYP (RADIND) > 0 AND < 8)                      
074700             MOVE MFS-NUM-FAELT-RAETT TO                                  
074800                               MOD-KDEMBTYP-ATTR (RADIND)                 
074900          ELSE                                                            
075000             MOVE FEL                   TO WS-INDATA-TEST                 
075100             MOVE MFS-NUM-FAELT-FEL TO                                    
075200                               MOD-KDEMBTYP-ATTR (RADIND)                 
075300          END-IF                                                          
075400         END-IF                                                           
075500                                                                          
075600         IF MID-VKORDBTO (RADIND) = ALL '+'                               
075700           MOVE 0                   TO WSTAB-VKORDBTO (RADIND)            
075800         ELSE                                                             
075900             MOVE MID-VKORDBTO (RADIND) TO DEC-IDFRIDATA                  
076000             MOVE 5                      TO DEC-KVHELTAL                  
076100             MOVE 1                      TO DEC-KVDECIMAL                 
076200             CALL WDECEDIT USING DEC-WDECAREA                             
076300                                                                          
076400             IF DEC-KDSVAR-OK                                             
076500                MOVE MFS-NUM-FAELT-RAETT TO                               
076600                            MOD-VKORDBTO-ATTR (RADIND)                    
076700               MOVE DEC-IDEDITDATA TO WS-IDEDITDATA                       
076800                                                                          
076900               IF  WS-VKORDBTO-DEC > 0                                    
077000                 MOVE WS-IDEDITDATA TO WS-VKORDBTO-RED                    
077100                 MOVE WS-VKORDBTO-X TO WSTAB-VKORDBTO-X (RADIND)          
077200               ELSE                                                       
077300                 MOVE WS-IDEDITDATA TO WSTAB-VKORDBTO (RADIND)            
077400               END-IF                                                     
077500             ELSE                                                         
077600                MOVE FEL TO WS-INDATA-TEST                                
077700                MOVE MFS-NUM-FAELT-FEL TO                                 
077800                              MOD-VKORDBTO-ATTR (RADIND)                  
077900             END-IF                                                       
078000         END-IF                                                           
078100                                                                          
078200                                                                          
078300        IF MID-KDKOLLI(RADIND) = ALL '+'                                  
078400        CONTINUE                                                          
078500        ELSE                                                              
078600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOLLI-ATTR(RADIND)          
078700        END-IF                                                            
078800                                                                          
078900        IF MID-IDPRODNR-SAMP (RADIND) = ALL '+'                           
079000        CONTINUE                                                          
079100        ELSE                                                              
079200                                                                          
079300          IF MID-IDPRODNR-SAMP (RADIND) NUMERIC                           
079400            MOVE MFS-NUM-FAELT-RAETT TO                                   
079500                              MOD-IDPRODNR-SAMP-ATTR (RADIND)             
079600          ELSE                                                            
079700            MOVE FEL TO WS-INDATA-TEST                                    
079800            MOVE MFS-NUM-FAELT-FEL                                        
079900                  TO MOD-IDPRODNR-SAMP-ATTR(RADIND)                       
080000          END-IF                                                          
080100        END-IF                                                            
080200      END-IF                                                              
080300      ADD +1 TO RADIND                                                    
080400     END-PERFORM                                                          
080500                                                                          
080600     IF WS-INDATA-RAETT                                                   
080700**** KONTROLL AV ATT INGA DUBLETTER (IDPRODNR) RAPPORTERAS                
080800                                                                          
080900        MOVE +1 TO RADIND                                                 
081000        PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                      
081100                                                                          
081200           IF MID-IDPRODNR(RADIND) NOT = ALL '+'                          
081300             MOVE RADIND TO JMF-IND                                       
081400             ADD +1      TO JMF-IND                                       
081500                                                                          
081600             PERFORM UNTIL JMF-IND NOT < MAX-RADIND-PLUS-1                
081700                                                                          
081800               IF MID-IDPRODNR(JMF-IND) NOT = ALL '+'                     
081900                                                                          
082000                  IF MID-IDPRODNR(RADIND) = MID-IDPRODNR(JMF-IND)         
082100                     MOVE FEL TO WS-INDATA-TEST                           
082200                     MOVE MFS-NUM-FAELT-FEL TO                            
082300                         MOD-IDPRODNR-ATTR(RADIND)                        
082400                     MOVE MFS-NUM-FAELT-FEL TO                            
082500                            MOD-IDPRODNR-ATTR(JMF-IND)                    
082600                  END-IF                                                  
082700              END-IF                                                      
082800              ADD +1 TO JMF-IND                                           
082900            END-PERFORM                                                   
083000          END-IF                                                          
083100          ADD +1 TO RADIND                                                
083200         END-PERFORM                                                      
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 D-REL-KOLL SECTION.                                                      
083700                                                                          
083800     PERFORM DD-LADDA-PRODNUMMER-TABELLEN                                 
083900     MOVE +1 TO RADIND                                                    
084000                                                                          
084100     MOVE MSGI-IDDC             TO W-IDDC-4305                            
084200     PERFORM IMS-GET-XXDJ-4305                                            
084300                                                                          
084400     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
084500                                                                          
084600       IF MID-RAD (RADIND) NOT = ALL '+'                                  
084700                                                                          
084800****** FLYTTAR NYCKLAR                                                    
084900         MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                    
085000                                       W-IDPRODNR-WDE6                    
085100                                       W-IDPRODNR-WDQ3D                   
085110                                       W-IDPRODNR-MIN                     
085120                                       W-IDPRODNR-MAX                     
085200         IF MID-KDKOLLI (RADIND) NOT = ALL '+'                            
085300           MOVE MID-KDKOLLI  (RADIND) TO W-KDKOLLI-WDK5                   
085400         END-IF                                                           
085500******                                                                    
085600                                                                          
085700        PERFORM IMS-GET-WDE601                                            
085800        IF SEGMENT-FINNS                                                  
085900          MOVE VORD-IDDISTR          TO SPAR-IDDISTR                      
086000          MOVE VORD-IDKUNDNR         TO SPAR-IDKUNDNR                     
086100          MOVE VORD-KDFRAKT          TO SPAR-KDFRAKT                      
086200                                                                          
086300           IF VORD-IDDC = W-IDDC-4305                                     
086400                                                                          
086500           IF MID-IDDISTR (RADIND) = VORD-IDDISTR                         
086600                                                                          
086700              IF VORD-KDORDSTA = 1                                        
086800                                                                          
086900                 IF VORD-FLMANORD = NEJ                                   
087000                                                                          
087100                    IF VORD-KVORDRAD > MAX-ANT-RAD                        
087200                       MOVE FEL-NR6 TO WS-INDATA-TEST                     
087300                       MOVE MFS-NUM-FAELT-FEL                             
087400                                    TO MOD-IDPRODNR-ATTR (RADIND)         
087500                    END-IF                                                
087600                   MOVE VORD-IDDISTR TO WSWDE6-IDDISTR (RADIND)           
087700                   MOVE VORD-IDKUNDNR TO WSWDE6-IDKUNDNR (RADIND)         
087800                   MOVE VORD-KDORDKL TO WSWDE6-KDORDKL (RADIND)           
087900                   MOVE VORD-KVORDRAD TO WSWDE6-KVORDRAD (RADIND)         
088000                   MOVE VORD-KDFRAKT TO WSWDE6-KDFRAKT (RADIND)           
088100                                                                          
088210                   PERFORM IMS-GU-WDE4E1                                  
088300                   IF SEGMENT-FINNS                                       
088400                      MOVE SEQE-IDKUNDRF TO                               
088500                                 WSWDE6-IDKUNDRF (RADIND)                 
088600                   ELSE                                                   
088700                      MOVE '++++++++++' TO                                
088800                                 WSWDE6-IDKUNDRF (RADIND)                 
088900                   END-IF                                                 
089000                 ELSE                                                     
089100                    MOVE FEL-NR1 TO WS-INDATA-TEST                        
089200                    MOVE MFS-NUM-FAELT-FEL TO                             
089300                                       MOD-IDPRODNR-ATTR (RADIND)         
089400                 END-IF                                                   
089500              ELSE                                                        
089600                 MOVE FEL-NR1 TO WS-INDATA-TEST                           
089700                 MOVE MFS-NUM-FAELT-FEL TO                                
089800                                    MOD-IDPRODNR-ATTR (RADIND)            
089900              END-IF                                                      
090000           ELSE                                                           
090100              MOVE FEL-NR1 TO WS-INDATA-TEST                              
090200              MOVE MFS-NUM-FAELT-FEL TO                                   
090300                                 MOD-IDPRODNR-ATTR (RADIND)               
090400           END-IF                                                         
090500           ELSE                                                           
090600              MOVE FEL-NR1 TO WS-INDATA-TEST                              
090700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)         
090800           END-IF                                                         
090900        ELSE                                                              
091000           MOVE FEL-NR1 TO WS-INDATA-TEST                                 
091100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)           
091200        END-IF                                                            
091300                                                                          
091400        PERFORM DE-KONTROLL-FLERA-ORDERDELAR                              
091500                                                                          
091600        IF WS-INDATA-RAETT                                                
091700          IF MID-IDPRODNR-SAMP (RADIND) = ALL '+'                         
091800            EVALUATE TRUE                                                 
091900            WHEN MID-KDKOLLI (RADIND) = ALL '+'   AND                     
092000                 MID-KDEMBTYP(RADIND) = ALL '+'                           
092100                                                                          
092200              IF MID-VKORDBTO(RADIND) = ALL '+'                           
092300              CONTINUE                                                    
092400              ELSE                                                        
092500                MOVE FEL-NR1 TO WS-INDATA-TEST                            
092600                MOVE MFS-NUM-FAELT-FEL TO                                 
092700                     MOD-VKORDBTO-ATTR (RADIND)                           
092800              END-IF                                                      
092900                                                                          
093000            WHEN                                                          
093100               MID-KDKOLLI (RADIND) NOT = ALL '+'   AND                   
093200               MID-KDEMBTYP(RADIND) NOT = ALL '+'                         
093300                 MOVE FEL-NR1 TO WS-INDATA-TEST                           
093400                 MOVE MFS-NUM-FAELT-FEL TO                                
093500                      MOD-KDEMBTYP-ATTR (RADIND)                          
093600            WHEN MID-KDEMBTYP(RADIND) = ALL '+'                           
093700                  PERFORM DA-KOLLA-KOLLIREG                               
093800            END-EVALUATE                                                  
093900          ELSE                                                            
094000            PERFORM DB-KONTROLL-SAMPACKNING                               
094100          END-IF                                                          
094200        END-IF                                                            
094300                                                                          
094400       PERFORM IMS-GET-XXDJ-4306-STAT-GE                                  
094500       IF SEGMENT-FINNS                                                   
094600                                                                          
094700          EVALUATE TRUE                                                   
094800                                                                          
094900          WHEN 4306-KDPACLAS = 0                                          
095000             CONTINUE                                                     
095100          WHEN 4306-KDPACLAS = 1 OR 2                                     
095200             MOVE FEL-NR1 TO WS-INDATA-TEST                               
095300             MOVE MFS-NUM-FAELT-FEL TO                                    
095400                                MOD-IDPRODNR-ATTR (RADIND)                
095500             MOVE INF-3 (INDX) TO MOD-TEMFSINF                            
095600          WHEN 4306-KDPACLAS = 3                                          
095700             MOVE FEL-NR1 TO WS-INDATA-TEST                               
095800             MOVE MFS-NUM-FAELT-FEL TO                                    
095900                                MOD-IDPRODNR-ATTR (RADIND)                
096000             MOVE INF-4  (INDX) TO MOD-TEMFSINF                           
096100          WHEN 4306-KDPACLAS = 5                                          
096200             MOVE FEL-NR1 TO WS-INDATA-TEST                               
096300             MOVE MFS-NUM-FAELT-FEL TO                                    
096400                                MOD-IDPRODNR-ATTR (RADIND)                
096500             MOVE INF-1  (INDX) TO MOD-TEMFSINF                           
096600          WHEN OTHER                                                      
096700             MOVE FEL-NR1 TO WS-INDATA-TEST                               
096800             MOVE MFS-NUM-FAELT-FEL TO                                    
096900                                MOD-IDPRODNR-ATTR (RADIND)                
097000             MOVE INF-5  (INDX) TO MOD-TEMFSINF                           
097100          END-EVALUATE                                                    
097200        END-IF                                                            
097300      END-IF                                                              
097400      ADD +1 TO RADIND                                                    
097500     END-PERFORM                                                          
097600                                                                          
097700     IF NOT WS-INDATA-RAETT                                               
097800       PERFORM DC-LAGG-UT-FELTEXT                                         
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200 DA-KOLLA-KOLLIREG SECTION.                                               
098300                                                                          
098400     PERFORM IMS-GET-EMBB                                                 
098500     IF SEGMENT-FINNS                                                     
098600        MOVE EMB-KDEMBTYP       TO WSEMB-KDEMBTYP (RADIND)                
098700     ELSE                                                                 
098800        MOVE FEL-NR1            TO WS-INDATA-TEST                         
098900        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-ATTR(RADIND)               
099000     END-IF                                                               
099100     SKIP2                                                                
099200     .                                                                    
099300     EJECT                                                                
099400 DB-KONTROLL-SAMPACKNING         SECTION.                                 
099500                                                                          
099600     PERFORM DBA-KONTROLL-NR1                                             
099700     PERFORM DBB-KONTROLL-NR2                                             
099800     PERFORM DBC-KONTROLL-NR3                                             
099900     .                                                                    
100000     EJECT                                                                
100100 DBA-KONTROLL-NR1                  SECTION.                               
100200                                                                          
100300****************************************************************          
100400*                                                              *          
100500* MAN FÅR INTE ANGE KOLLI-INFORMATION FÖR EN ORDER SOM MAN     *          
100600* ANGETT IDPRODNR-SAMP FÖR.                                    *          
100700*                                                              *          
100800****************************************************************          
100900                                                                          
101000     IF MID-KDKOLLI  (RADIND) = ALL '+'  AND                              
101100        MID-VKORDBTO (RADIND) = ALL '+'  AND                              
101200        MID-KDEMBTYP (RADIND) = ALL '+'                                   
101300        CONTINUE                                                          
101400     ELSE                                                                 
101500       MOVE FEL-NR1 TO WS-INDATA-TEST                                     
101600       MOVE MFS-NUM-FAELT-FEL TO                                          
101700            MOD-IDPRODNR-SAMP-ATTR (RADIND)                               
101800     END-IF                                                               
101900     SKIP2                                                                
102000     .                                                                    
102100     EJECT                                                                
102200 DBB-KONTROLL-NR2                  SECTION.                               
102300                                                                          
102400****************************************************************          
102500*                                                              *          
102600* HÄR KONTROLLERAR MAN ATT DEN ORDER MAN SAMPACKAR MED         *          
102700* VERKLIGEN FINNS.                                             *          
102800*                                                              *          
102900* PLUS NÅGRA YTTERLIGARE TESTER, SE RESPEKTIVE SEKTION.        *          
103000*                                                              *          
103100****************************************************************          
103200                                                                          
103300     MOVE MID-IDPRODNR-SAMP (RADIND) TO W-IDPRODNR-WDE6                   
103400     PERFORM IMS-GET-WDE601                                               
103500                                                                          
103600     IF SEGMENT-SAKNAS                                                    
103700       MOVE FEL-NR1 TO WS-INDATA-TEST                                     
103800       MOVE MFS-NUM-FAELT-FEL TO                                          
103900            MOD-IDPRODNR-SAMP-ATTR (RADIND)                               
104000     ELSE                                                                 
104100       PERFORM DBBA-KONTROLL-2A                                           
104200                                                                          
104300       IF WS-INDATA-RAETT                                                 
104400         PERFORM DBBB-KONTROLL-2B                                         
104500       END-IF                                                             
104600     END-IF                                                               
104700     SKIP2                                                                
104800     .                                                                    
104900     EJECT                                                                
105000 DBBA-KONTROLL-2A         SECTION.                                        
105100                                                                          
105200****************************************************************          
105300*                                                              *          
105400* HÄR KONTROLLERAR MAN ATT MAN INTE SAMPACKAR MED EN ORDER SOM *          
105500* VARS FRAKTSEDEL REDAN HAR BLIVIT UTSKRIVEN.                  *          
105600*                                                              *          
105700****************************************************************          
105800                                                                          
105900     IF VORD-KDORDSTA > 2 AND VORD-FLFRAKTS = NEJ                         
106000       MOVE FEL-NR4 TO WS-INDATA-TEST                                     
106100       MOVE MFS-NUM-FAELT-FEL TO                                          
106200            MOD-IDPRODNR-SAMP-ATTR (RADIND)                               
106300     END-IF                                                               
106400     SKIP2                                                                
106500     .                                                                    
106600     EJECT                                                                
106700 DBBB-KONTROLL-2B       SECTION.                                          
106800                                                                          
106900****************************************************************          
107000*                                                              *          
107100* MAN KONTROLLERAR DESSUTOM ATT MAN INTE SAMPACKAR MED EN      *          
107200* ORDER SOM TIDIGARE SAMPACKATS MED EN ANNAN ORDER.            *          
107300*                                                              *          
107400* HÄR KONTROLLERAR MAN ATT DE ORDER MAN SAMPACKAR ÄR SAMMA     *          
107500* DISTRIKT, KUND OCH FRAKTKOD.                                 *          
107600*                                                              *          
107700****************************************************************          
107800                                                                          
107900     EVALUATE TRUE                                                        
108000     WHEN VORD-IDPRODNR NOT = VORD-IDPRODNR-SAMP                          
108100       MOVE FEL-NR3 TO WS-INDATA-TEST                                     
108200       MOVE MFS-NUM-FAELT-FEL TO                                          
108300            MOD-IDPRODNR-SAMP-ATTR (RADIND)                               
108400     WHEN VORD-IDDISTR  = SPAR-IDDISTR    AND                             
108500          VORD-IDKUNDNR = SPAR-IDKUNDNR   AND                             
108600          VORD-KDFRAKT  = SPAR-KDFRAKT                                    
108700          CONTINUE                                                        
108800     WHEN OTHER                                                           
108900       MOVE FEL-NR1 TO WS-INDATA-TEST                                     
109000       MOVE MFS-NUM-FAELT-FEL TO                                          
109100            MOD-IDPRODNR-SAMP-ATTR (RADIND)                               
109200     END-EVALUATE                                                         
109300     SKIP2                                                                
109400     .                                                                    
109500     EJECT                                                                
109600 DBC-KONTROLL-NR3                  SECTION.                               
109700                                                                          
109800****************************************************************          
109900*                                                              *          
110000* HÄR KONTROLLERAR MAN ATT MAN INTE SAMPACKAR MED EN ORDER SOM *          
110100* I SIN TUR ÄR SAMPACKAD MED EN ANNAN ORDER.                   *          
110200*                                                              *          
110300****************************************************************          
110400                                                                          
110500     MOVE JA                            TO TRAFF                          
110600     SET T1-IX                          TO 1                              
110700                                                                          
110800     SEARCH TABELL-MED-PRODNR AT END                                      
110900         MOVE NEJ                       TO TRAFF                          
111000       WHEN MID-IDPRODNR-SAMP (RADIND) = T1-IDPRODNR (T1-IX)              
111100       CONTINUE                                                           
111200     END-SEARCH                                                           
111300                                                                          
111400                                                                          
111500     IF TRAFF = JA                                                        
111600       IF T1-IDPRODNR-SAMP (T1-IX) = 0                                    
111700         CONTINUE                                                         
111800       ELSE                                                               
111900         MOVE FEL-NR3                   TO WS-INDATA-TEST                 
112000         MOVE MFS-NUM-FAELT-FEL TO                                        
112100              MOD-IDPRODNR-SAMP-ATTR (RADIND)                             
112200       END-IF                                                             
112300     END-IF                                                               
112400     EJECT                                                                
112500     .                                                                    
112600 DC-LAGG-UT-FELTEXT       SECTION.                                        
112700                                                                          
112800     PERFORM S02-ROER-EJ-FAELT                                            
112900     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
113000                                                                          
113100     EVALUATE TRUE                                                        
113200     WHEN   WS-INDATA-FEL-NR1                                             
113300       MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                                  
113400     WHEN   WS-INDATA-FEL-NR2                                             
113500       MOVE FEL-2 (INDX) TO MOD-TEMFSFEL                                  
113600     WHEN   WS-INDATA-FEL-NR3                                             
113700       MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                                  
113800     WHEN   WS-INDATA-FEL-NR4                                             
113900       MOVE FEL-4 (INDX) TO MOD-TEMFSFEL                                  
114000     WHEN   WS-INDATA-FEL-NR5                                             
114100       MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                                  
114200     WHEN   WS-INDATA-FEL-NR6                                             
114300       MOVE FEL-6 (INDX) TO MOD-TEMFSFEL                                  
114400     WHEN   WS-INDATA-FEL-NR8                                             
114500       MOVE FEL-8 (INDX) TO MOD-TEMFSFEL                                  
114600     END-EVALUATE                                                         
114700     EJECT                                                                
114800     .                                                                    
114900 DD-LADDA-PRODNUMMER-TABELLEN      SECTION.                               
115000                                                                          
115100     MOVE +1                        TO RADIND                             
115200     SET T1-IX                      TO 1                                  
115300                                                                          
115400     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
115500       IF MID-IDPRODNR (RADIND) = ALL '+'                                 
115600         MOVE 0                          TO                               
115700              T1-IDPRODNR (T1-IX)                                         
115800       ELSE                                                               
115900         MOVE MID-IDPRODNR (RADIND)      TO                               
116000              T1-IDPRODNR (T1-IX)                                         
116100       END-IF                                                             
116200                                                                          
116300       IF MID-IDPRODNR-SAMP (RADIND) = ALL '+'                            
116400         MOVE 0                          TO                               
116500              T1-IDPRODNR-SAMP (T1-IX)                                    
116600       ELSE                                                               
116700         MOVE MID-IDPRODNR-SAMP (RADIND) TO                               
116800              T1-IDPRODNR-SAMP (T1-IX)                                    
116900       END-IF                                                             
117000                                                                          
117100       ADD +1                       TO RADIND                             
117200       SET T1-IX UP BY 1                                                  
117300     END-PERFORM                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 DE-KONTROLL-FLERA-ORDERDELAR           SECTION.                          
117700                                                                          
117800** SOFTWARE KONTROLL                                                      
117900     IF WS-INDATA-RAETT                                                   
118000       PERFORM IMS-GU-WDQ3D1                                              
118100                                                                          
118200       IF SEGMENT-FINNS                                                   
118300         IF W-IDPRODNR-WDQ3D     = ODEL-IDPRODNR     AND                  
118400           (ODEL-IDLEVNR = '1441 ' OR 'BP2TW') AND                        
118500            ODEL-IDPRC    = '9998'                                        
118600            MOVE FEL-NR8 TO WS-INDATA-TEST                                
118700            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
118800         END-IF                                                           
118900       END-IF                                                             
119000     END-IF                                                               
119100                                                                          
119200     IF WS-INDATA-RAETT                                                   
119300       PERFORM IMS-GU-WDQ3D1                                              
119400       PERFORM IMS-GN-WDQ3D1                                              
119500                                                                          
119600       IF SEGMENT-FINNS                                                   
119700         IF W-IDPRODNR-WDQ3D     = ODEL-IDPRODNR     AND                  
119800            MID-IDDISTR (RADIND) = ODEL-IDDISTR                           
119900            MOVE FEL-NR5 TO WS-INDATA-TEST                                
120000            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
120100         END-IF                                                           
120200       END-IF                                                             
120300     END-IF                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 E-BEARBETA SECTION.                                                      
120700***** - LÅSER ORDERN I LÅSNINGSREG. HTW4306.                              
120800***** - SKAPAR ETT SEGM ORDER PÅ ORDER-UNDER-ARBETE-BASEN,                
120900*****    HTR4312, SOM EV. TAS BORT DIREKT EFTER BEARBETNINGEN             
121000                                                                          
121100     MOVE +1 TO RADIND                                                    
121200                                                                          
121300     MOVE MSGI-IDDC             TO W-IDDC-4305                            
121400                                   W-IDDC-4311                            
121500     PERFORM IMS-GET-XXDJ-4305                                            
121600     PERFORM IMS-GET-XXDK-4311                                            
121700     PERFORM IMS-GET-XXDL-4315                                            
121800                                                                          
121900                                                                          
122000     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
122100                                                                          
122200        IF MID-RAD (RADIND) NOT = ALL '+'                                 
122300                                                                          
122400***** FLYTTAR NYCKLAR                                                     
122500           MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                  
122600                                         W-IDPRODNR-4312                  
122700                                         W-IDPRODNR-4316                  
122800                                                                          
122900*****                                                                     
123000          PERFORM IMS-GET-XXDJ-4306-STAT-GE                               
123100          IF SEGMENT-FINNS                                                
123200                                                                          
123300            IF 4306-KDPACLAS = 0                                          
123400              MOVE +1 TO 4306-KDPACLAS                                    
123500              PERFORM IMS-REPL-XXDJ                                       
123600            END-IF                                                        
123700          ELSE                                                            
123800              MOVE MID-IDPRODNR(RADIND)  TO 4306-IDPRODNR                 
123900              MOVE LOW-VALUE             TO 4306-LOWVALUE                 
124000              MOVE NEJ                   TO 4306-FLANNULL                 
124100              MOVE +1                    TO 4306-KDPACLAS                 
124200              PERFORM IMS-ISRT-XXDJ-4306                                  
124300          END-IF                                                          
124400                                                                          
124500          PERFORM EA-SKAPA-4312                                           
124600          PERFORM IMS-ISRT-XXDK-4312                                      
124700                                                                          
124800          IF MID-KDKOLLI       (RADIND) = ALL '+'  AND                    
124900             MID-VKORDBTO      (RADIND) = ALL '+'  AND                    
125000             MID-KDEMBTYP      (RADIND) = ALL '+'  AND                    
125100             MID-IDPRODNR-SAMP (RADIND) = ALL '+'                         
125200             CONTINUE                                                     
125300          ELSE                                                            
125400            PERFORM EB-SKAPA-4316-GEMEN                                   
125500            PERFORM EI-SKRIV-4316-FRAKTS                                  
125600          END-IF                                                          
125700                                                                          
125800          IF MID-FLAVVPACK (RADIND) = ALL '+'                             
125900             PERFORM ED-SKAPA-4316                                        
126000          ELSE                                                            
126100             PERFORM EB-SKAPA-4316-GEMEN                                  
126200             PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                        
126300          END-IF                                                          
126400                                                                          
126500          PERFORM EE-SKAPA-4316-004                                       
126600          PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                           
126700                                                                          
126800          PERFORM EF-KOLLA-4312                                           
126900        END-IF                                                            
127000        ADD +1 TO RADIND                                                  
127100     END-PERFORM                                                          
127200     .                                                                    
127300     EJECT                                                                
127400 EA-SKAPA-4312 SECTION.                                                   
127500                                                                          
127600     MOVE MID-IDPRODNR      (RADIND) TO 4312-IDPRODNR                     
127700     MOVE LOW-VALUE                  TO 4312-LOWVALUE                     
127800     MOVE MSG-SIGNON-USERID          TO 4312-IDUSER                       
127900     MOVE WSWDE6-IDDISTR  (RADIND)   TO 4312-IDDISTR                      
128000     MOVE WSWDE6-IDKUNDNR (RADIND)   TO 4312-IDKUNDNR                     
128100     MOVE WSWDE6-IDKUNDRF (RADIND)   TO 4312-IDKUNDRF                     
128200     MOVE WSWDE6-KDORDKL  (RADIND)   TO 4312-KDORDKL                      
128300     MOVE WSWDE6-KVORDRAD (RADIND)   TO 4312-KVORDRAD                     
128400     MOVE WSWDE6-KDFRAKT  (RADIND)   TO 4312-KDFRAKT                      
128500     ACCEPT 4312-TIDATUM FROM DATE                                        
128600     ACCEPT 4312-TIKLOCK FROM TIME                                        
128700                                                                          
128800     MOVE '4301'                     TO 4312-IDTRANS                      
128900     MOVE +1                         TO 4312-KDBEHAND-GRUND               
129000     MOVE +0                         TO 4312-KDBEHAND-RAD                 
129100     MOVE +0                         TO 4312-KDBEHAND-DEL                 
129200     MOVE +0                         TO 4312-KDBEHAND-URS                 
129300                                                                          
129400     IF MID-FLAVVPACK (RADIND) = 'J' OR 'Y'                               
129500       MOVE +1                       TO 4312-KDBEHAND-AVVIK               
129600     ELSE                                                                 
129700       MOVE +4                       TO 4312-KDBEHAND-AVVIK               
129800     END-IF                                                               
129900                                                                          
130000     IF MID-KDKOLLI       (RADIND) = ALL '+' AND                          
130100        MID-VKORDBTO      (RADIND) = ALL '+' AND                          
130200        MID-KDEMBTYP      (RADIND) = ALL '+' AND                          
130300        MID-IDPRODNR-SAMP (RADIND) = ALL '+'                              
130400       MOVE +1                       TO 4312-KDBEHAND-KOL                 
130500     ELSE                                                                 
130600       MOVE +0                       TO 4312-KDBEHAND-KOL                 
130700     END-IF                                                               
130800     .                                                                    
130900     EJECT                                                                
131000 EB-SKAPA-4316-GEMEN SECTION.                                             
131100                                                                          
131200     MOVE ALL '+'                    TO 4316-WDGX4316                     
131300     MOVE MID-IDPRODNR(RADIND)       TO 4316-IDPRODNR                     
131400     MOVE '002'                      TO 4316-IDPTYP                       
131500     MOVE LOW-VALUE                  TO 4316-LOWVALUE                     
131600     MOVE ZERO                       TO 4316-KDTRSTAT                     
131700     MOVE +312                       TO 4316-LL                           
131800     MOVE LOW-VALUE                  TO 4316-Z1                           
131900     MOVE LOW-VALUE                  TO 4316-Z2                           
132000     MOVE 'W4T315'                   TO 4316-KDTRANS                      
132100     MOVE '4301'                     TO 4316-IDTRANS                      
132200     MOVE MFS-KDMFSFOR               TO 4316-KDMFSFOR                     
132300     MOVE +1                         TO 4316-IDKOLLI                      
132400     MOVE MSGI-IDDC                  TO 4316-MID-IDDC-IN                  
132500                                        4316-MID-IDDC-UT                  
132600                                                                          
132700     MOVE '00001'                    TO 4316-MID-IDKOLLI-IN               
132800     MOVE '00001'                    TO 4316-MID-IDKOLLI-UT               
132900                                                                          
133000     MOVE MID-IDPRODNR (RADIND)      TO 4316-MID-IDPRODNR-IN              
133100                                        4316-MID-IDPRODNR-UT              
133200                                                                          
133300     MOVE WSWDE6-IDKUNDRF(RADIND)    TO 4316-MID-IDORDNR-IN               
133400                                        4316-MID-IDORDNR-UT               
133500                                                                          
133600     MOVE ZERO                       TO 4316-MID-IDANSTNR-UT              
133700                                                                          
133800     MOVE MID-IDDISTR(RADIND)        TO 4316-MID-IDDISTR-IN               
133900                                        4316-MID-IDDISTR-UT               
134000                                                                          
134100     MOVE WSWDE6-IDKUNDNR(RADIND)    TO WS-IDKUNDNR                       
134200     MOVE WS-IDKUNDNR                TO 4316-MID-IDKUNDNR-IN              
134300                                        4316-MID-IDKUNDNR-UT              
134400     MOVE 'U'                        TO 4316-MID-PRTVAL-ADRESSFL          
134500                                        4316-MID-PRTVAL-FOLJEFL           
134600     .                                                                    
134700     EJECT                                                                
134800 EC-SKAPA-4316-NAESTA SECTION.                                            
134900                                                                          
135000        MOVE LOW-VALUE               TO IO-AREA-1                         
135100        MOVE ALL '+'                 TO 4316-WDGX4316                     
135200        MOVE MID-IDPRODNR   (RADIND) TO 4316-IDPRODNR                     
135300        MOVE '003'                   TO 4316-IDPTYP                       
135400        MOVE LOW-VALUE               TO 4316-LOWVALUE                     
135500        MOVE ZERO                    TO 4316-KDTRSTAT                     
135600        MOVE +272                    TO 4316-LL                           
135700        MOVE LOW-VALUE               TO 4316-Z1                           
135800        MOVE LOW-VALUE               TO 4316-Z2                           
135900        MOVE 'W4T314'                TO 4316-KDTRANS                      
136000        MOVE '4301'                  TO 4316-IDTRANS                      
136100        MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                     
136200                                                                          
136300        MOVE ZERO                    TO 4316-B-MID-IDANSTNR-UT            
136400                                                                          
136500        MOVE MID-IDDISTR    (RADIND) TO 4316-B-MID-IDDISTR-IN             
136600                                        4316-B-MID-IDDISTR-UT             
136700                                                                          
136800        MOVE 4312-IDKUNDNR           TO WS-IDKUNDNR                       
136900        MOVE WS-IDKUNDNR             TO 4316-B-MID-IDKUNDNR-IN            
137000                                        4316-B-MID-IDKUNDNR-UT            
137100                                                                          
137200        MOVE WSWDE6-IDKUNDRF (RADIND) TO 4316-B-MID-IDORDNR-IN            
137300                                         4316-B-MID-IDORDNR-UT            
137400                                                                          
137500        MOVE +1                   TO 4316-IDKOLLI                         
137600                                                                          
137700        MOVE MSGI-IDDC            TO 4316-B-MID-IDDC-IN                   
137800                                     4316-B-MID-IDDC-UT                   
137900                                                                          
138000        MOVE '00001'              TO 4316-B-MID-IDKOLLI-IN                
138100                                     4316-B-MID-IDKOLLI-UT                
138200                                                                          
138300        MOVE MID-IDPRODNR   (RADIND) TO 4316-B-MID-IDPRODNR-IN            
138400                                        4316-B-MID-IDPRODNR-UT            
138500        MOVE '4301'                  TO 4316-B-MID-IDTRANS-START          
138600        MOVE 'J'                     TO 4316-B-MID-FLFORTSK               
138700        MOVE 0                       TO 4316-B-MID-IDRADNR-FOM-S          
138800        MOVE 0                       TO 4316-B-MID-IDRADNR-TOM-S          
138900        MOVE 0                       TO 4316-B-MID-KVLEVART-S             
139000        MOVE 'UU'                  TO 4316-B-MID-KDPRTVAL-ADRESSFL        
139100                                      4316-B-MID-KDPRTVAL-FOLJEFL         
139200                                                                          
139300        .                                                                 
139400        EJECT                                                             
139500******* OM DET FINNS ANNULLATIONER PÅ ORDERN SKALL ORDERRADERNA           
139600******* SKAPAS I INTERVALL I 4316-SEGM. BEROENDE PÅ HUR MÅNGA             
139700******* ANNULLERADE RADER SOM FINNS (4308-SEGM).                          
139800                                                                          
139900 ED-SKAPA-4316 SECTION.                                                   
140000                                                                          
140100         PERFORM EB-SKAPA-4316-GEMEN                                      
140200         MOVE +1           TO 4316-IND                                    
140300         MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                             
140400                                                                          
140500         IF  4306-FLANNULL = NEJ                                          
140600                                                                          
140700           IF 4312-KVORDRAD > MAX-ANT-RAD                                 
140800             PERFORM EG-MAX-200-EJ-ANNULL                                 
140900           ELSE                                                           
141000             MOVE '0001' TO 4316-MID-IDRADNR-FOM (4316-IND)               
141100             MOVE 4312-KVORDRAD TO WS-TOM                                 
141200                                                                          
141300             IF WS-TOM NOT = 1                                            
141400                MOVE  WS-TOM  TO                                          
141500                               4316-MID-IDRADNR-TOM (4316-IND)            
141600             END-IF                                                       
141700                                                                          
141800             PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                        
141900           END-IF                                                         
142000                                                                          
142100         ELSE                                                             
142200             MOVE 1            TO WS-FROM                                 
142300             MOVE 4312-KVORDRAD TO WS-TOM                                 
142400             PERFORM IMS-GET-XXDJ-4308                                    
142500                                                                          
142600             PERFORM UNTIL SEGMENT-SAKNAS                                 
142700                                                                          
142800               IF  4308-KVANNANT = ZERO                                   
142900                                                                          
143000                 IF WS-FROM < 4308-IDRADNR-ORD-FROM                       
143100                     COMPUTE WS-TOM = 4308-IDRADNR-ORD-FROM - 1           
143200                     PERFORM EDA-KOLLA-IND                                
143300                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
143400                     MOVE 4312-KVORDRAD TO WS-TOM                         
143500                     PERFORM IMS-GET-XXDJ-4308                            
143600                     ADD +1           TO 4316-IND                         
143700                 ELSE                                                     
143800                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
143900                     PERFORM IMS-GET-XXDJ-4308                            
144000                 END-IF                                                   
144100               ELSE                                                       
144200                   PERFORM IMS-GET-XXDJ-4308                              
144300               END-IF                                                     
144400             END-PERFORM                                                  
144500                                                                          
144600             IF  WS-FROM > 4312-KVORDRAD                                  
144700             CONTINUE                                                     
144800             ELSE                                                         
144900                 PERFORM EDA-KOLLA-IND                                    
145000             END-IF                                                       
145100                                                                          
145200             IF 4316-IDPTYP = '002'                                       
145300                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
145400                                                                          
145500                IF 4316-MID-RAD (MAX-4316-IND) NOT = ALL '+'              
145600                   PERFORM EC-SKAPA-4316-NAESTA                           
145700                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
145800                END-IF                                                    
145900                                                                          
146000             ELSE                                                         
146100                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
146200                                                                          
146300                IF 4316-B-MID-RAD (MAX-4316-IND) NOT = ALL '+'            
146400                   PERFORM EC-SKAPA-4316-NAESTA                           
146500                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
146600                END-IF                                                    
146700                                                                          
146800             END-IF                                                       
146900         END-IF                                                           
147000                                                                          
147100     .                                                                    
147200     EJECT                                                                
147300 EDA-KOLLA-IND SECTION.                                                   
147400                                                                          
147500        IF  4316-IND > MAX-4316-IND OR WS-ANT-RAD-REST = 0                
147600                                                                          
147700            IF  4316-IDPTYP = '002'                                       
147800                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
147900            ELSE                                                          
148000                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
148100            END-IF                                                        
148200                                                                          
148300            PERFORM EC-SKAPA-4316-NAESTA                                  
148400            MOVE +1       TO 4316-IND                                     
148500            MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                          
148600                                                                          
148700        END-IF                                                            
148800                                                                          
148900        COMPUTE WS-ANT-RAD-INT = WS-TOM - WS-FROM + 1                     
149000                                                                          
149100        IF 4316-IDPTYP = '002'                                            
149200                                                                          
149300           IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                            
149400              PERFORM EH-MAX-200-RAD                                      
149500           ELSE                                                           
149600              MOVE WS-FROM      TO                                        
149700                      4316-MID-IDRADNR-FOM (4316-IND)                     
149800                                                                          
149900              IF WS-FROM NOT = WS-TOM                                     
150000                 MOVE WS-TOM    TO                                        
150100                         4316-MID-IDRADNR-TOM (4316-IND)                  
150200              END-IF                                                      
150300                                                                          
150400              COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                 
150500                                        WS-ANT-RAD-INT                    
150600           END-IF                                                         
150700                                                                          
150800        ELSE                                                              
150900                                                                          
151000           IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                            
151100              PERFORM EH-MAX-200-RAD                                      
151200           ELSE                                                           
151300              MOVE WS-FROM      TO                                        
151400                      4316-B-MID-IDRADNR-FOM (4316-IND)                   
151500                                                                          
151600              IF WS-FROM NOT = WS-TOM                                     
151700                 MOVE WS-TOM    TO                                        
151800                         4316-B-MID-IDRADNR-TOM (4316-IND)                
151900              END-IF                                                      
152000                                                                          
152100              COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                 
152200                                        WS-ANT-RAD-INT                    
152300           END-IF                                                         
152400                                                                          
152500        END-IF                                                            
152600                                                                          
152700                                                                          
152800     .                                                                    
152900     EJECT                                                                
153000 EE-SKAPA-4316-004 SECTION.                                               
153100                                                                          
153200     MOVE LOW-VALUE               TO IO-AREA-1                            
153300     MOVE ALL '+'                 TO 4316-WDGX4316                        
153400     MOVE MID-IDPRODNR(RADIND)    TO 4316-IDPRODNR                        
153500     MOVE '004'                   TO 4316-IDPTYP                          
153600     MOVE ZERO                    TO 4316-IDKOLLI                         
153700     MOVE ZERO                    TO 4316-KDTRSTAT                        
153800     MOVE +346                    TO 4316-LL                              
153900     MOVE LOW-VALUE               TO 4316-Z1                              
154000     MOVE LOW-VALUE               TO 4316-Z2                              
154100     MOVE LOW-VALUE               TO 4316-LOWVALUE                        
154200     MOVE 'W4T398X'               TO 4316-KDTRANS                         
154300     MOVE '4301'                  TO 4316-IDTRANS                         
154400     MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                        
154500                                                                          
154600     MOVE WSWDE6-IDKUNDRF(RADIND) TO 4316-C-MID-IDORDNR-IN                
154700                                     4316-C-MID-IDORDNR-UT                
154800                                                                          
154900     MOVE MID-IDDISTR(RADIND)     TO 4316-C-MID-IDDISTR-IN                
155000                                     4316-C-MID-IDDISTR-UT                
155100                                                                          
155200     MOVE WSWDE6-IDKUNDNR (RADIND) TO WS-IDKUNDNR                         
155300     MOVE WS-IDKUNDNR             TO 4316-C-MID-IDKUNDNR-IN               
155400                                     4316-C-MID-IDKUNDNR-UT               
155500                                                                          
155600     MOVE MID-IDPRODNR(RADIND)    TO 4316-C-MID-IDPRODNR-IN               
155700                                     4316-C-MID-IDPRODNR-UT               
155800                                                                          
155900     MOVE ZERO                    TO 4316-C-MID-IDKOLLI-IN                
156000                                     4316-C-MID-IDKOLLI-UT                
156100                                                                          
156200     MOVE SPACE                   TO 4316-C-MID-FLSVAR                    
156300     .                                                                    
156400     EJECT                                                                
156500 EF-KOLLA-4312 SECTION.                                                   
156600                                                                          
156700        PERFORM IMS-GET-XXDK-4312-STAT-BLANK                              
156800                                                                          
156900        IF ((4312-KDBEHAND-AVVIK = 0 OR 2)                                
157000        AND (4312-KDBEHAND-RAD = 0 OR 2)                                  
157100        AND (4312-KDBEHAND-DEL = 0 OR 2)                                  
157200        AND (4312-KDBEHAND-URS = 0 OR 2)                                  
157300        AND (4312-KDBEHAND-KOL = 0 OR 2))                                 
157400            PERFORM IMS-DLET-XXDK-4312                                    
157500                                                                          
157600            PERFORM  IMS-GET-XXDJ-4306-STAT-BLANK                         
157700            MOVE +3 TO 4306-KDPACLAS                                      
157800            PERFORM IMS-REPL-XXDJ                                         
157900                                                                          
158000            MOVE '002' TO W-IDPTYP-4316                                   
158100                                                                          
158200            MOVE +1 TO W-IDKOLLI-4316                                     
158300            PERFORM IMS-GET-XXDL-4316-STAT-BLANK                          
158400            MOVE +1 TO 4316-KDTRSTAT                                      
158500            PERFORM IMS-REPL-XXDL                                         
158600** KOLLI FÖR FRAKTSEDEL                                                   
158700            MOVE +99001 TO W-IDKOLLI-4316                                 
158800            PERFORM IMS-GET-XXDL-4316-STAT-BLANK                          
158900            MOVE +1 TO 4316-KDTRSTAT                                      
159000            PERFORM IMS-REPL-XXDL                                         
159100            MOVE +1 TO W-IDKOLLI-4316                                     
159200                                                                          
159300            MOVE '003' TO W-IDPTYP-4316                                   
159400            PERFORM IMS-GET-XXDL-4316-STAT-GE                             
159500                                                                          
159600            PERFORM UNTIL SEGMENT-SAKNAS                                  
159700               MOVE +1 TO 4316-KDTRSTAT                                   
159800               PERFORM IMS-REPL-XXDL                                      
159900               PERFORM IMS-GET-XXDL-4316-STAT-GE                          
160000            END-PERFORM                                                   
160100                                                                          
160200            MOVE '004' TO W-IDPTYP-4316                                   
160300            MOVE ZERO TO W-IDKOLLI-4316                                   
160400            PERFORM IMS-GET-XXDL-4316                                     
160500            MOVE +1 TO 4316-KDTRSTAT                                      
160600            PERFORM IMS-REPL-XXDL                                         
160700                                                                          
160800            MOVE MFS-KDMFSFOR TO PTOP-KDMFSFOR                            
160900            PERFORM IMS-INSERT-MSG-ALT-PCB                                
161000        ELSE                                                              
161100            MOVE +2 TO 4312-KDBEHAND-GRUND                                
161200            PERFORM IMS-REPL-XXDK                                         
161300        END-IF                                                            
161400     .                                                                    
161500     EJECT                                                                
161600 EG-MAX-200-EJ-ANNULL SECTION.                                            
161700                                                                          
161800***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
161900***** UPPLÄGGNING AV ORDER SOM EJ HAR ANNULLERADE RADER                   
162000***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
162100                                                                          
162200         MOVE 1      TO WS-FROM                                           
162300         MOVE MAX-ANT-RAD  TO WS-TOM                                      
162400         MOVE WS-FROM    TO                                               
162500                 4316-MID-IDRADNR-FOM (4316-IND)                          
162600         MOVE  WS-TOM  TO                                                 
162700                  4316-MID-IDRADNR-TOM (4316-IND)                         
162800         PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                            
162900         MOVE +1       TO 4316-IND                                        
163000                                                                          
163100         PERFORM UNTIL WS-TOM NOT < 4312-KVORDRAD                         
163200            PERFORM EC-SKAPA-4316-NAESTA                                  
163300            COMPUTE WS-FROM = WS-TOM + 1                                  
163400            COMPUTE WS-TOM = WS-FROM + MAX-ANT-RAD - 1                    
163500                                                                          
163600            IF WS-TOM > 4312-KVORDRAD                                     
163700               MOVE 4312-KVORDRAD TO WS-TOM                               
163800            END-IF                                                        
163900                                                                          
164000            MOVE  WS-FROM TO                                              
164100                     4316-B-MID-IDRADNR-FOM (4316-IND)                    
164200                                                                          
164300            IF WS-FROM NOT = WS-TOM                                       
164400               MOVE  WS-TOM TO                                            
164500                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
164600            END-IF                                                        
164700                                                                          
164800            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
164900            MOVE +1       TO 4316-IND                                     
165000         END-PERFORM                                                      
165100                                                                          
165200     .                                                                    
165300     EJECT                                                                
165400 EH-MAX-200-RAD SECTION.                                                  
165500                                                                          
165600***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
165700***** UPPLÄGGNING AV ORDER SOM HAR ANNULLERADE RADER                      
165800***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
165900                                                                          
166000         MOVE WS-TOM       TO WS-TOM-SISTA                                
166100         COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1                   
166200                                                                          
166300         IF 4316-IDPTYP = '002'                                           
166400            MOVE WS-FROM    TO                                            
166500                    4316-MID-IDRADNR-FOM (4316-IND)                       
166600                                                                          
166700            IF WS-FROM NOT = WS-TOM                                       
166800               MOVE  WS-TOM  TO                                           
166900                        4316-MID-IDRADNR-TOM (4316-IND)                   
167000            END-IF                                                        
167100            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
167200                                                                          
167300         ELSE                                                             
167400            MOVE WS-FROM    TO                                            
167500                    4316-B-MID-IDRADNR-FOM (4316-IND)                     
167600                                                                          
167700            IF WS-FROM NOT = WS-TOM                                       
167800               MOVE  WS-TOM  TO                                           
167900                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
168000            END-IF                                                        
168100                                                                          
168200            PERFORM IMS-ISRT-XXDL-4316-STAT-II                            
168300         END-IF                                                           
168400                                                                          
168500         MOVE MAX-ANT-RAD          TO WS-ANT-RAD-REST                     
168600         MOVE +1                   TO 4316-IND                            
168700                                                                          
168800*****  SKAPA KOLLI-POSTER TILLS ALLA RADER I INTERVALLET                  
168900*****  ÄR PLACERADE I PT = 2 ELLER 3 HÄR SKAPAS EV FLERA PT=3             
169000                                                                          
169100         PERFORM UNTIL WS-TOM NOT < WS-TOM-SISTA                          
169200            PERFORM EC-SKAPA-4316-NAESTA                                  
169300            COMPUTE WS-FROM = WS-TOM + 1                                  
169400            COMPUTE WS-ANT-RAD-INT = WS-TOM-SISTA - WS-FROM + 1           
169500                                                                          
169600            IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                           
169700               COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1             
169800               MOVE WS-FROM TO                                            
169900                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
170000                                                                          
170100               IF WS-FROM NOT = WS-TOM                                    
170200                  MOVE  WS-TOM  TO                                        
170300                           4316-B-MID-IDRADNR-TOM (4316-IND)              
170400               END-IF                                                     
170500                                                                          
170600               PERFORM IMS-ISRT-XXDL-4316-STAT-II                         
170700               MOVE MAX-ANT-RAD     TO WS-ANT-RAD-REST                    
170800               MOVE +1           TO 4316-IND                              
170900            ELSE                                                          
171000               MOVE WS-TOM-SISTA   TO WS-TOM                              
171100               MOVE WS-FROM TO                                            
171200                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
171300                                                                          
171400               IF WS-FROM NOT = WS-TOM                                    
171500                  MOVE  WS-TOM  TO                                        
171600                           4316-B-MID-IDRADNR-TOM (4316-IND)              
171700               END-IF                                                     
171800                                                                          
171900               COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                
172000                                         WS-ANT-RAD-INT                   
172100            END-IF                                                        
172200                                                                          
172300         END-PERFORM                                                      
172400                                                                          
172500     .                                                                    
172600     EJECT                                                                
172700 EI-SKRIV-4316-FRAKTS       SECTION.                                      
172800                                                                          
172900     MOVE 99001                   TO 4316-IDKOLLI                         
173000                                     4316-MID-IDKOLLI-IN                  
173100                                     4316-MID-IDKOLLI-UT                  
173200                                                                          
173300     IF MID-IDPRODNR-SAMP (RADIND) = ALL '+'                              
173400       MOVE MID-KDKOLLI (RADIND)    TO 4316-MID-KDKOLLI                   
173500       MOVE WSTAB-VKORDBTO (RADIND) TO 4316-MID-VKORDBTO-KOLLI            
173600       MOVE JA                      TO 4316-MID-FLSISTAK                  
173700                                                                          
173800       IF  MID-KDEMBTYP (RADIND) = ALL '+'                                
173900           MOVE WSEMB-KDEMBTYP (RADIND) TO 4316-MID-KDEMBTYP              
174000       ELSE                                                               
174100           MOVE MID-KDEMBTYP (RADIND) TO 4316-MID-KDEMBTYP                
174200       END-IF                                                             
174300     ELSE                                                                 
174400       MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-WDE6                      
174500       PERFORM IMS-GHU-WDE601                                             
174600       MOVE MID-IDPRODNR-SAMP (RADIND)  TO                                
174700            VORD-IDPRODNR-SAMP                                            
174800       PERFORM IMS-REPL-WDE601                                            
174900     END-IF                                                               
175000                                                                          
175100     MOVE 1                       TO IX                                   
175200     PERFORM UNTIL IX NOT < MAX-4316-IND + 1                              
175300       MOVE ALL '+'               TO 4316-MID-RAD (IX)                    
175400       ADD +1                     TO IX                                   
175500     END-PERFORM                                                          
175600                                                                          
175700     PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                                
175800     .                                                                    
175900     EJECT                                                                
176000 S02-ROER-EJ-FAELT SECTION.                                               
176100                                                                          
176200     MOVE +1 TO RADIND                                                    
176300                                                                          
176400     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
176500        IF  MID-IDDISTR (RADIND) NOT = ALL '+' OR SPACE                   
176600            MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR(RADIND)                 
176700        END-IF                                                            
176800                                                                          
176900        IF  MID-IDPRODNR (RADIND) NOT = ALL '+' OR SPACE                  
177000            MOVE MFS-ROER-EJ-FAELT TO  MOD-IDPRODNR(RADIND)               
177100        END-IF                                                            
177200                                                                          
177300        IF  MID-FLAVVPACK (RADIND) NOT = ALL '+' OR SPACE                 
177400            MOVE MFS-ROER-EJ-FAELT TO  MOD-FLAVVPACK(RADIND)              
177500        END-IF                                                            
177600                                                                          
177700        IF  MID-KDKOLLI (RADIND) NOT = ALL '+' OR SPACE                   
177800            MOVE MFS-ROER-EJ-FAELT TO  MOD-KDKOLLI(RADIND)                
177900        END-IF                                                            
178000                                                                          
178100        IF  MID-VKORDBTO (RADIND) NOT = ALL '+' OR SPACE                  
178200            MOVE MFS-ROER-EJ-FAELT TO  MOD-VKORDBTO(RADIND)               
178300        END-IF                                                            
178400                                                                          
178500        IF  MID-KDEMBTYP (RADIND) NOT = ALL '+' OR SPACE                  
178600            MOVE MFS-ROER-EJ-FAELT TO  MOD-KDEMBTYP(RADIND)               
178700        END-IF                                                            
178800                                                                          
178900        IF  MID-IDPRODNR-SAMP (RADIND) NOT = ALL '+' OR SPACE             
179000            MOVE MFS-ROER-EJ-FAELT TO  MOD-IDPRODNR-SAMP(RADIND)          
179100        END-IF                                                            
179200                                                                          
179300        ADD +1 TO RADIND                                                  
179400     END-PERFORM                                                          
179500                                                                          
179600     .                                                                    
179700     EJECT                                                                
179800 S99-NAESTA-TRANS SECTION.                                                
179900                                                                          
180000        MOVE MSGI-IDDC             TO W-IDDC-4311                         
180100        PERFORM IMS-GET-XXDK-4311                                         
180200*** KVAL MED USERID                                                       
180300        MOVE MSG-SIGNON-USERID TO W-WDGXKEY-IDUSER-4312                   
180400        PERFORM IMS-GET-XXDK-4312-STAT-GE-F                               
180500                                                                          
180600        PERFORM UNTIL NOT (((SEGMENT-FINNS) AND                           
180700                 (4312-KDBEHAND-AVVIK = +3 OR                             
180800                  4312-KDBEHAND-RAD   = +3 OR                             
180900                  4312-KDBEHAND-DEL   = +3 OR                             
181000                  4312-KDBEHAND-URS   = +3 OR                             
181100                  4312-KDBEHAND-KOL   = +3))                              
181200        OR ((SEGMENT-FINNS) AND                                           
181300        (4312-IDTRANS NOT = '4301')))                                     
181400            PERFORM IMS-GET-XXDK-4312-STAT-GE                             
181500        END-PERFORM                                                       
181600                                                                          
181700        IF  SEGMENT-FINNS                                                 
181800            EVALUATE TRUE                                                 
181900            WHEN 4312-KDBEHAND-AVVIK = 1                                  
182000               MOVE 'W4O39101'         TO MFS-IDMOD                       
182100               MOVE M4391-MOD-LAENGD   TO MSG-KVLL                        
182200               MOVE '4391'             TO M4391-MOD-IDTRANS               
182300               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
182400               MOVE WS-IDPRODNR        TO M4391-MOD-IDPRODNR-UT           
182500               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
182600               MOVE WS-IDDISTR         TO M4391-MOD-IDDISTR-UT            
182700               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
182800               MOVE WS-IDKUNDNR        TO M4391-MOD-IDKUNDNR-UT           
182900               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
183000               MOVE WS-KDFRAKT         TO M4391-MOD-KDFRAKT-UT            
183100               MOVE 4312-IDKUNDRF      TO M4391-MOD-IDORDNR-UT            
183200               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
183300               MOVE WS-KDORDKL         TO M4391-MOD-KDORDKL-UT            
183400               MOVE MSGI-IDDC          TO M4391-MOD-IDDC-UT               
183410               MOVE 'UU'             TO M4391-MOD-PRTVAL-ADRESSFL         
183500               INSPECT M4391-MOD-IDPRODNR-UT REPLACING                    
183600                       LEADING ZERO BY SPACE                              
183700               INSPECT M4391-MOD-IDDISTR-UT REPLACING                     
183800                       LEADING ZERO BY SPACE                              
183900               INSPECT M4391-MOD-IDKUNDNR-UT REPLACING                    
184000                       LEADING ZERO BY SPACE                              
184100               INSPECT M4391-MOD-KDFRAKT-UT REPLACING                     
184200                       LEADING ZERO BY SPACE                              
184300               INSPECT M4391-MOD-IDORDNR-UT REPLACING                     
184400                       LEADING ZERO BY SPACE                              
184500               MOVE WS-VISA-NAESTA-BILD      TO WS-MSG-CALL               
184600            WHEN 4312-KDBEHAND-AVVIK = 4                                  
184700               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
184800               MOVE WS-IDPRODNR        TO PTOP1-IDPRODNR-IN               
184900                                          PTOP1-IDPRODNR-UT               
185000               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
185100               MOVE WS-IDDISTR         TO PTOP1-IDDISTR-UT                
185200               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
185300               MOVE WS-IDKUNDNR        TO PTOP1-IDKUNDNR-UT               
185400               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
185500               MOVE WS-KDFRAKT         TO PTOP1-KDFRAKT-UT                
185600               MOVE 4312-IDKUNDRF      TO PTOP1-IDORDNR-UT                
185700               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
185800               MOVE WS-KDORDKL         TO PTOP1-KDORDKL-UT                
185900               MOVE MSGI-IDDC          TO PTOP1-IDDC-UT                   
185910               MOVE 'UU'               TO PTOP1-PRTVAL-ADRESSFL           
186000               MOVE MFS-KDMFSFOR       TO PTOP1-KDMFSFOR                  
186100               MOVE WS-STARTA-4391     TO WS-MSG-CALL                     
186200            WHEN 4312-KDBEHAND-KOL = 1                                    
186300               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
186400               MOVE WS-IDPRODNR        TO PTOP2-IDPRODNR-IN               
186500                                          PTOP2-IDPRODNR-UT               
186600               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
186700               MOVE WS-IDDISTR         TO PTOP2-IDDISTR-UT                
186800               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
186900               MOVE WS-IDKUNDNR        TO PTOP2-IDKUNDNR-UT               
187000               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
187100               MOVE WS-KDFRAKT         TO PTOP2-KDFRAKT-UT                
187200               MOVE 4312-IDKUNDRF      TO PTOP2-IDORDNR-UT                
187300               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
187400               MOVE WS-KDORDKL         TO PTOP2-KDORDKL-UT                
187500               MOVE MSGI-IDDC          TO PTOP2-IDDC-UT                   
187600               MOVE MFS-KDMFSFOR       TO PTOP2-KDMFSFOR                  
187700               MOVE WS-STARTA-4392     TO WS-MSG-CALL                     
187800            WHEN OTHER                                                    
187900               MOVE INF-1(INDX) TO MOD-TEMFSINF                           
188000            END-EVALUATE                                                  
188100        ELSE                                                              
188200                                                                          
188300            IF MID-W4I30101 NOT = ALL '+'                                 
188400              MOVE INF-6(INDX) TO MOD-TEMFSINF                            
188500            END-IF                                                        
188600                                                                          
188700            MOVE MAX-MOD-LAENGD TO MSG-KVLL                               
188800        END-IF                                                            
188900                                                                          
189000     .                                                                    
189100     EJECT                                                                
189200 MFS-ROER-EJ-FAELT-MOD-INFAELT         SECTION.                           
189300                                                                          
189400     MOVE +1 TO RADIND                                                    
189500     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
189600       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDISTR        (RADIND)            
189700                                 MOD-IDPRODNR       (RADIND)              
189800                                 MOD-FLAVVPACK      (RADIND)              
189900                                 MOD-KDKOLLI        (RADIND)              
190000                                 MOD-VKORDBTO       (RADIND)              
190100                                 MOD-KDEMBTYP       (RADIND)              
190200                                 MOD-IDPRODNR-SAMP  (RADIND)              
190300       ADD +1 TO RADIND                                                   
190400     END-PERFORM                                                          
190500     .                                                                    
190600     SKIP2                                                                
190700* IMS SEKTIONER                                                           
190800     SKIP2                                                                
190900 IMS-GET-MSG SECTION.                                                     
191000     MOVE '  QC' TO GODK-STATUSKODER                                      
191100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
191200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
191300     PERFORM IMS-STATUSKONTROLL                                           
191400                                                                          
191500     .                                                                    
191600 IMS-INSERT-MSG SECTION.                                                  
191700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
191800       MOVE '0' TO MFS-KDHUVOMR                                           
191900     END-IF                                                               
192000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
192100     MOVE SPACE TO GODK-STATUSKODER                                       
192200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
192300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
192400     PERFORM IMS-STATUSKONTROLL                                           
192500                                                                          
192600     .                                                                    
192700 IMS-INSERT-MSG-ALT-PCB SECTION.                                          
192800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
192900     MOVE SPACE TO GODK-STATUSKODER                                       
193000     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
193100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     SKIP2                                                                
193500 IMS-INSERT-MSG-ALT1-PCB SECTION.                                         
193600     MOVE SPACE TO GODK-STATUSKODER                                       
193700     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
193800     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
193900     PERFORM IMS-STATUSKONTROLL                                           
194000     .                                                                    
194100     SKIP2                                                                
194200 IMS-INSERT-MSG-ALT2-PCB SECTION.                                         
194300     MOVE SPACE TO GODK-STATUSKODER                                       
194400     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
194500     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
194600     PERFORM IMS-STATUSKONTROLL                                           
194700     .                                                                    
194800     EJECT                                                                
194900 IMS-GET-XXDJ-4305 SECTION.                                               
195000     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
195100            DELIMITED BY SIZE INTO SSA1                                   
195200     MOVE '  '   TO GODK-STATUSKODER                                      
195300     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA-2 SSA1                    
195400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600                                                                          
195700     .                                                                    
195800 IMS-GET-XXDJ-4306-STAT-GE SECTION.                                       
195900     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
196000            DELIMITED BY SIZE INTO SSA1                                   
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
196300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500                                                                          
196600     .                                                                    
196700 IMS-GET-XXDJ-4306-STAT-BLANK SECTION.                                    
196800     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
196900            DELIMITED BY SIZE INTO SSA1                                   
197000     MOVE '  '   TO GODK-STATUSKODER                                      
197100     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
197200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
197300     PERFORM IMS-STATUSKONTROLL                                           
197400                                                                          
197500     .                                                                    
197600 IMS-ISRT-XXDJ-4306 SECTION.                                              
197700     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
197800            DELIMITED BY SIZE INTO SSA1                                   
197900     MOVE 'WLXXDJ11 '   TO SSA2                                           
198000     MOVE '  '   TO GODK-STATUSKODER                                      
198100     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2             
198200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
198300     PERFORM IMS-STATUSKONTROLL                                           
198400                                                                          
198500     .                                                                    
198600 IMS-GET-XXDJ-4308 SECTION.                                               
198700     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
198800            DELIMITED BY SIZE INTO SSA1                                   
198900     MOVE 'WLXXDJ21 '   TO SSA2                                           
199000     MOVE '  GE'    TO GODK-STATUSKODER                                   
199100     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2             
199200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
199300     PERFORM IMS-STATUSKONTROLL                                           
199400                                                                          
199500     .                                                                    
199600 IMS-REPL-XXDJ SECTION.                                                   
199700     MOVE '  '   TO GODK-STATUSKODER                                      
199800     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA-2                       
199900     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
200000     PERFORM IMS-STATUSKONTROLL                                           
200100     .                                                                    
200200     EJECT                                                                
200300 IMS-GET-XXDK-4311 SECTION.                                               
200400     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
200500            DELIMITED BY SIZE INTO SSA1                                   
200600     MOVE '  '     TO GODK-STATUSKODER                                    
200700     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA-3 SSA1                    
200800     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
200900     PERFORM IMS-STATUSKONTROLL                                           
201000                                                                          
201100     .                                                                    
201200 IMS-GET-XXDK-4312-STAT-BLANK SECTION.                                    
201300     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
201400            DELIMITED BY SIZE INTO SSA1                                   
201500     MOVE '  '     TO GODK-STATUSKODER                                    
201600     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA-3 SSA1                  
201700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
201800     PERFORM IMS-STATUSKONTROLL                                           
201900                                                                          
202000     .                                                                    
202100 IMS-GET-XXDK-4312-STAT-GE SECTION.                                       
202200     STRING 'WLXXDK11(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'               
202300            DELIMITED BY SIZE INTO SSA1                                   
202400     MOVE '  GE'   TO GODK-STATUSKODER                                    
202500     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
202600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800                                                                          
202900     .                                                                    
203000 IMS-GET-XXDK-4312-STAT-GE-F SECTION.                                     
203100     STRING 'WLXXDK11*F(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'             
203200            DELIMITED BY SIZE INTO SSA1                                   
203300     MOVE '  GE'   TO GODK-STATUSKODER                                    
203400     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
203500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
203600     PERFORM IMS-STATUSKONTROLL                                           
203700                                                                          
203800     .                                                                    
203900 IMS-ISRT-XXDK-4312 SECTION.                                              
204000     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
204100            DELIMITED BY SIZE INTO SSA1                                   
204200     MOVE 'WLXXDK11 '   TO SSA2                                           
204300     MOVE '  '   TO GODK-STATUSKODER                                      
204400     CALL CBLTDLI USING ISRT XXDK-PCB DLI-IO-AREA-3 SSA1 SSA2             
204500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
204600     PERFORM IMS-STATUSKONTROLL                                           
204700                                                                          
204800     .                                                                    
204900 IMS-DLET-XXDK-4312 SECTION.                                              
205000     MOVE '  '   TO GODK-STATUSKODER                                      
205100     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA-3                       
205200     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400                                                                          
205500     .                                                                    
205600 IMS-REPL-XXDK SECTION.                                                   
205700     MOVE '  '   TO GODK-STATUSKODER                                      
205800     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA-3                       
205900     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
206000     PERFORM IMS-STATUSKONTROLL                                           
206100     .                                                                    
206200     EJECT                                                                
206300 IMS-GET-XXDL-4315 SECTION.                                               
206400     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
206500            DELIMITED BY SIZE INTO SSA1                                   
206600     MOVE '  '     TO GODK-STATUSKODER                                    
206700     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA-1 SSA1                    
206800     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
206900     PERFORM IMS-STATUSKONTROLL                                           
207000                                                                          
207100     .                                                                    
207200 IMS-GET-XXDL-4316-STAT-BLANK SECTION.                                    
207300     STRING 'WLXXDL11*F(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
207400            DELIMITED BY SIZE INTO SSA1                                   
207500     MOVE '  '     TO GODK-STATUSKODER                                    
207600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
207700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
207800     PERFORM IMS-STATUSKONTROLL                                           
207900                                                                          
208000     .                                                                    
208100 IMS-GET-XXDL-4316-STAT-GE SECTION.                                       
208200     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
208300            DELIMITED BY SIZE INTO SSA1                                   
208400     MOVE '  GE'   TO GODK-STATUSKODER                                    
208500     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
208600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
208700     PERFORM IMS-STATUSKONTROLL                                           
208800                                                                          
208900     .                                                                    
209000 IMS-GET-XXDL-4316 SECTION.                                               
209100     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
209200            DELIMITED BY SIZE INTO SSA1                                   
209300     MOVE '  '     TO GODK-STATUSKODER                                    
209400     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
209500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
209600     PERFORM IMS-STATUSKONTROLL                                           
209700                                                                          
209800     .                                                                    
209900 IMS-ISRT-XXDL-4316-STAT-BLANK SECTION.                                   
210000     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
210100            DELIMITED BY SIZE INTO SSA1                                   
210200     MOVE 'WLXXDL11 '   TO SSA2                                           
210300     MOVE '  '   TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
210500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700                                                                          
210800     .                                                                    
210900 IMS-ISRT-XXDL-4316-STAT-II SECTION.                                      
211000     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
211100            DELIMITED BY SIZE INTO SSA1                                   
211200     MOVE 'WLXXDL11 '   TO SSA2                                           
211300     MOVE '  II'   TO GODK-STATUSKODER                                    
211400     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
211500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
211600     PERFORM IMS-STATUSKONTROLL                                           
211700                                                                          
211800     .                                                                    
211900 IMS-REPL-XXDL SECTION.                                                   
212000     MOVE '  '   TO GODK-STATUSKODER                                      
212100     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA-1                       
212200     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500     EJECT                                                                
212600 IMS-GET-EMBB SECTION.                                                    
212700     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
212800            DELIMITED BY SIZE INTO SSA1                                   
212900     MOVE '  GE' TO GODK-STATUSKODER                                      
213000     CALL CBLTDLI USING GU EMBB-PCB DLI-IO-AREA-1 SSA1                    
213100     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
213200     PERFORM IMS-STATUSKONTROLL                                           
213300     .                                                                    
213400     EJECT                                                                
213500 IMS-GET-WDE601 SECTION.                                                  
213600     STRING 'WDE601  (IDPRODNR =' W-WDE6-X ')'                            
213700            DELIMITED BY SIZE INTO SSA1                                   
213800     MOVE '  GE' TO GODK-STATUSKODER                                      
213900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
214000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
214100     PERFORM IMS-STATUSKONTROLL                                           
214200                                                                          
214300     .                                                                    
215200 IMS-GHU-WDE601 SECTION.                                                  
215300     STRING 'WDE601  (IDPRODNR =' W-WDE6-X ')'                            
215400            DELIMITED BY SIZE INTO SSA1                                   
215500     MOVE '  GE' TO GODK-STATUSKODER                                      
215600     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
215700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSKONTROLL                                           
215900                                                                          
216000     .                                                                    
216010 IMS-GU-WDE4E1      SECTION.                                              
216011     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
216012                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
216013          DELIMITED BY SIZE INTO SSA1                                     
216040     MOVE '  GE' TO GODK-STATUSKODER                                      
216050     CALL CBLTDLI USING GU E4E1-PCB DLI-IO-E4E1 SSA1                      
216060     MOVE E4E1-STATUS-CODE TO STATUS-WS                                   
216070     PERFORM IMS-STATUSKONTROLL                                           
216080                                                                          
216090     .                                                                    
216100 IMS-REPL-WDE601 SECTION.                                                 
216200     MOVE '  ' TO GODK-STATUSKODER                                        
216300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
216400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
216500     PERFORM IMS-STATUSKONTROLL                                           
216600     EJECT                                                                
216700     .                                                                    
216800 IMS-GU-WDQ3D1      SECTION.                                              
216900     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
217000            DELIMITED BY SIZE INTO SSA1                                   
217100     MOVE '  '   TO GODK-STATUSKODER                                      
217200     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
217300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
217400     PERFORM IMS-STATUSKONTROLL                                           
217500     .                                                                    
217600     SKIP2                                                                
217700 IMS-GN-WDQ3D1      SECTION.                                              
217800     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
217900            DELIMITED BY SIZE INTO SSA1                                   
218000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
218100     CALL CBLTDLI USING GN  ORQA-PCB ODEL-WDQ301 SSA1                     
218200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
218300     PERFORM IMS-STATUSKONTROLL                                           
218400     .                                                                    
218500     EJECT                                                                
218510 IMS-GU-WDB601    SECTION.                                                
218520                                                                          
218530     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
218540     DELIMITED BY SIZE INTO SSA1                                          
218550     MOVE '  GE' TO GODK-STATUSKODER                                      
218560     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
218570     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
218580     PERFORM IMS-STATUSKONTROLL                                           
218581     IF SEGMENT-SAKNAS                                                    
218582        MOVE SPACE TO DCS-KDDC                                            
218583     END-IF                                                               
218590     .                                                                    
218591     EJECT                                                                
218600 IMS-STATUSKONTROLL SECTION.                                              
218700     SET STATUS-IX TO 1                                                   
218800     SEARCH GODK-STATUS AT END CALL FELLOG                                
218900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
219000     END-SEARCH                                                           
219100     .                                                                    
