000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4030200.                                                
000400 AUTHOR.         LOTTA LANDSTEN.                                          
000500     DATE-WRITTEN.   SEPT-OKT -85.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4302.                                                       
001100*        PACKNINGSRAPPORTERING ORDERVIS                                   
001200*        GRUND-BILD EXPORT.                                               
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T302                                              
001700*        MID:         W4I30201-MID.                                       
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O30201-MOD.                                       
002100*    SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77   PROGRAM-NAMN           VALUE 'W4030200'                             
003000                                 PIC X(8).                                
003100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77    RADIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77    JMF-IND                   PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77    MAX-RADIND-PLUS-1         PIC S9(9)   VALUE +14  COMP SYNC.        
003500 77    4316-IND                  PIC S9(9)   VALUE +0   COMP SYNC.        
003600 77    MAX-4316-IND              PIC S9(9)   VALUE +12  COMP SYNC.        
003700 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +735 COMP SYNC.        
003800 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
003900 77    MOD4391-MOD-LAENGD        PIC S9(4)   VALUE +84  COMP SYNC.        
004000 77    MOD4393-MOD-LAENGD        PIC S9(4)   VALUE +84  COMP SYNC.        
004100 77    MOD4394-MOD-LAENGD        PIC S9(4)   VALUE +82  COMP SYNC.        
004200 77    M4395-MOD-LAENGD          PIC S9(4)   VALUE +82  COMP SYNC.        
004300 77    M4396-MOD-LAENGD          PIC S9(4)   VALUE +84  COMP SYNC.        
004400 77    RAETT                     PIC X       VALUE 'R'.                   
004500 77    FEL                       PIC X       VALUE 'F'.                   
004600 77    JA                        PIC X       VALUE 'J'.                   
004700 77    NEJ                       PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  WS-IDPRTLST.                                                         
005000     03 WS-SYSTDEL               PIC X(1).                                
005100     03 WS-LISTTYP               PIC X(2).                                
005200     03 WS-DC                    PIC X(2).                                
005300     03 WS-KDPRT                 PIC X(3).                                
005400                                                                          
005500 01  WS-MSG-CALL-GRP.                                                     
005600   03  WS-MSG-CALL               PIC X.                                   
005700     88  VISA-NAESTA-BILD                    VALUE '0'.                   
005800     88  STARTA-4301                         VALUE '1'.                   
005900     88  STARTA-4302                         VALUE '2'.                   
006000     88  STARTA-4303                         VALUE '3'.                   
006100     88  STARTA-4391                         VALUE '4'.                   
006200     88  STARTA-4392                         VALUE '5'.                   
006300     88  STARTA-4393                         VALUE '6'.                   
006400     88  STARTA-4394                         VALUE '7'.                   
006500     88  STARTA-4395                         VALUE '8'.                   
006600     88  STARTA-4396                         VALUE '9'.                   
006700   03  WS-VISA-NAESTA-BILD       PIC X       VALUE '0'.                   
006800   03  WS-STARTA-4301            PIC X       VALUE '1'.                   
006900   03  WS-STARTA-4302            PIC X       VALUE '2'.                   
007000   03  WS-STARTA-4303            PIC X       VALUE '3'.                   
007100   03  WS-STARTA-4391            PIC X       VALUE '4'.                   
007200   03  WS-STARTA-4392            PIC X       VALUE '5'.                   
007300   03  WS-STARTA-4393            PIC X       VALUE '6'.                   
007400   03  WS-STARTA-4394            PIC X       VALUE '7'.                   
007500   03  WS-STARTA-4395            PIC X       VALUE '8'.                   
007600   03  WS-STARTA-4396            PIC X       VALUE '9'.                   
007700     EJECT                                                                
007800 01    DYNAMISKA-SUBPROGRAM.                                              
007900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008100   03  WDECEDIT                  PIC X(8)    VALUE 'WDECEDIT'.            
008200   03    W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008300     SKIP3                                                                
008400*                                                                         
008500 01  GEMENSAMMA-SUBPROGRAM.                                               
008600     03  W006PRT                PIC X(8)    VALUE 'W006PRT '.             
008700*        PRINTERKONTROLL                                                  
008800*                                                                         
008900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009000*01 -COPY WMSGINIT                                                        
009100     SKIP3                                                                
009200*   -COPY W006PRT                                                         
009300     EJECT                                                                
009400 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
009500                                                                          
009600 01    FILLER                    PIC X(16)   VALUE 'WS-MODNAMN'.          
009700 01    WS-MODNAMN.                                                        
009800   03    FILLER                  PIC X       VALUE 'W'.                   
009900   03    WS-MOD-IDTRANS-POS-1    PIC X.                                   
010000   03    FILLER                  PIC X       VALUE 'O'.                   
010100   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3).                                
010200   03    FILLER                  PIC X(2)    VALUE '01'.                  
010300                                                                          
010400 01    FILLER                    PIC X(16)                                
010500                                 VALUE 'WS-IDTRANS-MOD'.                  
010600 01    WS-IDTRANS-MOD.                                                    
010700   03    WS-IDTRANS-POS-1-MOD    PIC X.                                   
010800   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3).                                
010900     EJECT                                                                
011000 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
011100 01    DIVERSE.                                                           
011200                                                                          
011300   03    WS-INDATA-TEST          PIC X       VALUE SPACE.                 
011400         88  WS-INDATA-FEL                   VALUE 'F'.                   
011500         88  WS-INDATA-RAETT                 VALUE 'R'.                   
011600                                                                          
011700   03    MAX-ANT-RAD             PIC S9(9)   VALUE +100.                  
011800   03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
011900   03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
012000   03    WS-TOM-SISTA            PIC 9(4)    VALUE ZERO.                  
012100                                                                          
012200   03    WS-FROM                 PIC 9(4)    VALUE ZERO.                  
012300   03    WS-TOM                  PIC 9(4)    VALUE ZERO.                  
012400   03    WS-IDPRODNR             PIC 9(7)    VALUE ZERO.                  
012500   03    WS-IDDISTR              PIC 9(4)    VALUE ZERO.                  
012600   03    WS-IDKUNDNR             PIC 9(6)    VALUE ZERO.                  
012700   03    WS-KDFRAKT              PIC 9(2)    VALUE ZERO.                  
012800   03    WS-KDORDKL              PIC 9       VALUE ZERO.                  
012900                                                                          
013000   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
013100         88  WS-EGEN-BILD        VALUE '4302'.                            
013200                                                                          
013300   03    WS-VKORDBTO-X.                                                   
013400         05  WS-VKORDBTO-RED     PIC 9(5).9   VALUE ZERO.                 
013500                                                                          
013600   03    WS-IDEDITDATA           PIC 9(5)V9   VALUE ZERO.                 
013700   03    WS-VKORDBTO REDEFINES WS-IDEDITDATA.                             
013800         05  FILLER              PIC 9(5).                                
013900         05  WS-VKORDBTO-DEC     PIC 9.                                   
014000                                                                          
014100*    --- VALID IDDC CODES                                                 
014200*                                                                         
014300*01  -COPY WWDC99                                                         
014400     EJECT                                                                
014500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014600     88  NYCKLAR-OK                          VALUE 'J'.                   
014700     88  NYCKLAR-FEL                         VALUE 'N'.                   
014800                                                                          
014900 01  FILLER                    PIC X(16)   VALUE 'WS-TAB'.                
015000 01  WS-TAB.                                                              
015100   03  WS-TABSTEG OCCURS 13.                                              
015200     05  WSWDE6-IDDISTR          PIC S9(5)        COMP-3.                 
015300     05  WSWDE6-IDKUNDNR         PIC S9(7)        COMP-3.                 
015400     05  WSWDE6-KDORDKL          PIC S9           COMP-3.                 
015500     05  WSWDE6-KVORDRAD         PIC S9(5)        COMP-3.                 
015600     05  WSWDE6-KDFRAKT          PIC S9(3)        COMP-3.                 
015700     05  WSWDE6-IDKUNDRF         PIC X(10).                               
015800     05  WSEMB-KDEMBTYP          PIC 9.                                   
015900     05  WSEMB-DIKOLLIL          PIC 9(5).                                
016000     05  WSEMB-DIKOLLIB          PIC 9(3).                                
016100     05  WSEMB-DIKOLLIH          PIC 9(3).                                
016200     05  WSTAB-VKORDBTO-X.                                                
016300         07  WSTAB-VKORDBTO      PIC 9(7).                                
016400     EJECT                                                                
016500 01    FILLER                    PIC X(16)   VALUE 'WDECAREA'.            
016600*01    -COPY WDECAREA                                                     
016700     EJECT                                                                
016800 01    FILLER                    PIC X(16)   VALUE 'WWDIST03'.            
016900*01    -COPY WWDIST03                                                     
017000     EJECT                                                                
017100 01    FILLER                    PIC X(16)   VALUE 'WWDIST08'.            
017200*01    -COPY WWDIST08                                                     
017300     EJECT                                                                
017400 01    FILLER                    PIC X(16)                                
017500                                 VALUE 'NYCKLAR-TILL-DLI'.                
017600 01    NYCKLAR-TILL-DLI.                                                  
017700   03    W-WDGXKEY-4305-X.                                                
017800     05    FILLER                PIC X(4)    VALUE '4305'.                
017900     05    W-IDDC-4305           PIC X(2).                                
018000     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
018100                                                                          
018200   03    W-WDGXKEY-4306-X.                                                
018300     05    W-IDPRODNR-4306       PIC S9(7)   VALUE ZERO  COMP-3.          
018400     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
018500                                                                          
018600   03    W-WDGXKEY-4311-X.                                                
018700     05    FILLER                PIC X(4)    VALUE '4311'.                
018800     05    W-IDDC-4311           PIC X(2).                                
018900     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
019000                                                                          
019100   03    W-WDGXKEY-4312-X.                                                
019200     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
019300     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
019400                                                                          
019500   03    W-WDGXKEY-IDUSER-4312   PIC X(8)    VALUE SPACE.                 
019600                                                                          
019700   03    W-WDGXKEY-4315-X.                                                
019800     05    FILLER                PIC X(4)    VALUE '4315'.                
019900     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
020000                                                                          
020100   03    W-WDGXKEY-4316-X.                                                
020200     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
020300     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
020400     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
020500     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
020600                                                                          
020700   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
020800                                                                          
020900   03    W-WDE6-X.                                                        
021000     05    W-IDPRODNR-WDE6       PIC S9(7)   VALUE ZERO  COMP-3.          
021100                                                                          
021200   03    W-WDQ3D-X.                                                       
021300     05    W-IDPRODNR-WDQ3D      PIC S9(7)   VALUE ZERO  COMP-3.          
021400     05    W-IDPLKLST-WDQ3D      PIC S9(3)   VALUE ZERO  COMP-3.          
021500                                                                          
021600   03    W-WDE4E1KY-MIN-X.                                                
021700     05  W-IDPRODNR-MIN          PIC S9(7)   VALUE ZERO  COMP-3.          
021800     05 FILLER-MIN               PIC X(19)   VALUE LOW-VALUE.             
021900                                                                          
022000   03    W-WDE4E1KY-MAX-X.                                                
022100     05  W-IDPRODNR-MAX          PIC S9(7)   VALUE ZERO  COMP-3.          
022200     05 FILLER-MAX               PIC X(19)   VALUE HIGH-VALUE.            
022300                                                                          
022400     EJECT                                                                
022500 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
022600 01    MEDDELANDE.                                                        
022700                                                                          
022800   03    FEL1.                                                            
022900      05    FILLER               PIC X(40)   VALUE                        
023000           '748. UPPLYSTA FÄLT FEL'.                                      
023100      05    FILLER               PIC X(40)   VALUE                        
023200           '748. HIGH LIGHTED FIELD WRONG'.                               
023300   03    FILLER  REDEFINES FEL1.                                          
023400      05    FEL-1                PIC X(40)   OCCURS 2.                    
023500                                                                          
023600   03    FEL2.                                                            
023700      05    FILLER               PIC X(40)   VALUE                        
023800           '782. ORDERN DELAD     '.                                      
023900      05    FILLER               PIC X(40)   VALUE                        
024000           '782. ORDER ALREADY SPLIT   '.                                 
024100   03    FILLER  REDEFINES FEL2.                                          
024200      05    FEL-2                PIC X(40)   OCCURS 2.                    
024300                                                                          
024400   03    FEL3.                                                            
024500      05    FILLER               PIC X(40)   VALUE                        
024600           'ANVÄND KOLLIVIS > 100 RADER             '.                    
024700      05    FILLER               PIC X(40)   VALUE                        
024800           'USE CASE BY CASE > 100 LINES            '.                    
024900   03    FILLER  REDEFINES FEL3.                                          
025000      05    FEL-3                PIC X(40)   OCCURS 2.                    
025100                                                                          
025200   03    FEL4.                                                            
025300      05    FILLER               PIC X(40)   VALUE                        
025400           'NYCKLAR SAKNAS                          '.                    
025500      05    FILLER               PIC X(40)   VALUE                        
025600           'KEYS ARE MISSING                        '.                    
025700   03    FILLER  REDEFINES FEL4.                                          
025800      05    FEL-4                PIC X(40)   OCCURS 2.                    
025900                                                                          
026000   03    FEL5.                                                            
026100      05    FILLER               PIC X(40)   VALUE                        
026200           '749 FELAKTIG DC-KOD.                    '.                    
026300      05    FILLER               PIC X(40)   VALUE                        
026400           '749 WRONG DC-KOD.                       '.                    
026500   03    FILLER  REDEFINES FEL5.                                          
026600      05    FEL-5                PIC X(40)   OCCURS 2.                    
026700                                                                          
026800   03    FEL6.                                                            
026900      05    FILLER               PIC X(40)   VALUE                        
027000           'SOFTWARE ORDER                          '.                    
027100      05    FILLER               PIC X(40)   VALUE                        
027200           'SOFTWARE ORDER                          '.                    
027300   03    FILLER  REDEFINES FEL6.                                          
027400      05    FEL-6                PIC X(40)   OCCURS 2.                    
027500                                                                          
027600   03    INF1.                                                            
027700      05    FILLER               PIC X(61)   VALUE                        
027800           '     UPPDATERING UTFÖRD'.                                     
027900      05    FILLER               PIC X(61)   VALUE                        
028000           '     UPDATING PERFORMED '.                                    
028100   03    FILLER  REDEFINES INF1.                                          
028200      05    INF-1                PIC X(61)   OCCURS 2.                    
028300                                                                          
028400   03    INF2.                                                            
028500      05    FILLER               PIC X(61)   VALUE                        
028600           '808. MAN.ORDER ANVÄND 4306'.                                  
028700      05    FILLER               PIC X(61)   VALUE                        
028800           '808. MANUEEL ORDER - TAKE 4306'.                              
028900   03    FILLER  REDEFINES INF2.                                          
029000      05    INF-2                PIC X(61)   OCCURS 2.                    
029100                                                                          
029200   03    INF3.                                                            
029300      05    FILLER               PIC X(61)   VALUE                        
029400           '702. ORDERVIS PACKNING PÅGÅR'.                                
029500      05    FILLER               PIC X(61)   VALUE                        
029600           '702. REPORTING PER ORDER IN PROGRESS'.                        
029700   03    FILLER  REDEFINES INF3.                                          
029800      05    INF-3                PIC X(61)   OCCURS 2.                    
029900                                                                          
030000   03    INF4.                                                            
030100      05    FILLER               PIC X(61)   VALUE                        
030200           '710. ORDERN FÄRDIGRAPPORTERAD'.                               
030300      05    FILLER               PIC X(61)   VALUE                        
030400           '710. ORDER ALREADY REPORTED      '.                           
030500   03    FILLER  REDEFINES INF4.                                          
030600      05    INF-4                PIC X(61)   OCCURS 2.                    
030700                                                                          
030800   03    INF5.                                                            
030900      05    FILLER               PIC X(61)   VALUE                        
031000           '755. PACKNING GÅR EJ - LÅSNINGSKOD FEL'.                      
031100      05    FILLER               PIC X(61)   VALUE                        
031200      '755. REPORTING NOT POSSIBLE LOADING CODE IS WRONG'.                
031300   03    FILLER  REDEFINES INF5.                                          
031400      05    INF-5                PIC X(61)   OCCURS 2.                    
031500                                                                          
031600   03    INF6.                                                            
031700      05    FILLER               PIC X(61)   VALUE                        
031800           '807. RAPPORTERING PÅBÖRJAD KOLLIVIS'.                         
031900      05    FILLER               PIC X(61)   VALUE                        
032000           '807. REPORT PER CASE IN PROGRESS'.                            
032100   03    FILLER  REDEFINES INF6.                                          
032200      05    INF-6                PIC X(61)   OCCURS 2.                    
032300                                                                          
032400     EJECT                                                                
032500******************************************************************        
032600*                                                                         
032700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
032800*                                                                         
032900 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
033000     SKIP3                                                                
033100 01    FILLER                    PIC X(16)                                
033200                                 VALUE 'MID W4I30201 MID'.                
033300     SKIP3                                                                
033400*01    -COPY W4I30201                                                     
033500     EJECT                                                                
033600*01    -COPY WMSGAREA                                                     
033700     EJECT                                                                
033800*  03  MOD -COPY W4O30201   -RED MSG-AREA                                 
033900     EJECT                                                                
034000*  03  MOD -COPY W4O39101   -RED MSG-AREA -PRE MOD4391-                   
034100     EJECT                                                                
034200*  03  MOD -COPY W4O39301   -RED MSG-AREA -PRE MOD4393-                   
034300     EJECT                                                                
034400*  03  MOD -COPY W4O39401   -RED MSG-AREA -PRE MOD4394-                   
034500     EJECT                                                                
034600*  03  MOD -COPY W4O39501   -RED MSG-AREA -PRE M4395-                     
034700     EJECT                                                                
034800*  03  MOD -COPY W4O39601   -RED MSG-AREA -PRE M4396-                     
034900     EJECT                                                                
035000 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW'.           
035100 01    P-TO-P-SW.                                                         
035200       03  PTOP-LL               PIC S9(4)   VALUE +17 COMP SYNC.         
035300       03  PTOP-Z1               PIC X       VALUE LOW-VALUE.             
035400       03  PTOP-Z2               PIC X       VALUE LOW-VALUE.             
035500       03  PTOP-TRANSKOD         PIC X(7)    VALUE 'W0T605U'.             
035600       03  FILLER                PIC X       VALUE SPACE.                 
035700       03  FILLER                PIC X(4)    VALUE '4302'.                
035800       03  PTOP-KDMFSFOR         PIC X.                                   
035900     SKIP3                                                                
036000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
036100 01  P-TO-P-SW1.                                                          
036200       03  PTOP1-LL              PIC S9(4)   VALUE +187 COMP SYNC.        
036300       03  PTOP1-Z1              PIC X       VALUE LOW-VALUE.             
036400       03  PTOP1-Z2              PIC X       VALUE LOW-VALUE.             
036500       03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W4T391U'.             
036600       03  FILLER                PIC X       VALUE SPACE.                 
036700       03  FILLER                PIC X(4)    VALUE '4302'.                
036800       03  PTOP1-KDMFSFOR        PIC X.                                   
036900       03  PTOP1-IDPRODNR-IN     PIC X(7).                                
037000       03  PTOP1-IDPRODNR-UT     PIC X(7).                                
037100       03  PTOP1-IDDISTR-UT      PIC X(4).                                
037200       03  PTOP1-IDKUNDNR-UT     PIC X(6).                                
037300       03  PTOP1-KDFRAKT-UT      PIC X(2).                                
037400       03  PTOP1-IDORDNR-UT      PIC X(5).                                
037500       03  PTOP1-KDORDKL-UT      PIC X(1).                                
037600       03  PTOP1-IDDC-UT         PIC X(2).                                
037700       03  PTOP1-PRTVAL-ADRESSFL PIC X(2).                                
037800       03  FILLER                PIC X(134)  VALUE ALL '+'.               
037900     EJECT                                                                
038000 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW2'.          
038100 01    P-TO-P-SW2.                                                        
038200       03  PTOP2-LL              PIC S9(4)   VALUE +89 COMP SYNC.         
038300       03  PTOP2-Z1              PIC X       VALUE LOW-VALUE.             
038400       03  PTOP2-Z2              PIC X       VALUE LOW-VALUE.             
038500       03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W4T396U'.             
038600       03  FILLER                PIC X       VALUE SPACE.                 
038700       03  FILLER                PIC X(4)    VALUE '4302'.                
038800       03  PTOP2-KDMFSFOR        PIC X(1).                                
038900       03  PTOP2-IDPRODNR-IN     PIC X(7).                                
039000       03  PTOP2-IDPRODNR-UT     PIC X(7).                                
039100       03  PTOP2-IDDISTR-UT      PIC X(4).                                
039200       03  PTOP2-IDKUNDNR-UT     PIC X(6).                                
039300       03  PTOP2-KDFRAKT-UT      PIC X(2).                                
039400       03  PTOP2-IDORDNR-UT      PIC X(5).                                
039500       03  PTOP2-KDORDKL-UT      PIC X(1).                                
039600       03  PTOP2-IDDC-UT         PIC X(2).                                
039700       03  PTOP2-PRTVAL-ADRESSFL PIC X(2).                                
039800                                                                          
039900     EJECT                                                                
040000*01    -COPY WMFSAREA                                                     
040100     EJECT                                                                
040200******************************************************************        
040300*                                                                         
040400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
040500*                                                                         
040600 01    IMS-WS.                                                            
040700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
040800     SKIP3                                                                
040900*                        **** STATUS-KOD FRÅN IMS                         
041000   03    STATUS-WS               PIC XX.                                  
041100     88    SEGMENT-FINNS                     VALUE '  '.                  
041200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
041300     SKIP3                                                                
041400   03    GODK-STATUSKODER.                                                
041500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
041600     SKIP3                                                                
041700 01    SSA1                      PIC X(128).                              
041800 01    SSA2                      PIC X(64).                               
041900     EJECT                                                                
042000*                            IMS FUNKTIONSKODER                           
042100*01    -COPY W0003                                                        
042200     EJECT                                                                
042300*                            DLI INPUT-OUTPUT AREA                        
042400 01    FILLER                    PIC X(16)   VALUE                        
042500                                 'DLI-IO-AREA-1'.                         
042600 01    DLI-IO-AREA-1.                                                     
042700   03    IO-AREA-1               PIC X(370)  VALUE SPACE.                 
042800     SKIP3                                                                
042900*  03    EMBB01 -COPY WDK501      -RED IO-AREA-1                          
043000     EJECT                                                                
043100*  03    XXDL11 -COPY WDGX4316    -RED IO-AREA-1                          
043200     EJECT                                                                
043300*     08 AREA -COPY W4I31501    -RED 4316-FILLER -PRE 4316-A-             
043400     EJECT                                                                
043500*     08 AREA -COPY W4I31401    -RED 4316-FILLER -PRE 4316-B-             
043600     EJECT                                                                
043700*     08 AREA -COPY W4I39801    -RED 4316-FILLER -PRE 4316-C-             
043800     EJECT                                                                
043900 01    FILLER                    PIC X(16)   VALUE                        
044000                                 'DLI-IO-AREA-2'.                         
044100 01    DLI-IO-AREA-2.                                                     
044200   03    IO-AREA-2               PIC X(60)   VALUE SPACE.                 
044300     SKIP3                                                                
044400*  03    XXDJ01 -COPY WDGX4305    -RED IO-AREA-2                          
044500     EJECT                                                                
044600*  03    XXDJ11 -COPY WDGX4306    -RED IO-AREA-2                          
044700     EJECT                                                                
044800*  03    XXDJ21 -COPY WDGX4308    -RED IO-AREA-2                          
044900     EJECT                                                                
045000 01    FILLER                    PIC X(16)   VALUE                        
045100                                 'DLI-IO-AREA-3'.                         
045200 01    DLI-IO-AREA-3.                                                     
045300   03    IO-AREA-3               PIC X(100)  VALUE SPACE.                 
045400     SKIP3                                                                
045500*  03    XXDK11 -COPY WDGX4312    -RED IO-AREA-3                          
045600     EJECT                                                                
045700*01             -COPY WDQ301                                              
045800     EJECT                                                                
045900 01    FILLER                    PIC X(16)   VALUE 'WDE601'.              
046000 01    DLI-IO-E601.                                                       
046100*  03  -COPY WDE601                                                       
046200     EJECT                                                                
046300 01    FILLER                    PIC X(16)   VALUE 'WDE4E1'.              
046400 01    DLI-IO-E4E1.                                                       
046500*  03  -COPY WDE4E1                                                       
046600     EJECT                                                                
046700 LINKAGE SECTION.                                                         
046800*01    -COPY W0009     -PRE MSG-                                          
046900     EJECT                                                                
047000*01    -COPY W0009     -PRE ALT-                                          
047100     EJECT                                                                
047200*01    -COPY W0009     -PRE ALT1-                                         
047300     EJECT                                                                
047400*01    -COPY W0009     -PRE ALT2-                                         
047500     EJECT                                                                
047600*01    -COPY W0008     -PRE USEA-                                         
047700        05 FILLER                PIC X.                                   
047800     EJECT                                                                
047900*01    -COPY W0008     -PRE XXDJ-                                         
048000        05 FILLER                PIC X.                                   
048100     EJECT                                                                
048200*01    -COPY W0008     -PRE XXDK-                                         
048300        05 FILLER                PIC X.                                   
048400     EJECT                                                                
048500*01    -COPY W0008     -PRE XXDL-                                         
048600        05 FILLER                PIC X.                                   
048700     EJECT                                                                
048800*01    -COPY W0008     -PRE EMBB-                                         
048900        05 FILLER                PIC X.                                   
049000     EJECT                                                                
049100*01    -COPY W0008     -PRE WDE6-                                         
049200        05 FILLER                PIC X.                                   
049300     EJECT                                                                
049400*01    -COPY W0008     -PRE ORQA-                                         
049500        05 FILLER                PIC X.                                   
049600     EJECT                                                                
049700*01    -COPY W0008     -PRE E4E1-                                         
049800        05 FILLER                PIC X.                                   
049900     EJECT                                                                
050000 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB               
050100                 USEA-PCB                                                 
050200                 XXDJ-PCB XXDK-PCB XXDL-PCB EMBB-PCB WDE6-PCB             
050300                          ORQA-PCB E4E1-PCB.                              
050400                                                                          
050500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB              
050600                 USEA-PCB                                                 
050700                  XXDJ-PCB XXDK-PCB XXDL-PCB EMBB-PCB WDE6-PCB            
050800                          ORQA-PCB E4E1-PCB.                              
050900                                                                          
051000     PERFORM IMS-GET-MSG                                                  
051100                                                                          
051200     IF SEGMENT-FINNS                                                     
051300       PERFORM A-INIT-SPARA-INPUT                                         
051400       MOVE MFS-IDTRANS  TO WS-IDTRANS                                    
051500         IF WS-EGEN-BILD AND NYCKLAR-OK                                   
051600            IF MID-W4I30201 NOT = ALL '+'                                 
051700               PERFORM B-INDATA-KOLL                                      
051800               IF WS-INDATA-RAETT                                         
051900                  PERFORM C-REL-KOLL                                      
052000                  IF WS-INDATA-RAETT                                      
052100                     PERFORM D-BEARBETA                                   
052200                     PERFORM S99-NAESTA-TRANS                             
052300                  ELSE                                                    
052400                     MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                    
052500*    --------------                                                       
052600*    --------------  MOVE 'FOUT ZIT HIER' TO MOD-TEMFSFEL                 
052700*    --------------                                                       
052800                     PERFORM S02-ROER-EJ-FAELT                            
052900                     MOVE MAX-MOD-LAENGD      TO MSG-KVLL                 
053000                  END-IF                                                  
053100               ELSE                                                       
053200                  MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                       
053300                  PERFORM S02-ROER-EJ-FAELT                               
053400                  MOVE MAX-MOD-LAENGD      TO MSG-KVLL                    
053500               END-IF                                                     
053600            ELSE                                                          
053700               PERFORM S99-NAESTA-TRANS                                   
053800            END-IF                                                        
053900         ELSE                                                             
054000            MOVE MAX-MOD-LAENGD   TO MSG-KVLL                             
054100         END-IF                                                           
054200                                                                          
054300         EVALUATE TRUE                                                    
054400           WHEN VISA-NAESTA-BILD PERFORM IMS-INSERT-MSG                   
054500           WHEN STARTA-4391      PERFORM IMS-INSERT-MSG-ALT1-PCB          
054600           WHEN STARTA-4396      PERFORM IMS-INSERT-MSG-ALT2-PCB          
054700         END-EVALUATE                                                     
054800     END-IF                                                               
054900                                                                          
055000     MOVE ZERO TO RETURN-CODE                                             
055100     GOBACK                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 A-INIT-SPARA-INPUT SECTION.                                              
055500                                                                          
055600     IF MSG-DUBBLA-TRANSKODER                                             
055700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I30201                 
055800       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
055900       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
056000       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
056100       MOVE MSG-IDPFK            TO MFS-IDPFK                             
056200     ELSE                                                                 
056300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I30201                  
056400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
056500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
056600       MOVE ' ' TO MFS-KDTRTYP                                            
056700                   MFS-IDPFK                                              
056800     END-IF                                                               
056900                                                                          
057000     MOVE LOW-VALUE TO MSG-AREA                                           
057100                       FILLER-MIN                                         
057200     MOVE HIGH-VALUE TO FILLER-MAX                                        
057300     MOVE 'W4O302N1' TO MFS-IDMOD                                         
057400     MOVE '4302' TO MOD-IDTRANS                                           
057500     MOVE WS-VISA-NAESTA-BILD            TO WS-MSG-CALL                   
057600                                                                          
057700     MOVE +1                TO RADIND                                     
057800     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
057900     MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR        (RADIND)                
058000                               MOD-IDPRODNR       (RADIND)                
058100                               MOD-FLAVVPACK      (RADIND)                
058200                               MOD-IDKOLLI        (RADIND)                
058300                               MOD-KDKOLLI        (RADIND)                
058400                               MOD-VKORDBTO-KOLLI (RADIND)                
058500                               MOD-ADFLGEO        (RADIND)                
058600                               MOD-ADRUTHYL       (RADIND)                
058700                               MOD-KDEMBTYP       (RADIND)                
058800                               MOD-DIKOLLIL       (RADIND)                
058900                               MOD-DIKOLLIB       (RADIND)                
059000                               MOD-DIKOLLIH       (RADIND)                
059100     ADD +1                 TO RADIND                                     
059200     END-PERFORM                                                          
059300                                                                          
059400* NOLLA WS-TAB FRÅN SKRÄP-TECKEN.                                         
059500                                                                          
059600     MOVE +1                TO RADIND                                     
059700     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
059800       MOVE ZERO          TO WSWDE6-IDDISTR  (RADIND)                     
059900       MOVE ZERO          TO WSWDE6-IDKUNDNR (RADIND)                     
060000       MOVE ZERO          TO WSWDE6-KDORDKL  (RADIND)                     
060100       MOVE ZERO          TO WSWDE6-KVORDRAD (RADIND)                     
060200       MOVE ZERO          TO WSWDE6-KDFRAKT  (RADIND)                     
060300       MOVE ZERO          TO WSEMB-KDEMBTYP  (RADIND)                     
060400       MOVE ZERO          TO WSEMB-DIKOLLIL  (RADIND)                     
060500       MOVE ZERO          TO WSEMB-DIKOLLIB  (RADIND)                     
060600       MOVE ZERO          TO WSEMB-DIKOLLIH  (RADIND)                     
060700       MOVE ZERO          TO WSTAB-VKORDBTO  (RADIND)                     
060800       MOVE SPACE         TO WSWDE6-IDKUNDRF (RADIND)                     
060900       ADD +1             TO RADIND                                       
061000     END-PERFORM                                                          
061100                                                                          
061200                                                                          
061300     MOVE MFS-RENSA-FAELT   TO MOD-PRTVAL-ADRESSFL                        
061400     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
061500                               MOD-TEMFSINF                               
061600     PERFORM AA-FLYTTA-IDDC                                               
061700     .                                                                    
061800     EJECT                                                                
061900 AA-FLYTTA-IDDC           SECTION.                                        
062000                                                                          
062100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
062200     MOVE '001'             TO MSGI-KDCALL                                
062300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
062400     MOVE '4302'            TO MSGI-IDTRANS                               
062500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
062600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062700                                                                          
062800     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
062900                                                                          
063000     MOVE JA                    TO NYCKLAR-SW                             
063100                                                                          
063200     MOVE MSGI-IDDC             TO WS-IDDC                                
063300                                                                          
063400     IF MSGI-IDLAND-SPR = 'GB'                                            
063500       MOVE +2 TO INDX                                                    
063600     ELSE                                                                 
063700       MOVE +1 TO INDX                                                    
063800     END-IF                                                               
063900                                                                          
064000     IF WS-IDDC IS > SPACE                                                
064100       MOVE WS-IDDC             TO MOD-IDDC-UT                            
064200     ELSE                                                                 
064300       MOVE NEJ                 TO NYCKLAR-SW                             
064400     END-IF                                                               
064500                                                                          
064600     IF NYCKLAR-FEL                                                       
064700        MOVE FEL-4 (INDX) TO MOD-TEMFSFEL                                 
064800        PERFORM MFS-ROER-EJ-FAELT-MOD-INFAELT                             
064900     END-IF                                                               
065000     .                                                                    
065100     SKIP2                                                                
065200 B-INDATA-KOLL SECTION.                                                   
065300                                                                          
065400     MOVE RAETT                 TO WS-INDATA-TEST                         
065500*KOLLIFLAGGA                                                              
065600     IF MID-PRTVAL-ADRESSFL = ALL '+'                                     
065700       IF CDC-SE                                                          
065800         MOVE FEL                  TO WS-INDATA-TEST                      
065900         MOVE FEL-1 (INDX)         TO MOD-TEMFSFEL                        
066000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-PRTVAL-ADRESSFL-ATTR            
066100       ELSE                                                               
066200         MOVE 'UU'                 TO MOD-PRTVAL-ADRESSFL                 
066300                                      WS-KDPRT                            
066400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-ADRESSFL-ATTR            
066500       END-IF                                                             
066600     ELSE                                                                 
066700       MOVE MID-PRTVAL-ADRESSFL     TO MOD-PRTVAL-ADRESSFL                
066800                                       WS-KDPRT                           
066900     END-IF                                                               
067000                                                                          
067100     IF MID-PRTVAL-ADRESSFL = 'U ' OR 'UU'                                
067200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-ADRESSFL-ATTR              
067300     ELSE                                                                 
067400       IF CDC-SE                                                          
067500         MOVE '4'                 TO WS-SYSTDEL                           
067600         MOVE 'KF'                TO WS-LISTTYP                           
067700         MOVE WS-IDDC             TO WS-DC                                
067800         MOVE MID-PRTVAL-ADRESSFL TO WS-KDPRT                             
067900                                                                          
068000         MOVE 001                 TO PRT-KDCALL                           
068100         MOVE WS-IDPRTLST         TO PRT-IDPRTLST                         
068200                                                                          
068300         CALL W006PRT USING PRT-W006PRT                                   
068400                                                                          
068500         IF PRT-KDSVAR = RAETT                                            
068600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-ADRESSFL-ATTR          
068700           MOVE MID-PRTVAL-ADRESSFL  TO MOD-PRTVAL-ADRESSFL               
068800                                        WS-KDPRT                          
068900         ELSE                                                             
069000           MOVE FEL                  TO WS-INDATA-TEST                    
069100           MOVE FEL-1 (INDX)         TO MOD-TEMFSFEL                      
069200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-PRTVAL-ADRESSFL-ATTR          
069300         END-IF                                                           
069400       ELSE                                                               
069500         MOVE 'UU'                TO MOD-PRTVAL-ADRESSFL                  
069600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-ADRESSFL-ATTR            
069700       END-IF                                                             
069800     END-IF                                                               
069900                                                                          
070000*END TEST KOLLIFLAGGA                                                     
070100                                                                          
070200     MOVE +1         TO RADIND                                            
070300     IF MID-RAD (RADIND) NOT = ALL '+'                                    
070400                                                                          
070500       PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                       
070600       IF MID-RAD (RADIND) NOT = ALL '+'                                  
070700                                                                          
070800                                                                          
070900       IF MID-IDDISTR (RADIND) = ALL '+'                                  
071000          MOVE FEL                   TO WS-INDATA-TEST                    
071100          MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)             
071200       ELSE                                                               
071300         IF MID-IDDISTR (RADIND) NUMERIC                                  
071400            MOVE MID-IDDISTR (RADIND)  TO DIST03-IDDISTR                  
071500            IF DIST03-SVERIGE AND SWEDISH-TEXT                            
071600               MOVE FEL                TO WS-INDATA-TEST                  
071700               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)        
071800            ELSE                                                          
071900               MOVE MFS-NUM-FAELT-RAETT TO                                
072000                                    MOD-IDDISTR-ATTR (RADIND)             
072100            END-IF                                                        
072200         ELSE                                                             
072300           MOVE FEL                   TO WS-INDATA-TEST                   
072400           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-ATTR (RADIND)        
072500         END-IF                                                           
072600       END-IF                                                             
072700                                                                          
072800       IF MID-IDPRODNR (RADIND) = ALL '+'                                 
072900          MOVE FEL                   TO WS-INDATA-TEST                    
073000          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIND)          
073100       ELSE                                                               
073200         IF MID-IDPRODNR (RADIND) NUMERIC                                 
073300            MOVE MFS-NUM-FAELT-RAETT TO                                   
073400                              MOD-IDPRODNR-ATTR (RADIND)                  
073500         ELSE                                                             
073600            MOVE FEL                   TO WS-INDATA-TEST                  
073700            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIND)        
073800         END-IF                                                           
073900       END-IF                                                             
074000                                                                          
074100       IF MID-FLAVVPACK (RADIND) = ALL '+'                                
074200       CONTINUE                                                           
074300       ELSE                                                               
074400          IF MID-FLAVVPACK (RADIND) = 'J' OR 'Y'                          
074500             MOVE MFS-ALFA-FAELT-RAETT TO                                 
074600                               MOD-FLAVVPACK-ATTR (RADIND)                
074700          ELSE                                                            
074800            MOVE FEL                 TO WS-INDATA-TEST                    
074900            MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAVVPACK-ATTR (RADIND)        
075000          END-IF                                                          
075100       END-IF                                                             
075200                                                                          
075300       IF MID-NIV2 (RADIND) NOT = ALL '+'                                 
075400         IF MID-IDKOLLI (RADIND) = ALL '+'                                
075500            MOVE FEL                   TO WS-INDATA-TEST                  
075600            MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKOLLI-ATTR (RADIND)          
075700         END-IF                                                           
075800       END-IF                                                             
075900                                                                          
076000       IF MID-ADFLGEO (RADIND) = ALL '+'                                  
076100       CONTINUE                                                           
076200       ELSE                                                               
076300          MOVE MFS-ALFA-FAELT-RAETT  TO                                   
076400                               MOD-ADFLGEO-ATTR (RADIND)                  
076500          IF MID-ADRUTHYL (RADIND) = ALL '+'                              
076600             MOVE FEL               TO WS-INDATA-TEST                     
076700             MOVE MFS-NUM-FAELT-FEL   TO                                  
076800                              MOD-ADRUTHYL-ATTR (RADIND)                  
076900          END-IF                                                          
077000       END-IF                                                             
077100                                                                          
077200       IF MID-ADRUTHYL (RADIND) = ALL '+'                                 
077300       CONTINUE                                                           
077400       ELSE                                                               
077500         IF (MID-ADRUTHYL (RADIND) NUMERIC)                               
077600         AND (MID-ADFLOMR (RADIND) < 500 OR                               
077700              MID-ADFLOMR (RADIND) > 899)                                 
077800            MOVE MFS-NUM-FAELT-RAETT   TO                                 
077900                              MOD-ADRUTHYL-ATTR (RADIND)                  
078000         ELSE                                                             
078100            MOVE FEL                   TO WS-INDATA-TEST                  
078200            MOVE MFS-NUM-FAELT-FEL   TO                                   
078300                             MOD-ADRUTHYL-ATTR (RADIND)                   
078400         END-IF                                                           
078500       END-IF                                                             
078600                                                                          
078700       IF MID-IDKOLLI (RADIND) = ALL '+'                                  
078800       CONTINUE                                                           
078900       ELSE                                                               
079000         IF  MID-IDKOLLI (RADIND) NUMERIC                                 
079100         AND MID-IDKOLLI (RADIND) > 0                                     
079200            MOVE MFS-NUM-FAELT-RAETT TO                                   
079300                              MOD-IDKOLLI-ATTR (RADIND)                   
079400         ELSE                                                             
079500            MOVE FEL                   TO WS-INDATA-TEST                  
079600            MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKOLLI-ATTR (RADIND)          
079700         END-IF                                                           
079800       END-IF                                                             
079900                                                                          
080000       IF MID-KDKOLLI (RADIND) = ALL '+'                                  
080100       CONTINUE                                                           
080200       ELSE                                                               
080300          MOVE MFS-ALFA-FAELT-RAETT  TO                                   
080400                               MOD-KDKOLLI-ATTR (RADIND)                  
080500       END-IF                                                             
080600                                                                          
080700       IF MID-KDEMBTYP (RADIND) = ALL '+'                                 
080800       CONTINUE                                                           
080900       ELSE                                                               
081000         IF (MID-KDEMBTYP (RADIND) NUMERIC)                               
081100         AND (MID-KDEMBTYP (RADIND) > 0 AND < 8)                          
081200         AND (MID-KDKOLLI  (RADIND) = ALL '+')                            
081300            MOVE MFS-NUM-FAELT-RAETT   TO                                 
081400                              MOD-KDEMBTYP-ATTR (RADIND)                  
081500         ELSE                                                             
081600            MOVE FEL                   TO WS-INDATA-TEST                  
081700            MOVE MFS-NUM-FAELT-FEL   TO                                   
081800                              MOD-KDEMBTYP-ATTR (RADIND)                  
081900         END-IF                                                           
082000       END-IF                                                             
082100                                                                          
082200       IF MID-VKORDBTO-KOLLI (RADIND) = ALL '+'                           
082300         MOVE MFS-NUM-FAELT-RAETT   TO                                    
082400                        MOD-VKORDBTO-KOLLI-ATTR (RADIND)                  
082500         CONTINUE                                                         
082600       ELSE                                                               
082700          MOVE MID-VKORDBTO-KOLLI (RADIND) TO DEC-IDFRIDATA               
082800          MOVE 5                     TO DEC-KVHELTAL                      
082900          MOVE 1                     TO DEC-KVDECIMAL                     
083000          CALL WDECEDIT USING DEC-WDECAREA                                
083100                                                                          
083200          IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > 0                         
083300             MOVE MFS-NUM-FAELT-RAETT   TO                                
083400                               MOD-VKORDBTO-KOLLI-ATTR (RADIND)           
083500             MOVE DEC-IDEDITDATA  TO WS-IDEDITDATA                        
083600                                                                          
083700             IF WS-VKORDBTO-DEC > 0                                       
083800                MOVE WS-IDEDITDATA TO WS-VKORDBTO-RED                     
083900                MOVE WS-VKORDBTO-X TO WSTAB-VKORDBTO-X (RADIND)           
084000             ELSE                                                         
084100                MOVE WS-IDEDITDATA TO WSTAB-VKORDBTO (RADIND)             
084200             END-IF                                                       
084300                                                                          
084400          ELSE                                                            
084500             MOVE FEL                TO WS-INDATA-TEST                    
084600             MOVE MFS-NUM-FAELT-FEL TO                                    
084700                           MOD-VKORDBTO-KOLLI-ATTR (RADIND)               
084800           END-IF                                                         
084900                                                                          
085000       END-IF                                                             
085100                                                                          
085200       IF MID-DIKOLLIL (RADIND) = ALL '+'                                 
085300       CONTINUE                                                           
085400       ELSE                                                               
085500         IF MID-DIKOLLIL (RADIND) NUMERIC                                 
085600         AND MID-DIKOLLIL (RADIND) > 0                                    
085700            MOVE MFS-NUM-FAELT-RAETT   TO                                 
085800                              MOD-DIKOLLIL-ATTR (RADIND)                  
085900         ELSE                                                             
086000            MOVE FEL                   TO WS-INDATA-TEST                  
086100            MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIL-ATTR (RADIND)        
086200         END-IF                                                           
086300       END-IF                                                             
086400                                                                          
086500       IF MID-DIKOLLIB (RADIND) = ALL '+'                                 
086600       CONTINUE                                                           
086700       ELSE                                                               
086800         IF MID-DIKOLLIB (RADIND) NUMERIC                                 
086900         AND MID-DIKOLLIB (RADIND) > 0                                    
087000            MOVE MFS-NUM-FAELT-RAETT   TO                                 
087100                              MOD-DIKOLLIB-ATTR (RADIND)                  
087200         ELSE                                                             
087300            MOVE FEL                   TO WS-INDATA-TEST                  
087400            MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIB-ATTR (RADIND)        
087500         END-IF                                                           
087600       END-IF                                                             
087700                                                                          
087800       IF MID-DIKOLLIH (RADIND) = ALL '+'                                 
087900       CONTINUE                                                           
088000       ELSE                                                               
088100         IF MID-DIKOLLIH (RADIND) NUMERIC                                 
088200         AND MID-DIKOLLIH (RADIND) > 0                                    
088300            MOVE MFS-NUM-FAELT-RAETT   TO                                 
088400                              MOD-DIKOLLIH-ATTR (RADIND)                  
088500         ELSE                                                             
088600            MOVE FEL                   TO WS-INDATA-TEST                  
088700            MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIH-ATTR (RADIND)        
088800         END-IF                                                           
088900       END-IF                                                             
089000       END-IF                                                             
089100                                                                          
089200       ADD +1          TO RADIND                                          
089300       END-PERFORM                                                        
089400*OM ENDAST PRINTER ÄR IFYLLT OCH INGEN RADDATA                            
089500     ELSE                                                                 
089600       MOVE FEL                   TO WS-INDATA-TEST                       
089700       MOVE FEL-1 (INDX)          TO MOD-TEMFSFEL                         
089800       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-ATTR (RADIND)            
089900* SLUT                                                                    
090000     END-IF                                                               
090100                                                                          
090200     IF WS-INDATA-RAETT                                                   
090300**** KONTROLL AV ATT INGA DUBLETTER (IDPRODNR) RAPPORTERAS                
090400                                                                          
090500        MOVE +1         TO RADIND                                         
090600                                                                          
090700        PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                      
090800           IF MID-IDPRODNR (RADIND) NOT = ALL '+'                         
090900              MOVE RADIND   TO JMF-IND                                    
091000              ADD +1        TO JMF-IND                                    
091100                                                                          
091200              PERFORM UNTIL JMF-IND NOT < MAX-RADIND-PLUS-1               
091300                 IF MID-IDPRODNR (JMF-IND) NOT = ALL '+'                  
091400                                                                          
091500                    IF MID-IDPRODNR (RADIND) =                            
091600                                     MID-IDPRODNR (JMF-IND)               
091700                       MOVE FEL     TO WS-INDATA-TEST                     
091800                       MOVE MFS-NUM-FAELT-FEL                             
091900                                TO MOD-IDPRODNR-ATTR (RADIND)             
092000                       MOVE MFS-NUM-FAELT-FEL                             
092100                                TO MOD-IDPRODNR-ATTR (JMF-IND)            
092200                    END-IF                                                
092300                                                                          
092400                 END-IF                                                   
092500                 ADD +1        TO JMF-IND                                 
092600              END-PERFORM                                                 
092700                                                                          
092800           END-IF                                                         
092900           ADD +1        TO RADIND                                        
093000        END-PERFORM                                                       
093100     END-IF                                                               
093200                                                                          
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 C-REL-KOLL SECTION.                                                      
093700                                                                          
093800     MOVE +1        TO RADIND                                             
093900     MOVE WS-IDDC               TO W-IDDC-4305                            
094000     PERFORM IMS-GET-XXDJ-4305                                            
094100                                                                          
094200     IF SEGMENT-FINNS                                                     
094300     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
094400                                                                          
094500     IF MID-RAD (RADIND) NOT = ALL '+'                                    
094600*** FLYTTAR NYCKLAR                                                       
094700     MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                        
094800                                   W-IDPRODNR-WDE6                        
094900                                   W-IDPRODNR-WDQ3D                       
095000                                   W-IDPRODNR-MIN                         
095100                                   W-IDPRODNR-MAX                         
095200                                                                          
095300     IF MID-KDKOLLI (RADIND) NOT = ALL '+'                                
095400        MOVE MID-KDKOLLI  (RADIND) TO W-KDKOLLI-WDK5                      
095500     END-IF                                                               
095600                                                                          
095700     IF MID-IDKOLLI (RADIND) NOT = ALL '+'                                
095800        IF MID-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                      
095900           EVALUATE TRUE                                                  
096000           WHEN MID-KDKOLLI (RADIND) NOT = ALL '+'                        
096100           CONTINUE                                                       
096200           WHEN MID-KDEMBTYP (RADIND) NOT = ALL '+'                       
096300                                                                          
096400              IF MID-DIKOLLIL (RADIND) = ALL '+'                          
096500                 MOVE FEL     TO WS-INDATA-TEST                           
096600                 MOVE MFS-NUM-FAELT-FEL TO                                
096700                              MOD-DIKOLLIL-ATTR (RADIND)                  
096800              END-IF                                                      
096900                                                                          
097000              IF MID-DIKOLLIB (RADIND) = ALL '+'                          
097100                 MOVE FEL     TO WS-INDATA-TEST                           
097200                 MOVE MFS-NUM-FAELT-FEL TO                                
097300                              MOD-DIKOLLIB-ATTR (RADIND)                  
097400              END-IF                                                      
097500                                                                          
097600              IF MID-DIKOLLIH (RADIND) = ALL '+'                          
097700                 MOVE FEL     TO WS-INDATA-TEST                           
097800                 MOVE MFS-NUM-FAELT-FEL TO                                
097900                              MOD-DIKOLLIH-ATTR (RADIND)                  
098000              END-IF                                                      
098100                                                                          
098200           WHEN OTHER                                                     
098300              MOVE FEL    TO WS-INDATA-TEST                               
098400              MOVE MFS-ALFA-FAELT-FEL TO                                  
098500                                 MOD-KDKOLLI-ATTR (RADIND)                
098600           END-EVALUATE                                                   
098700*       ELSE                                                              
098800*             MOVE FEL    TO WS-INDATA-TEST                               
098900*             MOVE MFS-NUM-FAELT-FEL TO                                   
099000*                                MOD-VKORDBTO-KOLLI-ATTR (RADIND)         
099100        END-IF                                                            
099200     END-IF                                                               
099300                                                                          
099400        PERFORM IMS-GET-XXDJ-4306-STAT-GE                                 
099500        IF SEGMENT-FINNS                                                  
099600           EVALUATE TRUE                                                  
099700           WHEN 4306-KDPACLAS = 0                                         
099800           CONTINUE                                                       
099900           WHEN 4306-KDPACLAS = 1 OR 2                                    
100000              MOVE FEL     TO WS-INDATA-TEST                              
100100              MOVE MFS-NUM-FAELT-FEL TO                                   
100200                                 MOD-IDPRODNR-ATTR (RADIND)               
100300              MOVE INF-3 (INDX) TO MOD-TEMFSINF                           
100400           WHEN 4306-KDPACLAS = 3                                         
100500              MOVE FEL      TO WS-INDATA-TEST                             
100600              MOVE MFS-NUM-FAELT-FEL TO                                   
100700                                 MOD-IDPRODNR-ATTR (RADIND)               
100800              MOVE INF-4  (INDX) TO MOD-TEMFSINF                          
100900           WHEN 4306-KDPACLAS = 5                                         
101000              MOVE FEL      TO WS-INDATA-TEST                             
101100              MOVE MFS-NUM-FAELT-FEL TO                                   
101200                                 MOD-IDPRODNR-ATTR (RADIND)               
101300              MOVE INF-6  (INDX) TO MOD-TEMFSINF                          
101400           WHEN OTHER                                                     
101500              MOVE FEL      TO WS-INDATA-TEST                             
101600              MOVE MFS-NUM-FAELT-FEL TO                                   
101700                                 MOD-IDPRODNR-ATTR (RADIND)               
101800              MOVE INF-5  (INDX) TO MOD-TEMFSINF                          
101900           END-EVALUATE                                                   
102000        END-IF                                                            
102100                                                                          
102200        PERFORM IMS-GET-WDE601                                            
102300        IF SEGMENT-FINNS                                                  
102400*** FIX 920115 SVANTE ********************************                    
102500        IF VORD-KVORDRAD < 101                                            
102600*** FIX 920115 SVANTE ********************************                    
102700          IF VORD-IDDC = W-IDDC-4305                                      
102800           IF MID-IDDISTR (RADIND) = VORD-IDDISTR                         
102900              IF VORD-KDORDSTA = 1                                        
103000                 IF VORD-FLMANORD = NEJ                                   
103100                   MOVE VORD-IDDISTR TO WSWDE6-IDDISTR (RADIND)           
103200                   MOVE VORD-IDKUNDNR TO WSWDE6-IDKUNDNR (RADIND)         
103300                   MOVE VORD-KDORDKL TO WSWDE6-KDORDKL (RADIND)           
103400                   MOVE VORD-KVORDRAD TO WSWDE6-KVORDRAD (RADIND)         
103500                   MOVE VORD-KDFRAKT TO WSWDE6-KDFRAKT (RADIND)           
103600                                                                          
103700                   PERFORM IMS-GU-WDE4E1                                  
103800                   IF SEGMENT-FINNS                                       
103900                     MOVE SEQE-IDKUNDRF TO WSWDE6-IDKUNDRF(RADIND)        
104000                   ELSE                                                   
104100                      MOVE '++++++++++' TO WSWDE6-IDKUNDRF(RADIND)        
104200                   END-IF                                                 
104300                                                                          
104400                 ELSE                                                     
104500                   MOVE FEL      TO WS-INDATA-TEST                        
104600                   MOVE MFS-NUM-FAELT-FEL TO                              
104700                        MOD-IDPRODNR-ATTR(RADIND)                         
104800                   MOVE INF-2 (INDX) TO MOD-TEMFSINF                      
104900                 END-IF                                                   
105000                                                                          
105100              ELSE                                                        
105200                 MOVE FEL      TO WS-INDATA-TEST                          
105300                 MOVE MFS-NUM-FAELT-FEL TO                                
105400                                    MOD-IDPRODNR-ATTR (RADIND)            
105500              END-IF                                                      
105600                                                                          
105700           ELSE                                                           
105800              MOVE FEL      TO WS-INDATA-TEST                             
105900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)         
106000           END-IF                                                         
106100                                                                          
106200          ELSE                                                            
106300           MOVE FEL      TO WS-INDATA-TEST                                
106400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)           
106500          END-IF                                                          
106600*** FIX 920115 SVANTE ********************************                    
106700        ELSE                                                              
106800           MOVE FEL-3 (INDX)      TO MOD-TEMFSFEL                         
106900           MOVE FEL               TO WS-INDATA-TEST                       
107000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)           
107100        END-IF                                                            
107200*** FIX 920115 SVANTE ********************************                    
107300        ELSE                                                              
107400           MOVE FEL      TO WS-INDATA-TEST                                
107500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)           
107600        END-IF                                                            
107700                                                                          
107800        PERFORM CA-KONTROLL-FLERA-ORDERDELAR                              
107900                                                                          
108000        IF WS-INDATA-RAETT                                                
108100           IF MID-KDKOLLI (RADIND) NOT = ALL '+'                          
108200              PERFORM IMS-GET-EMBB                                        
108300              IF SEGMENT-SAKNAS                                           
108400                 MOVE FEL      TO WS-INDATA-TEST                          
108500                 MOVE MFS-ALFA-FAELT-FEL TO                               
108600                                     MOD-KDKOLLI-ATTR (RADIND)            
108700              ELSE                                                        
108800                 MOVE EMB-KDEMBTYP TO WSEMB-KDEMBTYP (RADIND)             
108900                 MOVE EMB-DIKOLLIL TO WSEMB-DIKOLLIL (RADIND)             
109000                 MOVE EMB-DIKOLLIB TO WSEMB-DIKOLLIB (RADIND)             
109100                 MOVE EMB-DIKOLLIH TO WSEMB-DIKOLLIH (RADIND)             
109200                                                                          
109300                 IF EMB-DIKOLLIL = +0                                     
109400                    IF MID-DIKOLLIL (RADIND) = ALL '+'                    
109500                       MOVE FEL      TO WS-INDATA-TEST                    
109600                       MOVE MFS-NUM-FAELT-FEL TO                          
109700                                     MOD-DIKOLLIL-ATTR (RADIND)           
109800                    END-IF                                                
109900                 END-IF                                                   
110000                                                                          
110100                 IF EMB-DIKOLLIB = +0                                     
110200                    IF MID-DIKOLLIB (RADIND) = ALL '+'                    
110300                       MOVE FEL      TO WS-INDATA-TEST                    
110400                       MOVE MFS-NUM-FAELT-FEL TO                          
110500                                     MOD-DIKOLLIB-ATTR (RADIND)           
110600                    END-IF                                                
110700                 END-IF                                                   
110800                                                                          
110900                 IF EMB-DIKOLLIH = +0                                     
111000                    IF MID-DIKOLLIH (RADIND) = ALL '+'                    
111100                       MOVE FEL      TO WS-INDATA-TEST                    
111200                       MOVE MFS-NUM-FAELT-FEL TO                          
111300                                     MOD-DIKOLLIH-ATTR (RADIND)           
111400                    END-IF                                                
111500                 END-IF                                                   
111600                                                                          
111700              END-IF                                                      
111800           END-IF                                                         
111900        END-IF                                                            
112000     END-IF                                                               
112100     ADD +1           TO RADIND                                           
112200     END-PERFORM                                                          
112300     ELSE                                                                 
112400       MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                                  
112500       MOVE FEL TO WS-INDATA-TEST                                         
112600     END-IF                                                               
112700                                                                          
112800                                                                          
112900     .                                                                    
113000     EJECT                                                                
113100 CA-KONTROLL-FLERA-ORDERDELAR SECTION.                                    
113200                                                                          
113300** SOFTWARE KONTROLL                                                      
113400     IF WS-INDATA-RAETT                                                   
113500        PERFORM IMS-GU-WDQ3D1                                             
113600        IF SEGMENT-FINNS                                                  
113700          IF W-IDPRODNR-WDQ3D = ODEL-IDPRODNR AND                         
113800            (ODEL-IDLEVNR = '1441 ' OR 'BP2TW') AND                       
113900             ODEL-IDPRC = '9998'                                          
114000             MOVE FEL-6 (INDX)    TO MOD-TEMFSFEL                         
114100             MOVE FEL             TO WS-INDATA-TEST                       
114200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)         
114300          END-IF                                                          
114400        END-IF                                                            
114500     END-IF                                                               
114600                                                                          
114700     IF WS-INDATA-RAETT                                                   
114800        PERFORM IMS-GU-WDQ3D1                                             
114900        PERFORM IMS-GN-WDQ3D1                                             
115000                                                                          
115100        IF SEGMENT-FINNS                                                  
115200        IF W-IDPRODNR-WDQ3D = ODEL-IDPRODNR                               
115300           MOVE FEL-2 (INDX)      TO MOD-TEMFSFEL                         
115400           MOVE FEL               TO WS-INDATA-TEST                       
115500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)           
115600        END-IF                                                            
115700        END-IF                                                            
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 D-BEARBETA SECTION.                                                      
116200***** - LÅSER ORDERN I LÅSNINGSREG. HTW4306.                              
116300***** - SKAPAR ETT SEGM ORDER PÅ ORDER-UNDER-ARBETE-BASEN,                
116400*****    HTR4312, SOM EV. TAS BORT DIREKT EFTER BEARBETNINGEN             
116500***** - SKAPAR ALLTID MINST ETT 4316-SEGMENT                              
116600                                                                          
116700     MOVE +1          TO RADIND                                           
116800                                                                          
116900     MOVE WS-IDDC              TO W-IDDC-4311                             
117000     PERFORM IMS-GET-XXDK-4311                                            
117100     PERFORM IMS-GET-XXDL-4315                                            
117200                                                                          
117300     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
117400                                                                          
117500     IF MID-RAD (RADIND) NOT = ALL '+'                                    
117600        MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                     
117700                                      W-IDPRODNR-4312                     
117800                                      W-IDPRODNR-4316                     
117900                                                                          
118000        IF MID-IDKOLLI  (RADIND) NOT = ALL '+'                            
118100           MOVE MID-IDKOLLI  (RADIND) TO W-IDKOLLI-4316                   
118200        END-IF                                                            
118300                                                                          
118400        PERFORM DA-HTW4306                                                
118500        PERFORM DB-SKAPA-4312-GEMEN                                       
118600                                                                          
118700        EVALUATE TRUE                                                     
118800        WHEN MID-NIV1 (RADIND) = ALL '+'                                  
118900           PERFORM DBA-SKAPA-4312                                         
119000           PERFORM IMS-ISRT-XXDK-4312                                     
119100        WHEN MID-NIV2 (RADIND) = ALL '+'                                  
119200           PERFORM DBB-SKAPA-4312                                         
119300           PERFORM IMS-ISRT-XXDK-4312                                     
119400        WHEN MID-FLAVVPACK (RADIND) = ALL '+'                             
119500           PERFORM DBC-SKAPA-4312                                         
119600           PERFORM IMS-ISRT-XXDK-4312                                     
119700           PERFORM DC-SKAPA-4316                                          
119800        WHEN OTHER                                                        
119900           PERFORM DBD-SKAPA-4312                                         
120000           PERFORM IMS-ISRT-XXDK-4312                                     
120100           PERFORM DCA-SKAPA-4316-GEMEN                                   
120200           PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                          
120300        END-EVALUATE                                                      
120400                                                                          
120500*****  SKAPAR ALLTID EN PTYP 004 FÖR VARJE PRODNR                         
120600        PERFORM DD-SKAPA-4316-004                                         
120700        PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                             
120800                                                                          
120900        PERFORM DE-KOLLA-4312                                             
121000     END-IF                                                               
121100     ADD +1        TO RADIND                                              
121200     END-PERFORM                                                          
121300                                                                          
121400     .                                                                    
121500     EJECT                                                                
121600 DA-HTW4306 SECTION.                                                      
121700                                                                          
121800     PERFORM IMS-GET-XXDJ-4306-STAT-GE                                    
121900                                                                          
122000     IF  SEGMENT-FINNS                                                    
122100         MOVE +1           TO 4306-KDPACLAS                               
122200         PERFORM IMS-REPL-XXDJ                                            
122300     ELSE                                                                 
122400         MOVE LOW-VALUE              TO IO-AREA-2                         
122500         MOVE MID-IDPRODNR (RADIND)  TO 4306-IDPRODNR                     
122600         MOVE LOW-VALUE              TO 4306-LOWVALUE                     
122700         MOVE NEJ                    TO 4306-FLANNULL                     
122800         MOVE +1                     TO 4306-KDPACLAS                     
122900         PERFORM IMS-ISRT-XXDJ-4306                                       
123000     END-IF                                                               
123100                                                                          
123200     .                                                                    
123300     EJECT                                                                
123400 DB-SKAPA-4312-GEMEN SECTION.                                             
123500                                                                          
123600           MOVE LOW-VALUE                 TO IO-AREA-3                    
123700           MOVE MID-IDPRODNR     (RADIND) TO 4312-IDPRODNR                
123800           MOVE LOW-VALUE                 TO 4312-LOWVALUE                
123900           MOVE MSG-SIGNON-USERID         TO 4312-IDUSER                  
124000           MOVE WSWDE6-IDDISTR   (RADIND) TO 4312-IDDISTR                 
124100           MOVE WSWDE6-IDKUNDNR  (RADIND) TO 4312-IDKUNDNR                
124200           MOVE WSWDE6-IDKUNDRF  (RADIND) TO 4312-IDKUNDRF                
124300           MOVE WSWDE6-KDORDKL   (RADIND) TO 4312-KDORDKL                 
124400           MOVE WSWDE6-KVORDRAD  (RADIND) TO 4312-KVORDRAD                
124500           MOVE WSWDE6-KDFRAKT   (RADIND) TO 4312-KDFRAKT                 
124600           ACCEPT 4312-TIDATUM FROM DATE                                  
124700           ACCEPT 4312-TIKLOCK FROM TIME                                  
124800                                                                          
124900           MOVE '4302'            TO 4312-IDTRANS                         
125000                                                                          
125100           MOVE MID-IDDISTR (RADIND) TO DIST08-IDDISTR                    
125200                                                                          
125300           IF DIST08-URSP-RAPP OR                                         
125310              DIST08-URSP-SPX OR                                          
125400             (CDC AND DIST08-URSP-RAPP-CDC)                               
125500              MOVE +1             TO 4312-KDBEHAND-URS                    
125600           ELSE                                                           
125700              MOVE +0             TO 4312-KDBEHAND-URS                    
125800           END-IF                                                         
125900                                                                          
126000     .                                                                    
126100     EJECT                                                                
126200 DBA-SKAPA-4312 SECTION.                                                  
126300                                                                          
126400           MOVE +1          TO 4312-KDBEHAND-GRUND                        
126500           MOVE +0          TO 4312-KDBEHAND-AVVIK                        
126600           MOVE +1          TO 4312-KDBEHAND-RAD                          
126700           MOVE +0          TO 4312-KDBEHAND-DEL                          
126800           MOVE +1          TO 4312-KDBEHAND-KOL                          
126900                                                                          
127000                                                                          
127100     .                                                                    
127200 DBB-SKAPA-4312 SECTION.                                                  
127300                                                                          
127400           MOVE +1          TO 4312-KDBEHAND-GRUND                        
127500           MOVE +0          TO 4312-KDBEHAND-AVVIK                        
127600           MOVE +1          TO 4312-KDBEHAND-RAD                          
127700           MOVE +0          TO 4312-KDBEHAND-DEL                          
127800           MOVE +1          TO 4312-KDBEHAND-KOL                          
127900                                                                          
128000     .                                                                    
128100 DBC-SKAPA-4312 SECTION.                                                  
128200                                                                          
128300           MOVE +1          TO 4312-KDBEHAND-GRUND                        
128400           MOVE +4          TO 4312-KDBEHAND-AVVIK                        
128500           MOVE +0          TO 4312-KDBEHAND-RAD                          
128600           MOVE +0          TO 4312-KDBEHAND-DEL                          
128700           MOVE +0          TO 4312-KDBEHAND-KOL                          
128800                                                                          
128900     .                                                                    
129000 DBD-SKAPA-4312 SECTION.                                                  
129100                                                                          
129200           MOVE +1          TO 4312-KDBEHAND-GRUND                        
129300           MOVE +1          TO 4312-KDBEHAND-AVVIK                        
129400           MOVE +0          TO 4312-KDBEHAND-RAD                          
129500           MOVE +0          TO 4312-KDBEHAND-DEL                          
129600           MOVE +0          TO 4312-KDBEHAND-KOL                          
129700                                                                          
129800     EJECT                                                                
129900******* OM DET FINNS ANNULLATIONER PÅ ORDERN SKALL ORDERRADERNA           
130000******* SKAPAS I INTERVALL I 4316-SEGM. BEROENDE PÅ HUR MÅNGA             
130100******* ANNULLERADE RADER SOM FINNS (4308-SEGM).                          
130200     .                                                                    
130300 DC-SKAPA-4316 SECTION.                                                   
130400                                                                          
130500         PERFORM DCA-SKAPA-4316-GEMEN                                     
130600         MOVE +1           TO 4316-IND                                    
130700         MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                             
130800                                                                          
130900         IF  4306-FLANNULL = NEJ                                          
131000                                                                          
131100****** MAX 200 RADER I VARJE KOLLI-POST                                   
131200            IF 4312-KVORDRAD > MAX-ANT-RAD                                
131300               PERFORM DF-MAX-200-EJ-ANNULL                               
131400            ELSE                                                          
131500               MOVE '0001' TO 4316-A-MID-IDRADNR-FOM (4316-IND)           
131600               MOVE 4312-KVORDRAD TO WS-TOM                               
131700                                                                          
131800               IF WS-TOM NOT = 1                                          
131900                  MOVE  WS-TOM  TO                                        
132000                           4316-A-MID-IDRADNR-TOM (4316-IND)              
132100               END-IF                                                     
132200                                                                          
132300               PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                      
132400            END-IF                                                        
132500                                                                          
132600         ELSE                                                             
132700             MOVE 1            TO WS-FROM                                 
132800             MOVE 4312-KVORDRAD TO WS-TOM                                 
132900             PERFORM IMS-GET-XXDJ-4308                                    
133000                                                                          
133100             PERFORM UNTIL SEGMENT-SAKNAS                                 
133200               IF  4308-KVANNANT = ZERO                                   
133300                 IF WS-FROM < 4308-IDRADNR-ORD-FROM                       
133400                     COMPUTE WS-TOM = 4308-IDRADNR-ORD-FROM - 1           
133500                     PERFORM DCC-KOLLA-IND                                
133600                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
133700                     MOVE 4312-KVORDRAD TO WS-TOM                         
133800                     PERFORM IMS-GET-XXDJ-4308                            
133900                     ADD +1           TO 4316-IND                         
134000                 ELSE                                                     
134100                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
134200                     PERFORM IMS-GET-XXDJ-4308                            
134300                 END-IF                                                   
134400               ELSE                                                       
134500                     PERFORM IMS-GET-XXDJ-4308                            
134600               END-IF                                                     
134700             END-PERFORM                                                  
134800                                                                          
134900             IF  WS-FROM > 4312-KVORDRAD                                  
135000             CONTINUE                                                     
135100             ELSE                                                         
135200                 PERFORM DCC-KOLLA-IND                                    
135300             END-IF                                                       
135400                                                                          
135500             IF 4316-IDPTYP = '002'                                       
135600                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
135700                                                                          
135800****** HÄR SKAPAS EN TOM POSTTYP = 003                                    
135900                IF 4316-A-MID-RAD (MAX-4316-IND) NOT = ALL '+'            
136000                   PERFORM DCB-SKAPA-4316-NAESTA                          
136100                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
136200                END-IF                                                    
136300                                                                          
136400             ELSE                                                         
136500                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
136600                                                                          
136700****** HÄR SKAPAS EN TOM POSTTYP = 003                                    
136800                IF 4316-B-MID-RAD (MAX-4316-IND) NOT = ALL '+'            
136900                   PERFORM DCB-SKAPA-4316-NAESTA                          
137000                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
137100                END-IF                                                    
137200                                                                          
137300             END-IF                                                       
137400         END-IF                                                           
137500                                                                          
137600     .                                                                    
137700     EJECT                                                                
137800 DCA-SKAPA-4316-GEMEN SECTION.                                            
137900                                                                          
138000        MOVE LOW-VALUE               TO IO-AREA-1                         
138100        MOVE ALL '+'                 TO 4316-WDGX4316                     
138200        MOVE MID-IDPRODNR   (RADIND) TO 4316-IDPRODNR                     
138300        MOVE '002'                   TO 4316-IDPTYP                       
138400        MOVE LOW-VALUE               TO 4316-LOWVALUE                     
138500        MOVE ZERO                    TO 4316-KDTRSTAT                     
138600        MOVE +312                    TO 4316-LL                           
138700        MOVE LOW-VALUE               TO 4316-Z1                           
138800        MOVE LOW-VALUE               TO 4316-Z2                           
138900        MOVE 'W4T315'                TO 4316-KDTRANS                      
139000        MOVE '4302'                  TO 4316-IDTRANS                      
139100        MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                     
139200                                                                          
139300        MOVE ZERO                    TO 4316-A-MID-IDANSTNR-UT            
139400                                                                          
139500        MOVE MID-IDDISTR    (RADIND) TO 4316-A-MID-IDDISTR-IN             
139600                                        4316-A-MID-IDDISTR-UT             
139700                                                                          
139800        MOVE 4312-IDKUNDNR           TO WS-IDKUNDNR                       
139900        MOVE WS-IDKUNDNR             TO 4316-A-MID-IDKUNDNR-IN            
140000                                        4316-A-MID-IDKUNDNR-UT            
140100        MOVE WS-IDDC                 TO 4316-A-MID-IDDC-IN                
140200                                        4316-A-MID-IDDC-UT                
140300                                                                          
140400        MOVE WSWDE6-IDKUNDRF (RADIND) TO 4316-A-MID-IDORDNR-IN            
140500                                         4316-A-MID-IDORDNR-UT            
140600        MOVE WS-KDPRT           TO 4316-A-MID-PRTVAL-ADRESSFL             
140700        MOVE 'U'                TO 4316-A-MID-PRTVAL-FOLJEFL              
140800                                                                          
140900        IF  MID-IDKOLLI (RADIND) NOT = ALL '+'                            
141000            MOVE MID-IDKOLLI (RADIND) TO 4316-IDKOLLI                     
141100                                         4316-A-MID-IDKOLLI-IN            
141200                                         4316-A-MID-IDKOLLI-UT            
141300        END-IF                                                            
141400                                                                          
141500        MOVE MID-IDPRODNR   (RADIND) TO 4316-A-MID-IDPRODNR-IN            
141600                                        4316-A-MID-IDPRODNR-UT            
141700                                                                          
141800        IF  MID-KDKOLLI (RADIND) NOT = ALL '+'                            
141900            MOVE MID-KDKOLLI (RADIND) TO 4316-A-MID-KDKOLLI               
142000        END-IF                                                            
142100                                                                          
142200        IF  MID-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                     
142300            MOVE WSTAB-VKORDBTO (RADIND)                                  
142400                              TO 4316-A-MID-VKORDBTO-KOLLI                
142500        END-IF                                                            
142600                                                                          
142700        IF  MID-KDEMBTYP (RADIND) = ALL '+'                               
142800            MOVE WSEMB-KDEMBTYP (RADIND) TO 4316-A-MID-KDEMBTYP           
142900        ELSE                                                              
143000            MOVE MID-KDEMBTYP (RADIND) TO 4316-A-MID-KDEMBTYP             
143100        END-IF                                                            
143200                                                                          
143300        IF  MID-DIKOLLIL (RADIND) = ALL '+'                               
143400            MOVE WSEMB-DIKOLLIL (RADIND) TO 4316-A-MID-DIKOLLIL           
143500        ELSE                                                              
143600            MOVE MID-DIKOLLIL (RADIND) TO 4316-A-MID-DIKOLLIL             
143700        END-IF                                                            
143800                                                                          
143900        IF  MID-DIKOLLIB (RADIND) = ALL '+'                               
144000            MOVE WSEMB-DIKOLLIB (RADIND) TO 4316-A-MID-DIKOLLIB           
144100        ELSE                                                              
144200            MOVE MID-DIKOLLIB (RADIND) TO 4316-A-MID-DIKOLLIB             
144300        END-IF                                                            
144400                                                                          
144500        IF  MID-DIKOLLIH (RADIND) = ALL '+'                               
144600            MOVE WSEMB-DIKOLLIH (RADIND) TO 4316-A-MID-DIKOLLIH           
144700        ELSE                                                              
144800            MOVE MID-DIKOLLIH (RADIND) TO 4316-A-MID-DIKOLLIH             
144900        END-IF                                                            
145000                                                                          
145100        IF  MID-ADRUTHYL (RADIND) NOT = ALL '+'                           
145200            MOVE MID-ADFLOMR  (RADIND) TO 4316-A-MID-ADFLOMR              
145300            MOVE MID-ADRUTNIV (RADIND) TO 4316-A-MID-ADRUTNIV             
145400                                                                          
145500            IF  MID-ADFLGEO (RADIND) NOT = ALL '+'                        
145600                MOVE MID-ADFLGEO  (RADIND) TO 4316-A-MID-ADFLGEO          
145700            ELSE                                                          
145800                MOVE 'DAG'                 TO 4316-A-MID-ADFLGEO          
145900            END-IF                                                        
146000                                                                          
146100        END-IF                                                            
146200     .                                                                    
146300     EJECT                                                                
146400 DCB-SKAPA-4316-NAESTA SECTION.                                           
146500                                                                          
146600        MOVE LOW-VALUE               TO IO-AREA-1                         
146700        MOVE ALL '+'                 TO 4316-WDGX4316                     
146800        MOVE MID-IDPRODNR   (RADIND) TO 4316-IDPRODNR                     
146900        MOVE '003'                   TO 4316-IDPTYP                       
147000        MOVE LOW-VALUE               TO 4316-LOWVALUE                     
147100        MOVE ZERO                    TO 4316-KDTRSTAT                     
147200        MOVE +277                    TO 4316-LL                           
147300        MOVE LOW-VALUE               TO 4316-Z1                           
147400        MOVE LOW-VALUE               TO 4316-Z2                           
147500        MOVE 'W4T314'                TO 4316-KDTRANS                      
147600        MOVE '4302'                  TO 4316-IDTRANS                      
147700        MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                     
147800                                                                          
147900        MOVE ZERO                    TO 4316-B-MID-IDANSTNR-UT            
148000                                                                          
148100        MOVE MID-IDDISTR    (RADIND) TO 4316-B-MID-IDDISTR-IN             
148200                                        4316-B-MID-IDDISTR-UT             
148300        MOVE WS-IDDC                 TO 4316-B-MID-IDDC-IN                
148400                                        4316-B-MID-IDDC-UT                
148500                                                                          
148600        MOVE 4312-IDKUNDNR           TO WS-IDKUNDNR                       
148700        MOVE WS-IDKUNDNR             TO 4316-B-MID-IDKUNDNR-IN            
148800                                        4316-B-MID-IDKUNDNR-UT            
148900                                                                          
149000        MOVE WSWDE6-IDKUNDRF (RADIND) TO 4316-B-MID-IDORDNR-IN            
149100                                         4316-B-MID-IDORDNR-UT            
149200                                                                          
149300        IF  MID-IDKOLLI (RADIND) NOT = ALL '+'                            
149400            MOVE MID-IDKOLLI (RADIND) TO 4316-IDKOLLI                     
149500                                         4316-B-MID-IDKOLLI-IN            
149600                                         4316-B-MID-IDKOLLI-UT            
149700        END-IF                                                            
149800                                                                          
149900        MOVE MID-IDPRODNR   (RADIND) TO 4316-B-MID-IDPRODNR-IN            
150000                                        4316-B-MID-IDPRODNR-UT            
150100        MOVE '4302'                  TO 4316-B-MID-IDTRANS-START          
150200        MOVE 'J'                     TO 4316-B-MID-FLFORTSK               
150300        MOVE 0                       TO 4316-B-MID-IDRADNR-FOM-S          
150400        MOVE 0                       TO 4316-B-MID-IDRADNR-TOM-S          
150500        MOVE 0                       TO 4316-B-MID-KVLEVART-S             
150600*BA                                                                       
150700        IF MID-PRTVAL-ADRESSFL NOT = ALL '+'                              
150800          MOVE WS-KDPRT TO 4316-B-MID-KDPRTVAL-ADRESSFL                   
150900        END-IF                                                            
151000     .                                                                    
151100     EJECT                                                                
151200 DCC-KOLLA-IND SECTION.                                                   
151300                                                                          
151400******* FÖRUTOM KONTROLL AV INDEX FÖR UPPLÄGGNING AV KOLLI-POSTER         
151500******* GÖRS EN KONTROLL AV ATT VARJE KOLLI-POST INTE INNEHÅLLER          
151600******* MER ÄN 200 RADER                                                  
151700                                                                          
151800        IF  4316-IND > MAX-4316-IND OR WS-ANT-RAD-REST = 0                
151900                                                                          
152000            IF  4316-IDPTYP = '002'                                       
152100                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
152200            ELSE                                                          
152300                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
152400            END-IF                                                        
152500                                                                          
152600            PERFORM DCB-SKAPA-4316-NAESTA                                 
152700            MOVE +1       TO 4316-IND                                     
152800            MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                          
152900        END-IF                                                            
153000                                                                          
153100        COMPUTE WS-ANT-RAD-INT = WS-TOM - WS-FROM + 1                     
153200                                                                          
153300        IF  4316-IDPTYP = '002'                                           
153400                                                                          
153500           IF  WS-ANT-RAD-INT > WS-ANT-RAD-REST                           
153600               PERFORM DG-MAX-200-RAD                                     
153700           ELSE                                                           
153800               MOVE WS-FROM      TO                                       
153900                          4316-A-MID-IDRADNR-FOM (4316-IND)               
154000                                                                          
154100               IF WS-FROM NOT = WS-TOM                                    
154200                  MOVE WS-TOM    TO                                       
154300                          4316-A-MID-IDRADNR-TOM (4316-IND)               
154400               END-IF                                                     
154500                                                                          
154600               COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                
154700                                         WS-ANT-RAD-INT                   
154800                                                                          
154900           END-IF                                                         
155000                                                                          
155100        ELSE                                                              
155200                                                                          
155300           IF   WS-ANT-RAD-INT > WS-ANT-RAD-REST                          
155400                PERFORM DG-MAX-200-RAD                                    
155500           ELSE                                                           
155600              MOVE WS-FROM      TO                                        
155700                      4316-B-MID-IDRADNR-FOM (4316-IND)                   
155800                                                                          
155900              IF WS-FROM NOT = WS-TOM                                     
156000                 MOVE WS-TOM    TO                                        
156100                         4316-B-MID-IDRADNR-TOM (4316-IND)                
156200              END-IF                                                      
156300                                                                          
156400              COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                 
156500                                        WS-ANT-RAD-INT                    
156600                                                                          
156700           END-IF                                                         
156800                                                                          
156900        END-IF                                                            
157000                                                                          
157100                                                                          
157200     .                                                                    
157300     EJECT                                                                
157400 DD-SKAPA-4316-004 SECTION.                                               
157500                                                                          
157600        MOVE LOW-VALUE               TO IO-AREA-1                         
157700        MOVE ALL '+'                 TO 4316-WDGX4316                     
157800        MOVE MID-IDPRODNR (RADIND)   TO 4316-IDPRODNR                     
157900        MOVE '004'                   TO 4316-IDPTYP                       
158000        MOVE ZERO                    TO 4316-IDKOLLI                      
158100        MOVE LOW-VALUE               TO 4316-LOWVALUE                     
158200        MOVE ZERO                    TO 4316-KDTRSTAT                     
158300        MOVE +346                    TO 4316-LL                           
158400        MOVE LOW-VALUE               TO 4316-Z1                           
158500        MOVE LOW-VALUE               TO 4316-Z2                           
158600        MOVE 'W4T398X'               TO 4316-KDTRANS                      
158700        MOVE '4302'                  TO 4316-IDTRANS                      
158800        MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                     
158900                                                                          
159000        MOVE WSWDE6-IDKUNDRF (RADIND) TO 4316-C-MID-IDORDNR-IN            
159100                                         4316-C-MID-IDORDNR-UT            
159200                                                                          
159300        MOVE MID-IDDISTR (RADIND) TO 4316-C-MID-IDDISTR-IN                
159400                                     4316-C-MID-IDDISTR-UT                
159500                                                                          
159600        MOVE 4312-IDKUNDNR           TO WS-IDKUNDNR                       
159700        MOVE WS-IDKUNDNR          TO 4316-C-MID-IDKUNDNR-IN               
159800                                     4316-C-MID-IDKUNDNR-UT               
159900                                                                          
160000                                                                          
160100        MOVE MID-IDPRODNR (RADIND) TO 4316-C-MID-IDPRODNR-IN              
160200                                      4316-C-MID-IDPRODNR-UT              
160300                                                                          
160400        MOVE ZERO                 TO 4316-C-MID-IDKOLLI-IN                
160500                                     4316-C-MID-IDKOLLI-UT                
160600                                                                          
160700        MOVE SPACE                TO 4316-C-MID-FLSVAR                    
160800                                                                          
160900     .                                                                    
161000     EJECT                                                                
161100 DE-KOLLA-4312 SECTION.                                                   
161200                                                                          
161300        PERFORM IMS-GET-XXDK-4312-STAT-BLANK                              
161400                                                                          
161500        IF ((4312-KDBEHAND-AVVIK = 0 OR 2)                                
161600        AND (4312-KDBEHAND-RAD = 0 OR 2)                                  
161700        AND (4312-KDBEHAND-DEL = 0 OR 2)                                  
161800        AND (4312-KDBEHAND-URS = 0 OR 2)                                  
161900        AND (4312-KDBEHAND-KOL = 0 OR 2))                                 
162000            PERFORM IMS-DLET-XXDK-4312                                    
162100            PERFORM  IMS-GET-XXDJ-4306-STAT-BLANK                         
162200            MOVE +3 TO 4306-KDPACLAS                                      
162300            PERFORM IMS-REPL-XXDJ                                         
162400                                                                          
162500            MOVE '002'  TO W-IDPTYP-4316                                  
162600            PERFORM IMS-GET-XXDL-4316-STAT-BLANK                          
162700            MOVE +1 TO 4316-KDTRSTAT                                      
162800            PERFORM IMS-REPL-XXDL                                         
162900                                                                          
163000            MOVE '003'  TO W-IDPTYP-4316                                  
163100            PERFORM IMS-GET-XXDL-4316-STAT-GE                             
163200                                                                          
163300            PERFORM UNTIL SEGMENT-SAKNAS                                  
163400               MOVE +1 TO 4316-KDTRSTAT                                   
163500               PERFORM IMS-REPL-XXDL                                      
163600               PERFORM IMS-GET-XXDL-4316-STAT-GE                          
163700            END-PERFORM                                                   
163800                                                                          
163900            MOVE '004'  TO W-IDPTYP-4316                                  
164000            MOVE ZERO   TO W-IDKOLLI-4316                                 
164100            PERFORM IMS-GET-XXDL-4316                                     
164200            MOVE +1 TO 4316-KDTRSTAT                                      
164300            PERFORM IMS-REPL-XXDL                                         
164400                                                                          
164500            MOVE MFS-KDMFSFOR TO PTOP-KDMFSFOR                            
164600            PERFORM IMS-INSERT-MSG-ALT-PCB                                
164700        ELSE                                                              
164800            MOVE +2 TO 4312-KDBEHAND-GRUND                                
164900            PERFORM IMS-REPL-XXDK                                         
165000        END-IF                                                            
165100                                                                          
165200     .                                                                    
165300     EJECT                                                                
165400 DF-MAX-200-EJ-ANNULL SECTION.                                            
165500                                                                          
165600***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
165700***** UPPLÄGGNING AV ORDER SOM EJ HAR ANNULLERADE RADER                   
165800***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
165900                                                                          
166000         MOVE 1      TO WS-FROM                                           
166100         MOVE MAX-ANT-RAD  TO WS-TOM                                      
166200         MOVE WS-FROM    TO                                               
166300                 4316-A-MID-IDRADNR-FOM (4316-IND)                        
166400         MOVE  WS-TOM  TO                                                 
166500                  4316-A-MID-IDRADNR-TOM (4316-IND)                       
166600         PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                            
166700         MOVE +1       TO 4316-IND                                        
166800                                                                          
166900         PERFORM UNTIL WS-TOM NOT < 4312-KVORDRAD                         
167000            PERFORM DCB-SKAPA-4316-NAESTA                                 
167100            COMPUTE WS-FROM = WS-TOM + 1                                  
167200            COMPUTE WS-TOM = WS-FROM + MAX-ANT-RAD - 1                    
167300                                                                          
167400            IF WS-TOM > 4312-KVORDRAD                                     
167500               MOVE 4312-KVORDRAD TO WS-TOM                               
167600            END-IF                                                        
167700                                                                          
167800            MOVE  WS-FROM TO                                              
167900                     4316-B-MID-IDRADNR-FOM (4316-IND)                    
168000                                                                          
168100            IF WS-FROM NOT = WS-TOM                                       
168200               MOVE  WS-TOM TO                                            
168300                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
168400            END-IF                                                        
168500                                                                          
168600            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
168700            MOVE +1       TO 4316-IND                                     
168800         END-PERFORM                                                      
168900                                                                          
169000     .                                                                    
169100     EJECT                                                                
169200 DG-MAX-200-RAD SECTION.                                                  
169300                                                                          
169400***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
169500***** UPPLÄGGNING AV ORDER SOM HAR ANNULLERADE RADER                      
169600***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
169700                                                                          
169800         MOVE WS-TOM       TO WS-TOM-SISTA                                
169900         COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1                   
170000                                                                          
170100         IF 4316-IDPTYP = '002'                                           
170200            MOVE WS-FROM    TO                                            
170300                    4316-A-MID-IDRADNR-FOM (4316-IND)                     
170400                                                                          
170500            IF WS-FROM NOT = WS-TOM                                       
170600               MOVE  WS-TOM  TO                                           
170700                        4316-A-MID-IDRADNR-TOM (4316-IND)                 
170800            END-IF                                                        
170900            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
171000                                                                          
171100         ELSE                                                             
171200            MOVE WS-FROM    TO                                            
171300                    4316-B-MID-IDRADNR-FOM (4316-IND)                     
171400                                                                          
171500            IF WS-FROM NOT = WS-TOM                                       
171600               MOVE  WS-TOM  TO                                           
171700                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
171800            END-IF                                                        
171900                                                                          
172000            PERFORM IMS-ISRT-XXDL-4316-STAT-II                            
172100         END-IF                                                           
172200                                                                          
172300         MOVE MAX-ANT-RAD          TO WS-ANT-RAD-REST                     
172400         MOVE +1                   TO 4316-IND                            
172500                                                                          
172600*****  SKAPA KOLLI-POSTER TILLS ALLA RADER I INTERVALLET                  
172700*****  ÄR PLACERADE I PT = 2 ELLER 3 HÄR SKAPAS EV FLERA PT=3             
172800                                                                          
172900         PERFORM UNTIL WS-TOM NOT < WS-TOM-SISTA                          
173000            PERFORM DCB-SKAPA-4316-NAESTA                                 
173100            COMPUTE WS-FROM = WS-TOM + 1                                  
173200            COMPUTE WS-ANT-RAD-INT = WS-TOM-SISTA - WS-FROM + 1           
173300                                                                          
173400            IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                           
173500               COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1             
173600               MOVE WS-FROM TO                                            
173700                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
173800                                                                          
173900               IF WS-FROM NOT = WS-TOM                                    
174000                  MOVE  WS-TOM  TO                                        
174100                           4316-B-MID-IDRADNR-TOM (4316-IND)              
174200               END-IF                                                     
174300                                                                          
174400               PERFORM IMS-ISRT-XXDL-4316-STAT-II                         
174500               MOVE MAX-ANT-RAD     TO WS-ANT-RAD-REST                    
174600               MOVE +1           TO 4316-IND                              
174700            ELSE                                                          
174800               MOVE WS-TOM-SISTA   TO WS-TOM                              
174900               MOVE WS-FROM TO                                            
175000                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
175100                                                                          
175200               IF WS-FROM NOT = WS-TOM                                    
175300                  MOVE  WS-TOM  TO                                        
175400                           4316-B-MID-IDRADNR-TOM (4316-IND)              
175500               END-IF                                                     
175600                                                                          
175700               COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                
175800                                         WS-ANT-RAD-INT                   
175900            END-IF                                                        
176000                                                                          
176100         END-PERFORM                                                      
176200                                                                          
176300     .                                                                    
176400     EJECT                                                                
176500 S01-FORMATETS-ATTR SECTION.                                              
176600                                                                          
176700     MOVE +1                 TO RADIND                                    
176800                                                                          
176900     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
177000     MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-ATTR        (RADIND)          
177100                                MOD-IDPRODNR-ATTR       (RADIND)          
177200                                MOD-FLAVVPACK-ATTR      (RADIND)          
177300                                MOD-IDKOLLI-ATTR        (RADIND)          
177400                                MOD-KDKOLLI-ATTR        (RADIND)          
177500                                MOD-VKORDBTO-KOLLI-ATTR (RADIND)          
177600                                MOD-ADFLGEO-ATTR        (RADIND)          
177700                                MOD-ADRUTHYL-ATTR       (RADIND)          
177800                                MOD-KDEMBTYP-ATTR       (RADIND)          
177900                                MOD-DIKOLLIL-ATTR       (RADIND)          
178000                                MOD-DIKOLLIB-ATTR       (RADIND)          
178100                                MOD-DIKOLLIH-ATTR       (RADIND)          
178200                                                                          
178300     MOVE MFS-RENSA-FAELT    TO MOD-ADFLGEO             (RADIND)          
178400                                                                          
178500     ADD +1                 TO RADIND                                     
178600     END-PERFORM                                                          
178700     MOVE MFS-FORMATETS-ATTR TO MOD-PRTVAL-ADRESSFL-ATTR                  
178800                                                                          
178900     .                                                                    
179000     EJECT                                                                
179100 S02-ROER-EJ-FAELT SECTION.                                               
179200                                                                          
179300     IF  MID-PRTVAL-ADRESSFL NOT = ALL '+'                                
179400         MOVE MFS-ROER-EJ-FAELT  TO MOD-PRTVAL-ADRESSFL                   
179500     END-IF                                                               
179600                                                                          
179700     MOVE +1                TO RADIND                                     
179800                                                                          
179900     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
180000     IF  MID-IDDISTR (RADIND) NOT = ALL '+'                               
180100         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDISTR        (RADIND)           
180200     END-IF                                                               
180300                                                                          
180400     IF  MID-IDPRODNR (RADIND) NOT = ALL '+'                              
180500         MOVE MFS-ROER-EJ-FAELT TO  MOD-IDPRODNR       (RADIND)           
180600     END-IF                                                               
180700                                                                          
180800     IF  MID-FLAVVPACK (RADIND) NOT = ALL '+'                             
180900         MOVE MFS-ROER-EJ-FAELT TO  MOD-FLAVVPACK      (RADIND)           
181000     END-IF                                                               
181100                                                                          
181200     IF  MID-IDKOLLI (RADIND) NOT = ALL '+'                               
181300         MOVE MFS-ROER-EJ-FAELT TO  MOD-IDKOLLI        (RADIND)           
181400     END-IF                                                               
181500                                                                          
181600     IF  MID-KDKOLLI (RADIND) NOT = ALL '+'                               
181700         MOVE MFS-ROER-EJ-FAELT TO  MOD-KDKOLLI        (RADIND)           
181800     END-IF                                                               
181900                                                                          
182000*    IF  MID-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                        
182100*        MOVE MFS-ROER-EJ-FAELT TO  MOD-VKORDBTO-KOLLI (RADIND)           
182200*    END-IF                                                               
182300                                                                          
182400                                                                          
182500     IF  MID-ADFLGEO (RADIND) NOT = ALL '+'                               
182600         MOVE MFS-ROER-EJ-FAELT TO  MOD-ADFLGEO        (RADIND)           
182700     END-IF                                                               
182800                                                                          
182900     IF  MID-ADRUTHYL (RADIND) NOT = ALL '+'                              
183000         MOVE MFS-ROER-EJ-FAELT TO  MOD-ADRUTHYL       (RADIND)           
183100     END-IF                                                               
183200                                                                          
183300     IF  MID-KDEMBTYP (RADIND) NOT = ALL '+'                              
183400         MOVE MFS-ROER-EJ-FAELT TO  MOD-KDEMBTYP       (RADIND)           
183500     END-IF                                                               
183600                                                                          
183700     IF  MID-DIKOLLIL (RADIND) NOT = ALL '+'                              
183800         MOVE MFS-ROER-EJ-FAELT TO  MOD-DIKOLLIL       (RADIND)           
183900     END-IF                                                               
184000                                                                          
184100     IF  MID-DIKOLLIB (RADIND) NOT = ALL '+'                              
184200         MOVE MFS-ROER-EJ-FAELT TO  MOD-DIKOLLIB       (RADIND)           
184300     END-IF                                                               
184400                                                                          
184500     IF  MID-DIKOLLIH (RADIND) NOT = ALL '+'                              
184600         MOVE MFS-ROER-EJ-FAELT TO  MOD-DIKOLLIH       (RADIND)           
184700     END-IF                                                               
184800                                                                          
184900     ADD +1                 TO RADIND                                     
185000     END-PERFORM                                                          
185100                                                                          
185200     .                                                                    
185300     EJECT                                                                
185400 S99-NAESTA-TRANS SECTION.                                                
185500                                                                          
185600        MOVE WS-IDDC              TO W-IDDC-4311                          
185700        PERFORM IMS-GET-XXDK-4311                                         
185800*** KVAL MED USERID                                                       
185900        MOVE MSG-SIGNON-USERID TO W-WDGXKEY-IDUSER-4312                   
186000        PERFORM IMS-GET-XXDK-4312-STAT-GE-F                               
186100                                                                          
186200        PERFORM UNTIL NOT (((SEGMENT-FINNS)                               
186300             AND (4312-KDBEHAND-AVVIK = +3 OR                             
186400                  4312-KDBEHAND-RAD   = +3 OR                             
186500                  4312-KDBEHAND-DEL   = +3 OR                             
186600                  4312-KDBEHAND-URS   = +3 OR                             
186700                  4312-KDBEHAND-KOL   = +3))                              
186800             OR ((SEGMENT-FINNS) AND                                      
186900                 (4312-IDTRANS NOT = '4302')))                            
187000            PERFORM IMS-GET-XXDK-4312-STAT-GE                             
187100        END-PERFORM                                                       
187200                                                                          
187300        IF  SEGMENT-FINNS                                                 
187400            EVALUATE TRUE                                                 
187500            WHEN 4312-KDBEHAND-AVVIK = 1                                  
187600               MOVE 'W4O39101'         TO MFS-IDMOD                       
187700               MOVE MOD4391-MOD-LAENGD TO MSG-KVLL                        
187800               MOVE '4391'             TO MOD4391-MOD-IDTRANS             
187900               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
188000               MOVE WS-IDPRODNR        TO MOD4391-MOD-IDPRODNR-UT         
188100               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
188200               MOVE WS-IDDISTR         TO MOD4391-MOD-IDDISTR-UT          
188300               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
188400               MOVE WS-IDKUNDNR        TO MOD4391-MOD-IDKUNDNR-UT         
188500               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
188600               MOVE WS-KDFRAKT         TO MOD4391-MOD-KDFRAKT-UT          
188700               MOVE 4312-IDKUNDRF      TO MOD4391-MOD-IDORDNR-UT          
188800               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
188900               MOVE WS-KDORDKL         TO MOD4391-MOD-KDORDKL-UT          
189000               MOVE WS-IDDC            TO MOD4391-MOD-IDDC-UT             
189100               MOVE WS-KDPRT    TO MOD4391-MOD-PRTVAL-ADRESSFL            
189200                                                                          
189300               INSPECT MOD4391-MOD-IDPRODNR-UT REPLACING                  
189400                                       LEADING ZERO BY SPACE              
189500               INSPECT MOD4391-MOD-IDDISTR-UT REPLACING                   
189600                                       LEADING ZERO BY SPACE              
189700               INSPECT MOD4391-MOD-IDKUNDNR-UT REPLACING                  
189800                                       LEADING ZERO BY SPACE              
189900               INSPECT MOD4391-MOD-KDFRAKT-UT REPLACING                   
190000                                       LEADING ZERO BY SPACE              
190100               INSPECT MOD4391-MOD-IDORDNR-UT REPLACING                   
190200                                       LEADING ZERO BY SPACE              
190300               MOVE WS-VISA-NAESTA-BILD TO WS-MSG-CALL                    
190400            WHEN 4312-KDBEHAND-AVVIK = 4                                  
190500               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
190600               MOVE WS-IDPRODNR        TO PTOP1-IDPRODNR-IN               
190700                                          PTOP1-IDPRODNR-UT               
190800               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
190900               MOVE WS-IDDISTR         TO PTOP1-IDDISTR-UT                
191000               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
191100               MOVE WS-IDKUNDNR        TO PTOP1-IDKUNDNR-UT               
191200               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
191300               MOVE WS-KDFRAKT         TO PTOP1-KDFRAKT-UT                
191400               MOVE 4312-IDKUNDRF      TO PTOP1-IDORDNR-UT                
191500               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
191600               MOVE WS-KDORDKL         TO PTOP1-KDORDKL-UT                
191700               MOVE WS-IDDC            TO PTOP1-IDDC-UT                   
191800               MOVE WS-KDPRT           TO PTOP1-PRTVAL-ADRESSFL           
191900               MOVE MFS-KDMFSFOR       TO PTOP1-KDMFSFOR                  
192000               MOVE WS-STARTA-4391     TO WS-MSG-CALL                     
192100                                                                          
192200            WHEN 4312-KDBEHAND-RAD = 1                                    
192300               MOVE 'W4O39301'         TO MFS-IDMOD                       
192400               MOVE MOD4393-MOD-LAENGD TO MSG-KVLL                        
192500               MOVE '4393'             TO MOD4393-MOD-IDTRANS             
192600               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
192700               MOVE WS-IDPRODNR        TO MOD4393-MOD-IDPRODNR-UT         
192800               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
192900               MOVE WS-IDDISTR         TO MOD4393-MOD-IDDISTR-UT          
193000               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
193100               MOVE WS-IDKUNDNR        TO MOD4393-MOD-IDKUNDNR-UT         
193200               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
193300               MOVE WS-KDFRAKT         TO MOD4393-MOD-KDFRAKT-UT          
193400               MOVE 4312-IDKUNDRF      TO MOD4393-MOD-IDORDNR-UT          
193500               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
193600               MOVE WS-KDORDKL         TO MOD4393-MOD-KDORDKL-UT          
193700               MOVE WS-IDDC            TO MOD4393-MOD-IDDC-UT             
193800               MOVE WS-KDPRT    TO MOD4393-MOD-PRTVAL-ADRESSFL            
193900                                                                          
194000               INSPECT MOD4393-MOD-IDPRODNR-UT REPLACING                  
194100                                       LEADING ZERO BY SPACE              
194200               INSPECT MOD4393-MOD-IDDISTR-UT REPLACING                   
194300                                       LEADING ZERO BY SPACE              
194400               INSPECT MOD4393-MOD-IDKUNDNR-UT REPLACING                  
194500                                       LEADING ZERO BY SPACE              
194600               INSPECT MOD4393-MOD-KDFRAKT-UT REPLACING                   
194700                                       LEADING ZERO BY SPACE              
194800               INSPECT MOD4393-MOD-IDORDNR-UT REPLACING                   
194900                                       LEADING ZERO BY SPACE              
195000               MOVE WS-VISA-NAESTA-BILD TO WS-MSG-CALL                    
195100            WHEN 4312-KDBEHAND-URS = 1                                    
195200               MOVE 'W4O39401' TO MFS-IDMOD                               
195300               MOVE MOD4394-MOD-LAENGD TO MSG-KVLL                        
195400               MOVE '4394'             TO MOD4394-MOD-IDTRANS             
195500               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
195600               MOVE WS-IDPRODNR        TO MOD4394-MOD-IDPRODNR-UT         
195700               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
195800               MOVE WS-IDDISTR         TO MOD4394-MOD-IDDISTR-UT          
195900               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
196000               MOVE WS-IDKUNDNR        TO MOD4394-MOD-IDKUNDNR-UT         
196100               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
196200               MOVE WS-KDFRAKT         TO MOD4394-MOD-KDFRAKT-UT          
196300               MOVE 4312-IDKUNDRF      TO MOD4394-MOD-IDORDNR-UT          
196400               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
196500               MOVE WS-KDORDKL         TO MOD4394-MOD-KDORDKL-UT          
196600               MOVE WS-IDDC            TO MOD4394-MOD-IDDC-UT             
196700               MOVE WS-KDPRT      TO MOD4394-MOD-PRTVAL-ADRESSFL          
196800                                                                          
196900               INSPECT MOD4394-MOD-IDPRODNR-UT REPLACING                  
197000                                       LEADING ZERO BY SPACE              
197100               INSPECT MOD4394-MOD-IDDISTR-UT REPLACING                   
197200                                       LEADING ZERO BY SPACE              
197300               INSPECT MOD4394-MOD-IDKUNDNR-UT REPLACING                  
197400                                       LEADING ZERO BY SPACE              
197500               INSPECT MOD4394-MOD-KDFRAKT-UT REPLACING                   
197600                                       LEADING ZERO BY SPACE              
197700               INSPECT MOD4394-MOD-IDORDNR-UT REPLACING                   
197800                                       LEADING ZERO BY SPACE              
197900               MOVE WS-VISA-NAESTA-BILD TO WS-MSG-CALL                    
198000            WHEN 4312-KDBEHAND-DEL = 1                                    
198100               MOVE 'W4O39501' TO MFS-IDMOD                               
198200               MOVE M4395-MOD-LAENGD TO MSG-KVLL                          
198300               MOVE '4395'             TO M4395-MOD-IDTRANS               
198400               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
198500               MOVE WS-IDPRODNR        TO M4395-MOD-IDPRODNR-UT           
198600               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
198700               MOVE WS-IDDISTR         TO M4395-MOD-IDDISTR-UT            
198800               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
198900               MOVE WS-IDKUNDNR        TO M4395-MOD-IDKUNDNR-UT           
199000               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
199100               MOVE WS-KDFRAKT         TO M4395-MOD-KDFRAKT-UT            
199200               MOVE 4312-IDKUNDRF      TO M4395-MOD-IDORDNR-UT            
199300               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
199400               MOVE WS-KDORDKL         TO M4395-MOD-KDORDKL-UT            
199500               MOVE WS-IDDC            TO M4395-MOD-IDDC-UT               
199600               MOVE WS-KDPRT       TO M4395-MOD-PRTVAL-ADRESSFL           
199700                                                                          
199800               INSPECT M4395-MOD-IDPRODNR-UT REPLACING                    
199900                                       LEADING ZERO BY SPACE              
200000               INSPECT M4395-MOD-IDDISTR-UT REPLACING                     
200100                                       LEADING ZERO BY SPACE              
200200               INSPECT M4395-MOD-IDKUNDNR-UT REPLACING                    
200300                                       LEADING ZERO BY SPACE              
200400               INSPECT M4395-MOD-KDFRAKT-UT REPLACING                     
200500                                       LEADING ZERO BY SPACE              
200600               INSPECT M4395-MOD-IDORDNR-UT REPLACING                     
200700                                       LEADING ZERO BY SPACE              
200800               MOVE WS-VISA-NAESTA-BILD TO WS-MSG-CALL                    
200900            WHEN OTHER                                                    
201000               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
201100               MOVE WS-IDPRODNR        TO PTOP2-IDPRODNR-IN               
201200                                          PTOP2-IDPRODNR-UT               
201300               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
201400               MOVE WS-IDDISTR         TO PTOP2-IDDISTR-UT                
201500               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
201600               MOVE WS-IDKUNDNR        TO PTOP2-IDKUNDNR-UT               
201700               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
201800               MOVE WS-KDFRAKT         TO PTOP2-KDFRAKT-UT                
201900               MOVE 4312-IDKUNDRF      TO PTOP2-IDORDNR-UT                
202000               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
202100               MOVE WS-KDORDKL         TO PTOP2-KDORDKL-UT                
202200               MOVE WS-IDDC            TO PTOP2-IDDC-UT                   
202300               MOVE WS-KDPRT           TO PTOP2-PRTVAL-ADRESSFL           
202400               MOVE MFS-KDMFSFOR       TO PTOP2-KDMFSFOR                  
202500               MOVE WS-STARTA-4396     TO WS-MSG-CALL                     
202600            END-EVALUATE                                                  
202700        ELSE                                                              
202800            IF  MID-W4I30201 NOT = ALL '+'                                
202900               PERFORM S01-FORMATETS-ATTR                                 
203000               MOVE INF-1 (INDX) TO MOD-TEMFSINF                          
203100            END-IF                                                        
203200            MOVE MAX-MOD-LAENGD TO MSG-KVLL                               
203300        END-IF                                                            
203400     .                                                                    
203500     EJECT                                                                
203600 MFS-ROER-EJ-FAELT-MOD-INFAELT         SECTION.                           
203700                                                                          
203800     MOVE MFS-ROER-EJ-FAELT   TO MOD-PRTVAL-ADRESSFL                      
203900     MOVE +1                TO RADIND                                     
204000     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
204100       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDISTR        (RADIND)            
204200                                 MOD-IDPRODNR       (RADIND)              
204300                                 MOD-FLAVVPACK      (RADIND)              
204400                                 MOD-IDKOLLI        (RADIND)              
204500                                 MOD-KDKOLLI        (RADIND)              
204600                                 MOD-VKORDBTO-KOLLI (RADIND)              
204700                                 MOD-ADFLGEO        (RADIND)              
204800                                 MOD-ADRUTHYL       (RADIND)              
204900                                 MOD-KDEMBTYP       (RADIND)              
205000                                 MOD-DIKOLLIL       (RADIND)              
205100                                 MOD-DIKOLLIB       (RADIND)              
205200                                 MOD-DIKOLLIH       (RADIND)              
205300       ADD +1                 TO RADIND                                   
205400     END-PERFORM                                                          
205500     .                                                                    
205600     SKIP3                                                                
205700* IMS SEKTIONER                                                           
205800     SKIP3                                                                
205900 IMS-GET-MSG SECTION.                                                     
206000     MOVE '  QC' TO GODK-STATUSKODER                                      
206100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
206200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
206300     PERFORM IMS-STATUSKONTROLL                                           
206400                                                                          
206500     .                                                                    
206600 IMS-INSERT-MSG SECTION.                                                  
206700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
206800       MOVE '0' TO MFS-KDHUVOMR                                           
206900     END-IF                                                               
207000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
207100     MOVE SPACE TO GODK-STATUSKODER                                       
207200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
207300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
207400     PERFORM IMS-STATUSKONTROLL                                           
207500     SKIP3                                                                
207600     .                                                                    
207700 IMS-INSERT-MSG-ALT-PCB SECTION.                                          
207800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
207900     MOVE SPACE TO GODK-STATUSKODER                                       
208000     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
208100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     EJECT                                                                
208500 IMS-INSERT-MSG-ALT1-PCB SECTION.                                         
208600     MOVE SPACE TO GODK-STATUSKODER                                       
208700     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
208800     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
208900     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
209100     EJECT                                                                
209200 IMS-INSERT-MSG-ALT2-PCB SECTION.                                         
209300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
209400     MOVE SPACE TO GODK-STATUSKODER                                       
209500     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
209600     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
209700     PERFORM IMS-STATUSKONTROLL                                           
209800     .                                                                    
209900     EJECT                                                                
210000 IMS-GET-XXDJ-4305 SECTION.                                               
210100     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
210200            DELIMITED BY SIZE INTO SSA1                                   
210300     MOVE '  GE'   TO GODK-STATUSKODER                                    
210400     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA-2 SSA1                    
210500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700                                                                          
210800     .                                                                    
210900 IMS-GET-XXDJ-4306-STAT-GE SECTION.                                       
211000     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
211100            DELIMITED BY SIZE INTO SSA1                                   
211200     MOVE '  GE' TO GODK-STATUSKODER                                      
211300     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
211400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
211500     PERFORM IMS-STATUSKONTROLL                                           
211600                                                                          
211700     .                                                                    
211800 IMS-GET-XXDJ-4306-STAT-BLANK SECTION.                                    
211900     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
212000            DELIMITED BY SIZE INTO SSA1                                   
212100     MOVE '  '   TO GODK-STATUSKODER                                      
212200     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
212300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
212400     PERFORM IMS-STATUSKONTROLL                                           
212500                                                                          
212600     .                                                                    
212700 IMS-ISRT-XXDJ-4306 SECTION.                                              
212800     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
212900            DELIMITED BY SIZE INTO SSA1                                   
213000     MOVE 'WLXXDJ11 '   TO SSA2                                           
213100     MOVE '  '   TO GODK-STATUSKODER                                      
213200     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2             
213300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
213400     PERFORM IMS-STATUSKONTROLL                                           
213500                                                                          
213600     .                                                                    
213700 IMS-GET-XXDJ-4308 SECTION.                                               
213800     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
213900            DELIMITED BY SIZE INTO SSA1                                   
214000     MOVE 'WLXXDJ21 '    TO SSA2                                          
214100     MOVE '  GE' TO GODK-STATUSKODER                                      
214200     CALL CBLTDLI USING GNP XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2              
214300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
214400     PERFORM IMS-STATUSKONTROLL                                           
214500                                                                          
214600     .                                                                    
214700 IMS-REPL-XXDJ SECTION.                                                   
214800     MOVE '  '   TO GODK-STATUSKODER                                      
214900     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA-2                       
215000     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
215100     PERFORM IMS-STATUSKONTROLL                                           
215200     .                                                                    
215300     EJECT                                                                
215400 IMS-GET-XXDK-4311 SECTION.                                               
215500     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
215600            DELIMITED BY SIZE INTO SSA1                                   
215700     MOVE '  '     TO GODK-STATUSKODER                                    
215800     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA-3 SSA1                    
215900     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
216000     PERFORM IMS-STATUSKONTROLL                                           
216100                                                                          
216200     .                                                                    
216300 IMS-GET-XXDK-4312-STAT-BLANK SECTION.                                    
216400     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
216500            DELIMITED BY SIZE INTO SSA1                                   
216600     MOVE '  '     TO GODK-STATUSKODER                                    
216700     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA-3 SSA1                  
216800     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
216900     PERFORM IMS-STATUSKONTROLL                                           
217000                                                                          
217100     .                                                                    
217200 IMS-GET-XXDK-4312-STAT-GE SECTION.                                       
217300     STRING 'WLXXDK11(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'               
217400            DELIMITED BY SIZE INTO SSA1                                   
217500     MOVE '  GE'   TO GODK-STATUSKODER                                    
217600     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
217700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
217800     PERFORM IMS-STATUSKONTROLL                                           
217900                                                                          
218000     .                                                                    
218100 IMS-GET-XXDK-4312-STAT-GE-F SECTION.                                     
218200     STRING 'WLXXDK11*F(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'             
218300            DELIMITED BY SIZE INTO SSA1                                   
218400     MOVE '  GE'   TO GODK-STATUSKODER                                    
218500     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
218600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
218700     PERFORM IMS-STATUSKONTROLL                                           
218800                                                                          
218900     .                                                                    
219000 IMS-ISRT-XXDK-4312 SECTION.                                              
219100     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
219200            DELIMITED BY SIZE INTO SSA1                                   
219300     MOVE 'WLXXDK11 '   TO SSA2                                           
219400     MOVE '  '   TO GODK-STATUSKODER                                      
219500     CALL CBLTDLI USING ISRT XXDK-PCB DLI-IO-AREA-3 SSA1 SSA2             
219600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
219700     PERFORM IMS-STATUSKONTROLL                                           
219800                                                                          
219900     .                                                                    
220000 IMS-DLET-XXDK-4312 SECTION.                                              
220100     MOVE '  '   TO GODK-STATUSKODER                                      
220200     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA-3                       
220300     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
220400     PERFORM IMS-STATUSKONTROLL                                           
220500                                                                          
220600     .                                                                    
220700 IMS-REPL-XXDK SECTION.                                                   
220800     MOVE '  '   TO GODK-STATUSKODER                                      
220900     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA-3                       
221000     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
221100     PERFORM IMS-STATUSKONTROLL                                           
221200     .                                                                    
221300     EJECT                                                                
221400 IMS-GET-XXDL-4315 SECTION.                                               
221500     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
221600            DELIMITED BY SIZE INTO SSA1                                   
221700     MOVE '  '     TO GODK-STATUSKODER                                    
221800     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA-1 SSA1                    
221900     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
222000     PERFORM IMS-STATUSKONTROLL                                           
222100                                                                          
222200     .                                                                    
222300 IMS-GET-XXDL-4316 SECTION.                                               
222400     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
222500            DELIMITED BY SIZE INTO SSA1                                   
222600     MOVE '  '     TO GODK-STATUSKODER                                    
222700     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
222800     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
222900     PERFORM IMS-STATUSKONTROLL                                           
223000                                                                          
223100     .                                                                    
223200 IMS-GET-XXDL-4316-STAT-BLANK SECTION.                                    
223300     STRING 'WLXXDL11*F(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
223400            DELIMITED BY SIZE INTO SSA1                                   
223500     MOVE '  '     TO GODK-STATUSKODER                                    
223600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
223700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
223800     PERFORM IMS-STATUSKONTROLL                                           
223900                                                                          
224000     .                                                                    
224100 IMS-GET-XXDL-4316-STAT-GE SECTION.                                       
224200     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
224300            DELIMITED BY SIZE INTO SSA1                                   
224400     MOVE '  GE'     TO GODK-STATUSKODER                                  
224500     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
224600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
224700     PERFORM IMS-STATUSKONTROLL                                           
224800                                                                          
224900     .                                                                    
225000 IMS-ISRT-XXDL-4316-STAT-BLANK SECTION.                                   
225100     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
225200            DELIMITED BY SIZE INTO SSA1                                   
225300     MOVE 'WLXXDL11 '   TO SSA2                                           
225400     MOVE '  '   TO GODK-STATUSKODER                                      
225500     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
225600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
225700     PERFORM IMS-STATUSKONTROLL                                           
225800                                                                          
225900     .                                                                    
226000 IMS-ISRT-XXDL-4316-STAT-II  SECTION.                                     
226100     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
226200            DELIMITED BY SIZE INTO SSA1                                   
226300     MOVE 'WLXXDL11 '   TO SSA2                                           
226400     MOVE '  II'   TO GODK-STATUSKODER                                    
226500     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
226600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
226700     PERFORM IMS-STATUSKONTROLL                                           
226800                                                                          
226900     .                                                                    
227000 IMS-REPL-XXDL SECTION.                                                   
227100     MOVE '  '   TO GODK-STATUSKODER                                      
227200     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA-1                       
227300     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
227400     PERFORM IMS-STATUSKONTROLL                                           
227500     .                                                                    
227600     EJECT                                                                
227700 IMS-GET-EMBB SECTION.                                                    
227800     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
227900            DELIMITED BY SIZE INTO SSA1                                   
228000     MOVE '  GE' TO GODK-STATUSKODER                                      
228100     CALL CBLTDLI USING GHU EMBB-PCB DLI-IO-AREA-1 SSA1                   
228200     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
228300     PERFORM IMS-STATUSKONTROLL                                           
228400     .                                                                    
228500     EJECT                                                                
228600 IMS-GET-WDE601 SECTION.                                                  
228700     STRING 'WDE601  (IDPRODNR =' W-WDE6-X ')'                            
228800            DELIMITED BY SIZE INTO SSA1                                   
228900     MOVE '  GE' TO GODK-STATUSKODER                                      
229000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
229100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
229200     PERFORM IMS-STATUSKONTROLL                                           
229300                                                                          
229400     .                                                                    
229500 IMS-GU-WDE4E1      SECTION.                                              
229600     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
229700                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
229800          DELIMITED BY SIZE INTO SSA1                                     
229900     MOVE '  GE' TO GODK-STATUSKODER                                      
230000     CALL CBLTDLI USING GU E4E1-PCB DLI-IO-E4E1 SSA1                      
230100     MOVE E4E1-STATUS-CODE TO STATUS-WS                                   
230200     PERFORM IMS-STATUSKONTROLL                                           
230300                                                                          
230400     .                                                                    
230500 IMS-GU-WDQ3D1 SECTION.                                                   
230600     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
230700            DELIMITED BY SIZE INTO SSA1                                   
230800     MOVE '    ' TO GODK-STATUSKODER                                      
230900     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
231000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
231100     PERFORM IMS-STATUSKONTROLL                                           
231200     .                                                                    
231300     SKIP2                                                                
231400 IMS-GN-WDQ3D1 SECTION.                                                   
231500     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
231600            DELIMITED BY SIZE INTO SSA1                                   
231700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
231800     CALL CBLTDLI USING GN  ORQA-PCB ODEL-WDQ301 SSA1                     
231900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
232000     PERFORM IMS-STATUSKONTROLL                                           
232100     .                                                                    
232200     EJECT                                                                
232300 IMS-STATUSKONTROLL SECTION.                                              
232400     SET STATUS-IX TO 1                                                   
232500     SEARCH GODK-STATUS AT END CALL FELLOG                                
232600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
232700     END-SEARCH                                                           
232800     SKIP2                                                                
232900     .                                                                    
