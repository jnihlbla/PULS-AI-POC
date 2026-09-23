000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4030300.                                                
000400 AUTHOR.         SVANTE BJÖRKBERG.                                        
000500     DATE-WRITTEN.   JUNI 1988.                                           
000600*                                                                         
000700*    FUNKTION.                                                            
000800*        BILD 4303.                                                       
000900*        PACKNINGSRAPPORTERING ORDERVIS                                   
001000*        UDDA-BILD SVERIGE.                                               
001100*                                                                         
001200*        (I EN FÖRSTA VERSION AV PROGRAMMET KUNDE MAN BARA                
001300*        RAPPORTERA EN ORDER ÅT GÅNGEN. MEN NÄR MAN SKREV                 
001400*        OM PROGRAMMET (JUNI -88) BLEV ÄNDRINGARNA SÅ OMFATTANDE          
001500*        ATT MAN ÄNDRADE "LEVEL" TILL 1 OCH SLÄNGDE ALLA GAMLA            
001600*        LOGGAR.)                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T303                                              
002000*        MID:         W4I30301-MID.                                       
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O30301-MOD.                                       
002400*    SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77   PROGRAM-NAMN           VALUE 'W4030300'                             
003300                                 PIC X(8).                                
003400 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003600 77    RADIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
003700 77    JMF-IND                   PIC S9(9)   VALUE +0   COMP SYNC.        
003800 77    MAX-RADIND-PLUS-1         PIC S9(9)   VALUE +14  COMP SYNC.        
003900 77    4316-IND                  PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77    MAX-4316-IND              PIC S9(9)   VALUE +12  COMP SYNC.        
004100 77    MAX-4316-IND-PLUS-1       PIC S9(9)   VALUE +13  COMP SYNC.        
004200 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +421 COMP SYNC.        
004300 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
004400 77    M4391-MOD-LAENGD          PIC S9(4)   VALUE +84  COMP SYNC.        
004500 77    M4392-MOD-LAENGD          PIC S9(4)   VALUE +84  COMP SYNC.        
004600 77    RAETT                     PIC X       VALUE 'R'.                   
004700 77    FEL                       PIC X       VALUE 'F'.                   
004800 77    JA                        PIC X       VALUE 'J'.                   
004900 77    NEJ                       PIC X       VALUE 'N'.                   
005000 77    SW-RAD-FINNS              PIC X       VALUE 'N'.                   
005100     EJECT                                                                
005200 01  WS-MSG-CALL-GRP.                                                     
005300   03  WS-MSG-CALL               PIC X.                                   
005400     88  VISA-NAESTA-BILD                    VALUE '0'.                   
005500     88  STARTA-4301                         VALUE '1'.                   
005600     88  STARTA-4302                         VALUE '2'.                   
005700     88  STARTA-4303                         VALUE '3'.                   
005800     88  STARTA-4391                         VALUE '4'.                   
005900     88  STARTA-4392                         VALUE '5'.                   
006000     88  STARTA-4393                         VALUE '6'.                   
006100     88  STARTA-4394                         VALUE '7'.                   
006200     88  STARTA-4395                         VALUE '8'.                   
006300     88  STARTA-4396                         VALUE '9'.                   
006400   03  WS-VISA-NAESTA-BILD       PIC X       VALUE '0'.                   
006500   03  WS-STARTA-4301            PIC X       VALUE '1'.                   
006600   03  WS-STARTA-4302            PIC X       VALUE '2'.                   
006700   03  WS-STARTA-4303            PIC X       VALUE '3'.                   
006800   03  WS-STARTA-4391            PIC X       VALUE '4'.                   
006900   03  WS-STARTA-4392            PIC X       VALUE '5'.                   
007000   03  WS-STARTA-4393            PIC X       VALUE '6'.                   
007100   03  WS-STARTA-4394            PIC X       VALUE '7'.                   
007200   03  WS-STARTA-4395            PIC X       VALUE '8'.                   
007300   03  WS-STARTA-4396            PIC X       VALUE '9'.                   
007400     EJECT                                                                
007500 01    DYNAMISKA-SUBPROGRAM.                                              
007600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007800   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*01 -COPY WMSGINIT                                                        
008200     EJECT                                                                
008300 01    FILLER                    PIC X(16)   VALUE 'WS-MODNAMN'.          
008400 01    WS-MODNAMN.                                                        
008500   03    FILLER                  PIC X       VALUE 'W'.                   
008600   03    WS-MOD-IDTRANS-POS-1    PIC X.                                   
008700   03    FILLER                  PIC X       VALUE 'O'.                   
008800   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3).                                
008900   03    FILLER                  PIC X(2)    VALUE '01'.                  
009000                                                                          
009100 01    FILLER                    PIC X(16)                                
009200                                 VALUE 'WS-IDTRANS-MOD'.                  
009300 01    WS-IDTRANS-MOD.                                                    
009400   03    WS-IDTRANS-POS-1-MOD    PIC X.                                   
009500   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3).                                
009600     EJECT                                                                
009700 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
009800 01    DIVERSE.                                                           
009900                                                                          
010000   03    WS-INDATA-TEST          PIC X       VALUE SPACE.                 
010100         88  WS-INDATA-FEL                   VALUE 'F'.                   
010200         88  WS-INDATA-RAETT                 VALUE 'R'.                   
010300                                                                          
010400   03    MAX-ANT-RAD             PIC S9(9)   VALUE +300.                  
010500   03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
010600   03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
010700   03    WS-TOM-SISTA            PIC 9(4)    VALUE ZERO.                  
010800                                                                          
010900   03    WS-IDPRODNR             PIC 9(7)    VALUE ZERO.                  
011000   03    WS-IDDISTR              PIC 9(4)    VALUE ZERO.                  
011100   03    WS-IDKOLLI              PIC 9(5)    VALUE ZERO.                  
011200   03    WS-IDKUNDNR             PIC 9(6)    VALUE ZERO.                  
011300   03    WS-KDFRAKT              PIC 9(2)    VALUE ZERO.                  
011400   03    WS-KDORDKL              PIC 9       VALUE ZERO.                  
011500   03    WS-KVORDRAD             PIC 9(4)    VALUE ZERO.                  
011600                                                                          
011700   03    WS-FROM                 PIC 9(4)    VALUE ZERO.                  
011800   03    WS-TOM                  PIC 9(4)    VALUE ZERO.                  
011900                                                                          
012000   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
012100         88  WS-EGEN-BILD        VALUE '4303'.                            
012200*      --- VALID IDDD CODES                                               
012300*                                                                         
012400*01    -COPY WWDC99                                                       
012500       EJECT                                                              
012600                                                                          
012700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012800     88  NYCKLAR-OK                          VALUE 'J'.                   
012900     88  NYCKLAR-FEL                         VALUE 'F'.                   
013000                                                                          
013100 01  FILLER                    PIC X(16)   VALUE 'WS-TAB'.                
013200 01  WS-TAB.                                                              
013300   03  WS-TABSTEG OCCURS 13.                                              
013400     05  WSWDE6-IDDISTR          PIC S9(5) VALUE ZERO  COMP-3.            
013500     05  WSWDE6-IDKUNDNR         PIC S9(7) VALUE ZERO  COMP-3.            
013600     05  WSWDE6-KDORDKL          PIC S9    VALUE ZERO  COMP-3.            
013700     05  WSWDE6-KVORDRAD         PIC S9(5) VALUE ZERO  COMP-3.            
013800     05  WSWDE6-KDFRAKT          PIC S9(3) VALUE ZERO  COMP-3.            
013900     05  WSWDE6-IDKUNDRF         PIC X(10) VALUE SPACE.                   
014000     05  WSWDE6-IDKOLLI          PIC X(5)  VALUE SPACE.                   
014100     05  WSWDE6-IDKOLLI-NUM      PIC S9(3) VALUE ZERO  COMP-3.            
014200     EJECT                                                                
014300 01  FILLER                    PIC X(16)   VALUE 'VIKT  '.                
014400     SKIP3                                                                
014500                                                                          
014600 01  WS-VKORDBTO-X.                                                       
014700   03  WS-VKORDBTO-RED           PIC  9(5).9 VALUE ZERO.                  
014800     SKIP3                                                                
014900                                                                          
015000 01  WS-IDEDITDATA               PIC 9(5)V9  VALUE ZERO.                  
015100 01  WS-VKORDBTO REDEFINES WS-IDEDITDATA.                                 
015200   03  FILLER                    PIC  9(5).                               
015300   03  WS-VKORDBTO-DEC           PIC  9.                                  
015400     SKIP3                                                                
015500                                                                          
015600 01  FILLER.                                                              
015700   03  WS-TABSTEG OCCURS 13.                                              
015800     05  WSEMB-KDEMBTYP          PIC 9.                                   
015900     05  WSTAB-VKORDBTO-X.                                                
016000       07  WSTAB-VKORDBTO          PIC 9(7).                              
016100     EJECT                                                                
016200 01    FILLER                    PIC X(16)   VALUE 'WDECAREA'.            
016300*01    -COPY WDECAREA                                                     
016400     EJECT                                                                
016500 01    FILLER                    PIC X(16)   VALUE 'WWDIST  '.            
016600 01    TEST-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.           
016700 01    FILLER REDEFINES TEST-IDDISTR.                                     
016800*  03  -COPY WWDIST03.                                                    
016900     SKIP2                                                                
017000 01    FILLER REDEFINES TEST-IDDISTR.                                     
017100*  03  -COPY WWDIST19.                                                    
017200     EJECT                                                                
017300 01    FILLER                    PIC X(16)                                
017400                                 VALUE 'NYCKLAR-TILL-DLI'.                
017500 01    NYCKLAR-TILL-DLI.                                                  
017600   03    W-WDGXKEY-4305-X.                                                
017700     05    FILLER                PIC X(4)    VALUE '4305'.                
017800     05    W-IDDC-4305           PIC X(2).                                
017900     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
018000                                                                          
018100   03    W-WDGXKEY-4306-X.                                                
018200     05    W-IDPRODNR-4306       PIC S9(7)   VALUE ZERO  COMP-3.          
018300     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
018400                                                                          
018500   03    W-WDGXKEY-4311-X.                                                
018600     05    FILLER                PIC X(4)    VALUE '4311'.                
018700     05    W-IDDC-4311           PIC X(2).                                
018800     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
018900                                                                          
019000   03    W-WDGXKEY-4312-X.                                                
019100     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
019200     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
019300                                                                          
019400   03    W-WDGXKEY-IDUSER-4312   PIC X(8)    VALUE SPACE.                 
019500                                                                          
019600   03    W-WDGXKEY-4315-X.                                                
019700     05    FILLER                PIC X(4)    VALUE '4315'.                
019800     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
019900                                                                          
020000   03    W-WDGXKEY-4316-X.                                                
020100     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
020200     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
020300     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
020400     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
020500                                                                          
020600   03    W-WDE4KEY-X.                                                     
020700     05    W-IDDISTR-E4          PIC S9(5)   VALUE ZERO  COMP-3.          
020800     05    W-IDKUNDNR-E4         PIC S9(7)   VALUE ZERO  COMP-3.          
020900     05    W-IDKUNDRF-E4         PIC X(10).                               
021000     05    W-IDPRODNR-E4         PIC S9(7)   VALUE ZERO  COMP-3.          
021100     05    W-IDPLKLST-E4         PIC S9(3)   VALUE ZERO  COMP-3.          
021200                                                                          
021300   03    W-WDE6-X.                                                        
021400     05    W-IDPRODNR-WDE6       PIC S9(7)   VALUE ZERO  COMP-3.          
021500                                                                          
021600   03    W-WDQ3D-X.                                                       
021700     05    W-IDPRODNR-WDQ3D      PIC S9(7)   VALUE ZERO  COMP-3.          
021800     05    W-IDPLKLST-WDQ3D      PIC S9(3)   VALUE ZERO  COMP-3.          
021900                                                                          
022000   03    W-WDE4E1KY-MAX-X.                                                
022100     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
022200     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
022300                                                                          
022400   03    W-WDE4E1KY-MIN-X.                                                
022500     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
022600     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
022700                                                                          
022800     EJECT                                                                
022900 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
023000 01    MEDDELANDE.                                                        
023100                                                                          
023200   03    FEL1.                                                            
023300      05    FILLER               PIC X(40)   VALUE                        
023400           '748. UPPLYSTA FÄLT FEL'.                                      
023500      05    FILLER               PIC X(40)   VALUE                        
023600           '748. HIGHLIT FIELDS WRONG  '.                                 
023700   03    FILLER  REDEFINES FEL1.                                          
023800      05    FEL-1                PIC X(40)   OCCURS 2.                    
023900                                                                          
024000   03    FEL2.                                                            
024100      05    FILLER               PIC X(40)   VALUE                        
024200           '750. INGÅNG VIA ANNAN MENY'.                                  
024300      05    FILLER               PIC X(40)   VALUE                        
024400           '750. ENTRY THROUGH ANOTHER MENU '.                            
024500   03    FILLER  REDEFINES FEL2.                                          
024600      05    FEL-2                PIC X(40)   OCCURS 2.                    
024700                                                                          
024800   03    FEL3.                                                            
024900      05    FILLER               PIC X(40)   VALUE                        
025000           '782. ORDERN DELAD'.                                           
025100      05    FILLER               PIC X(40)   VALUE                        
025200           '782. ORDER ALREADY SPLIT'.                                    
025300   03    FILLER  REDEFINES FEL3.                                          
025400      05    FEL-3                PIC X(40)   OCCURS 2.                    
025500                                                                          
025600   03    FEL4.                                                            
025700      05    FILLER               PIC X(40)   VALUE                        
025800           'ANVÄND KOLLIVIS > 100 RADER             '.                    
025900      05    FILLER               PIC X(40)   VALUE                        
026000           'USE CASE BY CASE > 100 LINES            '.                    
026100   03    FILLER  REDEFINES FEL4.                                          
026200      05    FEL-4                PIC X(40)   OCCURS 2.                    
026300                                                                          
026400   03    FEL5.                                                            
026500      05    FILLER               PIC X(40)   VALUE                        
026600           'NYCKLAR SAKNAS                          '.                    
026700      05    FILLER               PIC X(40)   VALUE                        
026800           'KEYES ARE MISSING                       '.                    
026900   03    FILLER  REDEFINES FEL5.                                          
027000      05    FEL-5                PIC X(40)   OCCURS 2.                    
027100                                                                          
027200   03    FEL6.                                                            
027300      05    FILLER               PIC X(40)   VALUE                        
027400           '747 FELAKTIGT DISTRIKT.                 '.                    
027500      05    FILLER               PIC X(40)   VALUE                        
027600           '747 WRONG DISTRICT.                     '.                    
027700   03    FILLER  REDEFINES FEL6.                                          
027800      05    FEL-6                PIC X(40)   OCCURS 2.                    
027900                                                                          
028000   03    FEL7.                                                            
028100      05    FILLER               PIC X(40)   VALUE                        
028200           '749 FELAKTIG DC-KOD.                    '.                    
028300      05    FILLER               PIC X(40)   VALUE                        
028400           '749 WRONG DC-KOD.                       '.                    
028500   03    FILLER  REDEFINES FEL7.                                          
028600      05    FEL-7                PIC X(40)   OCCURS 2.                    
028700                                                                          
028800   03    FEL8.                                                            
028900      05    FILLER               PIC X(40)   VALUE                        
029000           'SOFTWARE ORDER                          '.                    
029100      05    FILLER               PIC X(40)   VALUE                        
029200           'SOFTWARE ORDER                          '.                    
029300   03    FILLER  REDEFINES FEL8.                                          
029400      05    FEL-8                PIC X(40)   OCCURS 2.                    
029500                                                                          
029600   03    FEL9.                                                            
029700      05    FILLER               PIC X(40)   VALUE                        
029800           'KOLLA ORDERN, UTSKRIVEN?, REDAN PACKAD? '.                    
029900      05    FILLER               PIC X(40)   VALUE                        
030000           'CHECK ORDER, PRINTED?, ALREADY PACKED?  '.                    
030100   03    FILLER  REDEFINES FEL9.                                          
030200      05    FEL-9                PIC X(40)   OCCURS 2.                    
030300                                                                          
030400   03    FEL10.                                                           
030500      05    FILLER               PIC X(40)   VALUE                        
030600           'DISTRIKT OCH PRODNR MATCHAR INTE.       '.                    
030700      05    FILLER               PIC X(40)   VALUE                        
030800           'WRONG DISTIKT OR PRODNUMBER.            '.                    
030900   03    FILLER  REDEFINES FEL10.                                         
031000      05    FEL-10                PIC X(40)   OCCURS 2.                   
031100                                                                          
031200   03    FEL11.                                                           
031300      05    FILLER               PIC X(40)   VALUE                        
031400           'DETTA PRODNR FINNS INTE.                '.                    
031500      05    FILLER               PIC X(40)   VALUE                        
031600           'PRODUCTION NUMBER DONT EXISTS           '.                    
031700   03    FILLER  REDEFINES FEL11.                                         
031800      05    FEL-11                PIC X(40)   OCCURS 2.                   
031900                                                                          
032000   03    INF1.                                                            
032100      05    FILLER               PIC X(40)   VALUE                        
032200           '8071 SÄTT PACKARID=0 MHA BILD 4312 '.                         
032300      05    FILLER               PIC X(40)   VALUE                        
032400           '8071 PUT PICKER=0   IN PICTURE 4312'.                         
032500   03    FILLER  REDEFINES INF1.                                          
032600      05    INF-1                PIC X(40)   OCCURS 2.                    
032700                                                                          
032800   03    INF2.                                                            
032900      05    FILLER               PIC X(40)   VALUE                        
033000           '8072 RAPPORTERING PÅBÖRJAD KOLLIVIS'.                         
033100      05    FILLER               PIC X(40)   VALUE                        
033200           '8072 REPORTING PER CASE IN PROGRESS'.                         
033300   03    FILLER  REDEFINES INF2.                                          
033400      05    INF-2                PIC X(40)   OCCURS 2.                    
033500                                                                          
033600   03    INF3.                                                            
033700      05    FILLER               PIC X(40)   VALUE                        
033800           '702. ORDERVIS PACKNING PÅGÅR'.                                
033900      05    FILLER               PIC X(40)   VALUE                        
034000           '702. REPORTING PER ORDER IN PROGRESS'.                        
034100   03    FILLER  REDEFINES INF3.                                          
034200      05    INF-3                PIC X(40)   OCCURS 2.                    
034300                                                                          
034400   03    INF4.                                                            
034500      05    FILLER               PIC X(40)   VALUE                        
034600           '710. ORDERN FÄRDIGRAPPORTERAD'.                               
034700      05    FILLER               PIC X(40)   VALUE                        
034800           '710. ORDER ALREADY REPORTED     '.                            
034900   03    FILLER  REDEFINES INF4.                                          
035000      05    INF-4                PIC X(40)   OCCURS 2.                    
035100                                                                          
035200   03    INF5.                                                            
035300      05    FILLER               PIC X(40)   VALUE                        
035400           '755. PACKNING GÅR EJ - LÅSNINGSKOD FEL'.                      
035500      05    FILLER               PIC X(40)   VALUE                        
035600           'REPORT NOT POSSIBLE - LOAD CODE IS WRONG'.                    
035700   03    FILLER  REDEFINES INF5.                                          
035800      05    INF-5                PIC X(40)   OCCURS 2.                    
035900                                                                          
036000   03    INF6.                                                            
036100      05    FILLER               PIC X(40)   VALUE                        
036200           ' UPPDATERING UTFÖRD                   '.                      
036300      05    FILLER               PIC X(40)   VALUE                        
036400           ' UPDATING PERFORMED.                  '.                      
036500   03    FILLER  REDEFINES INF6.                                          
036600      05    INF-6                PIC X(40)   OCCURS 2.                    
036700                                                                          
036800     EJECT                                                                
037200******************************************************************        
037300*                                                                         
037400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
037500*                                                                         
037600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
037700     SKIP3                                                                
037800 01    FILLER                    PIC X(16)                                
037900                                 VALUE 'MID W4I30301 MID'.                
038000     SKIP3                                                                
038100*01    -COPY W4I30301                                                     
038200     EJECT                                                                
038300*01    -COPY WMSGAREA                                                     
038400     EJECT                                                                
038500*  03  MOD -COPY W4O30301   -RED MSG-AREA                                 
038600     EJECT                                                                
038700*  03  MOD -COPY W4O39101   -RED MSG-AREA -PRE M4391-                     
038800     EJECT                                                                
038900 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW'.           
039000 01    P-TO-P-SW.                                                         
039100       03  PTOP-LL               PIC S9(4)   VALUE +17 COMP SYNC.         
039200       03  PTOP-Z1               PIC X       VALUE LOW-VALUE.             
039300       03  PTOP-Z2               PIC X       VALUE LOW-VALUE.             
039400       03  PTOP-TRANSKOD         PIC X(7)    VALUE 'W0T605U'.             
039500       03  FILLER                PIC X       VALUE SPACE.                 
039600       03  FILLER                PIC X(4)    VALUE '4303'.                
039700       03  PTOP-KDMFSFOR         PIC X.                                   
039800****** 03  MID -COPY W0I60501   -PRE MOD-                                 
039900     EJECT                                                                
040000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
040100 01  P-TO-P-SW1.                                                          
040200       03  PTOP1-LL              PIC S9(4)   VALUE +187 COMP SYNC.        
040300       03  PTOP1-Z1              PIC X       VALUE LOW-VALUE.             
040400       03  PTOP1-Z2              PIC X       VALUE LOW-VALUE.             
040500       03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W4T391U'.             
040600       03  FILLER                PIC X       VALUE SPACE.                 
040700       03  FILLER                PIC X(4)    VALUE '4303'.                
040800       03  PTOP1-KDMFSFOR        PIC X.                                   
040900       03  PTOP1-IDPRODNR-IN     PIC X(7).                                
041000       03  PTOP1-IDPRODNR-UT     PIC X(7).                                
041100       03  PTOP1-IDDISTR-UT      PIC X(4).                                
041200       03  PTOP1-IDKUNDNR-UT     PIC X(6).                                
041300       03  PTOP1-KDFRAKT-UT      PIC X(2).                                
041400       03  PTOP1-IDORDNR-UT      PIC X(5).                                
041500       03  PTOP1-KDORDKL-UT      PIC X(1).                                
041600       03  PTOP1-IDDC-UT         PIC X(2).                                
041700       03  PTOP1-KDPRTVAL-ADRESSFL PIC X(2).                              
041800       03  FILLER                PIC X(134)  VALUE ALL '+'.               
041900     EJECT                                                                
042000*01    -COPY WMFSAREA                                                     
042100     EJECT                                                                
042200******************************************************************        
042300*                                                                         
042400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042500*                                                                         
042600 01    IMS-WS.                                                            
042700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
042800     SKIP3                                                                
042900*                        **** STATUS-KOD FRÅN IMS                         
043000   03    STATUS-WS               PIC XX.                                  
043100     88    SEGMENT-FINNS                     VALUE '  '.                  
043200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
043300     SKIP3                                                                
043400   03    GODK-STATUSKODER.                                                
043500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
043600     SKIP3                                                                
043700 01    SSA1                      PIC X(96).                               
043800 01    SSA2                      PIC X(64).                               
043900     EJECT                                                                
044000*                            IMS FUNKTIONSKODER                           
044100*01    -COPY W0003                                                        
044200     EJECT                                                                
044300*                            DLI INPUT-OUTPUT AREA                        
044400 01    DLI-IO-AREA-1.                                                     
044500   03    IO-AREA-1               PIC X(370)  VALUE SPACE.                 
044600     SKIP3                                                                
044700*  03    WDE601 -COPY WDE601      -RED IO-AREA-1                          
044800     EJECT                                                                
044900*  03    WDE4E1 -COPY WDE4E1      -RED IO-AREA-1                          
045000     EJECT                                                                
045100*  03    XXDL11 -COPY WDGX4316    -RED IO-AREA-1                          
045200     EJECT                                                                
045300*     08 AREA -COPY W4I31501    -RED 4316-FILLER -PRE 4316-               
045400     EJECT                                                                
045500*     08 AREA -COPY W4I31401    -RED 4316-FILLER -PRE 4316-B-             
045600     EJECT                                                                
045700*     08 AREA -COPY W4I39801    -RED 4316-FILLER -PRE 4316-C-             
045800     EJECT                                                                
045900 01    DLI-IO-AREA-2.                                                     
046000   03    IO-AREA-2               PIC X(60)  VALUE SPACE.                  
046100     SKIP3                                                                
046200*  03    XXDJ01 -COPY WDGX4305    -RED IO-AREA-2                          
046300     EJECT                                                                
046400*  03    XXDJ11 -COPY WDGX4306    -RED IO-AREA-2                          
046500     EJECT                                                                
046600*  03    XXDJ11 -COPY WDGX4308    -RED IO-AREA-2                          
046700     EJECT                                                                
046800 01    DLI-IO-AREA-3.                                                     
046900   03    IO-AREA-3               PIC X(100)  VALUE SPACE.                 
047000     SKIP3                                                                
047100*  03    XXDK11 -COPY WDGX4312    -RED IO-AREA-3                          
047200     EJECT                                                                
047300*01             -COPY WDQ301                                              
047400     EJECT                                                                
047500 01    DLI-IO-AREA-4.                                                     
047600   03    IO-AREA-4               PIC X(384)  VALUE SPACE.                 
047700                                                                          
047800*  03    WDE401 -COPY WDE401      -RED IO-AREA-4                          
047900     EJECT                                                                
048000*  03    WDE411 -COPY WDE411      -RED IO-AREA-4                          
048100     EJECT                                                                
048200 LINKAGE SECTION.                                                         
048300*01    -COPY W0009     -PRE MSG-                                          
048400     EJECT                                                                
048500*01    -COPY W0009     -PRE ALT-                                          
048600     EJECT                                                                
048700*01    -COPY W0009     -PRE ALT1-                                         
048800     EJECT                                                                
048900*01  -COPY W0008      -PRE USEA-                                          
049000     05  FILLER                  PIC X.                                   
049100     EJECT                                                                
049200*01    -COPY W0008     -PRE XXDJ-                                         
049300        05 FILLER                PIC X.                                   
049400     EJECT                                                                
049500*01    -COPY W0008     -PRE XXDK-                                         
049600        05 FILLER                PIC X.                                   
049700     EJECT                                                                
049800*01    -COPY W0008     -PRE XXDL-                                         
049900        05 FILLER                PIC X.                                   
050000     EJECT                                                                
050100*01    -COPY W0008     -PRE WDE6-                                         
050200        05 FILLER                PIC X.                                   
050300     EJECT                                                                
050400*01    -COPY W0008     -PRE WDE4E-                                        
050500        05 FILLER                PIC X.                                   
050600     EJECT                                                                
050700*01    -COPY W0008     -PRE ORQA-                                         
050800        05 FILLER                PIC X.                                   
050900     EJECT                                                                
051000*01    -COPY W0008     -PRE WDE4-                                         
051100        05 FILLER                PIC X.                                   
051200     EJECT                                                                
051300 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB ALT1-PCB USEA-PCB              
051400                           XXDJ-PCB                                       
051500                           XXDK-PCB XXDL-PCB WDE6-PCB WDE4E-PCB           
051600                           ORQA-PCB WDE4-PCB.                             
051700                                                                          
051800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT1-PCB USEA-PCB              
051900                           XXDJ-PCB                                       
052000                           XXDK-PCB XXDL-PCB WDE6-PCB WDE4E-PCB           
052100                           ORQA-PCB WDE4-PCB.                             
052200                                                                          
052300     PERFORM IMS-GET-MSG                                                  
052400                                                                          
052500     IF SEGMENT-FINNS                                                     
052600       PERFORM A-INIT-SPARA-INPUT                                         
052700       MOVE MFS-IDTRANS  TO WS-IDTRANS                                    
052800       IF WS-EGEN-BILD                                                    
052900         IF MID-W4I30301 NOT = ALL '+'                                    
053000            PERFORM B-INDATA-KOLL                                         
053100            IF WS-INDATA-RAETT                                            
053200               PERFORM C-REL-KOLL                                         
053300               IF WS-INDATA-RAETT                                         
053400                  PERFORM D-BEARBETA                                      
053500                  PERFORM S99-NAESTA-TRANS                                
053600               ELSE                                                       
053700                  PERFORM S02-ROER-EJ-FAELT                               
053800                  MOVE MAX-MOD-LAENGD TO MSG-KVLL                         
053900               END-IF                                                     
054000            ELSE                                                          
054100               MOVE MAX-MOD-LAENGD TO MSG-KVLL                            
054200               PERFORM S02-ROER-EJ-FAELT                                  
054300            END-IF                                                        
054400         ELSE                                                             
054500            PERFORM S99-NAESTA-TRANS                                      
054600         END-IF                                                           
054700       ELSE                                                               
054800          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
054900       END-IF                                                             
055000                                                                          
055100       EVALUATE TRUE                                                      
055200         WHEN VISA-NAESTA-BILD PERFORM IMS-INSERT-MSG                     
055300         WHEN STARTA-4391      PERFORM IMS-INSERT-MSG-ALT1-PCB            
055400       END-EVALUATE                                                       
055500     END-IF                                                               
055600                                                                          
055700     MOVE ZERO TO RETURN-CODE                                             
055800     GOBACK                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 A-INIT-SPARA-INPUT SECTION.                                              
056200                                                                          
056300     IF MSG-DUBBLA-TRANSKODER                                             
056400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I30301                 
056500       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
056600       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
056700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
056800       MOVE MSG-IDPFK            TO MFS-IDPFK                             
056900     ELSE                                                                 
057000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I30301                  
057100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
057200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
057300       MOVE ' ' TO MFS-KDTRTYP                                            
057400                   MFS-IDPFK                                              
057500     END-IF                                                               
057600                                                                          
057700     MOVE LOW-VALUE TO MSG-AREA                                           
057800     MOVE 'W4O303N1' TO MFS-IDMOD                                         
057900     MOVE '4303' TO MOD-IDTRANS                                           
058000     MOVE WS-VISA-NAESTA-BILD            TO WS-MSG-CALL                   
058100                                                                          
058200     MOVE +1 TO RADIND                                                    
058300     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
058400       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR        (RADIND)              
058500                                 MOD-IDPRODNR       (RADIND)              
058600                                 MOD-FLAVVPACK      (RADIND)              
058700       ADD +1 TO RADIND                                                   
058800     END-PERFORM                                                          
058900                                                                          
059000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
059100                             MOD-TEMFSINF                                 
059200                                                                          
059300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
059400     MOVE '001'             TO MSGI-KDCALL                                
059500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
059600     MOVE '4303'            TO MSGI-IDTRANS                               
059700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
059800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
059900                                                                          
060000     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
060100                               WS-IDDC                                    
060200     IF NDC                                                               
060300       MOVE FEL   TO WS-INDATA-TEST                                       
060400     END-IF                                                               
060500                                                                          
060600     IF MSGI-IDLAND-SPR = 'GB'                                            
060700       MOVE +2 TO INDX                                                    
060800     ELSE                                                                 
060900       MOVE +1 TO INDX                                                    
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300 B-INDATA-KOLL SECTION.                                                   
061400                                                                          
061500     MOVE +1 TO RADIND                                                    
061600     MOVE RAETT TO WS-INDATA-TEST                                         
061700                                                                          
061800     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
061900                                                                          
062000       IF MID-RAD (RADIND) NOT = ALL '+'                                  
062100                                                                          
062200         IF MID-IDDISTR (RADIND) = ALL '+'                                
062300           MOVE FEL TO WS-INDATA-TEST                                     
062400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)            
062500         ELSE                                                             
062600           IF MID-IDDISTR (RADIND) NUMERIC                                
062700             MOVE MID-IDDISTR (RADIND) TO TEST-IDDISTR                    
062800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-ATTR (RADIND)        
062900*            IF DIST03-SVERIGE                                            
063000*              MOVE MFS-NUM-FAELT-RAETT TO                                
063100*                      MOD-IDDISTR-ATTR (RADIND)                          
063200*            ELSE                                                         
063300*              MOVE FEL TO WS-INDATA-TEST                                 
063400*              MOVE MFS-NUM-FAELT-FEL                                     
063500*                   TO MOD-IDDISTR-ATTR(RADIND)                           
063600*              MOVE FEL-6 (INDX)    TO MOD-TEMFSFEL                       
063700*            END-IF                                                       
063800           ELSE                                                           
063900             MOVE FEL TO WS-INDATA-TEST                                   
064000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)          
064100           END-IF                                                         
064200         END-IF                                                           
064300                                                                          
064400         IF MID-IDPRODNR (RADIND) = ALL '+'                               
064500           MOVE FEL TO WS-INDATA-TEST                                     
064600           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIND)         
064700         ELSE                                                             
064800           IF MID-IDPRODNR (RADIND) NUMERIC                               
064900             MOVE MFS-NUM-FAELT-RAETT TO                                  
065000                               MOD-IDPRODNR-ATTR (RADIND)                 
065100           ELSE                                                           
065200             MOVE FEL TO WS-INDATA-TEST                                   
065300             MOVE MFS-NUM-FAELT-FEL                                       
065400                   TO MOD-IDPRODNR-ATTR(RADIND)                           
065500           END-IF                                                         
065600         END-IF                                                           
065700                                                                          
065800         IF MID-FLAVVPACK (RADIND) = ALL '+'                              
065900           CONTINUE                                                       
066000         ELSE                                                             
066100           IF MID-FLAVVPACK (RADIND) = 'J' OR 'Y'                         
066200* * * SATSORDER FÅR EJ AVVIKELSERAPPORTERAS * * *                         
066300            IF DIST19-SATS                                                
066400               MOVE FEL              TO WS-INDATA-TEST                    
066500               MOVE MFS-ALFA-FAELT-FEL                                    
066600                    TO MOD-FLAVVPACK-ATTR(RADIND)                         
066700            ELSE                                                          
066800* * * * * * * * * * * * * * * * * * * * * * * * *                         
066900              MOVE MFS-ALFA-FAELT-RAETT TO                                
067000                                MOD-FLAVVPACK-ATTR(RADIND)                
067100            END-IF                                                        
067200           ELSE                                                           
067300             MOVE FEL                TO WS-INDATA-TEST                    
067400             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAVVPACK-ATTR(RADIND)        
067500           END-IF                                                         
067600         END-IF                                                           
067700      END-IF                                                              
067800      ADD +1 TO RADIND                                                    
067900     END-PERFORM                                                          
068000                                                                          
068100     IF WS-INDATA-RAETT                                                   
068200**** KONTROLL AV ATT INGA DUBLETTER (IDPRODNR) RAPPORTERAS                
068300                                                                          
068400        MOVE +1 TO RADIND                                                 
068500        PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                      
068600           IF MID-IDPRODNR(RADIND) NOT = ALL '+'                          
068700             MOVE RADIND TO JMF-IND                                       
068800             ADD +1      TO JMF-IND                                       
068900                                                                          
069000             PERFORM UNTIL JMF-IND NOT < MAX-RADIND-PLUS-1                
069100               IF MID-IDPRODNR(JMF-IND) NOT = ALL '+'                     
069200                  IF MID-IDPRODNR(RADIND) = MID-IDPRODNR(JMF-IND)         
069300                     MOVE FEL TO WS-INDATA-TEST                           
069400                     MOVE MFS-NUM-FAELT-FEL TO                            
069500                         MOD-IDPRODNR-ATTR(RADIND)                        
069600                     MOVE MFS-NUM-FAELT-FEL TO                            
069700                            MOD-IDPRODNR-ATTR(JMF-IND)                    
069800                  END-IF                                                  
069900              END-IF                                                      
070000              ADD +1 TO JMF-IND                                           
070100            END-PERFORM                                                   
070200          END-IF                                                          
070300          ADD +1 TO RADIND                                                
070400         END-PERFORM                                                      
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 C-REL-KOLL SECTION.                                                      
070900                                                                          
071000     MOVE +1 TO RADIND                                                    
071100     MOVE WS-IDDC     TO W-IDDC-4305                                      
071200     PERFORM IMS-GET-XXDJ-4305                                            
071300                                                                          
071400     IF SEGMENT-FINNS                                                     
071500     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
071600                                                                          
071700       IF MID-RAD (RADIND) NOT = ALL '+'                                  
071800                                                                          
071900****** FLYTTAR NYCKLAR                                                    
072000         MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                    
072100                                       W-IDPRODNR-WDE6                    
072200                                       W-IDPRODNR-WDQ3D                   
072300******                                                                    
072400                                                                          
072500         PERFORM IMS-GU-WDE6-WDE601                                       
072600         IF SEGMENT-FINNS                                                 
072700*** FIX 920115 SVANTE ********************************                    
072800         IF VORD-KVORDRAD < 101 OR DIST19-SATS                            
072900*** FIX 920115 SVANTE ********************************                    
073000           IF VORD-IDDC = W-IDDC-4305                                     
073100             IF MID-IDDISTR (RADIND) = VORD-IDDISTR                       
073200               IF VORD-KDORDSTA = 1                                       
073300                 IF VORD-FLMANORD = NEJ                                   
073400                   MOVE VORD-IDDISTR TO WSWDE6-IDDISTR (RADIND)           
073500                   MOVE VORD-IDKUNDNR TO WSWDE6-IDKUNDNR (RADIND)         
073600                   MOVE VORD-KDORDKL TO WSWDE6-KDORDKL (RADIND)           
073700                   MOVE VORD-KVORDRAD TO WSWDE6-KVORDRAD (RADIND)         
073800                   MOVE VORD-KDFRAKT TO WSWDE6-KDFRAKT (RADIND)           
073900                   MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MAX             
074000                   MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MIN             
074100                   PERFORM IMS-GET-WDE4E                                  
074200                   IF SEGMENT-FINNS                                       
074300                     MOVE SEQE-IDKUNDRF TO                                
074400                                WSWDE6-IDKUNDRF (RADIND)                  
074500                     MOVE SEQE-IDPRODNR TO W-IDPRODNR-E4                  
074600                     MOVE SEQE-IDDISTR TO W-IDDISTR-E4                    
074700                     MOVE SEQE-IDKUNDRF-GRP TO W-IDKUNDRF-E4              
074800                     MOVE SEQE-IDPLKLST TO W-IDPLKLST-E4                  
075000                     MOVE +1      TO WSWDE6-IDKOLLI-NUM (RADIND)          
075010                     MOVE '00001' TO WSWDE6-IDKOLLI     (RADIND)          
075100                   ELSE                                                   
075200                     MOVE '++++++++++' TO                                 
075300                                WSWDE6-IDKUNDRF (RADIND)                  
075400                   END-IF                                                 
075500                 ELSE                                                     
075600                   MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                      
075700                   MOVE FEL TO WS-INDATA-TEST                             
075800                   MOVE MFS-NUM-FAELT-FEL TO                              
075900                                      MOD-IDPRODNR-ATTR (RADIND)          
076000                 END-IF                                                   
076100               ELSE                                                       
076200                 MOVE FEL-9 (INDX) TO MOD-TEMFSFEL                        
076300                 MOVE FEL TO WS-INDATA-TEST                               
076400                 MOVE MFS-NUM-FAELT-FEL TO                                
076500                                    MOD-IDPRODNR-ATTR (RADIND)            
076600               END-IF                                                     
076700             ELSE                                                         
076800               MOVE FEL-10 (INDX) TO MOD-TEMFSFEL                         
076900               MOVE FEL TO WS-INDATA-TEST                                 
077000               MOVE MFS-NUM-FAELT-FEL TO                                  
077100                                  MOD-IDDISTR-ATTR (RADIND)               
077200             END-IF                                                       
077300           ELSE                                                           
077400             MOVE FEL-7 (INDX) TO MOD-TEMFSFEL                            
077500             MOVE FEL TO WS-INDATA-TEST                                   
077600             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)          
077700           END-IF                                                         
077800*** FIX 920115 SVANTE ********************************                    
077900         ELSE                                                             
078000            MOVE FEL-4 (INDX)      TO MOD-TEMFSFEL                        
078100            MOVE FEL               TO WS-INDATA-TEST                      
078200            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
078300         END-IF                                                           
078400*** FIX 920115 SVANTE ********************************                    
078500         ELSE                                                             
078600             MOVE FEL-11 (INDX) TO MOD-TEMFSFEL                           
078700             MOVE FEL TO WS-INDATA-TEST                                   
078800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)         
078900         END-IF                                                           
079000                                                                          
079100         IF NOT DIST19-SATS                                               
079200           PERFORM CA-KONTROLL-FLERA-ORDERDELAR                           
079300         END-IF                                                           
079400                                                                          
079500         IF WS-INDATA-RAETT                                               
079600           PERFORM IMS-GET-XXDJ-4306-STAT-GE                              
079700           IF SEGMENT-FINNS                                               
079800             EVALUATE TRUE                                                
079900             WHEN 4306-KDPACLAS = 0                                       
080000             CONTINUE                                                     
080100             WHEN 4306-KDPACLAS = 1 OR 2                                  
080200               MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                          
080300               MOVE FEL TO WS-INDATA-TEST                                 
080400               MOVE MFS-NUM-FAELT-FEL TO                                  
080500                                  MOD-IDPRODNR-ATTR (RADIND)              
080600               MOVE INF-3 (INDX) TO MOD-TEMFSINF                          
080700             WHEN 4306-KDPACLAS = 3                                       
080800               MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                          
080900               MOVE FEL TO WS-INDATA-TEST                                 
081000               MOVE MFS-NUM-FAELT-FEL TO                                  
081100                                  MOD-IDPRODNR-ATTR (RADIND)              
081200               MOVE INF-4  (INDX) TO MOD-TEMFSINF                         
081300             WHEN 4306-KDPACLAS = 5                                       
081400               MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                          
081500               MOVE FEL TO WS-INDATA-TEST                                 
081600               MOVE MFS-NUM-FAELT-FEL TO                                  
081700                                  MOD-IDPRODNR-ATTR (RADIND)              
081800               MOVE INF-1  (INDX) TO MOD-TEMFSINF                         
081900             WHEN OTHER                                                   
082000               MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                          
082100               MOVE FEL TO WS-INDATA-TEST                                 
082200               MOVE MFS-NUM-FAELT-FEL TO                                  
082300                                  MOD-IDPRODNR-ATTR (RADIND)              
082400               MOVE INF-5  (INDX) TO MOD-TEMFSINF                         
082500             END-EVALUATE                                                 
082600           END-IF                                                         
082700         END-IF                                                           
082800       END-IF                                                             
082900      ADD +1 TO RADIND                                                    
083000     END-PERFORM                                                          
083100     ELSE                                                                 
083200       MOVE FEL-7 (INDX) TO MOD-TEMFSFEL                                  
083300       MOVE FEL TO WS-INDATA-TEST                                         
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 CA-KONTROLL-FLERA-ORDERDELAR SECTION.                                    
083800** SOFTWARE KONTROLL                                                      
083900     IF WS-INDATA-RAETT                                                   
084000        PERFORM IMS-GU-WDQ3D                                              
084100        IF SEGMENT-FINNS                                                  
084200          IF W-IDPRODNR-WDQ3D = ODEL-IDPRODNR AND                         
084300             (ODEL-IDLEVNR = '1441 ' OR 'BP2TW') AND                      
084400             ODEL-IDPRC   = '9998'                                        
084500             MOVE FEL-8 (INDX)    TO MOD-TEMFSFEL                         
084600             MOVE FEL             TO WS-INDATA-TEST                       
084700             MOVE MFS-NUM-FAELT-FEL TO                                    
084800                                    MOD-IDPRODNR-ATTR (RADIND)            
084900          END-IF                                                          
085000        END-IF                                                            
085100     END-IF                                                               
085200                                                                          
085300     IF WS-INDATA-RAETT                                                   
085400        PERFORM IMS-GU-WDQ3D                                              
085500        PERFORM IMS-GN-WDQ3D                                              
085600        IF SEGMENT-FINNS                                                  
085700        IF W-IDPRODNR-WDQ3D = ODEL-IDPRODNR                               
085800           MOVE FEL-3 (INDX)      TO MOD-TEMFSFEL                         
085900           MOVE FEL               TO WS-INDATA-TEST                       
086000           MOVE MFS-NUM-FAELT-FEL TO                                      
086100                                  MOD-IDPRODNR-ATTR (RADIND)              
086200        END-IF                                                            
086300        END-IF                                                            
086400     END-IF                                                               
086500     .                                                                    
086600                                                                          
089800 D-BEARBETA SECTION.                                                      
089900***** - LÅSER ORDERN I LÅSNINGSREG. HTW4306.                              
090000***** - SKAPAR ETT SEGM ORDER PÅ ORDER-UNDER-ARBETE-BASEN,                
090100*****    HTR4312, SOM EV. TAS BORT DIREKT EFTER BEARBETNINGEN             
090200                                                                          
090300     MOVE +1 TO RADIND                                                    
090400     MOVE WS-IDDC        TO W-IDDC-4311                                   
090500                            W-IDDC-4305                                   
090600     PERFORM IMS-GET-XXDJ-4305                                            
090700     PERFORM IMS-GET-XXDK-4311                                            
090800     PERFORM IMS-GET-XXDL-4315                                            
090900                                                                          
091000                                                                          
091100     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
091200                                                                          
091300        IF MID-RAD (RADIND) NOT = ALL '+'                                 
091400                                                                          
091500           MOVE JA                    TO SW-RAD-FINNS                     
091600***** FLYTTAR NYCKLAR                                                     
091700           MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR-4306                  
091800                                         W-IDPRODNR-4312                  
091900                                         W-IDPRODNR-4316                  
092000                                                                          
092100*****                                                                     
092200          PERFORM IMS-GET-XXDJ-4306-STAT-GE                               
092300          IF SEGMENT-FINNS                                                
092400            IF 4306-KDPACLAS = 0                                          
092500              MOVE +1 TO 4306-KDPACLAS                                    
092600              PERFORM IMS-REPL-XXDJ                                       
092700            END-IF                                                        
092800          ELSE                                                            
092900              MOVE MID-IDPRODNR(RADIND)  TO 4306-IDPRODNR                 
093000              MOVE LOW-VALUE             TO 4306-LOWVALUE                 
093100              MOVE NEJ                   TO 4306-FLANNULL                 
093200              MOVE +1                    TO 4306-KDPACLAS                 
093300              PERFORM IMS-ISRT-XXDJ-4306                                  
093400          END-IF                                                          
093500                                                                          
093600          PERFORM DA-SKAPA-4312                                           
093700          PERFORM IMS-ISRT-XXDK-4312                                      
093800                                                                          
093900          IF MID-FLAVVPACK (RADIND) = ALL '+'                             
094000             PERFORM DD-SKAPA-4316                                        
094100          ELSE                                                            
094200             PERFORM DB-SKAPA-4316-GEMEN                                  
094300             PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                        
094400          END-IF                                                          
094500                                                                          
094600          PERFORM DE-SKAPA-4316-004                                       
094700          PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                           
094800                                                                          
094900          PERFORM DF-KOLLA-4312                                           
095000        END-IF                                                            
095100        ADD +1 TO RADIND                                                  
095200     END-PERFORM                                                          
095300     .                                                                    
095400     EJECT                                                                
095500 DA-SKAPA-4312 SECTION.                                                   
095600                                                                          
095700     MOVE MID-IDPRODNR      (RADIND) TO 4312-IDPRODNR                     
095800     MOVE LOW-VALUE                  TO 4312-LOWVALUE                     
095900     MOVE MSG-SIGNON-USERID          TO 4312-IDUSER                       
096000     MOVE WSWDE6-IDDISTR  (RADIND)   TO 4312-IDDISTR                      
096100     MOVE WSWDE6-IDKUNDNR (RADIND)   TO 4312-IDKUNDNR                     
096200     MOVE WSWDE6-IDKUNDRF (RADIND)   TO 4312-IDKUNDRF                     
096300     MOVE WSWDE6-KDORDKL  (RADIND)   TO 4312-KDORDKL                      
096400     MOVE WSWDE6-KVORDRAD (RADIND)   TO 4312-KVORDRAD                     
096500     MOVE WSWDE6-KDFRAKT  (RADIND)   TO 4312-KDFRAKT                      
096600     ACCEPT 4312-TIDATUM FROM DATE                                        
096700     ACCEPT 4312-TIKLOCK FROM TIME                                        
096800     MOVE '4303'                     TO 4312-IDTRANS                      
096900     MOVE +1                         TO 4312-KDBEHAND-GRUND               
097000     MOVE +0                         TO 4312-KDBEHAND-RAD                 
097100     MOVE +0                         TO 4312-KDBEHAND-DEL                 
097200     MOVE +0                         TO 4312-KDBEHAND-URS                 
097300     MOVE +0                         TO 4312-KDBEHAND-KOL                 
097400                                                                          
097500     IF MID-FLAVVPACK(RADIND) = 'J' OR 'Y'                                
097600        MOVE +1                      TO 4312-KDBEHAND-AVVIK               
097700     ELSE                                                                 
097800        MOVE +4                      TO 4312-KDBEHAND-AVVIK               
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200 DB-SKAPA-4316-GEMEN SECTION.                                             
098300                                                                          
098400     MOVE ALL '+'                    TO 4316-WDGX4316                     
098500     MOVE MID-IDPRODNR(RADIND)       TO 4316-IDPRODNR                     
098600     MOVE '002'                      TO 4316-IDPTYP                       
098700     MOVE LOW-VALUE                  TO 4316-LOWVALUE                     
098800     MOVE ZERO                       TO 4316-KDTRSTAT                     
098900     MOVE +312                       TO 4316-LL                           
099000     MOVE LOW-VALUE                  TO 4316-Z1                           
099100     MOVE LOW-VALUE                  TO 4316-Z2                           
099200     MOVE 'W4T315'                   TO 4316-KDTRANS                      
099300     MOVE '4303'                     TO 4316-IDTRANS                      
099400     MOVE MFS-KDMFSFOR               TO 4316-KDMFSFOR                     
099500     MOVE WSWDE6-IDKOLLI-NUM (RADIND) TO 4316-IDKOLLI                     
099600                                                                          
099700     IF MFS-KDMFSFOR = 2                                                  
099800       MOVE +1                         TO 4316-MID-VKORDBTO-KOLLI         
099900       MOVE SPACE                      TO 4316-MID-KDKOLLI                
100000     END-IF                                                               
100100                                                                          
100200     MOVE 'U'                     TO 4316-MID-PRTVAL-ADRESSFL             
100300                                     4316-MID-PRTVAL-FOLJEFL              
100400                                                                          
100500     MOVE WSWDE6-IDKOLLI (RADIND)    TO 4316-MID-IDKOLLI-IN               
100600     MOVE WSWDE6-IDKOLLI (RADIND)    TO 4316-MID-IDKOLLI-UT               
100700                                                                          
100800     MOVE MID-IDPRODNR (RADIND)      TO 4316-MID-IDPRODNR-IN              
100900                                        4316-MID-IDPRODNR-UT              
101000                                                                          
101100     MOVE WSWDE6-IDKUNDRF(RADIND)    TO 4316-MID-IDORDNR-IN               
101200                                        4316-MID-IDORDNR-UT               
101300                                                                          
101400     MOVE ZERO                       TO 4316-MID-IDANSTNR-UT              
101500                                                                          
101600     MOVE MID-IDDISTR(RADIND)        TO 4316-MID-IDDISTR-IN               
101700                                        4316-MID-IDDISTR-UT               
101800                                                                          
101900     MOVE WSWDE6-IDKUNDNR(RADIND)     TO WS-IDKUNDNR                      
102000     MOVE WS-IDKUNDNR                 TO 4316-MID-IDKUNDNR-IN             
102100                                         4316-MID-IDKUNDNR-UT             
102200     MOVE WS-IDDC                     TO 4316-MID-IDDC-IN                 
102300                                         4316-MID-IDDC-UT                 
102400     .                                                                    
102500     EJECT                                                                
102600 DC-SKAPA-4316-NAESTA SECTION.                                            
102700                                                                          
102800        MOVE LOW-VALUE               TO IO-AREA-1                         
102900        MOVE ALL '+'                 TO 4316-WDGX4316                     
103000        MOVE MID-IDPRODNR   (RADIND) TO 4316-IDPRODNR                     
103100        MOVE '003'                   TO 4316-IDPTYP                       
103200        MOVE LOW-VALUE               TO 4316-LOWVALUE                     
103300        MOVE ZERO                    TO 4316-KDTRSTAT                     
103400        MOVE +277                    TO 4316-LL                           
103500        MOVE LOW-VALUE               TO 4316-Z1                           
103600        MOVE LOW-VALUE               TO 4316-Z2                           
103700        MOVE 'W4T314'                TO 4316-KDTRANS                      
103800        MOVE '4303'                  TO 4316-IDTRANS                      
103900        MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                     
104000                                                                          
104100        MOVE ZERO                    TO 4316-B-MID-IDANSTNR-UT            
104200                                                                          
104300        MOVE MID-IDDISTR    (RADIND) TO 4316-B-MID-IDDISTR-IN             
104400                                        4316-B-MID-IDDISTR-UT             
104500                                                                          
104600        MOVE 4312-IDKUNDNR           TO WS-IDKUNDNR                       
104700        MOVE WS-IDKUNDNR             TO 4316-B-MID-IDKUNDNR-IN            
104800                                        4316-B-MID-IDKUNDNR-UT            
104900        MOVE WS-IDDC                 TO 4316-B-MID-IDDC-IN                
105000                                        4316-B-MID-IDDC-UT                
105100                                                                          
105200        MOVE WSWDE6-IDKUNDRF (RADIND) TO 4316-B-MID-IDORDNR-IN            
105300                                         4316-B-MID-IDORDNR-UT            
105400                                                                          
105500     MOVE WSWDE6-IDKOLLI-NUM (RADIND) TO 4316-IDKOLLI                     
105600*************** FIX 911108 **********************                         
105700       IF MFS-KDMFSFOR = 2                                                
105800         MOVE +1                  TO 4316-MID-VKORDBTO-KOLLI              
105900         MOVE SPACE               TO 4316-MID-KDKOLLI                     
106000       END-IF                                                             
106100*************** FIX 911108 **********************                         
106200       MOVE WSWDE6-IDKOLLI (RADIND) TO 4316-B-MID-IDKOLLI-IN              
106300                                       4316-B-MID-IDKOLLI-UT              
106400                                                                          
106500        MOVE MID-IDPRODNR   (RADIND) TO 4316-B-MID-IDPRODNR-IN            
106600                                        4316-B-MID-IDPRODNR-UT            
106700        MOVE '4303'                  TO 4316-B-MID-IDTRANS-START          
106800        MOVE 'J'                     TO 4316-B-MID-FLFORTSK               
106900        MOVE 0                       TO 4316-B-MID-IDRADNR-FOM-S          
107000        MOVE 0                       TO 4316-B-MID-IDRADNR-TOM-S          
107100        MOVE 0                       TO 4316-B-MID-KVLEVART-S             
107200        MOVE 'UU'                 TO 4316-B-MID-KDPRTVAL-ADRESSFL         
107300                                     4316-B-MID-KDPRTVAL-FOLJEFL          
107400                                                                          
107500                                                                          
107600     EJECT                                                                
107700******* OM DET FINNS ANNULLATIONER PÅ ORDERN SKALL ORDERRADERNA           
107800******* SKAPAS I INTERVALL I 4316-SEGM. BEROENDE PÅ HUR MÅNGA             
107900******* ANNULLERADE RADER SOM FINNS (4308-SEGM).                          
108000     .                                                                    
108100 DD-SKAPA-4316 SECTION.                                                   
108200                                                                          
108300         PERFORM DB-SKAPA-4316-GEMEN                                      
108400         MOVE +1           TO 4316-IND                                    
108500         MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                             
108600                                                                          
108700         IF  4306-FLANNULL = NEJ                                          
108800                                                                          
108900           IF 4312-KVORDRAD > MAX-ANT-RAD                                 
109000             PERFORM DG-MAX-200-EJ-ANNULL                                 
109100           ELSE                                                           
109200             MOVE '0001' TO 4316-MID-IDRADNR-FOM (4316-IND)               
109300             MOVE 4312-KVORDRAD TO WS-TOM                                 
109400                                                                          
109500             IF WS-TOM NOT = 1                                            
109600                MOVE  WS-TOM  TO                                          
109700                               4316-MID-IDRADNR-TOM (4316-IND)            
109800             END-IF                                                       
109900                                                                          
110000             PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                        
110100           END-IF                                                         
110200                                                                          
110300         ELSE                                                             
110400             MOVE 1            TO WS-FROM                                 
110500             MOVE 4312-KVORDRAD TO WS-TOM                                 
110600             PERFORM IMS-GET-XXDJ-4308                                    
110700                                                                          
110800             PERFORM UNTIL SEGMENT-SAKNAS                                 
110900               IF  4308-KVANNANT = ZERO                                   
111000                 IF WS-FROM < 4308-IDRADNR-ORD-FROM                       
111100                     COMPUTE WS-TOM = 4308-IDRADNR-ORD-FROM - 1           
111200                     PERFORM DDA-KOLLA-IND                                
111300                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
111400                     MOVE 4312-KVORDRAD TO WS-TOM                         
111500                     PERFORM IMS-GET-XXDJ-4308                            
111600                     ADD +1           TO 4316-IND                         
111700                 ELSE                                                     
111800                     COMPUTE WS-FROM = 4308-IDRADNR-ORD-TOM + 1           
111900                     PERFORM IMS-GET-XXDJ-4308                            
112000                 END-IF                                                   
112100               ELSE                                                       
112200                   PERFORM IMS-GET-XXDJ-4308                              
112300               END-IF                                                     
112400             END-PERFORM                                                  
112500                                                                          
112600             IF  WS-FROM > 4312-KVORDRAD                                  
112700             CONTINUE                                                     
112800             ELSE                                                         
112900                 PERFORM DDA-KOLLA-IND                                    
113000             END-IF                                                       
113100                                                                          
113200             IF 4316-IDPTYP = '002'                                       
113300                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
113400                                                                          
113500                IF 4316-MID-RAD (MAX-4316-IND) NOT = ALL '+'              
113600                   PERFORM DC-SKAPA-4316-NAESTA                           
113700                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
113800                END-IF                                                    
113900                                                                          
114000             ELSE                                                         
114100                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
114200                                                                          
114300                IF 4316-B-MID-RAD (MAX-4316-IND) NOT = ALL '+'            
114400                   PERFORM DC-SKAPA-4316-NAESTA                           
114500                   PERFORM IMS-ISRT-XXDL-4316-STAT-II                     
114600                END-IF                                                    
114700                                                                          
114800             END-IF                                                       
114900         END-IF                                                           
115000                                                                          
115100     .                                                                    
115200     EJECT                                                                
115300 DDA-KOLLA-IND SECTION.                                                   
115400                                                                          
115500        IF  4316-IND > MAX-4316-IND OR WS-ANT-RAD-REST = 0                
115600                                                                          
115700            IF  4316-IDPTYP = '002'                                       
115800                PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                     
115900            ELSE                                                          
116000                PERFORM IMS-ISRT-XXDL-4316-STAT-II                        
116100            END-IF                                                        
116200                                                                          
116300            PERFORM DC-SKAPA-4316-NAESTA                                  
116400            MOVE +1       TO 4316-IND                                     
116500            MOVE MAX-ANT-RAD  TO WS-ANT-RAD-REST                          
116600                                                                          
116700        END-IF                                                            
116800                                                                          
116900        COMPUTE WS-ANT-RAD-INT = WS-TOM - WS-FROM + 1                     
117000                                                                          
117100        IF 4316-IDPTYP = '002'                                            
117200                                                                          
117300           IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                            
117400              PERFORM DH-MAX-200-RAD                                      
117500           ELSE                                                           
117600              MOVE WS-FROM      TO                                        
117700                      4316-MID-IDRADNR-FOM (4316-IND)                     
117800                                                                          
117900              IF WS-FROM NOT = WS-TOM                                     
118000                 MOVE WS-TOM    TO                                        
118100                         4316-MID-IDRADNR-TOM (4316-IND)                  
118200              END-IF                                                      
118300                                                                          
118400              COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                 
118500                                        WS-ANT-RAD-INT                    
118600           END-IF                                                         
118700                                                                          
118800        ELSE                                                              
118900                                                                          
119000           IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                            
119100              PERFORM DH-MAX-200-RAD                                      
119200           ELSE                                                           
119300              MOVE WS-FROM      TO                                        
119400                      4316-B-MID-IDRADNR-FOM (4316-IND)                   
119500                                                                          
119600              IF WS-FROM NOT = WS-TOM                                     
119700                 MOVE WS-TOM    TO                                        
119800                         4316-B-MID-IDRADNR-TOM (4316-IND)                
119900              END-IF                                                      
120000                                                                          
120100              COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                 
120200                                        WS-ANT-RAD-INT                    
120300           END-IF                                                         
120400                                                                          
120500        END-IF                                                            
120600                                                                          
120700                                                                          
120800     .                                                                    
120900     EJECT                                                                
121000 DE-SKAPA-4316-004 SECTION.                                               
121100                                                                          
121200     MOVE LOW-VALUE               TO IO-AREA-1                            
121300     MOVE ALL '+'                 TO 4316-WDGX4316                        
121400     MOVE MID-IDPRODNR(RADIND)    TO 4316-IDPRODNR                        
121500     MOVE '004'                   TO 4316-IDPTYP                          
121600     MOVE ZERO                    TO 4316-IDKOLLI                         
121700     MOVE ZERO                    TO 4316-KDTRSTAT                        
121800     MOVE +346                    TO 4316-LL                              
121900     MOVE LOW-VALUE               TO 4316-Z1                              
122000     MOVE LOW-VALUE               TO 4316-Z2                              
122100     MOVE LOW-VALUE               TO 4316-LOWVALUE                        
122200     MOVE 'W4T398X'               TO 4316-KDTRANS                         
122300     MOVE '4303'                  TO 4316-IDTRANS                         
122400     MOVE MFS-KDMFSFOR            TO 4316-KDMFSFOR                        
122500                                                                          
122600     MOVE WSWDE6-IDKUNDRF(RADIND) TO 4316-C-MID-IDORDNR-IN                
122700                                     4316-C-MID-IDORDNR-UT                
122800                                                                          
122900     MOVE MID-IDDISTR(RADIND)     TO 4316-C-MID-IDDISTR-IN                
123000                                     4316-C-MID-IDDISTR-UT                
123100                                                                          
123200     MOVE WSWDE6-IDKUNDNR (RADIND) TO WS-IDKUNDNR                         
123300     MOVE WS-IDKUNDNR             TO 4316-C-MID-IDKUNDNR-IN               
123400                                     4316-C-MID-IDKUNDNR-UT               
123500                                                                          
123600     MOVE MID-IDPRODNR(RADIND)    TO 4316-C-MID-IDPRODNR-IN               
123700                                     4316-C-MID-IDPRODNR-UT               
123800                                                                          
123900     MOVE ZERO                    TO 4316-C-MID-IDKOLLI-IN                
124000                                     4316-C-MID-IDKOLLI-UT                
124100                                                                          
124200     MOVE SPACE                   TO 4316-C-MID-FLSVAR                    
124300     .                                                                    
124400     EJECT                                                                
124500 DF-KOLLA-4312 SECTION.                                                   
124600                                                                          
124700        PERFORM IMS-GET-XXDK-4312-STAT-BLANK                              
124800                                                                          
124900        IF ((4312-KDBEHAND-AVVIK = 0 OR 2)                                
125000        AND (4312-KDBEHAND-RAD = 0 OR 2)                                  
125100        AND (4312-KDBEHAND-DEL = 0 OR 2)                                  
125200        AND (4312-KDBEHAND-URS = 0 OR 2)                                  
125300        AND (4312-KDBEHAND-KOL = 0 OR 2))                                 
125400            PERFORM IMS-DLET-XXDK-4312                                    
125500                                                                          
125600            PERFORM  IMS-GET-XXDJ-4306-STAT-BLANK                         
125700            MOVE +3 TO 4306-KDPACLAS                                      
125800            PERFORM IMS-REPL-XXDJ                                         
125900                                                                          
126000            MOVE '002' TO W-IDPTYP-4316                                   
126100                                                                          
126200            MOVE WSWDE6-IDKOLLI-NUM (RADIND) TO W-IDKOLLI-4316            
126300            PERFORM IMS-GET-XXDL-4316-STAT-BLANK                          
126400            MOVE +1 TO 4316-KDTRSTAT                                      
126500            PERFORM IMS-REPL-XXDL                                         
126600                                                                          
126700            MOVE '003' TO W-IDPTYP-4316                                   
126800            PERFORM IMS-GET-XXDL-4316-STAT-GE                             
126900                                                                          
127000            PERFORM UNTIL SEGMENT-SAKNAS                                  
127100               MOVE +1 TO 4316-KDTRSTAT                                   
127200               PERFORM IMS-REPL-XXDL                                      
127300               PERFORM IMS-GET-XXDL-4316-STAT-GE                          
127400            END-PERFORM                                                   
127500                                                                          
127600            MOVE '004' TO W-IDPTYP-4316                                   
127700            MOVE ZERO TO W-IDKOLLI-4316                                   
127800            PERFORM IMS-GET-XXDL-4316                                     
127900            MOVE +1 TO 4316-KDTRSTAT                                      
128000            PERFORM IMS-REPL-XXDL                                         
128100                                                                          
128200            MOVE MFS-KDMFSFOR TO PTOP-KDMFSFOR                            
128300            PERFORM IMS-INSERT-MSG-ALT-PCB                                
128400        ELSE                                                              
128500            MOVE +2 TO 4312-KDBEHAND-GRUND                                
128600            PERFORM IMS-REPL-XXDK                                         
128700        END-IF                                                            
128800     .                                                                    
128900     EJECT                                                                
129000 DG-MAX-200-EJ-ANNULL SECTION.                                            
129100                                                                          
129200***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
129300***** UPPLÄGGNING AV ORDER SOM EJ HAR ANNULLERADE RADER                   
129400***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
129500                                                                          
129600         MOVE 1      TO WS-FROM                                           
129700         MOVE MAX-ANT-RAD  TO WS-TOM                                      
129800         MOVE WS-FROM    TO                                               
129900                 4316-MID-IDRADNR-FOM (4316-IND)                          
130000         MOVE  WS-TOM  TO                                                 
130100                  4316-MID-IDRADNR-TOM (4316-IND)                         
130200         PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                            
130300         MOVE +1       TO 4316-IND                                        
130400                                                                          
130500         PERFORM UNTIL WS-TOM NOT < 4312-KVORDRAD                         
130600            PERFORM DC-SKAPA-4316-NAESTA                                  
130700            COMPUTE WS-FROM = WS-TOM + 1                                  
130800            COMPUTE WS-TOM = WS-FROM + MAX-ANT-RAD - 1                    
130900                                                                          
131000            IF WS-TOM > 4312-KVORDRAD                                     
131100               MOVE 4312-KVORDRAD TO WS-TOM                               
131200            END-IF                                                        
131300                                                                          
131400            MOVE  WS-FROM TO                                              
131500                     4316-B-MID-IDRADNR-FOM (4316-IND)                    
131600                                                                          
131700            IF WS-FROM NOT = WS-TOM                                       
131800               MOVE  WS-TOM TO                                            
131900                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
132000            END-IF                                                        
132100                                                                          
132200            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
132300            MOVE +1       TO 4316-IND                                     
132400         END-PERFORM                                                      
132500                                                                          
132600     .                                                                    
132700     EJECT                                                                
132800 DH-MAX-200-RAD SECTION.                                                  
132900                                                                          
133000***** ANTALET RADER I KOLLI-POSTEN FÅR EJ ÖVERSTIGA 200                   
133100***** UPPLÄGGNING AV ORDER SOM HAR ANNULLERADE RADER                      
133200***** MEN SOM INNEHÅLLER FLER ÄN 200 RADER                                
133300                                                                          
133400         MOVE WS-TOM       TO WS-TOM-SISTA                                
133500         COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1                   
133600                                                                          
133700         IF 4316-IDPTYP = '002'                                           
133800            MOVE WS-FROM    TO                                            
133900                    4316-MID-IDRADNR-FOM (4316-IND)                       
134000                                                                          
134100            IF WS-FROM NOT = WS-TOM                                       
134200               MOVE  WS-TOM  TO                                           
134300                        4316-MID-IDRADNR-TOM (4316-IND)                   
134400            END-IF                                                        
134500            PERFORM IMS-ISRT-XXDL-4316-STAT-BLANK                         
134600                                                                          
134700         ELSE                                                             
134800            MOVE WS-FROM    TO                                            
134900                    4316-B-MID-IDRADNR-FOM (4316-IND)                     
135000                                                                          
135100            IF WS-FROM NOT = WS-TOM                                       
135200               MOVE  WS-TOM  TO                                           
135300                        4316-B-MID-IDRADNR-TOM (4316-IND)                 
135400            END-IF                                                        
135500                                                                          
135600            PERFORM IMS-ISRT-XXDL-4316-STAT-II                            
135700         END-IF                                                           
135800                                                                          
135900         MOVE MAX-ANT-RAD          TO WS-ANT-RAD-REST                     
136000         MOVE +1                   TO 4316-IND                            
136100                                                                          
136200*****  SKAPA KOLLI-POSTER TILLS ALLA RADER I INTERVALLET                  
136300*****  ÄR PLACERADE I PT = 2 ELLER 3 HÄR SKAPAS EV FLERA PT=3             
136400                                                                          
136500         PERFORM UNTIL WS-TOM NOT < WS-TOM-SISTA                          
136600            PERFORM DC-SKAPA-4316-NAESTA                                  
136700            COMPUTE WS-FROM = WS-TOM + 1                                  
136800            COMPUTE WS-ANT-RAD-INT = WS-TOM-SISTA - WS-FROM + 1           
136900                                                                          
137000            IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                           
137100               COMPUTE WS-TOM = WS-FROM + WS-ANT-RAD-REST - 1             
137200               MOVE WS-FROM TO                                            
137300                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
137400                                                                          
137500               IF WS-FROM NOT = WS-TOM                                    
137600                  MOVE  WS-TOM  TO                                        
137700                           4316-B-MID-IDRADNR-TOM (4316-IND)              
137800               END-IF                                                     
137900                                                                          
138000               PERFORM IMS-ISRT-XXDL-4316-STAT-II                         
138100               MOVE MAX-ANT-RAD     TO WS-ANT-RAD-REST                    
138200               MOVE +1           TO 4316-IND                              
138300            ELSE                                                          
138400               MOVE WS-TOM-SISTA   TO WS-TOM                              
138500               MOVE WS-FROM TO                                            
138600                       4316-B-MID-IDRADNR-FOM (4316-IND)                  
138700                                                                          
138800               IF WS-FROM NOT = WS-TOM                                    
138900                  MOVE  WS-TOM  TO                                        
139000                           4316-B-MID-IDRADNR-TOM (4316-IND)              
139100               END-IF                                                     
139200                                                                          
139300               COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                
139400                                         WS-ANT-RAD-INT                   
139500            END-IF                                                        
139600                                                                          
139700         END-PERFORM                                                      
139800                                                                          
139900     .                                                                    
140000     EJECT                                                                
140100 S02-ROER-EJ-FAELT SECTION.                                               
140200                                                                          
140300     MOVE +1 TO RADIND                                                    
140400                                                                          
140500     PERFORM UNTIL RADIND NOT < MAX-RADIND-PLUS-1                         
140600        IF  MID-IDDISTR (RADIND) NOT = ALL '+' OR SPACE                   
140700            MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR(RADIND)                 
140800        END-IF                                                            
140900                                                                          
141000        IF  MID-IDPRODNR (RADIND) NOT = ALL '+' OR SPACE                  
141100            MOVE MFS-ROER-EJ-FAELT TO  MOD-IDPRODNR(RADIND)               
141200        END-IF                                                            
141300                                                                          
141400        IF  MID-FLAVVPACK (RADIND) NOT = ALL '+' OR SPACE                 
141500            MOVE MFS-ROER-EJ-FAELT TO  MOD-FLAVVPACK(RADIND)              
141600        END-IF                                                            
141700                                                                          
141800        ADD +1 TO RADIND                                                  
141900     END-PERFORM                                                          
142000                                                                          
142100     .                                                                    
142200     EJECT                                                                
142300 S99-NAESTA-TRANS SECTION.                                                
142400                                                                          
142500        MOVE WS-IDDC              TO W-IDDC-4311                          
142600        PERFORM IMS-GET-XXDK-4311                                         
142700*** KVAL MED USERID                                                       
142800        MOVE MSG-SIGNON-USERID TO W-WDGXKEY-IDUSER-4312                   
142900        PERFORM IMS-GET-XXDK-4312-STAT-GE-F                               
143000                                                                          
143100        PERFORM UNTIL NOT (((SEGMENT-FINNS)                               
143200             AND (4312-KDBEHAND-AVVIK = +3 OR                             
143300                  4312-KDBEHAND-RAD   = +3 OR                             
143400                  4312-KDBEHAND-DEL   = +3 OR                             
143500                  4312-KDBEHAND-URS   = +3 OR                             
143600                  4312-KDBEHAND-KOL   = +3))                              
143700        OR ((SEGMENT-FINNS) AND                                           
143800        (4312-IDTRANS NOT = '4303')))                                     
143900            PERFORM IMS-GET-XXDK-4312-STAT-GE                             
144000        END-PERFORM                                                       
144100                                                                          
144200        IF  SEGMENT-FINNS                                                 
144300            IF 4312-KDBEHAND-AVVIK = 1                                    
144400               MOVE 'W4O39101'         TO MFS-IDMOD                       
144500               MOVE M4391-MOD-LAENGD   TO MSG-KVLL                        
144600               MOVE '4391'             TO M4391-MOD-IDTRANS               
144700               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
144800               MOVE WS-IDPRODNR        TO M4391-MOD-IDPRODNR-UT           
144900               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
145000               MOVE WS-IDDISTR         TO M4391-MOD-IDDISTR-UT            
145100               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
145200               MOVE WS-IDKUNDNR        TO M4391-MOD-IDKUNDNR-UT           
145300               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
145400               MOVE WS-KDFRAKT         TO M4391-MOD-KDFRAKT-UT            
145500               MOVE 4312-IDKUNDRF      TO M4391-MOD-IDORDNR-UT            
145600               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
145700               MOVE WS-KDORDKL         TO M4391-MOD-KDORDKL-UT            
145800               MOVE WS-IDDC            TO M4391-MOD-IDDC-UT               
145900               MOVE 'UU'             TO M4391-MOD-PRTVAL-ADRESSFL         
146000                                                                          
146100               INSPECT M4391-MOD-IDPRODNR-UT REPLACING                    
146200                                       LEADING ZERO BY SPACE              
146300               INSPECT M4391-MOD-IDDISTR-UT REPLACING                     
146400                                       LEADING ZERO BY SPACE              
146500               INSPECT M4391-MOD-IDKUNDNR-UT REPLACING                    
146600                                       LEADING ZERO BY SPACE              
146700               INSPECT M4391-MOD-KDFRAKT-UT REPLACING                     
146800                                       LEADING ZERO BY SPACE              
146900               INSPECT M4391-MOD-IDORDNR-UT REPLACING                     
147000                                       LEADING ZERO BY SPACE              
147100               MOVE WS-VISA-NAESTA-BILD TO WS-MSG-CALL                    
147200            ELSE                                                          
147300            IF 4312-KDBEHAND-AVVIK = 4                                    
147400               MOVE 4312-IDPRODNR      TO WS-IDPRODNR                     
147500               MOVE WS-IDPRODNR        TO PTOP1-IDPRODNR-IN               
147600                                          PTOP1-IDPRODNR-UT               
147700               MOVE 4312-IDDISTR       TO WS-IDDISTR                      
147800               MOVE WS-IDDISTR         TO PTOP1-IDDISTR-UT                
147900               MOVE 4312-IDKUNDNR      TO WS-IDKUNDNR                     
148000               MOVE WS-IDKUNDNR        TO PTOP1-IDKUNDNR-UT               
148100               MOVE 4312-KDFRAKT       TO WS-KDFRAKT                      
148200               MOVE WS-KDFRAKT         TO PTOP1-KDFRAKT-UT                
148300               MOVE 4312-IDKUNDRF      TO PTOP1-IDORDNR-UT                
148400               MOVE 4312-KDORDKL       TO WS-KDORDKL                      
148500               MOVE WS-KDORDKL         TO PTOP1-KDORDKL-UT                
148600               MOVE WS-IDDC            TO PTOP1-IDDC-UT                   
148700               MOVE 'UU'               TO PTOP1-KDPRTVAL-ADRESSFL         
148800               MOVE MFS-KDMFSFOR       TO PTOP1-KDMFSFOR                  
148900               MOVE WS-STARTA-4391     TO WS-MSG-CALL                     
149000            ELSE                                                          
149100               MOVE INF-2(INDX) TO MOD-TEMFSINF                           
149200            END-IF                                                        
149300            END-IF                                                        
149400        ELSE                                                              
149500            IF SW-RAD-FINNS = JA                                          
149600                MOVE INF-6(INDX) TO MOD-TEMFSINF                          
149700            END-IF                                                        
149800                                                                          
149900            MOVE MAX-MOD-LAENGD TO MSG-KVLL                               
150000        END-IF                                                            
150100     .                                                                    
150200     EJECT                                                                
150300* IMS SEKTIONER                                                           
150400     SKIP3                                                                
150500 IMS-GET-MSG SECTION.                                                     
150600     MOVE '  QC' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
150800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150900     PERFORM IMS-STATUSKONTROLL                                           
151000                                                                          
151100     .                                                                    
151200 IMS-INSERT-MSG SECTION.                                                  
151300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
151400       MOVE '0' TO MFS-KDHUVOMR                                           
151500     END-IF                                                               
151600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151700     MOVE SPACE TO GODK-STATUSKODER                                       
151800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
151900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152000     PERFORM IMS-STATUSKONTROLL                                           
152100                                                                          
152200     .                                                                    
152300 IMS-INSERT-MSG-ALT-PCB SECTION.                                          
152400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152500     MOVE SPACE TO GODK-STATUSKODER                                       
152600     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
152700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-INSERT-MSG-ALT1-PCB SECTION.                                         
153200     MOVE SPACE TO GODK-STATUSKODER                                       
153300     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
153400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
153500     PERFORM IMS-STATUSKONTROLL                                           
153600     .                                                                    
153700     EJECT                                                                
153800 IMS-GET-XXDJ-4305 SECTION.                                               
153900     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
154000            DELIMITED BY SIZE INTO SSA1                                   
154100     MOVE '  GE'   TO GODK-STATUSKODER                                    
154200     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA-2 SSA1                    
154300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500                                                                          
154600     .                                                                    
154700 IMS-GET-XXDJ-4306-STAT-GE SECTION.                                       
154800     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
154900            DELIMITED BY SIZE INTO SSA1                                   
155000     MOVE '  GE' TO GODK-STATUSKODER                                      
155100     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
155200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400                                                                          
155500     .                                                                    
155600 IMS-GET-XXDJ-4306-STAT-BLANK SECTION.                                    
155700     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
155800            DELIMITED BY SIZE INTO SSA1                                   
155900     MOVE '  '   TO GODK-STATUSKODER                                      
156000     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1                  
156100     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300                                                                          
156400     .                                                                    
156500 IMS-ISRT-XXDJ-4306 SECTION.                                              
156600     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
156700            DELIMITED BY SIZE INTO SSA1                                   
156800     MOVE 'WLXXDJ11 '   TO SSA2                                           
156900     MOVE '  '   TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2             
157100     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300                                                                          
157400     .                                                                    
157500 IMS-GET-XXDJ-4308 SECTION.                                               
157600     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
157700            DELIMITED BY SIZE INTO SSA1                                   
157800     MOVE 'WLXXDJ21 '   TO SSA2                                           
157900     MOVE '  GE'    TO GODK-STATUSKODER                                   
158000     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-2 SSA1 SSA2             
158100     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSKONTROLL                                           
158300                                                                          
158400     .                                                                    
158500 IMS-REPL-XXDJ SECTION.                                                   
158600     MOVE '  '   TO GODK-STATUSKODER                                      
158700     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA-2                       
158800     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
158900     PERFORM IMS-STATUSKONTROLL                                           
159000     .                                                                    
159100     EJECT                                                                
159200 IMS-GET-XXDK-4311 SECTION.                                               
159300     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
159400            DELIMITED BY SIZE INTO SSA1                                   
159500     MOVE '  '     TO GODK-STATUSKODER                                    
159600     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA-3 SSA1                    
159700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
159800     PERFORM IMS-STATUSKONTROLL                                           
159900                                                                          
160000     .                                                                    
160100 IMS-GET-XXDK-4312-STAT-BLANK SECTION.                                    
160200     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
160300            DELIMITED BY SIZE INTO SSA1                                   
160400     MOVE '  '     TO GODK-STATUSKODER                                    
160500     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA-3 SSA1                  
160600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
160700     PERFORM IMS-STATUSKONTROLL                                           
160800                                                                          
160900     .                                                                    
161000 IMS-GET-XXDK-4312-STAT-GE SECTION.                                       
161100     STRING 'WLXXDK11(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'               
161200            DELIMITED BY SIZE INTO SSA1                                   
161300     MOVE '  GE'   TO GODK-STATUSKODER                                    
161400     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
161500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
161600     PERFORM IMS-STATUSKONTROLL                                           
161700                                                                          
161800     .                                                                    
161900 IMS-GET-XXDK-4312-STAT-GE-F SECTION.                                     
162000     STRING 'WLXXDK11*F(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'             
162100            DELIMITED BY SIZE INTO SSA1                                   
162200     MOVE '  GE'   TO GODK-STATUSKODER                                    
162300     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA-3 SSA1                   
162400     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
162500     PERFORM IMS-STATUSKONTROLL                                           
162600                                                                          
162700     .                                                                    
162800 IMS-ISRT-XXDK-4312 SECTION.                                              
162900     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
163000            DELIMITED BY SIZE INTO SSA1                                   
163100     MOVE 'WLXXDK11 '   TO SSA2                                           
163200     MOVE '  '   TO GODK-STATUSKODER                                      
163300     CALL CBLTDLI USING ISRT XXDK-PCB DLI-IO-AREA-3 SSA1 SSA2             
163400     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
163500     PERFORM IMS-STATUSKONTROLL                                           
163600                                                                          
163700     .                                                                    
163800 IMS-DLET-XXDK-4312 SECTION.                                              
163900     MOVE '  '   TO GODK-STATUSKODER                                      
164000     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA-3                       
164100     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
164200     PERFORM IMS-STATUSKONTROLL                                           
164300                                                                          
164400     .                                                                    
164500 IMS-REPL-XXDK SECTION.                                                   
164600     MOVE '  '   TO GODK-STATUSKODER                                      
164700     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA-3                       
164800     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
164900     PERFORM IMS-STATUSKONTROLL                                           
165000     .                                                                    
165100     EJECT                                                                
165200 IMS-GET-XXDL-4315 SECTION.                                               
165300     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
165400            DELIMITED BY SIZE INTO SSA1                                   
165500     MOVE '  '     TO GODK-STATUSKODER                                    
165600     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA-1 SSA1                    
165700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
165800     PERFORM IMS-STATUSKONTROLL                                           
165900                                                                          
166000     .                                                                    
166100 IMS-GET-XXDL-4316-STAT-BLANK SECTION.                                    
166200     STRING 'WLXXDL11*F(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
166300            DELIMITED BY SIZE INTO SSA1                                   
166400     MOVE '  '     TO GODK-STATUSKODER                                    
166500     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
166600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
166700     PERFORM IMS-STATUSKONTROLL                                           
166800                                                                          
166900     .                                                                    
167000 IMS-GET-XXDL-4316-STAT-GE SECTION.                                       
167100     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
167200            DELIMITED BY SIZE INTO SSA1                                   
167300     MOVE '  GE'   TO GODK-STATUSKODER                                    
167400     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
167500     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
167600     PERFORM IMS-STATUSKONTROLL                                           
167700                                                                          
167800     .                                                                    
167900 IMS-GET-XXDL-4316 SECTION.                                               
168000     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
168100            DELIMITED BY SIZE INTO SSA1                                   
168200     MOVE '  '     TO GODK-STATUSKODER                                    
168300     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
168400     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
168500     PERFORM IMS-STATUSKONTROLL                                           
168600                                                                          
168700     .                                                                    
168800 IMS-ISRT-XXDL-4316-STAT-BLANK SECTION.                                   
168900     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
169000            DELIMITED BY SIZE INTO SSA1                                   
169100     MOVE 'WLXXDL11 '   TO SSA2                                           
169200     MOVE '  '   TO GODK-STATUSKODER                                      
169300     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
169400     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
169500     PERFORM IMS-STATUSKONTROLL                                           
169600                                                                          
169700     .                                                                    
169800 IMS-ISRT-XXDL-4316-STAT-II SECTION.                                      
169900     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
170000            DELIMITED BY SIZE INTO SSA1                                   
170100     MOVE 'WLXXDL11 '   TO SSA2                                           
170200     MOVE '  II'   TO GODK-STATUSKODER                                    
170300     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA-1 SSA1 SSA2             
170400     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
170500     PERFORM IMS-STATUSKONTROLL                                           
170600                                                                          
170700     .                                                                    
170800 IMS-REPL-XXDL SECTION.                                                   
170900     MOVE '  '   TO GODK-STATUSKODER                                      
171000     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA-1                       
171100     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400     EJECT                                                                
171500 IMS-KET-KORD SECTION.                                                    
171600                                                                          
171700     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
171800            DELIMITED BY SIZE INTO SSA1                                   
171900     MOVE '  GE' TO GODK-STATUSKODER                                      
172000     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA-4 SSA1                    
172100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     EJECT                                                                
172500 IMS-GNP-ORAD SECTION.                                                    
172600                                                                          
172700     MOVE 'WDE411   '  TO SSA1                                            
172800     MOVE '  GE' TO GODK-STATUSKODER                                      
172900     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA-4 SSA1                   
173000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
173100     PERFORM IMS-STATUSKONTROLL                                           
173200     .                                                                    
173300     EJECT                                                                
173400 IMS-GU-WDE6-WDE601 SECTION.                                              
173500     STRING 'WDE601  (IDPRODNR =' W-WDE6-X ')'                            
173600            DELIMITED BY SIZE INTO SSA1                                   
173700     MOVE '  GE' TO GODK-STATUSKODER                                      
173800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-1 SSA1                    
173900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
174000     PERFORM IMS-STATUSKONTROLL                                           
174100                                                                          
174200     .                                                                    
174300 IMS-GET-WDE4E SECTION.                                                   
174400     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
174500                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
174600            DELIMITED BY SIZE INTO SSA1                                   
174700     MOVE '  GE' TO GODK-STATUSKODER                                      
174800     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-AREA-1 SSA1                   
174900     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
175000     PERFORM IMS-STATUSKONTROLL                                           
175100     .                                                                    
175200     EJECT                                                                
175300 IMS-GU-WDQ3D SECTION.                                                    
175400     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
175500            DELIMITED BY SIZE INTO SSA1                                   
175600     MOVE '    ' TO GODK-STATUSKODER                                      
175700     CALL CBLTDLI USING GU ORQA-PCB ODEL-WDQ301 SSA1                      
175800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
175900     PERFORM IMS-STATUSKONTROLL                                           
176000                                                                          
176100     .                                                                    
176200 IMS-GN-WDQ3D SECTION.                                                    
176300     STRING 'WLORQA01(WDQ3DSEQ >' W-WDQ3D-X ')'                           
176400            DELIMITED BY SIZE INTO SSA1                                   
176500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
176600     CALL CBLTDLI USING GN ORQA-PCB ODEL-WDQ301 SSA1                      
176700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
176800     PERFORM IMS-STATUSKONTROLL                                           
176900                                                                          
177000     .                                                                    
177100 IMS-STATUSKONTROLL SECTION.                                              
177200     SET STATUS-IX TO 1                                                   
177300     SEARCH GODK-STATUS AT END CALL FELLOG                                
177400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
177500     END-SEARCH                                                           
177600     CONTINUE                                                             
177700     .                                                                    
