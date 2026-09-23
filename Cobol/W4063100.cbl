000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4063100.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/08/20.                                                
000510                                                                          
000600*    FUNCTION:                                                            
000700*        TO STEER WHAT DOCUMENT TO BE PRINTED FOR A SPECIFIC              
000800*        IDSHIPM AND FROM WHICH PRINTER. IT CREATES 1 BUNDLE OF           
000900*        PAPER DOCUMENTS PER DISTRICT TO EASE UP THE PROCEDURE            
001000*        FOR THE CUSTOMER. ADD-IT (VIA WZ01) OR W40622 STARTS THIS        
001100*        PROGRAM. IT CALLS DIFFERENT SUBPROGRAM FOR PRINTING.             
001200*        W476KLIS   CARGO SPEC                                            
001300*        W476VERS   CARGO VALUE SPEC                                      
001400*        W476SPED   SHIPPING SPEC                                         
001500*        W476GMTL   CARGO INF NORWAY/GERMANY                              
001600*        W476STAT   CUSTOM INFORMATION                                    
001700*        W476CUST   CUSTOM INFORMATION FOR DUBAI                          
001800*        W476PACK   PACKING LIST                                          
001900*        W476NAPR   AMERICAN PROFORMA                                     
002000*        W476KULB   KUL-BILAGA                                            
002100*        W476BLAD   BILL OF LADING   INTE UT HÄR (050527 SM)              
002200*        W476SASO   SASO LISTA TILL SAUDI-ARABIEN                         
002300*        W476MANF   MANUFACTORING-LIST                                    
002400*        W476TRPT   TRANSPORTLISTA/LOADING REPORT                         
002500*                                                                         
002600*        THE PROGRAM READS/WRITE  WDE1                                    
002700*        THE PROGRAM READS        WDB2                                    
002800*        THE PROGRAM READS        WDB3                                    
002900*        THE PROGRAM READS        WDB9                                    
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSACTION: W4T631                                              
003300*        MID:         W4I63101                                            
003400*                                                                         
003500*    OUTDATA.                                                             
003600*        MOD:         W4O63101                                            
003700*                                                                         
003800* CHANGE: APRIL 2004 BY LINDA NILSSON                                     
003900* IN ADDITION SENDS DOCUMENTS TO ONDEMAND                                 
004000*        20-2-07  SASO-LISTAN TILL SAUDI-ARABIEN BORTTAGEN                
004100*                                                                         
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 DATA DIVISION.                                                           
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800 77  IDPGM                       PIC X(08)   VALUE 'W4063100'.            
004900                                                                          
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  JA                          PIC X       VALUE 'Y'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300 77  WS-IDDC-11                  PIC X(2)    VALUE '11'.                  
005400                                                                          
005500 77  WS-SEC                      PIC X(10)   VALUE SPACE.                 
005600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005700                                                                          
005800                                                                          
005900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006000     88  KEYS-OK                             VALUE 'J'.                   
006100     88  KEYS-WRONG                          VALUE 'N'.                   
006200                                                                          
006300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500                                                                          
006600 77  SEARCH-IDPRTLST-SW          PIC X       VALUE 'N'.                   
006700     88  SEARCH-IDPRTLST                     VALUE 'J'.                   
006800                                                                          
006900 77  IDPRTLST-OPEN-SW            PIC X       VALUE 'N'.                   
007000     88  IDPRTLST-OPEN                       VALUE 'J'.                   
007100                                                                          
007200 77  READ-IDPRLST-SW             PIC X       VALUE 'N'.                   
007300     88  READ-IDPRLST                        VALUE 'J'.                   
007400                                                                          
007500 77  FLWEBDC-SW                  PIC X       VALUE 'N'.                   
007600     88  FLWEBDC                             VALUE 'J'.                   
007700                                                                          
007800 77  WEB-REPORT-SW               PIC X       VALUE 'N'.                   
007900     88  WEB-REPORT                          VALUE 'J'.                   
008000                                                                          
008100 77  SKRIV-DOK-SW                PIC X       VALUE 'N'.                   
008200     88  SKRIV-OK                            VALUE 'J'.                   
008300                                                                          
008400 77  SKRIV-DOK-D-SW              PIC X       VALUE 'N'.                   
008500     88  SKRIV-DOK-CUST                      VALUE 'J'.                   
008600                                                                          
008700 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
008800                                                                          
008900 77  W-IDDC                      PIC X(02)   VALUE SPACE.                 
009000 77  W-IDDISTR                   PIC S9(05)  VALUE ZERO COMP-3.           
009100 77  W-IDKUNDNR                  PIC S9(07)  VALUE ZERO COMP-3.           
009200 77  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
009300 77  W-IDPRTLST                  PIC X(8)    VALUE SPACE.                 
009400 77  W-IDDC-REC                  PIC X(2)    VALUE SPACE.                 
009500 01  WS-HEADER.                                                           
009600     03  WS-IDDC-HDR             PIC X(2)    VALUE SPACE.                 
009700     03  WS-IDDISTR-HDR          PIC 9(4)    VALUE ZERO.                  
009800                                                                          
009900 01  WS-IDLIST.                                                           
010000     03  WS-IDLIST-IDDISTR       PIC 9(4)    VALUE ZERO.                  
010100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
010200     03  WS-IDLIST-IDSHIPM       PIC 9(5)    VALUE ZERO.                  
010300                                                                          
010400 77  W-IDDC-PREV                 PIC X(02)   VALUE SPACE.                 
010500 77  W-IDDISTR-PREV              PIC S9(05)  VALUE ZERO COMP-3.           
010600 77  W-IDKUNDNR-PREV             PIC S9(07)  VALUE ZERO COMP-3.           
010700 77  W-IDLEVNR-PREV              PIC X(5)    VALUE SPACE.                 
010800                                                                          
010900 01  W-IDDISTR-DISP              PIC 9(4).                                
011000 01  W-IDKUND-DISP.                                                       
011100     03 W-IDKUNDNR-DISP          PIC 9(6).                                
011200                                                                          
011300 77  WS-COPY                     PIC S9(4)  VALUE ZERO  COMP SYNC.        
011400 77  INDX                        PIC S9(4)  VALUE ZERO  COMP SYNC.        
011500 77  WS-INDX                     PIC S9(4)  VALUE ZERO  COMP SYNC.        
011600 77  INDX-MAX                    PIC S9(4)  VALUE ZERO  COMP SYNC.        
011700 77  INDX-MAX2                   PIC S9(4)  VALUE +10   COMP SYNC.        
011800 01  CURR-SECTION                PIC X(32)  VALUE SPACE.                  
011900                                                                          
012000 01  FILLER                   PIC X(16)  VALUE '--WDB9-TAB------'.        
012100 01  TAB.                                                                 
012200     03 TAB-IDKUND               PIC X(11).                               
012300     03 TAB-ROW  OCCURS 12.                                               
012400         05 TAB-IDPRTLST         PIC X(8).                                
012500         05 TAB-KVCOPIES-GMTL    PIC 9(1).                                
012600         05 TAB-KVCOPIES-KLIS    PIC 9(1).                                
012700         05 TAB-KVCOPIES-PACK    PIC 9(1).                                
012800         05 TAB-KVCOPIES-SPED    PIC 9(1).                                
012900         05 TAB-KVCOPIES-STAT    PIC 9(1).                                
013000         05 TAB-KVCOPIES-VERS    PIC 9(1).                                
013100         05 TAB-KVCOPIES-NAPR    PIC 9(1).                                
013200         05 TAB-KVCOPIES-KULB    PIC 9(1).                                
013300         05 TAB-KVCOPIES-BLAD    PIC 9(1).                                
013400         05 TAB-KVCOPIES-TRPT    PIC 9(1).                                
013500         05 TAB-KVCOPIES-MANF    PIC 9(1).                                
013600         05 TAB-KVCOPIES-SASO    PIC 9(1).                                
013700         05 TAB-IDDC-REC         PIC X(2).                                
013800                                                                          
013900*      CONSTANTER                                                         
014000*01  -COPY WWDCKONS                                                       
014100*                                                                         
014200 01  TEST-IDDISTR                PIC  9(05)  VALUE ZERO COMP-3.           
014300*01  FILLER  -COPY WWDIST03 -RED   TEST-IDDISTR.                          
014400*01  FILLER  -COPY WWDIST10 -RED   TEST-IDDISTR.                          
014500*01  FILLER  -COPY WWDIST18 -RED   TEST-IDDISTR.                          
014510*01  FILLER  -COPY WWDIST20 -RED   TEST-IDDISTR.                          
014600*01  FILLER  -COPY WWDIST35 -RED   TEST-IDDISTR.                          
014700*01  FILLER  -COPY WWDIST73 -RED   TEST-IDDISTR.                          
014800*01  FILLER  -COPY WWDIST74 -RED   TEST-IDDISTR.                          
014900*01  FILLER  -COPY WWDIS134 -RED   TEST-IDDISTR.                          
015000                                                                          
015100                                                                          
015200     EJECT                                                                
015300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015400 01  GENERAL-SUBPROGRAMS.                                                 
015500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
015900     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
016000     03  W476KLIS                PIC X(8)    VALUE 'W476KLIS'.            
016100     03  W476VERS                PIC X(8)    VALUE 'W476VERS'.            
016200     03  W476SPED                PIC X(8)    VALUE 'W476SPED'.            
016300     03  W476GMTL                PIC X(8)    VALUE 'W476GMTL'.            
016400     03  W476STAT                PIC X(8)    VALUE 'W476STAT'.            
016500     03  W476CUST                PIC X(8)    VALUE 'W476CUST'.            
016600     03  W476PACK                PIC X(8)    VALUE 'W476PACK'.            
016700     03  W476NAPR                PIC X(8)    VALUE 'W476NAPR'.            
016800     03  W476KULB                PIC X(8)    VALUE 'W476KULB'.            
016900     03  W476BLAD                PIC X(8)    VALUE 'W476BLAD'.            
017000     03  W476SASO                PIC X(8)    VALUE 'W476SASO'.            
017100     03  W476MANF                PIC X(8)    VALUE 'W476MANF'.            
017200     03  W476TRPT                PIC X(8)    VALUE 'W476TRPT'.            
017300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
017400                                                                          
017500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
017600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017900     SKIP2                                                                
018000 01  ERRTEXT.                                                             
018100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
018200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
018300 77  KDRC-DISPLAY                PIC Z(5).                                
018400     EJECT                                                                
018500     EJECT                                                                
018600* VARIABLES TO SUBPROGRAM W006PRS1                                        
018700     EJECT                                                                
018800*01  -COPY W006PRAR                                                       
018900     SKIP2                                                                
019000 01  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
019100     SKIP2                                                                
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
019500 01  SEND-AREA.                                                           
019600*    03  -COPY WZ01SEND                                                   
019700                                                                          
019800 01  FILLER                      PIC X(16)   VALUE 'WZ01RECV'.            
019900*01  -COPY WZ01RECV                                                       
020000                                                                          
020100 01  HDR-AREA.                                                            
020200*    03  -COPY WZ01REQU                                                   
020300*    03  -COPY WZ04HDR                                                    
020400                                                                          
020500* VARIABLES TO SUBPROGRAM W476GMTL, W476KLIS, W476PACK                    
020600*                         W476SPED, W476STAT, W476CUST, W476VERS          
020700*                         W476NAPR, W476KULB, W476BLAD, W476TRPT          
020800*                         W476SASO, W476MANF                              
020900     EJECT                                                                
021000*01  -COPY W476TRPD                                                       
021100     SKIP2                                                                
021200*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
021300*                                                                         
021400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021500     SKIP3                                                                
021600*01  MID -COPY W4I63101                                                   
021700     EJECT                                                                
021800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  KEYS-TO-DLI.                                                         
022300     03  W-IDSHIPM-X.                                                     
022400         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
022500                                                                          
022600     03  W-WDE111KY-X.                                                    
022700         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
022800         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
022900                                                                          
023000     03  W-WDE111KY-N-X.                                                  
023100         05  W-WDE111-IDDISTR-N  PIC S9(05)  VALUE ZERO COMP-3.           
023200         05  FILLER              PIC X(04)   VALUE HIGH-VALUES.           
023300                                                                          
023400     03  W-IDGMT-X.                                                       
023500         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
023600         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
023700                                                                          
023800     03  W-WDB301KY-X.                                                    
023900         05  W-WDB301-IDDC       PIC X(02)   VALUE SPACE.                 
024000         05  W-WDB301-IDGMT.                                              
024100             07  W-WDB301-IDDISTR                                         
024200                                 PIC S9(05)  VALUE ZERO COMP-3.           
024300             07  W-WDB301-IDKUNDNR                                        
024400                                 PIC S9(07)  VALUE ZERO COMP-3.           
024500                                                                          
024600     03  W-WDB301KY-MIN.                                                  
024700         05  W-WDB301-IDDC-MIN   PIC X(02)   VALUE SPACE.                 
024800         05  W-WDB301-IDDISTR-MIN                                         
024900                                 PIC S9(05)  VALUE ZERO COMP-3.           
025000         05  FILLER              PIC X(04)   VALUE LOW-VALUES.            
025100                                                                          
025200     03  W-WDB301KY-MAX.                                                  
025300         05  W-WDB301-IDDC-MAX   PIC X(02)   VALUE SPACE.                 
025400         05  W-WDB301-IDDISTR-MAX                                         
025500                                 PIC S9(05)  VALUE ZERO COMP-3.           
025600         05  FILLER              PIC X(04)   VALUE HIGH-VALUES.           
025700                                                                          
025800     03  W-IDDC-B6-X.                                                     
025900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
026000                                                                          
026100     03  W-WDB901KY-MIN.                                                  
026200         05  W-WDB901-IDDC-MIN   PIC X(2)    VALUE LOW-VALUE.             
026300         05  W-WDB901-IDDISTR-MIN                                         
026400                                 PIC S9(5)   VALUE ZERO  COMP-3.          
026500         05  W-WDB901-IDKUND-MIN PIC X(10)   VALUE SPACE.                 
026600         05  W-IDDC-REC-MIN      PIC X(2)    VALUE LOW-VALUE.             
026700                                                                          
026800     03  W-WDB901KY-MAX.                                                  
026900         05  W-WDB901-IDDC-MAX   PIC X(2)    VALUE HIGH-VALUE.            
027000         05  W-WDB901-IDDISTR-MAX                                         
027100                                 PIC S9(5)   VALUE +99999 COMP-3.         
027200         05  W-WDB901-IDKUND-MAX PIC X(10)   VALUE SPACE.                 
027300         05  W-IDDC-REC-MAX      PIC X(2)    VALUE HIGH-VALUE.            
027400                                                                          
027500     03  W-WDB901KY-MIN2.                                                 
027600         05  W-WDB901-IDDC-MIN2  PIC X(2)    VALUE LOW-VALUE.             
027700         05  W-WDB901-IDDISTR-MIN2                                        
027800                                 PIC S9(5)   VALUE ZERO   COMP-3.         
027900         05  W-WDB901-IDKUND-MIN2                                         
028000                                 PIC X(10)   VALUE SPACE.                 
028100         05  FILLER REDEFINES W-WDB901-IDKUND-MIN2.                       
028200             07 W-WDB901-IDKUNDNR-MIN2                                    
028300                                 PIC 9(6).                                
028400             07 FILLER           PIC X(4).                                
028500         05  FILLER REDEFINES W-WDB901-IDKUND-MIN2.                       
028600             07 W-WDB901-IDLEVNR-MIN2                                     
028700                                 PIC X(5).                                
028800             07 FILLER           PIC X(5).                                
028900         05  W-IDDC-REC-MIN2     PIC X(2)    VALUE LOW-VALUE.             
029000                                                                          
029100     03  W-WDB901KY-MAX2.                                                 
029200         05  W-WDB901-IDDC-MAX2  PIC X(2)    VALUE HIGH-VALUE.            
029300         05  W-WDB901-IDDISTR-MAX2                                        
029400                                 PIC S9(5)   VALUE +99999 COMP-3.         
029500         05  W-WDB901-IDKUND-MAX2                                         
029600                                 PIC X(10)   VALUE SPACE.                 
029700         05  FILLER REDEFINES W-WDB901-IDKUND-MAX2.                       
029800             07 W-WDB901-IDKUNDNR-MAX2                                    
029900                                 PIC 9(6).                                
030000             07 FILLER           PIC X(4).                                
030100         05  FILLER REDEFINES W-WDB901-IDKUND-MAX2.                       
030200             07 W-WDB901-IDLEVNR-MAX2                                     
030300                                 PIC X(5).                                
030400             07 FILLER           PIC X(5).                                
030500         05  W-IDDC-REC-MAX2     PIC X(2)     VALUE HIGH-VALUE.           
030600                                                                          
030700     03  W-IDPRODNR-X.                                                    
030800         05 W-IDPRODNR           PIC S9(7)                 COMP-3.        
030900                                                                          
031000     SKIP2                                                                
031100*    --- STATUS-KOD FROM IMS                                              
031200 01  STATUS-WS                   PIC XX.                                  
031300     88  SEGMENT-FOUND                       VALUE '  '.                  
031400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
031500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
031600     SKIP2                                                                
031700 01  GOOD-STATUSCODES.                                                    
031800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031900     SKIP3                                                                
032000 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
032100 01  SSA1                        PIC X(128).                              
032200 01  SSA2                        PIC X(64).                               
032300     EJECT                                                                
032400*    --- IMS FUNCTION CODES                                               
032500*01  -COPY W0003                                                          
032600     EJECT                                                                
032700*    ---  DLI INPUT-OUTPUT AREA                                           
032800                                                                          
032900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
033000 01  DLI-IO-WDE101.                                                       
033100*    03  -COPY WDE101                                                     
033200                                                                          
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
033400 01  DLI-IO-WDE111.                                                       
033500*    03  -COPY WDE111                                                     
033600                                                                          
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
033800 01  DLI-IO-WDE121.                                                       
033900*    03  -COPY WDE121                                                     
034000                                                                          
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
034200 01  DLI-IO-WDB201.                                                       
034300*    03  -COPY WDB201                                                     
034400                                                                          
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB301'.                      
034600 01  DLI-IO-WDB301.                                                       
034700*    03  -COPY WDB301                                                     
034800                                                                          
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB901'.                      
035000 01  DLI-IO-WDB901.                                                       
035100*    03  -COPY WDB901                                                     
035200                                                                          
035300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
035400 01  DLI-IO-WDE601.                                                       
035500*    03  -COPY WDE601                                                     
035600                                                                          
035700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
035800 01  DLI-IO-WDB601.                                                       
035900*    03  -COPY WDB601                                                     
036000     EJECT                                                                
036100 LINKAGE SECTION.                                                         
036200                                                                          
036300 01  IO-PCB       PIC X.                                                  
036400                                                                          
036500*01  -COPY W0009  -PRE ALT-                                               
036600     EJECT                                                                
036700                                                                          
036800*01  -COPY W0009  -PRE DISTRDOC-                                          
036900                                                                          
037000*01  -COPY W0009  -PRE DISTRWEB-                                          
037100     EJECT                                                                
037200                                                                          
037300*01  -COPY W0008  -PRE WDE1-                                              
037400     05  FILLER                  PIC X.                                   
037500                                                                          
037600*01  -COPY W0008  -PRE WDB3-                                              
037700     05  FILLER                  PIC X.                                   
037800                                                                          
037900*01  -COPY W0008  -PRE WDB9-                                              
038000     05  FILLER                  PIC X.                                   
038100                                                                          
038200*01  -COPY W0008  -PRE WDE6-                                              
038300     05  FILLER                  PIC X.                                   
038400                                                                          
038500*01  -COPY W0008  -PRE WDB6-                                              
038600     05  FILLER                  PIC X.                                   
038700                                                                          
038800 01  KLIS-WDE1-PCB               PIC X.                                   
038900 01  KLIS-WDQ2-PCB               PIC X.                                   
039000 01  KLIS-WDR1-PCB               PIC X.                                   
039100 01  KLIS-WDB2-PCB               PIC X.                                   
039200 01  KLIS-WDG2-PCB               PIC X.                                   
039300 01  KLIS-WDE7-PCB               PIC X.                                   
039400                                                                          
039500 01  VERS-WDE1-PCB               PIC X.                                   
039600 01  VERS-4731-PCB               PIC X.                                   
039700 01  VERS-4735-PCB               PIC X.                                   
039800 01  VERS-WDB2-PCB               PIC X.                                   
039900                                                                          
040000 01  SPED-WDE1-PCB               PIC X.                                   
040100 01  SPED-4732-PCB               PIC X.                                   
040200 01  SPED-4735-PCB               PIC X.                                   
040300 01  SPED-4738-PCB               PIC X.                                   
040400 01  SPED-WDB2-PCB               PIC X.                                   
040500                                                                          
040600 01  GMTL-WDE1-PCB               PIC X.                                   
040700 01  GMTL-4738-PCB               PIC X.                                   
040800 01  GMTL-WDB1-PCB               PIC X.                                   
040900 01  GMTL-WDB2-PCB               PIC X.                                   
041000                                                                          
041100 01  STAT-WDE1-PCB               PIC X.                                   
041200 01  STAT-WDB2-PCB               PIC X.                                   
041300                                                                          
041400 01  PACK-WDE1-PCB               PIC X.                                   
041500 01  PACK-WDE2-PCB               PIC X.                                   
041600 01  PACK-WDB1-PCB               PIC X.                                   
041700 01  PACK-WDB2-PCB               PIC X.                                   
041800 01  PACK-4738-PCB               PIC X.                                   
041900 01  PACK-WDF5-PCB               PIC X.                                   
042000                                                                          
042100 01  NAPR-WDE1-PCB               PIC X.                                   
042200 01  NAPR-WDB2-PCB               PIC X.                                   
042300 01  NAPR-WDK7-PCB               PIC X.                                   
042400 01  NAPR-WDD3-PCB               PIC X.                                   
042500 01  NAPR-WDR7-PCB               PIC X.                                   
042600 01  NAPR-WDB6-PCB               PIC X.                                   
042700                                                                          
042800 01  KULB-WDE1-PCB               PIC X.                                   
042900 01  KULB-WDB2-PCB               PIC X.                                   
043000 01  KULB-WDB1-PCB               PIC X.                                   
043100 01  KULB-WDG7-PCB               PIC X.                                   
043200                                                                          
043300 01  BLAD-WDE1-PCB               PIC X.                                   
043400 01  BLAD-WDE6-PCB               PIC X.                                   
043500 01  BLAD-WDQ2-PCB               PIC X.                                   
043600 01  BLAD-WDR1-PCB               PIC X.                                   
043700 01  BLAD-1165-PCB               PIC X.                                   
043800 01  BLAD-WDR6-PCB               PIC X.                                   
043900                                                                          
044000 01  MANF-WDK6-PCB               PIC X.                                   
044100 01  MANF-WDF1-PCB               PIC X.                                   
044200                                                                          
044300 01  CUST-WDE1-PCB               PIC X.                                   
044400 01  CUST-WDD3-PCB               PIC X.                                   
044500 01  CUST-WDK6-PCB               PIC X.                                   
044600                                                                          
044700     EJECT                                                                
044800 PROCEDURE DIVISION  USING IO-PCB ALT-PCB                                 
044900                           DISTRDOC-PCB  DISTRWEB-PCB                     
045000                           WDE1-PCB WDB3-PCB WDB9-PCB                     
045100                           WDE6-PCB WDB6-PCB                              
045200                           KLIS-WDE1-PCB                                  
045300                           KLIS-WDQ2-PCB KLIS-WDR1-PCB                    
045400                           KLIS-WDB2-PCB                                  
045500                           KLIS-WDG2-PCB                                  
045600                           KLIS-WDE7-PCB                                  
045700                           VERS-WDE1-PCB VERS-4731-PCB                    
045800                           VERS-4735-PCB VERS-WDB2-PCB                    
045900                           SPED-WDE1-PCB                                  
046000                           SPED-4732-PCB SPED-4735-PCB                    
046100                           SPED-4738-PCB SPED-WDB2-PCB                    
046200                           GMTL-WDE1-PCB                                  
046300                           GMTL-4738-PCB GMTL-WDB1-PCB                    
046400                           GMTL-WDB2-PCB                                  
046500                           STAT-WDE1-PCB STAT-WDB2-PCB                    
046600                           PACK-WDE1-PCB PACK-WDE2-PCB                    
046700                           PACK-WDB1-PCB PACK-WDB2-PCB                    
046800                           PACK-4738-PCB PACK-WDF5-PCB                    
046900                           NAPR-WDE1-PCB NAPR-WDB2-PCB                    
047000                           NAPR-WDK7-PCB NAPR-WDD3-PCB                    
047100                           NAPR-WDR7-PCB NAPR-WDB6-PCB                    
047200                           KULB-WDE1-PCB KULB-WDB2-PCB                    
047300                           KULB-WDB1-PCB KULB-WDG7-PCB                    
047400                           BLAD-WDE1-PCB BLAD-WDE6-PCB                    
047500                           BLAD-WDQ2-PCB BLAD-WDR1-PCB                    
047600                           BLAD-1165-PCB BLAD-WDR6-PCB                    
047700                           MANF-WDK6-PCB MANF-WDF1-PCB                    
047800                           CUST-WDE1-PCB CUST-WDD3-PCB                    
047900                           CUST-WDK6-PCB.                                 
048000                                                                          
048100 MAIN SECTION.                                                            
048200     ENTRY 'DLITCBL' USING IO-PCB ALT-PCB                                 
048300                           DISTRDOC-PCB  DISTRWEB-PCB                     
048400                           WDE1-PCB WDB3-PCB WDB9-PCB                     
048500                           WDE6-PCB WDB6-PCB                              
048600                           KLIS-WDE1-PCB                                  
048700                           KLIS-WDQ2-PCB KLIS-WDR1-PCB                    
048800                           KLIS-WDB2-PCB                                  
048900                           KLIS-WDG2-PCB                                  
049000                           KLIS-WDE7-PCB                                  
049100                           VERS-WDE1-PCB VERS-4731-PCB                    
049200                           VERS-4735-PCB VERS-WDB2-PCB                    
049300                           SPED-WDE1-PCB                                  
049400                           SPED-4732-PCB SPED-4735-PCB                    
049500                           SPED-4738-PCB SPED-WDB2-PCB                    
049600                           GMTL-WDE1-PCB                                  
049700                           GMTL-4738-PCB GMTL-WDB1-PCB                    
049800                           GMTL-WDB2-PCB                                  
049900                           STAT-WDE1-PCB STAT-WDB2-PCB                    
050000                           PACK-WDE1-PCB PACK-WDE2-PCB                    
050100                           PACK-WDB1-PCB PACK-WDB2-PCB                    
050200                           PACK-4738-PCB PACK-WDF5-PCB                    
050300                           NAPR-WDE1-PCB NAPR-WDB2-PCB                    
050400                           NAPR-WDK7-PCB NAPR-WDD3-PCB                    
050500                           NAPR-WDR7-PCB NAPR-WDB6-PCB                    
050600                           KULB-WDE1-PCB KULB-WDB2-PCB                    
050700                           KULB-WDB1-PCB KULB-WDG7-PCB                    
050800                           BLAD-WDE1-PCB BLAD-WDE6-PCB                    
050900                           BLAD-WDQ2-PCB BLAD-WDR1-PCB                    
051000                           BLAD-1165-PCB BLAD-WDR6-PCB                    
051100                           MANF-WDK6-PCB MANF-WDF1-PCB                    
051200                           CUST-WDE1-PCB CUST-WDD3-PCB                    
051300                           CUST-WDK6-PCB.                                 
051400                                                                          
051500     PERFORM A-INIT                                                       
051600     PERFORM B-CHECK-KEYS                                                 
051700     IF KEYS-OK                                                           
051800       PERFORM C-LOAD-BASIC-DATA                                          
051900     END-IF                                                               
052000                                                                          
052100     IF ALLT-OK                                                           
052200       PERFORM IMS-GNP-WDE111                                             
052300       PERFORM S04-CHECK-IDLEVNR                                          
052400                                                                          
052500       PERFORM UNTIL SEGMENT-MISSING                                      
052600                                                                          
052700         PERFORM D-INIT-KEYS                                              
052800                                                                          
052900         PERFORM S01-GET-DIST-IDPRTLST                                    
053000                                                                          
053100         IF DOK-FLSKRIV-ONDEM = YES                                       
053200           MOVE YES            TO TRPD-FLSKRIV-ONDEM                      
053300         ELSE                                                             
053400           MOVE NOO            TO TRPD-FLSKRIV-ONDEM                      
053500         END-IF                                                           
053600         IF FLWEBDC                                                       
053700           MOVE YES            TO TRPD-FLLDCKND                           
053800         ELSE                                                             
053900           MOVE NOO            TO TRPD-FLLDCKND                           
054000         END-IF                                                           
054100                                                                          
054200*        -- LOOP OVER EACH MATCHING LINE FROM THE "4456 SCREEN"           
054300         PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > INDX-MAX         
054400                                                                          
054500*          -- EACH LINE MAY HAVE ITS OWN PRINTER SPECIFIED                
054600           IF SEARCH-IDPRTLST                                             
054700             MOVE TAB-IDPRTLST (INDX) TO W-IDPRTLST                       
054800           END-IF                                                         
054900                                                                          
055000*CARGO SPEC, KOLLISPEC                               KLIS (A)             
055100           MOVE 0 TO WS-INDX                                              
055200           MOVE '1' TO TRPD-KVCOPIES                                      
055300           PERFORM VARYING WS-COPY FROM +1 BY +1                          
055400           UNTIL WS-COPY > TAB-KVCOPIES-KLIS (INDX)                       
055500*            -- LOOP ONCE FOR EACH COPY                                   
055600                                                                          
055700*            -- THIS REPORT HAS A SPECIAL WEB VERSION                     
055800             MOVE YES TO WEB-REPORT-SW                                    
055900*            -- FOR NON-WEB DC:S THE PAPER VERSION IS SELECTED.           
056000*            -- S03 SECTION WILL DO AN OPEN ONLY THE FIRST TIME.          
056100             IF NOT FLWEBDC                                               
056200               PERFORM S03-OPEN-PRT                                       
056300             END-IF                                                       
056400                                                                          
056500             IF WS-INDX = 0                                               
056600*            -- EITHER WEB OR ONDEMAND REPORT CAN PE PRODUCED,            
056700*            -- NOT BOTH. THE REPORT IS ONLY OPENED FOR FIRST LAP         
056800*            -- OF THE LOOP (FIRST COPY) AND CLOSED AT THE END            
056900*            -- OF THIS LAP.                                              
057000               IF FLWEBDC                                                 
057100*                -- WEB VERSION. SUBPROGRAM WRITES HDR RECORD             
057200                 PERFORM S90-OPEN-SEND-WEB                                
057300               ELSE                                                       
057400                 IF DOK-FLSKRIV-ONDEM = YES                               
057500*                -- ONDEMAND VERSION. HDR RECORD IS WRITTEN HERE          
057600                   PERFORM S90-OPEN-SEND-ONDEM                            
057700                   MOVE 'SHIPDOC-CS' TO HDR-IDOUTTYPE                     
057800                   MOVE +1           TO SEND-IDCOM                        
057900                   PERFORM S90-PUT-DAPHDR-ONDEM                           
058000                 END-IF                                                   
058100               END-IF                                                     
058200             END-IF                                                       
058300                                                                          
058400             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
058500             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
058600             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
058700             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
058800             MOVE MID-IDPGM        TO TRPD-IDPGM                          
058900             CALL W476KLIS      USING TRPD-W476TRPD                       
059000                                      ALT-PCB                             
059100                                      KLIS-WDE1-PCB                       
059200                                      KLIS-WDQ2-PCB                       
059300                                      KLIS-WDR1-PCB                       
059400                                      KLIS-WDB2-PCB                       
059500                                      PACK-WDB1-PCB                       
059600                                      KLIS-WDG2-PCB                       
059700                                      KLIS-WDE7-PCB                       
059800             IF WS-INDX = 0                                               
059900*              -- CLOSE WEB/ONDEMAND REPORT AFTER FIRST LAP.              
060000*              -- EXTRA COPIES (LOOP LAPS) ARE IGNORED BY                 
060100*              -- THE SUBPROGRAM (PRINTS ONLY EXTRA COPIES                
060200*              -- FOR THE PAPER VERSION)                                  
060300               PERFORM S90-CLOSE-SEND-WEB-ONDEM                           
060400             END-IF                                                       
060500                                                                          
060600             ADD +1 TO WS-INDX                                            
060700*            -- 'S' WILL CAUSE SUBPROGRAM TO IGNORE                       
060800*            -- FURTHER CALLS FOR WEB OR ONDEMAND REPORTS                 
060900             MOVE 'S' TO TRPD-KVCOPIES                                    
061000           END-PERFORM                                                    
061100                                                                          
061200*CARGO VALUE SPEC                                    VERS (B)             
061300           MOVE 0 TO WS-INDX                                              
061400           MOVE '1' TO TRPD-KVCOPIES                                      
061500           PERFORM VARYING WS-COPY FROM +1 BY +1                          
061600           UNTIL WS-COPY > TAB-KVCOPIES-VERS (INDX)                       
061700                                                                          
061800             MOVE YES TO WEB-REPORT-SW                                    
061900             IF NOT FLWEBDC                                               
062000               PERFORM S03-OPEN-PRT                                       
062100             END-IF                                                       
062200             IF WS-INDX = 0                                               
062300               IF FLWEBDC                                                 
062400                 PERFORM S90-OPEN-SEND-WEB                                
062500               ELSE                                                       
062600                 PERFORM S90-OPEN-SEND-ONDEM                              
062700                 MOVE 'SHIPDOC-CVS'    TO HDR-IDOUTTYPE                   
062800                 MOVE +1               TO SEND-IDCOM                      
062900                 PERFORM S90-PUT-DAPHDR-ONDEM                             
063000               END-IF                                                     
063100             END-IF                                                       
063200                                                                          
063300             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
063400             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
063500             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
063600             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
063700             MOVE MID-IDPGM        TO TRPD-IDPGM                          
063800                                                                          
063900             CALL W476VERS      USING TRPD-W476TRPD                       
064000                                      ALT-PCB                             
064100                                      VERS-WDE1-PCB                       
064200                                      VERS-4731-PCB                       
064300                                      VERS-4735-PCB                       
064400                                      VERS-WDB2-PCB                       
064500                                      PACK-WDB1-PCB                       
064600                                      KLIS-WDQ2-PCB                       
064700                                      KLIS-WDG2-PCB                       
064800             IF WS-INDX = 0                                               
064900               PERFORM S90-CLOSE-SEND-WEB-ONDEM                           
065000             END-IF                                                       
065100                                                                          
065200             ADD +1 TO WS-INDX                                            
065300             MOVE 'S' TO TRPD-KVCOPIES                                    
065400           END-PERFORM                                                    
065500                                                                          
065600*CUSTOM INFORMATION, STATNUMMERBILAGA                STAT (C)             
065700*CUSTOM INFORMATION, STATNUMMERBILAGA FOR DUBAI      STAT (C)             
065710*DC 87 GETTING CLOSED.NEED CIS/HS DOCS FOR DISTRICTS 71,8224,8225         
065720*8480,8490 AND 8497(C-CUSTOM INFO IN 4456 NEEDS TO BE SET AND             
065730*INSTRUCTION SET IN 4415)                                                 
065740           MOVE W-IDDISTR          TO TEST-IDDISTR                        
065750                                                                          
065760           IF ((SHIP-IDDC = WC-NDC-AE)      AND                           
065770                 (DIST18-SCRAP-NDC-SC       OR                            
065780                  DIST18-SCRAP-NDC-QUAL     OR                            
065790                  DIST18-SCRAP-NDC-SC-LOCAL OR                            
065791                  DIST20-EMBALLAGE-SDC ))                                 
065792                                            OR                            
065793                  DIST35-CDC-AE-REFILL      OR                            
065794                  DIST35-AE-CDC-RETURNS                                   
065795              MOVE YES             TO SKRIV-DOK-D-SW                      
065796           END-IF                                                         
066100           IF SKRIV-DOK-CUST                                              
066200             MOVE 0 TO WS-INDX                                            
066300             MOVE '1' TO TRPD-KVCOPIES                                    
066400             PERFORM VARYING WS-COPY FROM +1 BY +1                        
066500             UNTIL WS-COPY > TAB-KVCOPIES-STAT (INDX)                     
066600                                                                          
066700*              -- THIS REPORT ONLY EXISTS IN A CLASSIC VERSION            
066800               MOVE NOO TO WEB-REPORT-SW                                  
066900*              -- S03 SECTION WILL DO AN OPEN ONLY IF NOT ALREADY         
067000*              -- DONE BY PREVIOUS PERFORMS                               
067100               PERFORM S03-OPEN-PRT                                       
067200                                                                          
067300*              -- ANY ONDEMAND REPORT IS ONLY OPENED IN FIRST LAP         
067400*              -- OF THE LOOP (FIRST COPY) AND CLOSED AT THE END          
067500*              -- OF THIS LAP.                                            
067600               IF WS-INDX = 0                                             
067700                 IF DOK-FLSKRIV-ONDEM = YES                               
067800                   PERFORM S90-OPEN-SEND-ONDEM                            
067900                   MOVE 'SHIPDOC-CIS' TO HDR-IDOUTTYPE                    
068000                   MOVE +1         TO SEND-IDCOM                          
068100                   PERFORM S90-PUT-DAPHDR-ONDEM                           
068200                 END-IF                                                   
068300               END-IF                                                     
068400                                                                          
068500               MOVE W-IDSHIPM      TO TRPD-IDSHIPM                        
068600               MOVE W-IDPRTLST     TO TRPD-IDPRTLST                       
068700               MOVE PRT-PFDEF-OVR  TO TRPD-PFDEF-OVR                      
068800               MOVE W-IDDISTR      TO TRPD-IDDISTR                        
068900               MOVE MID-IDPGM      TO TRPD-IDPGM                          
069000                                                                          
069100               CALL W476CUST    USING TRPD-W476TRPD                       
069200                                        ALT-PCB                           
069300                                        CUST-WDE1-PCB                     
069400                                        STAT-WDB2-PCB                     
069500                                        PACK-WDB1-PCB                     
069600                                        KLIS-WDG2-PCB                     
069700                                        CUST-WDD3-PCB                     
069800                                        CUST-WDK6-PCB                     
069900                                                                          
070000*              -- CLOSE ONDEMAND REPORT IF IT WAS OPENED ABOVE            
070100               IF WS-INDX = 0                                             
070200                 IF DOK-FLSKRIV-ONDEM = YES                               
070300                   PERFORM S90-CLOSE-SEND-WEB-ONDEM                       
070400                 END-IF                                                   
070500               END-IF                                                     
070600                                                                          
070700               ADD +1 TO WS-INDX                                          
070800               MOVE 'S' TO TRPD-KVCOPIES                                  
070900             END-PERFORM                                                  
071000                                                                          
071100                                                                          
071200           ELSE                                                           
071300             MOVE 0 TO WS-INDX                                            
071400             MOVE '1' TO TRPD-KVCOPIES                                    
071500             PERFORM VARYING WS-COPY FROM +1 BY +1                        
071600             UNTIL WS-COPY > TAB-KVCOPIES-STAT (INDX)                     
071700                                                                          
071800*              -- THIS REPORT ONLY EXISTS IN A CLASSIC VERSION            
071900               MOVE NOO TO WEB-REPORT-SW                                  
072000*              -- S03 SECTION WILL DO AN OPEN ONLY IF NOT ALREADY         
072100*              -- DONE BY PREVIOUS PERFORMS                               
072200               PERFORM S03-OPEN-PRT                                       
072300                                                                          
072400*              -- ANY ONDEMAND REPORT IS ONLY OPENED IN FIRST LAP         
072500*              -- OF THE LOOP (FIRST COPY) AND CLOSED AT THE END          
072600*              -- OF THIS LAP.                                            
072700               IF WS-INDX = 0                                             
072800                 IF DOK-FLSKRIV-ONDEM = YES                               
072900                   PERFORM S90-OPEN-SEND-ONDEM                            
073000                   MOVE 'SHIPDOC-CI' TO HDR-IDOUTTYPE                     
073100                   MOVE +1         TO SEND-IDCOM                          
073200                   PERFORM S90-PUT-DAPHDR-ONDEM                           
073300                 END-IF                                                   
073400               END-IF                                                     
073500                                                                          
073600               MOVE W-IDSHIPM      TO TRPD-IDSHIPM                        
073700               MOVE W-IDPRTLST     TO TRPD-IDPRTLST                       
073800               MOVE PRT-PFDEF-OVR  TO TRPD-PFDEF-OVR                      
073900               MOVE W-IDDISTR      TO TRPD-IDDISTR                        
074000               MOVE MID-IDPGM      TO TRPD-IDPGM                          
074100                                                                          
074200               CALL W476STAT    USING TRPD-W476TRPD                       
074300                                        ALT-PCB                           
074400                                        STAT-WDE1-PCB                     
074500                                        STAT-WDB2-PCB                     
074600                                        PACK-WDB1-PCB                     
074700                                        KLIS-WDG2-PCB                     
074800                                                                          
074900*              -- CLOSE ONDEMAND REPORT IF IT WAS OPENED ABOVE            
075000               IF WS-INDX = 0                                             
075100                 IF DOK-FLSKRIV-ONDEM = YES                               
075200                   PERFORM S90-CLOSE-SEND-WEB-ONDEM                       
075300                 END-IF                                                   
075400               END-IF                                                     
075500                                                                          
075600               ADD +1 TO WS-INDX                                          
075700               MOVE 'S' TO TRPD-KVCOPIES                                  
075800             END-PERFORM                                                  
075900           END-IF                                                         
076000                                                                          
076100*BOOKING DOCUMENT, SISTA SIDAN                       SPED (D)             
076200           MOVE 0 TO WS-INDX                                              
076300           MOVE '1' TO TRPD-KVCOPIES                                      
076400           PERFORM VARYING WS-COPY FROM +1 BY +1                          
076500           UNTIL WS-COPY > TAB-KVCOPIES-SPED (INDX)                       
076600                                                                          
076700             MOVE NOO  TO WEB-REPORT-SW                                   
076800             PERFORM S03-OPEN-PRT                                         
076900             IF WS-INDX = 0                                               
077000               IF DOK-FLSKRIV-ONDEM = YES                                 
077100                 PERFORM S90-OPEN-SEND-ONDEM                              
077200                 MOVE 'SHIPDOC-BD' TO HDR-IDOUTTYPE                       
077300                 MOVE +1           TO SEND-IDCOM                          
077400                 PERFORM S90-PUT-DAPHDR-ONDEM                             
077500               END-IF                                                     
077600             END-IF                                                       
077700                                                                          
077800             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
077900             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
078000             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
078100             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
078200             MOVE MID-IDPGM        TO TRPD-IDPGM                          
078300                                                                          
078400             CALL W476SPED      USING TRPD-W476TRPD                       
078500                                      ALT-PCB                             
078600                                      SPED-WDE1-PCB                       
078700                                      SPED-4732-PCB                       
078800                                      SPED-4735-PCB                       
078900                                      SPED-4738-PCB                       
079000                                      SPED-WDB2-PCB                       
079100                                      PACK-WDB1-PCB                       
079200                                      KLIS-WDE7-PCB                       
079300                                      KLIS-WDG2-PCB                       
079400                                                                          
079500             IF WS-INDX = 0                                               
079600               IF DOK-FLSKRIV-ONDEM = YES                                 
079700                 PERFORM S90-CLOSE-SEND-WEB-ONDEM                         
079800               END-IF                                                     
079900             END-IF                                                       
080000                                                                          
080100             ADD +1 TO WS-INDX                                            
080200             MOVE 'S' TO TRPD-KVCOPIES                                    
080300           END-PERFORM                                                    
080400                                                                          
080500*PACKING DOCUMENT                                    PACK (E)             
080600           MOVE 0 TO WS-INDX                                              
080700           MOVE '1' TO TRPD-KVCOPIES                                      
080800           PERFORM VARYING WS-COPY FROM +1 BY +1                          
080900           UNTIL WS-COPY > TAB-KVCOPIES-PACK (INDX)                       
081000                                                                          
081100             MOVE NOO TO WEB-REPORT-SW                                    
081200             PERFORM S03-OPEN-PRT                                         
081300             IF WS-INDX = 0                                               
081400               IF DOK-FLSKRIV-ONDEM = YES                                 
081500                 PERFORM S90-OPEN-SEND-ONDEM                              
081600                 MOVE 'SHIPDOC-PS' TO HDR-IDOUTTYPE                       
081700                 MOVE +1           TO SEND-IDCOM                          
081800                 PERFORM S90-PUT-DAPHDR-ONDEM                             
081900               END-IF                                                     
082000             END-IF                                                       
082100                                                                          
082200             MOVE W-IDDISTR        TO TEST-IDDISTR                        
082300                                                                          
082400             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
082500             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
082600             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
082700             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
082800             MOVE MID-IDPGM        TO TRPD-IDPGM                          
082900                                                                          
083000                                                                          
083100             CALL W476PACK      USING TRPD-W476TRPD                       
083200                                      ALT-PCB                             
083300                                      PACK-WDE1-PCB                       
083400                                      PACK-WDE2-PCB                       
083500                                      PACK-WDB1-PCB                       
083600                                      PACK-WDB2-PCB                       
083700                                      PACK-4738-PCB                       
083800                                      PACK-WDF5-PCB                       
083900                                                                          
084000             IF WS-INDX = 0                                               
084100               IF DOK-FLSKRIV-ONDEM = YES                                 
084200                 PERFORM S90-CLOSE-SEND-WEB-ONDEM                         
084300               END-IF                                                     
084400             END-IF                                                       
084500                                                                          
084600             ADD +1 TO WS-INDX                                            
084700             MOVE 'S' TO TRPD-KVCOPIES                                    
084800           END-PERFORM                                                    
084900                                                                          
085000*GOODS RECIEVER LIST, GODSMOTTAGARLISTA              GMTL (F)             
085100           MOVE 0 TO WS-INDX                                              
085200           MOVE '1' TO TRPD-KVCOPIES                                      
085300           PERFORM VARYING WS-COPY FROM +1 BY +1                          
085400           UNTIL WS-COPY > TAB-KVCOPIES-GMTL (INDX)                       
085500                                                                          
085600             MOVE NOO TO WEB-REPORT-SW                                    
085700             PERFORM S03-OPEN-PRT                                         
085800             IF WS-INDX = 0                                               
085900               IF DOK-FLSKRIV-ONDEM = YES                                 
086000                 PERFORM S90-OPEN-SEND-ONDEM                              
086100                 MOVE 'SHIPDOC-GRL' TO HDR-IDOUTTYPE                      
086200                 MOVE +1           TO SEND-IDCOM                          
086300                 PERFORM S90-PUT-DAPHDR-ONDEM                             
086400               END-IF                                                     
086500             END-IF                                                       
086600                                                                          
086700             MOVE W-IDDISTR     TO TEST-IDDISTR                           
086800                                                                          
086900             MOVE W-IDSHIPM     TO TRPD-IDSHIPM                           
087000             MOVE W-IDPRTLST    TO TRPD-IDPRTLST                          
087100             MOVE PRT-PFDEF-OVR TO TRPD-PFDEF-OVR                         
087200             MOVE W-IDDISTR     TO TRPD-IDDISTR                           
087300             MOVE MID-IDPGM     TO TRPD-IDPGM                             
087400                                                                          
087500             CALL W476GMTL USING TRPD-W476TRPD                            
087600                                 ALT-PCB                                  
087700                                 GMTL-WDE1-PCB                            
087800                                 GMTL-4738-PCB                            
087900                                 GMTL-WDB1-PCB                            
088000                                 GMTL-WDB2-PCB                            
088100                                                                          
088200             IF WS-INDX = 0                                               
088300               IF DOK-FLSKRIV-ONDEM = YES                                 
088400                 PERFORM S90-CLOSE-SEND-WEB-ONDEM                         
088500               END-IF                                                     
088600             END-IF                                                       
088700                                                                          
088800             ADD +1 TO WS-INDX                                            
088900             MOVE 'S' TO TRPD-KVCOPIES                                    
089000           END-PERFORM                                                    
089100                                                                          
089200                                                                          
089300*KULLAGER-BILAGA                                     KULB (H)             
089400           MOVE 0 TO WS-INDX                                              
089500           MOVE '1' TO TRPD-KVCOPIES                                      
089600           PERFORM VARYING WS-COPY FROM +1 BY +1                          
089700           UNTIL WS-COPY > TAB-KVCOPIES-KULB (INDX)                       
089800                                                                          
089900             MOVE NOO TO WEB-REPORT-SW                                    
090000             PERFORM S03-OPEN-PRT                                         
090100             IF WS-INDX = 0                                               
090200               IF DOK-FLSKRIV-ONDEM = YES                                 
090300                 PERFORM S90-OPEN-SEND-ONDEM                              
090400                 MOVE 'SHIPDOC-NAK' TO HDR-IDOUTTYPE                      
090500                 MOVE +1           TO SEND-IDCOM                          
090600                 PERFORM S90-PUT-DAPHDR-ONDEM                             
090700               END-IF                                                     
090800             END-IF                                                       
090900                                                                          
091000             MOVE W-IDDISTR        TO TEST-IDDISTR                        
091100                                                                          
091200             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
091300             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
091400             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
091500             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
091600             MOVE MID-IDPGM        TO TRPD-IDPGM                          
091700                                                                          
091800             CALL W476KULB      USING TRPD-W476TRPD                       
091900                                      ALT-PCB                             
092000                                      KULB-WDE1-PCB                       
092100                                      KULB-WDB2-PCB                       
092200                                      KULB-WDB1-PCB                       
092300                                      KULB-WDG7-PCB                       
092400                                                                          
092500             IF WS-INDX = 0                                               
092600               IF DOK-FLSKRIV-ONDEM = YES                                 
092700                 PERFORM S90-CLOSE-SEND-WEB-ONDEM                         
092800               END-IF                                                     
092900             END-IF                                                       
093000                                                                          
093100             ADD +1 TO WS-INDX                                            
093200             MOVE 'S' TO TRPD-KVCOPIES                                    
093300           END-PERFORM                                                    
093400                                                                          
093500*NORTH AMERICAN PROFORMA                             NAPR (G)             
093600                                                                          
093700*          -- CLOSE ANY CURRENT PRINTING BEFORE                           
093800*          -- CHANGING TO AMERICAN LASER FORM                             
093900           IF IDPRTLST-OPEN                                               
094000             CALL W006PRS1 USING PRT-SPOOL-OVR                            
094100                                 PRT-CLOSE                                
094200                                 W-IDPRTLST                               
094300                                 ALT-PCB                                  
094400                                 DUMMY-AREA                               
094500                                 DUMMY-AREA                               
094600             MOVE NOO  TO IDPRTLST-OPEN-SW                                
094700           END-IF                                                         
094800*          -- AMERICAN LASER FORM (LETTER FORMAT)                         
094900           MOVE 'W47602'         TO PRT-PFDEF-OVR                         
095000                                                                          
095100           MOVE 0 TO WS-INDX                                              
095200           MOVE '1' TO TRPD-KVCOPIES                                      
095300           PERFORM VARYING WS-COPY FROM +1 BY +1                          
095400           UNTIL WS-COPY > TAB-KVCOPIES-NAPR (INDX)                       
095500*            -- LOOP ONCE FOR EACH COPY                                   
095600                                                                          
095700*            -- THIS REPORT HAS A SPECIAL WEB VERSION                     
095800             MOVE YES TO WEB-REPORT-SW                                    
095900*            -- FOR NON-WEB DC:S THE PAPER VERSION IS SELECTED.           
096000*            -- S03 SECTION WILL DO AN OPEN ONLY THE FIRST TIME.          
096100             IF NOT FLWEBDC                                               
096200               PERFORM S03-OPEN-PRT                                       
096300             END-IF                                                       
096400                                                                          
096500             IF WS-INDX = 0                                               
096600*            -- EITHER WEB OR ONDEMAND REPORT CAN PE PRODUCED,            
096700*            -- NOT BOTH. THE REPORT IS ONLY OPENED FOR FIRST LAP         
096800*            -- OF THE LOOP (FIRST COPY) AND CLOSED AT THE END            
096900*            -- OF THIS LAP.                                              
097000               IF FLWEBDC                                                 
097100*                -- WEB VERSION                                           
097200                 PERFORM S90-OPEN-SEND-WEB                                
097300                 MOVE 'NA-PROFORMA' TO HDR-IDOUTTYPE                      
097400                 MOVE W-IDDISTR     TO WS-IDLIST-IDDISTR                  
097500                 MOVE W-IDSHIPM     TO WS-IDLIST-IDSHIPM                  
097600                 MOVE WS-IDLIST     TO HDR-IDLIST                         
097700                 MOVE +1            TO SEND-IDCOM                         
097800                 PERFORM S90-PUT-DAPHDR-WEB                               
097900               ELSE                                                       
098000*                -- CLASSIC VERSION                                       
098100                 IF DOK-FLSKRIV-ONDEM = YES                               
098200                   PERFORM S90-OPEN-SEND-ONDEM                            
098300                   MOVE 'SHIPDOC-NAP' TO HDR-IDOUTTYPE                    
098400                   MOVE +1            TO SEND-IDCOM                       
098500                   PERFORM S90-PUT-DAPHDR-ONDEM                           
098600                 END-IF                                                   
098700               END-IF                                                     
098800             END-IF                                                       
098900                                                                          
099000             MOVE W-IDDISTR        TO TEST-IDDISTR                        
099100             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
099200             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
099300             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
099400             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
099500             MOVE MID-IDPGM        TO TRPD-IDPGM                          
099600                                                                          
099700             CALL W476NAPR      USING TRPD-W476TRPD                       
099800                                      ALT-PCB                             
099900                                      NAPR-WDE1-PCB                       
100000                                      NAPR-WDB2-PCB                       
100100                                      PACK-WDB1-PCB                       
100200                                      NAPR-WDK7-PCB                       
100300                                      NAPR-WDD3-PCB                       
100400                                      NAPR-WDR7-PCB                       
100500                                      NAPR-WDB6-PCB                       
100600                                                                          
100700             IF WS-INDX = 0                                               
100800*              -- CLOSE WEB/ONDEMAND REPORT AFTER FIRST LAP.              
100900*              -- EXTRA COPIES (LOOP LAPS) ARE IGNORED BY                 
101000*              -- THE SUBPROGRAM (PRINTS ONLY EXTRA COPIES                
101100*              -- FOR THE PAPER VERSION)                                  
101200               PERFORM S90-CLOSE-SEND-WEB-ONDEM                           
101300             END-IF                                                       
101400                                                                          
101500             ADD +1 TO WS-INDX                                            
101600*            -- 'S' WILL CAUSE SUBPROGRAM TO IGNORE                       
101700*            -- FURTHER CALLS FOR WEB OR ONDEMAND REPORTS                 
101800             MOVE 'S' TO TRPD-KVCOPIES                                    
101900           END-PERFORM                                                    
102000                                                                          
102100*          -- RESET TO EUROPEAN LASER FORM (A4)                           
102200           MOVE 'W47601'         TO PRT-PFDEF-OVR                         
102300                                                                          
102400                                                                          
102500*BILL OF LADING                                      BLAD (I)             
102600*                                                                         
102700***   SKRIVS UT I PROGRAM W4068800, KANSKE SEDAN HÄR                      
102800                                                                          
102900                                                                          
103000*SASO LISTA SAUDI                                    SASO (I)             
103100*                                                                         
103200*                                                                         
103300           MOVE W-IDDISTR          TO TEST-IDDISTR                        
103400           IF DIST74-SAUDI AND DCS-CDC                                    
103500             MOVE 0 TO WS-INDX                                            
103600             MOVE '1' TO TRPD-KVCOPIES                                    
103700             PERFORM VARYING WS-COPY FROM +1 BY +1                        
103800             UNTIL WS-COPY > TAB-KVCOPIES-SASO (INDX)                     
103900                                                                          
104000               MOVE NOO TO WEB-REPORT-SW                                  
104100               PERFORM S03-OPEN-PRT                                       
104200               IF WS-INDX = 0                                             
104300                 IF DOK-FLSKRIV-ONDEM = YES                               
104400                   PERFORM S90-OPEN-SEND-ONDEM                            
104500                   MOVE 'SHIPDOC-SA' TO HDR-IDOUTTYPE                     
104600                   MOVE +1         TO SEND-IDCOM                          
104700                   PERFORM S90-PUT-DAPHDR-ONDEM                           
104800                 END-IF                                                   
104900               END-IF                                                     
105000                                                                          
105100               MOVE W-IDDISTR      TO TEST-IDDISTR                        
105200                                                                          
105300               MOVE W-IDSHIPM      TO TRPD-IDSHIPM                        
105400               MOVE W-IDPRTLST     TO TRPD-IDPRTLST                       
105500               MOVE PRT-PFDEF-OVR  TO TRPD-PFDEF-OVR                      
105600               MOVE W-IDDISTR      TO TRPD-IDDISTR                        
105700               MOVE MID-IDPGM      TO TRPD-IDPGM                          
105800                                                                          
105900               CALL W476SASO    USING TRPD-W476TRPD                       
106000                                        ALT-PCB                           
106100                                        PACK-WDE1-PCB                     
106200                                        PACK-WDB2-PCB                     
106300                                        PACK-WDB1-PCB                     
106400                                        NAPR-WDD3-PCB                     
106500                                                                          
106600               IF WS-INDX = 0                                             
106700*                IF DOK-FLSKRIV-ONDEM = YES                               
106800                   PERFORM S90-CLOSE-SEND-WEB-ONDEM                       
106900*                END-IF                                                   
107000               END-IF                                                     
107100                                                                          
107200               ADD +1 TO WS-INDX                                          
107300               MOVE 'S' TO TRPD-KVCOPIES                                  
107400             END-PERFORM                                                  
107500           END-IF                                                         
107600                                                                          
107700*MANUFACTORING LIST  SAUDI                           MANF (I)             
107800*                                                                         
107900*                                                                         
108000           MOVE W-IDDISTR          TO TEST-IDDISTR                        
108100           IF DIST74-SAUDI-M-LIST AND DCS-CDC                             
108200             MOVE 0 TO WS-INDX                                            
108300             MOVE '1' TO TRPD-KVCOPIES                                    
108400             PERFORM VARYING WS-COPY FROM +1 BY +1                        
108500             UNTIL WS-COPY > TAB-KVCOPIES-MANF (INDX)                     
108600                                                                          
108700               MOVE NOO TO WEB-REPORT-SW                                  
108800               PERFORM S03-OPEN-PRT                                       
108900               IF WS-INDX = 0                                             
109000                 IF DOK-FLSKRIV-ONDEM = YES                               
109100                   PERFORM S90-OPEN-SEND-ONDEM                            
109200                   MOVE 'SHIPDOC-MA' TO HDR-IDOUTTYPE                     
109300                   MOVE +1           TO SEND-IDCOM                        
109400                   PERFORM S90-PUT-DAPHDR-ONDEM                           
109500                 END-IF                                                   
109600               END-IF                                                     
109700                                                                          
109800               MOVE W-IDDISTR      TO TEST-IDDISTR                        
109900                                                                          
110000               MOVE W-IDSHIPM      TO TRPD-IDSHIPM                        
110100               MOVE W-IDPRTLST     TO TRPD-IDPRTLST                       
110200               MOVE PRT-PFDEF-OVR  TO TRPD-PFDEF-OVR                      
110300               MOVE W-IDDISTR      TO TRPD-IDDISTR                        
110400               MOVE MID-IDPGM      TO TRPD-IDPGM                          
110500                                                                          
110600                                                                          
110700               CALL W476MANF      USING TRPD-W476TRPD                     
110800                                        ALT-PCB                           
110900                                        PACK-WDE1-PCB                     
111000                                        PACK-WDB2-PCB                     
111100                                        PACK-WDB1-PCB                     
111200                                        MANF-WDK6-PCB                     
111300                                        MANF-WDF1-PCB                     
111400               IF WS-INDX = 0                                             
111500*                IF DOK-FLSKRIV-ONDEM = YES                               
111600                   PERFORM S90-CLOSE-SEND-WEB-ONDEM                       
111700*                END-IF                                                   
111800               END-IF                                                     
111900                                                                          
112000               ADD +1 TO WS-INDX                                          
112100               MOVE 'S' TO TRPD-KVCOPIES                                  
112200             END-PERFORM                                                  
112300           END-IF                                                         
112400                                                                          
112500*LOADING REPORT                                      TRPT (J)             
112600           IF IDPRTLST-OPEN                                               
112700*            OLIKA BLANKETTER                                             
112800             CALL W006PRS1 USING PRT-SPOOL-OVR                            
112900                                 PRT-CLOSE                                
113000                                 W-IDPRTLST                               
113100                                 ALT-PCB                                  
113200                                 DUMMY-AREA                               
113300                                 DUMMY-AREA                               
113400             MOVE NOO              TO IDPRTLST-OPEN-SW                    
113500           END-IF                                                         
113600                                                                          
113700           MOVE 0 TO WS-INDX                                              
113800           MOVE '1' TO TRPD-KVCOPIES                                      
113900           PERFORM VARYING WS-COPY FROM +1 BY +1                          
114000           UNTIL WS-COPY > TAB-KVCOPIES-TRPT (INDX)                       
114100                                                                          
114200             MOVE NOO TO WEB-REPORT-SW                                    
114300             PERFORM S03-OPEN-PRT                                         
114400             IF WS-INDX = 0                                               
114500               IF DOK-FLSKRIV-ONDEM = YES                                 
114600                 PERFORM S90-OPEN-SEND-ONDEM                              
114700                 MOVE 'SHIPDOC-TR'     TO HDR-IDOUTTYPE                   
114800                 MOVE +1               TO SEND-IDCOM                      
114900                 PERFORM S90-PUT-DAPHDR-ONDEM                             
115000               END-IF                                                     
115100             END-IF                                                       
115200                                                                          
115300             MOVE W-IDSHIPM        TO TRPD-IDSHIPM                        
115400             MOVE W-IDPRTLST       TO TRPD-IDPRTLST                       
115500             MOVE PRT-PFDEF-OVR    TO TRPD-PFDEF-OVR                      
115600             MOVE W-IDDISTR        TO TRPD-IDDISTR                        
115700             MOVE MID-IDPGM        TO TRPD-IDPGM                          
115800                                                                          
115900             CALL W476TRPT      USING TRPD-W476TRPD                       
116000                                      ALT-PCB                             
116100                                      KLIS-WDE1-PCB                       
116200                                      KLIS-WDQ2-PCB                       
116300                                      KLIS-WDR1-PCB                       
116400                                      KLIS-WDB2-PCB                       
116500                                      PACK-WDB1-PCB                       
116600                                      NAPR-WDB6-PCB                       
116700             IF WS-INDX = 0                                               
116800               PERFORM S90-CLOSE-SEND-WEB-ONDEM                           
116900             END-IF                                                       
117000                                                                          
117100             ADD +1 TO WS-INDX                                            
117200             MOVE 'S' TO TRPD-KVCOPIES                                    
117300           END-PERFORM                                                    
117400                                                                          
117500         END-PERFORM                                                      
117600                                                                          
117700** TO READ NEXT DIST                                                      
117800                                                                          
117900         PERFORM IMS-GNP-WDE111-DIST                                      
118000       END-PERFORM                                                        
118100                                                                          
118200       IF IDPRTLST-OPEN                                                   
118300         CALL W006PRS1 USING PRT-SPOOL-OVR                                
118400                             PRT-CLOSE                                    
118500                             W-IDPRTLST                                   
118600                             ALT-PCB                                      
118700                             DUMMY-AREA                                   
118800                             DUMMY-AREA                                   
118900       END-IF                                                             
119000                                                                          
119100       PERFORM IMS-GHU-WDE101                                             
119200       ADD +1 TO SHIP-KVANTEX                                             
119300       PERFORM IMS-REPL-WDE101                                            
119400     END-IF                                                               
119500     MOVE ZERO TO RETURN-CODE                                             
119600     GOBACK                                                               
119700     .                                                                    
119800     EJECT                                                                
119900 A-INIT SECTION.                                                          
120000     MOVE 'A'                     TO WS-SEC                               
120100                                                                          
120200     PERFORM S11-RECV-OPEN                                                
120300     PERFORM S12-RECV-MESSAGE                                             
120400     PERFORM S13-RECV-CLOSE                                               
120500                                                                          
120600     MOVE 'W47601'    TO PRT-PFDEF-OVR                                    
120700                                                                          
120800     MOVE SPACE       TO W-IDDC-PREV                                      
120900     MOVE ZERO        TO W-IDDISTR-PREV                                   
121000     MOVE ZERO        TO W-IDKUNDNR-PREV                                  
121100     MOVE SPACE       TO W-IDLEVNR-PREV                                   
121200                                                                          
121300     .                                                                    
121400     EJECT                                                                
121500 B-CHECK-KEYS SECTION.                                                    
121600     MOVE 'B'                     TO WS-SEC                               
121700                                                                          
121800     MOVE YES TO KEYS-SW                                                  
121900                                                                          
122000     IF MID-IDSHIPM  = ALL '+'                                            
122100       MOVE NOO TO KEYS-SW                                                
122200     ELSE                                                                 
122300       IF MID-IDSHIPM NUMERIC AND MID-IDSHIPM > ZERO                      
122400         MOVE MID-IDSHIPM  TO W-IDSHIPM                                   
122500       ELSE                                                               
122600         MOVE NOO TO KEYS-SW                                              
122700       END-IF                                                             
122800     END-IF                                                               
122900                                                                          
123000     MOVE YES              TO SEARCH-IDPRTLST-SW                          
123100     IF MID-IDPRTLST NOT = ALL '+'                                        
123200       IF MID-IDPRTLST > SPACE                                            
123300*........OM PRINTER ÄR IFYLLD PÅ 4622-BILDEN!                             
123400         MOVE MID-IDPRTLST TO W-IDPRTLST                                  
123500         MOVE NOO          TO SEARCH-IDPRTLST-SW                          
123600       END-IF                                                             
123700     END-IF                                                               
123800                                                                          
123900     IF MID-IDDC-REC NOT = ALL '+'                                        
124000       IF MID-IDDC-REC > SPACE                                            
124100*........ IDDC DÄR PRINTERN FINNS ÄR IFYLLD PÅ 4622-BILDEN!               
124200         MOVE MID-IDDC-REC TO W-IDDC-REC                                  
124300       END-IF                                                             
124400     END-IF                                                               
124500                                                                          
124600     IF KEYS-WRONG                                                        
124700       STRING 'IDSHIPM IS MISSING/WRONG'                                  
124800            DELIMITED BY SIZE INTO ERRTEXT                                
124900       CALL FELLOG                                                        
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300 C-LOAD-BASIC-DATA SECTION.                                               
125400     MOVE 'C'                     TO WS-SEC                               
125500                                                                          
125600     MOVE NOO                     TO ALLT-SW                              
125700                                     SKRIV-DOK-SW                         
125800     PERFORM IMS-GU-WDE101                                                
125900     IF SEGMENT-FOUND                                                     
126000       MOVE YES                   TO ALLT-SW                              
126100       MOVE SHIP-IDDC             TO W-IDDC-B6                            
126200*                                                                         
126300*---   KOLLA OM 'STUDS' FLÖDE: - REFILL EXPORT                            
126400*            --> DOK. SKAPAS FRÅN 'STUDS'DC'T (=DC11)                     
126500*                                                                         
126600*                              - IMPORTÖRSFLÖDE                           
126700*                                DCXX (EJ VCC) -> IMPORTÖR (VCC)          
126800*            --> DOK. SKAPAS FRÅN 'STUDS'DC'T (=DC11)                     
126900*                                                                         
127000*                              - VOR DC11 -> CN/IN                        
127100*            --> DOK. SKAPAS FRÅN 'STUDS'DC'T (EJ DC11)                   
127200*                                                                         
127300*  OBS: I DESSA FALL SKAPAS DOK. EFTER ANDRA FAKT.                        
127400*---                                                                      
127500       IF SHIP-IDDC-EXP > SPACE                                           
127600         IF SHIP-KDFAKSTA-EXP = '2'                                       
127700           MOVE SHIP-IDDC-EXP     TO W-IDDC-B6                            
127800           MOVE YES               TO SKRIV-DOK-SW                         
127900*                                                                         
128000*          SPEC. FÖR IMPORTÖRSFLÖDET FRÅN DUBAI (DC.87)                   
128100           IF SHIP-IDDC = WC-NDC-AE                                       
128200             MOVE YES             TO SKRIV-DOK-D-SW                       
128300           END-IF                                                         
128400         END-IF                                                           
128500       END-IF                                                             
128600*                                                                         
128700       PERFORM IMS-GU-WDB601                                              
128800       IF DCS-FLWEBDC = YES OR JA                                         
128900         MOVE YES                 TO FLWEBDC-SW                           
129000       END-IF                                                             
129100     END-IF                                                               
129200     .                                                                    
129300     EJECT                                                                
129400 D-INIT-KEYS SECTION.                                                     
129500     MOVE 'D'                     TO WS-SEC                               
129600                                                                          
129700     MOVE SGMT-IDDISTR       TO  W-WDB201-IDDISTR                         
129800                                 W-WDB301-IDDISTR                         
129900                                 W-WDB301-IDDISTR-MIN                     
130000                                 W-WDB301-IDDISTR-MAX                     
130100                                 W-WDE111-IDDISTR-N                       
130200                                 W-WDB901-IDDISTR-MIN                     
130300                                 W-WDB901-IDDISTR-MAX                     
130400                                 W-WDB901-IDDISTR-MIN2                    
130500                                 W-WDB901-IDDISTR-MAX2                    
130600                                                                          
130700     MOVE SGMT-IDKUNDNR      TO  W-WDB201-IDKUNDNR                        
130800                                 W-WDB301-IDKUNDNR                        
130900                                 W-WDB901-IDKUNDNR-MIN2                   
131000                                 W-WDB901-IDKUNDNR-MAX2                   
131100                                                                          
131200     MOVE SGMT-IDDC          TO  W-WDB301-IDDC                            
131300                                 W-WDB301-IDDC-MIN                        
131400                                 W-WDB301-IDDC-MAX                        
131500                                 W-WDB901-IDDC-MIN                        
131600                                 W-WDB901-IDDC-MAX                        
131700                                 W-WDB901-IDDC-MIN2                       
131800                                 W-WDB901-IDDC-MAX2                       
131900     IF SKRIV-OK                                                          
132000       MOVE SHIP-IDDC-EXP    TO  W-WDB301-IDDC                            
132100                                 W-WDB301-IDDC-MIN                        
132200                                 W-WDB301-IDDC-MAX                        
132300                                 W-WDB901-IDDC-MIN                        
132400                                 W-WDB901-IDDC-MAX                        
132500                                 W-WDB901-IDDC-MIN2                       
132600                                 W-WDB901-IDDC-MAX2                       
132700                                                                          
132800*--    IMPORTÖRSFLÖDET FRÅN DUBAI SKALL HA LEV.DC FÖR WDB9                
132900       IF SHIP-IDDC     = WC-NDC-AE AND                                   
133000          SHIP-IDDC-EXP = WC-CDC-SE                                       
133100         MOVE SHIP-IDDC      TO  W-WDB901-IDDC-MIN                        
133200                                 W-WDB901-IDDC-MIN                        
133300                                 W-WDB901-IDDC-MAX                        
133400                                 W-WDB901-IDDC-MIN2                       
133500                                 W-WDB901-IDDC-MAX2                       
133600       END-IF                                                             
133700     END-IF                                                               
133800     .                                                                    
133900     EJECT                                                                
134000 S01-GET-DIST-IDPRTLST SECTION.                                           
134100     MOVE 'S01'                   TO WS-SEC                               
134200                                                                          
134300     PERFORM S01A-CHECK-NEW                                               
134400     IF READ-IDPRLST                                                      
134500       PERFORM S01B-GET-DIST-IDPRTLST                                     
134600       IF W-IDDC-REC > SPACE                                              
134700         PERFORM S01C-HANDLE-RESTART                                      
134800       END-IF                                                             
134900     END-IF                                                               
135000     .                                                                    
135100     EJECT                                                                
135200                                                                          
135300 S01A-CHECK-NEW SECTION.                                                  
135400     MOVE 'S01A'                   TO WS-SEC                              
135500                                                                          
135600     MOVE NOO                      TO READ-IDPRLST-SW                     
135700                                                                          
135800     MOVE SGMT-IDDC                TO W-IDDC                              
135900                                                                          
136000     IF SKRIV-OK                                                          
136100       MOVE SHIP-IDDC-EXP          TO W-IDDC                              
136200     END-IF                                                               
136300                                                                          
136400     MOVE SGMT-IDDISTR             TO W-IDDISTR                           
136500     IF DCS-DDC                                                           
136600       MOVE VORD-IDLEVNR           TO W-IDLEVNR                           
136700       MOVE ZERO                   TO W-IDKUNDNR                          
136800     ELSE                                                                 
136900       MOVE SGMT-IDKUNDNR          TO W-IDKUNDNR                          
137000       MOVE SPACE                  TO W-IDLEVNR                           
137100     END-IF                                                               
137200                                                                          
137300     IF  W-IDDC            = W-IDDC-PREV                                  
137400     AND W-IDDISTR         = W-IDDISTR-PREV                               
137500     AND W-IDLEVNR         = W-IDLEVNR-PREV                               
137600     AND W-IDKUNDNR        = W-IDKUNDNR-PREV                              
137700       CONTINUE                                                           
137800     ELSE                                                                 
137900       MOVE YES                    TO READ-IDPRLST-SW                     
138000     END-IF                                                               
138100                                                                          
138200     MOVE W-IDDC                   TO W-IDDC-PREV                         
138300     MOVE W-IDDISTR                TO W-IDDISTR-PREV                      
138400     MOVE W-IDLEVNR                TO W-IDLEVNR-PREV                      
138500     MOVE W-IDKUNDNR               TO W-IDKUNDNR-PREV                     
138600     .                                                                    
138700     EJECT                                                                
138800 S01B-GET-DIST-IDPRTLST  SECTION.                                         
138900     MOVE 'S01B'                  TO WS-SEC                               
139000                                                                          
139100     MOVE +1                       TO INDX                                
139200     MOVE SPACE                    TO TAB                                 
139300     IF W-IDLEVNR > SPACE                                                 
139400       MOVE SPACE                  TO W-WDB901-IDKUND-MIN2                
139500       MOVE SPACE                  TO W-WDB901-IDKUND-MAX2                
139600       MOVE W-IDLEVNR              TO W-WDB901-IDLEVNR-MIN2               
139700       MOVE W-IDLEVNR              TO W-WDB901-IDLEVNR-MAX2               
139800     END-IF                                                               
139900                                                                          
140000*                                                                         
140100* IMP.FLÖDET SKALL HA LEVERERANDE IDDC I NYCKLARNA                        
140200*                                                                         
140300     MOVE W-IDDISTR                TO TEST-IDDISTR                        
140400     IF SHIP-IDDC-EXP = WS-IDDC-11                                        
140500       IF DIST35-NONVCC-NONVCC-REFILL  OR                                 
140510          DIST35-NONVCC-NONVCC-TRANSFER                                   
140600         CONTINUE                                                         
140700       ELSE                                                               
140800         MOVE SHIP-IDDC            TO W-WDB901-IDDC-MIN                   
140900                                      W-WDB901-IDDC-MAX                   
141000                                      W-WDB901-IDDC-MIN2                  
141100                                      W-WDB901-IDDC-MAX2                  
141200       END-IF                                                             
141300     END-IF                                                               
141400*                                                                         
141500     PERFORM IMS-GU-WDB901                                                
141600                                                                          
141700     PERFORM UNTIL  NOT SEGMENT-FOUND                                     
141800                                                                          
141900       IF  DOK-IDKUND > SPACE                                             
142000       AND TAB-IDKUND = SPACE                                             
142100*........EXEPTIONS EXISTS, DEFAULT VALUES NOT USED                        
142200         MOVE +1                   TO INDX                                
142300       END-IF                                                             
142400                                                                          
142500       MOVE DOK-IDKUND             TO TAB-IDKUND                          
142600       MOVE DOK-IDPRTLST           TO TAB-IDPRTLST      (INDX)            
142700       MOVE DOK-KVCOPIES-GMTL      TO TAB-KVCOPIES-GMTL (INDX)            
142800       MOVE DOK-KVCOPIES-KLIS      TO TAB-KVCOPIES-KLIS (INDX)            
142900       MOVE DOK-KVCOPIES-PACK      TO TAB-KVCOPIES-PACK (INDX)            
143000       MOVE DOK-KVCOPIES-SPED      TO TAB-KVCOPIES-SPED (INDX)            
143100       MOVE DOK-KVCOPIES-STAT      TO TAB-KVCOPIES-STAT (INDX)            
143200       MOVE DOK-KVCOPIES-VERS      TO TAB-KVCOPIES-VERS (INDX)            
143300       MOVE DOK-KVCOPIES-NAPR      TO TAB-KVCOPIES-NAPR (INDX)            
143400       MOVE DOK-KVCOPIES-KULB      TO TAB-KVCOPIES-KULB (INDX)            
143500       MOVE DOK-KVCOPIES-BLAD      TO TAB-KVCOPIES-BLAD (INDX)            
143600       MOVE DOK-KVCOPIES-TRPT      TO TAB-KVCOPIES-TRPT (INDX)            
143700*      MOVE ZERO                   TO TAB-KVCOPIES-BLAD (INDX)            
143800       MOVE 1                      TO TAB-KVCOPIES-MANF (INDX)            
143900       MOVE ZERO                   TO TAB-KVCOPIES-SASO (INDX)            
144000       MOVE DOK-IDDC-REC           TO TAB-IDDC-REC      (INDX)            
144100                                                                          
144200       MOVE SGMT-IDDISTR          TO TEST-IDDISTR                         
144300       IF DIST35-NONVCC-NONVCC-REFILL                                     
144310       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
144400       OR DIST35-CDC-AE-REFILL                                            
144410       OR DIST35-AE-CDC-RETURNS                                           
144420       OR ((SHIP-IDDC = WC-NDC-AE)    AND                                 
144430           (DIST18-SCRAP-NDC-SC       OR                                  
144440            DIST18-SCRAP-NDC-QUAL     OR                                  
144450            DIST18-SCRAP-NDC-SC-LOCAL OR                                  
144460            DIST20-EMBALLAGE-SDC ))                                       
144500         CONTINUE                                                         
144600       ELSE                                                               
144700         IF SKRIV-OK                                                      
144800*---     VOR DC11 -> CN/IN                                                
144900           CONTINUE                                                       
145000         ELSE                                                             
145100           PERFORM S01BB-W40634                                           
145200           PERFORM S05-CHECK-SASO-MANF                                    
145300         END-IF                                                           
145400       END-IF                                                             
145500                                                                          
145600       ADD +1                      TO INDX                                
145700                                                                          
145800       IF INDX > INDX-MAX2                                                
145900*........TABLE OVERFLOW                                                   
146000         MOVE DOK-IDDISTR          TO W-IDDISTR-DISP                      
146100         STRING 'MORE THAN 9 SEGMENTS IN WDB9:'                           
146200            DOK-IDDC ' ' W-IDDISTR-DISP ' ' DOK-IDKUND                    
146300               DELIMITED BY SIZE INTO ERRTEXT                             
146400         CALL FELLOG                                                      
146500       END-IF                                                             
146600                                                                          
146700       PERFORM IMS-GN-WDB901                                              
146800     END-PERFORM                                                          
146900*    PERFORM S01BA-USA                                                    
147000     COMPUTE INDX-MAX =  INDX - 1                                         
147100     .                                                                    
147200     EJECT                                                                
147300 S01BA-USA   SECTION.                                                     
147400     MOVE SGMT-IDDISTR            TO TEST-IDDISTR                         
147500     IF DIST35-NA-CDC-RETURN OR                                           
147600        DIST35-NA-TRANSFER OR                                             
147700        DIST18-SCRAP-NDC-SC  OR                                           
147800        DIS134-BYTESREN-NA                                                
147900       MOVE SPACE           TO TAB-IDKUND                                 
148000       MOVE 'REPRINT'       TO TAB-IDPRTLST      (INDX)                   
148100       MOVE ZERO            TO TAB-KVCOPIES-GMTL (INDX)                   
148200       MOVE ZERO            TO TAB-KVCOPIES-KLIS (INDX)                   
148300       MOVE ZERO            TO TAB-KVCOPIES-PACK (INDX)                   
148400       MOVE ZERO            TO TAB-KVCOPIES-SPED (INDX)                   
148500       MOVE ZERO            TO TAB-KVCOPIES-STAT (INDX)                   
148600       MOVE ZERO            TO TAB-KVCOPIES-VERS (INDX)                   
148700       MOVE 1               TO TAB-KVCOPIES-NAPR (INDX)                   
148800       MOVE 1               TO TAB-KVCOPIES-KULB (INDX)                   
148900       MOVE 0               TO TAB-KVCOPIES-BLAD (INDX)                   
149000       MOVE 0               TO TAB-KVCOPIES-TRPT (INDX)                   
149100       MOVE 0               TO TAB-KVCOPIES-MANF (INDX)                   
149200       MOVE 0               TO TAB-KVCOPIES-SASO (INDX)                   
149300       MOVE '11'            TO TAB-IDDC-REC      (INDX)                   
149400        ADD 1               TO INDX                                       
149500     END-IF                                                               
149600     .                                                                    
149700     EJECT                                                                
149800 S01BB-W40634  SECTION.                                                   
149900*   FRÅN W40634 SKALL BARA 'NAPR' SKRIVAS                                 
150000*   FRÅN W40634 SKALL BARA 'SASO' OCH 'MANF' SKRIVAS                      
150100*-- OM INTE REFILL-EXP ELLER VOR DC11->CN/IN                              
150200                                                                          
150300     IF MID-IDPGM = 'W4063400'                                            
150400       MOVE ZERO            TO TAB-KVCOPIES-GMTL (INDX)                   
150500       MOVE ZERO            TO TAB-KVCOPIES-KLIS (INDX)                   
150600       MOVE ZERO            TO TAB-KVCOPIES-PACK (INDX)                   
150700       MOVE ZERO            TO TAB-KVCOPIES-SPED (INDX)                   
150800       MOVE ZERO            TO TAB-KVCOPIES-STAT (INDX)                   
150900       MOVE ZERO            TO TAB-KVCOPIES-VERS (INDX)                   
151000*      MOVE 1               TO TAB-KVCOPIES-NAPR (INDX)                   
151100       MOVE ZERO            TO TAB-KVCOPIES-KULB (INDX)                   
151200*      MOVE ZERO            TO TAB-KVCOPIES-BLAD (INDX)                   
151300       MOVE ZERO            TO TAB-KVCOPIES-TRPT (INDX)                   
151400       MOVE 1               TO TAB-KVCOPIES-MANF (INDX)                   
151500       MOVE ZERO            TO TAB-KVCOPIES-SASO (INDX)                   
151600     END-IF                                                               
151700     .                                                                    
151800     EJECT                                                                
151900 S01C-HANDLE-RESTART SECTION.                                             
152000     MOVE 'S01C'                  TO WS-SEC                               
152100                                                                          
152200*....READ GIVEN IDDC-REC ONLY =IDDC WHERE THE PRINTER IS LOCATED          
152300                                                                          
152400     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > INDX-MAX             
152500       IF TAB-IDDC-REC (INDX) =    W-IDDC-REC                             
152600         MOVE TAB-ROW (INDX)       TO TAB-ROW (+1)                        
152700         MOVE INDX-MAX2            TO INDX                                
152800       END-IF                                                             
152900     END-PERFORM                                                          
153000                                                                          
153100     IF TAB-IDDC-REC(+1) = W-IDDC-REC                                     
153200       MOVE +1                     TO INDX-MAX                            
153300     ELSE                                                                 
153400*......NO DATA FOR GIVEN IDDC-REC                                         
153500       MOVE ZERO                   TO INDX-MAX                            
153600     END-IF                                                               
153700     .                                                                    
153800     EJECT                                                                
153900 S03-OPEN-PRT SECTION.                                                    
154000     MOVE 'S03'                   TO WS-SEC                               
154100                                                                          
154200     IF IDPRTLST-OPEN                                                     
154300       CONTINUE                                                           
154400     ELSE                                                                 
154500       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
154600                           PRT-OPEN                                       
154700                           W-IDPRTLST                                     
154800                           ALT-PCB                                        
154900                           DUMMY-AREA                                     
155000                           DUMMY-AREA                                     
155100                                                                          
155200       MOVE YES         TO IDPRTLST-OPEN-SW                               
155300     END-IF                                                               
155400     .                                                                    
155500     EJECT                                                                
155600 S04-CHECK-IDLEVNR SECTION.                                               
155700     MOVE 'S04'                    TO WS-SEC                              
155800                                                                          
155900     MOVE SPACE                    TO W-IDLEVNR                           
156000     MOVE SGMT-IDDC                TO W-IDDC                              
156100                                                                          
156200     IF SKRIV-OK                                                          
156300       MOVE SHIP-IDDC-EXP          TO W-IDDC                              
156400     END-IF                                                               
156500                                                                          
156600     MOVE SPACE                    TO VORD-IDLEVNR                        
156700                                                                          
156800     IF DCS-DDC                                                           
156900       PERFORM IMS-GNP-WDE121                                             
157000       IF SEGMENT-FOUND                                                   
157100         MOVE SKOLLI-IDPRODNR      TO W-IDPRODNR                          
157200         PERFORM IMS-GU-WDE601                                            
157300       END-IF                                                             
157400     END-IF                                                               
157500     .                                                                    
157600     EJECT                                                                
157700 S05-CHECK-SASO-MANF SECTION.                                             
157800     MOVE 'S05'                    TO WS-SEC                              
157900     MOVE SGMT-IDDISTR             TO TEST-IDDISTR                        
158000     IF DIST10-LEVBIL-KUWAIT OR                                           
158100        DIST10-LEVBIL-BAREIN OR                                           
158200        DIST10-LEVBIL-SYRIEN OR                                           
158300        DIST10-LEVBIL-SAUDI-PV                                            
158400        IF TAB-KVCOPIES-BLAD (INDX) > 0                                   
158500         MOVE TAB-KVCOPIES-BLAD(INDX) TO TAB-KVCOPIES-MANF(INDX)          
158600        END-IF                                                            
158700     END-IF                                                               
158800     .                                                                    
158900     EJECT                                                                
159000 S11-RECV-OPEN SECTION.                                                   
159100     MOVE 'S11'                   TO WS-SEC                               
159200                                                                          
159300     MOVE 'OPEN'                   TO RECV-KDFUNC                         
159400     MOVE 'CARPARTS.PULS.SHIPDOK'  TO RECV-ADDISPABS                      
159500                                                                          
159600     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
159700                                   RECV-OPEN-AREA                         
159800                                                                          
159900     IF RECV-KDRC > 0                                                     
160000      MOVE RECV-KDRC               TO KDRC-DISP                           
160100      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
160200        DELIMITED BY SIZE INTO ERRTEXT                                    
160300      CALL FELLOG                                                         
160400     END-IF                                                               
160500     .                                                                    
160600     EJECT                                                                
160700                                                                          
160800 S12-RECV-MESSAGE SECTION.                                                
160900     MOVE 'S12'                   TO WS-SEC                               
161000                                                                          
161100     MOVE 'GET'                    TO RECV-KDFUNC                         
161200     MOVE LENGTH OF MID-W4I63101   TO RECV-KVDLEN                         
161300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
161400                         RECV-KVDLEN                                      
161500                         MID-W4I63101                                     
161600                                                                          
161700     IF RECV-KDRC > 1                                                     
161800       MOVE RECV-KDRC            TO KDRC-DISP                             
161900       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
162000         DELIMITED BY SIZE INTO ERRTEXT                                   
162100       CALL FELLOG                                                        
162200     END-IF                                                               
162300     .                                                                    
162400     EJECT                                                                
162500                                                                          
162600 S13-RECV-CLOSE SECTION.                                                  
162700     MOVE 'S13'                  TO WS-SEC                                
162800                                                                          
162900     MOVE 'CLOSE'                TO RECV-KDFUNC                           
163000     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
163100                                                                          
163200     IF RECV-KDRC > 0                                                     
163300       MOVE RECV-KDRC            TO KDRC-DISP                             
163400       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
163500         DELIMITED BY SIZE INTO ERRTEXT                                   
163600       CALL FELLOG                                                        
163700     END-IF                                                               
163800     .                                                                    
163900     EJECT                                                                
164000                                                                          
164100 S90-OPEN-SEND-ONDEM SECTION.                                             
164200     MOVE 'S90-OPEN-ONDEM' TO CURR-SECTION                                
164300*    -- DO NOT PRINT TO ONDEMAND WHEN REQUEST FROM W4602200               
164400     IF MID-IDPGM = 'W4063600' OR 'W4063400'                              
164500       IF DOK-FLSKRIV-ONDEM = YES                                         
164600         MOVE 'OPEN'                    TO SEND-KDFUNC                    
164700         MOVE 'CARPARTS.DAP.DISTRDOC'   TO SEND-ADDISPABS                 
164800         CALL WZ01SEND USING SEND-CONTROL-AREA                            
164900                             SEND-OPEN-AREA                               
165000         IF SEND-KDRC > 0                                                 
165100           MOVE SEND-KDRC TO KDRC-DISPLAY                                 
165200           STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                  
165300           DELIMITED BY SIZE INTO ERRTEXT                                 
165400           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
165500         END-IF                                                           
165600       END-IF                                                             
165700     END-IF                                                               
165800     .                                                                    
165900     EJECT                                                                
166000 S90-OPEN-SEND-WEB SECTION.                                               
166100     MOVE 'S90-OPEN-WEB'   TO CURR-SECTION                                
166200                                                                          
166300     MOVE 'CARPARTS.DAP.DISTRDOC'    TO SEND-ADDISPABS                    
166400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
166500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
166600                         SEND-OPEN-AREA                                   
166700     IF SEND-KDRC > ZERO                                                  
166800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
166900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
167000       DELIMITED BY SIZE INTO ERRTEXT                                     
167100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
167200     END-IF                                                               
167300     .                                                                    
167400     EJECT                                                                
167500 S90-PUT-DAPHDR-ONDEM SECTION.                                            
167600     MOVE 'S90-PUT-DAPHDR-ONDEM' TO CURR-SECTION                          
167700                                                                          
167800*    -- DO NOT PRINT TO ONDEMAND WHEN REQUEST FROM W4062200               
167900     IF MID-IDPGM = 'W4063600' OR 'W4063400'                              
168000       IF DOK-FLSKRIV-ONDEM = YES                                         
168100         MOVE 001                 TO REQU-IDMSGVER                        
168200         MOVE SPACE               TO REQU-KDPGMACT                        
168300         MOVE IDPGM               TO REQU-IDUSER                          
168400                                                                          
168500         MOVE W-IDDC              TO WS-IDDC-HDR                          
168600         MOVE W-IDDISTR           TO WS-IDDISTR-HDR                       
168700         MOVE WS-HEADER           TO HDR-IDOUTREC                         
168800                                                                          
168900         MOVE W-IDSHIPM           TO HDR-IDLIST                           
169000                                                                          
169100         MOVE +1                  TO SEND-IDCOM                           
169200         MOVE 'PUT'               TO SEND-KDFUNC                          
169300         MOVE LENGTH OF HDR-AREA  TO SEND-KVDLEN                          
169400                                                                          
169500         CALL WZ01SEND USING SEND-CONTROL-AREA                            
169600                             SEND-KVDLEN                                  
169700                             HDR-AREA                                     
169800         IF SEND-KDRC > ZERO                                              
169900           MOVE SEND-KDRC         TO KDRC-DISPLAY                         
170000           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
170100           DELIMITED BY SIZE INTO ERRTEXT-STR                             
170200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
170300         END-IF                                                           
170400       END-IF                                                             
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800 S90-PUT-DAPHDR-WEB SECTION.                                              
170900     MOVE 'S90-PUT-DAPHDR-WEB' TO CURR-SECTION                            
171000                                                                          
171100     MOVE 001                 TO REQU-IDMSGVER                            
171200     MOVE SPACE               TO REQU-KDPGMACT                            
171300     MOVE IDPGM               TO REQU-IDUSER                              
171400                                                                          
171500     MOVE SPACE               TO HDR-IDOUTREC                             
171600     MOVE W-IDDC              TO HDR-IDOUTREC(1:2)                        
171700     MOVE SPACE               TO HDR-IDOUTREC(3:8)                        
171800                                                                          
171900     MOVE +1                  TO SEND-IDCOM                               
172000     MOVE 'PUT'               TO SEND-KDFUNC                              
172100     MOVE LENGTH OF HDR-AREA  TO SEND-KVDLEN                              
172200                                                                          
172300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
172400                         SEND-KVDLEN                                      
172500                         HDR-AREA                                         
172600     IF SEND-KDRC > ZERO                                                  
172700       MOVE SEND-KDRC         TO KDRC-DISPLAY                             
172800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
172900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
173000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
173100     END-IF                                                               
173200     .                                                                    
173300     EJECT                                                                
173400 S90-CLOSE-SEND-WEB-ONDEM SECTION.                                        
173500     MOVE 'S90-CLOSE-SEND-W-O' TO CURR-SECTION                            
173600*    -- FOR CLASSIC DC:S NOTHING IS PRINTED TO ONDEMAND                   
173700*    -- WHEN REQUEST FROM W40622, ONLY FROM W40634 OR W40636,             
173800*    -- SO DAP-SENDING IS THEN NOT OPEN AND SHOULD NOT BE CLOSED.         
173900*    -- BUT IF THE DC IS A WEB DC AND THE REPORT HAS A WEB                
174000*    -- VERSION, IT IS OPEN AND MUST BE CLOSED HERE.                      
174100                                                                          
174200     IF (FLWEBDC AND WEB-REPORT) OR                                       
174300       ((MID-IDPGM = 'W4063600' OR 'W4063400')                            
174400         AND DOK-FLSKRIV-ONDEM = YES)                                     
174500         MOVE 'CLOSE'             TO SEND-KDFUNC                          
174600         CALL WZ01SEND USING SEND-CONTROL-AREA                            
174700                                                                          
174800         IF SEND-KDRC > 0                                                 
174900           MOVE SEND-KDRC TO KDRC-DISPLAY                                 
175000           STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                 
175100           DELIMITED BY SIZE INTO ERRTEXT                                 
175200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
175300         END-IF                                                           
175400     END-IF                                                               
175500     .                                                                    
175600     EJECT                                                                
175700* --- IMS SECTIONS ---                                                    
175800     SKIP3                                                                
175900                                                                          
176000 IMS-GU-WDE101 SECTION.                                                   
176100     MOVE '010'                   TO WS-SEC                               
176200                                                                          
176300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
176400          DELIMITED BY SIZE INTO SSA1                                     
176500     MOVE '  GE' TO GOOD-STATUSCODES                                      
176600     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
176700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
176800     PERFORM IMS-STATUSCHECK                                              
176900     .                                                                    
177000     EJECT                                                                
177100                                                                          
177200 IMS-GHU-WDE101 SECTION.                                                  
177300     MOVE '020'                   TO WS-SEC                               
177400                                                                          
177500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
177600          DELIMITED BY SIZE INTO SSA1                                     
177700     MOVE '  GE' TO GOOD-STATUSCODES                                      
177800     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
177900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
178000     PERFORM IMS-STATUSCHECK                                              
178100     .                                                                    
178200     EJECT                                                                
178300 IMS-REPL-WDE101 SECTION.                                                 
178400     MOVE '030'                   TO WS-SEC                               
178500                                                                          
178600     MOVE '    ' TO GOOD-STATUSCODES                                      
178700     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
178800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
178900     PERFORM IMS-STATUSCHECK                                              
179000     .                                                                    
179100     EJECT                                                                
179200                                                                          
179300 IMS-GNP-WDE111 SECTION.                                                  
179400     MOVE '040'                   TO WS-SEC                               
179500                                                                          
179600     MOVE 'WDE111  ' TO SSA1                                              
179700     MOVE '  GE' TO GOOD-STATUSCODES                                      
179800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
179900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSCHECK                                              
180100     .                                                                    
180200     EJECT                                                                
180300 IMS-GNP-WDE111-DIST SECTION.                                             
180400     MOVE '050'                   TO WS-SEC                               
180500                                                                          
180600     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-N-X ')'                      
180700          DELIMITED BY SIZE INTO SSA1                                     
180800     MOVE '  GE' TO GOOD-STATUSCODES                                      
180900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
181000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
181100     PERFORM IMS-STATUSCHECK                                              
181200     .                                                                    
181300     EJECT                                                                
181400 IMS-GNP-WDE121 SECTION.                                                  
181500     MOVE '060'                   TO WS-SEC                               
181600                                                                          
181700     MOVE 'WDE121  ' TO SSA1                                              
181800     MOVE '  GE' TO GOOD-STATUSCODES                                      
181900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1                   
182000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
182100     PERFORM IMS-STATUSCHECK                                              
182200     .                                                                    
182300     EJECT                                                                
182400 IMS-GU-WDE601          SECTION.                                          
182500     MOVE '070'                   TO WS-SEC                               
182600                                                                          
182700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
182800            DELIMITED BY SIZE INTO SSA1                                   
182900     MOVE '  ' TO GOOD-STATUSCODES                                        
183000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
183100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSCHECK                                              
183300     .                                                                    
183400     EJECT                                                                
183500 IMS-GU-WDB601          SECTION.                                          
183600     MOVE '080'                   TO WS-SEC                               
183700                                                                          
183800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
183900            DELIMITED BY SIZE INTO SSA1                                   
184000     MOVE '  ' TO GOOD-STATUSCODES                                        
184100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
184200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
184300     PERFORM IMS-STATUSCHECK                                              
184400     .                                                                    
184500     EJECT                                                                
184600 IMS-GU-WDB901 SECTION.                                                   
184700     MOVE '090'                   TO WS-SEC                               
184800                                                                          
184900     STRING 'WDB901  (WDB901KY>=' W-WDB901KY-MIN2                         
185000                    '&WDB901KY<=' W-WDB901KY-MAX2                         
185100                    '+WDB901KY>=' W-WDB901KY-MIN                          
185200                    '&WDB901KY<=' W-WDB901KY-MAX  ')'                     
185300                   DELIMITED BY SIZE INTO SSA1                            
185400                                                                          
185500     MOVE '  GE'                   TO GOOD-STATUSCODES                    
185600     CALL CBLTDLI USING GU WDB9-PCB DLI-IO-WDB901 SSA1                    
185700     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
185800     PERFORM IMS-STATUSCHECK                                              
185900     .                                                                    
186000     EJECT                                                                
186100 IMS-GN-WDB901 SECTION.                                                   
186200     MOVE '100'                   TO WS-SEC                               
186300                                                                          
186400     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
186500     CALL CBLTDLI USING GN WDB9-PCB DLI-IO-WDB901 SSA1                    
186600     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
186700     PERFORM IMS-STATUSCHECK                                              
186800     .                                                                    
186900     EJECT                                                                
187000 IMS-STATUSCHECK SECTION.                                                 
187100                                                                          
187200     SET STATUS-IX TO 1                                                   
187300     SEARCH GOOD-STATUS                                                   
187400       AT END                                                             
187500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
187600         DELIMITED BY SIZE INTO ERRTEXT                                   
187700         CALL FELLOG                                                      
187800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
187900         CONTINUE                                                         
188000     END-SEARCH                                                           
188100     .                                                                    
