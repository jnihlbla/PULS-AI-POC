000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL012900.                                                
000400 AUTHOR.       TAPAS KUMAR GHOSH.                                         
000500 DATE-WRITTEN.   JUN  2004.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    WL012900 PROGRAM IS A REPLICA OF W4034100 PROGRAM                    
001100*    AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                             
001200*                                                                         
001300*    VAL 'U' VID UTSKRIFT AV FÖLJESEDEL SKALL INTE GE                     
001400*    NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                              
001500*    FÖR ATT HITTA PRIOBERÄKN. SÖK MED "*PRIO".                           
001600*                                                                         
001700*    FUNKTION.                                                            
001800*        PROGRAMMET SKRIVER FÖLJESEDEL.                                   
001900*                                                                         
002000*                                                                         
002100*        ADDRESS: CARPARTS.LDC.PRDELNOTE                                  
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: WL0129T                                             
002500*        REQUEST    : WZ01REQU                                            
002600*                     WL0129I1                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        RESPONSE   : WZ01RESP                                            
003000*                     WL0129O1                                            
003100*                                                                         
003200*                                                                         
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP3                                                                
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)   VALUE 'WL012900'.             
004200 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004600 77  JA                          PIC X      VALUE 'J'.                    
004700 77  NEJ                         PIC X      VALUE 'N'.                    
004800 77  WS-RECORD-NEXT-SW           PIC 9(1)   VALUE ZERO.                   
004900 77  INDX                        PIC S9(9)  VALUE +0   COMP SYNC.         
005000 77  WS-CNT                      PIC S9(9)  VALUE +0   COMP SYNC.         
005100 77  MAX-CNT-REC                 PIC S9(9)  VALUE +50  COMP SYNC.         
005200 77  FK-INDX                     PIC S9(9)  VALUE +0   COMP SYNC.         
005300 77  MAX-FK-INDX                 PIC S9(9)  VALUE +6   COMP SYNC.         
005400 77  WS-IDDC-KEY                 PIC X(2)   VALUE SPACE.                  
005500 77  WS-IDDISTR                  PIC X(4)   VALUE SPACE.                  
005600 77  WS-IDKUNDNR                 PIC X(6)   VALUE SPACE.                  
005700 77  WS-IDORDNR                  PIC X(5)   VALUE SPACE.                  
005800 77  WS-IDPRODNR-E420F           PIC 9(7)   VALUE ZERO.                   
005900 77  WS-IDKOLLI-PRT              PIC 9(5)   VALUE ZERO.                   
006000 77  WS-IDKOLLI                  PIC X(5)   VALUE SPACE.                  
006100 77  WS-IDKOLLI-TOM              PIC 9(5)   VALUE ZERO.                   
006200 77  WS-IDKOLLI-NUM              PIC 9(5)   VALUE ZERO.                   
006300 77  WS-IDKOLLI-TEST-MN          PIC 9(5)   VALUE ZERO.                   
006400 77    FILLER                    PIC X(8)   VALUE 'AAAAAAAA'.             
006500 77  W-IDDISTR                   PIC S9(5)  VALUE ZERO COMP-3.            
006600 77  W-IDKUNDNR                  PIC S9(7)  VALUE ZERO COMP-3.            
006700 77  W-IDKOLLI                   PIC S9(5)  VALUE ZERO COMP-3.            
006800 77  W-IDKOLLI-TOM               PIC S9(5)  VALUE ZERO COMP-3.            
006900 77  WS-SUM-ART                  PIC S9(5)  VALUE +0   COMP-3.            
007000 77  WS-SUM-LEV                  PIC S9(11) VALUE +0   COMP-3.            
007100 77  WS-KDFRAKT                  PIC S9(3)  VALUE +0   COMP-3.            
007200 77  WS-FL-SVENSK-FSEDEL         PIC X.                                   
007300 77  WS-VKORDBTO                 PIC S9(4)V9(3) VALUE 0.                  
007400 77  WS-VLORDBTO                 PIC S9(6)V9(3) VALUE 0.                  
007500 77  WS-RATT-PRODNR              PIC X      VALUE SPACE.                  
007600 77  WS-VORD-TIUTSKR             PIC S9(7)  VALUE +0   COMP-3.            
007700 77  WS-VORD-TIUTSTID            PIC S9(7)  VALUE +0   COMP-3.            
007800 01  WS-IDLIST.                                                           
007900     03  WS-CURRENT-TIME         PIC 9(8)    VALUE ZERO.                  
008000     03  WS-SEQNO                PIC 9(2)    VALUE ZERO.                  
008100                                                                          
008200*    --- PARAMETERS TO ABEND                                              
008300                                                                          
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700*************************************************************             
008800 01  WS-IDPLOCK-EDIT             PIC Z(7)9.                               
008900 01  WS-IDPLOCK-GRP              PIC 9(7).                                
009000 01  FILLER REDEFINES WS-IDPLOCK-GRP.                                     
009100     03  WS-IDPLOCK-PREFIX       PIC 9(2).                                
009200     03  WS-IDPLOCK              PIC 9(5).                                
009300                                                                          
009400 01  WS-TIME-HHMMSS              PIC 9(06).                               
009500 01  FILLER REDEFINES WS-TIME-HHMMSS.                                     
009600     03  WS-TIME-HOUR            PIC 9(02).                               
009700     03  WS-TIME-MINUTE          PIC 9(02).                               
009800     03  WS-TIME-SEC             PIC 9(02).                               
009900                                                                          
010000 01  FILLER REDEFINES WS-TIME-HHMMSS.                                     
010100     03 WS-TIPACTID-HHMM         PIC 9(04).                               
010200                                                                          
010300 01  WS-TIPACTID                 PIC 9(06).                               
010400 01  FILLER REDEFINES WS-TIPACTID.                                        
010500     03  WS-TIPACTIDTIM          PIC 9(02).                               
010600     03  WS-TIPACTIDMIN          PIC 9(02).                               
010700     03  WS-TIPACTIDSEK          PIC 9(02).                               
010800                                                                          
010900 01  WS-DARFS                    PIC 9(12).                               
011000 01  FILLER REDEFINES WS-DARFS.                                           
011100     03  WS-FILLER1              PIC 9(02).                               
011200     03  WS-DARFS-DATE           PIC 9(06).                               
011300     03  WS-DARFS-TIME.                                                   
011400         05  WS-DARFS-TIM        PIC 9(02).                               
011500         05  WS-DARFS-MIN        PIC 9(02).                               
011600 77    FILLER                    PIC X(8)   VALUE 'CCCCCCCC'.             
011700                                                                          
011800 77  WS-FEL-FUNNET               PIC X      VALUE 'N'.                    
011900     88  FEL-FUNNET                         VALUE 'J'.                    
012000     88  FEL-EJ-FUNNET                      VALUE 'N'.                    
012100                                                                          
012200 77  SW-NYCKLAR-OK               PIC X      VALUE 'J'.                    
012300     88  NYCKLAR-OK                         VALUE 'J'.                    
012400     88  NYCKLAR-FEL                        VALUE 'N'.                    
012500                                                                          
012600 77  WS-KDFRAKT-TEST             PIC X(01).                               
012700     88  KDFRAKT-FINNS                      VALUE 'J'.                    
012800     88  KDFRAKT-SAKNAS                     VALUE 'N'.                    
012900                                                                          
013000 77  WS-HEADER-SKRIVEN-SW        PIC X(01)  VALUE 'N'.                    
013100     88  HEADER-EJ-SKRIVEN                  VALUE 'N'.                    
013200     88  HEADER-SKRIVEN                     VALUE 'J'.                    
013300                                                                          
013400 77  SKRIV-DELNOTE-SW            PIC X(01).                               
013500*                                NÅGON, DVS MINST EN PLOCKLISTA           
013600*                                ÄR GODKÄND.                              
013700*                                FÖLJESEDEL SKALL SKRIVAS.                
013800 77  WS-NGN-PLKLST-GODK          PIC X(1)   VALUE SPACE.                  
013900*                                NÅGON PLOCKLISTA ÄR FELAKTIG.            
014000*                                FÄLTET ANVÄNDS FÖR ATT SPARA             
014100*                                FELTEXT VID 1:A FEL.                     
014200 77  WS-NGN-PLKLST-FEL           PIC X(1)   VALUE SPACE.                  
014300                                                                          
014400 01  GENERELLA-SUBPROGRAM.                                                
014500     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
014600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
014700     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
014800     03  WINTSOR                 PIC X(8)   VALUE 'WINTSOR '.             
014900     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
015000     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
015100     03  WTRAUTF8                PIC X(8)   VALUE 'WTRAUTF8'.             
015200     SKIP2                                                                
015300*    --- PARAMETRAR TILL SUBPROGRAM                                       
015400 01  FILLER                      PIC X(16)  VALUE 'WDATAREA '.            
015500*   -COPY WDATAREA                                                        
015600     SKIP2                                                                
015700 01  FILLER                      PIC X(16)  VALUE 'WDAGAREA '.            
015800*   -COPY WDAGAREA                                                        
015900     SKIP2                                                                
016000 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
016100*   -COPY WZ01SEND                                                        
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)  VALUE 'WZ01SUB '.             
016400*   -COPY WZ01SUB                                                         
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
016700                                                                          
016800 01  REQU-AREA.                                                           
016900*    03 -COPY WZ01REQU                                                    
017000*    03 -COPY WL0129I1                                                    
017100                                                                          
017200 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
017300*01  -COPY WWDC99                                                         
017400     EJECT                                                                
017500 01  FILLER               PIC X(16)  VALUE 'WTRAUTF8-AREA   '.            
017600*01  -COPY WTRAUTF8                                                       
017700 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
017800                                                                          
017900 01  RESP-AREA.                                                           
018000*    03 -COPY WZ01RESP                                                    
018100     03 FILLER                   PIC X(100) VALUE 'FILLER'.               
018200                                                                          
018300 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
018400                                                                          
018500 01  HDR-AREA.                                                            
018600*    03 -COPY WZ01REQU                                                    
018700*    03 -COPY WZ04HDR                                                     
018800                                                                          
018900 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-HEAD'.            
019000 01  RESP-AREA-HEAD.                                                      
019100*    03 -COPY WL01291                                                     
019200                                                                          
019300 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-LINE'.            
019400 01  RESP-AREA-LINE.                                                      
019500*    03 -COPY WL01292                                                     
019600                                                                          
019700 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-TOTAL'.           
019800 01  RESP-AREA-TOTAL.                                                     
019900*    03 -COPY WL01293                                                     
020000     EJECT                                                                
020100*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
020200*                                                                         
020300 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
020400     EJECT                                                                
020500*                                                                         
020600 01  FILLER                      PIC X(16)  VALUE 'STARTTAB '.            
020700 01  TABENTRY-LNGD               PIC S9(9)  COMP.                         
020800 01  ANTAL-ENTRY                 PIC S9(9)  COMP.                         
020900 01  SORTBGP-LNGD                PIC S9(9)  COMP.                         
021000                                                                          
021100 01  IX-TAB                      PIC S9(9)  COMP-3  VALUE ZERO.           
021200 01  TAB-ANT                     PIC S9(9)  COMP-3  VALUE ZERO.           
021300 01  TAB-MAX                     PIC S9(9)  COMP-3  VALUE 1100.           
021400 01  FILLER                      PIC X(16)  VALUE 'SORT-TAB '.            
021500 01  TABELL.                                                              
021600     03  TABELL-POST    OCCURS 1100.                                      
021700         05 TAB-PURAD            PIC S9(5)  COMP-3.                       
021800         05 TAB-IDARTNR          PIC S9(9)  COMP-3.                       
021900         05 TAB-REKSIFFR         PIC S9(1)  COMP-3.                       
022000         05 TAB-IDRONR           PIC S9(5)  COMP-3.                       
022100         05 TAB-BERADREF         PIC X(10).                               
022200         05 TAB-ORG              PIC X(2).                                
022300         05 TAB-IDARTNR-ERS      PIC X(1).                                
022400         05 TAB-KVLEVART         PIC S9(7)  COMP-3.                       
022500         05 TAB-INDEX            PIC S9(5)  COMP-3.                       
022600         05 TAB-IDSYSTEM         PIC X(4).                                
022700                                                                          
022800*                                                                         
022900 01  FILLER                   PIC X(16)  VALUE 'SPAR-AREA'.               
023000 01  SPAR.                                                                
023100     03 SPAR-IDRADNR-KO          PIC S9(5)  COMP-3.                       
023200     03 SPAR-IDRONR              PIC S9(5)  COMP-3.                       
023300     03 SPAR-IDARTNR             PIC S9(9)  COMP-3.                       
023400     03 SPAR-REKSIFFR            PIC S9(1)  COMP-3.                       
023500     03 SPAR-IDRADNR             PIC S9(5)  COMP-3.                       
023600     03 SPAR-IDARTNR-ERS         PIC X(1).                                
023700     03 SPAR-KVLEVART            PIC S9(7)  COMP-3.                       
023800     03 SPAR-BEART               PIC X(25).                               
023900     03 SPAR-BERADREF            PIC X(10).                               
024000     03 SPAR-IDSYSTEM            PIC X(4).                                
024100     EJECT                                                                
024200 01  WS-IDKUNDRF.                                                         
024300     03  WS-IDORDNR-KREF         PIC 9(5).                                
024400     03  FILLER                  PIC X(5)   VALUE SPACE.                  
024500                                                                          
024600 01  WS-IDKUNDRF-RO.                                                      
024700     03  WS-IDORDNR-RO           PIC 9(5).                                
024800     03  FILLER                  PIC X(5)   VALUE SPACE.                  
024900                                                                          
025000 01  FILLER                    PIC X(16) VALUE 'TABELL-1'.                
025100 01  TABELL1.                                                             
025200     03  TABELL-POST1     OCCURS 1100.                                    
025300         05  TAB-BEART1          PIC X(25).                               
025400                                                                          
025500 01  TABELL-INDEX                PIC S9(5)  COMP-3.                       
025600                                                                          
025700 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
025800 01  NYCKLAR-TILL-DLI.                                                    
025900*                                                                         
026000     03  W-WDE421KY-X.                                                    
026100         05  W-IDPRODNR-E4       PIC S9(7)   COMP-3.                      
026200         05  W-IDKOLLI-E4        PIC S9(5)   COMP-3.                      
026300     03  W-E601-IDPRODNR-X.                                               
026400         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
026500*                                                                         
026600     03  W-E611-IDKOLLI-X.                                                
026700         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
026800*                                                                         
026900     03  W-E611-IDKOLLI-TOM-X.                                            
027000         05  W-E611-IDKOLLI-TOM  PIC S9(5)   COMP-3.                      
027100*                                                                         
027200     03  W-DAINLEV-MIN-X.                                                 
027300         05  W-DAINLEV-MIN       PIC  9(16).                              
027400*                                                                         
027500     03  W-DAINLEV-MAX-X.                                                 
027600         05  W-DAINLEV-MAX       PIC  9(16).                              
027700*                                                                         
027800     03  W-E4A1-WDE4KEY-X.                                                
027900         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
028000         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
028100         05  W-E4A1-IDKUNDRF.                                             
028200             07  W-E4A1-IDORDNR  PIC 9(5).                                
028300             07  FILLER          PIC X(5)    VALUE SPACE.                 
028400*                                                                         
028500   03    W-WDE4F1KY.                                                      
028600     05    W-E4F1-IDPRODNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
028700     05    W-E4F1-IDKOLLI-MAX    PIC S9(5)   VALUE ZERO  COMP-3.          
028800                                                                          
028900   03    W-WDE4F1KY-MIN-X.                                                
029000     05    W-E4F1-IDPRODNR-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
029100     05    W-E4F1-IDKOLLI-MIN    PIC S9(5)   VALUE ZERO  COMP-3.          
029200     05    W-WDE4F1-MIN.                                                  
029300     07    W-E4F1-DISTR-MIN    PIC S9(5) COMP-3 VALUE ZERO.               
029400     07    W-E4F1-KUNDNR-MIN    PIC S9(7) COMP-3 VALUE ZERO.              
029500     07    W-E4F1-KUNDRF-MIN    PIC X(10) VALUE LOW-VALUE.                
029600     07    W-E4F1-PLKLST-MIN    PIC S9(3) COMP-3 VALUE ZERO.              
029700     07    W-E4F1-PURAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.               
029800                                                                          
029900                                                                          
030000     EJECT                                                                
030100     03  W-E401-WDE4KEY-X.                                                
030200         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
030300         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
030400         05  W-E401-IDKUNDRF.                                             
030500             07  W-E401-IDORDNR  PIC 9(5).                                
030600             07  FILLER          PIC X(5)    VALUE SPACE.                 
030700         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
030800         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
030900*                                                                         
031000     03  W-E411-IDPURAD-X.                                                
031100         05  W-E411-IDPURAD      PIC S9(5)   COMP-3.                      
031200*                                                                         
031300     03  W-KDKOLLI               PIC X(8)    VALUE SPACE.                 
031400*                                                                         
031500     03 W-WDB101KY-X.                                                     
031600         05 W-IDPARTNR           PIC  X(9).                               
031700         05 W-IDFTG              PIC  9(2).                               
031800*                                                                         
031900     03 W-IDGMT-X.                                                        
032000         05 W-B201-IDDISTR       PIC S9(5)   COMP-3.                      
032100         05 W-B201-IDKUNDNR      PIC S9(7)   COMP-3.                      
032200*                                                                         
032300     03 W-WDB501KY-X.                                                     
032400         05 W-501-IDDC           PIC X(2).                                
032500         05 W-501-KDFRAKT        PIC S9(3)   VALUE ZERO  COMP-3.          
032600         05  W-IDGMT-WDB5.                                                
032700           07 W-501-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
032800           07 W-501-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
032900*                                                                         
033000     03 W-WDB501KY-DEFAULT-X.                                             
033100         05 W-501-IDDC-DEFAULT     PIC X(2).                              
033200         05 W-501-KDFRAKT-DEFAULT  PIC S9(3) VALUE ZERO COMP-3.           
033300         05  W-IDGMT-WDB5-DEFAULT.                                        
033400         07 W-501-IDDISTR-DEFAULT PIC S9(5) VALUE ZERO COMP-3.            
033500         07 W-501-IDKUNDNR-DEFAULT PIC S9(7) VALUE 9999999 COMP-3.        
033600*                                                                         
033700     03  W-WDQ301KY-X.                                                    
033800         05  W-ODEL-IDORDER      PIC S9(7)   COMP-3.                      
033900         05  W-ODEL-IDDC         PIC  X(2).                               
034000         05  W-ODEL-IDPRODNR     PIC S9(7)   COMP-3.                      
034100         05  W-ODEL-IDPLKLST     PIC S9(3)   COMP-3.                      
034200*                                                                         
034300     03  W-IDORDER-X.                                                     
034400         05  W-IDORDER           PIC S9(7)   COMP-3.                      
034500*                                                                         
034600     03  W-IDARTNR-X.                                                     
034700         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
034800*                                                                         
034900     03  W-IDDC-X.                                                        
035000         05  W-IDDC              PIC X(2).                                
035100*                                                                         
035200     03  W-IDSKYLT-X.                                                     
035300         05  W-IDSKYLT           PIC X(3).                                
035400*                                                                         
035500     03  W-KDSEGKEY-X.                                                    
035600         05  W-KDSEGKEY          PIC 9(1)   VALUE 1.                      
035700*                                                                         
035800     03  W-IDDC-B6-X.                                                     
035900         05 W-IDDC-B6                  PIC X(2).                          
036000     EJECT                                                                
036100 01  FILLER                    PIC X(16) VALUE 'ERR-INFO-AREA   '.        
036200 01  MEDDELANDE.                                                          
036300                                                                          
036400   03  FEL-INVALID-KEY           PIC X(3)  VALUE '022'.                   
036500   03  FEL-X-IS-INVALID          PIC X(3)  VALUE '023'.                   
036600   03  FEL-NOT-NUMERIC           PIC X(3)  VALUE '024'.                   
036700   03  FEL-X-NOT-FOUND           PIC X(3)  VALUE '025'.                   
036800   03  FEL-LINE-NOT-FOUND        PIC X(3)  VALUE '027'.                   
036900   03  FEL-CASE-MISSING          PIC X(3)  VALUE '137'.                   
037000   03  FEL-ORDER-MISSING         PIC X(3)  VALUE '025'.                   
037100   03  FEL-NOT-ZERO              PIC X(3)  VALUE '126'.                   
037200   03  FEL-INVALID-DC            PIC X(3)  VALUE '128'.                   
037300   03  FEL-NO-APPROVED-CASE      PIC X(3)  VALUE '133'.                   
037400   03  FEL-MAX-50-CASES-IN-INTERVAL PIC X(3)  VALUE '139'.                
037500                                                                          
037600   03  FEL-CASE-OR-ORDER-MISSING PIC X(3)  VALUE '169'.                   
037700     EJECT                                                                
037800 01  LIST-TEXTER.                                                         
037900     03  PV-SV-NAMN              PIC X(29)                                
038000          VALUE 'VOLVO PERSONVAGNAR AB, PARTS '.                          
038100     03  PV-ENG-NAMN             PIC X(29)                                
038200          VALUE 'VOLVO CAR CORPORATION, PARTS '.                          
038300     03  LV-PV-ADRESS            PIC X(29)                                
038400          VALUE 'SE-405 31 GÖTEBORG, SWEDEN   '.                          
038500     EJECT                                                                
038600                                                                          
038700     EJECT                                                                
038800 01    IMS-WS.                                                            
038900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
039000                                                                          
039100                                                                          
039200                                                                          
039300*                        **** STATUS-KOD FRÅN IMS                         
039400   03    STATUS-WDE401-SEK-WS    PIC XX.                                  
039500     88    WDE401-SEK-FINNS                  VALUE '  '.                  
039600     88    WDE401-SEK-SAKNAS                 VALUE 'GE' 'GB'.             
039700   03    STATUS-WS               PIC XX.                                  
039800     88    SEGMENT-FINNS                     VALUE '  '.                  
039900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
040000   03    STATUS-WS-ARTS          PIC XX.                                  
040100     88    SEGMENT-FINNS-ARTS                VALUE '  '.                  
040200     88    SEGMENT-SAKNAS-ARTS               VALUE 'GE'.                  
040300   03    STATUS-WS-KOLLI         PIC XX.                                  
040400     88    SEGMENT-FINNS-KOLLI               VALUE '  '.                  
040500     88    SEGMENT-SAKNAS-KOLLI              VALUE 'GE'.                  
040600   03    STATUS-WS-RAD           PIC XX.                                  
040700     88    SEGMENT-FINNS-RAD                 VALUE '  '.                  
040800     88    SEGMENT-SAKNAS-RAD                VALUE 'GE'.                  
040900                                                                          
041000                                                                          
041100                                                                          
041200   03    GODK-STATUSKODER.                                                
041300     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
041400                                                                          
041500                                                                          
041600                                                                          
041700 01    SSA1                      PIC X(160).                              
041800 01    SSA2                      PIC X(64).                               
041900     SKIP2                                                                
042000*                            IMS FUNKTIONSKODER                           
042100*01    -COPY W0003                                                        
042200     SKIP2                                                                
042300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
042400 01    DLI-IO-AREA.                                                       
042500   03    IO-AREA                 PIC X(768)  VALUE SPACE.                 
042600                                                                          
042700*  03    WDE601   -COPY WDE601             -RED IO-AREA.                  
042800     SKIP2                                                                
042900*  03    WDE611   -COPY WDE611             -RED IO-AREA.                  
043000     SKIP2                                                                
043100*  03    WLORQI01 -COPY WDQ201             -RED IO-AREA.                  
043200     SKIP2                                                                
043300*  03    WLBENA01 -COPY WDD311             -RED IO-AREA.                  
043400     SKIP2                                                                
043500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
043600 01    DLI-IO-AREA2.                                                      
043700   03    IO-AREA2                PIC X(272)  VALUE SPACE.                 
043800                                                                          
043900*  03 WDE401   -COPY  WDE401            -RED IO-AREA2.                    
044000     SKIP2                                                                
044100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA3'.          
044200 01  DLI-IO-AREA3.                                                        
044300     03  IO-AREA3                PIC X(320)  VALUE SPACE.                 
044400*    03  WLARTS11 -COPY WDK711        -RED IO-AREA3.                      
044500     SKIP2                                                                
044600 01  FILLER                      PIC X(16)  VALUE 'E4-WDE401-ARE'.        
044700 01  WDE4F-401-AREA.                                                      
044800   03    WDE401   -COPY WDE401   -PRE FSEQ-                               
044900     SKIP2                                                                
045000 01  FILLER                      PIC X(16)  VALUE 'E4-WDE411-ARE'.        
045100 01  WDE4F-AREA.                                                          
045200   03    WDE411   -COPY WDE411                                            
045300   03    WDE421   -COPY WDE421                                            
045400     SKIP2                                                                
045500 01  FILLER                      PIC X(16)  VALUE 'WDB1-AREA'.            
045600*01  -COPY WDB101                                                         
045700     SKIP2                                                                
045800 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
045900*01  -COPY WDB201                                                         
046000     SKIP2                                                                
046100 01  FILLER                      PIC X(16)  VALUE 'WDB5-AREA'.            
046200*01  -COPY WDB501                                                         
046300     SKIP2                                                                
046400 01  FILLER                      PIC X(16)  VALUE 'WDQ3-AREA'.            
046500*01  -COPY WDQ301                                                         
046600     SKIP2                                                                
046700 01  FILLER                      PIC X(16)  VALUE 'WDK611-AREA'.          
046800*01  -COPY WDK611                                                         
046900     SKIP3                                                                
047000 01  FILLER                      PIC X(16)  VALUE 'W6D211-AREA'.          
047100*01  -COPY W6D211                                                         
047200     SKIP3                                                                
047300 01  FILLER                      PIC X(16)  VALUE 'WDD801-AREA'.          
047400*01  -COPY WDD801                                                         
047500     SKIP3                                                                
047600 01  FILLER                      PIC X(16)  VALUE 'WDD811-AREA'.          
047700*01  -COPY WDD811                                                         
047800     SKIP3                                                                
047900 01  FILLER                      PIC X(16)  VALUE 'WDL601-AREA'.          
048000*01  -COPY WDL601 -PRE INL-                                               
048100     SKIP3                                                                
048200 01  FILLER                      PIC X(16)  VALUE 'WDL611-AREA'.          
048300*01  -COPY WDL611                                                         
048400     EJECT                                                                
048500                                                                          
048600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
048700 01   DLI-IO-AREA-B601.                                                   
048800*     03  -COPY WDB601                                                    
048900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF502'.             
049000 01  DLI-IO-WDF502.                                                       
049100*  03  -COPY WDF502                                                       
049200                                                                          
049300 LINKAGE SECTION.                                                         
049400 01  MSG-PCB                     PIC X.                                   
049500                                                                          
049600 01  DISTRDOC-PCB                PIC X.                                   
049700                                                                          
049800*01    -COPY W0008     -PRE WDE4A-                                        
049900     05  FILLER                  PIC X.                                   
050000     SKIP2                                                                
050100*01    -COPY W0008     -PRE WDE4F-                                        
050200     05  FILLER                  PIC X.                                   
050300     SKIP2                                                                
050400*01    -COPY W0008     -PRE GMTA-                                         
050500     05  FILLER                  PIC X.                                   
050600     SKIP2                                                                
050700*01    -COPY W0008     -PRE ORQI-                                         
050800     05  FILLER                  PIC X.                                   
050900     SKIP2                                                                
051000*01    -COPY W0008     -PRE BENA-                                         
051100     05  FILLER                  PIC X.                                   
051200     SKIP2                                                                
051300*01    -COPY W0008     -PRE WDE6-                                         
051400     05  FILLER                  PIC X.                                   
051500     SKIP2                                                                
051600*01    -COPY W0008     -PRE BETC-                                         
051700     05  FILLER                  PIC X.                                   
051800     SKIP2                                                                
051900*                                                                         
052000*01    -COPY W0008     -PRE WDB6-                                         
052100     05  FILLER                  PIC X.                                   
052200     EJECT                                                                
052300*01    -COPY W0008     -PRE WDF5-                                         
052400     05  FILLER                  PIC X.                                   
052500     EJECT                                                                
052600*01    -COPY W0008     -PRE WDQ3-                                         
052700     05  FILLER                  PIC X.                                   
052800     EJECT                                                                
052900                                                                          
053000 PROCEDURE DIVISION USING  MSG-PCB   DISTRDOC-PCB                         
053100                           WDE4A-PCB WDE4F-PCB  GMTA-PCB                  
053200                           ORQI-PCB  BENA-PCB   WDE6-PCB                  
053300                           BETC-PCB  WDB6-PCB WDF5-PCB WDQ3-PCB.          
053400 MAIN SECTION.                                                            
053500     ENTRY 'DLITCBL' USING MSG-PCB   DISTRDOC-PCB                         
053600                           WDE4A-PCB WDE4F-PCB  GMTA-PCB                  
053700                           ORQI-PCB  BENA-PCB   WDE6-PCB                  
053800                           BETC-PCB  WDB6-PCB WDF5-PCB WDQ3-PCB.          
053900                                                                          
054000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
054100     IF SUB-KDRC = 0                                                      
054200        IF REQU-L129-KVRADER NUMERIC AND REQU-L129-KVRADER > 0            
054300                                                                          
054400           MOVE FUNCTION CURRENT-DATE (9:8)                               
054500                                      TO WS-CURRENT-TIME                  
054600                                                                          
054700           MOVE SPACE                 TO HDR-AREA                         
054800                                         HDR-IDOUTREC                     
054900           MOVE 'DELIVERY-NOTE'       TO HDR-IDOUTTYPE                    
055000           MOVE REQU-L129-IDDC-KEY(1) TO HDR-IDOUTREC(1:2)                
055100           MOVE 001         TO REQU-IDMSGVER IN HDR-AREA                  
055200           MOVE 'R'         TO REQU-KDPGMACT IN HDR-AREA                  
055300           MOVE REQU-IDUSER IN REQU-AREA                                  
055400                            TO REQU-IDUSER   IN HDR-AREA                  
055500                               HDR-IDOUTREC(3:8)                          
055600*          ---  HÄR LÅG HEADER-SKRIVNINGEN FÖRE ETR 5655543               
055700*                                                  / C.E.                 
055800           MOVE 1 TO WS-CNT                                               
055900           MOVE REQU-L129-KVRADER TO MAX-CNT-REC                          
056000                                                                          
056100           PERFORM UNTIL WS-CNT > MAX-CNT-REC OR WS-CNT > 50              
056200              MOVE NEJ TO WS-FEL-FUNNET                                   
056300              PERFORM A-INIT                                              
056400              PERFORM B-FORMELL-KONTROLL                                  
056500                                                                          
056600              IF FEL-EJ-FUNNET                                            
056700                                                                          
056800                 IF WS-IDKOLLI-TOM > 0                                    
056900                    MOVE WS-IDKOLLI       TO WS-IDKOLLI-PRT               
057000                    PERFORM UNTIL WS-IDKOLLI-PRT > WS-IDKOLLI-TOM         
057100*                     -- HÄR SKRIVS RAPP.HUVUD OCH RADER                  
057200*                     -- FÖR VARJE REQU-RAD. (WS-CNT)                     
057300                      PERFORM C-LAES-BASER-SKRIV-LISTA                    
057400                                                                          
057500                      ADD +1   TO WS-IDKOLLI-PRT                          
057600                      MOVE WS-IDKOLLI-TEST-MN TO  WS-IDKOLLI              
057700                      MOVE ZERO   TO   WS-SUM-ART                         
057800                      MOVE 1                   TO IX-TAB                  
057900                      PERFORM UNTIL IX-TAB   > TAB-MAX                    
058000                         MOVE ZERO  TO TAB-KVLEVART (IX-TAB)              
058100                         MOVE ZERO TO TAB-PURAD (IX-TAB)                  
058200                         MOVE ZERO TO TAB-IDARTNR (IX-TAB)                
058300                         MOVE ZERO TO TAB-REKSIFFR (IX-TAB)               
058400                         MOVE ZERO TO TAB-IDRONR (IX-TAB)                 
058500                         MOVE ZERO TO TAB-INDEX (IX-TAB)                  
058600                         MOVE SPACE TO TAB-ORG  (IX-TAB)                  
058700                         MOVE SPACE TO TAB-IDARTNR-ERS(IX-TAB)            
058800                         MOVE SPACE TO TAB-BERADREF (IX-TAB)              
058900                         ADD 1 TO IX-TAB                                  
059000                      END-PERFORM                                         
059100                    END-PERFORM                                           
059200                 ELSE                                                     
059300                    PERFORM C-LAES-BASER-SKRIV-LISTA                      
059400                 END-IF                                                   
059500              END-IF                                                      
059600                                                                          
059700              ADD +1 TO  WS-CNT                                           
059800           END-PERFORM                                                    
059900                                                                          
060000           IF FEL-FUNNET OR RESP-IDMSG-ERROR NOT = SPACE                  
060100*             IF REQU-L129-FLBG = 'Y'                                     
060200*                CALL ABEND USING RKOD-ABEND-WITH-DUMP                    
060300*             ELSE                                                        
060400                 PERFORM IMS-ROLLBACK                                     
060500                 PERFORM S02-RETURN-DATA                                  
060600*             END-IF                                                      
060700           ELSE                                                           
060800* CLOSE                                                                   
060900             IF WZ04-SEND-IDCOM > ZERO                                    
061000                PERFORM S03-SEND-CLOSE                                    
061100                MOVE ZERO TO WZ04-SEND-IDCOM                              
061200             END-IF                                                       
061300           END-IF                                                         
061400        ELSE                                                              
061500           MOVE FEL-X-IS-INVALID TO RESP-IDMSG-ERROR                      
061600           MOVE 'KVRADER'        TO RESP-IDELMT-ERROR                     
061700*          IF REQU-L129-FLBG = 'Y'                                        
061800*            CALL ABEND USING RKOD-ABEND-WITH-DUMP                        
061900*          END-IF                                                         
062000           PERFORM IMS-ROLLBACK                                           
062100           PERFORM S02-RETURN-DATA                                        
062200        END-IF                                                            
062300     END-IF                                                               
062400                                                                          
062500     MOVE ZERO TO RETURN-CODE                                             
062600     GOBACK                                                               
062700     .                                                                    
062800     EJECT                                                                
062900 A-INIT             SECTION.                                              
063000                                                                          
063100     MOVE SPACE   TO RESP-AREA                                            
063200                     RESP-AREA-HEAD                                       
063300                     RESP-AREA-LINE                                       
063400                     RESP-AREA-TOTAL                                      
063500     MOVE 001     TO RESP-IDMSGVER                                        
063600     MOVE ZERO    TO WS-SUM-ART                                           
063700                                                                          
063800     MOVE JA                      TO SW-NYCKLAR-OK                        
063900     MOVE NEJ                     TO WS-RATT-PRODNR                       
064000                                                                          
064100     MOVE 1                   TO IX-TAB                                   
064200     PERFORM UNTIL IX-TAB   > TAB-MAX                                     
064300       MOVE ZERO              TO TAB-PURAD      (IX-TAB)                  
064400       MOVE ZERO              TO TAB-KVLEVART   (IX-TAB)                  
064500       MOVE ZERO              TO TAB-IDARTNR    (IX-TAB)                  
064600       MOVE ZERO              TO TAB-REKSIFFR   (IX-TAB)                  
064700       MOVE ZERO              TO TAB-IDRONR     (IX-TAB)                  
064800       MOVE ZERO              TO TAB-INDEX      (IX-TAB)                  
064900       MOVE SPACE             TO TAB-ORG        (IX-TAB)                  
065000       MOVE SPACE             TO TAB-IDARTNR-ERS(IX-TAB)                  
065100       MOVE SPACE             TO TAB-BERADREF   (IX-TAB)                  
065200       ADD 1 TO IX-TAB                                                    
065300     END-PERFORM                                                          
065400                                                                          
065500     IF REQU-L129-IDDISTR-KEY (WS-CNT) = ALL '+'                          
065600         MOVE ZERO                           TO   WS-IDDISTR              
065700     ELSE                                                                 
065800         MOVE REQU-L129-IDDISTR-KEY (WS-CNT) TO   WS-IDDISTR              
065900     END-IF                                                               
066000                                                                          
066100     IF REQU-L129-IDKUNDNR-KEY (WS-CNT) = ALL '+'                         
066200         MOVE ZERO                            TO   WS-IDKUNDNR            
066300     ELSE                                                                 
066400         MOVE REQU-L129-IDKUNDNR-KEY (WS-CNT) TO   WS-IDKUNDNR            
066500     END-IF                                                               
066600                                                                          
066700     IF REQU-L129-IDORDNR-KEY (WS-CNT)   = ALL '+'                        
066800         MOVE ZERO                            TO   WS-IDORDNR             
066900     ELSE                                                                 
067000         MOVE REQU-L129-IDORDNR-KEY (WS-CNT)  TO   WS-IDORDNR             
067100     END-IF                                                               
067200                                                                          
067300     IF REQU-L129-IDKOLLI-KEY (WS-CNT) = ALL '+'                          
067400         MOVE ZERO                           TO   WS-IDKOLLI              
067500         MOVE ZERO                           TO   WS-IDKOLLI-NUM          
067600     ELSE                                                                 
067700         MOVE REQU-L129-IDKOLLI-KEY (WS-CNT) TO   WS-IDKOLLI              
067800         MOVE REQU-L129-IDKOLLI-KEY (WS-CNT) TO   WS-IDKOLLI-NUM          
067900     END-IF                                                               
068000                                                                          
068100     IF REQU-L129-IDKOLLI-TOM (WS-CNT) = ALL '+'                          
068200         MOVE ZERO                            TO   WS-IDKOLLI-TOM         
068300     ELSE                                                                 
068400         MOVE REQU-L129-IDKOLLI-TOM (WS-CNT)  TO   WS-IDKOLLI-TOM         
068500     END-IF                                                               
068600                                                                          
068700     IF REQU-L129-FLSKRIV-DELNOTE (WS-CNT) NOT = ALL '+'                  
068800       MOVE REQU-L129-FLSKRIV-DELNOTE  (WS-CNT)                           
068900                                    TO   WS-FL-SVENSK-FSEDEL              
069000     END-IF                                                               
069100                                                                          
069200     MOVE ZERO                            TO   SPAR-IDRADNR-KO            
069300                                               SPAR-KVLEVART              
069400     .                                                                    
069500     EJECT                                                                
069600 B-FORMELL-KONTROLL  SECTION.                                             
069700                                                                          
069800     MOVE REQU-L129-IDDC-KEY (WS-CNT)  TO WS-IDDC-KEY                     
069900                                          W-IDDC-B6                       
070000                                          WS-IDDC                         
070100                                                                          
070200     IF NYCKLAR-FEL                                                       
070300        IF FEL-EJ-FUNNET                                                  
070400           MOVE FEL-X-NOT-FOUND  TO RESP-IDMSG-ERROR                      
070500           MOVE 'IDDC'           TO RESP-IDELMT-ERROR                     
070600           MOVE JA               TO WS-FEL-FUNNET                         
070700        END-IF                                                            
070800     END-IF                                                               
070900                                                                          
071000     IF WS-IDKOLLI NOT NUMERIC                                            
071100        IF FEL-EJ-FUNNET                                                  
071200           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
071300           MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                      
071400           MOVE JA              TO WS-FEL-FUNNET                          
071500        END-IF                                                            
071600     ELSE                                                                 
071700       IF WS-IDKOLLI = ZERO                                               
071800           MOVE FEL-NOT-ZERO    TO RESP-IDMSG-ERROR                       
071900           MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                      
072000           MOVE JA              TO WS-FEL-FUNNET                          
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400     IF WS-IDKOLLI-TOM NOT NUMERIC                                        
072500        IF FEL-EJ-FUNNET                                                  
072600           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
072700           MOVE 'IDKOLLI-TOM'   TO RESP-IDELMT-ERROR                      
072800           MOVE JA              TO WS-FEL-FUNNET                          
072900        END-IF                                                            
073000     END-IF                                                               
073100                                                                          
073200     IF WS-IDKOLLI-NUM NOT NUMERIC                                        
073300        IF FEL-EJ-FUNNET                                                  
073400           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
073500           MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                      
073600           MOVE JA              TO WS-FEL-FUNNET                          
073700        END-IF                                                            
073800     END-IF                                                               
073900                                                                          
074000     IF WS-IDDISTR NOT NUMERIC                                            
074100        IF FEL-EJ-FUNNET                                                  
074200           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
074300           MOVE 'IDDISTR'       TO RESP-IDELMT-ERROR                      
074400           MOVE JA              TO WS-FEL-FUNNET                          
074500        END-IF                                                            
074600     ELSE                                                                 
074700       IF WS-IDDISTR = ZERO                                               
074800           MOVE FEL-NOT-ZERO    TO RESP-IDMSG-ERROR                       
074900           MOVE 'IDDISTR'       TO RESP-IDELMT-ERROR                      
075000           MOVE JA              TO WS-FEL-FUNNET                          
075100       END-IF                                                             
075200     END-IF                                                               
075300                                                                          
075400     IF WS-IDORDNR NOT NUMERIC                                            
075500        IF FEL-EJ-FUNNET                                                  
075600           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
075700           MOVE 'IDORDNR'       TO RESP-IDELMT-ERROR                      
075800           MOVE JA              TO WS-FEL-FUNNET                          
075900        END-IF                                                            
076000     ELSE                                                                 
076100       IF WS-IDORDNR = ZERO                                               
076200           MOVE FEL-NOT-ZERO    TO RESP-IDMSG-ERROR                       
076300           MOVE 'IDORDNR'       TO RESP-IDELMT-ERROR                      
076400           MOVE JA              TO WS-FEL-FUNNET                          
076500       END-IF                                                             
076600     END-IF                                                               
076700                                                                          
076800     IF WS-IDKUNDNR NOT NUMERIC                                           
076900        IF FEL-EJ-FUNNET                                                  
077000           MOVE FEL-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
077100           MOVE 'IDKUNDNR'      TO RESP-IDELMT-ERROR                      
077200           MOVE JA              TO WS-FEL-FUNNET                          
077300        END-IF                                                            
077400     END-IF                                                               
077500                                                                          
077600     IF  WS-IDKOLLI-TOM NOT = ZERO                                        
077700     AND WS-IDKOLLI-TOM < WS-IDKOLLI                                      
077800         IF FEL-EJ-FUNNET                                                 
077900           MOVE FEL-X-IS-INVALID   TO RESP-IDMSG-ERROR                    
078000           MOVE 'IDKOLLI-TOM'      TO RESP-IDELMT-ERROR                   
078100           MOVE JA                 TO WS-FEL-FUNNET                       
078200         END-IF                                                           
078300     END-IF                                                               
078400                                                                          
078500     IF FEL-EJ-FUNNET                                                     
078600     IF WS-IDKOLLI-TOM - WS-IDKOLLI-NUM > 50                              
078700                                                                          
078800       IF FEL-EJ-FUNNET                                                   
078900         MOVE FEL-MAX-50-CASES-IN-INTERVAL TO RESP-IDMSG-ERROR            
079000         MOVE 'IDKOLLI-TOM'          TO RESP-IDELMT-ERROR                 
079100         MOVE JA                     TO WS-FEL-FUNNET                     
079200       END-IF                                                             
079300     END-IF                                                               
079400     END-IF                                                               
079500                                                                          
079600                                                                          
079700     IF WS-FL-SVENSK-FSEDEL NOT = JA                                      
079800       MOVE 'N'                      TO   WS-FL-SVENSK-FSEDEL             
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200 C-LAES-BASER-SKRIV-LISTA SECTION.                                        
080300                                                                          
080400     MOVE NEJ                        TO WS-NGN-PLKLST-GODK                
080500     MOVE NEJ                        TO WS-NGN-PLKLST-FEL                 
080600                                                                          
080700     MOVE '1'                        TO HEAD-IDAFPRCD                     
080800     MOVE '001'                      TO HEAD-REP-IDPTYP-1                 
080900     MOVE WS-IDDC-KEY                TO HEAD-REP-IDDC                     
081000     MOVE WS-IDDISTR                 TO W-IDDISTR                         
081100                                        HEAD-REP-IDDISTR                  
081200     MOVE WS-IDKUNDNR                TO W-IDKUNDNR                        
081300                                        HEAD-REP-IDKUNDNR                 
081400     MOVE WS-IDORDNR                 TO WS-IDORDNR-KREF                   
081500                                        HEAD-REP-IDORDNR                  
081600                                                                          
081700     MOVE 0                          TO HEAD-REP-IDBORD                   
081800     MOVE WS-IDKOLLI                 TO W-IDKOLLI                         
081900     IF  WS-IDKOLLI-TOM = ZERO                                            
082000         MOVE WS-IDKOLLI             TO W-IDKOLLI-TOM                     
082100     ELSE                                                                 
082200         MOVE WS-IDKOLLI-PRT         TO W-IDKOLLI-TOM                     
082300     END-IF                                                               
082400     MOVE W-IDDISTR                  TO W-E4A1-IDDISTR                    
082500     MOVE W-IDKUNDNR                 TO W-E4A1-IDKUNDNR                   
082600     MOVE WS-IDORDNR-KREF            TO W-E4A1-IDORDNR                    
082700     PERFORM IMS-GU-E401-KVAL-SEK                                         
082800     IF WDE401-SEK-FINNS                                                  
082900                                                                          
083000       PERFORM UNTIL (WDE401-SEK-SAKNAS) OR WS-RATT-PRODNR = JA           
083100         IF  KORD-IDDC     = WS-IDDC-KEY                                  
083200         AND KORD-KVORDRAD-LEVPL   = ZERO                                 
083300           MOVE KORD-IDDISTR             TO W-E401-IDDISTR                
083400           MOVE KORD-IDKUNDNR            TO W-E401-IDKUNDNR               
083500           MOVE KORD-IDKUNDRF            TO W-E401-IDKUNDRF               
083600           MOVE KORD-IDPRODNR            TO W-E601-IDPRODNR               
083700                                            W-E401-IDPRODNR               
083800           MOVE KORD-IDPLKLST            TO W-E401-IDPLKLST               
083900           MOVE KORD-KDORDKL             TO HEAD-REP-KDORDKL              
084000           MOVE KORD-IDORDER             TO W-IDORDER                     
084100           MOVE JA                       TO WS-RATT-PRODNR                
084200         ELSE                                                             
084300           PERFORM IMS-GN-E401-KVAL-SEK                                   
084400         END-IF                                                           
084500       END-PERFORM                                                        
084600                                                                          
084700                                                                          
084800       IF WDE401-SEK-FINNS                                                
084900         PERFORM IMS-GU-E601-KVAL                                         
085000         IF SEGMENT-FINNS                                                 
085100           PERFORM CC-HAEMTA-DATA-I-E601                                  
085200           IF WS-IDKOLLI-PRT > 1                                          
085300             MOVE WS-IDKOLLI-PRT TO W-E611-IDKOLLI                        
085400           ELSE                                                           
085500             MOVE W-IDKOLLI    TO W-E611-IDKOLLI                          
085600           END-IF                                                         
085700           MOVE W-IDKOLLI-TOM  TO W-E611-IDKOLLI-TOM                      
085800           PERFORM IMS-GNP-E611-KVAL                                      
085900                                                                          
086000           IF  SEGMENT-FINNS-KOLLI                                        
086100             MOVE JA           TO WS-NGN-PLKLST-GODK                      
086200             MOVE JA           TO SKRIV-DELNOTE-SW                        
086300             PERFORM CE-HAMTA-KLI-RAD-DATA                                
086400           ELSE                                                           
086500             MOVE NEJ           TO SKRIV-DELNOTE-SW                       
086600             IF  WS-NGN-PLKLST-FEL = NEJ                                  
086700               MOVE JA            TO WS-NGN-PLKLST-FEL                    
086800               MOVE FEL-CASE-MISSING                                      
086900                    TO RESP-IDMSG-ERROR                                   
087000             END-IF                                                       
087100           END-IF                                                         
087200         ELSE                                                             
087300           IF  WS-NGN-PLKLST-FEL = NEJ                                    
087400             MOVE JA            TO WS-NGN-PLKLST-FEL                      
087500             MOVE FEL-ORDER-MISSING                                       
087600                TO RESP-IDMSG-ERROR                                       
087700             MOVE 'IDORDNR'                                               
087800                TO RESP-IDELMT-ERROR                                      
087900           END-IF                                                         
088000         END-IF                                                           
088100       ELSE                                                               
088200         IF    WS-NGN-PLKLST-FEL = NEJ                                    
088300           MOVE JA              TO WS-NGN-PLKLST-FEL                      
088400           MOVE FEL-ORDER-MISSING                                         
088500              TO RESP-IDMSG-ERROR                                         
088600           MOVE 'IDORDNR'                                                 
088700              TO RESP-IDELMT-ERROR                                        
088800         END-IF                                                           
088900       END-IF                                                             
089000                                                                          
089100       IF SKRIV-DELNOTE-SW = JA                                           
089200       AND WS-NGN-PLKLST-GODK = JA                                        
089300                                                                          
089400         MOVE 1 TO IX-TAB                                                 
089500         PERFORM UNTIL TAB-PURAD (IX-TAB) < 1                             
089600            MOVE IX-TAB TO TAB-ANT                                        
089700            ADD +1 TO IX-TAB                                              
089800         END-PERFORM                                                      
089900         SUBTRACT 1 FROM IX-TAB                                           
090000         MOVE IX-TAB                  TO TAB-ANT                          
090100                                                                          
090200         PERFORM CM-SORTERA-TABELL                                        
090300                                                                          
090400         PERFORM IMS-GU-ORQI                                              
090500                                                                          
090600         MOVE WS-IDDC-KEY TO W-IDDC-B6                                    
090700         PERFORM IMS-GU-WDB601                                            
090800                                                                          
090900                                                                          
091000         MOVE OHUV-BEKUNDRF         TO HEAD-REP-BEKUNDRF                  
091100         INSPECT HEAD-REP-BEKUNDRF   REPLACING ALL '¤' BY 'Ü'             
091200         INSPECT HEAD-REP-BEKUNDRF   REPLACING ALL '#' BY 'O'             
091300                                                                          
091400*        -- FIX BAD DATA IN GMT-OVR FIELDS                                
091500         IF GMT-BEGMT-OVR-RAD1 = LOW-VALUE                                
091600            MOVE SPACE TO GMT-BEGMT-OVR-RAD1                              
091700         END-IF                                                           
091800         IF GMT-BEGMT-OVR-RAD2 = LOW-VALUE                                
091900            MOVE SPACE TO GMT-BEGMT-OVR-RAD2                              
092000         END-IF                                                           
092100         IF GMT-ADGMT-OVR-GATA = LOW-VALUE                                
092200            MOVE SPACE TO GMT-ADGMT-OVR-GATA                              
092300         END-IF                                                           
092400         IF GMT-ADGMT-OVR-PADR = LOW-VALUE                                
092500            MOVE SPACE TO GMT-ADGMT-OVR-PADR                              
092600         END-IF                                                           
092700         IF (NDC-CN OR LDC-CN)                                            
092800         AND (                                                            
092900*            -- IF OVR FIELDS CONTAIN ANY SIGNIFICANT VALUES              
093000                 GMT-BEGMT-OVR-RAD1 NOT = SPACE                           
093100              OR GMT-BEGMT-OVR-RAD2 NOT = SPACE                           
093200              OR GMT-ADGMT-OVR-GATA NOT = SPACE                           
093300              OR GMT-ADGMT-OVR-PADR NOT = SPACE                           
093400             )                                                            
093500*            -- ADDRESS IN DOUBLE-BYTE CHINESE CODE FETCHED FROM          
093600*            -- CUSTOMER DB "OVR" FIELDS INSTEAD OF ORDER HEAD            
093700*            -- (EXCEPT BERADREF/LAND)                                    
093800             MOVE '935' TO TRAUTF8-KDCP                                   
093900                                                                          
094000             MOVE GMT-BEGMT-OVR-RAD1 TO HEAD-REP-BEGMT-RAD1               
094100             MOVE GMT-BEGMT-OVR-RAD2 TO HEAD-REP-BEGMT-RAD2               
094200             MOVE GMT-ADGMT-OVR-GATA TO HEAD-REP-ADGMT-GATA               
094300             MOVE GMT-ADGMT-OVR-PADR TO HEAD-REP-ADGMT-PADR               
094400             MOVE GMT-ADGMT-OVR-LAND TO HEAD-REP-ADGMT-LAND               
094500                                                                          
094600         ELSE                                                             
094700*            -- ADDRESS IN NORMAL EBCDIC CODE                             
094800             MOVE '278' TO TRAUTF8-KDCP                                   
094900                                                                          
095000             IF OHUV-BEGMT = SPACE                                        
095100             AND OHUV-ADGMT = SPACE                                       
095200               PERFORM CD-LAES-HAEMTA-B201-B101                           
095300             ELSE                                                         
095400               MOVE OHUV-BEGMT-RAD1 TO HEAD-REP-BEGMT-RAD1                
095500               MOVE OHUV-BEGMT-RAD2 TO HEAD-REP-BEGMT-RAD2                
095600               MOVE OHUV-ADGMT-GATA TO HEAD-REP-ADGMT-GATA                
095700               MOVE OHUV-ADGMT-PADR TO HEAD-REP-ADGMT-PADR                
095800               MOVE OHUV-ADGMT-LAND TO HEAD-REP-ADGMT-LAND                
095900             END-IF                                                       
096000         END-IF                                                           
096100                                                                          
096200         MOVE 35 TO TRAUTF8-KVMAXTL                                       
096300         IF (HEAD-REP-BEGMT-RAD1 = SPACE OR LOW-VALUE)                    
096400         AND (HEAD-REP-BEGMT-RAD2 = SPACE OR LOW-VALUE)                   
096500*          -- SHIFT UP TWO LINES                                          
096600           MOVE HEAD-REP-ADGMT-GATA TO TRAUTF8-TECONV-FROM                
096700           CALL WTRAUTF8 USING TRAUTF8-AREA                               
096800           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-BEGMT-RAD1                  
096900                                                                          
097000           MOVE HEAD-REP-ADGMT-PADR TO TRAUTF8-TECONV-FROM                
097100           CALL WTRAUTF8 USING TRAUTF8-AREA                               
097200           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-BEGMT-RAD2                  
097300                                                                          
097400           MOVE HEAD-REP-ADGMT-LAND TO TRAUTF8-TECONV-FROM                
097500           CALL WTRAUTF8 USING TRAUTF8-AREA                               
097600           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-ADGMT-GATA                  
097700                                                                          
097800           MOVE ALL X'20'       TO HEAD-REP-ADGMT-PADR                    
097900                                   HEAD-REP-ADGMT-LAND                    
098000         ELSE                                                             
098100           MOVE HEAD-REP-BEGMT-RAD1 TO TRAUTF8-TECONV-FROM                
098200           CALL WTRAUTF8 USING TRAUTF8-AREA                               
098300           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-BEGMT-RAD1                  
098400                                                                          
098500           MOVE HEAD-REP-BEGMT-RAD2 TO TRAUTF8-TECONV-FROM                
098600           CALL WTRAUTF8 USING TRAUTF8-AREA                               
098700           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-BEGMT-RAD2                  
098800                                                                          
098900           MOVE HEAD-REP-ADGMT-GATA TO TRAUTF8-TECONV-FROM                
099000           CALL WTRAUTF8 USING TRAUTF8-AREA                               
099100           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-ADGMT-GATA                  
099200                                                                          
099300           MOVE HEAD-REP-ADGMT-PADR TO TRAUTF8-TECONV-FROM                
099400           CALL WTRAUTF8 USING TRAUTF8-AREA                               
099500           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-ADGMT-PADR                  
099600                                                                          
099700           MOVE HEAD-REP-ADGMT-LAND TO TRAUTF8-TECONV-FROM                
099800           CALL WTRAUTF8 USING TRAUTF8-AREA                               
099900           MOVE TRAUTF8-TECONV-TO TO HEAD-REP-ADGMT-LAND                  
100000                                                                          
100100         END-IF                                                           
100200*NEW                                                                      
100300         MOVE OHUV-BEBETRAD-1         TO HEAD-REP-BEBETRAD-1              
100400         MOVE OHUV-BETELNR            TO HEAD-REP-BETELNR                 
100500         MOVE OHUV-IDMAIL             TO HEAD-REP-IDMAIL                  
100600                                                                          
100700         MOVE 1                       TO IX-TAB                           
100800                                                                          
100900*LK    --- SKRIV HEADER FÖRST NÄR GODKÄND RAD FUNNEN                      
101000         IF HEADER-EJ-SKRIVEN                                             
101100*           *OPEN                                                         
101200          IF WZ04-SEND-IDCOM = ZERO                                       
101300            PERFORM S03-SEND-OPEN                                         
101400            MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                            
101500          END-IF                                                          
101600*           *SYS HEADER                                                   
101700          COMPUTE WS-SEQNO = WS-SEQNO + 1                                 
101800          MOVE WS-IDLIST       TO HDR-IDLIST                              
101900          PERFORM S03-PUT-HEADER                                          
102000*         --- SKRIVES BARA EN GÅNG PER KÖRNING.                           
102100          SET HEADER-SKRIVEN TO TRUE                                      
102200         END-IF                                                           
102300                                                                          
102400*HEADER                                                                   
102500         PERFORM S03-PUT-REPORT-HEAD                                      
102600                                                                          
102700*DETAILS                                                                  
102800         PERFORM UNTIL  IX-TAB > TAB-ANT                                  
102900           MOVE SPACE TO RESP-AREA-LINE                                   
103000           PERFORM CI-SKRIV-DETALJRAD                                     
103100           PERFORM S03-PUT-REPORT-LINE                                    
103200           ADD +1                     TO IX-TAB                           
103300         END-PERFORM                                                      
103400                                                                          
103500*FOOTER                                                                   
103600         MOVE SPACE TO RESP-AREA-TOTAL                                    
103700         MOVE '3'                    TO TOTAL-IDAFPRCD                    
103800         MOVE '003'                  TO TOTAL-REP-IDPTYP-3                
103900         MOVE WS-VLORDBTO            TO TOTAL-REP-VLORDBTO                
104000         MOVE WS-VKORDBTO            TO TOTAL-REP-VKORDBTO                
104100         MOVE WS-SUM-ART             TO TOTAL-REP-SUM-ART                 
104200         PERFORM S03-PUT-REPORT-TOTAL                                     
104300                                                                          
104400       ELSE                                                               
104500         MOVE FEL-CASE-OR-ORDER-MISSING TO RESP-IDMSG-ERROR               
104600         MOVE JA                  TO WS-FEL-FUNNET                        
104700       END-IF                                                             
104800                                                                          
104900     ELSE                                                                 
105000       MOVE FEL-ORDER-MISSING      TO RESP-IDMSG-ERROR                    
105100       MOVE 'IDORDNR'              TO RESP-IDELMT-ERROR                   
105200       MOVE JA                     TO WS-FEL-FUNNET                       
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 CC-HAEMTA-DATA-I-E601 SECTION.                                           
105700                                                                          
105800     MOVE VORD-IDPRODNR                TO HEAD-REP-IDPRODNR               
105900     MOVE VORD-IDPRODNR                TO WS-IDPRODNR-E420F               
106000     MOVE VORD-KDFRAKT                 TO WS-KDFRAKT                      
106100     MOVE VORD-TIUTSKR                 TO WS-VORD-TIUTSKR                 
106200     MOVE VORD-TIUTSTID                TO WS-VORD-TIUTSTID                
106300     .                                                                    
106400     EJECT                                                                
106500 CE-HAMTA-KLI-RAD-DATA SECTION.                                           
106600                                                                          
106700     MOVE KOLLI-IDKOLLI                TO HEAD-REP-IDKOLLI-FOM            
106800                                                                          
106900     IF KOLLI-TIPACKN > ZERO                                              
107000       MOVE KOLLI-TIPACKN              TO HEAD-REP-TIPACKN                
107100       MOVE KOLLI-TIPACTID             TO WS-TIME-HHMMSS                  
107200       MOVE WS-TIPACTID-HHMM           TO HEAD-REP-TIPACTID               
107300     ELSE                                                                 
107400                                                                          
107500                                                                          
107600       MOVE WS-VORD-TIUTSKR            TO HEAD-REP-TIPACKN                
107700       MOVE WS-VORD-TIUTSTID           TO WS-TIME-HHMMSS                  
107800       MOVE WS-TIPACTID-HHMM           TO HEAD-REP-TIPACTID               
107900     END-IF                                                               
108000                                                                          
108100     MOVE KOLLI-IDPLOCK                TO WS-IDPLOCK-GRP                  
108200                                          WS-IDPLOCK-EDIT                 
108300     MOVE WS-IDPLOCK-EDIT              TO HEAD-REP-IDUSER                 
108400     MOVE KOLLI-DARFS                  TO WS-DARFS                        
108500     MOVE WS-DARFS-DATE                TO HEAD-REP-TIRFSDAT               
108600     MOVE WS-DARFS-TIME                TO HEAD-REP-TIRFSTID               
108700     MOVE ZERO                         TO WS-VLORDBTO                     
108800                                          WS-VKORDBTO                     
108900                                          SPAR-KVLEVART                   
109000                                          SPAR-IDRADNR                    
109100                                          IX-TAB                          
109200     MOVE ZERO  TO WS-RECORD-NEXT-SW                                      
109300     PERFORM UNTIL SEGMENT-SAKNAS-KOLLI                                   
109400       MOVE KOLLI-IDKOLLI              TO W-E611-IDKOLLI                  
109500       IF WS-IDKOLLI-TOM > 0                                              
109600         MOVE KOLLI-VLORDBTO-KOLLI           TO WS-VLORDBTO               
109700         MOVE KOLLI-VKORDBTO-KOLLI           TO WS-VKORDBTO               
109800       ELSE                                                               
109900         MOVE KOLLI-VLORDBTO-KOLLI        TO WS-VLORDBTO                  
110000         MOVE KOLLI-VKORDBTO-KOLLI        TO WS-VKORDBTO                  
110100       END-IF                                                             
110200       MOVE WS-IDPRODNR-E420F        TO W-E4F1-IDPRODNR-MAX               
110300       MOVE KOLLI-IDKOLLI            TO W-E4F1-IDKOLLI-MAX                
110400       MOVE WS-IDPRODNR-E420F        TO W-E4F1-IDPRODNR-MIN               
110500       MOVE KOLLI-IDKOLLI            TO W-E4F1-IDKOLLI-MIN                
110600       MOVE WS-IDPRODNR-E420F        TO W-IDPRODNR-E4                     
110700       MOVE KOLLI-IDKOLLI            TO W-IDKOLLI-E4                      
110800                                                                          
110900       PERFORM IMS-GU-WDE411-01-FSEQ                                      
111000       IF SEGMENT-FINNS-RAD                                               
111100         MOVE FSEQ-KORD-IDORDER  TO W-ODEL-IDORDER                        
111200         MOVE FSEQ-KORD-IDDC     TO W-ODEL-IDDC                           
111300         MOVE FSEQ-KORD-IDPRODNR TO W-ODEL-IDPRODNR                       
111400         MOVE FSEQ-KORD-IDPLKLST TO W-ODEL-IDPLKLST                       
111500         PERFORM IMS-GU-WDQ301                                            
111600         IF SEGMENT-FINNS                                                 
111700           MOVE ODEL-IDLOPNR-ORD TO HEAD-REP-IDLOPNR-ORD                  
111800         END-IF                                                           
111900       END-IF                                                             
112000       PERFORM IMS-GU-WDE411-21-FSEQ                                      
112100                                                                          
112200       IF SEGMENT-FINNS-RAD                                               
112300                                                                          
112400         MOVE ORAD-BERADREF          TO HEAD-REP-ADGMT-LAND               
112500         INSPECT HEAD-REP-ADGMT-LAND REPLACING ALL '¤' BY 'Ü'             
112600         INSPECT HEAD-REP-ADGMT-LAND REPLACING ALL '#' BY 'O'             
112700                                                                          
112800         MOVE ZERO TO IX-TAB                                              
112900         PERFORM UNTIL SEGMENT-SAKNAS-RAD OR IX-TAB >= TAB-MAX            
113000           ADD +1 TO IX-TAB                                               
113100           PERFORM CEA-HAMTA-DATA-E411                                    
113200           PERFORM CEB-LAGRA                                              
113300           PERFORM IMS-GN-WDE411-21-FSEQ                                  
113400         END-PERFORM                                                      
113500       END-IF                                                             
113600       PERFORM IMS-GNP-E611-MIN-MAX                                       
113700       IF WS-IDKOLLI-TOM > 0                                              
113800          IF WS-RECORD-NEXT-SW = 0                                        
113900            MOVE KOLLI-IDKOLLI  TO WS-IDKOLLI-TEST-MN                     
114000            MOVE 1  TO  WS-RECORD-NEXT-SW                                 
114100          END-IF                                                          
114200       END-IF                                                             
114300     END-PERFORM                                                          
114400                                                                          
114500     .                                                                    
114600     EJECT                                                                
114700 CEA-HAMTA-DATA-E411 SECTION.                                             
114800                                                                          
114900     MOVE ORAD-IDPURAD          TO SPAR-IDRADNR-KO                        
115000     MOVE ORAD-IDKUNDRF-RO      TO WS-IDKUNDRF-RO                         
115100     MOVE WS-IDORDNR-RO         TO SPAR-IDRONR                            
115200     MOVE ORAD-IDARTNR          TO SPAR-IDARTNR                           
115300     MOVE ORAD-REKSIFFR         TO SPAR-REKSIFFR                          
115400     MOVE KKOLLI-KVLEVART       TO SPAR-KVLEVART                          
115500     MOVE ORAD-BEART            TO SPAR-BEART                             
115600     MOVE ORAD-BERADREF         TO SPAR-BERADREF                          
115700     MOVE ORAD-IDSYSTEM         TO SPAR-IDSYSTEM                          
115800     INSPECT SPAR-BERADREF   REPLACING ALL '¤' BY 'Ü'                     
115900     INSPECT SPAR-BERADREF   REPLACING ALL '#' BY 'O'                     
116000     IF ORAD-FLTILLK > ZERO                                               
116100       MOVE '*'                 TO SPAR-IDARTNR-ERS                       
116200     ELSE                                                                 
116300       MOVE SPACE               TO SPAR-IDARTNR-ERS                       
116400     END-IF                                                               
116500     .                                                                    
116600     EJECT                                                                
116700 CEB-LAGRA SECTION.                                                       
116800                                                                          
116900     MOVE SPAR-IDRADNR-KO  TO TAB-PURAD    (IX-TAB)                       
117000     MOVE SPAR-IDRONR      TO TAB-IDRONR   (IX-TAB)                       
117100     MOVE SPAR-IDARTNR     TO TAB-IDARTNR  (IX-TAB)                       
117200     MOVE SPAR-REKSIFFR    TO TAB-REKSIFFR (IX-TAB)                       
117300     IF WS-IDKOLLI-TOM > 0                                                
117400        IF WS-RECORD-NEXT-SW = 0                                          
117500           ADD  SPAR-KVLEVART    TO TAB-KVLEVART (IX-TAB)                 
117600        END-IF                                                            
117700     ELSE                                                                 
117800        ADD  SPAR-KVLEVART    TO TAB-KVLEVART (IX-TAB)                    
117900     END-IF                                                               
118000     MOVE SPAR-IDARTNR-ERS TO TAB-IDARTNR-ERS (IX-TAB)                    
118100     MOVE IX-TAB           TO TAB-INDEX    (IX-TAB)                       
118200     MOVE SPAR-BEART       TO TAB-BEART1   (IX-TAB)                       
118300     MOVE SPAR-BERADREF    TO TAB-BERADREF (IX-TAB)                       
118400     MOVE SPAR-IDSYSTEM    TO TAB-IDSYSTEM (IX-TAB)                       
118500     .                                                                    
118600     EJECT                                                                
118700 CI-SKRIV-DETALJRAD SECTION.                                              
118800                                                                          
118900     ADD  +1                    TO WS-SUM-ART                             
119000     ADD  TAB-KVLEVART (IX-TAB) TO WS-SUM-LEV                             
119100     MOVE '2'                   TO LINE-IDAFPRCD                          
119200     MOVE '002'                 TO LINE-REP-IDPTYP-2                      
119300     MOVE TAB-IDRONR   (IX-TAB) TO LINE-REP-IDORDNR-RO                    
119400     MOVE TAB-IDARTNR  (IX-TAB) TO LINE-REP-IDARTNR                       
119500                                   W-IDARTNR                              
119600     MOVE TAB-BERADREF (IX-TAB) TO LINE-REP-BERADREF                      
119700     MOVE TAB-REKSIFFR (IX-TAB) TO LINE-REP-REKSIFFR                      
119800     MOVE TAB-IDARTNR-ERS (IX-TAB)                                        
119900                                TO LINE-REP-IDARTNR-ERS                   
120000     MOVE TAB-KVLEVART (IX-TAB) TO LINE-REP-KVLEVART                      
120100     MOVE TAB-INDEX    (IX-TAB) TO TABELL-INDEX                           
120200                                                                          
120300     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
120400     IF DCS-UNICODE-IDSKYLT                                               
120500        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
120600     ELSE                                                                 
120700        MOVE '278 '             TO TRAUTF8-KDCP                           
120800     END-IF                                                               
120900     PERFORM IMS-GET-BENA                                                 
121000     IF SEGMENT-FINNS                                                     
121100        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
121200     ELSE                                                                 
121300        MOVE SPACES             TO TRAUTF8-TECONV-FROM                    
121400     END-IF                                                               
121500                                                                          
121600     IF TRAUTF8-TECONV-FROM = SPACES                                      
121700       MOVE WS-IDSKYLT-GB       TO W-IDSKYLT                              
121800       MOVE '278 '              TO TRAUTF8-KDCP                           
121900       PERFORM IMS-GET-BENA                                               
122000       IF SEGMENT-FINNS                                                   
122100         MOVE TEXT-BEART        TO TRAUTF8-TECONV-FROM                    
122200       END-IF                                                             
122300     END-IF                                                               
122400                                                                          
122500*     -- STRIP SPACE OR CONVERT TO UNICODE                                
122600     MOVE 25 TO TRAUTF8-KVMAXTL                                           
122700     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
122800*     -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                       
122900                                                                          
123000     MOVE TRAUTF8-TECONV-TO     TO LINE-REP-BEART                         
123100*LK  MOVE TAB-IDSYSTEM (IX-TAB) TO LINE-IDSYSTEM                          
123110                                                                          
123200*LK* ADD LYNK&CO TO DELIVERY NOTE                                         
123201     MOVE WS-IDKUNDNR                  TO W-B201-IDKUNDNR                 
123202     MOVE WS-IDDISTR                   TO W-B201-IDDISTR                  
123203     PERFORM IMS-GU-GMTA01-WDB201                                         
123204     IF SEGMENT-FINNS                                                     
123300       IF GMT-KDKUNDKAT = 3                                               
123400          PERFORM IMS-GU-WDF502                                           
123500            IF SEGMENT-FINNS                                              
123600              MOVE XLEV-IDLEVART TO LINE-IDLEVART                         
123610              MOVE 'LYN'         TO LINE-IDSYSTEM                         
123700            END-IF                                                        
123800       END-IF                                                             
123810     END-IF                                                               
123900     .                                                                    
124000     EJECT                                                                
124100 CM-SORTERA-TABELL SECTION.                                               
124200                                                                          
124300     MOVE +36            TO TABENTRY-LNGD                                 
124400     MOVE TAB-ANT        TO ANTAL-ENTRY                                   
124500     MOVE +5             TO SORTBGP-LNGD                                  
124600                                                                          
124700     CALL WINTSOR    USING TABELL                                         
124800                           TABENTRY-LNGD                                  
124900                           ANTAL-ENTRY                                    
125000                           TAB-IDARTNR (1)                                
125100                           SORTBGP-LNGD                                   
125200     .                                                                    
125300                                                                          
125400 CD-LAES-HAEMTA-B201-B101      SECTION.                                   
125500                                                                          
125600     MOVE WS-IDKUNDNR                  TO W-B201-IDKUNDNR                 
125700     MOVE WS-IDDISTR                   TO W-B201-IDDISTR                  
125800     PERFORM IMS-GU-GMTA01-WDB201                                         
125900                                                                          
126000     IF SEGMENT-FINNS                                                     
126100         IF  GMT-ADGMT  = SPACE                                           
126200         AND GMT-BEGMT  = SPACE                                           
126300             MOVE GMT-IDPARTNR         TO W-IDPARTNR                      
126400                                                                          
126500*FROM WDB6                                                                
126600             MOVE DCS-IDFTG            TO W-IDFTG                         
126700                                                                          
126800             PERFORM IMS-GU-BETC01-WDB101                                 
126900             IF SEGMENT-FINNS                                             
127000               MOVE BET-BEBETRAD-1     TO HEAD-REP-BEGMT-RAD1             
127100               MOVE BET-BEBETRAD-2     TO HEAD-REP-BEGMT-RAD2             
127200               MOVE BET-ADBETRAD-1     TO HEAD-REP-ADGMT-GATA             
127300               MOVE BET-ADBETRAD-2     TO HEAD-REP-ADGMT-PADR             
127400               MOVE SPACE              TO HEAD-REP-ADGMT-LAND             
127500             ELSE                                                         
127600               MOVE 'TEXT MISSING'     TO HEAD-REP-BEGMT-RAD1             
127700             END-IF                                                       
127800                                                                          
127900         ELSE                                                             
128000             IF GMT-BEGMT NOT = SPACE                                     
128100                 MOVE GMT-BEGMT-RAD1       TO HEAD-REP-BEGMT-RAD1         
128200                 MOVE GMT-BEGMT-RAD2       TO HEAD-REP-BEGMT-RAD2         
128300                 MOVE GMT-ADGMT-GATA       TO HEAD-REP-ADGMT-GATA         
128400                 MOVE GMT-ADGMT-PADR       TO HEAD-REP-ADGMT-PADR         
128500                 MOVE GMT-ADGMT-LAND       TO HEAD-REP-ADGMT-LAND         
128600             ELSE                                                         
128700                 MOVE GMT-ADGMT-GATA       TO HEAD-REP-BEGMT-RAD1         
128800                 MOVE GMT-ADGMT-PADR       TO HEAD-REP-BEGMT-RAD2         
128900                 MOVE SPACE                TO HEAD-REP-ADGMT-GATA         
129000                                              HEAD-REP-ADGMT-PADR         
129100                                              HEAD-REP-ADGMT-LAND         
129200             END-IF                                                       
129300         END-IF                                                           
129400     ELSE                                                                 
129500         MOVE SPACE                    TO HEAD-REP-BEGMT-RAD1             
129600                                          HEAD-REP-BEGMT-RAD2             
129700                                          HEAD-REP-ADGMT-GATA             
129800                                          HEAD-REP-ADGMT-PADR             
129900                                          HEAD-REP-ADGMT-LAND             
130000     END-IF                                                               
130100     .                                                                    
130200* DISPATCHER-SEKTIONER                                                    
130300     SKIP3                                                                
130400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
130500                                                                          
130600     MOVE 'GETARG'                        TO SUB-KDFUNC                   
130700     MOVE 'CARPARTS.LDC.PRDELNOTE'        TO SUB-ADDISPABS                
130800     MOVE LENGTH OF REQU-AREA             TO SUB-KVDLEN                   
130900                                                                          
131000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
131100                                                                          
131200     IF SUB-KDRC > 0                                                      
131300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
131400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
131500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
131600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
131700     END-IF                                                               
131800     .                                                                    
131900     SKIP3                                                                
132000 S02-RETURN-DATA     SECTION.                                             
132100                                                                          
132200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
132300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
132400                                                                          
132500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN                       
132600                        RESP-AREA                                         
132700                                                                          
132800     IF SUB-KDRC > 0                                                      
132900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
133000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
133100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
133200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
133300     END-IF                                                               
133400     .                                                                    
133500     SKIP3                                                                
133600 S03-SEND-OPEN SECTION.                                                   
133700                                                                          
133800     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
133900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
134000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
134100                         SEND-OPEN-AREA                                   
134200     IF SEND-KDRC > ZERO                                                  
134300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
134400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
134500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
134600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
134700     END-IF                                                               
134800     .                                                                    
134900     SKIP3                                                                
135000 S03-PUT-HEADER SECTION.                                                  
135100                                                                          
135200     MOVE 'PUT'                           TO SEND-KDFUNC                  
135300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
135400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
135500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
135600                         SEND-KVDLEN                                      
135700                         HDR-AREA                                         
135800     IF SEND-KDRC > ZERO                                                  
135900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
136000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
136100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
136200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
136300     END-IF                                                               
136400     .                                                                    
136500     EJECT                                                                
136600 S03-PUT-REPORT-HEAD    SECTION.                                          
136700                                                                          
136800     MOVE 'PUT'                           TO SEND-KDFUNC                  
136900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
137000     MOVE LENGTH OF RESP-AREA-HEAD        TO SEND-KVDLEN                  
137100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
137200                         SEND-KVDLEN                                      
137300                         RESP-AREA-HEAD                                   
137400     IF SEND-KDRC > ZERO                                                  
137500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
137600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
137700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
137800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
137900     END-IF                                                               
138000     .                                                                    
138100     SKIP3                                                                
138200 S03-PUT-REPORT-LINE    SECTION.                                          
138300                                                                          
138400     MOVE 'PUT'                           TO SEND-KDFUNC                  
138500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
138600     MOVE LENGTH OF RESP-AREA-LINE        TO SEND-KVDLEN                  
138700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
138800                         SEND-KVDLEN                                      
138900                         RESP-AREA-LINE                                   
139000     IF SEND-KDRC > ZERO                                                  
139100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
139200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
139300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
139400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
139500     END-IF                                                               
139600     .                                                                    
139700     SKIP3                                                                
139800 S03-PUT-REPORT-TOTAL SECTION.                                            
139900                                                                          
140000     MOVE 'PUT'                           TO SEND-KDFUNC                  
140100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140200     MOVE LENGTH OF RESP-AREA-TOTAL       TO SEND-KVDLEN                  
140300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140400                         SEND-KVDLEN                                      
140500                         RESP-AREA-TOTAL                                  
140600     IF SEND-KDRC > ZERO                                                  
140700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
141000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
141100     END-IF                                                               
141200     .                                                                    
141300     SKIP3                                                                
141400 S03-SEND-CLOSE SECTION.                                                  
141500                                                                          
141600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
141700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
141800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
141900     .                                                                    
142000     EJECT                                                                
142100* IMS SEKTIONER                                                           
142200                                                                          
142300     EJECT                                                                
142400 IMS-GU-E601-KVAL SECTION.                                                
142500                                                                          
142600     MOVE 'WDE601  '      TO SSA1                                         
142700     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
142800            DELIMITED BY SIZE INTO SSA1                                   
142900     MOVE '  ' TO GODK-STATUSKODER                                        
143000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
143100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
143200     PERFORM IMS-STATUSKONTROLL                                           
143300     .                                                                    
143400     SKIP3                                                                
143500 IMS-GU-E401-KVAL-SEK SECTION.                                            
143600     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
143700            DELIMITED BY SIZE INTO SSA1                                   
143800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
143900     CALL CBLTDLI USING GU                                                
144000                          WDE4A-PCB                                       
144100                          DLI-IO-AREA2                                    
144200                          SSA1                                            
144300     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
144400                               STATUS-WDE401-SEK-WS                       
144500     PERFORM IMS-STATUSKONTROLL                                           
144600     .                                                                    
144700     SKIP3                                                                
144800 IMS-GU-WDE411-01-FSEQ SECTION.                                           
144900     STRING 'WDE411  (WDE4FSEQ =' W-WDE4F1KY ')'                          
145000            DELIMITED BY SIZE INTO SSA1                                   
145100     MOVE 'WDE401' TO SSA2                                                
145200     MOVE '  GE' TO GODK-STATUSKODER                                      
145300     CALL CBLTDLI USING GU WDE4F-PCB WDE4F-401-AREA SSA1 SSA2             
145400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
145500                               STATUS-WS-RAD                              
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     .                                                                    
145800     EJECT                                                                
145900 IMS-GU-WDE411-21-FSEQ SECTION.                                           
146000     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4F1KY ')'                        
146100            DELIMITED BY SIZE INTO SSA1                                   
146200     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
146300            DELIMITED BY SIZE INTO SSA2                                   
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GU WDE4F-PCB WDE4F-AREA SSA1 SSA2                 
146600     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
146700                               STATUS-WS-RAD                              
146800     PERFORM IMS-STATUSKONTROLL                                           
146900     .                                                                    
147000     EJECT                                                                
147100 IMS-GN-WDE411-21-FSEQ SECTION.                                           
147200     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4F1KY ')'                        
147300            DELIMITED BY SIZE INTO SSA1                                   
147400     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
147500            DELIMITED BY SIZE INTO SSA2                                   
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GN WDE4F-PCB WDE4F-AREA SSA1 SSA2                 
147800     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
147900                               STATUS-WS-RAD                              
148000     PERFORM IMS-STATUSKONTROLL                                           
148100     .                                                                    
148200     EJECT                                                                
148300 IMS-GN-E401-KVAL-SEK SECTION.                                            
148400     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
148500            DELIMITED BY SIZE INTO SSA1                                   
148600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
148700     CALL CBLTDLI USING GN                                                
148800                          WDE4A-PCB                                       
148900                          DLI-IO-AREA2                                    
149000                          SSA1                                            
149100     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
149200                               STATUS-WDE401-SEK-WS                       
149300     PERFORM IMS-STATUSKONTROLL                                           
149400     .                                                                    
149500     EJECT                                                                
149600 IMS-GU-GMTA01-WDB201      SECTION.                                       
149700     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
149800            DELIMITED BY SIZE INTO SSA1                                   
149900     MOVE '  GE' TO GODK-STATUSKODER                                      
150000     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
150100     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
150200     PERFORM IMS-STATUSKONTROLL                                           
150300     SKIP3                                                                
150400     .                                                                    
150500 IMS-GU-BETC01-WDB101      SECTION.                                       
150600     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
150700            DELIMITED BY SIZE INTO SSA1                                   
150800     MOVE '  GE' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING GU BETC-PCB BET-WDB101 SSA1                       
151000     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     SKIP3                                                                
151300     .                                                                    
151400 IMS-GNP-E611-KVAL SECTION.                                               
151500                                                                          
151600     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
151700            DELIMITED BY SIZE INTO SSA1                                   
151800     MOVE '  GE' TO GODK-STATUSKODER                                      
151900     CALL CBLTDLI USING GNP                                               
152000                          WDE6-PCB                                        
152100                          DLI-IO-AREA                                     
152200                          SSA1                                            
152300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
152400                              STATUS-WS-KOLLI                             
152500     PERFORM IMS-STATUSKONTROLL                                           
152600     .                                                                    
152700     SKIP3                                                                
152800 IMS-GNP-E611-MIN-MAX   SECTION.                                          
152900                                                                          
153000     STRING 'WDE611  (IDKOLLI >=' W-E611-IDKOLLI-X                        
153100                    '&IDKOLLI <=' W-E611-IDKOLLI-TOM-X ')'                
153200            DELIMITED BY SIZE INTO SSA1                                   
153300     MOVE '  GE' TO GODK-STATUSKODER                                      
153400     CALL CBLTDLI USING GNP                                               
153500                          WDE6-PCB                                        
153600                          DLI-IO-AREA                                     
153700                          SSA1                                            
153800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
153900                              STATUS-WS-KOLLI                             
154000     PERFORM IMS-STATUSKONTROLL                                           
154100     .                                                                    
154200     SKIP3                                                                
154300 IMS-GU-ORQI        SECTION.                                              
154400                                                                          
154500     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X  ')'                        
154600            DELIMITED BY SIZE INTO SSA1                                   
154700     MOVE '  ' TO GODK-STATUSKODER                                        
154800     CALL CBLTDLI USING GU                                                
154900                        ORQI-PCB                                          
155000                        DLI-IO-AREA                                       
155100                        SSA1                                              
155200     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     EJECT                                                                
155600 IMS-GET-BENA            SECTION.                                         
155700                                                                          
155800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
155900            DELIMITED BY SIZE INTO SSA1                                   
156000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
156100            DELIMITED BY SIZE INTO SSA2                                   
156200     MOVE '  GE' TO GODK-STATUSKODER                                      
156300     CALL  CBLTDLI  USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2               
156400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     SKIP2                                                                
156700     .                                                                    
156800                                                                          
156900 IMS-GU-WDB601    SECTION.                                                
157000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
157100          DELIMITED BY SIZE INTO SSA1                                     
157200     MOVE '  '   TO GODK-STATUSKODER                                      
157300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
157400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
157500     PERFORM IMS-STATUSKONTROLL                                           
157600     .                                                                    
157700 IMS-GU-WDQ301    SECTION.                                                
157800     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
157900          DELIMITED BY SIZE INTO SSA1                                     
158000     MOVE '  '   TO GODK-STATUSKODER                                      
158100     CALL CBLTDLI USING GU WDQ3-PCB ODEL-WDQ301 SSA1                      
158200     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     SKIP2                                                                
158500     .                                                                    
158600 IMS-GU-WDF502 SECTION.                                                   
158700     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
158800            DELIMITED BY SIZE INTO SSA1                                   
158900     MOVE 'WDF502  '       TO SSA2                                        
159000     MOVE '  GE'           TO GODK-STATUSKODER                            
159100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
159200     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
159300     PERFORM IMS-STATUSKONTROLL                                           
159400     .                                                                    
159500                                                                          
159600 IMS-ROLLBACK    SECTION.                                                 
159700                                                                          
159800     CALL CBLTDLI USING ROLB    MSG-PCB                                   
159900     .                                                                    
160000     SKIP2                                                                
160100                                                                          
160200 IMS-STATUSKONTROLL SECTION.                                              
160300     SET STATUS-IX TO 1                                                   
160400     SEARCH GODK-STATUS                                                   
160500       AT END                                                             
160600         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
160700           DELIMITED BY SIZE INTO FELTEXT                                 
160800           CALL FELLOG                                                    
160900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
161000         CONTINUE                                                         
161100     END-SEARCH                                                           
161200     .                                                                    
