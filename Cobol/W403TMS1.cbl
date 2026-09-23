000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W403TMS1.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JANUARI  2022.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION.                                                            
000900*                                                                         
001000*        W403TMS1 SENDS EVENT TRANSACTION TO TMS.                         
001100*                                                                         
001200*    LÄNKAREA :       W403TMS1                                            
001300*                                                                         
001400*                                                                         
001500 DATA DIVISION.                                                           
001600                                                                          
001700 WORKING-STORAGE SECTION.                                                 
001800 77    IDPGM                     PIC X(8)    VALUE 'W403TMS1'.            
001900 77    ERRORTEXT                 PIC X(64)   VALUE SPACE.                 
002000 77    FILLER                    PIC X(8)    VALUE 'CURRENT'.             
002100 77    CURRENT-SECTION           PIC X(30)   VALUE SPACE.                 
002200 77    FILLER                    PIC X(8)    VALUE 'PGMPOS'.              
002300 77    WS-PGM-POSITION           PIC X(32)   VALUE SPACE.                 
002400 77    JA                        PIC X       VALUE 'J'.                   
002500 77    FILLER                    PIC X(8)    VALUE 'IMSSEC'.              
002600 77    IMS-SECTION               PIC X(32)   VALUE SPACE.                 
002700 77    YES                       PIC X       VALUE 'Y'.                   
002800 77    NEJ                       PIC X       VALUE 'N'.                   
002900 77    WS-FALSE                  PIC X       VALUE X'00'.                 
003000 77    WS-TRUE                   PIC X       VALUE X'01'.                 
003100 77    KDRC-DISPLAY              PIC Z(5).                                
003200 77    TABLE-SW                  PIC X       VALUE 'N'.                   
003300 77    EMPTY-SW                  PIC X       VALUE 'N'.                   
003400 77    RAETT                     PIC X       VALUE 'R'.                   
003500 77    FEL                       PIC X       VALUE 'F'.                   
003600 77    SAKNAS                    PIC X       VALUE 'S'.                   
003700 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
003800 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
004200 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
004300 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
004400 77    WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.          
004500 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
004600 77    WS-EMBPROF                PIC X(1)   VALUE SPACE.                  
004700 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
004800 77    WS-KDEMBTYP               PIC 9(1)   VALUE ZERO.                   
004900 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
005000 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
005100 77    WS-TIHHMM                 PIC 9(4)   VALUE ZERO.                   
005200 77    WS-LOCAL-LANG             PIC X(2).                                
005300 77    WS-TEPSNNOT               PIC X(225).                              
005400 77    WS-BEPSN                  PIC X(75).                               
005500 77    WS-BEART                  PIC X(25).                               
005600 77    WS-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005700 77    WS-TMS-IDDC-NUM           PIC  9(2).                               
005800 77    WS-IDARTNR                PIC Z(8)9.                               
005900 77    WS-IDSTATNR               PIC Z(8)9.                               
006000 77    WS-HOME-IDDC              PIC X(2).                                
006100 77    WS-REFERAL-IDDC           PIC X(2).                                
006200 77    WS-FACING-IDDC            PIC X(2).                                
006300 77    WS-IDLEVNR                PIC X(5) VALUE SPACES.                   
006400 77    WS-PROC-PARTY-CNT         PIC 9(1) VALUE 0.                        
006500 77    WS-PRICE                  PIC 9(7)V9(2) VALUE 0.                   
006600 77    WS-KDVALISO               PIC X(3) VALUE SPACES.                   
006700 77    HELP-PRARTNTO             PIC 9(7)V9(2) VALUE 0.                   
006800 77    WS-VLORDBTO               PIC S9(4)V9(3) COMP-3.                   
006900 77    WS-DIKOLLIL               PIC S9(5)   COMP-3.                      
007000 77    WS-DIKOLLIB               PIC S9(3)   COMP-3.                      
007100 77    WS-DIKOLLIH               PIC S9(3)   COMP-3.                      
007200 77    WS-DGDESC-START           PIC 9(3).                                
007300 77    WS-IDDISTR                PIC S9(5)   COMP-3.                      
007400 77    WS-IDKUNDNR               PIC S9(7)   COMP-3.                      
007500 77    WS-IDORDNR                PIC 9(7) VALUE 0.                        
007600 77    WS-IDPRODNR               PIC 9(7) VALUE 0.                        
007700                                                                          
007800 77  SW-DG-FLAG                  PIC X      VALUE 'N'.                    
007900     88  SW-DG-EXISTS                       VALUE 'Y'.                    
008000     88  SW-DG-NOT-EXIST                    VALUE 'N'.                    
008100 77  SW-DG-DESC                  PIC X      VALUE 'N'.                    
008200     88  SW-DG-DESC-FOUND                   VALUE 'Y'.                    
008300     88  SW-DG-DESC-NOT-FOUND               VALUE 'N'.                    
008400 77  SW-DG-NOTE                  PIC X      VALUE 'N'.                    
008500     88  SW-DG-NOTE-FOUND                   VALUE 'Y'.                    
008600     88  SW-DG-NOTE-NOT-FOUND               VALUE 'N'.                    
008700 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008800 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
008900 77  TMS-IX                      PIC  9(9)  VALUE ZERO.                   
009000 77  TMS-MAX                     PIC S9(9)  VALUE +1500 COMP SYNC.        
009100 77  DANG-IX                     PIC S9(9)  VALUE ZERO  COMP SYNC.        
009200 77  PSN-IX                      PIC S9(9)  VALUE ZERO  COMP SYNC.        
009300 77  CASE-IX                     PIC S9(9)  VALUE ZERO  COMP SYNC.        
009400                                                                          
009500 01  WS-DCUSER.                                                           
009600     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
009700     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
009800     03 FILLER                   PIC X(1)   VALUE SPACE.                  
009900                                                                          
010000 01  WS-LANG-COUNTRY-LOC.                                                 
010100     03 WS-LANGUAGE-LOC          PIC X(2)   VALUE SPACE.                  
010200     03 FILLER                   PIC X(1)   VALUE '_'.                    
010300     03 WS-COUNTRY-LOC           PIC X(2)   VALUE SPACE.                  
010400                                                                          
010500 01  WS-LANG-COUNTRY-GB.                                                  
010600     03 WS-LANGUAGE-GB           PIC X(2)   VALUE 'en'.                   
010700     03 FILLER                   PIC X(1)   VALUE '_'.                    
010800     03 WS-COUNTRY-GB            PIC X(2)   VALUE 'GB'.                   
010900                                                                          
011000 01  WS-LANG-COUNTRY-GRP.                                                 
011100     03 WS-LANGUAGE              PIC X(2)   VALUE SPACE.                  
011200     03 FILLER                   PIC X(1)   VALUE '_'.                    
011300     03 WS-COUNTRY               PIC X(2)   VALUE SPACE.                  
011400                                                                          
011500 01    WS-TID-W.                                                          
011600   03  WS-TTMMSS                 PIC 9(6).                                
011700   03  WS-HH                     PIC 9(2).                                
011800 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
011900                                                                          
012000 01  ARBETSFALT.                                                          
012100                                                                          
012200     03 WS-9KOMPL-GRUND          PIC 9(9) VALUE 999999999.                
012300*                                                                         
012400*    03 WS-STRING.                                                        
012500        03 WS-SPACE              PIC X(10)   VALUE ZERO.                  
012600*                                                                         
012700     03 WS-CUSTOMER-Z.                                                    
012800        05 WS-IDDISTR-Z          PIC ZZZZ.                                
012900        05 WS-IDKUNDNR-Z         PIC ZZZZZZ.                              
013000*                                                                         
013100     03 WS-CUSTOMERIDENTIFIER.                                            
013200        05 WS-IDDISTR-CUSTOM     PIC Z(3)9  VALUE ZERO.                   
013300        05 FILLER                PIC X(1)   VALUE '-'.                    
013400        05 WS-IDKUNDNR-CUSTOM    PIC Z(5)9  VALUE ZERO.                   
013500*                                                                         
013600     03 WS-PARCELIDENTIFIER.                                              
013700        05 WS-IDDISTR-PARCEL     PIC 9(4)   VALUE ZERO.                   
013800        05 WS-IDKUNDNR-PARCEL    PIC 9(6)   VALUE ZERO.                   
013900        05 WS-IDORDNR7-PARCEL    PIC 9(7)   VALUE ZERO.                   
014000        05 WS-IDKOLLI-PARCEL     PIC 9(5)   VALUE ZERO.                   
014100*                                                                         
014200     03 WS-ORDERIDENTIFIER.                                               
014300        05 WS-IDDISTR-ORDER      PIC 9(4)   VALUE ZERO.                   
014400        05 WS-IDKUNDNR-ORDER     PIC 9(6)   VALUE ZERO.                   
014500        05 WS-IDORDNR7-ORDER     PIC 9(7)   VALUE ZERO.                   
014600        05 WS-TIORDREG-ORDER     PIC 9(6)   VALUE ZERO.                   
014700*                                                                         
014800     03 WS-ORDERTYPE.                                                     
014900        05 WS-KDORDKL-TYPE       PIC 9(1)   VALUE ZERO.                   
015000        05 WS-KDFRAKT-TYPE       PIC 9(2)   VALUE ZERO.                   
015100                                                                          
015200 77    FILLER                    PIC X(8)    VALUE 'KKKKKKKK'.            
015300*                                                                         
015400 01   WS-TRP-DATE.                                                        
015500   03 WS-TRP-SEKEL             PIC 9(02)  VALUE ZERO.                     
015600   03 WS-TRP-YEAR              PIC 9(02)  VALUE ZERO.                     
015700   03 WS-TRP-MONTH             PIC 9(02)  VALUE ZERO.                     
015800   03 WS-TRP-DAY               PIC 9(02)  VALUE ZERO.                     
015900 01   WS-TRP-TIME.                                                        
016000   03 WS-TRP-HOUR              PIC 9(02)  VALUE ZERO.                     
016100   03 WS-TRP-MIN               PIC 9(02)  VALUE ZERO.                     
016200*                                                                         
016300 01    WS-DATRP-GRP.                                                      
016400   03  WS-DATRP-SEKEL            PIC 9(02)  VALUE ZERO.                   
016500   03  WS-DATRP-DATE.                                                     
016600     05 WS-DATRP-YEAR            PIC 9(02)  VALUE ZERO.                   
016700     05 FILLER                   PIC X(01)  VALUE '-'.                    
016800     05 WS-DATRP-MONTH           PIC 9(02)  VALUE ZERO.                   
016900     05 FILLER                   PIC X(01)  VALUE '-'.                    
017000     05 WS-DATRP-DAY             PIC 9(02)  VALUE ZERO.                   
017100     05 FILLER                   PIC X(01)  VALUE 'T'.                    
017200   03 WS-DATRP-TIME.                                                      
017300     05 WS-DATRP-HOUR            PIC 9(02)  VALUE ZERO.                   
017400     05 FILLER                   PIC X(01)  VALUE ':'.                    
017500     05 WS-DATRP-MIN             PIC 9(02)  VALUE ZERO.                   
017600     05 FILLER                   PIC X(01)  VALUE ':'.                    
017700     05 WS-DATRP-SEC             PIC 9(02)  VALUE ZERO.                   
017800     05 FILLER                   PIC X(01)  VALUE '.'.                    
017900     05 WS-DATRP-HUN             PIC 9(03)  VALUE ZERO.                   
018000     05 FILLER                   PIC X(01)  VALUE 'Z'.                    
018100*                                                                         
018200 01  WS-DATE-FIELDS.                                                      
018300     03  WS-DATE                 PIC 9(8).                                
018400     03  FILLER REDEFINES WS-DATE.                                        
018500         05  WS-DATE-CC          PIC 9(2).                                
018600         05  WS-DATE-YYMMDD      PIC 9(6).                                
018700     03  WS-TIME                 PIC 9(6).                                
018800     03  FILLER REDEFINES WS-TIME.                                        
018900         05  WS-TIME-HHMM        PIC 9(4).                                
019000         05  WS-TIME-SS          PIC 9(2).                                
019100                                                                          
019200 01  WS-DATE-UTC-FIELDS.                                                  
019300     03  WS-DATE-UTC             PIC 9(8).                                
019400     03  FILLER REDEFINES WS-DATE-UTC.                                    
019500         05  WS-DATE-UTC-CC      PIC 9(2).                                
019600         05  WS-DATE-UTC-YYMMDD  PIC 9(6).                                
019700     03  WS-TIME-UTC             PIC 9(6).                                
019800     03  WS-TIME-UTC-X REDEFINES WS-TIME-UTC.                             
019900         05  WS-TIME-UTC-HHMM    PIC 9(4).                                
020000         05  WS-TIME-UTC-SS      PIC 9(2).                                
020100                                                                          
020200 77  WS-TIMESTAMP-UTC            PIC X(20).                               
020300                                                                          
020400 77    FILLER                    PIC X(8)    VALUE 'LLLLLLLL'.            
020500*                                                                         
020600*                                                                         
020700 01     DYNAMIC-SUBPROGRAM.                                               
020800     03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
020900     03 FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
021000     03 ABEND                   PIC X(8)    VALUE 'ABEND   '.             
021100     03 WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.             
021200     03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.             
021300     03 WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.             
021400*    --- PARAMETERS TO ABEND                                              
021500                                                                          
021600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
021900*                                                                         
022000*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
022100*                                                                         
022200*01  -COPY WL01TIDZ                                                       
022300*                                                                         
022400*    --- PARAMETERS TO WZ01SEND FOR TMS                                   
022500 01  FILLER                      PIC X(16) VALUE 'WZ01SEND-TMS'.          
022600*01  -COPY WZ01SEND                                                       
022700                                                                          
022800*API INFO FILE                                                            
022900*    01  -COPY WAPIINFO                                                   
023000                                                                          
023100*01  FILLER                     PIC X(16) VALUE 'SENDAREA-TMS'.           
023200***  SEND-AREA-TO-TMS.                                                    
023300*                                                                         
023400 01  WTM00Q01-AREA.                                                       
023500*    03  -COPY WTM00Q01                                                   
023600*                                                                         
023700 01  WTM01Q01-AREA.                                                       
023800*    03  -COPY WTM01Q01 -PRE  DEL-                                        
023900*                                                                         
024000 01  FILLER                     PIC X(8)  VALUE 'WWLNDSPR'.               
024100*    -COPY WWLNDSPR                                                       
024200*                                                                         
024300 01  FILLER                     PIC X(8)  VALUE 'WWDC99  '.               
024400*01 -COPY WWDC99                                                          
024500*                                                                         
024600 01  FILLER                     PIC X(8)  VALUE 'WWDCKONS'.               
024700*01 -COPY WWDCKONS                                                        
024800*                                                                         
024900 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
025000 01     FILLER REDEFINES TEST-IDDISTR.                                    
025100*  03   -COPY WWDIST79.                                                   
025200                                                                          
025300 01     WS-TIDPUNKT-RED.                                                  
025400   03   WS-HHMMSS               PIC  9(6).                                
025500   03   WS-DD                   PIC  9(2).                                
025600                                                                          
025700 01     DAGDAT.                                                           
025800   03   DAGDAT-AAMMD             PIC 9(6).                                
025900   03   DAGDAT-AAMMDD-X      REDEFINES DAGDAT-AAMMD.                      
026000     05 DAGDAT-AAMMDD-AA         PIC 9(2).                                
026100     05 DAGDAT-AAMMDD-MM         PIC 9(2).                                
026200     05 DAGDAT-AAMMDD-DD         PIC 9(2).                                
026300*                                                                         
026400   03   DAGDAT-AAVVD             PIC 9(5).                                
026500   03   DAGDAT-AAVVD-X       REDEFINES DAGDAT-AAVVD.                      
026600     05 DAGDAT-AAVVD-AA          PIC 9(2).                                
026700     05 DAGDAT-AAVVD-VV          PIC 9(2).                                
026800     05 DAGDAT-AAVVD-D           PIC 9(1).                                
026900*                                                                         
027000*01  WDATAREA      -COPY WDATAREA.                                        
027100                                                                          
027200***************************************************************           
027300 01    NYCKLAR-TILL-DLI.                                                  
027400*                                                                         
027500   03    W-WDGX01-4141-X.                                                 
027600     05    W-IDHTYP-4141         PIC X(04)   VALUE '4141'.                
027700     05    W-LOW-VALUE           PIC X(26)   VALUE LOW-VALUE.             
027800*                                                                         
027900   03  W-WDB101KY-X.                                                      
028000     05  W-WDB1-IDPARTNR         PIC X(9)    VALUE SPACE.                 
028100     05  W-WDB1-IDFTG            PIC 9(2)    VALUE ZERO.                  
028200                                                                          
028300     03  W-KY4142-X.                                                      
028400         05  W-IDDC              PIC X(2).                                
028500         05  W-KDORDKL           PIC S9    COMP-3 VALUE ZERO.             
028600         05  W-KDFRAKT           PIC S9(3) COMP-3 VALUE ZERO.             
028700         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
028800         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
028900*                                                                         
029000   03  W-IDLAND-X.                                                        
029100      05  W-IDLAND               PIC X(2).                                
029200*                                                                         
029300     03  W-1165KEY-X.                                                     
029400         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
029500         05  W-IDPSN             PIC 9(3).                                
029600         05  W-IDSPRAK           PIC X(2).                                
029700         05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
029800                                                                          
029900     03  W-KDFGTRP-X.                                                     
030000         05  W-KDFGTRP           PIC 9(2)    VALUE 0.                     
030100                                                                          
030200*--------------------WDB2                                                 
030300     03  W-IDGMT-X.                                                       
030400         05  W-IDDISTR-WDB2      PIC S9(5) COMP-3 VALUE ZERO.             
030500         05  W-IDKUNDNR-WDB2     PIC S9(7) COMP-3 VALUE ZERO.             
030600                                                                          
030700     03 W-WDGXKEY-0103-X.                                                 
030800        05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                 
030900        05  FILLER              PIC X(26)   VALUE LOW-VALUE.              
031000     03 W-KY0104-X.                                                       
031100        05  W-ADDISPABS         PIC X(50)                                 
031200                              VALUE 'APIOUT.TMS.CREATEPARCEL'.            
031300     03 W-KY0104-DEL.                                                     
031400        05  W-ADDISPABS-DEL     PIC X(50)                                 
031500                              VALUE 'APIOUT.TMS.DELETEPARCEL'.            
031600                                                                          
031700*----> SEKUNDÄR INDEX ARTIKELBENÄMNING                                    
031800                                                                          
031900     03  FILLER                  PIC X(8)    VALUE ALL 'B'.               
032000     03  W-WDD3BSEQ-X.                                                    
032100         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
032200                                                                          
032300     03  W-IDSKYLT-X             PIC X(3).                                
032400                                                                          
032500   03    W-IDDC-B6-X.                                                     
032600     05    W-IDDC-B6             PIC  X(2).                               
032700                                                                          
032800   03    W-IDDC-PRIM-X.                                                   
032900     05    W-IDDC-PRIM           PIC  X(2).                               
033000*LK                                                                       
033100     03  W-WDE4ASEQ-X.                                                    
033200         05  W-ASEQ-IDDISTR      PIC S9(5)  VALUE ZERO COMP-3.            
033300         05  W-ASEQ-IDKUNDNR     PIC S9(7)  VALUE ZERO COMP-3.            
033400         05  W-ASEQ-IDKUNDRF.                                             
033500           07  W-ASEQ-IDORDNR        PIC X(5).                            
033600           07  FILLER                PIC X(5)     VALUE SPACE.            
033700                                                                          
033800     03  W-WDE4FSEQ-X.                                                    
033900         05  W-IDPRODNR-F        PIC S9(7)  VALUE ZERO  COMP-3.           
034000         05  W-IDKOLLI-F         PIC S9(5)  VALUE ZERO  COMP-3.           
034100                                                                          
034200     03  W-WDE421KY-X.                                                    
034300         05  W-IDPRODNR-421      PIC S9(7)   COMP-3.                      
034400         05  W-IDKOLLI-421       PIC S9(5)   COMP-3.                      
034500                                                                          
034600     03  W-WDQ201-X.                                                      
034700         05  W-Q201-IDORDER      PIC S9(7)   COMP-3.                      
034800                                                                          
034900     03  W-WDQ2CSEQ-X.                                                    
035000         05 W-WDQ2C-IDGMTREF-X.                                           
035100           07 W-WDQ2C-IDDISTR    PIC S9(5) COMP-3 VALUE +0.               
035200           07 W-WDQ2C-IDKUNDNR   PIC S9(7) COMP-3 VALUE +0.               
035300           07 W-WDQ2C-IDKUNDRF.                                           
035400         09    FILLER            PIC  9(2)        VALUE ZERO.             
035500         09    W-WDQ2C-IDORDNR5                                           
035600                                 PIC  9(5)        VALUE ZERO.             
035700         09    FILLER            PIC  X(3)        VALUE SPACE.            
035800     03  W-Q301-KEY-X.                                                    
035900         05  W-Q301-IDORDER      PIC S9(7)   COMP-3.                      
036000         05  W-Q301-IDDC         PIC X(2).                                
036100         05  W-Q301-IDPRODNR     PIC S9(7)   COMP-3.                      
036200         05  W-Q301-IDPLKLST     PIC S9(3)   COMP-3.                      
036300                                                                          
036400     03  W-IDARTNR-X.                                                     
036500         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
036600*                                                                         
036700     03  W-KDSEGKEY-X.                                                    
036800         05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                     
036900*                                                                         
037000     03  W-E6-IDPRODNR-X.                                                 
037100         05  W-E6-IDPRODNR     PIC S9(7)   COMP-3.                        
037200*                                                                         
037300     03  W-E6-IDKOLLI-X.                                                  
037400         05  W-E6-IDKOLLI      PIC S9(5)   COMP-3.                        
037500*                                                                         
037600     03  W-K5-KDKOLLI-X.                                                  
037700         05  W-K5-KDKOLLI      PIC  X(8)   VALUE SPACE.                   
037800                                                                          
037900*************************************************                         
038000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
038100*                                                                         
038200 01    IMS-WS.                                                            
038300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
038400*                        **** STATUS-KOD FRÅN IMS                         
038500   03    STATUS-RAD-WS           PIC XX.                                  
038600     88    RAD-FINNS                         VALUE '  '.                  
038700     88    RAD-SAKNAS                        VALUE 'GE'.                  
038800   03    STATUS-WS               PIC XX.                                  
038900     88    SEGMENT-FOUND                     VALUE '  '.                  
039000     88    ISRT-OK                           VALUE '  '.                  
039100     88    SEGMENT-END                       VALUE 'GB'.                  
039200     88    SEGMENT-MISSING                   VALUE 'GE'.                  
039300     88    SEGMENT-EXISTS                    VALUE 'II'.                  
039400   03    GODK-STATUSKODER.                                                
039500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
039600 01    SSA1                      PIC X(960).                              
039700 01    SSA2                      PIC X(320).                              
039800*                            IMS FUNKTIONSKODER                           
039900*01  -COPY W0003                                                          
040000*    ---  DLI INPUT-OUTPUT AREA                                           
040100                                                                          
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1165'.                    
040300 01  DLI-IO-WDGX1165.                                                     
040400*    03  -COPY WDGX1165                                                   
040500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1166'.                    
040600 01  DLI-IO-WDGX1166.                                                     
040700*    03  -COPY WDGX1166                                                   
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1168'.                    
040900 01  DLI-IO-WDGX1168.                                                     
041000*    03  -COPY WDGX1168                                                   
041100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
041200 01  DLI-IO-WDB201.                                                       
041300*    03  -COPY WDB201                                                     
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
041500 01  DLI-IO-WDR201.                                                       
041600     03  WDR201.                                                          
041700*        05  -COPY WDGX01                                                 
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-4142'.                        
041900 01  DLI-IO-4142.                                                         
042000*    03  -COPY WDGX4142                                                   
042100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
042200 01  DLI-IO-WDR501.                                                       
042300*    03   -COPY WDGX01                                                    
042400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX0104'.                    
042500 01  DLI-IO-WDGX0104.                                                     
042600*    03   -COPY WDGX0104                                                  
042700                                                                          
042800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
042900 01  DLI-IO-WDD301.                                                       
043000*    03  -COPY WDD301                                                     
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
043200 01  DLI-IO-WDB601.                                                       
043300*    03  -COPY WDB601                                                     
043400 01  FILLER         PIC X(16) VALUE 'DLI-IO-B6-PRIM'.                     
043500 01  DLI-IO-WDB601-PRIM.                                                  
043600*    03  -COPY WDB601 -PRE PRIM-                                          
043700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
043800 01  DLI-IO-WDD311.                                                       
043900*    03  -COPY WDD311                                                     
044000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
044100 01  DLI-IO-WDB101.                                                       
044200*    03  -COPY WDB101                                                     
044300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
044400 01  DLI-IO-WDE401.                                                       
044500*    03  -COPY WDE401 -PRE ASEQ-                                          
044600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4'.                        
044700 01  DLI-IO-WDE4.                                                         
044800*    03  -COPY WDE401                                                     
044900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
045000 01  DLI-IO-WDE421.                                                       
045100*    03  -COPY WDE411                                                     
045200*    03  -COPY WDE421                                                     
045300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
045400 01  DLI-IO-WDQ201.                                                       
045500*    03  -COPY WDQ201                                                     
045600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
045700 01  DLI-IO-WDQ301.                                                       
045800*    03  -COPY WDQ301                                                     
045900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
046000 01  DLI-IO-WDK601.                                                       
046100*    03  -COPY WDK601                                                     
046200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
046300 01  DLI-IO-WDK611.                                                       
046400*    03  -COPY WDK611                                                     
046500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
046600 01  DLI-IO-WDE611.                                                       
046700*    03  -COPY WDE611                                                     
046800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
046900 01  DLI-IO-WDK501.                                                       
047000*    03  -COPY WDK501                                                     
047100 01  FILLER         PIC X(16) VALUE 'DLI-IO-ORQI01'.                      
047200 01  DLI-IO-ORQI01.                                                       
047300*    03  -COPY WDQ201  -PRE CSEQ-                                         
047400                                                                          
047500 LINKAGE SECTION.                                                         
047600*                                                                         
047700*    -COPY W403TMS1                                                       
047800 01  TMS-CRE-PCB                 PIC X.                                   
047900 01  TMS-DEL-PCB                 PIC X.                                   
048000*01    -COPY W0008     -PRE ATAB-                                         
048100     05  FILLER                  PIC X.                                   
048200*01    -COPY W0008     -PRE 1165-                                         
048300     05  FILLER                  PIC X.                                   
048400*01    -COPY W0008     -PRE 4141-                                         
048500     05  FILLER                  PIC X.                                   
048600*01    -COPY W0008     -PRE WDB2-                                         
048700     05  FILLER                  PIC X.                                   
048800*01    -COPY W0008     -PRE WDB6-                                         
048900     05  FILLER                  PIC X.                                   
049000*01    -COPY W0008     -PRE WDD3-                                         
049100     05  FILLER                  PIC X.                                   
049200*01    -COPY W0008     -PRE WDB1-                                         
049300     05  FILLER                  PIC X.                                   
049400*01    -COPY W0008     -PRE WDE4A-                                        
049500     05  FILLER                  PIC X.                                   
049600*01    -COPY W0008     -PRE WDE4F-                                        
049700     05  FILLER                  PIC X.                                   
049800*01    -COPY W0008     -PRE WDQ2-                                         
049900     05  FILLER                  PIC X.                                   
050000*01    -COPY W0008     -PRE WDQ3-                                         
050100     05  FILLER                  PIC X.                                   
050200*01    -COPY W0008     -PRE WDK6-                                         
050300     05  FILLER                  PIC X.                                   
050400*01    -COPY W0008     -PRE WDE6-                                         
050500     05  FILLER                  PIC X.                                   
050600*01    -COPY W0008     -PRE WDK5-                                         
050700     05  FILLER                  PIC X.                                   
050800*01    -COPY W0008     -PRE WDQ2C-                                        
050900     05  FILLER                  PIC X.                                   
051000                                                                          
051100 PROCEDURE DIVISION USING TMS-W403TMS1                                    
051200                          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                
051300                          1165-PCB 4141-PCB WDB2-PCB WDB6-PCB             
051400                          WDD3-PCB WDB1-PCB WDE4A-PCB WDE4F-PCB           
051500                          WDQ2-PCB WDQ3-PCB WDK6-PCB WDE6-PCB             
051600                          WDK5-PCB WDQ2C-PCB.                             
051700 MAIN SECTION.                                                            
051800                                                                          
051900     PERFORM A-INIT                                                       
052000                                                                          
052100     PERFORM B-READ-TMS-PROCESS-TABEL                                     
052200     IF TABLE-SW = 'Y'                                                    
052300                                                                          
052400       IF EMPTY-SW = 'Y'                                                  
052500         PERFORM G-SEND-TMS-EMPTY                                         
052600       END-IF                                                             
052700       PERFORM C-GET-RECIEVER-INFO                                        
052800       PERFORM D-ADD-TO-TMS-INFO                                          
052900       PERFORM UNTIL TMS-IDKOLLI (CASE-IX) = ZERO                         
053000         PERFORM E-CASE-LINES-INFO                                        
053100         PERFORM F-SEND-TMS                                               
053200         ADD +1 TO CASE-IX                                                
053300       END-PERFORM                                                        
053400     END-IF                                                               
053500     MOVE ZERO TO RETURN-CODE                                             
053600     GOBACK                                                               
053700     .                                                                    
053800                                                                          
053900 A-INIT             SECTION.                                              
054000                                                                          
054100     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
054200     ACCEPT WS-TIDPUNKT                   FROM TIME                       
054300     ACCEPT WS-TID-W                      FROM TIME                       
054400                                                                          
054500     MOVE DAT-TIAAVVD            TO   DAGDAT-AAVVD                        
054600     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
054700     CALL WDATKONV USING DAT-KDDATFORM                                    
054800                         DAT-I-TIDATUM                                    
054900                         DAT-O-TIDATUM                                    
055000                         DAT-KDSVAR                                       
055100                                                                          
055200     MOVE DAT-TIAAMMDD           TO   DAGDAT-AAMMD                        
055300     MOVE DAT-TIAAVVD            TO   DAGDAT-AAVVD                        
055400                                                                          
055500     MOVE 'N' TO TABLE-SW                                                 
055600     MOVE 'N' TO EMPTY-SW                                                 
055700     MOVE +1         TO CASE-IX                                           
055800     MOVE ZERO       TO WS-IDPRODNR                                       
055900     .                                                                    
056000                                                                          
056100 B-READ-TMS-PROCESS-TABEL     SECTION.                                    
056200     MOVE 'B-READ-TMS-PROCESS-TABEL'  TO CURRENT-SECTION                  
056300                                                                          
056400     IF TMS-IDPRODNR > 0                                                  
056500       MOVE TMS-IDPRODNR           TO WS-IDPRODNR                         
056600     ELSE                                                                 
056700       MOVE TMS-IDDISTR         TO W-ASEQ-IDDISTR                         
056800       MOVE TMS-IDKUNDNR        TO W-ASEQ-IDKUNDNR                        
056900       MOVE TMS-IDORDNR7(3:5)   TO W-ASEQ-IDORDNR                         
057000       PERFORM IMS-GU-WDE401-ASEQ                                         
057100       IF SEGMENT-FOUND                                                   
057200         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                     
057300                    OR ( ASEQ-KORD-IDDC = TMS-IDDC                        
057400                   AND ASEQ-KORD-KVORDRAD-LEVPL = 0 )                     
057500            PERFORM IMS-GN-WDE401-ASEQ                                    
057600         END-PERFORM                                                      
057700         IF ASEQ-KORD-IDDC = TMS-IDDC                                     
057800           MOVE ASEQ-KORD-IDPRODNR   TO WS-IDPRODNR                       
057900           MOVE ASEQ-KORD-IDDISTR    TO WS-IDDISTR                        
058000           MOVE ASEQ-KORD-IDKUNDNR   TO WS-IDKUNDNR                       
058100           MOVE ASEQ-KORD-IDORDNR5   TO WS-IDORDNR                        
058200         END-IF                                                           
058300       END-IF                                                             
058400     END-IF                                                               
058500                                                                          
058600     IF WS-IDPRODNR > ZERO                                                
058700       MOVE WS-IDPRODNR              TO W-IDPRODNR-F                      
058800                                      W-E6-IDPRODNR                       
058900       MOVE TMS-IDKOLLI (1)          TO W-IDKOLLI-F                       
059000                                      W-E6-IDKOLLI                        
059100       PERFORM IMS-GU-WDE401-FSEQ                                         
059200*LK when old case is empty after moving all lines from 4343               
059300       IF SEGMENT-MISSING                                                 
059400         PERFORM BA-SKAPA-EMPTY-CASE                                      
059500         MOVE      TMS-IDKOLLI (2)   TO W-IDKOLLI-F                       
059600                                    W-E6-IDKOLLI                          
059700         PERFORM IMS-GU-WDE401-FSEQ                                       
059800         ADD +1 TO CASE-IX                                                
059900       END-IF                                                             
060000                                                                          
060100       IF SEGMENT-FOUND                                                   
060200         MOVE KORD-IDDC         TO W-IDDC                                 
060300         MOVE KORD-KDORDKL      TO W-KDORDKL                              
060400         MOVE KORD-KDFRAKT      TO W-KDFRAKT                              
060500         MOVE KORD-IDDISTR      TO W-IDDISTR                              
060600                                   WS-IDDISTR                             
060700         MOVE KORD-IDKUNDNR     TO W-IDKUNDNR                             
060800                                   WS-IDKUNDNR                            
060900         MOVE KORD-IDORDNR5     TO WS-IDORDNR                             
061000                                                                          
061100         PERFORM IMS-GU-WDGX4142                                          
061200                                                                          
061300         IF SEGMENT-FOUND                                                 
061400           MOVE 'Y' TO TABLE-SW                                           
061500         ELSE                                                             
061600           MOVE ZERO            TO W-IDKUNDNR                             
061700           PERFORM IMS-GU-WDGX4142                                        
061800           IF SEGMENT-FOUND                                               
061900             MOVE 'Y' TO TABLE-SW                                         
062000           ELSE                                                           
062100             MOVE ZERO          TO W-IDDISTR                              
062200             PERFORM IMS-GU-WDGX4142                                      
062300             IF SEGMENT-FOUND                                             
062400               MOVE 'Y' TO TABLE-SW                                       
062500             ELSE                                                         
062600               MOVE ZERO        TO W-KDFRAKT                              
062700               PERFORM IMS-GU-WDGX4142                                    
062800               IF SEGMENT-FOUND                                           
062900                 MOVE 'Y' TO TABLE-SW                                     
063000               ELSE                                                       
063100                 MOVE +9        TO W-KDORDKL                              
063200                 PERFORM IMS-GU-WDGX4142                                  
063300                 IF SEGMENT-FOUND                                         
063400                   MOVE 'Y' TO TABLE-SW                                   
063500                 END-IF                                                   
063600               END-IF                                                     
063700             END-IF                                                       
063800           END-IF                                                         
063900         END-IF                                                           
064000       END-IF                                                             
064100     END-IF                                                               
064200                                                                          
064300     IF TABLE-SW = 'Y'                                                    
064400       MOVE 4142-IDACCNT   TO ACCOUNTINDICATOR2                           
064500       MOVE 4142-KDTMSPROC TO DATAROUTING                                 
064600       MOVE 4142-KDTRPPROC TO TRANSPORTPROCESS                            
064700                                                                          
064800       COMPUTE DATAROUTING-LENGTH = FUNCTION BYTE-LENGTH(                 
064900               FUNCTION TRIM (DATAROUTING))                               
065000                                                                          
065100       COMPUTE TRANSPORTPROCESS-LENGTH =                                  
065200               FUNCTION BYTE-LENGTH (                                     
065300               FUNCTION TRIM (TRANSPORTPROCESS))                          
065400                                                                          
065500       MOVE 1           TO ACCOUNTINDICATOR-NUM                           
065600       COMPUTE ACCOUNTINDICATOR2-LENGTH =                                 
065700               FUNCTION BYTE-LENGTH (                                     
065800               FUNCTION TRIM (ACCOUNTINDICATOR2))                         
065900     END-IF                                                               
066000     .                                                                    
066100                                                                          
066200 BA-SKAPA-EMPTY-CASE  SECTION.                                            
066300     MOVE 'BA-SKAPA-EMPTY-CASE'  TO CURRENT-SECTION                       
066400                                                                          
066500     MOVE 'Y'                    TO EMPTY-SW                              
066600     MOVE WS-IDDISTR             TO WS-IDDISTR-Z                          
066700     MOVE WS-IDKUNDNR            TO WS-IDKUNDNR-Z                         
066800                                                                          
066900     MOVE SPACES                 TO DEL-CUSTOMERIDENTIFIER                
067000     STRING FUNCTION TRIM (WS-IDDISTR-Z) DELIMITED BY SIZE                
067100            '-'        DELIMITED BY SIZE                                  
067200            FUNCTION TRIM (WS-IDKUNDNR-Z) DELIMITED BY SIZE               
067300     INTO DEL-CUSTOMERIDENTIFIER                                          
067400     COMPUTE DEL-CUSTOMERIDENTIFIER-LENGTH                                
067500                                  = FUNCTION BYTE-LENGTH (                
067600                  FUNCTION TRIM (DEL-CUSTOMERIDENTIFIER))                 
067700                                                                          
067800     MOVE WS-IDDISTR             TO WS-IDDISTR-PARCEL                     
067900     MOVE WS-IDKUNDNR            TO WS-IDKUNDNR-PARCEL                    
068000     MOVE WS-IDORDNR             TO WS-IDORDNR7-PARCEL                    
068100     MOVE TMS-IDKOLLI(CASE-IX)   TO WS-IDKOLLI-PARCEL                     
068200     MOVE WS-PARCELIDENTIFIER    TO DEL-PARCELIDENTIFIER                  
068300     COMPUTE DEL-PARCELIDENTIFIER-LENGTH                                  
068400                                 = FUNCTION BYTE-LENGTH (                 
068500             FUNCTION TRIM (DEL-PARCELIDENTIFIER))                        
068600                                                                          
068700     MOVE "VOLVOCARS_PULS"       TO DEL-ISSUINGAPPLICATION                
068800     MOVE 14                     TO DEL-ISSUINGAPPLICATION-LENGTH         
068900                                                                          
069000     .                                                                    
069100                                                                          
069200 C-GET-RECIEVER-INFO   SECTION.                                           
069300     MOVE 'C-GET-RECIEVER-INFO ' TO CURRENT-SECTION                       
069400                                                                          
069500     MOVE WS-IDDISTR             TO  W-IDDISTR-WDB2                       
069600     MOVE WS-IDKUNDNR            TO  W-IDKUNDNR-WDB2                      
069700     PERFORM IMS-GU-WDB201                                                
069800                                                                          
069900     MOVE GMT-IDDC-BULK(1)       TO WS-HOME-IDDC                          
070000                                    WS-FACING-IDDC                        
070100                                                                          
070200*TMS-SEND-IDDC from KORD- (WDE401)                                        
070300*this test for referal DC (can be different DC)                           
070400     IF TMS-IDDC = GMT-IDDC-BULK(1)                                       
070500       MOVE GMT-IDDC-BULK(1)     TO WS-REFERAL-IDDC                       
070600       MOVE GMT-IDDC-BULK(1)     TO W-IDDC-B6                             
070700     ELSE                                                                 
070800       MOVE TMS-IDDC             TO WS-REFERAL-IDDC                       
070900       MOVE GMT-IDDC-BULK(1)     TO W-IDDC-B6                             
071000     END-IF                                                               
071100*    MOVE REFERAL DC             TO ISSUINGSITE2                          
071200                                                                          
071300     MOVE GMT-IDPARTNR           TO W-WDB1-IDPARTNR                       
071400     MOVE GMT-IDFTG              TO W-WDB1-IDFTG                          
071500                                                                          
071600     PERFORM IMS-GU-WDB601                                                
071700*                                                                         
071800     MOVE TMS-IDDC               TO WS-IDDC                               
071900     .                                                                    
072000                                                                          
072100 D-ADD-TO-TMS-INFO         SECTION.                                       
072200     MOVE 'D-ADD-TO-TMS-INFO '   TO CURRENT-SECTION                       
072300*                                                                         
072400*CUSTOMERIDENTIFIER                                                       
072500     MOVE WS-IDDISTR             TO WS-IDDISTR-CUSTOM                     
072600                                    WS-IDDISTR-Z                          
072700     MOVE WS-IDKUNDNR            TO WS-IDKUNDNR-CUSTOM                    
072800                                    WS-IDKUNDNR-Z                         
072900                                                                          
073000     MOVE SPACES                 TO customerIdentifier                    
073100     STRING FUNCTION TRIM (WS-IDDISTR-Z) DELIMITED BY SIZE                
073200            '-'        DELIMITED BY SIZE                                  
073300            FUNCTION TRIM (WS-IDKUNDNR-Z) DELIMITED BY SIZE               
073400     INTO customerIdentifier                                              
073500     COMPUTE CUSTOMERIDENTIFIER-LENGTH                                    
073600                                  = FUNCTION BYTE-LENGTH (                
073700                            FUNCTION TRIM (customerIdentifier))           
073800                                                                          
073900     MOVE WS-REFERAL-IDDC        TO ISSUINGSITE2                          
074000     MOVE 2                      TO ISSUINGSITE2-LENGTH                   
074100     MOVE 1                      TO ISSUINGSITE-NUM                       
074200                                                                          
074300     MOVE WS-FACING-IDDC         TO FACINGDC2                             
074400     MOVE 2                      TO FACINGDC2-LENGTH                      
074500     MOVE 1                      TO FACINGDC-NUM                          
074600                                                                          
074700     MOVE KORD-IDORDER           TO W-Q201-IDORDER                        
074800     PERFORM IMS-GU-WDQ201                                                
074900*LK  MOVE OHUV-IDSKYLT           TO WS-LANGUAGE                           
075000*                                                                         
075100     IF OHUV-IDSKYLT = 'S '                                               
075200       MOVE 'sv'                 TO WS-LANGUAGE                           
075300       MOVE 'SE'                 TO WS-COUNTRY                            
075400     ELSE                                                                 
075500       MOVE 'en'                 TO WS-LANGUAGE                           
075600       MOVE 'GB'                 TO WS-COUNTRY                            
075700     END-IF                                                               
075800                                                                          
075900     MOVE WS-LANG-COUNTRY-GRP    TO LANGUAGE                              
076000     COMPUTE LANGUAGE-LENGTH = FUNCTION BYTE-LENGTH (                     
076100               FUNCTION TRIM (LANGUAGE))                                  
076200                                                                          
076300     MOVE "VOLVOCARS_PULS"       TO ISSUINGAPPLICATION2                   
076400     MOVE 14                     TO ISSUINGAPPLICATION2-LENGTH            
076500     MOVE 1                      TO ISSUINGAPPLICATION-NUM                
076600*                                                                         
076700*SHIPTO RECEIVER                                                          
076800     MOVE +1                     TO WS-PROC-PARTY-CNT                     
076900     PERFORM DA-RECEIVER-PROC-PART1                                       
077000                                                                          
077100     MOVE KORD-KDORDKL           TO WS-KDORDKL-TYPE                       
077200     MOVE KORD-KDFRAKT           TO WS-KDFRAKT-TYPE                       
077300     MOVE WS-ORDERTYPE           TO ORDERTYPE2                            
077400     MOVE 3                      TO ORDERTYPE2-LENGTH                     
077500     MOVE 1                      TO ORDERTYPE-NUM                         
077600*BUYER DETAILS                                                            
077700*                                                                         
077800     ADD +1                      TO WS-PROC-PARTY-CNT                     
077900     PERFORM DB-BUYER-PROC-PART2                                          
078000                                                                          
078100*SHIPFROM  SENDER                                                         
078200      MOVE KORD-IDORDER          TO W-Q301-IDORDER                        
078300      MOVE KORD-IDDC             TO W-Q301-IDDC                           
078400      MOVE WS-IDPRODNR           TO W-Q301-IDPRODNR                       
078500      MOVE KORD-IDPLKLST         TO W-Q301-IDPLKLST                       
078600      PERFORM IMS-GU-WDQ301                                               
078700     IF GOOD-DDC                                                          
078800        MOVE ODEL-IDLEVNR           TO WS-IDLEVNR                         
078900        ADD +1                      TO WS-PROC-PARTY-CNT                  
079000*ISSUING SITE IS BLANK IF ORDER IS A DDGS ORDER                           
079100        MOVE SPACES                 TO ISSUINGSITE2                       
079200        MOVE 0                      TO ISSUINGSITE2-LENGTH                
079300                                       ISSUINGSITE-NUM                    
079400        PERFORM DC-SHIPFROM-PROC-PART3                                    
079500     END-IF                                                               
079600                                                                          
079700*MOVE THE TOTAL COUNT OF PROCESSING PARTIES                               
079800     MOVE WS-PROC-PARTY-CNT      TO PROCESSINGPARTIES2-NUM                
079900                                                                          
080000*ETA = ESTIMATED TIME OF ARRIVAL NOT IN PULS                              
080100     MOVE SPACE                  TO REQUESTEDARRIVALTIME2                 
080200     COMPUTE REQUESTEDARRIVALTIME-LENGTH = FUNCTION BYTE-LENGTH (         
080300             FUNCTION TRIM (REQUESTEDARRIVALTIME2))                       
080400*                                                                         
080500     MOVE ZERO                  TO  WS-DATRP-HOUR                         
080600     MOVE ZERO                  TO  WS-DATRP-MIN                          
080700     MOVE ZERO                  TO  WS-DATRP-SEC                          
080800     MOVE ZERO                  TO  WS-DATRP-HUN                          
080900*                                                                         
081000*DATRPAVT   TRANSPORT DEPARTURE TIME                                      
081100     MOVE ODEL-DATRPAVD         TO WS-TRP-DATE                            
081200     MOVE ODEL-TIHHMM           TO WS-TIHHMM                              
081300     MOVE WS-TIHHMM             TO WS-TRP-TIME                            
081400     MOVE WS-TRP-SEKEL           TO WS-DATRP-SEKEL                        
081500     MOVE WS-TRP-YEAR            TO WS-DATRP-YEAR                         
081600     MOVE WS-TRP-MONTH           TO WS-DATRP-MONTH                        
081700     MOVE WS-TRP-DAY             TO WS-DATRP-DAY                          
081800     MOVE WS-TRP-HOUR            TO WS-DATRP-HOUR                         
081900     MOVE WS-TRP-MIN             TO WS-DATRP-MIN                          
082000     MOVE WS-TIDPUNKT (5:2)      TO WS-DATRP-SEC                          
082100     MOVE WS-TIDPUNKT (7:2)      TO WS-DATRP-HUN                          
082200     MOVE WS-DATRP-GRP           TO PLANNEDDISPATCHTIME2                  
082300     MOVE 1                      TO PLANNEDDISPATCHTIME-NUM               
082400                                                                          
082500     COMPUTE PLANNEDDISPATCHTIME2-LENGTH = FUNCTION BYTE-LENGTH (         
082600             FUNCTION TRIM (PLANNEDDISPATCHTIME2))                        
082700     .                                                                    
082800                                                                          
082900 DA-RECEIVER-PROC-PART1    SECTION.                                       
083000     MOVE 'DA-RECEIVER-PROC-P'    TO CURRENT-SECTION                      
083100                                                                          
083200     MOVE 1                      TO ADDRESS2-NUM(1)                       
083300     MOVE 'Receiver'             TO ADDRESSTYPE(1)                        
083400     COMPUTE ADDRESSTYPE-LENGTH(1) = FUNCTION BYTE-LENGTH (               
083500               FUNCTION TRIM (ADDRESSTYPE(1)))                            
083600                                                                          
083700     IF OHUV-ADGMT = GMT-ADGMT AND                                        
083800        OHUV-BEGMT = GMT-BEGMT                                            
083900       MOVE SPACES               TO locationAlias2    (1)                 
084000       MOVE 1                    TO locationAlias-num (1)                 
084100                                                                          
084200       STRING FUNCTION TRIM (WS-IDDISTR-Z)                                
084300                       DELIMITED BY SIZE                                  
084400              '-'      DELIMITED BY SIZE                                  
084500              FUNCTION TRIM (WS-IDKUNDNR-Z)                               
084600                       DELIMITED BY SIZE                                  
084700                               INTO locationAlias2 (1)                    
084800       COMPUTE locationAlias2-length (1)                                  
084900                                  = FUNCTION BYTE-LENGTH (                
085000                                    FUNCTION TRIM (                       
085100                                    locationAlias2 (1)))                  
085200                                                                          
085300       MOVE 0                      TO address2-num (1)                    
085400                                      contact2-num (1)                    
085500                                      timeZone-num (1)                    
085600                                                                          
085700     ELSE                                                                 
085800                                                                          
085900       MOVE 1                    TO CONTACT2-NUM (1)                      
086000       MOVE OHUV-IDMAIL          TO EMAIL2(1)                             
086100       MOVE 1                    TO EMAIL-NUM(1)                          
086200       COMPUTE EMAIL2-LENGTH(1) = FUNCTION BYTE-LENGTH (                  
086300               FUNCTION TRIM (EMAIL2(1)))                                 
086400                                                                          
086500       MOVE 1                    TO PHONE-NUM(1)                          
086600       MOVE OHUV-BETELNR         TO PHONE2(1)                             
086700                                                                          
086800       COMPUTE PHONE2-LENGTH(1) = FUNCTION BYTE-LENGTH (                  
086900               FUNCTION TRIM (PHONE2(1)))                                 
087000                                                                          
087100                                                                          
087200       MOVE OHUV-ADGMT-GATA      TO STREETADDRESS(1)                      
087300       COMPUTE STREETADDRESS-LENGTH(1) = FUNCTION BYTE-LENGTH (           
087400               FUNCTION TRIM (STREETADDRESS(1)))                          
087500                                                                          
087600       IF GMT-KDPOSTNR = 'L'                                              
087700         MOVE OHUV-ADPOSTNR IN OHUV-ADPOST-PNRORT                         
087800                                 TO POSTALCODE(1)                         
087900         MOVE OHUV-ADCITY IN OHUV-ADPOST-PNRORT                           
088000                                 TO CITY(1)                               
088100       ELSE                                                               
088200         MOVE OHUV-ADPOSTNR IN OHUV-ADPOST-ORTPNR                         
088300                                 TO POSTALCODE(1)                         
088400         MOVE OHUV-ADCITY IN OHUV-ADPOST-ORTPNR                           
088500                                 TO CITY(1)                               
088600       END-IF                                                             
088700       COMPUTE POSTALCODE-LENGTH(1) = FUNCTION BYTE-LENGTH (              
088800               FUNCTION TRIM (POSTALCODE(1)))                             
088900                                                                          
089000       COMPUTE CITY-LENGTH(1) = FUNCTION BYTE-LENGTH (                    
089100               FUNCTION TRIM (CITY(1)))                                   
089200                                                                          
089300       MOVE 1                    TO rawCity-num (1)                       
089400       MOVE FUNCTION TRIM (OHUV-ADGMT-PADR)                               
089500                                 TO rawCity2 (1)                          
089600       COMPUTE rawCity2-length (1) = FUNCTION BYTE-LENGTH (               
089700               FUNCTION TRIM (rawCity2(1)))                               
089800                                                                          
089900       MOVE 1                    TO TIMEZONE-NUM (1)                      
090000       MOVE DCS-IDTIDZON         TO TIMEZONE2(1)                          
090100       COMPUTE TIMEZONE2-LENGTH(1) = FUNCTION BYTE-LENGTH (               
090200               FUNCTION TRIM (TIMEZONE2(1)))                              
090300                                                                          
090400       MOVE DCS-IDVAT            TO VATCODE(1)                            
090500       MOVE 0                    TO VATCODE-NUM(1)                        
090600       COMPUTE VATCODE2-LENGTH(1) = FUNCTION BYTE-LENGTH (                
090700               FUNCTION TRIM (VATCODE(1)))                                
090800                                                                          
090900       MOVE GMT-IDLANDX2         TO COUNTRY(1)                            
091000       COMPUTE COUNTRY-LENGTH(1) = FUNCTION BYTE-LENGTH (                 
091100               FUNCTION TRIM (COUNTRY(1)))                                
091200                                                                          
091300       MOVE OHUV-BEGMT-RAD1      TO NAME(1)                               
091400       COMPUTE NAME-LENGTH(1) = FUNCTION BYTE-LENGTH (                    
091500               FUNCTION TRIM (NAME(1)))                                   
091600                                                                          
091700       MOVE 1                    TO name2-num (1)                         
091800       MOVE OHUV-BEGMT-RAD2      TO name22 (1)                            
091900       COMPUTE name22-LENGTH (1) = FUNCTION BYTE-LENGTH (                 
092000               FUNCTION TRIM (name22 (1)))                                
092100                                                                          
092200       MOVE OHUV-BEBETRAD-1      TO NAME3(1)                              
092300       COMPUTE NAME3-LENGTH(1) = FUNCTION BYTE-LENGTH (                   
092400               FUNCTION TRIM (NAME3(1)))                                  
092500                                                                          
092600       MOVE SPACE                TO STATE(1)                              
092700       COMPUTE STATE-LENGTH(1) = FUNCTION BYTE-LENGTH (                   
092800             FUNCTION TRIM (STATE(1)))                                    
092900                                                                          
093000     END-IF                                                               
093100     .                                                                    
093200                                                                          
093300 DB-BUYER-PROC-PART2    SECTION.                                          
093400     MOVE 'DB-BUYER-PROC-PART'    TO CURRENT-SECTION                      
093500                                                                          
093600     PERFORM IMS-GU-WDB101                                                
093700                                                                          
093800     MOVE 'Buyer'                TO ADDRESSTYPE(2)                        
093900     COMPUTE ADDRESSTYPE-LENGTH(2) = FUNCTION BYTE-LENGTH (               
094000               FUNCTION TRIM (ADDRESSTYPE(2)))                            
094100                                                                          
094200     MOVE BET-IDPARTNR           TO LOCATIONALIAS2(2)                     
094300     COMPUTE LOCATIONALIAS2-LENGTH(2) = FUNCTION BYTE-LENGTH (            
094400             FUNCTION TRIM (LOCATIONALIAS2(2)))                           
094500     MOVE 1                      TO LOCATIONALIAS-NUM(2)                  
094600                                                                          
094700                                                                          
094800     MOVE BET-IDVAT              TO VATCODE2(2)                           
094900     MOVE 1                      TO VATCODE-NUM(2)                        
095000       COMPUTE VATCODE2-LENGTH(2) = FUNCTION BYTE-LENGTH (                
095100               FUNCTION TRIM (VATCODE2(2)))                               
095200                                                                          
095300     MOVE 0                      TO ADDRESS2-NUM(2)                       
095400     .                                                                    
095500                                                                          
095600 DC-SHIPFROM-PROC-PART3    SECTION.                                       
095700     MOVE 'DC-SHIPFROM-PROC-P'    TO CURRENT-SECTION                      
095800                                                                          
095900     MOVE WS-IDLEVNR             TO LOCATIONALIAS2(3)                     
096000     MOVE 5                      TO LOCATIONALIAS2-LENGTH(3)              
096100     MOVE 1                      TO LOCATIONALIAS-NUM(3)                  
096200                                                                          
096300     MOVE 0                      TO ADDRESS2-NUM(3)                       
096400                                                                          
096500     MOVE 'ShipFrom'             TO ADDRESSTYPE(3)                        
096600     COMPUTE ADDRESSTYPE-LENGTH(3) = FUNCTION BYTE-LENGTH (               
096700               FUNCTION TRIM (ADDRESSTYPE(3)))                            
096800                                                                          
096900     MOVE 0                      TO CONTACT2-NUM (3)                      
097000     MOVE 0                      TO TIMEZONE-NUM (3)                      
097100     .                                                                    
097200                                                                          
097300 E-CASE-LINES-INFO SECTION.                                               
097400     MOVE 'E-CASE-LINES-INF'     TO CURRENT-SECTION                       
097500     SET SW-DG-NOT-EXIST         TO TRUE                                  
097600*PARCELIDENTIFIER                                                         
097700     MOVE WS-IDDISTR             TO WS-IDDISTR-PARCEL                     
097800     MOVE WS-IDKUNDNR            TO WS-IDKUNDNR-PARCEL                    
097900     MOVE WS-IDORDNR             TO WS-IDORDNR7-PARCEL                    
098000     MOVE TMS-IDKOLLI(CASE-IX)   TO WS-IDKOLLI-PARCEL                     
098100     MOVE WS-PARCELIDENTIFIER    TO PARCELIDENTIFIER                      
098200     COMPUTE PARCELIDENTIFIER-LENGTH = FUNCTION BYTE-LENGTH (             
098300             FUNCTION TRIM (PARCELIDENTIFIER))                            
098400*                                                                         
098500     MOVE WS-IDPRODNR            TO W-IDPRODNR-F                          
098600                                    W-IDPRODNR-421                        
098700                                    W-E6-IDPRODNR                         
098800     MOVE TMS-IDKOLLI(CASE-IX)   TO W-IDKOLLI-F                           
098900                                    W-IDKOLLI-421                         
099000                                    W-E6-IDKOLLI                          
099100     PERFORM IMS-GU-WDE411-21-FSEQ                                        
099200*PARCELCONTENTS = ORDER LINES IN CASE                                     
099300     MOVE +1                        TO TMS-IX                             
099400     PERFORM UNTIL SEGMENT-MISSING                                        
099500                OR SEGMENT-END                                            
099600*                                                                         
099700*WS-ORDERIDENTIFIER START                                                 
099800       MOVE WS-IDDISTR           TO WS-IDDISTR-ORDER                      
099900       MOVE WS-IDKUNDNR          TO WS-IDKUNDNR-ORDER                     
100000*                                                                         
100100*TMS-IDKUNDRF-RO(TMS-IX) FROM ORAD-IDKUNDRF-RO                            
100200       IF ORAD-IDKUNDRF-RO = '00000     '                                 
100300         MOVE WS-IDORDNR         TO WS-IDORDNR7-ORDER                     
100400         MOVE OHUV-TIREGDAT      TO WS-TIORDREG-ORDER                     
100500         MOVE OHUV-IDDC-PRIM     TO W-IDDC-PRIM                           
100600         MOVE OHUV-TIREGDAT      TO WS-DATE-YYMMDD                        
100700         MOVE OHUV-TIREGTID      TO WS-TIME                               
100800         PERFORM S10-GET-UTC-TIMESTAMP                                    
100900       ELSE                                                               
101000         MOVE ORAD-IDKUNDRF-RO(1:5) TO WS-IDORDNR7-ORDER(3:5)             
101100                                       W-WDQ2C-IDORDNR5                   
101200         MOVE WS-IDDISTR            TO W-WDQ2C-IDDISTR                    
101300         MOVE WS-IDKUNDNR           TO W-WDQ2C-IDKUNDNR                   
101400         PERFORM IMS-GET-WDQ201-CSEQ                                      
101500         IF SEGMENT-FOUND                                                 
101600           MOVE CSEQ-OHUV-TIREGDAT TO WS-TIORDREG-ORDER                   
101700           MOVE CSEQ-OHUV-IDDC-PRIM                                       
101800                                 TO W-IDDC-PRIM                           
101900           MOVE CSEQ-OHUV-TIREGDAT                                        
102000                                 TO WS-DATE-YYMMDD                        
102100           MOVE CSEQ-OHUV-TIREGTID                                        
102200                                 TO WS-TIME                               
102300           PERFORM S10-GET-UTC-TIMESTAMP                                  
102400         END-IF                                                           
102500       END-IF                                                             
102600                                                                          
102700       MOVE WS-ORDERIDENTIFIER   TO ORDERIDENTIFIER (TMS-IX)              
102800                                                                          
102900       COMPUTE ORDERIDENTIFIER-LENGTH(TMS-IX)                             
103000             = FUNCTION BYTE-LENGTH (                                     
103100               FUNCTION TRIM (ORDERIDENTIFIER(TMS-IX)))                   
103200*WS-ORDERIDENTIFIER END                                                   
103300                                                                          
103400       MOVE 1                    TO orderRegistrationTimeX-num            
103500                                                     (TMS-IX)             
103600       MOVE 20                   TO orderRegistrationTim-length           
103700                                                     (TMS-IX)             
103800       MOVE WS-TIMESTAMP-UTC     TO orderRegistrationTimeX2               
103900                                                     (TMS-IX)             
104000                                                                          
104100       MOVE ORAD-IDARTNR            TO WS-IDARTNR                         
104200       MOVE FUNCTION TRIM (WS-IDARTNR)                                    
104300                                    TO PARTNUMBER (TMS-IX)                
104400       COMPUTE PARTNUMBER-LENGTH(TMS-IX) = FUNCTION BYTE-LENGTH (         
104500             FUNCTION TRIM (PARTNUMBER(TMS-IX)))                          
104600                                                                          
104700*FOR TEST PURPOSE                                                         
104800*      IF TMS-BERADREF  (TMS-IX) = SPACE                                  
104900*        MOVE 'LINE REFERENCE'      TO TMS-BERADREF  (TMS-IX)             
105000*      END-IF                                                             
105100       MOVE ORAD-BERADREF           TO ORDERLINEREFERENCE(TMS-IX)         
105200       COMPUTE ORDERLINEREFERENCE-LENGTH(TMS-IX)                          
105300             = FUNCTION BYTE-LENGTH (                                     
105400             FUNCTION TRIM (ORDERLINEREFERENCE(TMS-IX)))                  
105500                                                                          
105600       MOVE ORAD-BEART              TO PARTDESCRIPTION(TMS-IX)            
105700       IF OHUV-IDSKYLT NOT = 'EN' OR 'GB'                                 
105800         PERFORM EA-CHANGE-LANGUAGE                                       
105900         MOVE WS-BEART              TO PARTDESCRIPTION(TMS-IX)            
106000       END-IF                                                             
106100       COMPUTE PARTDESCRIPTION-LENGTH(TMS-IX) =                           
106200               FUNCTION BYTE-LENGTH (                                     
106300               FUNCTION TRIM (PARTDESCRIPTION(TMS-IX)))                   
106400                                                                          
106500       PERFORM EB-GET-PRICE                                               
106600                                                                          
106700       MOVE WS-KDVALISO             TO XCURRENCY (TMS-IX)                 
106800       MOVE 3                       TO XCURRENCY-LENGTH(TMS-IX)           
106900       MOVE WS-PRICE                 TO AMOUNT (TMS-IX)                   
107000                                                                          
107100       MOVE ORAD-VKARTNTO           TO PARTGROSSWEIGHTKG(TMS-IX)          
107200       MOVE 1                    TO PARTGROSSWEIGHTKG-NUM(TMS-IX)         
107300                                                                          
107400       MOVE ORAD-VLARTNTO           TO PARTGROSSVOLUMEDM3(TMS-IX)         
107500       MOVE 1                    TO PARTGROSSVOLUMEDM3-NUM(TMS-IX)        
107600                                                                          
107700       COMPUTE PARTGROSSVOLUMEDM3(TMS-IX) =                               
107800               ORAD-VLARTNTO  / 1000                                      
107900       END-COMPUTE                                                        
108000                                                                          
108100       MOVE ORAD-IDARTNR        TO W-IDARTNR                              
108200       PERFORM IMS-GU-WDK601                                              
108300       PERFORM IMS-GNP-WDK611                                             
108400       IF CLAG-IDSTATNR(3) > ZERO                                         
108500         MOVE CLAG-IDSTATNR(3)  TO WS-IDSTATNR                            
108600       ELSE                                                               
108700         MOVE '087089997'       TO WS-IDSTATNR                            
108800       END-IF                                                             
108900                                                                          
109000       IF ORAD-KDARTURS   = SPACE                                         
109100          MOVE CLAG-KDARTURS    TO COUNTRYOFORIGIN (TMS-IX)               
109200       ELSE                                                               
109300          MOVE ORAD-KDARTURS    TO COUNTRYOFORIGIN (TMS-IX)               
109400       END-IF                                                             
109500       MOVE 2                   TO COUNTRYOFORIGIN-LENGTH (TMS-IX)        
109600                                                                          
109700       MOVE FUNCTION TRIM(WS-IDSTATNR) TO HSCODE(TMS-IX)                  
109800*THIS IS THE GENERAL STATISICAL CODE FOR PARTS FOR ENGINE VEHICLES        
109900                                                                          
110000       COMPUTE HSCODE-LENGTH(TMS-IX) = FUNCTION BYTE-LENGTH (             
110100               FUNCTION TRIM (HSCODE(TMS-IX)))                            
110200*      MOVE SPACE             TO PARTADDITIONALDESCRIPTION(TMS-IX)        
110300                                                                          
110400       MOVE KKOLLI-KVLEVART         TO QUANTITY (TMS-IX)                  
110500       MOVE ART-KDSORT         TO QUANTITYUNITOFMEASURE2(TMS-IX)          
110600       MOVE 1         TO QUANTITYUNITOFMEASURE-NUM     (TMS-IX)           
110700                                                                          
110800       COMPUTE QUANTITYUNITOFMEASUR-LENGTH(TMS-IX)                        
110900             = FUNCTION BYTE-LENGTH (                                     
111000           FUNCTION TRIM (QUANTITYUNITOFMEASURE2(TMS-IX)))                
111100*                                                                         
111200*DANGEROUS GOODS INFO                                                     
111300       PERFORM EC-READ-WDR2-DANGEROUS-GOODS                               
111400       ADD  +1                        TO TMS-IX                           
111500       PERFORM IMS-GN-WDE411-21-FSEQ                                      
111600     END-PERFORM                                                          
111700                                                                          
111800     COMPUTE TMS-IX = TMS-IX - 1                                          
111900     END-COMPUTE                                                          
112000     MOVE TMS-IX                 TO PARCELCONTENTS2-NUM                   
112100                                                                          
112200     IF SW-DG-EXISTS                                                      
112300       MOVE WS-TRUE              TO DANGEROUSGOODS                        
112400     ELSE                                                                 
112500       MOVE WS-FALSE             TO DANGEROUSGOODS                        
112600     END-IF                                                               
112700                                                                          
112800     PERFORM IMS-GU-WDE611                                                
112900     MOVE 0                      TO PACKAGINGTAREWEIGHTKG                 
113000     IF KOLLI-KDKOLLI > SPACE                                             
113100       MOVE KOLLI-KDKOLLI TO W-K5-KDKOLLI                                 
113200       PERFORM IMS-GU-WDK501                                              
113300       MOVE EMB-VKTARA           TO PACKAGINGTAREWEIGHTKG                 
113400     END-IF                                                               
113500                                                                          
113600     MOVE KOLLI-VKORDBTO-KOLLI   TO GROSSWEIGHTKG                         
113700     MOVE KOLLI-VLORDBTO-KOLLI   TO WS-VLORDBTO                           
113800     MOVE 1                      TO GROSSWEIGHTKG-NUM                     
113900     MOVE 1                      TO PACKAGINGTAREWEIGHTKG-NUM             
114000                                                                          
114100     COMPUTE GROSSVOLUMEDM3 = WS-VLORDBTO * 1000                          
114200     END-COMPUTE                                                          
114300     MOVE 1                      TO GROSSVOLUMEDM3-NUM                    
114400                                                                          
114500     MOVE KOLLI-KDKOLLI          TO PACKAGINGCODE                         
114600     MOVE KOLLI-KDEMBTYP         TO WS-KDEMBTYP                           
114700     MOVE KOLLI-DIKOLLIL         TO WS-DIKOLLIL                           
114800     MOVE KOLLI-DIKOLLIB         TO WS-DIKOLLIB                           
114900     MOVE KOLLI-DIKOLLIH         TO WS-DIKOLLIH                           
115000                                                                          
115100     COMPUTE PACKAGINGCODE-LENGTH = FUNCTION BYTE-LENGTH (                
115200             FUNCTION TRIM (PACKAGINGCODE))                               
115300                                                                          
115400*    MOVE 1                      TO PACKAGETYPE-NUM                       
115500                                                                          
115600     EVALUATE TRUE                                                        
115700       WHEN WS-KDEMBTYP = 1                                               
115800         MOVE 1                  TO PACKAGETYPE2                          
115900         MOVE 'CASE'             TO PACKAGINGTYPE                         
116000         MOVE 4                  TO PACKAGINGTYPE-LENGTH                  
116100       WHEN WS-KDEMBTYP = 2                                               
116200         MOVE 2                  TO PACKAGETYPE2                          
116300         MOVE 'PACKAGE'          TO PACKAGINGTYPE                         
116400         MOVE 7                  TO PACKAGINGTYPE-LENGTH                  
116500       WHEN WS-KDEMBTYP = 3                                               
116600         MOVE 3                  TO PACKAGETYPE2                          
116700         MOVE 'BUNDLE'           TO PACKAGINGTYPE                         
116800         MOVE 6                  TO PACKAGINGTYPE-LENGTH                  
116900       WHEN WS-KDEMBTYP = 4                                               
117000         MOVE 4                  TO PACKAGETYPE2                          
117100         MOVE 'CRATE'            TO PACKAGINGTYPE                         
117200         MOVE 5                  TO PACKAGINGTYPE-LENGTH                  
117300       WHEN WS-KDEMBTYP = 5                                               
117400         MOVE 5                  TO PACKAGETYPE2                          
117500         MOVE 'PIECE'            TO PACKAGINGTYPE                         
117600         MOVE 5                  TO PACKAGINGTYPE-LENGTH                  
117700       WHEN WS-KDEMBTYP = 6                                               
117800         MOVE 6                  TO PACKAGETYPE2                          
117900         MOVE 'CONTAINER'        TO PACKAGINGTYPE                         
118000         MOVE 9                  TO PACKAGINGTYPE-LENGTH                  
118100       WHEN WS-KDEMBTYP = 7                                               
118200         MOVE 7                  TO PACKAGETYPE2                          
118300         MOVE 'PALLET'           TO PACKAGINGTYPE                         
118400         MOVE 6                  TO PACKAGINGTYPE-LENGTH                  
118500       WHEN WS-KDEMBTYP = 8                                               
118600         MOVE 8                  TO PACKAGETYPE2                          
118700         MOVE 'RETURN BLUE CARTON' TO PACKAGINGTYPE                       
118800         MOVE 18                 TO PACKAGINGTYPE-LENGTH                  
118900       WHEN OTHER                                                         
119000         MOVE 0                  TO PACKAGETYPE2                          
119100         MOVE 'UNKNOWN'            TO PACKAGINGTYPE                       
119200         MOVE 7                  TO PACKAGINGTYPE-LENGTH                  
119300     END-EVALUATE                                                         
119400                                                                          
119500*SEND DIMENSIONS IN MM                                                    
119600     COMPUTE LENGTHMM = WS-DIKOLLIL * 10                                  
119700     END-COMPUTE                                                          
119800     MOVE 1                      TO LENGTHMM-NUM                          
119900                                                                          
120000     COMPUTE WIDTHMM =  WS-DIKOLLIB * 10                                  
120100     END-COMPUTE                                                          
120200     MOVE 1                      TO WIDTHMM-NUM                           
120300                                                                          
120400     COMPUTE HEIGHTMM = WS-DIKOLLIH * 10                                  
120500     END-COMPUTE                                                          
120600     MOVE 1                      TO HEIGHTMM-NUM                          
120700     .                                                                    
120800                                                                          
120900 EA-CHANGE-LANGUAGE      SECTION.                                         
121000     MOVE 'EA-CHANGE-LANGUAGE'    TO CURRENT-SECTION                      
121100                                                                          
121200     MOVE 'GB'         TO W-IDSKYLT-X                                     
121300                                                                          
121400     MOVE ORAD-IDARTNR        TO W-D3BSEQ-IDARTNR                         
121500                                                                          
121600     PERFORM IMS-GU-WDD311                                                
121700     IF SEGMENT-FOUND                                                     
121800        MOVE TEXT-BEART              TO WS-BEART                          
121900     ELSE                                                                 
122000        MOVE 'DESCRIPTION MISSING'   TO WS-BEART                          
122100     END-IF                                                               
122200     .                                                                    
122300                                                                          
122400 EB-GET-PRICE        SECTION.                                             
122500     MOVE 'EB-GET-PRICE      '    TO CURRENT-SECTION                      
122600                                                                          
122700     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
122800*                                                                         
122900     IF DIST79-DEALER-PRICE OR                                            
123000        DIST79-ECOM-PRICE                                                 
123100        IF ORAD-PRARTNTO-LOC  > 0                                         
123200           MOVE ORAD-PRARTNTO-LOC   TO WS-PRICE                           
123300        ELSE                                                              
123400           IF ORAD-PRARTNTO-LOCPREL > 0                                   
123500              MOVE ORAD-PRARTNTO-LOCPREL    TO HELP-PRARTNTO              
123600              IF HELP-PRARTNTO    < 1                                     
123700                 MOVE 1    TO HELP-PRARTNTO                               
123800              END-IF                                                      
123900              MOVE HELP-PRARTNTO    TO  WS-PRICE                          
124000           END-IF                                                         
124100        END-IF                                                            
124200        MOVE ORAD-KDVALISO          TO WS-KDVALISO                        
124300     ELSE                                                                 
124400        IF ODEL-IDDC-EXP = WC-CDC-SE OR                                   
124500           ODEL-IDDC-EXP = SPACE                                          
124600           IF ODEL-IDDC-EXP = KORD-IDDC                                   
124700              MOVE ORAD-PRARTNTO            TO WS-PRICE                   
124800              MOVE ORAD-KDVALISO            TO WS-KDVALISO                
124900           ELSE                                                           
125000              IF ORAD-PRAVCOST   > 0                                      
125100                 MOVE ORAD-PRAVCOST         TO WS-PRICE                   
125200                 MOVE ORAD-KDVALISO-EXP     TO WS-KDVALISO                
125300              ELSE                                                        
125400                 MOVE ORAD-PRARTNTO         TO WS-PRICE                   
125500                 MOVE ORAD-KDVALISO         TO WS-KDVALISO                
125600              END-IF                                                      
125700           END-IF                                                         
125800        END-IF                                                            
125900        IF ODEL-IDDC-EXP NOT = WC-CDC-SE OR                               
126000           ODEL-IDDC-EXP = SPACE                                          
126100           IF ODEL-IDDC-EXP = KORD-IDDC                                   
126200              MOVE ORAD-PRAVCOST           TO WS-PRICE                    
126300              MOVE ORAD-KDVALISO-EXP       TO WS-KDVALISO                 
126400           ELSE                                                           
126500              MOVE ORAD-PRARTNTO          TO WS-PRICE                     
126600              MOVE ORAD-KDVALISO          TO WS-KDVALISO                  
126700           END-IF                                                         
126800        END-IF                                                            
126900     END-IF                                                               
127000     .                                                                    
127100                                                                          
127200 EC-READ-WDR2-DANGEROUS-GOODS  SECTION.                                   
127300     MOVE 'EC-READ-WDR2      '   TO CURRENT-SECTION                       
127400                                                                          
127500     MOVE ZERO                   TO DANG-IX                               
127600     MOVE ZERO                   TO PSN-IX                                
127700     MOVE ORAD-IDPSN             TO W-IDPSN                               
127800                                                                          
127900     IF W-IDPSN > ZERO                                                    
128000       PERFORM ECA-SRCH-LANGUAGE                                          
128100       PERFORM ECB-GET-DG-INFO                                            
128200     END-IF                                                               
128300                                                                          
128400     MOVE PSN-IX                 TO DANGEROUSGOODSDETAILS2-NUM            
128500                                                    (TMS-IX)              
128600     IF PSN-IX > ZERO                                                     
128700       IF W-IDPSN = 900 OR 901 OR 902 OR 903 OR 905 OR 907 OR 910         
128800         MOVE WS-FALSE           TO DANGEROUSGOODS2 (TMS-IX)              
128900       ELSE                                                               
129000         MOVE WS-TRUE            TO DANGEROUSGOODS2 (TMS-IX)              
129100         SET SW-DG-EXISTS        TO TRUE                                  
129200       END-IF                                                             
129300     ELSE                                                                 
129400       MOVE WS-FALSE             TO DANGEROUSGOODS2 (TMS-IX)              
129500     END-IF                                                               
129600     .                                                                    
129700                                                                          
129800 ECA-SRCH-LANGUAGE  SECTION.                                              
129900     MOVE 'ECA-SRCH-LANGUAGE '    TO CURRENT-SECTION                      
130000*DCS-IDLANDX2 = LOCAL                                                     
130100*                                                                         
130200     SEARCH ALL WWLNDSPR-RAD                                              
130300       AT END                                                             
130400         MOVE 'GB'                TO WS-LOCAL-LANG                        
130500       WHEN WWLNDSPR-IDLANDX2(SPR-IX) = DCS-IDLANDX2                      
130600         MOVE WWLNDSPR-IDSPRAK(SPR-IX)                                    
130700                                  TO WS-LOCAL-LANG                        
130800         MOVE WWLNDSPR-IDLANDX2-IDSPRAK-GRP(SPR-IX)                       
130900                                  TO WS-LANG-COUNTRY-LOC                  
131000     END-SEARCH                                                           
131100     .                                                                    
131200                                                                          
131300 ECB-GET-DG-INFO SECTION.                                                 
131400     MOVE 'ECB-GET-DG-INFO '     TO CURRENT-SECTION                       
131500                                                                          
131600*    Start with English (GB)                                              
131700     MOVE 'GB'                   TO W-IDSPRAK                             
131800     PERFORM IMS-GU-1165                                                  
131900     IF SEGMENT-FOUND                                                     
132000*     See if there is any Notes and save it                               
132100       PERFORM ECBA-READ-1166-NOTES                                       
132200       SET SW-DG-DESC-NOT-FOUND  TO TRUE                                  
132300                                                                          
132400*     DGR = AIR; KDFGTRP = 01; MODE-OF-TRANSPORT = 40                     
132500       MOVE 01                   TO W-KDFGTRP                             
132600       PERFORM ECBB-READ-1168                                             
132700                                                                          
132800*     IMDG = BOAT; KDFGTRP = 02/03; MODE-OF-TRANSPORT = 10                
132900       IF W-IDPSN = 32 OR 33 OR 34                                        
133000         MOVE 03                 TO W-KDFGTRP                             
133100       ELSE                                                               
133200         MOVE 02                 TO W-KDFGTRP                             
133300       END-IF                                                             
133400       PERFORM ECBB-READ-1168                                             
133500                                                                          
133600*     ADR = ROAD; KDFGTRP = 04; MODE-OF-TRANSPORT = 30                    
133700       MOVE 04                   TO W-KDFGTRP                             
133800       PERFORM ECBB-READ-1168                                             
133900                                                                          
134000*     If description is not found, and if notes are, then we              
134100*     should send that.                                                   
134200       IF SW-DG-NOTE-FOUND AND                                            
134300          SW-DG-DESC-NOT-FOUND                                            
134400         PERFORM ECBC-SEND-NOTES                                          
134500       END-IF                                                             
134600     END-IF                                                               
134700*                                                                         
134800*    Now get details in local language. Skip if local lang is GB          
134900     MOVE WS-LOCAL-LANG          TO W-IDSPRAK                             
135000     IF WS-LOCAL-LANG = 'GB'                                              
135100       CONTINUE                                                           
135200     ELSE                                                                 
135300       PERFORM IMS-GU-1165                                                
135400       IF SEGMENT-FOUND                                                   
135500*     See if there is any Notes and save it                               
135600         PERFORM ECBA-READ-1166-NOTES                                     
135700         SET SW-DG-DESC-NOT-FOUND                                         
135800                                 TO TRUE                                  
135900                                                                          
136000*       DGR = AIR; KDFGTRP = 01; MODE-OF-TRANSPORT = 40                   
136100         MOVE 01                 TO W-KDFGTRP                             
136200         PERFORM ECBB-READ-1168                                           
136300                                                                          
136400*       IMDG = BOAT; KDFGTRP = 02/03; MODE-OF-TRANSPORT = 10              
136500         IF W-IDPSN = 32 OR 33 OR 34                                      
136600           MOVE 03               TO W-KDFGTRP                             
136700         ELSE                                                             
136800           MOVE 02               TO W-KDFGTRP                             
136900         END-IF                                                           
137000         PERFORM ECBB-READ-1168                                           
137100                                                                          
137200*       ADR = ROAD; KDFGTRP = 04; MODE-OF-TRANSPORT = 30                  
137300         MOVE 04                 TO W-KDFGTRP                             
137400         PERFORM ECBB-READ-1168                                           
137500                                                                          
137600*     If description is not found, and if notes are, then we              
137700*     should send that.                                                   
137800         IF SW-DG-NOTE-FOUND AND                                          
137900            SW-DG-DESC-NOT-FOUND                                          
138000           PERFORM ECBC-SEND-NOTES                                        
138100         END-IF                                                           
138200       END-IF                                                             
138300     END-IF                                                               
138400     .                                                                    
138500                                                                          
138600 ECBA-READ-1166-NOTES SECTION.                                            
138700     MOVE 'ECBA-READ-1166-NOTES' TO CURRENT-SECTION                       
138800                                                                          
138900     SET SW-DG-NOTE-NOT-FOUND    TO TRUE                                  
139000     PERFORM IMS-GNP-1166                                                 
139100     MOVE SPACES                 TO WS-TEPSNNOT                           
139200     IF SEGMENT-FOUND                                                     
139300       SET SW-DG-NOTE-FOUND      TO TRUE                                  
139400       STRING FUNCTION TRIM (1166-TEPSNNOT (1) )                          
139500                       DELIMITED BY SIZE                                  
139600              ' '      DELIMITED BY SIZE                                  
139700              FUNCTION TRIM (1166-TEPSNNOT (2) )                          
139800                       DELIMITED BY SIZE                                  
139900              ' '      DELIMITED BY SIZE                                  
140000              FUNCTION TRIM (1166-TEPSNNOT (3) )                          
140100                       DELIMITED BY SIZE                                  
140200                               INTO WS-TEPSNNOT                           
140300     END-IF                                                               
140400     .                                                                    
140500                                                                          
140600 ECBB-READ-1168 SECTION.                                                  
140700     MOVE 'ECBB-READ-1168'       TO CURRENT-SECTION                       
140800*                                                                         
140900                                                                          
141000     PERFORM IMS-GNP-1168                                                 
141100     IF SEGMENT-FOUND                                                     
141200       SET SW-DG-DESC-FOUND      TO TRUE                                  
141300       ADD +1                    TO PSN-IX                                
141400                                    DANG-IX                               
141500                                                                          
141600       MOVE 3                    TO dgPulsPSN-length                      
141700                                                 (TMS-IX, DANG-IX)        
141800       MOVE W-IDPSN              TO dgPulsPSN    (TMS-IX, DANG-IX)        
141900                                                                          
142000       MOVE ORAD-IDPSN           TO DGCLASS(TMS-IX, DANG-IX)              
142100       MOVE 3                    TO DGCLASS-LENGTH                        
142200                                      (TMS-IX, DANG-IX)                   
142300       EVALUATE W-KDFGTRP                                                 
142400         WHEN 1                                                           
142500           MOVE 40               TO modeOfTransport2                      
142600                                      (TMS-IX, DANG-IX)                   
142700         WHEN 2                                                           
142800         WHEN 3                                                           
142900           MOVE 10               TO modeOfTransport2                      
143000                                      (TMS-IX, DANG-IX)                   
143100         WHEN 4                                                           
143200           MOVE 30               TO modeOfTransport2                      
143300                                      (TMS-IX, DANG-IX)                   
143400       END-EVALUATE                                                       
143500       MOVE 1                    TO modeOfTransport-num                   
143600                                      (TMS-IX, DANG-IX)                   
143700       MOVE 2                    TO modeOfTransport2-LENGTH               
143800                                      (TMS-IX, DANG-IX)                   
143900                                                                          
144000       IF W-IDSPRAK = 'GB'                                                
144100         MOVE WS-LANG-COUNTRY-GB                                          
144200                                 TO LANGUAGE2(TMS-IX, DANG-IX)            
144300       ELSE                                                               
144400         MOVE WS-LANG-COUNTRY-LOC                                         
144500                                 TO LANGUAGE2(TMS-IX, DANG-IX)            
144600       END-IF                                                             
144700       MOVE 5                    TO LANGUAGE2-LENGTH                      
144800                                       (TMS-IX, DANG-IX)                  
144900                                                                          
145000       MOVE SPACE                TO WS-BEPSN                              
145100       MOVE 1168-BEPSN(1)        TO WS-BEPSN                              
145200                                                                          
145300       IF WS-BEPSN (1:2) = 'UN'                                           
145400*        If BEPSN starts with UN, then we consider that we have           
145500*        a valid UN code in first 6 chars.                                
145600         MOVE 1                  TO dgUNCode-num                          
145700                                       (TMS-IX, DANG-IX)                  
145800         MOVE WS-BEPSN(1:6)      TO dgUNCode2                             
145900                                       (TMS-IX, DANG-IX)                  
146000         MOVE 6                  TO dgUNCode2-LENGTH                      
146100                                       (TMS-IX, DANG-IX)                  
146200                                                                          
146300         IF WS-BEPSN (7:1) = ','                                          
146400           MOVE 8                TO WS-DGDESC-START                       
146500         ELSE                                                             
146600           MOVE 7                TO WS-DGDESC-START                       
146700         END-IF                                                           
146800                                                                          
146900       ELSE                                                               
147000*        If not, consider no valid UN code and send full BEPSN as         
147100*        description!                                                     
147200         MOVE 0                  TO dgUNCode-num                          
147300                                       (TMS-IX, DANG-IX)                  
147400         MOVE 1                  TO WS-DGDESC-START                       
147500       END-IF                                                             
147600                                                                          
147700       MOVE SPACES               TO dgDescription2                        
147800                                       (TMS-IX, DANG-IX)                  
147900       MOVE 1                    TO dgDescription-num                     
148000                                       (TMS-IX, DANG-IX)                  
148100       STRING FUNCTION TRIM (1168-BEPSN (1) (WS-DGDESC-START:) )          
148200                       DELIMITED BY SIZE                                  
148300              ' '      DELIMITED BY SIZE                                  
148400              FUNCTION TRIM (1168-BEPSN (2) )                             
148500                       DELIMITED BY SIZE                                  
148600              ' '      DELIMITED BY SIZE                                  
148700              FUNCTION TRIM (1168-BEPSN (3) )                             
148800                       DELIMITED BY SIZE                                  
148900                               INTO dgDescription2                        
149000                                       (TMS-IX, DANG-IX)                  
149100       COMPUTE dgDescription2-LENGTH(TMS-IX, DANG-IX) =                   
149200              FUNCTION BYTE-LENGTH (                                      
149300              FUNCTION TRIM (dgDescription2(TMS-IX, DANG-IX)))            
149400                                                                          
149500       IF SW-DG-NOTE-FOUND                                                
149600         MOVE 1                  TO dgNotes-num (TMS-IX, DANG-IX)         
149700         MOVE WS-TEPSNNOT        TO dgNotes2    (TMS-IX, DANG-IX)         
149800         COMPUTE dgNotes2-length (TMS-IX, DANG-IX) =                      
149900              FUNCTION BYTE-LENGTH (                                      
150000              FUNCTION TRIM (WS-TEPSNNOT))                                
150100       ELSE                                                               
150200         MOVE 0                  TO dgNotes-num (TMS-IX, DANG-IX)         
150300       END-IF                                                             
150400     END-IF                                                               
150500     .                                                                    
150600                                                                          
150700 ECBC-SEND-NOTES SECTION.                                                 
150800     MOVE 'ECBC-SEND-NOTES'      TO CURRENT-SECTION                       
150900*                                                                         
151000     ADD +1                      TO PSN-IX                                
151100                                    DANG-IX                               
151200                                                                          
151300     MOVE 3                      TO dgPulsPSN-length                      
151400                                       (TMS-IX, DANG-IX)                  
151500     MOVE W-IDPSN                TO dgPulsPSN                             
151600                                       (TMS-IX, DANG-IX)                  
151700                                                                          
151800     MOVE ORAD-IDPSN             TO dgClass                               
151900                                       (TMS-IX, DANG-IX)                  
152000     MOVE 3                      TO DGCLASS-LENGTH                        
152100                                       (TMS-IX, DANG-IX)                  
152200     MOVE 0                      TO modeOfTransport-num                   
152300                                       (TMS-IX, DANG-IX)                  
152400                                                                          
152500     IF W-IDSPRAK = 'GB'                                                  
152600       MOVE WS-LANG-COUNTRY-GB                                            
152700                                 TO language2                             
152800                                       (TMS-IX, DANG-IX)                  
152900     ELSE                                                                 
153000       MOVE WS-LANG-COUNTRY-LOC                                           
153100                                 TO language2                             
153200                                       (TMS-IX, DANG-IX)                  
153300     END-IF                                                               
153400     MOVE 5                      TO language2-length                      
153500                                       (TMS-IX, DANG-IX)                  
153600                                                                          
153700     MOVE 0                      TO dgDescription-num                     
153800                                       (TMS-IX, DANG-IX)                  
153900                                                                          
154000     MOVE 0                      TO dgUNCode-num                          
154100                                       (TMS-IX, DANG-IX)                  
154200                                                                          
154300     MOVE 1                      TO dgNotes-num (TMS-IX, DANG-IX)         
154400     MOVE WS-TEPSNNOT            TO dgNotes2    (TMS-IX, DANG-IX)         
154500     COMPUTE dgNotes2-length (TMS-IX, DANG-IX) =                          
154600              FUNCTION BYTE-LENGTH (                                      
154700              FUNCTION TRIM (WS-TEPSNNOT))                                
154800     .                                                                    
154900                                                                          
155000 F-SEND-TMS   SECTION.                                                    
155100     MOVE 'F-SEND-TMS   '        TO CURRENT-SECTION                       
155200                                                                          
155300     PERFORM F01-SEND-TMS-OPEN                                            
155400     PERFORM F02-SEND-TMS-HEADER                                          
155500     PERFORM F03-SEND-TMS-DATA                                            
155600     PERFORM F04-SEND-TMS-CLOSE                                           
155700                                                                          
155800     .                                                                    
155900                                                                          
156000 F01-SEND-TMS-OPEN SECTION.                                               
156100     MOVE 'F01-SEND-TMS-OPEN'     TO CURRENT-SECTION                      
156200                                                                          
156300                                                                          
156400     MOVE W-ADDISPABS             TO SEND-ADDISPABS                       
156500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
156600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
156700                                     SEND-OPEN-AREA                       
156800     IF SEND-KDRC > ZERO                                                  
156900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
157000       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
157100       DELIMITED BY SIZE INTO ERRORTEXT                                   
157200       CALL FELLOG                                                        
157300     ELSE                                                                 
157400       MOVE SEND-IDCOM            TO WS-SEND-IDCOM                        
157500     END-IF                                                               
157600                                                                          
157700     .                                                                    
157800                                                                          
157900 F02-SEND-TMS-HEADER SECTION.                                             
158000     MOVE 'F02-SEND-TMS-HEADER'  TO CURRENT-SECTION                       
158100                                                                          
158200     PERFORM IMS-GU-WDGX0104                                              
158300     IF SEGMENT-FOUND                                                     
158400       MOVE 0104-IDAPI             TO IDAPI                               
158500       COMPUTE IDAPI-LEN       = FUNCTION BYTE-LENGTH (                   
158600                                 FUNCTION TRIM (IDAPI))                   
158700       MOVE 0104-IDPATH-API        TO IDPATH-API                          
158800       COMPUTE IDPATH-API-LEN  = FUNCTION BYTE-LENGTH (                   
158900                                 FUNCTION TRIM (IDPATH-API))              
159000       MOVE 0104-IDPTYP-API        TO IDPTYP-API                          
159100       COMPUTE IDPTYP-API-LEN  = FUNCTION BYTE-LENGTH (                   
159200                                 FUNCTION TRIM (IDPTYP-API))              
159300       MOVE 0104-IDUSERKEY        TO USER-KEY                             
159400       COMPUTE USER-KEY-LENGTH = FUNCTION BYTE-LENGTH (                   
159500                                 FUNCTION TRIM (USER-KEY))                
159600     ELSE                                                                 
159700       CALL FELLOG                                                        
159800     END-IF                                                               
159900                                                                          
160000     MOVE 'PUT'                   TO SEND-KDFUNC                          
160100     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
160200     MOVE LENGTH OF WAPIINFO      TO SEND-KVDLEN                          
160300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
160400                                     SEND-KVDLEN                          
160500                                     WAPIINFO                             
160600     IF SEND-KDRC > ZERO                                                  
160700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
160800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
160900              DELIMITED BY SIZE INTO ERRORTEXT                            
161000       CALL FELLOG                                                        
161100     END-IF                                                               
161200     .                                                                    
161300                                                                          
161400 F03-SEND-TMS-DATA  SECTION.                                              
161500     MOVE 'F03-SEND-TMS-DATA '    TO CURRENT-SECTION                      
161600                                                                          
161700     MOVE 'PUT'                   TO SEND-KDFUNC                          
161800     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
161900     MOVE LENGTH OF WTM00Q01-AREA TO SEND-KVDLEN                          
162000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
162100                                     SEND-KVDLEN                          
162200                                     WTM00Q01-AREA                        
162300                                                                          
162400     IF SEND-KDRC > ZERO                                                  
162500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
162600       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
162700       DELIMITED BY SIZE       INTO ERRORTEXT                             
162800       CALL FELLOG                                                        
162900     END-IF                                                               
163000     .                                                                    
163100                                                                          
163200 F04-SEND-TMS-CLOSE          SECTION.                                     
163300     MOVE 'F04-SEND-TMS-CLOSE'     TO CURRENT-SECTION                     
163400                                                                          
163500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
163600     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
163700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
163800                                                                          
163900     IF SEND-KDRC > 0                                                     
164000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
164100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
164200       DELIMITED BY SIZE INTO ERRORTEXT                                   
164300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
164400     END-IF                                                               
164500     .                                                                    
164600                                                                          
164700 G-SEND-TMS-EMPTY SECTION.                                                
164800     MOVE 'G-SEND-TMS-EMPTY '    TO CURRENT-SECTION                       
164900                                                                          
165000     PERFORM G01-SEND-TMS-EMPTY-OPEN                                      
165100     PERFORM G02-SEND-TMS-EMPTY-HEADER                                    
165200     PERFORM G03-SEND-TMS-EMPTY-DATA                                      
165300     PERFORM G04-SEND-TMS-EMPTY-CLOSE                                     
165400     .                                                                    
165500                                                                          
165600 G01-SEND-TMS-EMPTY-OPEN SECTION.                                         
165700     MOVE 'G01-SEND-TMS-EMPTY-OPEN'                                       
165800                                 TO CURRENT-SECTION                       
165900                                                                          
166000                                                                          
166100     MOVE W-ADDISPABS-DEL         TO SEND-ADDISPABS                       
166200     MOVE 'OPEN'                  TO SEND-KDFUNC                          
166300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
166400                                     SEND-OPEN-AREA                       
166500     IF SEND-KDRC > ZERO                                                  
166600       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
166700       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
166800       DELIMITED BY SIZE INTO ERRORTEXT                                   
166900       CALL FELLOG                                                        
167000     ELSE                                                                 
167100       MOVE SEND-IDCOM            TO WS-SEND-IDCOM                        
167200     END-IF                                                               
167300     .                                                                    
167400                                                                          
167500 G02-SEND-TMS-EMPTY-HEADER  SECTION.                                      
167600     MOVE 'G02-SEND-TMS-EMPTY-HEADER'                                     
167700                                 TO CURRENT-SECTION                       
167800                                                                          
167900     PERFORM IMS-GU-WDGX0104-EMPTY                                        
168000     IF SEGMENT-FOUND                                                     
168100       MOVE 0104-IDAPI             TO IDAPI                               
168200       COMPUTE IDAPI-LEN       = FUNCTION BYTE-LENGTH (                   
168300                                 FUNCTION TRIM (IDAPI))                   
168400       MOVE 0104-IDPATH-API        TO IDPATH-API                          
168500       COMPUTE IDPATH-API-LEN  = FUNCTION BYTE-LENGTH (                   
168600                                 FUNCTION TRIM (IDPATH-API))              
168700       MOVE 0104-IDPTYP-API        TO IDPTYP-API                          
168800       COMPUTE IDPTYP-API-LEN  = FUNCTION BYTE-LENGTH (                   
168900                                 FUNCTION TRIM (IDPTYP-API))              
169000       MOVE 0104-IDUSERKEY        TO DEL-USER-KEY                         
169100       COMPUTE DEL-USER-KEY-LENGTH = FUNCTION BYTE-LENGTH (               
169200                                 FUNCTION TRIM (DEL-USER-KEY))            
169300     ELSE                                                                 
169400       CALL FELLOG                                                        
169500     END-IF                                                               
169600                                                                          
169700     MOVE 'PUT'                   TO SEND-KDFUNC                          
169800     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
169900     MOVE LENGTH OF WAPIINFO      TO SEND-KVDLEN                          
170000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
170100                                     SEND-KVDLEN                          
170200                                     WAPIINFO                             
170300     IF SEND-KDRC > ZERO                                                  
170400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
170500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
170600              DELIMITED BY SIZE INTO ERRORTEXT                            
170700       CALL FELLOG                                                        
170800     END-IF                                                               
170900     .                                                                    
171000                                                                          
171100 G03-SEND-TMS-EMPTY-DATA SECTION.                                         
171200     MOVE 'G03-SEND-TMS-EMPTY-DATA'                                       
171300                                 TO CURRENT-SECTION                       
171400                                                                          
171500     MOVE 'PUT'                   TO SEND-KDFUNC                          
171600     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
171700     MOVE LENGTH OF WTM01Q01-AREA TO SEND-KVDLEN                          
171800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
171900                                     SEND-KVDLEN                          
172000                                     WTM01Q01-AREA                        
172100                                                                          
172200     IF SEND-KDRC > ZERO                                                  
172300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
172400       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
172500       DELIMITED BY SIZE       INTO ERRORTEXT                             
172600       CALL FELLOG                                                        
172700     END-IF                                                               
172800     .                                                                    
172900                                                                          
173000 G04-SEND-TMS-EMPTY-CLOSE    SECTION.                                     
173100     MOVE 'G04-SEND-TMS-EMPTY-CLOSE'                                      
173200                                 TO CURRENT-SECTION                       
173300                                                                          
173400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
173500     MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
173600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
173700                                                                          
173800     IF SEND-KDRC > 0                                                     
173900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
174000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
174100       DELIMITED BY SIZE INTO ERRORTEXT                                   
174200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
174300     END-IF                                                               
174400     .                                                                    
174500                                                                          
174600 S10-GET-UTC-TIMESTAMP SECTION.                                           
174700                                                                          
174800     PERFORM IMS-GU-WDB601-PRIM                                           
174900     MOVE PRIM-DCS-IDTIDZON      TO MSGI-IDTIDZON                         
175000     MOVE PRIM-DCS-IDDC          TO MSGI-IDDC                             
175100     MOVE '014'                  TO MSGI-KDCALL                           
175200     MOVE 20                     TO WS-DATE-CC                            
175300     MOVE WS-DATE-YYMMDD         TO MSGI-TILOKDAT                         
175400     MOVE WS-TIME-HHMM           TO MSGI-TILOKTID                         
175500     CALL WL01TIDZ            USING MSGI-WL01TIDZ                         
175600     MOVE 20                     TO WS-DATE-UTC-CC                        
175700     MOVE MSGI-TILOKDAT          TO WS-DATE-UTC-YYMMDD                    
175800     MOVE MSGI-TILOKTID          TO WS-TIME-UTC-HHMM                      
175900     MOVE WS-TIME-SS             TO WS-TIME-UTC-SS                        
176000                                                                          
176100     MOVE FUNCTION FORMATTED-DATETIME (                                   
176200            'YYYY-MM-DDThh:mm:ssZ',                                       
176300            FUNCTION INTEGER-OF-DATE (WS-DATE-UTC),                       
176400            FUNCTION SECONDS-FROM-FORMATTED-TIME (                        
176500              'hhmmss', WS-TIME-UTC-X), 0)                                
176600                                 TO WS-TIMESTAMP-UTC                      
176700     .                                                                    
176800                                                                          
176900*IMS SEKTIONER                                                            
177000*                                                                         
177100*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
177200*                 III     III MM MMMMM MM SSSS   SSSS                     
177300*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
177400*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
177500*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
177600*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
177700*                 III     III MM MMMMM MM SSSS   SSSS                     
177800*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
177900*                                                                         
178000*                                                                         
178100 IMS-GU-1165 SECTION.                                                     
178200     MOVE 'IMS-GU-1165       '   TO IMS-SECTION                           
178300                                                                          
178400     STRING 'WL116501(WDGXKEY  =' W-1165KEY-X ')'                         
178500             DELIMITED BY SIZE INTO SSA1                                  
178600     MOVE '  GE'                 TO GODK-STATUSKODER                      
178700     CALL CBLTDLI             USING GU                                    
178800                                    1165-PCB                              
178900                                    DLI-IO-WDGX1165                       
179000                                    SSA1                                  
179100     MOVE 1165-STATUS-CODE       TO STATUS-WS                             
179200     PERFORM IMS-STATUSCHECK                                              
179300     .                                                                    
179400                                                                          
179500 IMS-GNP-1166 SECTION.                                                    
179600     MOVE 'IMS-GNP-1166       '  TO IMS-SECTION                           
179700                                                                          
179800     STRING 'WL116511(KDSEGKEY =' W-KDSEGKEY-X ')'                        
179900             DELIMITED BY SIZE INTO SSA1                                  
180000     MOVE '  GE'                 TO GODK-STATUSKODER                      
180100     CALL CBLTDLI             USING GNP                                   
180200                                    1165-PCB                              
180300                                    DLI-IO-WDGX1166                       
180400                                    SSA1                                  
180500     MOVE 1165-STATUS-CODE       TO STATUS-WS                             
180600     PERFORM IMS-STATUSCHECK                                              
180700     .                                                                    
180800                                                                          
180900 IMS-GNP-1168 SECTION.                                                    
181000     MOVE 'IMS-GNP-1168       '  TO IMS-SECTION                           
181100                                                                          
181200     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
181300             DELIMITED BY SIZE INTO SSA1                                  
181400     MOVE '  GE'                 TO GODK-STATUSKODER                      
181500     CALL CBLTDLI             USING GNP                                   
181600                                    1165-PCB                              
181700                                    DLI-IO-WDGX1168                       
181800                                    SSA1                                  
181900     MOVE 1165-STATUS-CODE       TO STATUS-WS                             
182000     PERFORM IMS-STATUSCHECK                                              
182100     .                                                                    
182200                                                                          
182300 IMS-GU-WDGX4142    SECTION.                                              
182400     MOVE 'IMS-GU-WDGX4142    '   TO IMS-SECTION                          
182500                                                                          
182600     STRING 'WDR201  (WDGXKEY  =' W-WDGX01-4141-X ')'                     
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     STRING 'WDGX4142(KY4142   =' W-KY4142-X ')'                          
182900          DELIMITED BY SIZE INTO SSA2                                     
183000     MOVE '  GE' TO GODK-STATUSKODER                                      
183100     CALL CBLTDLI USING GU  4141-PCB DLI-IO-4142 SSA1 SSA2                
183200     MOVE 4141-STATUS-CODE TO STATUS-WS                                   
183300     PERFORM IMS-STATUSCHECK                                              
183400     .                                                                    
183500                                                                          
183600 IMS-GU-WDB101 SECTION.                                                   
183700     MOVE 'IMS-GU-WDB101      '   TO IMS-SECTION                          
183800                                                                          
183900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
184000          DELIMITED BY SIZE INTO SSA1                                     
184100     MOVE '  GE' TO GODK-STATUSKODER                                      
184200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
184300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
184400     PERFORM IMS-STATUSCHECK                                              
184500     .                                                                    
184600                                                                          
184700 IMS-GU-WDB201 SECTION.                                                   
184800     MOVE 'IMS-GU-WDB201      '   TO IMS-SECTION                          
184900                                                                          
185000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
185100          DELIMITED BY SIZE INTO SSA1                                     
185200     MOVE '  GE' TO GODK-STATUSKODER                                      
185300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
185400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
185500     PERFORM IMS-STATUSCHECK                                              
185600     .                                                                    
185700                                                                          
185800 IMS-GU-WDB601    SECTION.                                                
185900     MOVE 'IMS-GU-WDB601      '   TO IMS-SECTION                          
186000                                                                          
186100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
186200     DELIMITED BY SIZE INTO SSA1                                          
186300     MOVE '  GE'                  TO GODK-STATUSKODER                     
186400     CALL CBLTDLI        USING GU WDB6-PCB DLI-IO-WDB601 SSA1             
186500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
186600     PERFORM IMS-STATUSCHECK                                              
186700     IF SEGMENT-MISSING                                                   
186800        MOVE SPACE TO DCS-KDDC                                            
186900     END-IF                                                               
187000     .                                                                    
187100                                                                          
187200 IMS-GU-WDB601-PRIM SECTION.                                              
187300     MOVE 'IMS-GU-WDB601-PRIM '  TO IMS-SECTION                           
187400                                                                          
187500     STRING 'WDB601  (IDDC     =' W-IDDC-PRIM-X ')'                       
187600             DELIMITED BY SIZE INTO SSA1                                  
187700     MOVE '  GE'                 TO GODK-STATUSKODER                      
187800     CALL CBLTDLI             USING GU                                    
187900                                    WDB6-PCB                              
188000                                    DLI-IO-WDB601-PRIM                    
188100                                    SSA1                                  
188200     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
188300     PERFORM IMS-STATUSCHECK                                              
188400     .                                                                    
188500                                                                          
188600 IMS-GU-WDGX0104  SECTION.                                                
188700     MOVE 'IMS-GU-WDGX0104    '   TO IMS-SECTION                          
188800                                                                          
188900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
189000             DELIMITED BY SIZE INTO SSA1                                  
189100     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
189200             DELIMITED BY SIZE INTO SSA2                                  
189300     MOVE '  GE'                 TO GODK-STATUSKODER                      
189400     CALL CBLTDLI USING GU  ATAB-PCB DLI-IO-WDGX0104 SSA1 SSA2            
189500     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
189600     PERFORM IMS-STATUSCHECK                                              
189700     .                                                                    
189800                                                                          
189900 IMS-GU-WDGX0104-EMPTY SECTION.                                           
190000     MOVE 'IMS-GU-WDGX0104    '   TO IMS-SECTION                          
190100                                                                          
190200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
190300             DELIMITED BY SIZE INTO SSA1                                  
190400     STRING 'WDGX0104(ADDISPAB =' W-KY0104-DEL ')'                        
190500             DELIMITED BY SIZE INTO SSA2                                  
190600     MOVE '  GE'                 TO GODK-STATUSKODER                      
190700     CALL CBLTDLI USING GU  ATAB-PCB DLI-IO-WDGX0104 SSA1 SSA2            
190800     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
190900     PERFORM IMS-STATUSCHECK                                              
191000     .                                                                    
191100                                                                          
191200 IMS-GU-WDD311     SECTION.                                               
191300     MOVE 'IMS-GU-WDD311      '   TO IMS-SECTION                          
191400                                                                          
191500     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
191800          DELIMITED BY SIZE INTO SSA2                                     
191900     MOVE '  GE' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
192100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSCHECK                                              
192300     .                                                                    
192400                                                                          
192500 IMS-GU-WDE401-ASEQ SECTION.                                              
192600     MOVE 'GU-WDE401-ASEQ    '   TO IMS-SECTION                           
192700                                                                          
192800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
192900          DELIMITED BY SIZE INTO SSA1                                     
193000     MOVE '  GE' TO GODK-STATUSKODER                                      
193100     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-WDE401 SSA1                   
193200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
193300     PERFORM IMS-STATUSCHECK                                              
193400     .                                                                    
193500                                                                          
193600 IMS-GN-WDE401-ASEQ SECTION.                                              
193700     MOVE 'GN-WDE401-ASEQ    '   TO IMS-SECTION                           
193800                                                                          
193900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
194000          DELIMITED BY SIZE INTO SSA1                                     
194100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
194200     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE401 SSA1                   
194300     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
194400     PERFORM IMS-STATUSCHECK                                              
194500     .                                                                    
194600                                                                          
194700 IMS-GU-WDE401-FSEQ  SECTION.                                             
194800     MOVE 'GU-WDE401-FSEQ    '   TO IMS-SECTION                           
194900                                                                          
195000     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
195100          DELIMITED BY SIZE INTO SSA1                                     
195200     MOVE 'WDE401' TO SSA2                                                
195300     MOVE '  GE' TO GODK-STATUSKODER                                      
195400     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4 SSA1 SSA2                
195500     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
195600     PERFORM IMS-STATUSCHECK                                              
195700     .                                                                    
195800                                                                          
195900 IMS-GU-WDE411-21-FSEQ SECTION.                                           
196000     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
196100            DELIMITED BY SIZE INTO SSA1                                   
196200     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
196300            DELIMITED BY SIZE INTO SSA2                                   
196400     MOVE '    ' TO GODK-STATUSKODER                                      
196500     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE421 SSA1 SSA2              
196600     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
196700     PERFORM IMS-STATUSCHECK                                              
196800     .                                                                    
196900                                                                          
197000 IMS-GN-WDE411-21-FSEQ SECTION.                                           
197100     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
197200            DELIMITED BY SIZE INTO SSA1                                   
197300     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
197400            DELIMITED BY SIZE INTO SSA2                                   
197500     MOVE '  GE' TO GODK-STATUSKODER                                      
197600     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-WDE421 SSA1 SSA2              
197700     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
197800     PERFORM IMS-STATUSCHECK                                              
197900     .                                                                    
198000                                                                          
198100 IMS-GU-WDQ201    SECTION.                                                
198200     MOVE 'IMS-GU-WDQ201     '   TO IMS-SECTION                           
198300                                                                          
198400     STRING 'WDQ201  (IDORDER  =' W-WDQ201-X ')'                          
198500            DELIMITED BY SIZE INTO SSA1                                   
198600     MOVE '    ' TO GODK-STATUSKODER                                      
198700     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
198800     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
198900     PERFORM IMS-STATUSCHECK                                              
199000     .                                                                    
199100                                                                          
199200 IMS-GET-WDQ201-CSEQ SECTION.                                             
199300                                                                          
199400     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
199500             DELIMITED BY SIZE INTO    SSA1                               
199600     MOVE '  GE' TO GODK-STATUSKODER                                      
199700     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-ORQI01 SSA1                   
199800     MOVE WDQ2C-STATUS-CODE TO STATUS-WS                                  
199900     PERFORM IMS-STATUSCHECK                                              
200000     .                                                                    
200100                                                                          
200200 IMS-GU-WDQ301    SECTION.                                                
200300     MOVE 'IMS-GU-WDQ301     '   TO IMS-SECTION                           
200400                                                                          
200500     STRING 'WDQ301  (WDQ301KY =' W-Q301-KEY-X ')'                        
200600            DELIMITED BY SIZE INTO SSA1                                   
200700     MOVE '  GE' TO GODK-STATUSKODER                                      
200800     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
200900     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
201000     PERFORM IMS-STATUSCHECK                                              
201100     .                                                                    
201200                                                                          
201300 IMS-GU-WDK601  SECTION.                                                  
201400     MOVE 'IMS-GU-WDK601     '   TO IMS-SECTION                           
201500                                                                          
201600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
201700          DELIMITED BY SIZE INTO SSA1                                     
201800     MOVE '  GE' TO GODK-STATUSKODER                                      
201900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
202000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
202100     PERFORM IMS-STATUSCHECK                                              
202200     .                                                                    
202300                                                                          
202400 IMS-GNP-WDK611  SECTION.                                                 
202500     MOVE 'IMS-GNP-WDK611    '   TO IMS-SECTION                           
202600                                                                          
202700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
202800          DELIMITED BY SIZE INTO SSA1                                     
202900     MOVE '  GE' TO GODK-STATUSKODER                                      
203000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
203100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSCHECK                                              
203300     .                                                                    
203400                                                                          
203500 IMS-GU-WDE611 SECTION.                                                   
203600     MOVE 'IMS-GU-WDE611     '   TO IMS-SECTION                           
203700                                                                          
203800     STRING 'WDE601  (IDPRODNR =' W-E6-IDPRODNR-X ')'                     
203900            DELIMITED BY SIZE INTO SSA1                                   
204000     STRING 'WDE611  (IDKOLLI  =' W-E6-IDKOLLI-X ')'                      
204100            DELIMITED BY SIZE INTO SSA2                                   
204200     MOVE '  ' TO GODK-STATUSKODER                                        
204300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
204400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
204500     PERFORM IMS-STATUSCHECK                                              
204600     .                                                                    
204700                                                                          
204800 IMS-GU-WDK501    SECTION.                                                
204900     MOVE 'IMS-GU-WDK501     '   TO IMS-SECTION                           
205000                                                                          
205100     STRING 'WDK501  (KDKOLLI  =' W-K5-KDKOLLI-X ')'                      
205200            DELIMITED BY SIZE INTO SSA1                                   
205300     MOVE '  GE' TO GODK-STATUSKODER                                      
205400     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
205500     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
205600     PERFORM IMS-STATUSCHECK                                              
205700     .                                                                    
205800                                                                          
205900 IMS-STATUSCHECK SECTION.                                                 
206000     SET STATUS-IX TO 1                                                   
206100     SEARCH GODK-STATUS AT END CALL FELLOG                                
206200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
206300     END-SEARCH                                                           
206400     CONTINUE                                                             
206500     .                                                                    
