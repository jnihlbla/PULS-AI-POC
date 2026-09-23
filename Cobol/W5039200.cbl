000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5039200.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   MARS  2000.                                              
000500                                                                          
000600*    FUNKTION.                                                            
000700*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000800*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0174               
000900*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001000*        INVENTERING, SUBPROGRAM TILL BILD 5302.                          
001100*        UTSKRIFT AV INVENTERINGSANMODAN                                  
001200*        UTSKRIFT KAN VÄLJAS PÅ FÖLJANDE SÄTT:                            
001300*        OM ANTAL KOMBINERAS MED OMRÅDE LÄSES BASEN FRÅN BÖRJAN.          
001400*        PRIORITERADE ARTIKLAR (PRIORITET = 1) SKRIVS UT I FÖRSTA         
001500*        HAND OBEROENDE AV OMRÅDE.                                        
001600*                                                                         
001700*    SUBPROGRAM:                                                          
001800*        W006PRS1   - SKÖTER ALL SKRIVNING MOT IMS-PRINTER                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W5T392X                                             
002200*        MID:         W5I39201                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        LISTA:  INVENTERINGSANMODAN                                      
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200*    -COPY WY2000W1                                                       
003300                                                                          
003400 77    IDPGM                     PIC X(8)    VALUE 'W5030200'.            
003500 77    JA                        PIC X       VALUE 'J'.                   
003600 77    NEJ                       PIC X       VALUE 'N'.                   
003700 77    WS-RAD-RAEKNARE           PIC S9(3)   VALUE  +0  COMP-3.           
003800 77    W-ANT-SIDOR               PIC 9(2)    VALUE   0.                   
003900 77    DUMMY-AREA                PIC X(50)   VALUE SPACE.                 
004000 77    SPRAK-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
004100 77    TAB-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77    IND                       PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77    ANTAL                     PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77    MAX-IX-7                  PIC S9(9)   VALUE +7   COMP SYNC.        
004700 77    MAX-IX-16                 PIC S9(9)   VALUE +16  COMP SYNC.        
004800 77    MAX-IX-20                 PIC S9(9)   VALUE +21  COMP SYNC.        
004900 77    CD-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
005000 77    K6-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
005100 77    CD-IX-MAX                 PIC S9(3)   VALUE +4   COMP-3.           
005200 77    SALDO-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77    PRINT-ANT                 PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77    INV-ANM1-3-CDC            PIC X(8)    VALUE '302     '.            
005500 77    INV-ANM1-3-BJ             PIC X(8)    VALUE '301     '.            
005600 77    INV-ANM-DC21ET            PIC X(8)    VALUE '303     '.            
005700 77    INV-ANM-DC21              PIC X(8)    VALUE '304     '.            
005800                                                                          
005900 77    WDB6-A-SW                 PIC X       VALUE 'J'.                   
006000       88  WDB6-A-FINNS                      VALUE 'J'.                   
006100       88  WDB6-A-SAKNAS                     VALUE 'N'.                   
006200                                                                          
006300*01  -COPY WWDCKONS -PRE KONS-                                            
006400                                                                          
006500 01    WS-QTY-CDC                PIC S9(7)   VALUE  +0.                   
006600 01    WS-QTY-SDC                PIC S9(7)   VALUE  +0.                   
006700                                                                          
006800 01    W-BLANKRAD                PIC X(132)  VALUE SPACE.                 
006900 01    WS-TISEGKEY               PIC 9(9).                                
007000                                                                          
007100 01    WS-IDPRTINV.                                                       
007200    03 WS-IDPRTOMG               PIC S9     COMP-3.                       
007300    03 WS-IDLOPNR                PIC S9(5)  COMP-3.                       
007400                                                                          
007500 01    W-IDPRTOMG-ALFA           PIC X.                                   
007600                                                                          
007700 01    WS-IDPRTINV-NUM           PIC 9(6).                                
007800 01    WS-IDLOPNR-5              PIC 9(5).                                
007900 01    WS-IDUSER                 PIC X(8) VALUE SPACE.                    
008000 01    WS-DAGENS-DATUM           PIC 9(8) VALUE ZERO.                     
008100     EJECT                                                                
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300   88  EGEN-TRANS                            VALUE '5392'.                
008400   88  GODK-TRANS                            VALUE '5392' '5302'.         
008500                                                                          
008600 77  WS-SATS                     PIC X(1)    VALUE 'N'.                   
008700   88  SATS-FINNS                            VALUE 'J'.                   
008800                                                                          
008900 77    INDATA-SW                 PIC X.                                   
009000   88  INDATA-OK                             VALUE 'J'.                   
009100                                                                          
009200 77    PRINT-SW                  PIC X.                                   
009300   88  PRINT-OK                              VALUE 'J'.                   
009400                                                                          
009500 77    BUFFERT-SW                PIC X.                                   
009600   88  BUFFERT-PRINT                         VALUE 'J'.                   
009700                                                                          
009800 77    CROSSD-SW                 PIC X.                                   
009900   88  CROSSDOCKING-PRINT                    VALUE 'J'.                   
010000                                                                          
010100 77    RUBRIK-SW                 PIC X       VALUE 'N'.                   
010200   88  RUBRIK-UTSKRIVEN                      VALUE 'J'.                   
010300   88  RUBRIK-INTE-UTSKRIVEN                 VALUE 'N'.                   
010400                                                                          
010500 01  WS-KVEFRS-OLD               PIC S9(7) VALUE ZERO COMP-3.             
010600 01  WS-ANTAL-RADER-EFR          PIC 9(7) VALUE ZERO.                     
010700 01  WS-LAGE                     PIC X(16).                               
010800 01  WSATS-KVLEVART-TOT          PIC S9(9) VALUE ZERO.                    
010900 01  WS-KVLEVART-TOT             PIC S9(9) VALUE ZERO.                    
011000 01  WS-BELEV                    PIC X(30) VALUE SPACE.                   
011100 01  WS-IDBENR                   PIC S9  COMP-3 VALUE ZERO.               
011200 01  WS-IDLEVNR                  PIC X(5).                                
011300                                                                          
011400 01  TABELL.                                                              
011500     03 TAB-RAD OCCURS 5.                                                 
011600        05   TAB-ADLAGOMR-CD     PIC S9(3) COMP-3.                        
011700        05   TAB-ADGANG-CD       PIC S9(3) COMP-3.                        
011800        05   TAB-ADPLATS-CD      PIC S9(5) COMP-3.                        
011900        05   TAB-KVLS-CD         PIC S9(7) COMP-3.                        
012000     EJECT                                                                
012100 01  DYNAMISKA-SUBPROGRAM.                                                
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012600     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
012700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013000*01  -COPY  WMSGINIT                                                      
013100     EJECT                                                                
013200*    -- VALID IDDC CODES                                                  
013300 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
013400*01  -COPY  WWDC99                                                        
013500*01  -COPY  WWDC99  -PRE SPAR-                                            
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
013800*01  -COPY  WDATAREA                                                      
013900     EJECT                                                                
014000 01  WS-FAELT.                                                            
014100     03  WS-TIAAVVD              PIC 9(5).                                
014200     03  W-SDC-ANTAL             PIC S9(9)   VALUE  ZERO COMP-3.          
014300                                                                          
014400 01  WS-PRINTERDEST.                                                      
014500     03  WS-PRT1                 PIC X(8).                                
014600                                                                          
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'NYC-TILL-DLI'.        
014900 01    NYCKLAR-TILL-DLI.                                                  
015000     03  W-WDJ1CSEQ-X.                                                    
015100         05  W-IDLEVNR            PIC X(5)  VALUE SPACE.                  
015200         05  W-BELEVART           PIC X(30) VALUE SPACE.                  
015300         05  W-IDARTNR-WDJ1C      PIC S9(9) VALUE ZERO    COMP-3.         
015400   03  W-ROT-WDG3KEY-X.                                                   
015500     05  FILLER                  PIC X(4)    VALUE '5113'.                
015600     05  W-IDDC-G3               PIC X(2)    VALUE SPACE.                 
015700     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
015800                                                                          
015900   03  W-IDARTNR-100000000-X.                                             
016000     05  W-IDARTNR-100000000     PIC S9(9)   COMP-3                       
016100                                 VALUE +100000000.                        
016200   03  W-IDARTNR-X.                                                       
016300     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
016400   03  W-ORDSTA-X.                                                        
016500     05  W-ORDSTA                PIC S9      COMP-3 VALUE +4.             
016600   03  W-IDSKYLT-X.                                                       
016700     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
016800                                                                          
016900   03  W-IDDC                    PIC X(2)    VALUE SPACE.                 
017000                                                                          
017100   03  W-TISEGKEY-X.                                                      
017200     05  W-TISEGKEY         PIC S9(9)   VALUE +999999999 COMP-3.          
017300                                                                          
018801   03  W-WDD8B1KY-MIN-X.                                                  
018900     05 W-IDDC-D8-MIN            PIC X(2)        VALUE SPACE.             
019002     05 W-IDARTNR-D8-MIN         PIC S9(9) COMP-3 VALUE ZERO.             
019102     05 FILLER                   PIC X(15)   VALUE LOW-VALUE.             
019300                                                                          
019402   03 W-WDD8B1KY-MAX-X.                                                   
019500     05 W-IDDC-D8-MAX            PIC X(2)         VALUE SPACE.            
019602     05 W-IDARTNR-D8-MAX         PIC S9(9) COMP-3 VALUE ZERO.             
019702     05 FILLER                   PIC X(15)   VALUE HIGH-VALUE.            
019900                                                                          
020700   03  W-WDH111KY-MIN-X.                                                  
020800     05  W-IDDC-WDH1-MIN      PIC X(2)  VALUE SPACE.                      
020900     05  W-KDINVKAT-WDH1-MIN  PIC S9(3)                  COMP-3.          
021000     05  W-TISEGKEY-WDH1-MIN  PIC S9(9) VALUE ZERO       COMP-3.          
021100     05  W-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                         
021200   03  W-WDH111KY-MAX-X.                                                  
021300     05  W-IDDC-WDH1-MAX      PIC X(2)  VALUE SPACE.                      
021400     05  W-KDINVKAT-WDH1-MAX  PIC S9(3)                  COMP-3.          
021500     05  W-TISEGKEY-WDH1-MAX  PIC S9(9) VALUE +999999999 COMP-3.          
021600     05  W-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                     
021700     EJECT                                                                
021800   03  W-WDE4C1KY-LOW.                                                    
021900     05  W-IDARTNR-LOW           PIC S9(9)   COMP-3.                      
022000     05  FILLER                  PIC X(7)    VALUE  LOW-VALUE.            
022100   03  W-WDE4C1KY-HIGH.                                                   
022200     05  W-IDARTNR-HIGH          PIC S9(9)   COMP-3.                      
022300     05  FILLER                  PIC X(7)    VALUE  HIGH-VALUE.           
022400   03  W-IDDISTR-X.                                                       
022500     05  W-IDDISTR               PIC S9(5)   COMP-3.                      
022600   03  W-WDE4KEY-X.                                                       
022700     05  W-IDDISTR-WDE4          PIC S9(5)   COMP-3.                      
022800     05  W-IDKUNDNR              PIC S9(7)   COMP-3.                      
022900     05  W-IDKUNDRF              PIC X(10).                               
023000     05  W-IDPRODNR              PIC S9(7)   COMP-3.                      
023100     05  W-IDPLKLST              PIC S9(3)   COMP-3.                      
023200   03  W-IDPURAD-X.                                                       
023300     05  W-IDPURAD               PIC S9(5)   COMP-3.                      
023400   03  W-WDG3KEY01-X.                                                     
023500     05  W-IDHTYP                PIC X(4)    VALUE '5101'.                
023600     05  W-FILLER                PIC X(26)   VALUE LOW-VALUE.             
023700   03  W-WDGX5102-X.                                                      
023800     05  W-KDSEGKEY              PIC X       VALUE '1'.                   
023900     05  W-IDLOPNR               PIC S9(5)   COMP-3.                      
024000                                                                          
024100   03  W-IDDC-B6-X.                                                       
024200     05 W-IDDC-B6                PIC X(2).                                
024300                                                                          
024400     EJECT                                                                
024500 01  ANT-MEDDELANDE.                                                      
024600   03  SV-ANT-MED.                                                        
024700     05  FILLER                  PIC X(12)                                
024800         VALUE 'BEGÄRT ANT  '.                                            
024900     05  SV-BEG-ANT              PIC 9(2).                                
025000     05  FILLER                  PIC X(9)                                 
025100         VALUE ' PRINTAT '.                                               
025200     05  SV-PRINT-ANT            PIC 9(2).                                
025300   03  BG-ANT-MED.                                                        
025400     05  FILLER                  PIC X(12)                                
025500         VALUE 'AANGEVRAAGD '.                                            
025600     05  BG-BEG-ANT              PIC 9(2).                                
025700     05  FILLER                  PIC X(9)                                 
025800         VALUE ' GEDRUKT '.                                               
025900     05  BG-PRINT-ANT            PIC 9(2).                                
026000     SKIP3                                                                
026100 01  ANTAL-MED REDEFINES ANT-MEDDELANDE.                                  
026200   03  ANTALS-RAD OCCURS 2.                                               
026300     05  FILLER                  PIC X(12).                               
026400     05  BEGAERT-ANT             PIC 9(2).                                
026500     05  FILLER                  PIC X(9).                                
026600     05  PRINTAT-ANT             PIC 9(2).                                
026700     EJECT                                                                
026800 01  ART-MEDDELANDE.                                                      
026900   03  SV-ART-MED.                                                        
027000     05  SV-PRINT-ARTIKEL        PIC Z(9).                                
027100     05  FILLER                  PIC X(10)                                
027200         VALUE ' UTSKRIVEN'.                                              
027300   03  BG-ART-MED.                                                        
027400     05  BG-PRINT-ARTIKEL        PIC Z(9).                                
027500     05  FILLER                  PIC X(10)                                
027600         VALUE ' GEDRUKT'.                                                
027700     SKIP2                                                                
027800 01  ARTIKEL-MED REDEFINES ART-MEDDELANDE.                                
027900   03  ARTIKEL-RAD OCCURS 2.                                              
028000     05  PRINT-ARTIKEL           PIC Z(9).                                
028100     05  FILLER                  PIC X(10).                               
028200     EJECT                                                                
028300 01    MEDDELANDE.                                                        
028400   03  FEL1.                                                              
028500     05  FILLER                  PIC X(40)                                
028600           VALUE 'UPPDATERING ENBART FRÅN EGEN BILD'.                     
028700     05  FILLER                  PIC X(40)                                
028800           VALUE 'UPPDATERING ENBART FRÅN EGEN BILD'.                     
028900   03  FILLER REDEFINES FEL1.                                             
029000     05  FEL-1                   PIC X(40)  OCCURS 2.                     
029100     EJECT                                                                
029200*************************************************                         
029300*****    LISTTRANSAR FÖR CDC-STOCKTAKING   ******                         
029400*************************************************                         
029500 01  RUBRAD-LISTA.                                                        
029600   03  FILLER                       PIC X(24)  VALUE                      
029700              'VOLVO CAR CORPORATION.  '.                                 
029800   03  FILLER                       PIC X(13)  VALUE                      
029900              'W50302-002   '.                                            
030000   03  FILLER                       PIC X(25)  VALUE                      
030100              'STOCKTAKING DOKUMENT  DC '.                                
030200   03  RUB-IDDC                     PIC X(2)   VALUE SPACE.               
030300   03  FILLER                       PIC X(10)  VALUE SPACE.               
030400   03  FILLER                       PIC X(6)   VALUE                      
030500              'PRINT '.                                                   
030600   03  RUB-DATUM                    PIC 9(6)   VALUE ZERO.                
030700   03  FILLER                       PIC X(1)   VALUE '-'.                 
030800   03  RUB-TID                      PIC 9(4)   VALUE ZERO.                
030900   03  FILLER                       PIC X(2)   VALUE SPACE.               
031000   03  FILLER                       PIC X(7)   VALUE                      
031100             'LISTNO:'.                                                   
031200   03  RUB-PRINTNR                  PIC Z(6)   VALUE ZERO.                
031300   03  FILLER                       PIC X(8)   VALUE                      
031400              ' PAGENR '.                                                 
031500   03  ANTAL-SIDOR                  PIC Z9     VALUE ZERO.                
031600   03  FILLER                       PIC X(16)  VALUE SPACE.               
031700 01  RAD-1-LISTA.                                                         
031800   03  FILLER                       PIC X(10)  VALUE 'PARTNO.   '.        
031900   03  FILLER                       PIC X(14)  VALUE                      
032000              'DESCRIPT.     '.                                           
032100   03  FILLER                       PIC X(13)  VALUE                      
032200              ' ADRESS      '.                                            
032300   03  FILLER                       PIC X(9)   VALUE                      
032400              '     QTY '.                                                
032500   03  FILLER                       PIC X(14)  VALUE                      
032600              '  BUFFER     '.                                            
032700   03  FILLER                       PIC X(8)   VALUE                      
032800              '   QTY  '.                                                 
032900   03  FILLER                       PIC X(8)   VALUE                      
033000              '    QTY '.                                                 
033100   03  FILLER                       PIC X(10)   VALUE                     
033200              'TOT.QTY   '.                                               
033300   03  FILLER                       PIC X(09)  VALUE                      
033400              ' DIFF    '.                                                
033500   03  FILLER                       PIC X(9)   VALUE                      
033600              'ST.BAL   '.                                                
033700   03  FILLER                       PIC X(7)   VALUE                      
033800              '    AK '.                                                  
033900   03  FILLER                       PIC X(6)   VALUE                      
034000              '   EFR'.                                                   
034100   03  FILLER                       PIC X(11)  VALUE SPACE.               
034200 01  RAD-2-LISTA.                                                         
034300   03  RAD-IDARTNR                  PIC Z(9)   VALUE ZERO.                
034400   03  FILLER                       PIC X      VALUE SPACE.               
034500   03  RAD-BEART                    PIC X(13)  VALUE SPACE.               
034600   03  FILLER                       PIC X(1)   VALUE SPACE.               
034700   03  RAD-ADLAGOMR                 PIC Z(2)9  VALUE ZERO.                
034800   03  FILLER                       PIC X(1)   VALUE SPACE.               
034900   03  RAD-ADGANG                   PIC Z(2)9  VALUE ZERO.                
035000   03  FILLER                       PIC X(1)   VALUE SPACE.               
035100   03  RAD-ADPLATS                  PIC Z(4)9  VALUE ZERO.                
035200   03  FILLER                       PIC X(1)   VALUE SPACE.               
035300   03  FILLER                       PIC X(8)   VALUE                      
035400              '....... '.                                                 
035500   03  RAD-BUFF-ADLAGOMR            PIC Z(2)9  VALUE ZERO.                
035600   03  FILLER                       PIC X(1)   VALUE SPACE.               
035700   03  RAD-BUFF-ADGANG              PIC Z(2)9  VALUE ZERO.                
035800   03  FILLER                       PIC X(1)   VALUE SPACE.               
035900   03  RAD-BUFF-ADPLATS             PIC Z(4)9  VALUE ZERO.                
036000   03  FILLER                       PIC X(1)   VALUE SPACE.               
036100   03  RAD-BUFF-KVLS                PIC Z(5)9- VALUE ZERO.                
036200   03  FILLER                       PIC X(1)   VALUE SPACE.               
036300   03  FILLER                       PIC X(8)   VALUE                      
036400              '....... '.                                                 
036500   03  FILLER                       PIC X(8)   VALUE                      
036600              '....... '.                                                 
036700   03  FILLER                       PIC X(8)   VALUE                      
036800              '....... '.                                                 
036900   03  FILLER                       PIC X(3)   VALUE SPACE.               
037000   03  RAD-KVLS                     PIC Z(5)9- VALUE ZERO.                
037100   03  FILLER                       PIC X(1)   VALUE SPACE.               
037200   03  RAD-KVAKS                    PIC Z(5)9- VALUE ZERO.                
037300   03  FILLER                       PIC X(1)   VALUE SPACE.               
037400   03  RAD-KVEFRS                   PIC Z(5)9- VALUE ZERO.                
037500   03  FILLER                       PIC X(11)  VALUE SPACE.               
037600*01 RAD-2X-LISTA.                                                         
037700** 03  RAD2X-RENAULT-NR             PIC X(10)  VALUE SPACE.               
037800** 03  RAD2X-RENARTNR               PIC X(30)  VALUE SPACE.               
037900   03  FILLER                       PIC X(90)  VALUE SPACE.               
038000 01 RAD-3-LISTA.                                                          
038100   03  FILLER                       PIC X(24)  VALUE SPACE.               
038200   03  RAD3-ADLAGOMR-CD             PIC Z(2)9  BLANK WHEN ZERO.           
038300   03  FILLER                       PIC X(1)   VALUE SPACE.               
038400   03  RAD3-ADGANG-CD               PIC Z(2)9  BLANK WHEN ZERO.           
038500   03  FILLER                       PIC X(1)   VALUE SPACE.               
038600   03  RAD3-ADPLATS-CD              PIC Z(4)9  BLANK WHEN ZERO.           
038700   03  FILLER                       PIC X(1)   VALUE SPACE.               
038800   03  RAD3-KVLS-CD                 PIC Z(5)9- BLANK WHEN ZERO.           
038900   03  FILLER                       PIC X(1)   VALUE SPACE.               
039000   03  RAD3-BUFF-ADLAGOMR           PIC Z(2)9  BLANK WHEN ZERO.           
039100   03  FILLER                       PIC X(1)   VALUE SPACE.               
039200   03  RAD3-BUFF-ADGANG             PIC Z(2)9  BLANK WHEN ZERO.           
039300   03  FILLER                       PIC X(1)   VALUE SPACE.               
039400   03  RAD3-BUFF-ADPLATS            PIC Z(4)9  BLANK WHEN ZERO.           
039500   03  FILLER                       PIC X(1)   VALUE SPACE.               
039600   03  RAD3-BUFF-KVLS               PIC Z(5)9- BLANK WHEN ZERO.           
039700   03  FILLER                       PIC X(1)   VALUE SPACE.               
039800   03  FILLER                       PIC X(8)   VALUE                      
039900              '....... '.                                                 
040000   03  FILLER                       PIC X(8)   VALUE                      
040100              '....... '.                                                 
040200   03  FILLER                       PIC X(8)   VALUE                      
040300              '....... '.                                                 
040400   03  FILLER                       PIC X(36)  VALUE SPACE.               
040500                                                                          
040600     EJECT                                                                
040700 01    FILLER                    PIC X(16)     VALUE 'W006PRAR'.          
040800                                                                          
040900*01  -COPY W006PRAR                                                       
041000     EJECT                                                                
041100******************************************************************        
041200*                                                                         
041300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041400*                                                                         
041500 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
041600     SKIP3                                                                
041700*01    MID -COPY W5I39201                                                 
041800     EJECT                                                                
041900* - - - - - - - - - - - - - - - MSG-AREA                                  
042000*01    -COPY WMSGAREA                                                     
042100     EJECT                                                                
042200*01    -COPY WMFSAREA                                                     
042300     EJECT                                                                
042400******************************************************************        
042500 01  FILLER                      PIC X(16)   VALUE 'SPAR-WDH111'.         
042600 01  SPAR-WDH111-AREA.                                                    
042700     03  WDH111.                                                          
042800*        05  -COPY WDH111 -PRE SPAR-                                      
042900     EJECT                                                                
043000*                                                                         
043100******************************************************************        
043200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043300*                                                                         
043400 01    IMS-WS.                                                            
043500   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
043600     SKIP3                                                                
043700*                        **** STATUS-KOD FRÅN IMS                         
043800   03    STATUS-WS               PIC XX.                                  
043900     88    SEGMENT-FINNS                     VALUE '  '.                  
044000     88    SEGMENT-EXISTS                    VALUE 'II'.                  
044100     88    SEGMENT-SAKNAS                    VALUE 'GE' 'GB'.             
044200     88    INDEX-EXISTS                      VALUE 'NI'.                  
044300     88    BASEN-SLUT                        VALUE 'GB'.                  
044400     SKIP3                                                                
044500   03    GODK-STATUSKODER.                                                
044600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
044700     SKIP3                                                                
044800 01    SSA1                      PIC X(128).                              
044900 01    SSA2                      PIC X(128).                              
045000     EJECT                                                                
045100*                            IMS FUNKTIONSKODER                           
045200*01    -COPY W0003                                                        
045300     EJECT                                                                
045400*                            DL1 INPUT-OUTPUT AREA                        
045500 01    DLI-IO-AREA.                                                       
045600   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
045700     SKIP3                                                                
045800*  03    WLARTC01 -COPY WDK601              -RED IO-AREA.                 
045900     EJECT                                                                
046000*  03    WLARTC11 -COPY WDK611              -RED IO-AREA.                 
046100     EJECT                                                                
046200*  03  WLBENA11 -COPY WDD311 -PRE BEN-      -RED IO-AREA.                 
046300     EJECT                                                                
046400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E4C1'.           
046500 01  DLI-IO-E4C1.                                                         
046600*  03  -COPY WDE4C1                                                       
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E401'.           
046900 01  DLI-IO-E401.                                                         
047000*  03  -COPY WDE401                                                       
047100     EJECT                                                                
047200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E411'.           
047300 01  DLI-IO-E411.                                                         
047400*  03  -COPY WDE411                                                       
047500     EJECT                                                                
047600 01  FILLER                   PIC X(16) VALUE 'DLI-IO-AREA3    '.         
047700                                                                          
047800 01  DLI-IO-AREA3.                                                        
047900    03  IO-AREA3              PIC X(350)  VALUE SPACE.                    
048000     SKIP2                                                                
048100*    03 WLSATE -COPY WDJ1C1      -PRE SATE01-  -RED IO-AREA3.             
048200     EJECT                                                                
048300   03   WLSATB-IN   REDEFINES IO-AREA3.                                   
048400*    05 WLSATB -COPY WDJ111      -PRE SATB11-.                            
048500     SKIP3                                                                
048600*    05 WLSATB -COPY WDJ101      -PRE SATB01-.                            
048700     EJECT                                                                
048800 01    DLI-IO-AREA4.                                                      
048900   03  IO-AREA4                  PIC X(600)  VALUE SPACE.                 
049000     SKIP3                                                                
049100*  03  WLARTS01 -COPY WDK701               -RED IO-AREA4.                 
049200     EJECT                                                                
049300*  03  WLARTS11 -COPY WDK711               -RED IO-AREA4.                 
049400     EJECT                                                                
049500 01  FILLER                      PIC X(16)   VALUE 'WDH101-AREA'.         
049600 01  DLI-IO-AREA-INVA01.                                                  
049700     03  WDH101.                                                          
049800*        05  -COPY WDH101                                                 
049900 01  FILLER                      PIC X(16)   VALUE 'WDH111-AREA'.         
050000 01  DLI-IO-AREA-INVA11.                                                  
050100     03  WDH111.                                                          
050200*        05  -COPY WDH111                                                 
050300     EJECT                                                                
050400 01  FILLER                      PIC X(16)   VALUE 'WDH121-AREA'.         
050500 01  DLI-IO-AREA-INVA21.                                                  
050600     03  WDH121.                                                          
050700*        05  -COPY WDH121                                                 
050800     EJECT                                                                
050900 01  FILLER                      PIC X(16)   VALUE 'WDGX5102AREA'.        
051000 01  DLI-IO-AREA-WDGX5102.                                                
051100     03  WDGX5102.                                                        
051200*        05  -COPY WDGX5102                                               
051300     EJECT                                                                
051400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
051500 01   DLI-IO-AREA-B601.                                                   
051600*     03  -COPY WDB601                                                    
051700     EJECT                                                                
052102 01  FILLER               PIC X(16)   VALUE 'WDD8B1 AREA'.                
052202 01   DLI-IO-WDD8B1.                                                      
052302*     03  -COPY WDD8B1                                                    
052400     EJECT                                                                
052500 LINKAGE SECTION.                                                         
052600                                                                          
052700*01    -COPY W0009     -PRE MSG-                                          
052800                                                                          
052900 01  ALT-PCB                     PIC X(32).                               
053000     EJECT                                                                
053100*01    -COPY W0008     -PRE USEA-                                         
053200     05  FILLER                  PIC X.                                   
053300                                                                          
053400*01    -COPY W0008     -PRE ARTC-                                         
053500     05  FILLER                  PIC X.                                   
053600     EJECT                                                                
053702*01    -COPY W0008     -PRE WDD8B-                                        
053800     05  FILLER                  PIC X.                                   
053900                                                                          
054000*01    -COPY W0008     -PRE WDE4C-                                        
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300*01    -COPY W0008     -PRE WDE4-                                         
054400     05  FILLER                  PIC X.                                   
054500                                                                          
054600*01    -COPY W0008     -PRE INV-                                          
054700     05  FILLER                  PIC X.                                   
054800                                                                          
054900*01    -COPY W0008     -PRE BEN-                                          
055000     05  FILLER                  PIC X.                                   
055100                                                                          
055200*    -COPY W0008       -PRE SATB-.                                        
055300      05    FILLER           PIC X.                                       
055400     EJECT                                                                
055500                                                                          
055600*    -COPY W0008       -PRE ARTS-.                                        
055700      05    FILLER           PIC X.                                       
055800                                                                          
055900*    -COPY W0008       -PRE WDG3-.                                        
056000      05    FILLER           PIC X.                                       
056100                                                                          
056200                                                                          
056300*01  -COPY W0008       -PRE WDB6-                                         
056400      05    FILLER           PIC X.                                       
056800                                                                          
056900     EJECT                                                                
057000 PROCEDURE DIVISION  USING   MSG-PCB  ALT-PCB  USEA-PCB ARTC-PCB          
057102                  WDD8B-PCB WDE4C-PCB WDE4-PCB   INV-PCB                  
057200                   BEN-PCB  SATB-PCB ARTS-PCB  WDG3-PCB                   
057302                  WDB6-PCB.                                               
057400 MAIN SECTION.                                                            
057500     ENTRY 'DLITCBL' USING   MSG-PCB  ALT-PCB  USEA-PCB ARTC-PCB          
057602                  WDD8B-PCB WDE4C-PCB WDE4-PCB   INV-PCB                  
057700                   BEN-PCB  SATB-PCB ARTS-PCB  WDG3-PCB                   
057802                  WDB6-PCB.                                               
057900                                                                          
058000     PERFORM IMS-GET-MSG                                                  
058100     IF SEGMENT-FINNS                                                     
058200        PERFORM A-INIT-SPARA-INPUT                                        
058300        IF GODK-TRANS                                                     
058400           IF MFS-UPD-X                                                   
058500              PERFORM B-BEARBETNING                                       
058600           END-IF                                                         
058700        END-IF                                                            
058800     END-IF                                                               
058900                                                                          
059000     MOVE ZERO TO RETURN-CODE                                             
059100     GOBACK                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 A-INIT-SPARA-INPUT SECTION.                                              
059500                                                                          
059600     IF MSG-DUBBLA-TRANSKODER                                             
059700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I39201                 
059800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
059900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
060000       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
060100     ELSE                                                                 
060200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I39201                 
060300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
060400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
060500     END-IF                                                               
060600                                                                          
060700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
060800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
060900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
061000                                                                          
061100     MOVE LOW-VALUE                       TO MSG-AREA                     
061200     MOVE 'W5O39201'                      TO MFS-IDMOD                    
061300     IF SWEDISH-TEXT                                                      
061400       MOVE +1                            TO SPRAK-IX                     
061500     ELSE                                                                 
061600       MOVE +2                            TO SPRAK-IX                     
061700     END-IF                                                               
061800     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM                  
061900     .                                                                    
062000     EJECT                                                                
062100 B-BEARBETNING SECTION.                                                   
062200     MOVE 'BSECTION' TO WS-LAGE                                           
062300     PERFORM BAA-NOLLA-LISTAN                                             
062400     MOVE MID-IDDC                   TO WS-IDDC                           
062500                                        RUB-IDDC                          
062600                                        W-IDDC                            
062700                                        W-IDDC-G3                         
062800                                        W-IDDC-WDH1-MIN                   
062900                                        W-IDDC-WDH1-MAX                   
063000                                        SPAR-WS-IDDC                      
063100                                        W-IDDC-B6                         
063200     MOVE JA                         TO WDB6-A-SW                         
063300     PERFORM IMS-GU-WDB601                                                
063400     IF SEGMENT-SAKNAS                                                    
063500        MOVE NEJ                     TO WDB6-A-SW                         
063600     END-IF                                                               
063700                                                                          
063800     PERFORM BA-OEPPNA-PRINTER                                            
063900     PERFORM BB-HAMTA-DATUM-OCH-TID                                       
064000                                                                          
064100     MOVE +1 TO IX                                                        
064200     PERFORM UNTIL IX > 10 OR MID-IDARTNR-PRINT(IX) = ALL '+'             
064300       PERFORM BAA-NOLLA-LISTAN                                           
064400       MOVE MID-IDARTNR-PRINT(IX)    TO W-IDARTNR                         
064500                                        RAD-IDARTNR                       
064600                                        W-IDARTNR-LOW                     
064700                                        W-IDARTNR-HIGH                    
064803                                        W-IDARTNR-D8-MIN                  
064903                                        W-IDARTNR-D8-MAX                  
065000       MOVE MID-KDINVKAT-PRINT (IX)  TO W-KDINVKAT-WDH1-MIN               
065100                                        W-KDINVKAT-WDH1-MAX               
065200       PERFORM BC-HAEMTA-ANM-WDH1                                         
065300       IF MFS-UPD-X AND SEGMENT-SAKNAS                                    
065400         MOVE +99 TO IX                                                   
065500       ELSE                                                               
065600         EVALUATE TRUE                                                    
065700         WHEN  WDB6-A-FINNS                                               
065800         AND DCS-CDC                                                      
065900           PERFORM BE-HAEMTA-UPPG-WDK6-CDC                                
066000           PERFORM BH-HAEMTA-UPPG-WDD8-CDC                                
066100           PERFORM BI-HAEMTA-UPPG-WDD3-CDC                                
066200           PERFORM BJ-HAEMTA-CDC-WDE4-OVR-DISTR                           
066300           PERFORM BK-HAMTA-UPPG-WDE4-DISTR-98                            
066400           PERFORM BL-CDC-SKRIV-LISTA1                                    
066500           IF BUFFERT-PRINT OR CROSSDOCKING-PRINT                         
066600             PERFORM BM-SKRIV-EXTRA-BUFFRADER-CDC                         
066700           END-IF                                                         
066800         WHEN WDB6-A-FINNS                                                
066900         AND (DCS-SDC OR DCS-NDC-PF OR DCS-NDC-NA)                        
067000           PERFORM BN-HAEMTA-UPPG-WDK6-WDK7-SDC                           
067100           PERFORM BO-HAEMTA-UPPG-WDD8-SDC                                
067200           PERFORM BP-HAEMTA-UPPG-WDD3-SDC                                
067300           PERFORM BQ-HAEMTA-SDC-WDE4-OVR-DISTR                           
067400           PERFORM BR-SDC-SKRIV-LISTA1                                    
067500           IF BUFFERT-PRINT                                               
067600             PERFORM BS-SKRIV-EXTRA-BUFFRADER-SDC                         
067700           END-IF                                                         
067800         END-EVALUATE                                                     
067900                                                                          
068000         ADD +1 TO PRINT-ANT                                              
068100         ADD +1 TO IX                                                     
068200       END-IF                                                             
068300     END-PERFORM                                                          
068400     PERFORM BZ-STAENG-PRINTER                                            
068500     .                                                                    
068600     EJECT                                                                
068700 BA-OEPPNA-PRINTER SECTION.                                               
068800                                                                          
068900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
069000     MOVE '013'             TO MSGI-KDCALL                                
069100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
069200     MOVE '5301'            TO MSGI-IDTRANS                               
069300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
069400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
069500                                                                          
069600     MOVE 'BA-SECTION'      TO WS-LAGE                                    
069700*    IF WDB6-A-FINNS AND NOT DCS-NDC-NA                                   
069800     IF WDB6-A-FINNS                                                      
069900       IF WS-IDDC = KONS-WC-SDC-NL-ET                                     
070000         MOVE DCS-IDPRTLST-INVAB   TO WS-PRT1                             
070100       ELSE                                                               
070200         MOVE DCS-IDPRTLST-INVA    TO WS-PRT1                             
070300       END-IF                                                             
070400     ELSE                                                                 
070500       CALL FELLOG                                                        
070600     END-IF                                                               
070700     IF MID-IDPRTLST = SPACE                                              
070800        CONTINUE                                                          
070900     ELSE                                                                 
071000        MOVE MID-IDPRTLST TO WS-PRT1                                      
071100     END-IF                                                               
071200                                                                          
071300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN WS-PRT1 ALT-PCB           
071400                                   DUMMY-AREA DUMMY-AREA                  
071500     .                                                                    
071600     EJECT                                                                
071700 BAA-NOLLA-LISTAN SECTION.                                                
071800                                                                          
071900     MOVE ZERO TO   RAD-IDARTNR                                           
072000                    RAD-ADLAGOMR                                          
072100                    RAD-ADGANG                                            
072200                    RAD-ADPLATS                                           
072300                    RAD-BUFF-ADLAGOMR                                     
072400                    RAD-BUFF-ADGANG                                       
072500                    RAD-BUFF-ADPLATS                                      
072600                    RAD-BUFF-KVLS                                         
072700                    RAD-KVLS                                              
072800                    RAD-KVAKS                                             
072900                    RAD-KVEFRS                                            
073000                    RAD3-BUFF-ADLAGOMR                                    
073100                    RAD3-BUFF-ADGANG                                      
073200                    RAD3-BUFF-ADPLATS                                     
073300                    RAD3-BUFF-KVLS                                        
073400                    RAD3-ADLAGOMR-CD                                      
073500                    RAD3-ADGANG-CD                                        
073600                    RAD3-ADPLATS-CD                                       
073700                    RAD3-KVLS-CD                                          
073800     MOVE SPACE TO  RAD-BEART                                             
073900     MOVE ZERO  TO  WS-KVLEVART-TOT                                       
074000                    WS-KVEFRS-OLD                                         
074100                    WSATS-KVLEVART-TOT                                    
074200                    WS-QTY-CDC                                            
074300                    WS-QTY-SDC                                            
074400     .                                                                    
074500     EJECT                                                                
074600 BB-HAMTA-DATUM-OCH-TID SECTION.                                          
074700     MOVE 'BBSECTION' TO WS-LAGE                                          
074800     MOVE FUNCTION CURRENT-DATE(3:6)  TO RUB-DATUM                        
074900     MOVE FUNCTION CURRENT-DATE(9:4)  TO RUB-TID                          
075000**  BERÄKNA DATUM OCH TID FÖR LAB                                         
075100     IF NDC                                                               
075200       MOVE '011'                       TO MSGI-KDCALL                    
075300       MOVE 'WIDDCXX '                  TO MSGI-IDUSER                    
075400       MOVE WS-IDDC                     TO MSGI-IDUSER(6:2)               
075500       MOVE FUNCTION CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                  
075600       MOVE FUNCTION CURRENT-DATE(9:4)  TO MSGI-TILOKTID                  
075700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
075800       IF NDC-NA                                                          
075900         MOVE MSGI-TILOKDAT             TO RUB-DATUM                      
076000         MOVE MSGI-TILOKTID             TO RUB-TID                        
076100       END-IF                                                             
076200                                                                          
076300       IF NDC-PACIFIC                                                     
076400         MOVE MSGI-TILOKDAT             TO RUB-DATUM                      
076500         MOVE MSGI-TILOKTID             TO RUB-TID                        
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 BC-HAEMTA-ANM-WDH1 SECTION.                                              
077100     MOVE 'BC-SECTION' TO WS-LAGE                                         
077200     PERFORM IMS-GHU-WDH101                                               
077300     IF SEGMENT-FINNS                                                     
077400       PERFORM IMS-GHNP-WDH111                                            
077500       IF SEGMENT-FINNS                                                   
077600*        IF NOT NDC-NA                                                    
077700****  KJH START, FIXA TILL FÖRSTA ARTIKELNS PRINT-ID,                     
077800***       BLIR SAMMA PRINT-ID FÖR ALLA RADERNA                            
077900           IF IX = 1                                                      
078000*--- EN DUBBLETT                                                          
078100            IF INV-IDLOPNR > 0                                            
078200              MOVE INV-IDPRTOMG   TO WS-IDPRTINV-NUM(1:1)                 
078300                                     WS-IDPRTOMG                          
078400              MOVE INV-IDLOPNR    TO WS-IDLOPNR-5                         
078500                                     WS-IDLOPNR                           
078600                                     WS-IDPRTINV-NUM(2:5)                 
078700            ELSE                                                          
078800             IF INV-IDPRTOMG = +0 OR +1 OR +2                             
078900               EVALUATE INV-IDPRTOMG                                      
079000                 WHEN +0 MOVE 1    TO WS-IDPRTINV-NUM(1:1)                
079100                                      WS-IDPRTOMG                         
079200                 WHEN +1 MOVE 2    TO WS-IDPRTINV-NUM(1:1)                
079300                                      WS-IDPRTOMG                         
079400                 WHEN +2 MOVE 3    TO WS-IDPRTINV-NUM(1:1)                
079500                                      WS-IDPRTOMG                         
079600               END-EVALUATE                                               
079700****  HÄMTA NÄSTA INV-IDLOPNR FRÅN HÄNDELSEBASEN.                         
079800               PERFORM IMS-GHU-WDGX5102                                   
079900               IF SEGMENT-SAKNAS                                          
080000                 PERFORM IMS-GU-WDG301                                    
080100                 MOVE +1           TO 5102-KDSEGKEY                       
080200                 MOVE +1           TO 5102-IDLOPNR                        
080300                 MOVE 5102-IDLOPNR TO WS-IDPRTINV-NUM(2:5)                
080400                 PERFORM IMS-ISRT-WDGX5102                                
080500               ELSE                                                       
080600                 ADD +1 TO 5102-IDLOPNR                                   
080700                 MOVE 5102-IDLOPNR   TO WS-IDLOPNR-5                      
080800                                        WS-IDLOPNR                        
080900                 MOVE WS-IDLOPNR-5   TO WS-IDPRTINV-NUM(2:5)              
081000                 PERFORM IMS-REPL-WDGX5102                                
081100               END-IF                                                     
081200             ELSE                                                         
081300               MOVE ZERO             TO WS-IDPRTOMG                       
081400                                        WS-IDLOPNR                        
081500             END-IF                                                       
081600            END-IF                                                        
081700           END-IF                                                         
081800           MOVE WS-IDPRTINV-NUM    TO RUB-PRINTNR                         
081900*        END-IF                                                           
082000****  KJH SLUT                                                            
082100                                                                          
082200         MOVE INV-TISEGKEY         TO WS-TISEGKEY                         
082300*        MOVE WS-TISEGKEY(3:6)     TO RUB-TIREGDAT                        
082400         IF INV-FLINVSKR = 'N'                                            
082500           MOVE 'J'                TO INV-FLINVSKR                        
082600*          PERFORM IMS-REPLACE                                            
082700* VÄNTA MED REPLACEN TILLS LAGERSALDON ÄR HÄMTADE                         
082800* VÄNTA MED PERFORM IMS-REPLACE                                           
082900         END-IF                                                           
083000       END-IF                                                             
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 BE-HAEMTA-UPPG-WDK6-CDC SECTION.                                         
083500     MOVE 'BE-SECTION' TO WS-LAGE                                         
083600     PERFORM IMS-GET-ART-WDK6                                             
083700                                                                          
083800     MOVE +1 TO CD-IX                                                     
083900     PERFORM UNTIL CD-IX > CD-IX-MAX                                      
084000        MOVE ZERO TO TAB-ADLAGOMR-CD(CD-IX)                               
084100                     TAB-ADGANG-CD(CD-IX)                                 
084200                     TAB-ADPLATS-CD(CD-IX)                                
084300                     TAB-KVLS-CD(CD-IX)                                   
084400        ADD +1 TO CD-IX                                                   
084500     END-PERFORM                                                          
084600                                                                          
084700     IF ART-FLIART = JA                                                   
084800       MOVE JA                        TO WS-SATS                          
084900     ELSE                                                                 
085000       MOVE NEJ                       TO WS-SATS                          
085100     END-IF                                                               
085200     PERFORM IMS-GNP-CLAGERINFO                                           
085300     MOVE CLAG-KVAKS-CDC              TO RAD-KVAKS                        
085400     MOVE CLAG-KVLS                   TO RAD-KVLS                         
085500*    MOVE CLAG-KVEFRS                 TO RAD-KVEFRS                       
085600     MOVE CLAG-ADLAGOMR               TO RAD-ADLAGOMR                     
085700     MOVE CLAG-ADGANG                 TO RAD-ADGANG                       
085800     MOVE CLAG-ADPLATS                TO RAD-ADPLATS                      
085900                                                                          
086000     MOVE +1  TO CD-IX                                                    
086100     IF CLAG-ADLAGOMR-CD(1) = ZERO                                        
086200        MOVE NEJ TO CROSSD-SW                                             
086300     ELSE                                                                 
086400        MOVE JA TO CROSSD-SW                                              
086500        MOVE +1 TO K6-IX                                                  
086600        PERFORM UNTIL K6-IX >= CD-IX-MAX                                  
086700           MOVE CLAG-ADLAGOMR-CD(K6-IX) TO TAB-ADLAGOMR-CD(CD-IX)         
086800           MOVE CLAG-ADGANG-CD(K6-IX)   TO TAB-ADGANG-CD(CD-IX)           
086900           MOVE CLAG-ADPLATS-CD(K6-IX)  TO TAB-ADPLATS-CD(CD-IX)          
087000           MOVE CLAG-KVLS-CD(K6-IX)     TO TAB-KVLS-CD(CD-IX)             
087100           ADD +1 TO CD-IX K6-IX                                          
087200        END-PERFORM                                                       
087300     END-IF                                                               
087400                                                                          
087500     MOVE CLAG-KVAKS-CDC                   TO INV-KVAKS-OLD               
087600     MOVE CLAG-KVLS                        TO INV-KVLS-OLD                
087700     MOVE WS-IDLOPNR                       TO INV-IDLOPNR                 
087800     IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                    
087900       EVALUATE WS-IDPRTOMG                                               
088000         WHEN 1                                                           
088100*          MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR1              
088200           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1            
088300         WHEN 2                                                           
088400*          MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR2              
088500           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2            
088600         WHEN 3                                                           
088700*          MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR3              
088800           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3            
088900       END-EVALUATE                                                       
089000     END-IF                                                               
089100     MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG                
089200     .                                                                    
089300     EJECT                                                                
089400 BH-HAEMTA-UPPG-WDD8-CDC SECTION.                                         
089500     MOVE 'BH-SECTION' TO WS-LAGE                                         
089600     PERFORM BHA-NOLLSTAELL-CDC                                           
089700     MOVE NEJ                     TO BUFFERT-SW                           
089800                                                                          
089900     MOVE WS-IDDC                 TO W-IDDC-D8-MIN                        
090000                                     W-IDDC-D8-MAX                        
090102     PERFORM IMS-GU-WDD8B1                                                
090200     IF SEGMENT-FINNS                                                     
090300       MOVE +1 TO SALDO-IX                                                
090400       PERFORM UNTIL SEGMENT-SAKNAS OR SALDO-IX > 1                       
090500        IF SALDO-IX = 1                                                   
091000                                                                          
091102          MOVE SEQB-ADBUFFOMR  TO RAD-BUFF-ADLAGOMR                       
091202          MOVE SEQB-ADBUFFGANG TO RAD-BUFF-ADGANG                         
091302          MOVE SEQB-ADBUFFPL   TO RAD-BUFF-ADPLATS                        
091400                                                                          
091502          COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                            
091602                               SEQB-KVBUFF-OF                             
091700          MOVE WS-QTY-CDC             TO RAD-BUFF-KVLS                    
091800        END-IF                                                            
091900        ADD +1 TO SALDO-IX                                                
092002        PERFORM IMS-GN-WDD8B1                                             
092100       END-PERFORM                                                        
092200       IF SEGMENT-FINNS                                                   
092300          MOVE JA                TO BUFFERT-SW                            
092400       END-IF                                                             
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 BHA-NOLLSTAELL-CDC     SECTION.                                          
092900     MOVE 'BHA-SECTION' TO WS-LAGE                                        
093000     MOVE +0 TO RAD-BUFF-ADLAGOMR                                         
093100                RAD-BUFF-ADGANG                                           
093200                RAD-BUFF-ADPLATS                                          
093300                RAD3-BUFF-ADLAGOMR                                        
093400                RAD3-BUFF-ADGANG                                          
093500                RAD3-BUFF-ADPLATS                                         
093600                RAD3-BUFF-KVLS                                            
093700                RAD-BUFF-KVLS                                             
093800     .                                                                    
093900     EJECT                                                                
094000 BHB-NOLLSTAELL-CDC     SECTION.                                          
094100     MOVE 'BHA-SECTION' TO WS-LAGE                                        
094200     MOVE +0 TO RAD-BUFF-ADLAGOMR                                         
094300                RAD-BUFF-ADGANG                                           
094400                RAD-BUFF-ADPLATS                                          
094500                RAD3-BUFF-ADLAGOMR                                        
094600                RAD3-BUFF-ADGANG                                          
094700                RAD3-BUFF-ADPLATS                                         
094800                RAD3-BUFF-KVLS                                            
094900                RAD-BUFF-KVLS                                             
095000     .                                                                    
095100     EJECT                                                                
095200 BI-HAEMTA-UPPG-WDD3-CDC SECTION.                                         
095300     MOVE 'BI-SECTION' TO WS-LAGE                                         
095400     MOVE 'S  '                TO W-IDSKYLT                               
095500     PERFORM IMS-GU-BEN-SEQ                                               
095600     IF SEGMENT-FINNS                                                     
095700       MOVE BEN-TEXT-BEART     TO RAD-BEART                               
095800     ELSE                                                                 
095900       MOVE SPACE              TO RAD-BEART                               
096000     END-IF                                                               
096100     .                                                                    
096200     SKIP2                                                                
096300 BJ-HAEMTA-CDC-WDE4-OVR-DISTR SECTION.                                    
096400     MOVE 'BJ-SECTION' TO WS-LAGE                                         
096500                                                                          
096600     MOVE +0                                TO WS-ANTAL-RADER-EFR         
096700     MOVE +1 TO INDX                                                      
096800*9748228 START                                                            
096900*W-IDARTNR-*   SÄTTS I B-BEARBETNING SECTION.                             
097000                                                                          
097100     PERFORM IMS-GU-WDE4C1                                                
097200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
097300                                                                          
097400       MOVE SEQC-IDDISTR              TO W-IDDISTR-WDE4                   
097500       MOVE SEQC-IDKUNDNR             TO W-IDKUNDNR                       
097600       MOVE SEQC-IDKUNDRF             TO W-IDKUNDRF                       
097700       MOVE SEQC-IDPRODNR             TO W-IDPRODNR                       
097800       MOVE SEQC-IDPLKLST             TO W-IDPLKLST                       
097900       MOVE SEQC-IDPURAD              TO W-IDPURAD                        
098000                                                                          
098100       PERFORM IMS-GU-WDE4-ROT                                            
098200       IF KORD-IDDC = WS-IDDC                                             
098300         IF KORD-IDDISTR NOT = +98                                        
098400                                                                          
098500           PERFORM IMS-GNP-WDE411                                         
098600           IF ORAD-FLDIRLEV NOT = 'J'                                     
098700             COMPUTE WS-KVLEVART-TOT = WS-KVLEVART-TOT +                  
098800                     ORAD-KVAVBART - ORAD-KVLEVART END-COMPUTE            
098900             ADD +1 TO WS-ANTAL-RADER-EFR                                 
099000           END-IF                                                         
099100         END-IF                                                           
099200       END-IF                                                             
099300                                                                          
099400       PERFORM IMS-GN-WDE4C1                                              
099500     END-PERFORM                                                          
099600*9748228 END                                                              
099700     MOVE WS-KVLEVART-TOT TO WS-KVEFRS-OLD                                
099800     .                                                                    
099900     EJECT                                                                
100000 BK-HAMTA-UPPG-WDE4-DISTR-98 SECTION.                                     
100100****    SATSORDER-RADER                                                   
100200     MOVE 'BK-SECTION' TO WS-LAGE                                         
100300     MOVE +98                              TO W-IDDISTR                   
100400                                              W-IDDISTR-WDE4              
100500     PERFORM IMS-GU-WDE4C-DISTR-98                                        
100600     MOVE 'GU-WDE4-DIST98' TO WS-LAGE                                     
100700     MOVE +1 TO IND                                                       
100800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
100900       MOVE SEQC-IDKUNDNR             TO W-IDKUNDNR                       
101000       MOVE SEQC-IDKUNDRF             TO W-IDKUNDRF                       
101100       MOVE SEQC-IDPRODNR             TO W-IDPRODNR                       
101200       MOVE SEQC-IDPLKLST             TO W-IDPLKLST                       
101300       MOVE SEQC-IDPURAD              TO W-IDPURAD                        
101400       PERFORM IMS-GU-WDE4-ROT                                            
101500       MOVE KORD-IDDC                 TO SPAR-WS-IDDC                     
101600       PERFORM IMS-GNP-WDE411                                             
101700       MOVE 'GNP-KUND' TO WS-LAGE                                         
101800       IF ORAD-FLDIRLEV NOT = 'J'                                         
101900         IF SPAR-CDC                                                      
102000           COMPUTE WSATS-KVLEVART-TOT = WSATS-KVLEVART-TOT +              
102100                        ORAD-KVAVBART - ORAD-KVLEVART                     
102200           ADD +1 TO WS-ANTAL-RADER-EFR                                   
102300         END-IF                                                           
102400       END-IF                                                             
102500       PERFORM IMS-GN-WDE4C-DISTR-98                                      
102600       MOVE 'GN-WDE4-DISTX' TO WS-LAGE                                    
102700     END-PERFORM                                                          
102800*--- NU RAKNAR VI UT DEN TOTALA  EFR SOM FINNS MED STATUS N               
102900*    FÖR ATT SEDAN UPPDATERA WDH1 BASEN                                   
103000     COMPUTE WS-KVEFRS-OLD   = WS-KVEFRS-OLD   +                          
103100                               WSATS-KVLEVART-TOT                         
103200     IF WS-ANTAL-RADER-EFR > 1                                            
103300       COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
103400       COMPUTE RAD-KVEFRS = WS-KVEFRS-OLD / 2                             
103500     ELSE                                                                 
103600       MOVE WS-KVEFRS-OLD   TO INV-KVEFRS-OLD                             
103700       MOVE WS-KVEFRS-OLD   TO RAD-KVEFRS                                 
103800     END-IF                                                               
103900     MOVE FUNCTION CURRENT-DATE(1:8)  TO INV-DAREGDAT                     
104000     MOVE 'IMS-REPL' TO WS-LAGE                                           
104100**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
104200**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
104300     IF WS-IDPRTOMG = 1                                                   
104400       PERFORM IMS-GNP-WDH121                                             
104500       IF SEGMENT-FINNS                                                   
104600         IF INVL-KDSEGKEY = 0                                             
104700           MOVE INVL-IDUSER     TO WS-IDUSER                              
104800         END-IF                                                           
104900       END-IF                                                             
105000       PERFORM IMS-GHU-WDH101                                             
105100       MOVE DLI-IO-AREA-INVA11 TO SPAR-WDH111-AREA                        
105200       PERFORM IMS-GHNP-WDH111                                            
105300                                                                          
105400       PERFORM IMS-DELETE-WDH111                                          
105500       MOVE WS-DAGENS-DATUM TO SPAR-INV-DAREGDAT-SORT                     
105600                                                                          
105700       PERFORM IMS-GHU-WDH101                                             
105800       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
105900       PERFORM IMS-INSERT-WDH111                                          
106000                                                                          
106100       IF SEGMENT-EXISTS                                                  
106200         PERFORM UNTIL SEGMENT-FINNS                                      
106300           IF SEGMENT-EXISTS OR INDEX-EXISTS                              
106400             ADD +1 TO INV-TISEGKEY                                       
106500             PERFORM IMS-INSERT-WDH111                                    
106600           END-IF                                                         
106700         END-PERFORM                                                      
106800       END-IF                                                             
106900                                                                          
107000       MOVE '0'            TO INVL-KDSEGKEY                               
107100       MOVE WS-IDUSER      TO INVL-IDUSER                                 
107200       PERFORM IMS-INSERT-WDH121                                          
107300       MOVE '1'            TO INVL-KDSEGKEY                               
107400       MOVE MSG-SIGNON-USERID TO INVL-IDUSER                              
107500       PERFORM IMS-INSERT-WDH121                                          
107600       MOVE '2'            TO INVL-KDSEGKEY                               
107700       MOVE SPACE          TO INVL-IDUSER                                 
107800       PERFORM IMS-INSERT-WDH121                                          
107900       MOVE '3'            TO INVL-KDSEGKEY                               
108000       MOVE SPACE          TO INVL-IDUSER                                 
108100       PERFORM IMS-INSERT-WDH121                                          
108200     ELSE                                                                 
108300       PERFORM IMS-REPLACE                                                
108400******* REPLACE PÅ WDH121 ****                                            
108500       MOVE WS-IDPRTOMG TO W-IDPRTOMG-ALFA                                
108600       PERFORM IMS-GNP-WDH121                                             
108700       MOVE 'GNP  PÅ WDH121 EK' TO WS-LAGE                                
108800       IF SEGMENT-FINNS                                                   
108900         PERFORM UNTIL SEGMENT-SAKNAS                                     
109000           IF W-IDPRTOMG-ALFA  = INVL-KDSEGKEY                            
109100             MOVE MSG-SIGNON-USERID  TO INVL-IDUSER                       
109200             PERFORM IMS-REPLACE-WDH121                                   
109300           END-IF                                                         
109400           PERFORM IMS-GNP-WDH121                                         
109500         END-PERFORM                                                      
109600       END-IF                                                             
109700     END-IF                                                               
109800                                                                          
109900     MOVE 'EFTER REPL' TO WS-LAGE                                         
110000*---                                                                      
110100     .                                                                    
110200     EJECT                                                                
110300 BL-CDC-SKRIV-LISTA1 SECTION.                                             
110400     MOVE 'BL-SECTION' TO WS-LAGE                                         
110500**********************  SKRIVER LISTAN   *******************              
110600                                                                          
110700* SKRIVER RUBRIKER                                                        
110800     IF WS-RAD-RAEKNARE  > 41                                             
110900       MOVE NEJ                    TO RUBRIK-SW                           
111000       MOVE +0                     TO WS-RAD-RAEKNARE                     
111100     END-IF                                                               
111200     IF RUBRIK-INTE-UTSKRIVEN                                             
111300       ADD +1                      TO W-ANT-SIDOR                         
111400       MOVE W-ANT-SIDOR            TO ANTAL-SIDOR                         
111500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
111600                           PRT-NYSIDA-RAD2 RUBRAD-LISTA                   
111700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
111800                           PRT-AFTER-2   RAD-1-LISTA                      
111900       MOVE JA TO RUBRIK-SW                                               
112000       ADD +6                      TO WS-RAD-RAEKNARE                     
112100     END-IF                                                               
112200* SKRIVER RADEN                                                           
112300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
112400                         PRT-AFTER-2 RAD-2-LISTA                          
112500**   CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
112600**                       PRT-AFTER-1 RAD-2X-LISTA                         
112700     ADD +3                        TO WS-RAD-RAEKNARE                     
112800     .                                                                    
112900     EJECT                                                                
113000 BLX-CDC-SKRIV-LISTA1 SECTION.                                            
113100     MOVE 'BLX-SECTION' TO WS-LAGE                                        
113200**********************  SKRIVER LISTAN   *******************              
113300                                                                          
113400* SKRIVER RUBRIKER                                                        
113500     IF WS-RAD-RAEKNARE  > 41                                             
113600       MOVE NEJ                    TO RUBRIK-SW                           
113700       MOVE +0                     TO WS-RAD-RAEKNARE                     
113800     END-IF                                                               
113900     IF RUBRIK-INTE-UTSKRIVEN                                             
114000       ADD +1                      TO W-ANT-SIDOR                         
114100       MOVE W-ANT-SIDOR            TO ANTAL-SIDOR                         
114200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
114300                           PRT-NYSIDA-RAD2 RUBRAD-LISTA                   
114400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
114500                           PRT-AFTER-2   RAD-1-LISTA                      
114600       MOVE JA TO RUBRIK-SW                                               
114700       ADD +6                      TO WS-RAD-RAEKNARE                     
114800     END-IF                                                               
114900* SKRIVER RADEN                                                           
115000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
115100                         PRT-AFTER-2 RAD-3-LISTA                          
115200     ADD +2                        TO WS-RAD-RAEKNARE                     
115300     .                                                                    
115400     EJECT                                                                
115500 BM-SKRIV-EXTRA-BUFFRADER-CDC SECTION.                                    
115600     MOVE 'BM-SECTION' TO WS-LAGE                                         
115700     MOVE +1 TO CD-IX                                                     
115800     MOVE +1                      TO SALDO-IX                             
115900     IF BUFFERT-PRINT                                                     
116000        MOVE SPACE                    TO STATUS-WS                        
116100        PERFORM BHB-NOLLSTAELL-CDC                                        
116200                                                                          
116300        PERFORM UNTIL SEGMENT-SAKNAS                                      
117000                                                                          
117102          MOVE SEQB-ADBUFFOMR  TO RAD3-BUFF-ADLAGOMR                      
117202          MOVE SEQB-ADBUFFGANG TO RAD3-BUFF-ADGANG                        
117302          MOVE SEQB-ADBUFFPL   TO RAD3-BUFF-ADPLATS                       
117402          COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                            
117502                               SEQB-KVBUFF-OF                             
117600          MOVE WS-QTY-CDC             TO RAD3-BUFF-KVLS                   
117700                                                                          
117800          IF CROSSDOCKING-PRINT                                           
117900             IF CD-IX <= CD-IX-MAX                                        
118000                IF TAB-ADLAGOMR-CD(CD-IX) = ZERO                          
118100                   MOVE ZERO TO RAD3-ADLAGOMR-CD                          
118200                                RAD3-ADGANG-CD                            
118300                                RAD3-ADPLATS-CD                           
118400                                RAD3-KVLS-CD                              
118500                ELSE                                                      
118600                   MOVE TAB-ADLAGOMR-CD(CD-IX) TO RAD3-ADLAGOMR-CD        
118702                   MOVE TAB-ADGANG-CD(CD-IX)   TO RAD3-ADGANG-CD          
118802                   MOVE TAB-ADPLATS-CD(CD-IX)  TO RAD3-ADPLATS-CD         
118902                   MOVE TAB-KVLS-CD(CD-IX)     TO RAD3-KVLS-CD            
119000                END-IF                                                    
119100                ADD +1 TO CD-IX                                           
119200             ELSE                                                         
119300                MOVE ZERO TO RAD3-ADLAGOMR-CD                             
119400                             RAD3-ADGANG-CD                               
119500                             RAD3-ADPLATS-CD                              
119600                             RAD3-KVLS-CD                                 
119700             END-IF                                                       
119800          END-IF                                                          
119900                                                                          
120000          IF WS-RAD-RAEKNARE  > 41                                        
120100            MOVE NEJ                    TO RUBRIK-SW                      
120200            MOVE +0                     TO WS-RAD-RAEKNARE                
120300            MOVE RAD3-BUFF-ADLAGOMR     TO RAD-BUFF-ADLAGOMR              
120400            MOVE RAD3-BUFF-ADGANG       TO RAD-BUFF-ADGANG                
120500            MOVE RAD3-BUFF-ADPLATS      TO RAD-BUFF-ADPLATS               
120600            MOVE RAD3-BUFF-KVLS         TO RAD-BUFF-KVLS                  
120700            IF CROSSDOCKING-PRINT                                         
120800               IF RAD3-ADLAGOMR-CD = ZERO                                 
120900                  CONTINUE                                                
121000               ELSE                                                       
121100                  MOVE RAD3-ADLAGOMR-CD    TO RAD-ADLAGOMR                
121200                  MOVE RAD3-ADGANG-CD      TO RAD-ADGANG                  
121300                  MOVE RAD3-ADPLATS-CD     TO RAD-ADPLATS                 
121400                  MOVE RAD3-KVLS-CD        TO RAD-KVLS                    
121500               END-IF                                                     
121600            END-IF                                                        
121700            PERFORM BLX-CDC-SKRIV-LISTA1                                  
121800          ELSE                                                            
121900            CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1           
122000                                ALT-PCB PRT-AFTER-2 RAD-3-LISTA           
122100            ADD +2                      TO WS-RAD-RAEKNARE                
122200          END-IF                                                          
122300          MOVE +1                     TO SALDO-IX                         
122402          PERFORM IMS-GN-WDD8B1                                           
122500        END-PERFORM                                                       
122600                                                                          
122700        IF CROSSDOCKING-PRINT                                             
122800           IF CD-IX <= CD-IX-MAX                                          
122900              PERFORM UNTIL CD-IX > CD-IX-MAX                             
123000                IF TAB-ADLAGOMR-CD(CD-IX) = ZERO                          
123100                   CONTINUE                                               
123200                ELSE                                                      
123300                   MOVE ZERO TO RAD3-BUFF-ADLAGOMR                        
123400                                RAD3-BUFF-ADGANG                          
123500                                RAD3-BUFF-ADPLATS                         
123600                                RAD3-BUFF-KVLS                            
123700                   MOVE TAB-ADLAGOMR-CD(CD-IX) TO RAD3-ADLAGOMR-CD        
123800                   MOVE TAB-ADGANG-CD(CD-IX) TO RAD3-ADGANG-CD            
123900                   MOVE TAB-ADPLATS-CD(CD-IX) TO RAD3-ADPLATS-CD          
124000                   MOVE TAB-KVLS-CD(CD-IX) TO RAD3-KVLS-CD                
124100                                                                          
124200                   IF WS-RAD-RAEKNARE  > 41                               
124300                      MOVE NEJ              TO RUBRIK-SW                  
124400                      MOVE +0               TO WS-RAD-RAEKNARE            
124500                      MOVE RAD3-ADLAGOMR-CD TO RAD-ADLAGOMR               
124600                      MOVE RAD3-ADGANG-CD   TO RAD-ADGANG                 
124700                      MOVE RAD3-ADPLATS-CD  TO RAD-ADPLATS                
124800                      MOVE RAD3-KVLS-CD     TO RAD-KVLS                   
124900                      PERFORM BLX-CDC-SKRIV-LISTA1                        
125000                   ELSE                                                   
125100                      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE         
125200                          WS-PRT1 ALT-PCB PRT-AFTER-2 RAD-3-LISTA         
125300                      ADD +2 TO WS-RAD-RAEKNARE                           
125400                   END-IF                                                 
125500                END-IF                                                    
125600                ADD +1 TO CD-IX                                           
125700             END-PERFORM                                                  
125800           END-IF                                                         
125900       END-IF                                                             
126000     ELSE                                                                 
126100        IF CROSSDOCKING-PRINT                                             
126200           IF CD-IX <= CD-IX-MAX                                          
126300              PERFORM UNTIL CD-IX > CD-IX-MAX                             
126400                 IF TAB-ADLAGOMR-CD(CD-IX) = ZERO                         
126500                    CONTINUE                                              
126600                 ELSE                                                     
126700                    MOVE ZERO TO RAD3-BUFF-ADLAGOMR                       
126800                                 RAD3-BUFF-ADGANG                         
126900                                 RAD3-BUFF-ADPLATS                        
127000                                 RAD3-BUFF-KVLS                           
127100                   MOVE TAB-ADLAGOMR-CD(CD-IX) TO RAD3-ADLAGOMR-CD        
127200                    MOVE TAB-ADGANG-CD(CD-IX) TO RAD3-ADGANG-CD           
127300                    MOVE TAB-ADPLATS-CD(CD-IX) TO RAD3-ADPLATS-CD         
127400                    MOVE TAB-KVLS-CD(CD-IX) TO RAD3-KVLS-CD               
127500                                                                          
127600                    IF WS-RAD-RAEKNARE  > 41                              
127700                       MOVE NEJ               TO RUBRIK-SW                
127800                       MOVE +0                TO WS-RAD-RAEKNARE          
127900                       MOVE RAD3-ADLAGOMR-CD  TO RAD-ADLAGOMR             
128000                       MOVE RAD3-ADGANG-CD    TO RAD-ADGANG               
128100                       MOVE RAD3-ADPLATS-CD   TO RAD-ADPLATS              
128200                       MOVE RAD3-KVLS-CD      TO RAD-KVLS                 
128300                       PERFORM BLX-CDC-SKRIV-LISTA1                       
128400                    ELSE                                                  
128500                     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE          
128600                        WS-PRT1 ALT-PCB PRT-AFTER-2 RAD-3-LISTA           
128700                     ADD +2 TO WS-RAD-RAEKNARE                            
128800                    END-IF                                                
128900                 END-IF                                                   
129000                 ADD +1 TO CD-IX                                          
129100              END-PERFORM                                                 
129200           END-IF                                                         
129300        END-IF                                                            
129400     END-IF                                                               
129500     .                                                                    
129600     EJECT                                                                
129700 BN-HAEMTA-UPPG-WDK6-WDK7-SDC SECTION.                                    
129800     MOVE 'BN-SECTION' TO WS-LAGE                                         
129900                                                                          
130000     PERFORM IMS-GET-ART-WDK7                                             
130100     IF SEGMENT-FINNS                                                     
130200       MOVE SLAG-KVLS                        TO RAD-KVLS                  
130300       MOVE SLAG-KVAKS-SDC                   TO RAD-KVAKS                 
130400       MOVE SLAG-ADLAGOMR                    TO RAD-ADLAGOMR              
130500       MOVE SLAG-ADGANG                      TO RAD-ADGANG                
130600       MOVE SLAG-ADPLATS                     TO RAD-ADPLATS               
130700       MOVE SLAG-KVLS                        TO INV-KVLS-OLD              
130800       MOVE SLAG-KVAKS-SDC                   TO INV-KVAKS-OLD             
130900       MOVE WS-IDLOPNR                       TO INV-IDLOPNR               
131000       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
131100         EVALUATE WS-IDPRTOMG                                             
131200           WHEN 1                                                         
131300*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR1            
131400             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
131500           WHEN 2                                                         
131600*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR2            
131700             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
131800           WHEN 3                                                         
131900*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR3            
132000             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
132100         END-EVALUATE                                                     
132200       END-IF                                                             
132300       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
132400     END-IF                                                               
132500     .                                                                    
132600     EJECT                                                                
132700 BO-HAEMTA-UPPG-WDD8-SDC SECTION.                                         
132800     MOVE 'BO-SECTION' TO WS-LAGE                                         
132900     PERFORM BOB-NOLLSTAELL-SDC                                           
133000     MOVE NEJ                   TO BUFFERT-SW                             
133100     MOVE WS-IDDC               TO W-IDDC-D8-MIN                          
133200                                   W-IDDC-D8-MAX                          
133302     PERFORM IMS-GU-WDD8B1                                                
133400     IF SEGMENT-FINNS                                                     
133500       MOVE +1 TO SALDO-IX                                                
133600       PERFORM UNTIL SEGMENT-SAKNAS OR SALDO-IX > +1                      
133700          IF SALDO-IX = 1                                                 
134400                                                                          
134502             MOVE SEQB-ADBUFFOMR  TO RAD-BUFF-ADLAGOMR                    
134602             MOVE SEQB-ADBUFFGANG TO RAD-BUFF-ADGANG                      
134702             MOVE SEQB-ADBUFFPL   TO RAD-BUFF-ADPLATS                     
134800                                                                          
134902             COMPUTE WS-QTY-SDC = SEQB-KVBUFF-F +                         
135002                                  SEQB-KVBUFF-OF                          
135100             MOVE WS-QTY-SDC             TO RAD-BUFF-KVLS                 
135200                                                                          
135300          END-IF                                                          
135400          ADD +1 TO SALDO-IX                                              
135502          PERFORM IMS-GN-WDD8B1                                           
135600       END-PERFORM                                                        
135700       IF SEGMENT-FINNS                                                   
135800          MOVE JA TO BUFFERT-SW                                           
135900       END-IF                                                             
136000     END-IF                                                               
136100     .                                                                    
136200     EJECT                                                                
136300 BOB-NOLLSTAELL-SDC     SECTION.                                          
136400     MOVE 'BOB-SECTION' TO WS-LAGE                                        
136500     MOVE +0 TO RAD-BUFF-ADLAGOMR                                         
136600                RAD-BUFF-ADGANG                                           
136700                RAD-BUFF-ADPLATS                                          
136800                RAD-BUFF-KVLS                                             
136900     .                                                                    
137000     EJECT                                                                
137100 BOC-NOLLSTAELL-SDC     SECTION.                                          
137200     MOVE 'BOB-SECTION' TO WS-LAGE                                        
137300     MOVE +0 TO RAD3-BUFF-ADLAGOMR                                        
137400                RAD3-BUFF-ADGANG                                          
137500                RAD3-BUFF-ADPLATS                                         
137600                RAD3-BUFF-KVLS                                            
137700     .                                                                    
137800     EJECT                                                                
137900 BP-HAEMTA-UPPG-WDD3-SDC SECTION.                                         
138000     MOVE 'BP-SECTION' TO WS-LAGE                                         
138100     MOVE 'GB '                TO W-IDSKYLT                               
138200     PERFORM IMS-GU-BEN-SEQ                                               
138300     IF SEGMENT-FINNS                                                     
138400       MOVE BEN-TEXT-BEART     TO RAD-BEART                               
138500     ELSE                                                                 
138600       MOVE SPACE              TO RAD-BEART                               
138700     END-IF                                                               
138800     .                                                                    
138900     SKIP2                                                                
139000 BQ-HAEMTA-SDC-WDE4-OVR-DISTR SECTION.                                    
139100     MOVE 'BQ-SECTION' TO WS-LAGE                                         
139200                                                                          
139300     MOVE +1 TO INDX                                                      
139400     MOVE +0 TO WS-ANTAL-RADER-EFR                                        
139500     MOVE +0 TO W-SDC-ANTAL                                               
139600*9748228 START                                                            
139700*W-IDARTNR-*   SÄTTS I B-BEARBETNING SECTION.                             
139800                                                                          
139900     PERFORM IMS-GU-WDE4C1                                                
140000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
140100                OR INDX > MAX-IX-7                                        
140200                                                                          
140300       MOVE SEQC-IDDISTR              TO W-IDDISTR-WDE4                   
140400       MOVE SEQC-IDKUNDNR             TO W-IDKUNDNR                       
140500       MOVE SEQC-IDKUNDRF             TO W-IDKUNDRF                       
140600       MOVE SEQC-IDPRODNR             TO W-IDPRODNR                       
140700       MOVE SEQC-IDPLKLST             TO W-IDPLKLST                       
140800       MOVE SEQC-IDPURAD              TO W-IDPURAD                        
140900       PERFORM IMS-GU-WDE4-ROT                                            
141000       IF KORD-IDDC = WS-IDDC                                             
141100         IF KORD-IDDISTR NOT = +98                                        
141200                                                                          
141300           PERFORM IMS-GNP-WDE411                                         
141400           IF (ORAD-FLDIRLEV NOT = 'J')                                   
141500             COMPUTE W-SDC-ANTAL = W-SDC-ANTAL +                          
141600               (ORAD-KVAVBART - ORAD-KVLEVART) END-COMPUTE                
141700             ADD +1 TO INDX                                               
141800             ADD +1 TO WS-ANTAL-RADER-EFR                                 
141900           END-IF                                                         
142000         END-IF                                                           
142100       END-IF                                                             
142200       PERFORM IMS-GN-WDE4C1                                              
142300     END-PERFORM                                                          
142400*9748228 END                                                              
142500                                                                          
142600* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
142700* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
142800     IF WS-ANTAL-RADER-EFR < 2                                            
142900       MOVE W-SDC-ANTAL                    TO INV-KVEFRS-OLD              
143000       MOVE W-SDC-ANTAL                    TO RAD-KVEFRS                  
143100     ELSE                                                                 
143200       COMPUTE INV-KVEFRS-OLD = W-SDC-ANTAL / 2                           
143300       COMPUTE  RAD-KVEFRS    = W-SDC-ANTAL / 2                           
143400     END-IF                                                               
143500                                                                          
143600     MOVE FUNCTION CURRENT-DATE(1:8)  TO INV-DAREGDAT                     
143700**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
143800**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
143900     IF WS-IDPRTOMG = 1                                                   
144000       PERFORM IMS-GNP-WDH121                                             
144100       IF SEGMENT-FINNS                                                   
144200         IF INVL-KDSEGKEY = 0                                             
144300           MOVE INVL-IDUSER     TO WS-IDUSER                              
144400         END-IF                                                           
144500       END-IF                                                             
144600       PERFORM IMS-GHU-WDH101                                             
144700       MOVE DLI-IO-AREA-INVA11 TO SPAR-WDH111-AREA                        
144800       PERFORM IMS-GHNP-WDH111                                            
144900                                                                          
145000       PERFORM IMS-DELETE-WDH111                                          
145100       MOVE WS-DAGENS-DATUM TO SPAR-INV-DAREGDAT-SORT                     
145200                                                                          
145300       PERFORM IMS-GHU-WDH101                                             
145400       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
145500       PERFORM IMS-INSERT-WDH111                                          
145600                                                                          
145700       IF SEGMENT-EXISTS                                                  
145800         PERFORM UNTIL SEGMENT-FINNS                                      
145900           IF SEGMENT-EXISTS OR INDEX-EXISTS                              
146000             ADD +1 TO INV-TISEGKEY                                       
146100             PERFORM IMS-INSERT-WDH111                                    
146200           END-IF                                                         
146300         END-PERFORM                                                      
146400       END-IF                                                             
146500                                                                          
146600       MOVE '0'            TO INVL-KDSEGKEY                               
146700       MOVE WS-IDUSER      TO INVL-IDUSER                                 
146800       PERFORM IMS-INSERT-WDH121                                          
146900       MOVE '1'            TO INVL-KDSEGKEY                               
147000       MOVE  MSG-SIGNON-USERID TO INVL-IDUSER                             
147100       PERFORM IMS-INSERT-WDH121                                          
147200       MOVE '2'            TO INVL-KDSEGKEY                               
147300       MOVE SPACE          TO INVL-IDUSER                                 
147400       PERFORM IMS-INSERT-WDH121                                          
147500       MOVE '3'            TO INVL-KDSEGKEY                               
147600       MOVE SPACE          TO INVL-IDUSER                                 
147700       PERFORM IMS-INSERT-WDH121                                          
147800     ELSE                                                                 
147900                                                                          
148000       PERFORM IMS-REPLACE                                                
148100       MOVE ZERO TO W-SDC-ANTAL                                           
148200******* REPLACE PÅ WDH121 ****                                            
148300       MOVE WS-IDPRTOMG TO W-IDPRTOMG-ALFA                                
148400       PERFORM IMS-GNP-WDH121                                             
148500       MOVE 'GNP  PÅ WDH121 EK' TO WS-LAGE                                
148600       IF SEGMENT-FINNS                                                   
148700         PERFORM UNTIL SEGMENT-SAKNAS                                     
148800           IF W-IDPRTOMG-ALFA  = INVL-KDSEGKEY                            
148900             MOVE MSG-SIGNON-USERID  TO INVL-IDUSER                       
149000             PERFORM IMS-REPLACE-WDH121                                   
149100           END-IF                                                         
149200           PERFORM IMS-GNP-WDH121                                         
149300         END-PERFORM                                                      
149400       END-IF                                                             
149500     END-IF                                                               
149600     .                                                                    
149700     EJECT                                                                
149800 BR-SDC-SKRIV-LISTA1 SECTION.                                             
149900     MOVE 'BR-SECTION' TO WS-LAGE                                         
150000**********************  SKRIVER SDC-LISTAN   *******************          
150100* SKRIVER RUBRIK                                                          
150200     IF WS-RAD-RAEKNARE  > 41                                             
150300       MOVE NEJ                    TO RUBRIK-SW                           
150400       MOVE +0                     TO WS-RAD-RAEKNARE                     
150500     END-IF                                                               
150600     IF RUBRIK-INTE-UTSKRIVEN                                             
150700       ADD +1                      TO W-ANT-SIDOR                         
150800       MOVE W-ANT-SIDOR            TO ANTAL-SIDOR                         
150900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
151000                           PRT-NYSIDA-RAD2 RUBRAD-LISTA                   
151100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
151200                           PRT-AFTER-2     RAD-1-LISTA                    
151300       MOVE JA TO RUBRIK-SW                                               
151400       ADD +6                       TO WS-RAD-RAEKNARE                    
151500     END-IF                                                               
151600* SKRIVER RADER                                                           
151700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
151800                         PRT-AFTER-2 RAD-2-LISTA                          
151900     ADD +2                         TO WS-RAD-RAEKNARE                    
152000     .                                                                    
152100     EJECT                                                                
152200 BS-SKRIV-EXTRA-BUFFRADER-SDC SECTION.                                    
152300     MOVE 'BS-SECTION' TO WS-LAGE                                         
152400     MOVE ZERO TO RAD3-ADLAGOMR-CD                                        
152500                  RAD3-ADGANG-CD                                          
152600                  RAD3-ADPLATS-CD                                         
152700                  RAD3-KVLS-CD                                            
152800                                                                          
152900     MOVE +1 TO SALDO-IX                                                  
153000     MOVE SPACE TO STATUS-WS                                              
153100     PERFORM BOC-NOLLSTAELL-SDC                                           
153200     PERFORM UNTIL SEGMENT-SAKNAS                                         
153900                                                                          
154002      MOVE SEQB-ADBUFFOMR  TO RAD3-BUFF-ADLAGOMR                          
154102      MOVE SEQB-ADBUFFGANG TO RAD3-BUFF-ADGANG                            
154202      MOVE SEQB-ADBUFFPL   TO RAD3-BUFF-ADPLATS                           
154300                                                                          
154402      COMPUTE WS-QTY-SDC = SEQB-KVBUFF-F +                                
154502                           SEQB-KVBUFF-OF                                 
154600      MOVE WS-QTY-SDC             TO RAD3-BUFF-KVLS                       
154700                                                                          
154800      IF WS-RAD-RAEKNARE > 41                                             
154900        MOVE NEJ                  TO RUBRIK-SW                            
155000        MOVE +0                   TO WS-RAD-RAEKNARE                      
155100        MOVE RAD3-BUFF-ADLAGOMR   TO RAD-BUFF-ADLAGOMR                    
155200        MOVE RAD3-BUFF-ADGANG     TO RAD-BUFF-ADGANG                      
155300        MOVE RAD3-BUFF-ADPLATS    TO RAD-BUFF-ADPLATS                     
155400        MOVE RAD3-BUFF-KVLS       TO RAD-BUFF-KVLS                        
155500        PERFORM BR-SDC-SKRIV-LISTA1                                       
155600      ELSE                                                                
155700        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1               
155800                          ALT-PCB PRT-AFTER-2 RAD-3-LISTA                 
155900        ADD +2                    TO WS-RAD-RAEKNARE                      
156000      END-IF                                                              
156100      ADD  +1 TO SALDO-IX                                                 
156202      PERFORM IMS-GN-WDD8B1                                               
156300     END-PERFORM                                                          
156400     .                                                                    
156500     SKIP2                                                                
156600 BZ-STAENG-PRINTER SECTION.                                               
156700     MOVE 'BZ-SECTION' TO WS-LAGE                                         
156800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE WS-PRT1 ALT-PCB          
156900                                          DUMMY-AREA DUMMY-AREA           
157000     .                                                                    
157100     EJECT                                                                
157200* IMS SEKTIONER                                                           
157300     SKIP3                                                                
157400 IMS-GET-MSG SECTION.                                                     
157500     MOVE '  QC' TO GODK-STATUSKODER                                      
157600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
157700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     EJECT                                                                
158100******* OPERATIONER MOT WDK6                                              
158200                                                                          
158300 IMS-GET-ART-WDK6 SECTION.                                                
158400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
158500            DELIMITED BY SIZE INTO SSA1                                   
158600     MOVE '  ' TO GODK-STATUSKODER                                        
158700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
158800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
158900     PERFORM IMS-STATUSKONTROLL                                           
159000     .                                                                    
159100     SKIP3                                                                
159200 IMS-GNP-CLAGERINFO SECTION.                                              
159300     MOVE 'WLARTC11 ' TO SSA1                                             
159400     MOVE '  ' TO GODK-STATUSKODER                                        
159500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
159600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
159700     PERFORM IMS-STATUSKONTROLL                                           
159800     .                                                                    
159900     EJECT                                                                
160000******* OPERATIONER MOT WDK7                                              
160100     SKIP2                                                                
160200 IMS-GET-ART-WDK7 SECTION.                                                
160300     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
160400            DELIMITED BY SIZE INTO SSA1                                   
160500     STRING 'WLARTS11(IDDC     =' W-IDDC ')'                              
160600            DELIMITED BY SIZE INTO SSA2                                   
160700     MOVE '  GE' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA4 SSA1 SSA2                
160900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     SKIP3                                                                
161300******* OPERATIONER MOT WDD8                                              
161400                                                                          
162602 IMS-GU-WDD8B1 SECTION.                                                   
162702     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
162802                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
163000            DELIMITED BY SIZE INTO SSA1                                   
163100     MOVE '  GE'   TO GODK-STATUSKODER                                    
163202     CALL CBLTDLI USING GU WDD8B-PCB DLI-IO-WDD8B1 SSA1                   
163302     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600     EJECT                                                                
163702 IMS-GN-WDD8B1 SECTION.                                                   
163802     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
163902                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
164100            DELIMITED BY SIZE INTO SSA1                                   
164200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
164302     CALL CBLTDLI USING GN WDD8B-PCB DLI-IO-WDD8B1 SSA1                   
164402     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700     EJECT                                                                
164800******* OPERATIONER MOT WDH1                                              
164900     SKIP2                                                                
165000 IMS-GHU-WDH101 SECTION.                                                  
165100     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
165200             DELIMITED BY SIZE INTO SSA1                                  
165300     MOVE '  GE' TO GODK-STATUSKODER                                      
165400     CALL CBLTDLI USING GU INV-PCB DLI-IO-AREA-INVA01 SSA1                
165500     MOVE INV-STATUS-CODE TO STATUS-WS                                    
165600     PERFORM IMS-STATUSKONTROLL                                           
165700     .                                                                    
165800     SKIP2                                                                
165900 IMS-GHNP-WDH111 SECTION.                                                 
166000     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                        
166100                    '&WDH111KY<=' W-WDH111KY-MAX-X                        
166200                    '&FLINVBEH =' NEJ ')'                                 
166300             DELIMITED BY SIZE INTO SSA1                                  
166400     MOVE '  GE' TO GODK-STATUSKODER                                      
166500     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA11 SSA1              
166600     MOVE INV-STATUS-CODE TO STATUS-WS                                    
166700     PERFORM IMS-STATUSKONTROLL                                           
166800     .                                                                    
166900     SKIP2                                                                
167000 IMS-DELETE-WDH111 SECTION.                                               
167100     MOVE 'WDH111 ' TO SSA1                                               
167200     MOVE '  ' TO GODK-STATUSKODER                                        
167300     CALL CBLTDLI USING DLET INV-PCB DLI-IO-AREA-INVA11                   
167400     MOVE INV-STATUS-CODE TO STATUS-WS                                    
167500     PERFORM IMS-STATUSKONTROLL                                           
167600     .                                                                    
167700     SKIP2                                                                
167800 IMS-INSERT-WDH111 SECTION.                                               
167900                                                                          
168000*    STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
168100*         DELIMITED BY SIZE INTO SSA1                                     
168200     MOVE 'WDH111 ' TO SSA1                                               
168300     MOVE '  IINI' TO GODK-STATUSKODER                                    
168400     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA11 SSA1              
168500     MOVE INV-STATUS-CODE TO STATUS-WS                                    
168600     PERFORM IMS-STATUSKONTROLL                                           
168700     .                                                                    
168800     SKIP3                                                                
168900 IMS-GNP-WDH121 SECTION.                                                  
169000*    MOVE 'IMS-GNP-WDH121        ' TO WS-IMS                              
169100     MOVE 'WDH121 ' TO SSA1                                               
169200     MOVE '  GE' TO GODK-STATUSKODER                                      
169300     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA21 SSA1              
169400     MOVE INV-STATUS-CODE TO STATUS-WS                                    
169500     PERFORM IMS-STATUSKONTROLL                                           
169600     .                                                                    
169700     SKIP2                                                                
169800 IMS-REPLACE SECTION.                                                     
169900     MOVE '  ' TO GODK-STATUSKODER                                        
170000     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA11                   
170100     MOVE INV-STATUS-CODE TO STATUS-WS                                    
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     .                                                                    
170400     EJECT                                                                
170500 IMS-REPLACE-WDH121 SECTION.                                              
170600     MOVE '  ' TO GODK-STATUSKODER                                        
170700     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA21                   
170800     MOVE INV-STATUS-CODE TO STATUS-WS                                    
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100     EJECT                                                                
171200 IMS-INSERT-WDH121 SECTION.                                               
171300**   MOVE 'IMS-INSERT-WDH121     ' TO WS-IMS                              
171400     MOVE 'WDH121 ' TO SSA1                                               
171500     MOVE '  ' TO GODK-STATUSKODER                                        
171600     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA21 SSA1              
171700     MOVE INV-STATUS-CODE TO STATUS-WS                                    
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     EJECT                                                                
172100******* OPERATIONER MOT WDE4                                              
172200     SKIP2                                                                
172300*9748228 START                                                            
172400 IMS-GU-WDE4C1   SECTION.                                                 
172500     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
172600                    '&WDE4C1KY=<' W-WDE4C1KY-HIGH                         
172700                    '&KDRADSTA <' W-ORDSTA-X ')'                          
172800             DELIMITED BY SIZE INTO SSA1                                  
172900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
173000     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
173100     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
173200     PERFORM IMS-STATUSKONTROLL                                           
173300     .                                                                    
173400     SKIP2                                                                
173500 IMS-GN-WDE4C1   SECTION.                                                 
173600     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
173700                    '&WDE4C1KY=<' W-WDE4C1KY-HIGH                         
173800                    '&KDRADSTA <' W-ORDSTA-X ')'                          
173900             DELIMITED BY SIZE INTO SSA1                                  
174000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
174100     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
174200     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
174300     PERFORM IMS-STATUSKONTROLL                                           
174400     .                                                                    
174500     SKIP2                                                                
174600*9748228 END                                                              
174700 IMS-GU-WDE4C-DISTR-98 SECTION.                                           
174800                                                                          
174900     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
175000             '&WDE4C1KY=<' W-WDE4C1KY-HIGH                                
175100             '&IDDISTR  =' W-IDDISTR-X ')'                                
175200             DELIMITED BY SIZE INTO SSA1                                  
175300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
175400     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
175500     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
175600     PERFORM IMS-STATUSKONTROLL                                           
175700     .                                                                    
175800     SKIP2                                                                
175900 IMS-GN-WDE4C-DISTR-98 SECTION.                                           
176000                                                                          
176100     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
176200             '&WDE4C1KY=<' W-WDE4C1KY-HIGH                                
176300             '&IDDISTR  =' W-IDDISTR-X ')'                                
176400             DELIMITED BY SIZE INTO SSA1                                  
176500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
176600     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
176700     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
176800     PERFORM IMS-STATUSKONTROLL                                           
176900     .                                                                    
177000     EJECT                                                                
177100 IMS-GU-WDE4-ROT SECTION.                                                 
177200                                                                          
177300     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
177400             DELIMITED BY SIZE INTO SSA1                                  
177500     MOVE '    ' TO GODK-STATUSKODER                                      
177600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
177700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
177800     PERFORM IMS-STATUSKONTROLL                                           
177900     .                                                                    
178000     EJECT                                                                
178100 IMS-GNP-WDE411       SECTION.                                            
178200                                                                          
178300     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
178400             DELIMITED BY SIZE INTO SSA1                                  
178500     MOVE '  ' TO GODK-STATUSKODER                                        
178600     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
178700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
178800     PERFORM IMS-STATUSKONTROLL                                           
178900     .                                                                    
179000     EJECT                                                                
179100 IMS-GU-BEN-SEQ SECTION.                                                  
179200                                                                          
179300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
179400             DELIMITED BY SIZE INTO SSA1                                  
179500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
179600             DELIMITED BY SIZE INTO SSA2                                  
179700     MOVE '  GE' TO GODK-STATUSKODER                                      
179800     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1 SSA2                  
179900     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
180000     PERFORM IMS-STATUSKONTROLL                                           
180100     .                                                                    
180200     EJECT                                                                
180300 IMS-GU-WDG301 SECTION.                                                   
180400     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
180500            DELIMITED BY SIZE INTO SSA1                                   
180600     MOVE '  ' TO GODK-STATUSKODER                                        
180700     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-AREA-WDGX5102 SSA1             
180800     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     EJECT                                                                
181200 IMS-GHU-WDGX5102 SECTION.                                                
181300     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
181400            DELIMITED BY SIZE INTO SSA1                                   
181500     STRING 'WDGX5102 '                                                   
181600            DELIMITED BY SIZE INTO SSA2                                   
181700     MOVE '  GE' TO GODK-STATUSKODER                                      
181800     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-AREA-WDGX5102                 
181900                                               SSA1 SSA2                  
182000     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     .                                                                    
182300     EJECT                                                                
182400 IMS-REPL-WDGX5102 SECTION.                                               
182500     MOVE '  ' TO GODK-STATUSKODER                                        
182600     CALL CBLTDLI USING REPL WDG3-PCB DLI-IO-AREA-WDGX5102                
182700     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
182800     PERFORM IMS-STATUSKONTROLL                                           
182900     .                                                                    
183000     EJECT                                                                
183100 IMS-ISRT-WDGX5102 SECTION.                                               
183200     MOVE 'WDGX5102 ' TO SSA1                                             
183300     MOVE '  ' TO GODK-STATUSKODER                                        
183400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-WDGX5102                
183500                                  SSA1                                    
183600     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
183700     PERFORM IMS-STATUSKONTROLL                                           
183800     .                                                                    
183900     EJECT                                                                
184000 IMS-GU-WDB601    SECTION.                                                
184100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
184200          DELIMITED BY SIZE INTO SSA1                                     
184300     MOVE '  GE' TO GODK-STATUSKODER                                      
184400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
184500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
184600     PERFORM IMS-STATUSKONTROLL                                           
184700     .                                                                    
184800 IMS-STATUSKONTROLL SECTION.                                              
184900     SET STATUS-IX TO 1                                                   
185000     SEARCH GODK-STATUS                                                   
185100       AT END                                                             
185200         CALL FELLOG                                                      
185300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
185400         CONTINUE                                                         
185500     END-SEARCH                                                           
186000     .                                                                    
