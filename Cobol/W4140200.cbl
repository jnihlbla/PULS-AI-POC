000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4140200.                                                
000400*AUTHOR.         GUNNAR LARSSON IDK.                                      
000500*DATE-WRITTEN.   92/03/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER INFILEN W41402 (SORTERAD I ARTIKELNR-ORDNING)              
001100*        KOMPLETTERAR MED ARTIKELINFO OCH SKRIVER UTFIL W41403.           
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDK7                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*      --- ORDERRAD,ORDERBEKR, TILL.TPO & ANNVOR (SORT. ARTNR)            
002900     SELECT W41402                     ASSIGN TO W41402D1.                
003000     SKIP2                                                                
003100*      --- INFILEN-UT KOMPLETTERAT MED ART.INFO                           
003200     SELECT W41403                     ASSIGN TO W41402D2.                
003300     SKIP2                                                                
003400*      --- PASSIVE PARTS INFO                                             
003500     SELECT W41403A                    ASSIGN TO W41402D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W41402                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W414S002      -L.                                              
004600     SKIP2                                                                
004700*01  -COPY W414S003      -L.                                              
004800     SKIP2                                                                
004900*01  -COPY W414S005      -L.                                              
005000     SKIP3                                                                
005100*01  -COPY W414S011      -L.                                              
005200     SKIP3                                                                
005300 FD  W41403                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS  0.                                                   
005600     SKIP2                                                                
005700*01  ORAD-POST -COPY W414002 -PRE  W41403-  -L.                           
005800     SKIP2                                                                
005900*01  OBKR-POST -COPY W414003 -PRE  W41403-  -L.                           
006000     SKIP2                                                                
006100*01  TILLTPO-POST -COPY W414005 -PRE  W41403-  -L.                        
006200     SKIP2                                                                
006300*01  ANNVOR-POST -COPY W414011 -PRE  W41403-  -L.                         
006400 FD  W41403A                                                              
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700 01  PASSIVE-PART          PIC X(165).                                    
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000     SKIP2                                                                
007100*    -COPY WY2000W1                                                       
007200                                                                          
007300*    -- CHECKED BY WY2000                                                 
007400 77  IDPGM                       PIC X(8)    VALUE 'W4140200'.            
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700                                                                          
007800*01  -COPY WWDC99                                                         
007900*01  -COPY WWLNDKON                                                       
008000                                                                          
008100 77  W-LAST-ARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
008200                                                                          
008300 77  W41402-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-W41402                       VALUE 'J'.                   
008500     EJECT                                                                
008600 01      WS.                                                              
008700*        -- AKTUELLT ARTIKELNR (BRYT-BEGREPP)                             
008800  02     WS-AKT-IDARTNR          PIC S9(9)   VALUE ZERO  COMP-3.          
008900*        -- SPARAT FRÅN SEGMENT                                           
009000  02     WS-WDK6.                                                         
009100   03    WS-ART.                                                          
009200    04   WS-ART-IDFKNGRP         PIC S9(5)      VALUE ZERO COMP-3.        
009300    04   WS-ART-KDPRODSL         PIC S9(3)      VALUE ZERO COMP-3.        
009400   03    WS-CLAG.                                                         
009500    04   WS-CLAG-FLREFILL        PIC X(1)       VALUE SPACE.              
009600    04   WS-CLAG-KDUART          PIC X(1)       VALUE SPACE.              
009700    04   WS-CLAG-KDTIPPR         PIC S9(1)      VALUE ZERO COMP-3.        
009800    04   WS-CLAG-PRARTSJK        PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009900    04   WS-CLAG-REDIRLEV        PIC S9V9(2)    VALUE ZERO COMP-3.        
010000     EJECT                                                                
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200*                                                                         
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010900     SKIP2                                                                
011000*    --- PARAMETRAR TILL ABEND                                            
011100                                                                          
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE +0.          
011500 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
011600 77  WS-TIRFS                    PIC 9(6).                                
011700 77  W-SPAR-TILEVBSK             PIC S9(7) COMP-3 VALUE ZERO.             
011800     SKIP2                                                                
011900 01  WS-HELPDAT                  PIC 9(6).                                
012000 01  WS-TODAY                    PIC 9(6).                                
012100 01  FILLER REDEFINES WS-TODAY.                                           
012200     03 WS-YEAR                  PIC 9(2).                                
012300     03 WS-MONTH                 PIC 9(2).                                
012400     03 WS-DAYS                  PIC 9(2).                                
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL POSTSUM                                          
013000*                                                                         
013100*01  -COPY W0005   -PRE  POSTSUM-                                         
013200     EJECT                                                                
013300*---- PARAMETER TO SUBPROGRAM WORKDAY                                     
013400*01 -COPY WORKAREA                                                        
013500*---- PARAMETER TO SUBPROGRAM WDATKONV                                    
013600*01 -COPY WDATAREA                                                        
013700     EJECT                                                                
013800 01  W41402-AREA-START           PIC X(24)   VALUE                        
013900                                 'W41402-AREA-START  '.                   
014000     SKIP2                                                                
014100 01  IN-AREA.                                                             
014200     03  W41402-IDARTNR-S        PIC S9(9)           COMP-3.              
014300     03  W41402-IDARTNR-TILLK-S  PIC S9(9)           COMP-3.              
014400     03  W41402-AREA.                                                     
014500         05  W41402-IDPTYP       PIC X(3).                                
014600         05  W41402-IDARTNR      PIC S9(9)           COMP-3.              
014700         05  FILLER              PIC X(300).                              
014800     SKIP2                                                                
014900*03  ORAD-AREA -COPY W414002   -PRE W41402-   -RED  W41402-AREA           
015000     EJECT                                                                
015100*03  OBKR-AREA -COPY W414003   -PRE W41402-   -RED  W41402-AREA           
015200     EJECT                                                                
015300*03  TILLTPO-AREA -COPY W414005  -PRE W41402-   -RED  W41402-AREA         
015400     EJECT                                                                
015500*03  ANNVOR-AREA -COPY W414011   -PRE W41402-   -RED  W41402-AREA         
015600     EJECT                                                                
015700 01  W41403-ORAD-AREA-START      PIC X(24)   VALUE                        
015800                                 'W41403-ORAD-AREA-START'.                
015900     SKIP2                                                                
016000*01  ORAD-AREA -COPY W414002     -PRE W41403-                             
016100     EJECT                                                                
016200                                                                          
016300 01  W41403-OBKR-AREA-START      PIC X(24)   VALUE                        
016400                                 'W41403-OBKR-AREA-START'.                
016500     SKIP2                                                                
016600*01  OBKR-AREA -COPY W414003     -PRE W41403-                             
016700     EJECT                                                                
016800 01  W41403-TILLTPO-AREA-START      PIC X(24)   VALUE                     
016900                                 'W41403-TILLTPO-AREA-STAR'.              
017000     SKIP2                                                                
017100*01  TILLTPO-AREA -COPY W414005     -PRE W41403-                          
017200     EJECT                                                                
017300 01  W41403-ANNVOR-AREA-START       PIC X(24)   VALUE                     
017400                                 'W41403-ANNVOR-AREA-STAR'.               
017500     SKIP2                                                                
017600*01  ANNVOR-AREA -COPY W414011      -PRE W41403-                          
017700     EJECT                                                                
017800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000     SKIP2                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018200     SKIP3                                                                
018300 01  NYCKLAR-TILL-DLI.                                                    
018400     03  W-IDARTNR-X.                                                     
018500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018600     SKIP2                                                                
018700     03  W-IDLAND-X.                                                      
018800         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
018900     SKIP2                                                                
019000     03  W-IDGMT-X.                                                       
019100         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
019200         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
019300     SKIP2                                                                
019400     03  W-WDD901KY-X.                                                    
019500         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
019600         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
019700     SKIP2                                                                
019800     03  W-IDLEVNR-X.                                                     
019900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
020000     SKIP2                                                                
020100 01  W41221UT-AREA-START         PIC X(24)   VALUE                        
020200                                 'W41221UT-AREA-START'.                   
020300                                                                          
020400 01  W001-DAP.                                                            
020500     03  FILLER                  PIC X(165)  VALUE SPACE.                 
020600     SKIP2                                                                
020700 03  WS-DAP-SUBTYPE.                                                      
020800     05  WS-IDDISTR-X            PIC 9(4)    VALUE ZERO.                  
020900     05  WS-IDKUNDNR-X           PIC 9(6)    VALUE ZERO.                  
021000     SKIP2                                                                
021100 01  DAP-MAIL-BODY-AREA1.                                                 
021200      03  DAP-MAIL-LINE1-AREA1.                                           
021300          05 DAP-MAIL-LINE-TEXT-1      PIC X(47) VALUE                    
021400               'FROM  VOLVO CAR CUSTOMER SERVICE CDC GOTHENBURG'.         
021500                                                                          
021600      03  DAP-MAIL-LINE2-AREA1.                                           
021700          05 DAP-MAIL-LINE-TEXT-2      PIC X(57) VALUE                    
021800     'FOR THE URGENT ATTENTION OF THE PARTS ORDERING DEPARTMENT'.         
021900                                                                          
022000      03  DAP-MAIL-LINE3-AREA1.                                           
022100          05 DAP-MAIL-LINE-TEXT-3      PIC X(23) VALUE                    
022200                                       'Please see the details:'.         
022300     SKIP2                                                                
022400 01  DAP-MAIL-BODY-AREA2.                                                 
022500      03  DAP-MAIL-LINE1-AREA2.                                           
022600          05 DAP-MAIL-LINE-TEXT-4      PIC X(14) VALUE                    
022700                                                'District No.  '.         
022800      03  DAP-MAIL-LINE2-AREA2.                                           
022900          05 DAP-MAIL-LINE-TEXT-5      PIC X(10) VALUE                    
023000                                                'Customer  '.             
023100      03  DAP-MAIL-LINE3-AREA2.                                           
023200          05 DAP-MAIL-LINE-TEXT-6      PIC X(10) VALUE                    
023300                                                'Part No.  '.             
023400      03  DAP-MAIL-LINE4-AREA2.                                           
023500          05 DAP-MAIL-LINE-TEXT-7      PIC X(10) VALUE                    
023600                                                'Quantity  '.             
023700      03  DAP-MAIL-LINE5-AREA2.                                           
023800          05 DAP-MAIL-LINE-TEXT-8      PIC X(10) VALUE                    
023900                                                'Order No. '.             
024000      03  DAP-MAIL-LINE6-AREA2.                                           
024100          05 DAP-MAIL-LINE-TEXT-9      PIC X(12) VALUE                    
024200                                                'Order Class '.           
024300      03  DAP-MAIL-LINE7-AREA2.                                           
024400          05 DAP-MAIL-LINE-TEXT-10     PIC X(13) VALUE                    
024500                                                'Freight Code '.          
024600      03  DAP-MAIL-LINE8-AREA2.                                           
024700          05 DAP-MAIL-LINE-TEXT-11     PIC X(10) VALUE                    
024800                                                'Work Days '.             
024900      03  DAP-MAIL-LINE9-AREA2.                                           
025000          05 DAP-MAIL-LINE-TEXT-12     PIC X(10) VALUE                    
025100                                                'RFS DATE  '.             
025200      03  DAP-MAIL-LINEA-AREA2.                                           
025300          05 DAP-MAIL-LINE-TEXT-13     PIC X(10) VALUE                    
025400                                                'Repair DT '.             
025500      03  DAP-MAIL-LINEB-AREA2.                                           
025600          05 DAP-MAIL-LINE-TEXT-14     PIC X(15) VALUE                    
025700                                              'Workshop Order '.          
025800      03  DAP-MAIL-LINEC-AREA2.                                           
025900          05 DAP-MAIL-LINE-TEXT-15     PIC X(3)  VALUE                    
026000                                                'DC '.                    
026100     SKIP2                                                                
026200 01  DAP-MAIL-BODY-AREA3.                                                 
026300      03  DAP-MAIL-LINE1-AREA3.                                           
026400          05 DAP-MAIL-LINE-FILLER1     PIC X(8) VALUE SPACE.              
026500          05 DAP-MAIL-DISTRICT-NO      PIC Z(3)9.                         
026600          05 DAP-MAIL-LINE-FILLER2     PIC X(2) VALUE SPACE.              
026700      03  DAP-MAIL-LINE2-AREA3.                                           
026800          05 DAP-MAIL-LINE-FILLER3     PIC X(2) VALUE SPACE.              
026900          05 DAP-MAIL-CUSTOMER-NO      PIC Z(5)9.                         
027000          05 DAP-MAIL-LINE-FILLER4     PIC X(2) VALUE SPACE.              
027100      03  DAP-MAIL-LINE3-AREA3.                                           
027200          05 DAP-MAIL-PART-NO          PIC Z(7)9.                         
027300          05 DAP-MAIL-LINE-FILLER5     PIC X(2) VALUE SPACE.              
027400      03  DAP-MAIL-LINE4-AREA3.                                           
027500          05 DAP-MAIL-LINE-FILLER6     PIC X(2) VALUE SPACE.              
027600          05 DAP-MAIL-QTY              PIC Z(6)9.                         
027700          05 DAP-MAIL-LINE-FILLER6     PIC X(2) VALUE SPACE.              
027800      03  DAP-MAIL-LINE5-AREA3.                                           
027900          05 DAP-MAIL-ORDER-NO         PIC X(10).                         
028000      03  DAP-MAIL-LINE6-AREA3.                                           
028100          05 DAP-MAIL-ORDER-CLASS      PIC 9(1).                          
028200          05 DAP-MAIL-LINE-FILLER7     PIC X(11) VALUE SPACE.             
028300      03  DAP-MAIL-LINE7-AREA3.                                           
028400          05 DAP-MAIL-FREIGHT-CODE     PIC 9(2).                          
028500          05 DAP-MAIL-LINE-FILLER8     PIC X(11) VALUE SPACE.             
028600      03  DAP-MAIL-LINE8-AREA3.                                           
028700          05 DAP-MAIL-WORK-DAYS        PIC 9(2).                          
028800          05 DAP-MAIL-LINE-FILLER9     PIC X(8) VALUE SPACE.              
028900      03  DAP-MAIL-LINE9-AREA3.                                           
029000          05 DAP-MAIL-RFS-DATE         PIC 9(6).                          
029100          05 DAP-MAIL-LINE-FILLERA     PIC X(4) VALUE SPACE.              
029200      03  DAP-MAIL-LINEA-AREA3.                                           
029300          05 DAP-MAIL-REPAIR-DATE      PIC 9(6).                          
029400          05 DAP-MAIL-LINE-FILLERB     PIC X(4) VALUE SPACE.              
029500      03  DAP-MAIL-LINEB-AREA3.                                           
029600          05 DAP-MAIL-WORKSHOP-ORDER   PIC X(10).                         
029700          05 DAP-MAIL-LINE-FILLERC     PIC X(5) VALUE SPACE.              
029800      03  DAP-MAIL-LINEC-AREA3.                                           
029900          05 DAP-MAIL-DC               PIC X(2).                          
030000          05 DAP-MAIL-LINE-FILLERD     PIC X(1) VALUE SPACE.              
030100     SKIP2                                                                
030200 01  DAP-MAIL-BODY-AREA4.                                                 
030300      03  DAP-MAIL-LINE1-AREA4.                                           
030400          05 DAP-MAIL-LINE-TEXT-12-1   PIC X(55) VALUE                    
030500     'WE WISH TO ADVISE YOU THAT THIS PART IS PASSIVE AND WE '.           
030600                                                                          
030700      03  DAP-MAIL-LINE2-AREA4.                                           
030800          05 DAP-MAIL-LINE-TEXT-13-1   PIC X(55) VALUE                    
030900     'WILL BE UNABLE TO OBTAIN YOUR ORDERED QUANTITY IN TIME '.           
031000          05 DAP-MAIL-LINE-TEXT-13-2   PIC X(04) VALUE    'FOR '.         
031100                                                                          
031200      03  DAP-MAIL-LINE3-AREA4.                                           
031300          05 DAP-MAIL-LINE-TEXT-14     PIC X(17) VALUE                    
031400                         'YOUR REPAIR DATE.'.                             
031500                                                                          
031600      03  DAP-MAIL-LINE4-AREA4.                                           
031700          05 DAP-MAIL-LINE-TEXT-15-1   PIC X(54) VALUE                    
031800     'BACK ORDER IS KEPT AND WILL BE RELEASED FROM CDC WHEN '.            
031900          05 DAP-MAIL-LINE-TEXT-15-2   PIC X(45) VALUE                    
032000     'STOCK ARRIVES. IT IS NOT MONITORED MANUALLY.'.                      
032100                                                                          
032200      03  DAP-MAIL-LINE5-AREA4.                                           
032300          05 DAP-MAIL-LINE-TEXT-16-1   PIC X(55) VALUE                    
032400     'IF YOU HAVE QUESTIONS PLEASE KINDLY CONTACT YOUR SALES '.           
032500          05 DAP-MAIL-LINE-TEXT-16-2   PIC X(32) VALUE                    
032600     'COMPANY (Dealer Service Office).'.                                  
032700                                                                          
032800      03  DAP-MAIL-LINE6-AREA4.                                           
032900          05 DAP-MAIL-LINE-TEXT-16     PIC X(46) VALUE                    
033000                        '(DO NOT REPLY ON THIS E-MAIL MESSAGE.)'.         
033100                                                                          
033200      03  DAP-MAIL-LINE7-AREA4.                                           
033300          05 DAP-MAIL-LINE-TEXT-17-1   PIC X(43) VALUE                    
033400                   'EXPECTED TIME OF ARRIVAL AT DC 11 EARLIEST '.         
033500          05 DAP-MAIL-LINE-VALUE-WW    PIC 9(2) VALUE 0.                  
033600          05 DAP-MAIL-LINE-CONST       PIC X(1) VALUE ':'.                
033700          05 DAP-MAIL-LINE-VALUE-D     PIC 9(1) VALUE 0.                  
033800          05 DAP-MAIL-LINE-SPACE       PIC X(2) VALUE SPACE.              
033900          05 DAP-MAIL-LINE-TEXT-17-2   PIC X(53) VALUE                    
034000         '-  FYI Passive parts can have a lead time of 10 weeks'.         
034100                                                                          
034200      03  DAP-MAIL-LINE-AREA.                                             
034300          05 DAP-MAIL-LINE-TEXT-SPACE  PIC X(80) VALUE SPACES.            
034400     SKIP2                                                                
034500*    --- STATUS-KOD FRÅN IMS                                              
034600 01  STATUS-WS                   PIC XX.                                  
034700     88  SEGMENT-FINNS                       VALUE '  '.                  
034800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035000     SKIP2                                                                
035100 01  GODK-STATUSKODER.                                                    
035200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035300     SKIP3                                                                
035400 01  SSA1                        PIC X(64).                               
035500 01  SSA2                        PIC X(64).                               
035600     EJECT                                                                
035700*    --- IMS FUNKTIONSKODER                                               
035800*01  -COPY W0003                                                          
035900     EJECT                                                                
036000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK601'.          
036100 01  DLI-IO-WDK601.                                                       
036200*  03  WDK601 -COPY WDK601                                                
036300     EJECT                                                                
036400 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK611'.          
036500 01  DLI-IO-WDK611.                                                       
036600*  05  WDK611 -COPY WDK611                                                
036700     EJECT                                                                
036800 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK712'.          
036900 01  DLI-IO-WDK712.                                                       
037000*  05  WDK712 -COPY WDK712                                                
037100 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDB201'.          
037200 01  DLI-IO-WDB201.                                                       
037300*  05  WDB201 -COPY WDB201                                                
037400 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDD901'.          
037500 01  DLI-IO-WDD901.                                                       
037600*  05  WDD901 -COPY WDD901.                                               
037700 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDD902'.          
037800 01  DLI-IO-WDD902.                                                       
037900*  05  WDD902 -COPY WDD902                                                
038000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDD924'.          
038100 01  DLI-IO-WDD924.                                                       
038200*  05  WDD924 -COPY WDD924                                                
038300     EJECT                                                                
038400 LINKAGE SECTION.                                                         
038500                                                                          
038600*01  -COPY W0009            -PRE MSG-                                     
038700                                                                          
038800*01  -COPY W0009            -PRE DISTRDOC-                                
038900                                                                          
039000*01  -COPY W0008  -PRE WDK6-                                              
039100     05  FILLER                  PIC X.                                   
039200                                                                          
039300*01  -COPY W0008  -PRE WDK7-                                              
039400     05  FILLER                  PIC X.                                   
039500                                                                          
039600*01  -COPY W0008  -PRE WDB2-                                              
039700     05  FILLER                  PIC X.                                   
039800                                                                          
039900*01  -COPY W0008  -PRE WDD9-                                              
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200 PROCEDURE DIVISION  USING WDK6-PCB WDK7-PCB WDB2-PCB WDD9-PCB.           
040300                                                                          
040400     ENTRY 'DLITCBL' USING WDK6-PCB WDK7-PCB WDB2-PCB WDD9-PCB.           
040500                                                                          
040600     SKIP2                                                                
040700     PERFORM A-INIT                                                       
040800     PERFORM S01-LAES-W41402                                              
040900     PERFORM S02-LAST-ARTNR                                               
041000                                                                          
041100     PERFORM UNTIL END-OF-W41402                                          
041200       PERFORM B-BEH-ART                                                  
041300                                                                          
041400       PERFORM UNTIL (END-OF-W41402                                       
041500                   OR W-LAST-ARTNR NOT = WS-AKT-IDARTNR)                  
041600                                                                          
041700         IF W41402-IDPTYP = '201'                                         
041800           PERFORM C-SKAPA-ORAD-POST                                      
041900         END-IF                                                           
042000         IF W41402-IDPTYP = '202'                                         
042100           PERFORM D-SKAPA-OBKR-POST                                      
042200         END-IF                                                           
042300         IF W41402-IDPTYP = '203'                                         
042400           PERFORM E-SKAPA-TILLTPO-POST                                   
042500         END-IF                                                           
042600         IF W41402-IDPTYP = '205'                                         
042700           PERFORM F-SKAPA-ANNVOR-POST                                    
042800         END-IF                                                           
042900                                                                          
043000         PERFORM S01-LAES-W41402                                          
043100         PERFORM S02-LAST-ARTNR                                           
043200       END-PERFORM                                                        
043300     END-PERFORM                                                          
043400                                                                          
043500                                                                          
043600     PERFORM Z-FINIT                                                      
043700                                                                          
043800     MOVE ZERO TO RETURN-CODE                                             
043900     GOBACK                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 A-INIT SECTION.                                                          
044300                                                                          
044400     OPEN INPUT  W41402                                                   
044500                                                                          
044600     OPEN OUTPUT W41403                                                   
044700                 W41403A                                                  
044800                                                                          
044900     ACCEPT WS-TODAY           FROM DATE                                  
045000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
045100                                                                          
045200     MOVE ZERO                   TO WS-AKT-IDARTNR                        
045300     .                                                                    
045400     EJECT                                                                
045500 B-BEH-ART SECTION.                                                       
045600                                                                          
045700     IF W41402-IDARTNR-TILLK-S > ZERO                                     
045800        MOVE W41402-IDARTNR-TILLK-S TO WS-AKT-IDARTNR                     
045900     ELSE                                                                 
046000        MOVE W41402-IDARTNR-S       TO WS-AKT-IDARTNR                     
046100     END-IF                                                               
046200                                                                          
046300     MOVE ZERO                   TO WS-ART-IDFKNGRP                       
046400     MOVE ZERO                   TO WS-ART-KDPRODSL                       
046500     MOVE ZERO                   TO WS-CLAG-KDTIPPR                       
046600     MOVE ZERO                   TO WS-CLAG-PRARTSJK                      
046700     MOVE ZERO                   TO WS-CLAG-REDIRLEV                      
046800     MOVE SPACE                  TO WS-CLAG-KDUART                        
046900     MOVE SPACE                  TO WS-CLAG-FLREFILL                      
047000                                                                          
047100     MOVE WS-AKT-IDARTNR         TO W-IDARTNR                             
047200                                                                          
047300     PERFORM IMS-GU-WDK601                                                
047400     IF SEGMENT-FINNS                                                     
047500       MOVE ART-IDFKNGRP         TO WS-ART-IDFKNGRP                       
047600       MOVE ART-KDPRODSL         TO WS-ART-KDPRODSL                       
047700                                                                          
047800       PERFORM IMS-GNP-WDK611                                             
047900       IF SEGMENT-FINNS                                                   
048000          MOVE CLAG-KDTIPPR      TO WS-CLAG-KDTIPPR                       
048100          MOVE CLAG-PRARTSJK     TO WS-CLAG-PRARTSJK                      
048200          MOVE CLAG-REDIRLEV     TO WS-CLAG-REDIRLEV                      
048300          MOVE CLAG-FLREFILL     TO WS-CLAG-FLREFILL                      
048400          MOVE CLAG-KDUART       TO WS-CLAG-KDUART                        
048500       ELSE                                                               
048600          DISPLAY '611-SEGMENT SAKNAS ' W-IDARTNR                         
048700       END-IF                                                             
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 C-SKAPA-ORAD-POST SECTION.                                               
049200                                                                          
049300     MOVE W41402-ORAD-AREA       TO W41403-ORAD-AREA                      
049400                                                                          
049500     MOVE WS-ART-IDFKNGRP        TO W41403-ORAD-IDFKNGRP                  
049600     MOVE WS-ART-KDPRODSL        TO W41403-ORAD-KDPRODSL                  
049700     MOVE WS-CLAG-FLREFILL       TO W41403-ORAD-FLREFILL                  
049800     MOVE WS-CLAG-KDTIPPR        TO W41403-ORAD-KDTIPPR                   
049900     MOVE WS-CLAG-KDUART         TO W41403-ORAD-KDUART                    
050000     MOVE WS-CLAG-PRARTSJK       TO W41403-ORAD-PRARTSJK                  
050100     MOVE WS-CLAG-REDIRLEV       TO W41403-ORAD-REDIRLEV                  
050200     MOVE ZERO                   TO W41403-ORAD-PRARTBTO-EXP              
050300                                                                          
050400     MOVE W41403-ORAD-IDDC       TO WS-IDDC                               
050600     IF NDC-CN                                                            
050700       MOVE WC-LAND-CN           TO W-IDLAND                              
050800       PERFORM IMS-GU-WDK712                                              
050900       MOVE LART-PRARTSJK        TO W41403-ORAD-PRARTSJK                  
051000     END-IF                                                               
051100                                                                          
051200     PERFORM S12-SKRIV-W41403-ORAD                                        
051300     .                                                                    
051400     EJECT                                                                
051500 D-SKAPA-OBKR-POST SECTION.                                               
051600                                                                          
051700     MOVE W41402-OBKR-AREA       TO W41403-OBKR-AREA                      
051800                                                                          
051900     MOVE WS-ART-IDFKNGRP        TO W41403-OBKR-IDFKNGRP                  
052000     MOVE WS-ART-KDPRODSL        TO W41403-OBKR-KDPRODSL                  
052100     MOVE WS-CLAG-KDTIPPR        TO W41403-OBKR-KDTIPPR                   
052200     MOVE WS-CLAG-KDUART         TO W41403-OBKR-KDUART                    
052300     MOVE WS-CLAG-PRARTSJK       TO W41403-OBKR-PRARTSJK                  
052400     MOVE ZERO                   TO W41403-OBKR-PRARTBTO-EXP              
052500     MOVE W41403-OBKR-IDDC       TO WS-IDDC                               
052600                                                                          
052700     PERFORM S13-SKRIV-W41403-OBKR                                        
052800     IF ((WS-CLAG-KDUART = 'P' OR 'L' OR 'S' OR 'M') AND                  
052900          W41403-OBKR-KDORDBEK = 70  AND                                  
053000          W41403-OBKR-TIREPDAT > 0 )                                      
053100          PERFORM S90-WRITE-DAP-LINE1                                     
053200          MOVE W41402-OBKR-IDDISTR    TO WS-IDDISTR-X                     
053300                                         DAP-MAIL-DISTRICT-NO             
053400          MOVE W41402-OBKR-IDKUNDNR   TO WS-IDKUNDNR-X                    
053500                                         DAP-MAIL-CUSTOMER-NO             
053600          MOVE W41402-OBKR-IDARTNR    TO DAP-MAIL-PART-NO                 
053700          PERFORM S90-WRITE-DAP-LINE2                                     
053800          PERFORM DA-PUT-MAIL-BODY                                        
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 DA-PUT-MAIL-BODY SECTION.                                                
054300                                                                          
054400     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
054500     WRITE PASSIVE-PART             FROM W001-DAP                         
054600                                                                          
054700     MOVE SPACE                       TO W001-DAP                         
054800                                                                          
054900     MOVE DAP-MAIL-LINE1-AREA1        TO W001-DAP                         
055000     WRITE PASSIVE-PART             FROM W001-DAP                         
055100                                                                          
055200     MOVE SPACE                       TO W001-DAP                         
055300                                                                          
055400     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
055500     WRITE PASSIVE-PART             FROM W001-DAP                         
055600                                                                          
055700     MOVE SPACE                       TO W001-DAP                         
055800                                                                          
055900     MOVE DAP-MAIL-LINE2-AREA1        TO W001-DAP                         
056000     WRITE PASSIVE-PART             FROM W001-DAP                         
056100                                                                          
056200     MOVE SPACE                       TO W001-DAP                         
056300                                                                          
056400     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
056500     WRITE PASSIVE-PART             FROM W001-DAP                         
056600                                                                          
056700     MOVE SPACE                       TO W001-DAP                         
056800                                                                          
056900     MOVE DAP-MAIL-LINE3-AREA1        TO W001-DAP                         
057000     WRITE PASSIVE-PART             FROM W001-DAP                         
057100                                                                          
057200     MOVE SPACE                       TO W001-DAP                         
057300                                                                          
057400     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
057500     WRITE PASSIVE-PART             FROM W001-DAP                         
057600                                                                          
057700     MOVE SPACE                       TO W001-DAP                         
057800                                                                          
057900     PERFORM DAA-BUILD-DAP-ROW-DATA                                       
058000                                                                          
058100     MOVE DAP-MAIL-BODY-AREA2         TO W001-DAP                         
058200     WRITE PASSIVE-PART             FROM W001-DAP                         
058300                                                                          
058400     MOVE SPACE                       TO W001-DAP                         
058500                                                                          
058600     MOVE DAP-MAIL-BODY-AREA3         TO W001-DAP                         
058700     WRITE PASSIVE-PART             FROM W001-DAP                         
058800                                                                          
058900     MOVE SPACE                       TO W001-DAP                         
059000                                                                          
059100     PERFORM DAB-REDIGERA-WDD9-INLA                                       
059200     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
059300     WRITE PASSIVE-PART             FROM W001-DAP                         
059400                                                                          
059500     MOVE SPACE                       TO W001-DAP                         
059600                                                                          
059700     MOVE DAP-MAIL-LINE1-AREA4        TO W001-DAP                         
059800     WRITE PASSIVE-PART             FROM W001-DAP                         
059900                                                                          
060000     MOVE SPACE                       TO W001-DAP                         
060100                                                                          
060200     MOVE DAP-MAIL-LINE2-AREA4        TO W001-DAP                         
060300     WRITE PASSIVE-PART             FROM W001-DAP                         
060400                                                                          
060500     MOVE SPACE                       TO W001-DAP                         
060600                                                                          
060700     MOVE DAP-MAIL-LINE3-AREA4        TO W001-DAP                         
060800     WRITE PASSIVE-PART             FROM W001-DAP                         
060900                                                                          
061000     MOVE SPACE                       TO W001-DAP                         
061100                                                                          
061200     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
061300     WRITE PASSIVE-PART             FROM W001-DAP                         
061400                                                                          
061500     MOVE SPACE                       TO W001-DAP                         
061600                                                                          
061700     MOVE DAP-MAIL-LINE4-AREA4        TO W001-DAP                         
061800     WRITE PASSIVE-PART             FROM W001-DAP                         
061900                                                                          
062000     MOVE SPACE                       TO W001-DAP                         
062100                                                                          
062200     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
062300     WRITE PASSIVE-PART             FROM W001-DAP                         
062400                                                                          
062500     MOVE SPACE                       TO W001-DAP                         
062600                                                                          
062700     MOVE DAP-MAIL-LINE5-AREA4        TO W001-DAP                         
062800     WRITE PASSIVE-PART             FROM W001-DAP                         
062900                                                                          
063000     MOVE SPACE                       TO W001-DAP                         
063100                                                                          
063200     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
063300     WRITE PASSIVE-PART             FROM W001-DAP                         
063400                                                                          
063500     MOVE SPACE                       TO W001-DAP                         
063600                                                                          
063700     MOVE DAP-MAIL-LINE6-AREA4        TO W001-DAP                         
063800     WRITE PASSIVE-PART             FROM W001-DAP                         
063900                                                                          
064000     MOVE SPACE                       TO W001-DAP                         
064100                                                                          
064200     MOVE DAP-MAIL-LINE-TEXT-SPACE    TO W001-DAP                         
064300     WRITE PASSIVE-PART             FROM W001-DAP                         
064400                                                                          
064500     MOVE SPACE                       TO W001-DAP                         
064600                                                                          
064700     MOVE DAP-MAIL-LINE7-AREA4        TO W001-DAP                         
064800     WRITE PASSIVE-PART             FROM W001-DAP                         
064900                                                                          
065000     MOVE SPACE                       TO W001-DAP                         
065100                                                                          
065200     .                                                                    
065300     EJECT                                                                
065400 DAA-BUILD-DAP-ROW-DATA SECTION.                                          
065500                                                                          
065600     MOVE W41402-OBKR-IDDISTR      TO W-IDDISTR-WDB2                      
065700     MOVE W41402-OBKR-IDKUNDNR     TO W-IDKUNDNR-WDB2                     
065800     PERFORM IMS-GU-WDB201                                                
065900     MOVE 001                      TO WORK-KDCALL                         
066000     MOVE GMT-IDDC-BULK(1)         TO WORK-IDDC                           
066100     MOVE W41402-OBKR-TIREGDAT     TO WORK-TIAAMMDD-FOM                   
066200     MOVE W41402-OBKR-TIREPDAT     TO WORK-TIAAMMDD-TOM                   
066300     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
066400     IF WORK-KDSVAR-FEL                                                   
066500        MOVE ZERO                  TO WORK-KVWORKD                        
066600     END-IF                                                               
066700                                                                          
066800     PERFORM DAAA-SKAPA-RFSDATE                                           
066900                                                                          
067000     MOVE W41402-OBKR-KVBEART      TO DAP-MAIL-QTY                        
067100     MOVE W41402-OBKR-IDKUNDRF     TO DAP-MAIL-ORDER-NO                   
067200     MOVE W41402-OBKR-KDORDKL      TO DAP-MAIL-ORDER-CLASS                
067300     MOVE W41402-OBKR-KDFRAKT      TO DAP-MAIL-FREIGHT-CODE               
067400     MOVE WORK-KVWORKD             TO DAP-MAIL-WORK-DAYS                  
067500     MOVE WS-TIRFS                 TO DAP-MAIL-RFS-DATE                   
067600     MOVE W41402-OBKR-TIREPDAT     TO WS-HELPDAT                          
067700     MOVE WS-HELPDAT               TO DAP-MAIL-REPAIR-DATE                
067800     MOVE W41402-OBKR-BERADREF     TO DAP-MAIL-WORKSHOP-ORDER             
067900     MOVE W41402-OBKR-IDDC         TO DAP-MAIL-DC                         
068000     .                                                                    
068100     EJECT                                                                
068200 DAAA-SKAPA-RFSDATE SECTION.                                              
068300                                                                          
068400     MOVE W41402-OBKR-IDDC         TO WORK-IDDC                           
068500     MOVE +002                     TO WORK-KDCALL                         
068600     MOVE +001                     TO WORK-KVWORKD                        
068700     MOVE W41402-OBKR-TIREPDAT     TO WORK-TIAAMMDD-FOM                   
068800     CALL WORKDAY               USING WORK-KDCALL                         
068900                                      WORK-DATE-AREA                      
069000                                      WORK-KDSVAR                         
069100     IF WORK-KDSVAR-FEL                                                   
069200        MOVE 'SECT BA-1, DATUM SAKNAS I WORKDAY'                          
069300                                   TO FELTEXT-STR                         
069400        CALL ABEND              USING RKOD-ABEND-UTAN-DUMP                
069500     ELSE                                                                 
069600       MOVE +003                   TO WORK-KDCALL                         
069700       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
069800       PERFORM                                                            
069900       VARYING RFS-IX FROM 1 BY 1                                         
070000         UNTIL RFS-IX > MAX-RFS-IX                                        
070100         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
070200           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
070300                                   TO WORK-KVWORKD                        
070400         END-IF                                                           
070500       END-PERFORM                                                        
070600       ADD +1                      TO WORK-KVWORKD                        
070700*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
070800*      ANTAL DAGAR FÖRE RFS.                                              
070900*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
071000*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
071100*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
071200*                                                                         
071300                                                                          
071400       CALL WORKDAY             USING WORK-KDCALL                         
071500                                      WORK-DATE-AREA                      
071600                                      WORK-KDSVAR                         
071700       IF WORK-KDSVAR-FEL                                                 
071800          MOVE 'SECT BA-2, DATUM SAKNAS I WORKDAY'                        
071900                                   TO FELTEXT-STR                         
072000          CALL ABEND            USING RKOD-ABEND-UTAN-DUMP                
072100       ELSE                                                               
072200         IF WORK-TIAAMMDD-FOM < WS-TODAY                                  
072300           MOVE GMT-IDDC-BULK(1)   TO WORK-IDDC                           
072400           MOVE +002               TO WORK-KDCALL                         
072500           MOVE +001               TO WORK-KVWORKD                        
072600           MOVE WS-TODAY           TO WORK-TIAAMMDD-FOM                   
072700           CALL WORKDAY         USING WORK-KDCALL                         
072800                                      WORK-DATE-AREA                      
072900                                      WORK-KDSVAR                         
073000           IF WORK-KDSVAR-FEL                                             
073100              MOVE 'SECT BA-3, DATUM SAKNAS I WORKDAY'                    
073200                                   TO FELTEXT-STR                         
073300              CALL ABEND        USING RKOD-ABEND-UTAN-DUMP                
073400           ELSE                                                           
073500              MOVE WORK-TIAAMMDD-TOM                                      
073600                                   TO WS-TIRFS                            
073700           END-IF                                                         
073800         ELSE                                                             
073900           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
074000         END-IF                                                           
074100       END-IF                                                             
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 DAB-REDIGERA-WDD9-INLA SECTION.                                          
074600                                                                          
074700     MOVE 999999                             TO W-SPAR-TILEVBSK           
074800                                                                          
074900     MOVE W-IDARTNR                          TO W-IDARTNR-D9              
075000     MOVE WS-IDDC                            TO W-IDDC-D9                 
075100     PERFORM IMS-GU-WDD901                                                
075200     IF SEGMENT-FINNS                                                     
075300       PERFORM IMS-GNP-WDD902                                             
075400       PERFORM UNTIL SEGMENT-SAKNAS                                       
075500         MOVE IDLEVNR                        TO W-IDLEVNR                 
075600         PERFORM IMS-GNP-WDD924                                           
075700         IF SEGMENT-FINNS                                                 
075800            MOVE LEV-TILEVBSK-INL            TO TMP1-YYMMDD               
075900            MOVE W-SPAR-TILEVBSK             TO TMP2-YYMMDD               
076000            PERFORM WY2000P1                                              
076100            IF TMP1-YYMMDD < TMP2-YYMMDD AND                              
076200               LEV-FLSENLEV = NEJ                                         
076300                MOVE LEV-TILEVBSK-INL        TO W-SPAR-TILEVBSK           
076400            END-IF                                                        
076500         END-IF                                                           
076600         PERFORM IMS-GNP-WDD902                                           
076700       END-PERFORM                                                        
076800     ELSE                                                                 
076900       MOVE ZERO                        TO DAP-MAIL-LINE-VALUE-WW         
077000                                           DAP-MAIL-LINE-VALUE-D          
077100     END-IF                                                               
077200                                                                          
077300     IF W-SPAR-TILEVBSK < 999999                                          
077400         MOVE W-SPAR-TILEVBSK                TO DAT-I-TIDATUM             
077500         MOVE 'AAMMDD'                       TO DAT-KDDATFORM             
077600         CALL WDATKONV USING DAT-KDDATFORM,                               
077700                             DAT-I-TIDATUM,                               
077800                             DAT-O-TIDATUM,                               
077900                             DAT-KDSVAR                                   
078000         IF DAT-KDSVAR-OK                                                 
078100            MOVE DAT-TIAAVVD(3:2)       TO DAP-MAIL-LINE-VALUE-WW         
078200            MOVE DAT-TIAAVVD(5:1)       TO DAP-MAIL-LINE-VALUE-D          
078300         ELSE                                                             
078400            MOVE ZERO                   TO DAP-MAIL-LINE-VALUE-WW         
078500                                           DAP-MAIL-LINE-VALUE-D          
078600         END-IF                                                           
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000 E-SKAPA-TILLTPO-POST SECTION.                                            
079100                                                                          
079200     MOVE W41402-TILLTPO-AREA    TO W41403-TILLTPO-AREA                   
079300                                                                          
079400     MOVE WS-ART-IDFKNGRP        TO W41403-TILLTPO-IDFKNGRP               
079500     MOVE ZERO                   TO W41403-TILLTPO-PRARTBTO-EXP           
079600                                                                          
079700     PERFORM S14-SKRIV-W41403-TILLTPO                                     
079800     .                                                                    
079900     EJECT                                                                
080000 F-SKAPA-ANNVOR-POST SECTION.                                             
080100                                                                          
080200     MOVE W41402-ANNVOR-AREA     TO W41403-ANNVOR-AREA                    
080300                                                                          
080400     MOVE WS-ART-KDPRODSL        TO W41403-ANNVOR-KDPRODSL                
080500                                                                          
080600     PERFORM S15-SKRIV-W41403-ANNVOR                                      
080700     .                                                                    
080800     EJECT                                                                
080900 Z-FINIT SECTION.                                                         
081000     CLOSE W41402                                                         
081100           W41403                                                         
081200           W41403A                                                        
081300     SKIP2                                                                
081400     MOVE 'S' TO POSTSUM-OPKOD                                            
081500     CALL POSTSUM USING POSTSUM-PARM                                      
081600     .                                                                    
081700     EJECT                                                                
081800 S01-LAES-W41402  SECTION.                                                
081900     SKIP2                                                                
082000     READ W41402 INTO IN-AREA                                             
082100     AT END                                                               
082200        SET END-OF-W41402 TO TRUE                                         
082300                                                                          
082400     NOT AT END                                                           
082500        MOVE 'W41402' TO POSTSUM-FDNAMN                                   
082600        MOVE 'W41402D1' TO POSTSUM-DDNAMN2                                
082700        MOVE W41402-IDPTYP TO POSTSUM-TRANSTYP                            
082800        CALL POSTSUM USING POSTSUM-PARM                                   
082900     END-READ                                                             
083000     .                                                                    
083100     EJECT                                                                
083200 S02-LAST-ARTNR SECTION.                                                  
083300     SKIP2                                                                
083400     IF END-OF-W41402                                                     
083500        CONTINUE                                                          
083600     ELSE                                                                 
083700        IF W41402-IDARTNR-TILLK-S > ZERO                                  
083800           MOVE W41402-IDARTNR-TILLK-S   TO W-LAST-ARTNR                  
083900        ELSE                                                              
084000           MOVE W41402-IDARTNR-S         TO W-LAST-ARTNR                  
084100        END-IF                                                            
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 S12-SKRIV-W41403-ORAD SECTION.                                           
084600     SKIP2                                                                
084700     WRITE W41403-ORAD-POST FROM W41403-ORAD-AREA                         
084800                                                                          
084900     MOVE W41403-ORAD-IDPTYP TO POSTSUM-TRANSTYP                          
085000     MOVE 'W41403' TO POSTSUM-FDNAMN                                      
085100     MOVE 'W41402D2' TO POSTSUM-DDNAMN2                                   
085200     CALL POSTSUM USING POSTSUM-PARM                                      
085300     .                                                                    
085400     EJECT                                                                
085500 S13-SKRIV-W41403-OBKR SECTION.                                           
085600     SKIP2                                                                
085700     WRITE W41403-OBKR-POST FROM W41403-OBKR-AREA                         
085800                                                                          
085900     MOVE W41403-OBKR-IDPTYP TO POSTSUM-TRANSTYP                          
086000     MOVE 'W41403' TO POSTSUM-FDNAMN                                      
086100     MOVE 'W41402D2' TO POSTSUM-DDNAMN2                                   
086200     CALL POSTSUM USING POSTSUM-PARM                                      
086300     .                                                                    
086400     EJECT                                                                
086500 S14-SKRIV-W41403-TILLTPO SECTION.                                        
086600                                                                          
086700     WRITE W41403-TILLTPO-POST FROM W41403-TILLTPO-AREA                   
086800                                                                          
086900     MOVE W41403-TILLTPO-IDPTYP TO POSTSUM-TRANSTYP                       
087000     MOVE 'W41403' TO POSTSUM-FDNAMN                                      
087100     MOVE 'W41402D2' TO POSTSUM-DDNAMN2                                   
087200     CALL POSTSUM USING POSTSUM-PARM                                      
087300     .                                                                    
087400     EJECT                                                                
087500 S15-SKRIV-W41403-ANNVOR SECTION.                                         
087600                                                                          
087700     WRITE W41403-ANNVOR-POST FROM W41403-ANNVOR-AREA                     
087800                                                                          
087900     MOVE W41403-ANNVOR-IDPTYP TO POSTSUM-TRANSTYP                        
088000     MOVE 'W41403' TO POSTSUM-FDNAMN                                      
088100     MOVE 'W41402D2' TO POSTSUM-DDNAMN2                                   
088200     CALL POSTSUM USING POSTSUM-PARM                                      
088300     .                                                                    
088400     EJECT                                                                
088500 S90-WRITE-DAP-LINE1 SECTION.                                             
088600                                                                          
088700     MOVE ' ¤DAPW41402'               TO W001-DAP                         
088800     WRITE PASSIVE-PART             FROM W001-DAP                         
088900                                                                          
089000     MOVE SPACE                       TO W001-DAP                         
089100     .                                                                    
089200     EJECT                                                                
089300 S90-WRITE-DAP-LINE2 SECTION.                                             
089400                                                                          
089500     STRING ' ¤DAP' WS-DAP-SUBTYPE                                        
089600            DELIMITED BY SIZE INTO W001-DAP                               
089700     WRITE PASSIVE-PART             FROM W001-DAP                         
089800                                                                          
089900     MOVE SPACE                       TO W001-DAP                         
090000     .                                                                    
090100     .                                                                    
090200     EJECT                                                                
090300* --- IMS SEKTIONER ---                                                   
090400 IMS-GU-WDK601 SECTION.                                                   
090500                                                                          
090600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090700          DELIMITED BY SIZE INTO SSA1                                     
090800     MOVE '  GE' TO GODK-STATUSKODER                                      
090900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
091000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091100     PERFORM IMS-STATUSKONTROLL                                           
091200     .                                                                    
091300     SKIP3                                                                
091400 IMS-GNP-WDK611 SECTION.                                                  
091500                                                                          
091600     MOVE 'WDK611   ' TO SSA1                                             
091700     MOVE '  GE' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
091900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300 IMS-GU-WDK712                 SECTION.                                   
092400                                                                          
092500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
092600     DELIMITED BY SIZE INTO SSA1                                          
092700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
092800     DELIMITED BY SIZE INTO SSA2                                          
092900     MOVE '    ' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
093100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GU-WDB201 SECTION.                                                   
093600                                                                          
093700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
093800            DELIMITED BY SIZE INTO SSA1                                   
093900                                                                          
094000     MOVE '  GE'                   TO GODK-STATUSKODER                    
094100     CALL CBLTDLI         USING GU WDB2-PCB DLI-IO-WDB201 SSA1            
094200     MOVE WDB2-STATUS-CODE         TO STATUS-WS                           
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     EJECT                                                                
094600 IMS-GU-WDD901 SECTION.                                                   
094700                                                                          
094800     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
094900              DELIMITED BY SIZE INTO SSA1                                 
095000     MOVE '  GE'                TO GODK-STATUSKODER                       
095100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
095200     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     EJECT                                                                
095600 IMS-GNP-WDD902 SECTION.                                                  
095700                                                                          
095800     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
095900              DELIMITED BY SIZE INTO SSA1                                 
096000     MOVE 'WDD902  '            TO SSA2                                   
096100     MOVE '  GE'                TO GODK-STATUSKODER                       
096200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
096300     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600     EJECT                                                                
096700 IMS-GNP-WDD924 SECTION.                                                  
096800                                                                          
096900     STRING 'WDD902  (IDLEVNR = ' W-IDLEVNR-X ')'                         
097000              DELIMITED BY SIZE INTO SSA1                                 
097100     MOVE 'WDD924  '            TO SSA2                                   
097200     MOVE '  GE'                TO GODK-STATUSKODER                       
097300     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1 SSA2              
097400     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
097500     PERFORM IMS-STATUSKONTROLL                                           
097600     .                                                                    
097700     EJECT                                                                
097800 IMS-STATUSKONTROLL SECTION.                                              
097900                                                                          
098000     SET STATUS-IX TO 1                                                   
098100     SEARCH GODK-STATUS                                                   
098200       AT END                                                             
098300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
098400           DELIMITED BY SIZE INTO FELTEXT-STR                             
098500         DISPLAY FELTEXT                                                  
098600         CALL FELLOG                                                      
098700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
098800         CONTINUE                                                         
098900     END-SEARCH                                                           
099000     .                                                                    
099100*    -COPY WY2000P1                                                       
