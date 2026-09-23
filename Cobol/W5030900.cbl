000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5030900.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   AUG   99.                                                
000500                                                                          
000600*    FUNKTION.                                                            
000700*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000800*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0179               
000900*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001000*        INVENTERING                                                      
001100*        PROGRAMMET HAR TVÅ FUNKTIONER                                    
001200*        - TITTA PÅ INVENTERINGSKÖN                                       
001300*        - VÄLJA VILKA ARTIKLAR MAN VILL HA UT PÅ INVENTERINGS-           
001400*          UNDERLAG                                                       
001500*          TRANSAR MED ARTIKELNR SKICKAS TILL W5030100                    
001600*          SOM TAR HAND OM UTSKRIFTEN AV INVENTERINGSUNDERLAGEN.          
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W5T309                                              
002100*        MID:         W5I30901                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W5O30901                                            
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -COPY WY2000W1                                                       
003300     SKIP3                                                                
003400 77  IDPGM                     PIC X(8)   VALUE 'W5030900'.               
003500 77  WS-SECTION                PIC X(32).                                 
003600 77  WS-IMS                    PIC X(32).                                 
003700 77  JA                        PIC X      VALUE 'J'.                      
003800 77  NEJ                       PIC X      VALUE 'N'.                      
003900 77  KONTROLL-OK               PIC X      VALUE 'N'.                      
004000 77  SPRAK-IX                  PIC S9(9)  VALUE +0   COMP SYNC.           
004100 77  IX                        PIC S9(9)  VALUE +0   COMP SYNC.           
004200 77  INDX                      PIC S9(9)  VALUE +0   COMP SYNC.           
004300 77  IND                       PIC S9(9)  VALUE +0   COMP SYNC.           
004400 77  ALT-IND                   PIC S9(9)  VALUE +0   COMP SYNC.           
004500 77  MAX-IX                    PIC S9(9)  VALUE +10  COMP SYNC.           
004600 77  MAX-INDX-PLUS-1           PIC S9(9)  VALUE +8   COMP SYNC.           
004700 77  ADD-KVINVSKR-PRINT        PIC S9(9)  VALUE +0   COMP SYNC.           
004800 77  RAKNARE                   PIC  9(3)  VALUE ZERO.                     
004900                                                                          
005000 77  ALLT-OK-SW                 PIC X         VALUE 'J'.                  
005100      88  ALLT-OK                             VALUE 'J'.                  
005200      88  ALLT-FEL                            VALUE 'N'.                  
005300                                                                          
005400 77  FLINVSKR-SW                PIC X         VALUE 'J'.                  
005500      88  FLINVSKR-Y                          VALUE 'J'.                  
005600      88  FLINVSKR-N                          VALUE 'N'.                  
005700                                                                          
005800 77  FIRST-SW                  PIC X         VALUE 'J'.                   
005900      88  FIRST-TIME                         VALUE 'J'.                   
006000      88  LAST-TIME                          VALUE 'N'.                   
006100                                                                          
006200 77  INDATA-SW                 PIC X         VALUE 'J'.                   
006300      88  INDATA-OK                          VALUE 'J'.                   
006400      88  INDATA-FEL                         VALUE 'N'.                   
006500                                                                          
006600 77  NYCKLAR-SW                PIC X         VALUE 'J'.                   
006700      88  NYCKLAR-OK                         VALUE 'J'.                   
006800      88  NYCKLAR-FEL                        VALUE 'N'.                   
006900                                                                          
007000 77  POST-SW                   PIC X         VALUE 'J'.                   
007100      88  POST-FINNS                         VALUE 'J'.                   
007200      88  POST-SAKNAS                        VALUE 'N'.                   
007300                                                                          
007400 77  FORTSATT-SW               PIC X         VALUE 'J'.                   
007500      88  FORTSATT                           VALUE 'J'.                   
007600      88  FORTSATT-NO                        VALUE 'N'.                   
007700                                                                          
007800 77  W-IDTRANS                 PIC X(4)   VALUE SPACE.                    
007900      88   EGEN-MID                       VALUE '5309'.                   
008000      88   GODK-MID                       VALUE '5301' '5302'             
008100                                                '5303' '5304'             
008200                                                '5305' '5306'             
008300                                                '5307' '5308'             
008400                                                '5309'.                   
008500                                                                          
008600                                                                          
008700*   --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS             
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'SAVE-AREA'.           
009000 01  SAVE-AREA.                                                           
009100   03  SAVE-IDTRANS              PIC X(8)    VALUE '5309'.                
009200   03  SAVE-IDDC-ENTER           PIC X(2).                                
009300   03  SAVE-ADLAGOMR-ENTER       PIC S9(3)     COMP-3.                    
009400   03  SAVE-ADGANG-ENTER         PIC S9(3)     COMP-3.                    
009500   03  SAVE-ADPLATS-ENTER        PIC S9(5)     COMP-3.                    
009600   03  SAVE-KDINVPRIO-ENTER      PIC S9        COMP-3.                    
009700   03  SAVE-KDVVKL-ENTER         PIC S9        COMP-3.                    
009800   03  SAVE-IDARTNR-ENTER        PIC S9(9)     COMP-3.                    
009900   03  SAVE-KDINVKAT-ENTER       PIC S9(3)     COMP-3.                    
010000   03  SAVE-IDPRTOMG-ENTER       PIC S9        COMP-3.                    
010100   03  SAVE-IDLOPNR-ENTER        PIC S9(5)     COMP-3.                    
010200   03  SAVE-FLINVSKR             PIC X.                                   
010300   03  SAVE-RAKNARE              PIC 9(3).                                
010400                                                                          
010500   03  SAVE-IDDC-NEXT            PIC X(2)  VALUE SPACE.                   
010600   03  SAVE-ADLAGOMR-NEXT        PIC S9(3) VALUE ZERO COMP-3.             
010700   03  SAVE-ADGANG-NEXT          PIC S9(3) VALUE ZERO COMP-3.             
010800   03  SAVE-ADPLATS-NEXT         PIC S9(5) VALUE ZERO COMP-3.             
010900   03  SAVE-KDINVPRIO-NEXT       PIC S9    VALUE ZERO COMP-3.             
011000   03  SAVE-KDVVKL-NEXT          PIC S9    VALUE ZERO COMP-3.             
011100   03  SAVE-IDARTNR-NEXT         PIC S9(9) VALUE ZERO COMP-3.             
011200   03  SAVE-KDINVKAT-NEXT        PIC S9(3) VALUE ZERO COMP-3.             
011300   03  SAVE-IDPRTOMG-NEXT        PIC S9    VALUE ZERO COMP-3.             
011400   03  SAVE-IDLOPNR-NEXT         PIC S9(5) VALUE ZERO COMP-3.             
011500                                                                          
011600                                                                          
011700 01  GENERELLA-SUBPROGRAM.                                                
011800   03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.               
011900   03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.               
012000   03  WMEDKONV                PIC X(8)   VALUE 'WMEDKONV'.               
012100   03  W005INIT                PIC X(8)   VALUE 'W005INIT'.               
012200*    --- VALID IDDC CODES                                                 
012300*01 -COPY WWDC99                                                          
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012600*01 -COPY WMSGINIT                                                        
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012900 01 -COPY WMEDAREA                                                        
013000                                                                          
013100 01  MESSAGE-CODES.                                                       
013200     03   ERR-CORR-HILITE-FLDS      PIC X(3)    VALUE '001'.              
013300     03   ERR-PRESS-PF11            PIC X(3)    VALUE '168'.              
013400     03   ERR-PF11-AND-NO-DATA      PIC X(3)    VALUE '011'.              
013500     03   INF-UPDATE-DONE           PIC X(3)    VALUE '101'.              
013600     03   INF-FIRST-PAGE            PIC X(3)    VALUE '006'.              
013700     03   INF-LAST-PAGE             PIC X(3)    VALUE '106'.              
013800     03   ERR-WRONG-KEY             PIC X(3)    VALUE '401'.              
013900     03   ERR-NO-UPDATE             PIC X(3)    VALUE '007'.              
014000     03   ERR-PART-MISSING          PIC X(3)    VALUE '017'.              
014100     03   INF-MORE-INFO-PF8         PIC X(3)    VALUE '105'.              
014200     03   INF-PRINTING-REQ          PIC X(3)    VALUE '118'.              
014300     03   INF-NO-PRINTING           PIC X(3)    VALUE '167'.              
014400                                                                          
014500 01  SW-SIDA-FULL                    PIC X(1).                            
014600 01  WS-IDARTNR                      PIC X(9) VALUE ZERO.                 
014700 01  WS-FLINVSKR                     PIC X(1) VALUE 'N'.                  
014800 01  WS-VARIABEL                     PIC 9.                               
014900 01  WS-IDARTNR-REDIG                                PIC X(9).            
015000 01  WS-KDINVKAT                                     PIC X(2).            
015100 01  KEY-KDINVKAT REDEFINES WS-KDINVKAT              PIC 9(2).            
015200 01  WS-ADLAGOMR                                     PIC X(2).            
015300 01  KEY-ADLAGOMR REDEFINES WS-ADLAGOMR              PIC 9(2).            
015400 01  WS-ADGANG                                       PIC X(2).            
015500 01  KEY-ADGANG REDEFINES WS-ADGANG                  PIC 9(2).            
015600 01  WS-ADPLATS                                      PIC X(5).            
015700 01  KEY-ADPLATS REDEFINES WS-ADPLATS                PIC 9(5).            
015800 01  WS-KDVVKL                                       PIC X.               
015900 01  KEY-KDVVKL REDEFINES WS-KDVVKL                  PIC 9.               
016000 01  WS-KDINVPRIO                                    PIC X(1).            
016100 01  KEY-KDINVPRIO REDEFINES WS-KDINVPRIO            PIC 9.               
016200 01  WS-KDPRODSL                                     PIC X(2).            
016300 01  KEY-KDPRODSL REDEFINES WS-KDPRODSL              PIC 9(2).            
016400 01  WS-IDFKNGRP                                     PIC X(4).            
016500 01  KEY-IDFKNGRP REDEFINES WS-IDFKNGRP              PIC 9(4).            
016600 01  WS-TIREGDAT                                     PIC X(6).            
016700 01  KEY-TIREGDAT REDEFINES WS-TIREGDAT              PIC 9(6).            
016800 01  WS-KVINVSKR-PRINT                               PIC 9(2).            
016900                                                                          
017000 01  WS-FIX-DATUM.                                                        
017100     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
017200     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
017300         05 WS-FILLER1-1-2   PIC 9(2).                                    
017400         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
017500         05 WS-FILLER1-9     PIC 9(1).                                    
017600     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
017700         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
017800         05 WS-FILLER2-9     PIC 9(1).                                    
017900     EJECT                                                                
018000 01  WS-INV-DAREGDAT.                                                     
018100     03  WS-FIX-DAREGDAT      PIC 9(9).                                   
018200     03  WS-FILLER REDEFINES WS-FIX-DAREGDAT.                             
018300       05  WS-FILLER-AAR      PIC 9(3).                                   
018400       05  WS-DAREGDAT-AAMMDD PIC 9(6).                                   
018500                                                                          
018600                                                                          
018700 01  WS-KDINVKAT-REDIG                               PIC X(2).            
018800 01  WS-KDINVKAT-NUM                                 PIC 9(3).            
018900 01  WS-ADLAGOMR-REDIG                               PIC X(3).            
019000 01  WS-ADLAGOMR-NUM                                 PIC 9(3).            
019100 01  WS-IDARTNR-SPAR             PIC 9(9).                                
019200 01  WS-LISTNR.                                                           
019300     03 WS-IDPRTOMG-MOD          PIC 9.                                   
019400     03 WS-IDLOPNR-MOD           PIC 9(5).                                
019500                                                                          
019600 01  WS-IDPRTOMG                 PIC 9       VALUE ZERO.                  
019700 01  WS-IDLOPNR                  PIC 9(5)    VALUE ZERO.                  
019800                                                                          
019900 01  FILLER                      PIC X(11)   VALUE 'W-WDH11-KEY'.         
020000 01    NYCKLAR-TILL-DLI.                                                  
020100   03    W-IDARTNR-X.                                                     
020200     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
020300                                                                          
020400   03    W-WDH111KY-X.                                                    
020500     05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.                 
020600     05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.           
020700     05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.           
020800     05    W-DAREGDAT-SORT-UNIK PIC 9(8)     VALUE ZERO.                  
020900                                                                          
021000   03    W-WDH11-KEY-MIN-X.                                               
021100     05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                       
021200     05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.                 
021300     05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.                 
021400     05    W-DAREGDAT-SORT-MIN    PIC 9(8)   VALUE ZERO.                  
021500                                                                          
021600   03    W-WDH11-KEY-MAX-X.                                               
021700     05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                       
021800     05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.           
021900     05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.           
022000     05    W-DAREGDAT-SORT-MAX    PIC 9(8)   VALUE 99999999.              
022100                                                                          
022200   03    W-KDINVPRIO-X.                                                   
022300     05    W-KDINVPRIO           PIC S9      VALUE ZERO  COMP-3.          
022400                                                                          
022500   03    W-ARTC-IDARTNR-X.                                                
022600     05    W-IDARTNR-ARTC        PIC S9(9)   VALUE ZERO  COMP-3.          
022700                                                                          
022800   03    W-WDH1A1KY-MIN-X.                                                
022900     05    W-SEQA-IDDC-MIN       PIC X(2)    VALUE SPACE.                 
023000     05    W-SEQA-ADLAGOMR-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
023100     05    W-SEQA-ADGANG-MIN     PIC S9(3)   VALUE ZERO  COMP-3.          
023200     05    W-SEQA-ADPLATS-MIN    PIC S9(5)   VALUE ZERO  COMP-3.          
023300     05    W-SEQA-KDINVPRIO-MIN  PIC S9      VALUE ZERO  COMP-3.          
023400     05    W-SEQA-KDVVKL-MIN     PIC S9      VALUE ZERO  COMP-3.          
023500     05    W-SEQA-IDARTNR-MIN    PIC S9(9)   VALUE ZERO  COMP-3.          
023600     05    W-SEQA-KDINVKAT-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
023700     05    W-SEQA-TISEGKEY-MIN   PIC S9(9)   VALUE ZERO  COMP-3.          
023800                                                                          
023900   03    W-WDH1A1KY-MAX-X.                                                
024000     05    W-SEQA-IDDC-MAX      PIC X(2)  VALUE SPACE.                    
024100     05    W-SEQA-ADLAGOMR-MAX  PIC S9(3) VALUE ZERO       COMP-3.        
024200     05    W-SEQA-ADGANG-MAX    PIC S9(3) VALUE ZERO       COMP-3.        
024300     05    W-SEQA-ADPLATS-MAX   PIC S9(5) VALUE ZERO       COMP-3.        
024400     05    W-SEQA-KDINVPRIO-MAX PIC S9    VALUE ZERO       COMP-3.        
024500     05    W-SEQA-KDVVKL-MAX    PIC S9    VALUE ZERO       COMP-3.        
024600     05    W-SEQA-IDARTNR-MAX   PIC S9(9) VALUE ZERO       COMP-3.        
024700     05    W-SEQA-KDINVKAT-MAX  PIC S9(3) VALUE ZERO       COMP-3.        
024800     05    W-SEQA-TISEGKEY-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
024900   03    W-WDH1BSEQ-MIN-X.                                                
025000     05    W-BSEQ-IDDC-MIN      PIC X(2)  VALUE SPACE.                    
025100     05    W-BSEQ-TISEGKEY      PIC S9(9) VALUE ZERO       COMP-3.        
025200     05    W-BSEQ-ADLAGOMR-MIN  PIC S9(3) VALUE ZERO       COMP-3.        
025300     05    W-BSEQ-ADGANG-MIN    PIC S9(3) VALUE ZERO       COMP-3.        
025400     05    W-BSEQ-ADPLATS-MIN   PIC S9(5) VALUE ZERO       COMP-3.        
025500     05    W-BSEQ-IDPRTOMG-MIN  PIC S9    VALUE ZERO       COMP-3.        
025600     05    W-BSEQ-IDLOPNR-MIN   PIC S9(5) VALUE ZERO       COMP-3.        
025700   03    W-WDH1BSEQ-MAX-X.                                                
025800     05    W-BSEQ-IDDC-MAX      PIC X(2)  VALUE SPACE.                    
025900     05    W-BSEQ-TISEGKEY      PIC S9(9) VALUE +999999999 COMP-3.        
026000     05    W-BSEQ-ADLAGOMR-MAX  PIC S9(3) VALUE +999       COMP-3.        
026100     05    W-BSEQ-ADGANG-MAX    PIC S9(3) VALUE +999       COMP-3.        
026200     05    W-BSEQ-ADPLATS-MAX   PIC S9(5) VALUE +99999     COMP-3.        
026300     05    W-BSEQ-IDPRTOMG-MAX  PIC S9    VALUE +3         COMP-3.        
026400     05    W-BSEQ-IDLOPNR-MAX   PIC S9(5) VALUE +99999     COMP-3.        
026500***************************************************************           
026600*                                                                         
026700*              AREOR FÖR MFS OCH SKÄRMHANTERING                           
026800*                                                                         
026900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
027000     SKIP3                                                                
027100*01  MID -COPY W5I30901                                                   
027200     EJECT                                                                
027300* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
027400*01    -COPY WMSGAREA                                                     
027500     EJECT                                                                
027600*  03  MOD -COPY W5O30901  -RED MSG-AREA.                                 
027700     EJECT                                                                
027800*01    -COPY WMFSAREA                                                     
027900     EJECT                                                                
028000* - - - - - - - - - - - - - - - - - - - - ALT-AREA                        
028100 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
028200 01  W-PROG-TO-PROG-SW.                                                   
028300   03  M-SW-LL                   PIC S9(4)   VALUE  ZERO                  
028400                                             COMP SYNC.                   
028500   03  M-SW-Z1-Z2                PIC X(2)    VALUE LOW-VALUE.             
028600   03  M-SW-KDTRANS              PIC X(8)    VALUE 'W5T301X '.            
028700   03  M-SW-IDTRANS              PIC X(4)    VALUE '530B'.                
028800   03  M-SW-KDMFSTYP             PIC X(1).                                
028900*  03  -COPY W5I30101                                                     
029000     EJECT                                                                
029100* **************************************************************          
029200*                                                                         
029300*                  ARBETS-AREOR TILL IMS-SEKTIONERNA                      
029400*                                                                         
029500 01  IMS-WS.                                                              
029600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS'.              
029700     SKIP3                                                                
029800*                        **** STATUS-KOD FRÅN IMS                         
029900   03  STATUS-WS                 PIC XX.                                  
030000     88  SEGMENT-FINNS                       VALUE '  '.                  
030100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030200     SKIP3                                                                
030300   03    GODK-STATUSKODER.                                                
030400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030500     SKIP3                                                                
030600 01    SSA1                      PIC X(256).                              
030700 01    SSA2                      PIC X(128).                              
030800     EJECT                                                                
030900*                            IMS FUNKTIONSKODER                           
031000*01    -COPY W0003                                                        
031100     EJECT                                                                
031200 01  FILLER                    PIC X(16)   VALUE 'IMS-AREA1'.             
031300*                            DLI INPUT-OUTPUT AREA                        
031400 01    DLI-IO-AREA.                                                       
031500   03    IO-AREA                 PIC X(200)  VALUE SPACE.                 
031600     SKIP3                                                                
031700*  03    WDH101 -COPY WDH101                 -RED IO-AREA.                
031800     EJECT                                                                
031900*  03    WDH111 -COPY WDH111                 -RED IO-AREA.                
032000     EJECT                                                                
032100*                            DLI INPUT-OUTPUT AREA 2                      
032200 01    DLI-IO-AREA3.                                                      
032300   03    IO-AREA3                PIC X(100)  VALUE SPACE.                 
032400     SKIP3                                                                
032500*  03    WLINVB01 -COPY WDH1A1               -RED IO-AREA3.               
032600     EJECT                                                                
032700*                            DLI INPUT-OUTPUT AREA 2                      
032800 01  FILLER                    PIC X(16)   VALUE 'IMS-AREA2'.             
032900 01    DLI-IO-AREA2.                                                      
033000   03    IO-AREA2                PIC X(900)  VALUE SPACE.                 
033100*  03    WLARTC01 -COPY WDK601 -PRE ARTC01- -RED IO-AREA2.                
033200     EJECT                                                                
033300*  03    WLARTC11 -COPY WDK611 -PRE ARTC11- -RED IO-AREA2.                
033400     EJECT                                                                
033500                                                                          
033600 01   FILLER                      PIC X(16) VALUE 'DLI-IO-WDH1B1'.        
033700 01   DLI-IO-WDH1B1.                                                      
033800*   03 -COPY WDH111 -PRE BSEQ-                                            
033900     EJECT                                                                
034000 LINKAGE SECTION.                                                         
034100*01    -COPY W0009     -PRE MSG-                                          
034200     EJECT                                                                
034300*01    -COPY W0009     -PRE ALT-                                          
034400     EJECT                                                                
034500*01    -COPY W0008     -PRE USEA-                                         
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01    -COPY W0008     -PRE INVB-                                         
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01    -COPY W0008     -PRE INVA-                                         
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01    -COPY W0008     -PRE ARTC-                                         
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01    -COPY W0008     -PRE WDH1B-                                        
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01    -COPY W0008     -PRE ALT2-                                         
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300                                                                          
036400 PROCEDURE DIVISION USING MSG-PCB ALT-PCB USEA-PCB INVB-PCB               
036500                          INVA-PCB ARTC-PCB WDH1B-PCB ALT2-PCB.           
036600 MAIN SECTION.                                                            
036700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB INVB-PCB              
036800                          INVA-PCB ARTC-PCB WDH1B-PCB ALT2-PCB.           
036900                                                                          
037000     PERFORM IMS-GET-MSG                                                  
037100     IF SEGMENT-FINNS                                                     
037200        PERFORM A-INIT-SPARA-INPUT                                        
037300        PERFORM B-KOLLA-NYCKLAR                                           
037400        IF NYCKLAR-FEL                                                    
037500           MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                          
037600           CALL WMEDKONV         USING MED-WMEDAREA                       
037700           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
037800           PERFORM MFS-RENSA                                              
037900           MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                           
038000        ELSE                                                              
038100           IF MFS-UPDATE                                                  
038200              PERFORM C-KOLLA-INDATA-UPPDATERING                          
038300              IF INDATA-OK                                                
038400                 PERFORM D-SKICKA-ART-TILL-UTSKRIFT                       
038500                  IF MID-KVINVSKR-5309 > ZERO AND < +11                   
038600                     PERFORM G-LAES-NAESTA                                
038700                     PERFORM S02-LAES-VISA-KOE2                           
038800                     IF SW-SIDA-FULL = JA                                 
038900                       MOVE ERR-PRESS-PF11 TO MED-IDMFSFEL                
039000                       CALL WMEDKONV    USING MED-WMEDAREA                
039100                       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                
039200                     END-IF                                               
039300                  ELSE                                                    
039400                     PERFORM MFS-RENSA                                    
039500                  END-IF                                                  
039600              END-IF                                                      
039700           ELSE                                                           
039800             IF MFS-FIRST                                                 
039900               PERFORM F-LAES-FOERSTA                                     
040000             ELSE                                                         
040100                IF MFS-NEXT                                               
040200                   PERFORM G-LAES-NAESTA                                  
040300                ELSE                                                      
040400                                                                          
040500                   PERFORM H-LAES-SAMMA                                   
040600                   IF MID-KVINVSKR-5309 NUMERIC AND                       
040700                      (WS-FLINVSKR = 'N')                                 
040800                      IF MID-KVINVSKR-5309 > ZERO AND < +11               
040900                         MOVE ERR-PRESS-PF11 TO MED-IDMFSFEL              
041000                         CALL WMEDKONV    USING MED-WMEDAREA              
041100                         MOVE MED-MFSFEL     TO MOD-TEMFSFEL              
041200                      END-IF                                              
041300                   END-IF                                                 
041400                END-IF                                                    
041500             END-IF                                                       
041600             PERFORM S02-LAES-VISA-KOE2                                   
041700           END-IF                                                         
041800        END-IF                                                            
041900        COMPUTE MSG-KVLL = LENGTH OF MOD-W5O30901 + 4                     
042000        PERFORM IMS-INSERT-MSG                                            
042100     END-IF                                                               
042200     MOVE ZERO TO RETURN-CODE                                             
042300     GOBACK                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 A-INIT-SPARA-INPUT SECTION.                                              
042700                                                                          
042800     IF MSG-DUBBLA-TRANSKODER                                             
042900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30901                 
043000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
043100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
043200     ELSE                                                                 
043300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30901                  
043400       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
043500       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
043600     END-IF                                                               
043700                                                                          
043800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
043900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
044000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044100     MOVE LOW-VALUE   TO W-WDH1A1KY-MIN-X                                 
044200     MOVE HIGH-VALUE  TO W-WDH1A1KY-MAX-X                                 
044300                                                                          
044400     MOVE LOW-VALUE TO MSG-AREA                                           
044500     MOVE 'W5O309N1' TO MFS-IDMOD                                         
044600     MOVE '5309' TO MOD-IDTRANS                                           
044700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
044800                                                                          
044900     IF NOT EGEN-MID                                                      
045000       MOVE SPACE TO MFS-KDTRTYP                                          
045100       MOVE '7' TO MFS-IDPFK                                              
045200     END-IF                                                               
045300                                                                          
045400     IF SWEDISH-TEXT                                                      
045500       MOVE +1 TO M-SW-KDMFSTYP                                           
045600       MOVE +1 TO SPRAK-IX                                                
045700       MOVE 'S  ' TO MED-IDSKYLT                                          
045800     ELSE                                                                 
045900       MOVE +2 TO M-SW-KDMFSTYP                                           
046000       MOVE +2 TO SPRAK-IX                                                
046100       MOVE 'GB ' TO MED-IDSKYLT                                          
046200     END-IF                                                               
046300     MOVE NEJ TO FLINVSKR-SW                                              
046400     .                                                                    
046500 B-KOLLA-NYCKLAR SECTION.                                                 
046600     SKIP2                                                                
046700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046800     MOVE '001'             TO MSGI-KDCALL                                
046900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
047000     MOVE '5309'            TO MSGI-IDTRANS                               
047100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
047200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
047400     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
047500     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
047600                                                                          
047700     MOVE JA TO NYCKLAR-SW                                                
047800                                                                          
047900*     -- KONTROLL AV IDDC                                                 
048000      MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                 
048100                                                                          
048200      MOVE MSGI-IDDC     TO WS-IDDC                                       
048300                            W-IDDC-UNIK                                   
048400                            W-IDDC-MIN                                    
048500                            W-IDDC-MAX                                    
048600                            W-SEQA-IDDC-MIN                               
048700                            W-SEQA-IDDC-MAX                               
048800                            W-BSEQ-IDDC-MIN                               
048900                            W-BSEQ-IDDC-MAX                               
049000                                                                          
049100*     -- KONTROLL AV ADLAGOMR                                             
049200      MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                             
049300                                                                          
049400     IF MID-ADLAGOMR-IN = ALL '+'                                         
049500       MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                                
049600       INSPECT WS-ADLAGOMR REPLACING LEADING SPACE BY ZERO                
049700     ELSE                                                                 
049800       MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                                
049900       MOVE '7'             TO MFS-IDPFK                                  
050000       MOVE SPACE           TO MFS-KDTRTYP                                
050100     END-IF                                                               
050200     IF WS-ADLAGOMR NOT NUMERIC                                           
050300       MOVE NEJ TO NYCKLAR-SW                                             
050400     END-IF                                                               
050500                                                                          
050600*     -- KONTROLL AV ADGANG                                               
050700     MOVE MFS-RENSA-FAELT TO MOD-ADGANG-IN                                
050800                                                                          
050900     IF MID-ADGANG-IN = ALL '+'                                           
051000        MOVE MID-ADGANG-UT TO WS-ADGANG                                   
051100        INSPECT WS-ADGANG REPLACING LEADING SPACE BY ZERO                 
051200     ELSE                                                                 
051300        MOVE MID-ADGANG-IN TO WS-ADGANG                                   
051400        MOVE '7'           TO MFS-IDPFK                                   
051500       MOVE SPACE          TO MFS-KDTRTYP                                 
051600     END-IF                                                               
051700     IF WS-ADGANG NOT NUMERIC                                             
051800        MOVE NEJ TO NYCKLAR-SW                                            
051900     END-IF                                                               
052000                                                                          
052100*     -- KONTROLL AV ADPLATS                                              
052200     MOVE MFS-RENSA-FAELT TO MOD-ADPLATS-IN                               
052300                                                                          
052400     IF MID-ADPLATS-IN = ALL '+'                                          
052500        MOVE MID-ADPLATS-UT TO WS-ADPLATS                                 
052600        INSPECT WS-ADPLATS REPLACING LEADING SPACE BY ZERO                
052700     ELSE                                                                 
052800        MOVE MID-ADPLATS-IN TO WS-ADPLATS                                 
052900        MOVE '7'            TO MFS-IDPFK                                  
053000        MOVE SPACE          TO MFS-KDTRTYP                                
053100     END-IF                                                               
053200     IF WS-ADPLATS NOT NUMERIC                                            
053300        MOVE NEJ TO NYCKLAR-SW                                            
053400     END-IF                                                               
053500                                                                          
053600*   -- KONTROLL AV IDPRTOMG                                               
053700                                                                          
053800     MOVE MFS-RENSA-FAELT TO MOD-IDPRTOMG-IN                              
053900     IF MID-IDPRTOMG-IN = ALL '+'                                         
054000*      COMPUTE WS-IDPRTOMG = MID-IDPRTOMG-IN - 1                          
054100       MOVE  MID-IDPRTOMG-UT       TO WS-IDPRTOMG                         
054200                                      MOD-IDPRTOMG-UT                     
054300     ELSE                                                                 
054400       IF MID-IDPRTOMG-IN NUMERIC                                         
054500         IF MID-IDPRTOMG-IN = 1 OR 2 OR 3                                 
054600           COMPUTE W-BSEQ-IDPRTOMG-MIN = MID-IDPRTOMG-IN - 1              
054700           MOVE W-BSEQ-IDPRTOMG-MIN TO W-BSEQ-IDPRTOMG-MAX                
054800           MOVE MID-IDPRTOMG-IN    TO WS-IDPRTOMG                         
054900                                                                          
055000           MOVE '7'                TO MFS-IDPFK                           
055100           MOVE SPACE              TO MFS-KDTRTYP                         
055200         ELSE                                                             
055300           MOVE NEJ                TO NYCKLAR-SW                          
055400           MOVE MFS-RENSA-FAELT    TO MOD-IDPRTOMG-UT                     
055500         END-IF                                                           
055600       ELSE                                                               
055700         MOVE NEJ                  TO NYCKLAR-SW                          
055800         MOVE MFS-RENSA-FAELT      TO MOD-IDPRTOMG-UT                     
055900       END-IF                                                             
056000     END-IF                                                               
056100                                                                          
056200     IF WS-IDPRTOMG NOT NUMERIC                                           
056300        MOVE NEJ                   TO NYCKLAR-SW                          
056400     END-IF                                                               
056500                                                                          
056600*--- KONTROLL AV PRINTKOD FLINVSKR                                        
056700     MOVE MFS-RENSA-FAELT     TO MOD-FLINVSKR-IN                          
056800                                                                          
056900     IF MID-FLINVSKR-IN = '+'                                             
057000       MOVE MID-FLINVSKR-UT   TO WS-FLINVSKR                              
057100       IF WS-FLINVSKR = ' '                                               
057200         MOVE NEJ             TO WS-FLINVSKR                              
057300       ELSE                                                               
057400         IF WS-FLINVSKR = 'N' OR 'J' OR 'Y'                               
057500           CONTINUE                                                       
057600         ELSE                                                             
057700           MOVE NEJ TO NYCKLAR-SW                                         
057800         END-IF                                                           
057900       END-IF                                                             
058000     ELSE                                                                 
058100       IF MID-FLINVSKR-IN = 'J' OR 'N' OR 'Y'                             
058200        IF MID-FLINVSKR-IN = 'Y'                                          
058300         MOVE 'Y'             TO MOD-FLINVSKR-UT                          
058400         MOVE 'J'             TO WS-FLINVSKR                              
058500        ELSE                                                              
058600         MOVE MID-FLINVSKR-IN TO MOD-FLINVSKR-UT                          
058700                                 WS-FLINVSKR                              
058800        END-IF                                                            
058900        MOVE '7'             TO MFS-IDPFK                                 
059000        MOVE SPACE           TO MFS-KDTRTYP                               
059100       ELSE                                                               
059200         MOVE NEJ             TO NYCKLAR-SW                               
059300       END-IF                                                             
059400     END-IF                                                               
059500     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
059600                                                                          
059700     IF EGEN-MID OR NYCKLAR-OK                                            
059800        MOVE WS-ADLAGOMR    TO MOD-ADLAGOMR-UT                            
059900                               W-BSEQ-ADLAGOMR-MIN                        
060000        INSPECT MOD-ADLAGOMR-UT REPLACING LEADING ZERO BY SPACE           
060100        MOVE WS-ADGANG    TO MOD-ADGANG-UT                                
060200                             W-BSEQ-ADGANG-MIN                            
060300        INSPECT MOD-ADGANG-UT REPLACING LEADING ZERO BY SPACE             
060400        MOVE WS-ADPLATS   TO MOD-ADPLATS-UT                               
060500                             W-BSEQ-ADPLATS-MIN                           
060600        INSPECT MOD-ADPLATS-UT REPLACING LEADING ZERO BY SPACE            
060700        MOVE WS-IDPRTOMG  TO MOD-IDPRTOMG-UT                              
060800        INSPECT MOD-IDPRTOMG-UT REPLACING LEADING ZERO BY SPACE           
060900        IF MID-FLINVSKR-IN = 'Y'                                          
061000          MOVE 'Y'            TO MOD-FLINVSKR-UT                          
061100        ELSE                                                              
061200          MOVE WS-FLINVSKR  TO MOD-FLINVSKR-UT                            
061300        END-IF                                                            
061400     ELSE                                                                 
061500        MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UT                           
061600                                MOD-ADGANG-UT                             
061700                                MOD-ADPLATS-UT                            
061800                                MOD-IDPRTOMG-UT                           
061900                                MOD-FLINVSKR-UT                           
062000     END-IF                                                               
062100     IF NOT EGEN-MID                                                      
062200       MOVE '002'         TO MSGI-KDCALL                                  
062300       MOVE '5309'        TO MSGI-IDTRANS                                 
062400       MOVE SPACE TO SAVE-AREA                                            
062500       MOVE SAVE-AREA     TO MSGI-SPAR-AREA                               
062600       CALL W005INIT USING MSGI-WMSGINIT                                  
062700            USEA-PCB                                                      
062800     END-IF                                                               
062900                                                                          
063000     IF ENGLISH-TEXT                                                      
063100       MOVE 'GB ' TO MED-IDSKYLT                                          
063200       MOVE 'N' TO MFS-KDHUVOMR                                           
063300     ELSE                                                                 
063400       MOVE 'SE ' TO MED-IDSKYLT                                          
063500       MOVE '0' TO MFS-KDHUVOMR                                           
063600     END-IF                                                               
063700                                                                          
063800                                                                          
063900     .                                                                    
064000     EJECT                                                                
064100 C-KOLLA-INDATA-UPPDATERING SECTION.                                      
064200     MOVE JA TO INDATA-SW                                                 
064300     MOVE +1 TO IX                                                        
064400     IF MID-KVINVSKR-5309 NUMERIC                                         
064500        IF MID-KVINVSKR-5309 > ZERO AND < +11                             
064600           IF MID-IDARTNR-UTSKR(IX) = ' ' OR                              
064700             (WS-FLINVSKR = 'J')                                          
064800                                                                          
064900              MOVE ERR-NO-UPDATE         TO MED-IDMFSFEL                  
065000              CALL WMEDKONV              USING MED-WMEDAREA               
065100              MOVE MED-MFSFEL            TO MOD-TEMFSFEL                  
065200              MOVE NEJ TO INDATA-SW                                       
065300           END-IF                                                         
065400        ELSE                                                              
065500           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
065600           CALL WMEDKONV              USING MED-WMEDAREA                  
065700           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
065800           MOVE NEJ TO INDATA-SW                                          
065900        END-IF                                                            
066000     ELSE                                                                 
066100        MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                        
066200        CALL WMEDKONV              USING MED-WMEDAREA                     
066300        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
066400        MOVE NEJ TO INDATA-SW                                             
066500     END-IF                                                               
066600                                                                          
066700     IF INDATA-FEL                                                        
066800        MOVE MFS-NUM-FAELT-FEL TO MOD-KVINVSKR-ATTR                       
066900        PERFORM MFS-ROER-EJ-MOD-RAD                                       
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 D-SKICKA-ART-TILL-UTSKRIFT SECTION.                                      
067400                                                                          
067500     MOVE +1 TO IX                                                        
067600     MOVE +0 TO IND                                                       
067700     PERFORM DA-INITIERA-ALT-AREA                                         
067800     MOVE MSGI-IDUSER TO MID-IDUSER                                       
067900     MOVE '01' TO MID-KVINVSKR-PRINT                                      
068000                                                                          
068100     PERFORM UNTIL (IND = MID-KVINVSKR-5309) OR                           
068200                   (IX > MAX-IX)             OR                           
068300                   (MID-IDARTNR-UTSKR(IX) = ' ')                          
068400      PERFORM DC-KOLLA-PRINTKOD-WDH1                                      
068500                                                                          
068600      IF INV-FLINVSKR = 'N'                                               
068700         ADD +1 TO IND                                                    
068800         PERFORM DB-REDIGERA-FLYTTA-ART                                   
068900      END-IF                                                              
069000      ADD +1 TO IX                                                        
069100     END-PERFORM                                                          
069200                                                                          
069300     IF IND > +0                                                          
069400        PERFORM IMS-INSERT-ALT-MSG                                        
069500        PERFORM IMS-PURGE-ALT-MSG                                         
069600        MOVE INF-PRINTING-REQ TO MED-IDMFSINF                             
069700        CALL WMEDKONV USING MED-WMEDAREA                                  
069800        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
069900     ELSE                                                                 
070000        MOVE INF-NO-PRINTING TO MED-IDMFSINF                              
070100        CALL WMEDKONV USING MED-WMEDAREA                                  
070200        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
070300     END-IF                                                               
070400                                                                          
070500     .                                                                    
070600     EJECT                                                                
070700 DA-INITIERA-ALT-AREA SECTION.                                            
070800                                                                          
070900     COMPUTE M-SW-LL = LENGTH OF MID-W5I30101 + 17                        
071000     MOVE ZERO            TO MID-KVINVSKR                                 
071100     MOVE ZERO            TO MID-ADLAGOMR                                 
071200     MOVE WS-IDDC         TO MID-IDDC                                     
071300     MOVE SPACE           TO MID-IDPRTLST                                 
071400     MOVE +1              TO ALT-IND                                      
071500     PERFORM 10 TIMES                                                     
071600        MOVE ALL '+'      TO MID-IDARTNR-PRINT   (ALT-IND)                
071700        MOVE SPACE        TO MID-KDINVPRIO-PRINT (ALT-IND)                
071800        MOVE ZERO         TO MID-KDINVKAT-PRINT (ALT-IND)                 
071900        ADD +1            TO ALT-IND                                      
072000     END-PERFORM                                                          
072100     .                                                                    
072200     EJECT                                                                
072300 DB-REDIGERA-FLYTTA-ART SECTION.                                          
072400                                                                          
072500     MOVE MID-IDARTNR-UTSKR(IX)      TO WS-IDARTNR-REDIG                  
072600     INSPECT WS-IDARTNR-REDIG REPLACING LEADING SPACE BY ZERO             
072700     MOVE WS-IDARTNR-REDIG           TO W-IDARTNR                         
072800                                        MID-IDARTNR-PRINT  (IND)          
072900                                        W-IDARTNR-ARTC                    
073000                                                                          
073100     IF MID-KDINVPRIO-UTSKR(IX) = SPACE                                   
073200        MOVE '2'                     TO MID-KDINVPRIO-PRINT(IND)          
073300     ELSE                                                                 
073400        MOVE MID-KDINVPRIO-UTSKR(IX) TO MID-KDINVPRIO-PRINT(IND)          
073500     END-IF                                                               
073600     MOVE MID-KDINVKAT-UTSKR(IX)     TO WS-KDINVKAT-REDIG                 
073700     INSPECT WS-KDINVKAT-REDIG REPLACING LEADING SPACE BY ZERO            
073800     MOVE WS-KDINVKAT-REDIG          TO MID-KDINVKAT-PRINT(IND)           
073900     .                                                                    
074000     EJECT                                                                
074100 DC-KOLLA-PRINTKOD-WDH1 SECTION.                                          
074200                                                                          
074300     MOVE MID-IDARTNR-UTSKR(IX)      TO WS-IDARTNR-REDIG                  
074400     INSPECT WS-IDARTNR-REDIG REPLACING LEADING SPACE BY ZERO             
074500     MOVE WS-IDARTNR-REDIG           TO W-IDARTNR                         
074600                                        W-IDARTNR-ARTC                    
074700                                                                          
074800     IF MID-KDINVPRIO-UTSKR(IX) = SPACE                                   
074900        MOVE +2                      TO WS-KDINVPRIO                      
075000     ELSE                                                                 
075100        MOVE MID-KDINVPRIO-UTSKR(IX) TO WS-KDINVPRIO                      
075200     END-IF                                                               
075300     MOVE MID-KDINVKAT-UTSKR(IX)     TO WS-KDINVKAT-REDIG                 
075400     INSPECT WS-KDINVKAT-REDIG REPLACING LEADING SPACE BY ZERO            
075500     MOVE     WS-KDINVKAT-REDIG      TO W-KDINVKAT-MIN                    
075600                                        W-KDINVKAT-MAX                    
075700     PERFORM IMS-GET-INVENTERINGS-ROT                                     
075800     PERFORM IMS-GNP-INVENTERINGS                                         
075900     .                                                                    
076000     EJECT                                                                
076100 F-LAES-FOERSTA SECTION.                                                  
076200*                                                                         
076300     MOVE 'F-LAS               ' TO WS-SECTION                            
076400     MOVE INF-FIRST-PAGE   TO MED-IDMFSINF                                
076500     CALL WMEDKONV         USING MED-WMEDAREA                             
076600     MOVE MED-MFSINF       TO MOD-TEMFSINF                                
076700     .                                                                    
076800     SKIP2                                                                
076900 G-LAES-NAESTA SECTION.                                                   
077000*                                                                         
077100      MOVE 'G-LAS               ' TO WS-SECTION                           
077200     IF SAVE-IDTRANS = '5309'                                             
077300        MOVE SAVE-IDPRTOMG-NEXT    TO W-BSEQ-IDPRTOMG-MIN                 
077400*       MOVE SAVE-IDLOPNR-NEXT     TO W-BSEQ-IDLOPNR-MIN                  
077500        MOVE SAVE-ADLAGOMR-NEXT    TO W-BSEQ-ADLAGOMR-MIN                 
077600        MOVE SAVE-ADGANG-NEXT      TO W-BSEQ-ADGANG-MIN                   
077700        MOVE SAVE-ADPLATS-NEXT     TO W-BSEQ-ADPLATS-MIN                  
077800        MOVE SAVE-IDPRTOMG-NEXT    TO W-BSEQ-IDPRTOMG-MIN                 
077900        MOVE SAVE-IDDC-NEXT        TO W-BSEQ-IDDC-MIN                     
078000        MOVE SAVE-IDARTNR-NEXT     TO W-IDARTNR                           
078100        MOVE SAVE-FLINVSKR         TO WS-FLINVSKR                         
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 H-LAES-SAMMA SECTION.                                                    
078600      MOVE 'H-LAS               ' TO WS-SECTION                           
078700     IF SAVE-IDTRANS = '5309'                                             
078800        MOVE SAVE-IDPRTOMG-ENTER   TO W-BSEQ-IDPRTOMG-MIN                 
078900*       MOVE SAVE-IDLOPNR-ENTER    TO W-BSEQ-IDLOPNR-MIN                  
079000        MOVE SAVE-ADLAGOMR-ENTER   TO W-BSEQ-ADLAGOMR-MIN                 
079100        MOVE SAVE-ADGANG-ENTER     TO W-BSEQ-ADGANG-MIN                   
079200        MOVE SAVE-ADPLATS-ENTER    TO W-BSEQ-ADPLATS-MIN                  
079300        MOVE SAVE-IDPRTOMG-ENTER   TO W-BSEQ-IDPRTOMG-MIN                 
079400        MOVE SAVE-IDDC-ENTER       TO W-BSEQ-IDDC-MIN                     
079500        MOVE SAVE-IDARTNR-ENTER    TO W-IDARTNR                           
079600        MOVE SAVE-FLINVSKR         TO WS-FLINVSKR                         
079700        MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                        
079800        CALL WMEDKONV USING MED-WMEDAREA                                  
079900        MOVE MED-MFSINF            TO MOD-TEMFSINF                        
080000     END-IF                                                               
080100     .                                                                    
080200     EJECT                                                                
080300 S02-LAES-VISA-KOE2 SECTION.                                              
080400*    MOVE 'S02-LAS-VISA        ' TO WS-SECTION                            
080500     MOVE +1 TO INDX                                                      
080600     MOVE 'FÖRSTA-IMS-GU-WDH1B1' TO WS-SECTION                            
080700     PERFORM IMS-GU-WDH1B1                                                
080800     IF WS-FLINVSKR = 'N'                                                 
080900       MOVE 1 TO WS-VARIABEL                                              
081000     ELSE                                                                 
081100       MOVE 0 TO WS-VARIABEL                                              
081200     END-IF                                                               
081300     MOVE JA TO FIRST-SW                                                  
081400     MOVE NEJ TO FORTSATT-SW                                              
081500     PERFORM UNTIL INDX > MAX-IX                                          
081600     IF SEGMENT-FINNS                                                     
081700       IF WS-IDPRTOMG = BSEQ-INV-IDPRTOMG + WS-VARIABEL                   
081800        IF KEY-ADLAGOMR = ZERO OR                                         
081900           (KEY-ADLAGOMR = BSEQ-INV-ADLAGOMR)                             
082000         IF KEY-ADGANG <= BSEQ-INV-ADGANG OR                              
082100            (KEY-ADGANG = ZERO)                                           
082200          IF KEY-ADPLATS <= BSEQ-INV-ADPLATS OR                           
082300             (KEY-ADPLATS = ZERO)                                         
082400           IF BSEQ-INV-FLINVBEH = NEJ AND                                 
082500              BSEQ-INV-KDINVKAT NOT = +8                                  
082600            IF BSEQ-INV-FLINVSKR = WS-FLINVSKR                            
082700            MOVE 'SKA GÖRA GNP-WDH1B1 ' TO WS-SECTION                     
082800             PERFORM IMS-GNP-WDH1B1                                       
082900              IF INDX = 1                                                 
083000               IF W-IDARTNR = ART-IDARTNR OR                              
083100                  W-IDARTNR = 0                                           
083200                 MOVE JA TO FORTSATT-SW                                   
083300               END-IF                                                     
083400              END-IF                                                      
083500              IF FORTSATT                                                 
085000               PERFORM S021-SKRIV-RAD                                     
085100               ADD +1 TO INDX                                             
085300               IF FIRST-TIME                                              
085400                 MOVE '5309'             TO SAVE-IDTRANS                  
085500                 MOVE BSEQ-INV-IDDC      TO SAVE-IDDC-ENTER               
085600                                            SAVE-IDDC-NEXT                
085700                 MOVE BSEQ-INV-ADLAGOMR  TO                               
085800                                            SAVE-ADLAGOMR-ENTER           
085900                                            SAVE-ADLAGOMR-NEXT            
086000                 MOVE BSEQ-INV-ADGANG    TO SAVE-ADGANG-ENTER             
086100                                            SAVE-ADGANG-NEXT              
086200                 MOVE BSEQ-INV-ADPLATS   TO SAVE-ADPLATS-ENTER            
086300                                            SAVE-ADPLATS-NEXT             
086400                 MOVE BSEQ-INV-KDINVPRIO TO                               
086500                                            SAVE-KDINVPRIO-ENTER          
086600                                            SAVE-KDINVPRIO-NEXT           
086700                 MOVE BSEQ-INV-KDVVKL   TO  SAVE-KDVVKL-ENTER             
086800                                            SAVE-KDVVKL-NEXT              
086900                 MOVE ART-IDARTNR       TO                                
087000                                            SAVE-IDARTNR-ENTER            
087100                                            SAVE-IDARTNR-NEXT             
087200                 MOVE BSEQ-INV-KDINVKAT TO                                
087300                                            SAVE-KDINVKAT-ENTER           
087400                                            SAVE-KDINVKAT-NEXT            
087500                 MOVE BSEQ-INV-IDPRTOMG TO                                
087600                                            SAVE-IDPRTOMG-ENTER           
087700                                            SAVE-IDPRTOMG-NEXT            
087800                 MOVE BSEQ-INV-IDLOPNR  TO                                
087900                                            SAVE-IDLOPNR-ENTER            
088000                                            SAVE-IDLOPNR-NEXT             
088100                 MOVE WS-FLINVSKR       TO  SAVE-FLINVSKR                 
088200                 MOVE '002'             TO MSGI-KDCALL                    
088300                 MOVE '5309'            TO MSGI-IDTRANS                   
088400                 MOVE SAVE-AREA         TO MSGI-SPAR-AREA                 
088500                 CALL W005INIT USING MSGI-WMSGINIT                        
088600                      USEA-PCB                                            
088700                 MOVE NEJ TO FIRST-SW                                     
088800               END-IF                                                     
088900              END-IF                                                      
089000            END-IF                                                        
089100           END-IF                                                         
089200          END-IF                                                          
089300         END-IF                                                           
089400        END-IF                                                            
089500       END-IF                                                             
089600       MOVE 'SKA 2A GN-WDH1B1    ' TO WS-SECTION                          
089700       PERFORM IMS-GN-WDH1B1                                              
089800     ELSE                                                                 
089900       IF INDX = 1                                                        
090000         MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                     
090100         CALL WMEDKONV                USING MED-WMEDAREA                  
090200         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
090300         MOVE MFS-RENSA-FAELT         TO MOD-TEMFSINF                     
090400       END-IF                                                             
090500       PERFORM MFS-RENSA-RAD                                              
090600       ADD +1 TO INDX                                                     
090700     END-IF                                                               
090800     END-PERFORM                                                          
090900                                                                          
091000     MOVE NEJ TO SW-SIDA-FULL                                             
091100     IF SEGMENT-FINNS                                                     
091200        MOVE JA TO SW-SIDA-FULL                                           
091300        PERFORM UNTIL SEGMENT-SAKNAS OR KONTROLL-OK = JA                  
091400          PERFORM S022-KONTROLLERA-VILLKOR                                
091500          IF KONTROLL-OK = NEJ                                            
091600             PERFORM IMS-GN-WDH1B1                                        
091700          END-IF                                                          
091800        END-PERFORM                                                       
091900        IF SEGMENT-SAKNAS                                                 
092000           IF MFS-NEXT                                                    
092100               MOVE INF-LAST-PAGE      TO MED-IDMFSINF                    
092200               CALL WMEDKONV           USING MED-WMEDAREA                 
092300               MOVE MED-MFSINF         TO MOD-TEMFSINF                    
092400           END-IF                                                         
092500        ELSE                                                              
092600          IF KONTROLL-OK = JA                                             
092700             MOVE '5309'             TO SAVE-IDTRANS                      
092800             MOVE BSEQ-INV-IDDC      TO SAVE-IDDC-NEXT                    
092900             MOVE BSEQ-INV-ADLAGOMR  TO SAVE-ADLAGOMR-NEXT                
093000             MOVE BSEQ-INV-ADGANG    TO SAVE-ADGANG-NEXT                  
093100             MOVE BSEQ-INV-ADPLATS   TO SAVE-ADPLATS-NEXT                 
093200             MOVE BSEQ-INV-KDINVPRIO TO SAVE-KDINVPRIO-NEXT               
093300             MOVE BSEQ-INV-KDVVKL    TO SAVE-KDVVKL-NEXT                  
093400             MOVE BSEQ-INV-KDINVKAT  TO SAVE-KDINVKAT-NEXT                
093500             MOVE BSEQ-INV-IDPRTOMG  TO SAVE-IDPRTOMG-NEXT                
093600             MOVE BSEQ-INV-IDLOPNR   TO SAVE-IDLOPNR-NEXT                 
093700             PERFORM IMS-GNP-WDH1B1                                       
093800             MOVE ART-IDARTNR        TO SAVE-IDARTNR-NEXT                 
093900             IF MFS-FIRST                                                 
094000               MOVE +0 TO RAKNARE                                         
094100               MOVE 'SKA GÖRA GU WDH1B1  ' TO WS-SECTION                  
094200               PERFORM IMS-GU-WDH1B1                                      
094300               PERFORM UNTIL SEGMENT-SAKNAS                               
094400               IF SEGMENT-FINNS                                           
094500                 PERFORM S022-KONTROLLERA-VILLKOR                         
094600                   IF KONTROLL-OK = JA                                    
096000                     ADD +1 TO RAKNARE                                    
096200                     MOVE 'SKA GÖRA1GN-WDH1B1  ' TO WS-SECTION            
096300                     PERFORM IMS-GN-WDH1B1                                
096400                   ELSE                                                   
096500                     MOVE 'SKA GÖRA 2 GN-WDH1B1' TO WS-SECTION            
096600                     PERFORM IMS-GN-WDH1B1                                
096700                   END-IF                                                 
096800               END-IF                                                     
096900               END-PERFORM                                                
097000               MOVE RAKNARE TO MOD-KVANT-ART                              
097100                               SAVE-RAKNARE                               
097200             ELSE                                                         
097300               MOVE SAVE-RAKNARE TO MOD-KVANT-ART                         
097400             END-IF                                                       
097500             MOVE '002'              TO MSGI-KDCALL                       
097600             MOVE '5309'             TO MSGI-IDTRANS                      
097700             MOVE SAVE-AREA          TO MSGI-SPAR-AREA                    
097800             CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                   
097900             IF MFS-ENTER OR MFS-NEXT                                     
098000                MOVE INF-MORE-INFO-PF8  TO MED-IDMFSINF                   
098100                CALL WMEDKONV           USING MED-WMEDAREA                
098200                MOVE MED-MFSINF         TO MOD-TEMFSINF                   
098300             END-IF                                                       
098400                                                                          
098500          ELSE                                                            
098600             IF MFS-NEXT                                                  
098700                 MOVE INF-LAST-PAGE      TO MED-IDMFSINF                  
098800                 CALL WMEDKONV           USING MED-WMEDAREA               
098900                 MOVE MED-MFSINF         TO MOD-TEMFSINF                  
099000             END-IF                                                       
099100          END-IF                                                          
099200        END-IF                                                            
099300     ELSE                                                                 
099400        IF MFS-NEXT                                                       
099500           MOVE INF-LAST-PAGE      TO MED-IDMFSINF                        
099600           CALL WMEDKONV           USING MED-WMEDAREA                     
099700           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
099800        END-IF                                                            
099900     END-IF                                                               
100000     MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                                 
100100                                                                          
100200     .                                                                    
100300     EJECT                                                                
100400                                                                          
100500 S021-SKRIV-RAD SECTION.                                                  
100600*                                                                         
100700      MOVE 'S021-SKRIV          ' TO WS-SECTION                           
100800     IF WS-VARIABEL = 0                                                   
100900       MOVE BSEQ-INV-IDPRTOMG      TO WS-IDPRTOMG-MOD                     
101000       MOVE BSEQ-INV-IDLOPNR       TO WS-IDLOPNR-MOD                      
101100       MOVE WS-LISTNR              TO MOD-LISTNR        (INDX)            
101200     ELSE                                                                 
101300       MOVE SPACE                  TO MOD-LISTNR        (INDX)            
101400     END-IF                                                               
101500     MOVE BSEQ-INV-ADLAGOMR        TO WS-ADLAGOMR-NUM                     
101600     MOVE WS-ADLAGOMR-NUM          TO MOD-ADLAGOMR-UTSKR(INDX)            
101700     MOVE BSEQ-INV-ADGANG          TO MOD-ADGANG-UTSKR  (INDX)            
101800     MOVE BSEQ-INV-ADPLATS         TO MOD-ADPLATS-UTSKR (INDX)            
101900     IF CDC                                                               
102000        IF BSEQ-INV-KDINVPRIO = +2                                        
102100           MOVE +0 TO MOD-KDINVPRIO-UTSKR (INDX)                          
102200        ELSE                                                              
102300           MOVE BSEQ-INV-KDINVPRIO TO MOD-KDINVPRIO-UTSKR (INDX)          
102400        END-IF                                                            
102500     ELSE                                                                 
102600        MOVE BSEQ-INV-KDINVPRIO    TO MOD-KDINVPRIO-UTSKR (INDX)          
102700     END-IF                                                               
102800     IF NOT NDC-NA                                                        
102900        MOVE BSEQ-INV-KDVVKL       TO MOD-KDVVKL-UTSKR (INDX)             
102910     END-IF                                                               
103000     MOVE BSEQ-INV-KDINVKAT        TO WS-KDINVKAT-NUM                     
103100     MOVE WS-KDINVKAT-NUM          TO MOD-KDINVKAT-UTSKR  (INDX)          
103200     MOVE BSEQ-INV-FLINVSKR        TO MOD-FLINVSKR-UTSKR (INDX)           
103300     MOVE BSEQ-INV-KDPRODSL        TO MOD-KDPRODSL-UTSKR (INDX)           
103400     MOVE BSEQ-INV-IDFKNGRP        TO MOD-IDFKNGRP-UTSKR (INDX)           
103500***** NYTT DATUM TILL BILDEN ***                                          
103600     MOVE BSEQ-INV-DAREGDAT        TO WS-FIX-DAREGDAT                     
103700     MOVE WS-DAREGDAT-AAMMDD       TO MOD-TIREGDAT-UTSKR  (INDX)          
103800*    MOVE BSEQ-INV-TISEGKEY        TO WS-FIX-TISEGKEY                     
103900*    MOVE WS-TISEGKEY-3-8          TO MOD-TIREGDAT-UTSKR  (INDX)          
104000***** SLUT PÅ NYTT DATUM *******                                          
104100     MOVE ART-IDARTNR              TO MOD-IDARTNR-UTSKR (INDX)            
104200     IF MFS-FIRST                                                         
104300       MOVE ZERO                   TO MOD-KVANT-ART                       
104400       MOVE ZERO                   TO SAVE-RAKNARE                        
104500     ELSE                                                                 
104600       MOVE SAVE-RAKNARE           TO MOD-KVANT-ART                       
104700     END-IF                                                               
104800     .                                                                    
104900     EJECT                                                                
105000 S022-KONTROLLERA-VILLKOR SECTION.                                        
105100*                                                                         
105200     MOVE 'S022-KONTROLL       ' TO WS-SECTION                            
105300     MOVE NEJ TO KONTROLL-OK                                              
105400     IF WS-IDPRTOMG = BSEQ-INV-IDPRTOMG + WS-VARIABEL                     
105500         IF KEY-ADLAGOMR = ZERO OR                                        
105600           (KEY-ADLAGOMR = BSEQ-INV-ADLAGOMR)                             
105700            IF KEY-ADGANG <= BSEQ-INV-ADGANG OR                           
105800              (KEY-ADGANG = ZERO)                                         
105900               IF KEY-ADPLATS <= BSEQ-INV-ADPLATS OR                      
106000                 (KEY-ADPLATS = ZERO)                                     
106100                 IF BSEQ-INV-KDINVKAT NOT = +6                            
106200                   IF BSEQ-INV-FLINVBEH = NEJ AND                         
106300                      BSEQ-INV-KDINVKAT NOT = +8                          
106400                     IF BSEQ-INV-FLINVSKR = WS-FLINVSKR                   
106500                       MOVE JA TO KONTROLL-OK                             
106600                     END-IF                                               
106700                   END-IF                                                 
106800                 END-IF                                                   
106900               END-IF                                                     
107000            END-IF                                                        
107100         END-IF                                                           
107200     END-IF                                                               
107300     .                                                                    
107400     EJECT                                                                
107500 MFS-RENSA SECTION.                                                       
107600*                                                                         
107700     MOVE +1 TO INDX                                                      
107800                                                                          
107900     PERFORM 10 TIMES                                                     
108000        MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR-UTSKR (INDX)               
108100                                  MOD-ADGANG-UTSKR   (INDX)               
108200                                  MOD-ADPLATS-UTSKR  (INDX)               
108300                                  MOD-KDINVPRIO-UTSKR(INDX)               
108400                                  MOD-KDVVKL-UTSKR   (INDX)               
108500                                  MOD-IDARTNR-UTSKR  (INDX)               
108600                                  MOD-KDINVKAT-UTSKR (INDX)               
108700                                  MOD-FLINVSKR-UTSKR (INDX)               
108800                                  MOD-KDPRODSL-UTSKR (INDX)               
108900                                  MOD-IDFKNGRP-UTSKR (INDX)               
109000                                  MOD-TIREGDAT-UTSKR (INDX)               
109100                                  MOD-LISTNR         (INDX)               
109200                                                                          
109300        ADD +1 TO INDX                                                    
109400     END-PERFORM                                                          
109500     .                                                                    
109600     SKIP2                                                                
109700 MFS-RENSA-RAD SECTION.                                                   
109800*                                                                         
109900        MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR-UTSKR (INDX)               
110000                                  MOD-ADGANG-UTSKR   (INDX)               
110100                                  MOD-ADPLATS-UTSKR  (INDX)               
110200                                  MOD-KDINVPRIO-UTSKR(INDX)               
110300                                  MOD-KDVVKL-UTSKR   (INDX)               
110400                                  MOD-IDARTNR-UTSKR  (INDX)               
110500                                  MOD-KDINVKAT-UTSKR (INDX)               
110600                                  MOD-FLINVSKR-UTSKR (INDX)               
110700                                  MOD-KDPRODSL-UTSKR (INDX)               
110800                                  MOD-IDFKNGRP-UTSKR (INDX)               
110900                                  MOD-TIREGDAT-UTSKR (INDX)               
111000                                  MOD-LISTNR         (INDX)               
111100                                                                          
111200     .                                                                    
111300     SKIP2                                                                
111400 MFS-ROER-EJ-MOD-RAD SECTION.                                             
111500*                                                                         
111600     MOVE MFS-ROER-EJ-FAELT    TO MOD-KVINVSKR                            
111700                                  MOD-KVANT-ART                           
111800     MOVE +1 TO INDX                                                      
111900                                                                          
112000     PERFORM 10 TIMES                                                     
112100        MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-UTSKR (INDX)               
112200                                  MOD-ADGANG-UTSKR   (INDX)               
112300                                  MOD-ADPLATS-UTSKR  (INDX)               
112400                                  MOD-KDINVPRIO-UTSKR(INDX)               
112500                                  MOD-KDVVKL-UTSKR   (INDX)               
112600                                  MOD-IDARTNR-UTSKR  (INDX)               
112700                                  MOD-KDINVKAT-UTSKR (INDX)               
112800                                  MOD-FLINVSKR-UTSKR (INDX)               
112900                                  MOD-KDPRODSL-UTSKR (INDX)               
113000                                  MOD-IDFKNGRP-UTSKR (INDX)               
113100                                  MOD-TIREGDAT-UTSKR (INDX)               
113200                                  MOD-LISTNR         (INDX)               
113300                                                                          
113400        ADD +1 TO INDX                                                    
113500     END-PERFORM                                                          
113600     .                                                                    
113700     EJECT                                                                
113800* IMS SEKTIONER                                                           
113900     SKIP3                                                                
114000 IMS-GET-MSG SECTION.                                                     
114100     MOVE '  QC' TO GODK-STATUSKODER                                      
114200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
114300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600     SKIP3                                                                
114700 IMS-INSERT-MSG SECTION.                                                  
114800     IF MSGI-IDLAND-SPR = 'SE'                                            
114900       MOVE '0' TO MFS-KDHUVOMR                                           
115000     ELSE                                                                 
115100       MOVE 'N' TO MFS-KDHUVOMR                                           
115200     END-IF                                                               
115300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
115400     MOVE SPACE TO GODK-STATUSKODER                                       
115500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
115600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115700     PERFORM IMS-STATUSKONTROLL                                           
115800     .                                                                    
115900     SKIP3                                                                
116000 IMS-INSERT-ALT-MSG SECTION.                                              
116100     MOVE SPACE TO GODK-STATUSKODER                                       
116200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
116300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600     SKIP3                                                                
116700 IMS-PURGE-ALT-MSG SECTION.                                               
116800     MOVE SPACE TO GODK-STATUSKODER                                       
116900     CALL CBLTDLI USING PURG ALT-PCB                                      
117000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
117100     PERFORM IMS-STATUSKONTROLL                                           
117200     .                                                                    
117300     EJECT                                                                
117400 IMS-GET-INVENTERINGS-ROT  SECTION.                                       
117500      MOVE 'IMS-GET-INVENT-ROT  ' TO WS-IMS                               
117600     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
117700              DELIMITED BY SIZE INTO SSA1                                 
117800     MOVE '  GE' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA SSA1                      
118000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     SKIP2                                                                
118400 IMS-GNP-INVENTERINGS SECTION.                                            
118500     MOVE 'IMS-GNP-INVENT      ' TO WS-IMS                                
118600     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
118700                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
118800                    '&FLINVBEH =' NEJ ')'                                 
118900              DELIMITED BY SIZE INTO SSA1                                 
119000     MOVE '  GE' TO GODK-STATUSKODER                                      
119100     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA SSA1                     
119200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
119300     PERFORM IMS-STATUSKONTROLL                                           
119400     .                                                                    
119500     SKIP2                                                                
119600 IMS-GU-WDH1B1   SECTION.                                                 
119700     MOVE 'IMS-GU-WDH1B1       ' TO WS-IMS                                
119800     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
119900                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
120000                                                                          
120100            DELIMITED BY SIZE INTO SSA1                                   
120200     MOVE '  GE' TO GODK-STATUSKODER                                      
120300     CALL CBLTDLI USING GHU WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
120400     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     .                                                                    
120700                                                                          
120800 IMS-GN-WDH1B1   SECTION.                                                 
120900     MOVE 'IMS-GN-WDH1B1       ' TO WS-IMS                                
121000     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
121100                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
121200            DELIMITED BY SIZE INTO SSA1                                   
121300     MOVE '  GE' TO GODK-STATUSKODER                                      
121400     CALL CBLTDLI USING GHN WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
121500     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
121600     PERFORM IMS-STATUSKONTROLL                                           
121700     .                                                                    
121800                                                                          
121900 IMS-GNP-WDH1B1   SECTION.                                                
122000     MOVE 'IMS-GNP-WDH1B1      ' TO WS-IMS                                
122100     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
122200                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
122300            DELIMITED BY SIZE INTO SSA1                                   
122400     STRING 'WDH101   '                                                   
122500            DELIMITED BY SIZE INTO SSA2                                   
122600     MOVE '  ' TO GODK-STATUSKODER                                        
122700     CALL CBLTDLI USING GNP WDH1B-PCB DLI-IO-AREA SSA1 SSA2               
122800     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
122900     PERFORM IMS-STATUSKONTROLL                                           
123000     .                                                                    
123100                                                                          
123200 IMS-STATUSKONTROLL SECTION.                                              
123300     SET STATUS-IX TO 1                                                   
123400     SEARCH GODK-STATUS AT END CALL FELLOG                                
123500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
123600     END-SEARCH                                                           
123700     .                                                                    
