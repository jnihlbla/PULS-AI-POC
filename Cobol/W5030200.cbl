000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5030200.                                                
000300 AUTHOR.         CHRISTINA BRUHN                                          
000400 DATE-WRITTEN.   MAJ   86.                                                
000500                                                                          
000600*    FUNKTION.                                                            
000700*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000800*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0175               
000900*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001000*        INVENTERING                                                      
001100*        PROGRAMMET HAR TRE FUNKTIONER                                    
001200*        - TITTA PÅ INVENTERINGSKÖN                                       
001300*        - VÄLJA VILKA ARTIKLAR MAN VILL HA UT PÅ INVENTERINGS-           
001400*          UNDERLAG                                                       
001500*          TRANSAR MED ARTIKELNR SKICKAS TILL W5030100                    
001600*          SOM TAR HAND OM UTSKRIFTEN AV INVENTERINGSUNDERLAGEN.          
001700*        - VISAR INTE ARTIKLAR SOM HAR 1,2 ELLER 3 I IDPRTOMG             
001800*          OCH FLINVSKR = N, DESSA VISAS PÅ BILD 5308                     
001900*                                                                         
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W5T302                                              
002400*        MID:         W5I30201                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W5O30201                                            
002800*                                                                         
002900*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
003000*                                                                         
003100*    E-TRACKER: 7956816                                                   
003200*        NOT TO FALG FLAGGA-FL = 'J', FOR PARTS WITH VOHF ADDRESS         
003300*                                                                         
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP3                                                                
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300 77  IDPGM                     PIC X(8)   VALUE 'W5030200'.               
004400 77  JA                        PIC X      VALUE 'J'.                      
004500 77  NEJ                       PIC X      VALUE 'N'.                      
004600 77  KONTROLL-OK               PIC X      VALUE 'N'.                      
004700 77  SPRAK-IX                  PIC S9(9)  VALUE +0   COMP SYNC.           
004800 77  IX                        PIC S9(9)  VALUE +0   COMP SYNC.           
004900 77  INDX                      PIC S9(9)  VALUE +0   COMP SYNC.           
005000 77  IND                       PIC S9(9)  VALUE +0   COMP SYNC.           
005100 77  ALT-IND                   PIC S9(9)  VALUE +0   COMP SYNC.           
005200 77  ALT2-IND                   PIC S9(9)  VALUE +0   COMP SYNC.          
005300 77  MAX-IX                    PIC S9(9)  VALUE +10  COMP SYNC.           
005400 77  MAX-INDX-PLUS-1           PIC S9(9)  VALUE +8   COMP SYNC.           
005500 77  ADD-KVINVSKR-PRINT        PIC S9(9)  VALUE +0   COMP SYNC.           
005600 77  RAKNARE                   PIC  9(5)  VALUE ZERO.                     
005700 77  WS-IDPRTLST               PIC  X(8)  VALUE SPACE.                    
005800 77  SW-URVAL-OK               PIC  X(1)  VALUE SPACE.                    
005900 77  SPAR-STATUS-WS            PIC  X(2)  VALUE SPACE.                    
006000 77  WS-SECTION                PIC  X(32) VALUE                           
006100     'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                                  
006200                                                                          
006300*01  -COPY WWDCKONS                                                       
006400                                                                          
006500 77  ALLT-OK-SW                 PIC X         VALUE 'J'.                  
006600      88  ALLT-OK                             VALUE 'J'.                  
006700      88  ALLT-FEL                            VALUE 'N'.                  
006800                                                                          
006900 77  FLINVSKR-SW                PIC X         VALUE 'J'.                  
007000      88  FLINVSKR-Y                          VALUE 'J'.                  
007100      88  FLINVSKR-N                          VALUE 'N'.                  
007200                                                                          
007300 77  FIRST-SW                  PIC X         VALUE 'J'.                   
007400      88  FIRST-TIME                         VALUE 'J'.                   
007500      88  LAST-TIME                          VALUE 'N'.                   
007600                                                                          
007700 77  INDATA-SW                 PIC X         VALUE 'J'.                   
007800      88  INDATA-OK                          VALUE 'J'.                   
007900      88  INDATA-FEL                         VALUE 'N'.                   
008000                                                                          
008100 77  NYCKLAR-SW                PIC X         VALUE 'J'.                   
008200      88  NYCKLAR-OK                         VALUE 'J'.                   
008300      88  NYCKLAR-FEL                        VALUE 'N'.                   
008400                                                                          
008500 77  POST-SW                   PIC X         VALUE 'J'.                   
008600      88  POST-FINNS                         VALUE 'J'.                   
008700      88  POST-SAKNAS                        VALUE 'N'.                   
008800                                                                          
008900 77  W-IDTRANS                 PIC X(4)   VALUE SPACE.                    
009000      88   EGEN-MID                       VALUE '5302'.                   
009100      88   GODK-MID                       VALUE '5301' '5302'             
009200                                                '5303' '5304'             
009300                                                '5305' '5306'             
009400                                                '5307' '5308'             
009500                                                '5309'.                   
009600                                                                          
009700*   --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS             
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'SAVE-AREA'.           
010000 01  SAVE-AREA.                                                           
010100   03  SAVE-IDTRANS              PIC X(8)    VALUE '5302'.                
010200   03  SAVE-IDDC-ENTER           PIC X(2).                                
010300   03  SAVE-ADLAGOMR-ENTER       PIC S9(3)     COMP-3.                    
010400   03  SAVE-ADGANG-ENTER         PIC S9(3)     COMP-3.                    
010500   03  SAVE-ADPLATS-ENTER        PIC S9(5)     COMP-3.                    
010600   03  SAVE-KDINVPRIO-ENTER      PIC S9        COMP-3.                    
010700   03  SAVE-KDVVKL-ENTER         PIC S9        COMP-3.                    
010800   03  SAVE-IDARTNR-ENTER        PIC S9(9)     COMP-3.                    
010900   03  SAVE-KDINVKAT-ENTER       PIC S9(3)     COMP-3.                    
011000                                                                          
011100   03  SAVE-IDDC-NEXT            PIC X(2)  VALUE SPACE.                   
011200   03  SAVE-ADLAGOMR-NEXT        PIC S9(3) VALUE ZERO COMP-3.             
011300   03  SAVE-ADGANG-NEXT          PIC S9(3) VALUE ZERO COMP-3.             
011400   03  SAVE-ADPLATS-NEXT         PIC S9(5) VALUE ZERO COMP-3.             
011500   03  SAVE-KDINVPRIO-NEXT       PIC S9    VALUE ZERO COMP-3.             
011600   03  SAVE-KDVVKL-NEXT          PIC S9    VALUE ZERO COMP-3.             
011700   03  SAVE-IDARTNR-NEXT         PIC S9(9) VALUE ZERO COMP-3.             
011800   03  SAVE-KDINVKAT-NEXT        PIC S9(3) VALUE ZERO COMP-3.             
011900   03  SAVE-RAKNARE              PIC  9(5)  VALUE ZERO.                   
012000                                                                          
012100                                                                          
012200 01  GENERELLA-SUBPROGRAM.                                                
012300   03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.               
012400   03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.               
012500   03  WMEDKONV                PIC X(8)   VALUE 'WMEDKONV'.               
012600   03  W005INIT                PIC X(8)   VALUE 'W005INIT'.               
012700   03  W006PRT                 PIC X(8)   VALUE 'W006PRT '.               
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013000*01 -COPY WMSGINIT                                                        
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013300 01 -COPY WMEDAREA                                                        
013400     EJECT                                                                
013500*01 -COPY W006PRT                                                         
013600     EJECT                                                                
013700 01  MESSAGE-CODES.                                                       
013800     03   ERR-CORR-HILITE-FLDS      PIC X(3)    VALUE '001'.              
013900     03   ERR-PRESS-PF11            PIC X(3)    VALUE '168'.              
014000     03   ERR-PF11-AND-NO-DATA      PIC X(3)    VALUE '011'.              
014100     03   INF-UPDATE-DONE           PIC X(3)    VALUE '101'.              
014200     03   INF-FIRST-PAGE            PIC X(3)    VALUE '006'.              
014300     03   INF-LAST-PAGE             PIC X(3)    VALUE '106'.              
014400     03   ERR-WRONG-KEY             PIC X(3)    VALUE '401'.              
014500     03   ERR-NO-UPDATE             PIC X(3)    VALUE '007'.              
014600     03   ERR-PART-MISSING          PIC X(3)    VALUE '017'.              
014700     03   INF-MORE-INFO-PF8         PIC X(3)    VALUE '105'.              
014800     03   INF-PRINTING-REQ          PIC X(3)    VALUE '118'.              
014900     03   INF-NO-PRINTING           PIC X(3)    VALUE '167'.              
015000                                                                          
015100 01  SW-SIDA-FULL                                    PIC X(1).            
015200 01  WS-FLURVAL                                      PIC X.               
015300 01  WS-IDARTNR                                      PIC X(9).            
015400 01  WS-IDARTNR-REDIG                                PIC X(9).            
015500 01  WS-KDINVKAT                                     PIC X(2).            
015600 01  KEY-KDINVKAT REDEFINES WS-KDINVKAT              PIC 9(2).            
015700 01  WS-ADLAGOMR                                     PIC X(2).            
015800 01  KEY-ADLAGOMR REDEFINES WS-ADLAGOMR              PIC 9(2).            
015900 01  WS-ADGANG                                       PIC X(2).            
016000 01  KEY-ADGANG REDEFINES WS-ADGANG                  PIC 9(2).            
016100 01  WS-ADPLATS                                      PIC X(5).            
016200 01  KEY-ADPLATS REDEFINES WS-ADPLATS                PIC 9(5).            
016300 01  WS-KDVVKL                                       PIC X.               
016400 01  KEY-KDVVKL REDEFINES WS-KDVVKL                  PIC 9.               
016500 01  WS-FLINVSKR                                     PIC X(1).            
016600 01  WS-KDINVPRIO                                    PIC X(1).            
016700 01  KEY-KDINVPRIO REDEFINES WS-KDINVPRIO            PIC 9.               
016800 01  WS-KDPRODSL                                     PIC X(2).            
016900 01  KEY-KDPRODSL REDEFINES WS-KDPRODSL              PIC 9(2).            
017000 01  WS-IDFKNGRP                                     PIC X(4).            
017100 01  KEY-IDFKNGRP REDEFINES WS-IDFKNGRP              PIC 9(4).            
017200 01  WS-TIREGDAT                                     PIC X(6).            
017300 01  KEY-TIREGDAT REDEFINES WS-TIREGDAT              PIC 9(6).            
017400 01  WS-KVINVSKR-PRINT                               PIC 9(2).            
017500                                                                          
017600 01  WS-FIX-DATUM.                                                        
017700     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
017800     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
017900         05 WS-FILLER1-1-2   PIC 9(2).                                    
018000         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
018100         05 WS-FILLER1-9     PIC 9(1).                                    
018200     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
018300         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
018400         05 WS-FILLER2-9     PIC 9(1).                                    
018500     EJECT                                                                
018600 01  WS-KDINVKAT-REDIG                               PIC X(2).            
018700 01  WS-KDINVKAT-NUM                                 PIC 9(3).            
018800 01  WS-ADLAGOMR-REDIG                               PIC X(3).            
018900 01  WS-ADLAGOMR-NUM                                 PIC 9(3).            
019000 01  WS-IDARTNR-SPAR             PIC 9(9).                                
019100                                                                          
019200 01  FILLER                      PIC X(11)   VALUE 'W-WDH11-KEY'.         
019300 01    NYCKLAR-TILL-DLI.                                                  
019400   03    W-IDARTNR-X.                                                     
019500     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
019600                                                                          
019700   03    W-WDH111KY-X.                                                    
019800     05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.                 
019900     05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.           
020000     05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.           
020100     05    W-DAREGDAT-SORT-UNIK PIC 9(8)     VALUE ZERO.                  
020200                                                                          
020300   03    W-WDH111KY-MIN.                                                  
020400     05    W-IDDC-MIN-KY         PIC X(2)    VALUE SPACE.                 
020500     05    W-KDINVKAT-MIN-KY     PIC S9(3)   VALUE ZERO COMP-3.           
020600     05    W-TISEGKEY-MIN-KY     PIC S9(9)   VALUE ZERO COMP-3.           
020700     05    W-DAREGDAT-SORT-MIN-KY PIC 9(8) VALUE ZERO.                    
020800                                                                          
020900   03    W-WDH111KY-MAX.                                                  
021000     05    W-IDDC-MAX-KY         PIC X(2)    VALUE SPACE.                 
021100     05    W-KDINVKAT-MAX-KY     PIC S9(3)   VALUE ZERO COMP-3.           
021200     05    W-TISEGKEY-MAX-KY     PIC S9(9)   VALUE ZERO COMP-3.           
021300     05    W-DAREGDAT-SORT-MAX-KY PIC 9(8) VALUE 99999999.                
021400                                                                          
021500   03    W-WDH11-KEY-MIN-X.                                               
021600     05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                       
021700     05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.                 
021800     05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.                 
021900     05    W-DAREGDAT-SORT-MIN       PIC  9(8)   VALUE ZERO.              
022000                                                                          
022100   03    W-WDH11-KEY-MAX-X.                                               
022200     05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                       
022300     05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.           
022400     05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.           
022500     05    W-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                   
022600                                                                          
022700   03    W-KDINVPRIO-X.                                                   
022800     05    W-KDINVPRIO           PIC S9      VALUE ZERO  COMP-3.          
022900                                                                          
023000   03    W-ARTC-IDARTNR-X.                                                
023100     05    W-IDARTNR-ARTC        PIC S9(9)   VALUE ZERO  COMP-3.          
023200                                                                          
023300   03    W-WDH1A1KY-MIN-X.                                                
023400     05    W-SEQA-IDDC-MIN       PIC X(2)    VALUE SPACE.                 
023500     05    W-SEQA-ADLAGOMR-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
023600     05    W-SEQA-ADGANG-MIN     PIC S9(3)   VALUE ZERO  COMP-3.          
023700     05    W-SEQA-ADPLATS-MIN    PIC S9(5)   VALUE ZERO  COMP-3.          
023800     05    W-SEQA-KDINVPRIO-MIN  PIC S9      VALUE ZERO  COMP-3.          
023900     05    W-SEQA-KDVVKL-MIN     PIC S9      VALUE ZERO  COMP-3.          
024000     05    W-SEQA-IDARTNR-MIN    PIC S9(9)   VALUE ZERO  COMP-3.          
024100     05    W-SEQA-KDINVKAT-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
024200     05    W-SEQA-TISEGKEY-MIN   PIC S9(9)   VALUE ZERO  COMP-3.          
024300                                                                          
024400   03    W-WDH1A1KY-MAX-X.                                                
024500     05    W-SEQA-IDDC-MAX      PIC X(2)  VALUE SPACE.                    
024600     05    W-SEQA-ADLAGOMR-MAX  PIC S9(3) VALUE ZERO       COMP-3.        
024700     05    W-SEQA-ADGANG-MAX    PIC S9(3) VALUE ZERO       COMP-3.        
024800     05    W-SEQA-ADPLATS-MAX   PIC S9(5) VALUE ZERO       COMP-3.        
024900     05    W-SEQA-KDINVPRIO-MAX PIC S9    VALUE ZERO       COMP-3.        
025000     05    W-SEQA-KDVVKL-MAX    PIC S9    VALUE ZERO       COMP-3.        
025100     05    W-SEQA-IDARTNR-MAX   PIC S9(9) VALUE ZERO       COMP-3.        
025200     05    W-SEQA-KDINVKAT-MAX  PIC S9(3) VALUE ZERO       COMP-3.        
025300     05    W-SEQA-TISEGKEY-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
025400                                                                          
025500   03  W-IDDC-B6-X.                                                       
025600     05 W-IDDC-B6               PIC X(2).                                 
025700                                                                          
025800***************************************************************           
025900*                                                                         
026000*              AREOR FÖR MFS OCH SKÄRMHANTERING                           
026100*                                                                         
026200 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
026300     SKIP3                                                                
026400*01  MID -COPY W5I30201                                                   
026500     EJECT                                                                
026600* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
026700 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
026800*01    -COPY WMSGAREA                                                     
026900     EJECT                                                                
027000*  03  MOD -COPY W5O30201  -RED MSG-AREA.                                 
027100     EJECT                                                                
027200*01    -COPY WMFSAREA                                                     
027300     EJECT                                                                
027400* - - - - - - - - - - - - - - - - - - - - ALT-AREA1                       
027500 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
027600 01  W-PROG-TO-PROG-SW.                                                   
027700   03  M-SW-LL                   PIC S9(4)   VALUE  ZERO                  
027800                                             COMP SYNC.                   
027900   03  M-SW-Z1-Z2                PIC X(2)    VALUE LOW-VALUE.             
028000   03  M-SW-KDTRANS              PIC X(8)    VALUE 'W5T301X '.            
028100   03  M-SW-IDTRANS              PIC X(4)    VALUE '530B'.                
028200   03  M-SW-KDMFSTYP             PIC X(1).                                
028300*  03  -COPY W5I30101                                                     
028400     EJECT                                                                
028500* - - - - - - - - - - - - - - - - - - - - ALT-AREA2                       
028600 01  FILLER                      PIC X(16)   VALUE 'ALT2-AREA'.           
028700 01  W-PROG-TO-PROG-SW2.                                                  
028800   03  M-SW-LL-2                 PIC S9(4)   VALUE  ZERO                  
028900                                             COMP SYNC.                   
029000   03  M-SW-Z1-Z2-2              PIC X(2)    VALUE LOW-VALUE.             
029100   03  M-SW-KDTRANS-2            PIC X(8)    VALUE 'W5T392X '.            
029200   03  M-SW-IDTRANS-2            PIC X(4)    VALUE '5302'.                
029300   03  M-SW-KDMFSTYP-2           PIC X(1).                                
029400*  03  -COPY W5I39201 -PRE 5392-                                          
029500     EJECT                                                                
029600* **************************************************************          
029700*                                                                         
029800*                  ARBETS-AREOR TILL IMS-SEKTIONERNA                      
029900*                                                                         
030000 01  IMS-WS.                                                              
030100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS'.              
030200     SKIP3                                                                
030300*                        **** STATUS-KOD FRÅN IMS                         
030400   03  STATUS-WS                 PIC XX.                                  
030500     88  SEGMENT-FINNS                       VALUE '  '.                  
030600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030700     SKIP3                                                                
030800   03    GODK-STATUSKODER.                                                
030900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
031000     SKIP3                                                                
031100 01    SSA1                      PIC X(256).                              
031200 01    SSA2                      PIC X(128).                              
031300     EJECT                                                                
031400*                            IMS FUNKTIONSKODER                           
031500*01    -COPY W0003                                                        
031600     EJECT                                                                
031700 01  FILLER                    PIC X(16)   VALUE 'IMS-AREA1'.             
031800*                            DLI INPUT-OUTPUT AREA                        
031900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDH111'.         
032000 01  DLI-IO-WDH111.                                                       
032100*    03 -COPY WDH111 -PRE INVA-                                           
032200     EJECT                                                                
032300                                                                          
032400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDH121'.         
032500 01  DLI-IO-WDH121.                                                       
032600*    03 -COPY WDH121 -PRE INVA-                                           
032700     EJECT                                                                
032800                                                                          
032900 01    DLI-IO-AREA.                                                       
033000   03    IO-AREA                 PIC X(200)  VALUE SPACE.                 
033100     SKIP3                                                                
033200*  03    WDH1A01 -COPY WDH101                -RED IO-AREA.                
033300     EJECT                                                                
033400*  03    WDH1A11 -COPY WDH111                -RED IO-AREA.                
033500     EJECT                                                                
033600*                            DLI INPUT-OUTPUT AREA 2                      
033700 01    DLI-IO-AREA3.                                                      
033800   03    IO-AREA3                PIC X(100)  VALUE SPACE.                 
033900     SKIP3                                                                
034000*  03    WDH1A01 -COPY WDH1A1                -RED IO-AREA3.               
034100     EJECT                                                                
034200*                            DLI INPUT-OUTPUT AREA 2                      
034300 01  FILLER                    PIC X(16)   VALUE 'IMS-AREA2'.             
034400 01    DLI-IO-AREA2.                                                      
034500   03    IO-AREA2                PIC X(900)  VALUE SPACE.                 
034600*  03    WLARTC01 -COPY WDK601 -PRE ARTC01- -RED IO-AREA2.                
034700     EJECT                                                                
034800*  03    WLARTC11 -COPY WDK611 -PRE ARTC11- -RED IO-AREA2.                
034900     EJECT                                                                
035000                                                                          
035100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
035200 01   DLI-IO-AREA-B601.                                                   
035300*     03  -COPY WDB601                                                    
035400                                                                          
035500     EJECT                                                                
035600 LINKAGE SECTION.                                                         
035700*01    -COPY W0009     -PRE MSG-                                          
035800     EJECT                                                                
035900*01    -COPY W0009     -PRE ALT-                                          
036000     EJECT                                                                
036100*01    -COPY W0009     -PRE ALT2-                                         
036200     EJECT                                                                
036300*01    -COPY W0008     -PRE USEA-                                         
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01    -COPY W0008     -PRE INVB-                                         
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01    -COPY W0008     -PRE INVA-                                         
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01    -COPY W0008     -PRE ARTC-                                         
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008       -PRE WDB6-                                         
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800                                                                          
037900 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ALT2-PCB                        
038000                          USEA-PCB INVB-PCB                               
038100                          INVA-PCB ARTC-PCB WDB6-PCB.                     
038200 MAIN SECTION.                                                            
038300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT2-PCB                       
038400                           USEA-PCB INVB-PCB                              
038500                           INVA-PCB ARTC-PCB WDB6-PCB.                    
038600                                                                          
038700     PERFORM IMS-GET-MSG                                                  
038800     IF SEGMENT-FINNS                                                     
038900        PERFORM A-INIT-SPARA-INPUT                                        
039000        PERFORM B-KOLLA-NYCKLAR                                           
039100        IF NYCKLAR-FEL                                                    
039200           MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                          
039300           CALL WMEDKONV         USING MED-WMEDAREA                       
039400           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
039500           PERFORM MFS-RENSA                                              
039600           MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                           
039700        ELSE                                                              
039800           IF MFS-UPDATE                                                  
039900              PERFORM C-KOLLA-INDATA-UPPDATERING                          
040000              IF INDATA-OK                                                
040100                 PERFORM D-SKICKA-ART-TILL-UTSKRIFT                       
040200                 IF WS-IDARTNR NUMERIC AND                                
040300                    (WS-IDARTNR > ZERO)                                   
040400                    PERFORM MFS-RENSA                                     
040500                 ELSE                                                     
040600                    IF MID-KVINVSKR-5302 > ZERO AND < +11                 
040700                       PERFORM G-LAES-NAESTA                              
040800                       PERFORM S01-FIXA-ARTIKEL-NYCKEL                    
040900                       PERFORM S02-LAES-VISA-INFO                         
041000                       IF SW-SIDA-FULL = JA                               
041100                         MOVE ERR-PRESS-PF11 TO MED-IDMFSFEL              
041200                         CALL WMEDKONV    USING MED-WMEDAREA              
041300                         MOVE MED-MFSFEL     TO MOD-TEMFSFEL              
041400                       END-IF                                             
041500                    ELSE                                                  
041600                       PERFORM MFS-RENSA                                  
041700                    END-IF                                                
041800                 END-IF                                                   
041900              END-IF                                                      
042000           ELSE                                                           
042100              IF WS-IDARTNR NUMERIC AND                                   
042200                 (WS-IDARTNR > ZERO)                                      
042300                 PERFORM E-LAES-ARTIKEL                                   
042400              ELSE                                                        
042500                 IF MFS-FIRST                                             
042600                   PERFORM F-LAES-FOERSTA                                 
042700                 ELSE                                                     
042800                    IF MFS-NEXT                                           
042900                       PERFORM G-LAES-NAESTA                              
043000                    ELSE                                                  
043100                       PERFORM H-LAES-SAMMA                               
043200                       IF MID-KVINVSKR-5302 NUMERIC                       
043300                          IF MID-KVINVSKR-5302 > ZERO AND < +11           
043400                             MOVE ERR-PRESS-PF11 TO MED-IDMFSFEL          
043500                             CALL WMEDKONV    USING MED-WMEDAREA          
043600                             MOVE MED-MFSFEL     TO MOD-TEMFSFEL          
043700                          END-IF                                          
043800                       END-IF                                             
043900                    END-IF                                                
044000                 END-IF                                                   
044100              END-IF                                                      
044200*             IF MID-FLAGGA NOT = ALL '+'                                 
044300*                IF MID-FLAGGA = JA OR MID-FLAGGA = 'Y'                   
044400                   IF WS-IDARTNR > ZERO                                   
044500                     PERFORM S11-LAES-VISA-INFO-ART                       
044600                   ELSE                                                   
044700                     PERFORM S02-LAES-VISA-INFO                           
044800                   END-IF                                                 
044900*                ELSE                                                     
045000*                   MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-ATTR            
045100*                   MOVE ERR-CORR-HILITE-FLDS TO                          
045200*                        MED-IDMFSFEL                                     
045300*                   CALL WMEDKONV USING MED-WMEDAREA                      
045400*                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
045500*                   PERFORM MFS-ROER-EJ-MOD-RAD                           
045600*                END-IF                                                   
045700*             ELSE                                                        
045800*               IF WS-IDARTNR > ZERO                                      
045900*                 PERFORM S11-LAES-VISA-INFO-ART                          
046000*               ELSE                                                      
046100*                 PERFORM S02-LAES-VISA-INFO                              
046200*               END-IF                                                    
046300*             END-IF                                                      
046400           END-IF                                                         
046500        END-IF                                                            
046600        COMPUTE MSG-KVLL = LENGTH OF MOD-W5O30201 + 4                     
046700        PERFORM IMS-INSERT-MSG                                            
046800     END-IF                                                               
046900     MOVE ZERO TO RETURN-CODE                                             
047000**      CALL FELLOG                                                       
047100     GOBACK                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 A-INIT-SPARA-INPUT SECTION.                                              
047500                                                                          
047600     IF MSG-DUBBLA-TRANSKODER                                             
047700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30201                 
047800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
047900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
048000     ELSE                                                                 
048100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30201                  
048200       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
048300       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
048400     END-IF                                                               
048500                                                                          
048600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
048800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048900     MOVE LOW-VALUE   TO W-WDH1A1KY-MIN-X                                 
049000     MOVE HIGH-VALUE  TO W-WDH1A1KY-MAX-X                                 
049100                                                                          
049200     MOVE LOW-VALUE TO MSG-AREA                                           
049300     MOVE 'W5O30201' TO MFS-IDMOD                                         
049400     MOVE '5302' TO MOD-IDTRANS                                           
049500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
049600                                                                          
049700     IF NOT EGEN-MID                                                      
049800       MOVE SPACE TO MFS-KDTRTYP                                          
049900       MOVE '7' TO MFS-IDPFK                                              
050000     END-IF                                                               
050100                                                                          
050200     IF SWEDISH-TEXT                                                      
050300       MOVE +1 TO M-SW-KDMFSTYP                                           
050400       MOVE +1 TO SPRAK-IX                                                
050500       MOVE 'S  ' TO MED-IDSKYLT                                          
050600     ELSE                                                                 
050700       MOVE +2 TO M-SW-KDMFSTYP                                           
050800       MOVE +2 TO SPRAK-IX                                                
050900       MOVE 'GB ' TO MED-IDSKYLT                                          
051000     END-IF                                                               
051100     MOVE NEJ TO FLINVSKR-SW                                              
051200     .                                                                    
051300 B-KOLLA-NYCKLAR SECTION.                                                 
051400     SKIP2                                                                
051500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
051600     MOVE '001'             TO MSGI-KDCALL                                
051700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
051800     MOVE '5302'            TO MSGI-IDTRANS                               
051900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
052000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
052100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
052200     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
052300     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
052400                                                                          
052500      MOVE JA TO NYCKLAR-SW                                               
052600                                                                          
052700*     -- KONTROLL AV IDDC                                                 
052800      MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                 
052900                                                                          
053000      MOVE MSGI-IDDC     TO W-IDDC-UNIK                                   
053100                            W-IDDC-MIN                                    
053200                            W-IDDC-MAX                                    
053300                            W-SEQA-IDDC-MIN                               
053400                            W-SEQA-IDDC-MAX                               
053500                            W-IDDC-B6                                     
053600      PERFORM IMS-GU-WDB601                                               
053700                                                                          
053800      IF  DCS-CDC-TR                                                      
053900        MOVE NEJ TO NYCKLAR-SW                                            
054000      END-IF                                                              
054100                                                                          
054200*     -- KONTROLL AV ADLAGOMR                                             
054300      MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                             
054400                                                                          
054500     IF MID-ADLAGOMR-IN = ALL '+'                                         
054600       MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                                
054700       INSPECT WS-ADLAGOMR REPLACING LEADING SPACE BY ZERO                
054800     ELSE                                                                 
054900       MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                                
055000       MOVE '7'             TO MFS-IDPFK                                  
055100       MOVE SPACE           TO MFS-KDTRTYP                                
055200     END-IF                                                               
055300     IF WS-ADLAGOMR NOT NUMERIC                                           
055400       MOVE NEJ TO NYCKLAR-SW                                             
055500     END-IF                                                               
055600                                                                          
055700*     -- KONTROLL AV ADGANG                                               
055800     MOVE MFS-RENSA-FAELT TO MOD-ADGANG-IN                                
055900                                                                          
056000     IF MID-ADGANG-IN = ALL '+'                                           
056100        MOVE MID-ADGANG-UT TO WS-ADGANG                                   
056200        INSPECT WS-ADGANG REPLACING LEADING SPACE BY ZERO                 
056300     ELSE                                                                 
056400        MOVE MID-ADGANG-IN TO WS-ADGANG                                   
056500        MOVE '7'           TO MFS-IDPFK                                   
056600       MOVE SPACE          TO MFS-KDTRTYP                                 
056700     END-IF                                                               
056800     IF WS-ADGANG NOT NUMERIC                                             
056900        MOVE NEJ TO NYCKLAR-SW                                            
057000     END-IF                                                               
057100                                                                          
057200*     -- KONTROLL AV ADPLATS                                              
057300     MOVE MFS-RENSA-FAELT TO MOD-ADPLATS-IN                               
057400                                                                          
057500     IF MID-ADPLATS-IN = ALL '+'                                          
057600        MOVE MID-ADPLATS-UT TO WS-ADPLATS                                 
057700        INSPECT WS-ADPLATS REPLACING LEADING SPACE BY ZERO                
057800     ELSE                                                                 
057900        MOVE MID-ADPLATS-IN TO WS-ADPLATS                                 
058000        MOVE '7'            TO MFS-IDPFK                                  
058100        MOVE SPACE          TO MFS-KDTRTYP                                
058200     END-IF                                                               
058300     IF WS-ADPLATS NOT NUMERIC                                            
058400        MOVE NEJ TO NYCKLAR-SW                                            
058500     END-IF                                                               
058600                                                                          
058700*     -- KONTROLL AV KDINVPRIO                                            
058800     MOVE MFS-RENSA-FAELT TO MOD-KDINVPRIO-IN                             
058900                                                                          
059000     IF MID-KDINVPRIO-IN = ALL '+'                                        
059100        MOVE MID-KDINVPRIO-UT TO WS-KDINVPRIO                             
059200        INSPECT WS-KDINVPRIO REPLACING LEADING SPACE BY ZERO              
059300     ELSE                                                                 
059400        MOVE MID-KDINVPRIO-IN TO WS-KDINVPRIO                             
059500        MOVE '7'              TO MFS-IDPFK                                
059600        MOVE SPACE            TO MFS-KDTRTYP                              
059700     END-IF                                                               
059800     IF WS-KDINVPRIO = 0 OR 1 OR 2                                        
059900        CONTINUE                                                          
060000     ELSE                                                                 
060100        MOVE NEJ TO NYCKLAR-SW                                            
060200     END-IF                                                               
060300                                                                          
060400*     -- KONTROLL AV KDVVKL                                               
060500     MOVE MFS-RENSA-FAELT TO MOD-KDVVKL-IN                                
060600                                                                          
060700     IF MID-KDVVKL-IN = ALL '+'                                           
060800        MOVE MID-KDVVKL-UT TO WS-KDVVKL                                   
060900*       INSPECT WS-KDVVKL REPLACING LEADING SPACE BY ZERO                 
061000     ELSE                                                                 
061100        MOVE MID-KDVVKL-IN TO WS-KDVVKL                                   
061200        MOVE '7'           TO MFS-IDPFK                                   
061300        MOVE SPACE         TO MFS-KDTRTYP                                 
061400     END-IF                                                               
061500*    IF WS-KDVVKL NOT NUMERIC                                             
061600*       MOVE NEJ TO NYCKLAR-SW                                            
061700*    END-IF                                                               
061800                                                                          
061900*     -- KONTROLL AV KDINVKAT                                             
062000     MOVE MFS-RENSA-FAELT TO MOD-KDINVKAT-IN                              
062100                                                                          
062200     IF MID-KDINVKAT-IN = ALL '+'                                         
062300        MOVE MID-KDINVKAT-UT TO WS-KDINVKAT                               
062400        INSPECT WS-KDINVKAT REPLACING LEADING SPACE BY ZERO               
062500     ELSE                                                                 
062600        MOVE MID-KDINVKAT-IN TO WS-KDINVKAT                               
062700        MOVE '7'             TO MFS-IDPFK                                 
062800        MOVE SPACE           TO MFS-KDTRTYP                               
062900     END-IF                                                               
063000     IF WS-KDINVKAT NOT NUMERIC                                           
063100        MOVE NEJ TO NYCKLAR-SW                                            
063200     END-IF                                                               
063300                                                                          
063400*     -- KONTROLL AV FLINVSKR                                             
063500     MOVE MFS-RENSA-FAELT TO MOD-FLINVSKR-IN                              
063600                                                                          
063700     IF MID-FLINVSKR-IN = ALL '+'                                         
063800       IF MID-FLINVSKR-UT = 'Y'                                           
063900         MOVE JA              TO WS-FLINVSKR                              
064000         INSPECT WS-FLINVSKR REPLACING LEADING SPACE BY ZERO              
064100         MOVE JA TO FLINVSKR-SW                                           
064200       ELSE                                                               
064300         MOVE MID-FLINVSKR-UT TO WS-FLINVSKR                              
064400         INSPECT WS-FLINVSKR REPLACING LEADING SPACE BY ZERO              
064500       END-IF                                                             
064600     ELSE                                                                 
064700       IF MID-FLINVSKR-IN = 'Y'                                           
064800         MOVE JA              TO WS-FLINVSKR                              
064900         MOVE JA TO FLINVSKR-SW                                           
065000       ELSE                                                               
065100         MOVE MID-FLINVSKR-IN TO WS-FLINVSKR                              
065200       END-IF                                                             
065300       MOVE '7'               TO MFS-IDPFK                                
065400       MOVE SPACE             TO MFS-KDTRTYP                              
065500     END-IF                                                               
065600                                                                          
065700*     -- KONTROLL AV KDPRODSL                                             
065800     MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-IN                              
065900                                                                          
066000     IF MID-KDPRODSL-IN = ALL '+'                                         
066100        MOVE MID-KDPRODSL-UT TO WS-KDPRODSL                               
066200        INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO               
066300     ELSE                                                                 
066400        MOVE MID-KDPRODSL-IN TO WS-KDPRODSL                               
066500        MOVE '7'             TO MFS-IDPFK                                 
066600        MOVE SPACE           TO MFS-KDTRTYP                               
066700     END-IF                                                               
066800     IF WS-KDPRODSL NOT NUMERIC                                           
066900        MOVE NEJ TO NYCKLAR-SW                                            
067000       MOVE '7' TO MOD-TEMFSINF                                           
067100     END-IF                                                               
067200                                                                          
067300*     -- KONTROLL AV IDFKNGRP                                             
067400     MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-IN                              
067500                                                                          
067600     IF MID-IDFKNGRP-IN = ALL '+'                                         
067700        MOVE MID-IDFKNGRP-UT TO WS-IDFKNGRP                               
067800        INSPECT WS-IDFKNGRP REPLACING LEADING SPACE BY ZERO               
067900     ELSE                                                                 
068000        MOVE MID-IDFKNGRP-IN TO WS-IDFKNGRP                               
068100        MOVE '7'             TO MFS-IDPFK                                 
068200        MOVE SPACE           TO MFS-KDTRTYP                               
068300     END-IF                                                               
068400     IF WS-IDFKNGRP NOT NUMERIC                                           
068500        MOVE NEJ TO NYCKLAR-SW                                            
068600     END-IF                                                               
068700                                                                          
068800*     -- KONTROLL AV TIREGDAT                                             
068900     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
069000                                                                          
069100     IF MID-TIREGDAT-IN = ALL '+'                                         
069200        MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                               
069300        INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO               
069400     ELSE                                                                 
069500        MOVE MID-TIREGDAT-IN TO WS-TIREGDAT                               
069600        MOVE '7'             TO MFS-IDPFK                                 
069700        MOVE SPACE           TO MFS-KDTRTYP                               
069800     END-IF                                                               
069900     IF WS-TIREGDAT NOT NUMERIC                                           
070000        MOVE NEJ TO NYCKLAR-SW                                            
070100     END-IF                                                               
070200                                                                          
070300*     -- KONTROLL AV IDARTNR                                              
070400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
070500                                                                          
070600     IF MID-IDARTNR-IN = ALL '+'                                          
070700        MOVE MID-IDARTNR-UT TO WS-IDARTNR                                 
070800        INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                
070900     ELSE                                                                 
071000        MOVE MID-IDARTNR-IN TO WS-IDARTNR                                 
071100        MOVE '7'            TO MFS-IDPFK                                  
071200        MOVE SPACE          TO MFS-KDTRTYP                                
071300     END-IF                                                               
071400      IF WS-IDARTNR NOT NUMERIC                                           
071500        MOVE NEJ TO NYCKLAR-SW                                            
071600      END-IF                                                              
071700                                                                          
071800*     -- KONTROLL AV FLURVAL                                              
071900     MOVE MFS-RENSA-FAELT TO MOD-FLURVAL-IN                               
072000                                                                          
072100     IF MID-FLURVAL-IN = ALL '+'                                          
072200        IF MID-FLURVAL-UT = SPACE                                         
072300           MOVE SPACE          TO WS-FLURVAL                              
072400        ELSE                                                              
072500           MOVE MID-FLURVAL-UT TO WS-FLURVAL                              
072600        END-IF                                                            
072700     ELSE                                                                 
072800        MOVE MID-FLURVAL-IN TO WS-FLURVAL                                 
072900        MOVE '7'            TO MFS-IDPFK                                  
073000        MOVE SPACE          TO MFS-KDTRTYP                                
073100     END-IF                                                               
073200     IF WS-FLURVAL = 'J' OR 'N' OR SPACE                                  
073300        CONTINUE                                                          
073400     ELSE                                                                 
073500        MOVE NEJ TO NYCKLAR-SW                                            
073600     END-IF                                                               
073700                                                                          
073800*     -- KONTROLL AV IDLISTNR , 1 ELLER 2                                 
073900                                                                          
074000     IF MID-IDLISTNR = '1' OR '2'                                         
074100       IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-NA                             
074200         MOVE '2' TO MOD-IDLISTNR-UT                                      
074300       ELSE                                                               
074400         MOVE '1' TO MOD-IDLISTNR-UT                                      
074500       END-IF                                                             
074600         MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-IDLISTNR-ATTR              
074700     ELSE                                                                 
074800       MOVE NEJ TO NYCKLAR-SW                                             
074900     END-IF                                                               
075000                                                                          
075100     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
075200                                                                          
075300     IF EGEN-MID OR NYCKLAR-OK                                            
075400        MOVE WS-ADLAGOMR    TO MOD-ADLAGOMR-UT                            
075500        INSPECT MOD-ADLAGOMR-UT REPLACING LEADING ZERO BY SPACE           
075600        MOVE WS-ADGANG    TO MOD-ADGANG-UT                                
075700        INSPECT MOD-ADGANG-UT REPLACING LEADING ZERO BY SPACE             
075800        MOVE WS-ADPLATS   TO MOD-ADPLATS-UT                               
075900        INSPECT MOD-ADPLATS-UT REPLACING LEADING ZERO BY SPACE            
076000        MOVE WS-KDINVPRIO TO MOD-KDINVPRIO-UT                             
076100        INSPECT MOD-KDINVPRIO-UT REPLACING LEADING ZERO BY SPACE          
076200        MOVE WS-KDVVKL   TO MOD-KDVVKL-UT                                 
076300*       INSPECT MOD-KDVVKL-UT REPLACING LEADING ZERO BY SPACE             
076400        MOVE WS-KDINVKAT  TO MOD-KDINVKAT-UT                              
076500        INSPECT MOD-KDINVKAT-UT REPLACING LEADING ZERO BY SPACE           
076600        IF FLINVSKR-Y                                                     
076700          MOVE 'Y'         TO MOD-FLINVSKR-UT                             
076800        ELSE                                                              
076900          MOVE WS-FLINVSKR TO MOD-FLINVSKR-UT                             
077000        END-IF                                                            
077100        INSPECT MOD-FLINVSKR-UT REPLACING LEADING ZERO BY SPACE           
077200        MOVE WS-KDPRODSL  TO MOD-KDPRODSL-UT                              
077300        INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE           
077400        MOVE WS-IDFKNGRP  TO MOD-IDFKNGRP-UT                              
077500        INSPECT MOD-IDFKNGRP-UT REPLACING LEADING ZERO BY SPACE           
077600        MOVE WS-TIREGDAT  TO MOD-TIREGDAT-UT                              
077700        INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE           
077800        MOVE WS-IDARTNR   TO MOD-IDARTNR-UT                               
077900        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
078000        MOVE WS-FLURVAL   TO MOD-FLURVAL-UT                               
078100     ELSE                                                                 
078200        MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UT                           
078300                                MOD-ADGANG-UT                             
078400                                MOD-ADPLATS-UT                            
078500                                MOD-KDINVPRIO-UT                          
078600                                MOD-KDVVKL-UT                             
078700                                MOD-KDINVKAT-UT                           
078800                                MOD-FLINVSKR-UT                           
078900                                MOD-KDPRODSL-UT                           
079000                                MOD-IDFKNGRP-UT                           
079100                                MOD-TIREGDAT-UT                           
079200                                MOD-IDARTNR-UT                            
079300                                MOD-FLURVAL-UT                            
079400     END-IF                                                               
079500     IF NOT EGEN-MID                                                      
079600       MOVE '002'         TO MSGI-KDCALL                                  
079700       MOVE '5302'        TO MSGI-IDTRANS                                 
079800       MOVE SPACE TO SAVE-AREA                                            
079900       MOVE SAVE-AREA     TO MSGI-SPAR-AREA                               
080000       CALL W005INIT USING MSGI-WMSGINIT                                  
080100            USEA-PCB                                                      
080200     END-IF                                                               
080300                                                                          
080400     IF ENGLISH-TEXT                                                      
080500       MOVE 'GB ' TO MED-IDSKYLT                                          
080600       MOVE 'N' TO MFS-KDHUVOMR                                           
080700     ELSE                                                                 
080800       MOVE 'S  ' TO MED-IDSKYLT                                          
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 C-KOLLA-INDATA-UPPDATERING SECTION.                                      
081300     MOVE JA TO INDATA-SW                                                 
081400     MOVE +1 TO IX                                                        
081500     IF MID-KVINVSKR-5302 NUMERIC                                         
081600        IF MID-KVINVSKR-5302 > ZERO AND < +11                             
081700           IF MID-IDARTNR-UTSKR(IX) = ' '                                 
081800              MOVE ERR-NO-UPDATE         TO MED-IDMFSFEL                  
081900              CALL WMEDKONV              USING MED-WMEDAREA               
082000              MOVE MED-MFSFEL            TO MOD-TEMFSFEL                  
082100              MOVE NEJ TO INDATA-SW                                       
082200           END-IF                                                         
082300        ELSE                                                              
082400           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
082500           CALL WMEDKONV              USING MED-WMEDAREA                  
082600           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
082700           MOVE NEJ TO INDATA-SW                                          
082800        END-IF                                                            
082900     ELSE                                                                 
083000        MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                        
083100        CALL WMEDKONV              USING MED-WMEDAREA                     
083200        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
083300        MOVE NEJ TO INDATA-SW                                             
083400     END-IF                                                               
083500     IF INDATA-FEL                                                        
083600        MOVE MFS-NUM-FAELT-FEL TO MOD-KVINVSKR-ATTR                       
083700     END-IF                                                               
083800                                                                          
083900     MOVE SPACE TO WS-IDPRTLST                                            
084000     IF MID-IDNODE = ALL '+'                                              
084100        CONTINUE                                                          
084200     ELSE                                                                 
084300        MOVE '002'             TO PRT-KDCALL                              
084400        MOVE MID-IDNODE        TO PRT-IDLTERM                             
084500        CALL W006PRT USING PRT-W006PRT                                    
084600        IF PRT-KDSVAR = 'R'                                               
084700           MOVE PRT-IDPRTLST TO WS-IDPRTLST                               
084800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDNODE-ATTR                   
084900           MOVE MFS-ROER-EJ-FAELT TO MOD-IDNODE-UT                        
085000        ELSE                                                              
085100           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDNODE-ATTR                     
085200           MOVE MFS-ROER-EJ-FAELT TO MOD-IDNODE-UT                        
085300           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
085400           CALL WMEDKONV              USING MED-WMEDAREA                  
085500           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
085600           MOVE NEJ TO INDATA-SW                                          
085700        END-IF                                                            
085800     END-IF                                                               
085900                                                                          
086000     IF DCS-CDC                                                           
086100        IF MID-IDLISTNR = '2'                                             
086200           IF WS-IDARTNR > ZERO                                           
086300              MOVE WS-IDARTNR TO W-IDARTNR-ARTC                           
086400              PERFORM IMS-GU-WLARTC01                                     
086500              IF SEGMENT-FINNS                                            
086600                 PERFORM IMS-GNP-WLARTC11                                 
086700                 IF SEGMENT-FINNS                                         
086800                   IF (ARTC11-CLAG-ADLAGOMR > ZERO)                       
086900                   AND (ARTC11-CLAG-ADLAGOMR-SVS > ZERO)                  
087000                      MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLISTNR-ATTR        
087100                      MOVE NEJ TO INDATA-SW                               
087200                      MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL          
087300                      CALL WMEDKONV USING MED-WMEDAREA                    
087400                      MOVE MED-MFSFEL TO MOD-TEMFSFEL                     
087500                   END-IF                                                 
087600                 END-IF                                                   
087700              END-IF                                                      
087800           ELSE                                                           
087900              MOVE +1 TO IX                                               
088000              PERFORM UNTIL (IX > MAX-IX) OR                              
088100                      (MID-IDARTNR-UTSKR(IX) = ' ')                       
088200                 MOVE MID-IDARTNR-UTSKR(IX) TO WS-IDARTNR-REDIG           
088300                 INSPECT WS-IDARTNR-REDIG REPLACING                       
088400                                             LEADING SPACE BY ZERO        
088500                 MOVE WS-IDARTNR-REDIG TO W-IDARTNR-ARTC                  
088600                 PERFORM IMS-GU-WLARTC01                                  
088700                 IF SEGMENT-FINNS                                         
088800                    PERFORM IMS-GNP-WLARTC11                              
088900                    IF SEGMENT-FINNS                                      
089000                       IF (ARTC11-CLAG-ADLAGOMR > ZERO)                   
089100                       AND (ARTC11-CLAG-ADLAGOMR-SVS > ZERO)              
089200                           MOVE NEJ TO INDATA-SW                          
089300                           MOVE MFS-ALFA-FAELT-FEL                        
089400                                         TO MOD-IDLISTNR-ATTR             
089500                        MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL        
089600                           CALL WMEDKONV USING MED-WMEDAREA               
089700                           MOVE MED-MFSFEL TO MOD-TEMFSFEL                
089800                       END-IF                                             
089900                    END-IF                                                
090000                 END-IF                                                   
090100                 ADD +1 TO IX                                             
090200              END-PERFORM                                                 
090300           END-IF                                                         
090400        END-IF                                                            
090500     END-IF                                                               
090600     IF INDATA-FEL                                                        
090700        PERFORM MFS-ROER-EJ-MOD-RAD                                       
090800     END-IF                                                               
090900                                                                          
091000     .                                                                    
091100     EJECT                                                                
091200 D-SKICKA-ART-TILL-UTSKRIFT SECTION.                                      
091300                                                                          
091400     MOVE +1 TO IX                                                        
091500     MOVE +0 TO IND                                                       
091600     PERFORM DA-INITIERA-ALT-AREA                                         
091700     PERFORM DAA-INITIERA-ALT2-AREA                                       
091800     MOVE '01' TO MID-KVINVSKR-PRINT                                      
091900                                                                          
092000     IF WS-IDARTNR > ZERO                                                 
092100        MOVE +1        TO IND                                             
092200        PERFORM DB-REDIGERA-FLYTTA-ART                                    
092300        MOVE INF-PRINTING-REQ TO MED-IDMFSINF                             
092400        CALL WMEDKONV USING MED-WMEDAREA                                  
092500        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
092600        IF MID-IDLISTNR = '1'                                             
092700          PERFORM IMS-INSERT-ALT-MSG                                      
092800        ELSE                                                              
092900          PERFORM IMS-INSERT-ALT2-MSG                                     
093000        END-IF                                                            
093100     ELSE                                                                 
093200        PERFORM UNTIL (IND = MID-KVINVSKR-5302) OR                        
093300                      (IX > MAX-IX)             OR                        
093400                      (MID-IDARTNR-UTSKR(IX) = ' ')                       
093500           PERFORM DC-KOLLA-PRINTKOD-WDH1                                 
093600                                                                          
093700           IF INV-FLINVSKR = 'N'                                          
093800              ADD +1 TO IND                                               
093900              PERFORM DB-REDIGERA-FLYTTA-ART                              
094000           END-IF                                                         
094100           ADD +1 TO IX                                                   
094200        END-PERFORM                                                       
094300                                                                          
094400        IF IND > +0                                                       
094500           IF MID-IDLISTNR = '1'                                          
094600             PERFORM IMS-INSERT-ALT-MSG                                   
094700             PERFORM IMS-PURGE-ALT-MSG                                    
094800             MOVE INF-PRINTING-REQ TO MED-IDMFSINF                        
094900             CALL WMEDKONV USING MED-WMEDAREA                             
095000             MOVE MED-MFSINF TO MOD-TEMFSINF                              
095100           ELSE                                                           
095200             PERFORM IMS-INSERT-ALT2-MSG                                  
095300             PERFORM IMS-PURGE-ALT2-MSG                                   
095400             MOVE 'PRINT LISTA2'   TO MOD-TEMFSINF                        
095500*            CALL WMEDKONV USING MED-WMEDAREA                             
095600*            MOVE MED-MFSINF TO MOD-TEMFSINF                              
095700           END-IF                                                         
095800        ELSE                                                              
095900           MOVE INF-NO-PRINTING TO MED-IDMFSINF                           
096000           CALL WMEDKONV USING MED-WMEDAREA                               
096100           MOVE MED-MFSINF TO MOD-TEMFSINF                                
096200        END-IF                                                            
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 DA-INITIERA-ALT-AREA SECTION.                                            
096700                                                                          
096800     COMPUTE M-SW-LL = LENGTH OF MID-W5I30101 + 17                        
096900     MOVE ZERO            TO MID-KVINVSKR                                 
097000     MOVE ZERO            TO MID-ADLAGOMR                                 
097100     MOVE MSGI-IDDC       TO MID-IDDC                                     
097200     MOVE WS-IDPRTLST     TO MID-IDPRTLST                                 
097300     MOVE MSGI-IDUSER     TO MID-IDUSER                                   
097400     MOVE +1              TO ALT-IND                                      
097500     PERFORM 10 TIMES                                                     
097600        MOVE ALL '+'      TO MID-IDARTNR-PRINT   (ALT-IND)                
097700        MOVE SPACE        TO MID-KDINVPRIO-PRINT (ALT-IND)                
097800        MOVE ZERO         TO MID-KDINVKAT-PRINT (ALT-IND)                 
097900        ADD +1            TO ALT-IND                                      
098000     END-PERFORM                                                          
098100     .                                                                    
098200     EJECT                                                                
098300 DAA-INITIERA-ALT2-AREA SECTION.                                          
098400                                                                          
098500     COMPUTE M-SW-LL-2 = LENGTH OF 5392-MID-W5I39201 + 17                 
098600     MOVE ZERO            TO 5392-MID-KVINVSKR                            
098700     MOVE ZERO            TO 5392-MID-ADLAGOMR                            
098800     MOVE MSGI-IDDC       TO 5392-MID-IDDC                                
098900     MOVE WS-IDPRTLST     TO 5392-MID-IDPRTLST                            
099000     MOVE +1              TO ALT2-IND                                     
099100     PERFORM 10 TIMES                                                     
099200        MOVE ALL '+'      TO 5392-MID-IDARTNR-PRINT   (ALT2-IND)          
099300        MOVE SPACE        TO 5392-MID-KDINVPRIO-PRINT (ALT2-IND)          
099400        MOVE ZERO         TO 5392-MID-KDINVKAT-PRINT (ALT2-IND)           
099500        ADD +1            TO ALT2-IND                                     
099600     END-PERFORM                                                          
099700     .                                                                    
099800     EJECT                                                                
099900 DB-REDIGERA-FLYTTA-ART SECTION.                                          
100000                                                                          
100100     MOVE MID-IDARTNR-UTSKR(IX)      TO WS-IDARTNR-REDIG                  
100200     INSPECT WS-IDARTNR-REDIG REPLACING LEADING SPACE BY ZERO             
100300     MOVE WS-IDARTNR-REDIG           TO W-IDARTNR                         
100400                                        MID-IDARTNR-PRINT  (IND)          
100500                                   5392-MID-IDARTNR-PRINT  (IND)          
100600                                        W-IDARTNR-ARTC                    
100700                                                                          
100800     IF MID-KDINVPRIO-UTSKR(IX) = SPACE                                   
100900        MOVE '2'                     TO MID-KDINVPRIO-PRINT(IND)          
101000        MOVE '2'                TO 5392-MID-KDINVPRIO-PRINT(IND)          
101100     ELSE                                                                 
101200        MOVE MID-KDINVPRIO-UTSKR(IX) TO MID-KDINVPRIO-PRINT(IND)          
101300                                   5392-MID-KDINVPRIO-PRINT(IND)          
101400     END-IF                                                               
101500     MOVE MID-KDINVKAT-UTSKR(IX)     TO WS-KDINVKAT-REDIG                 
101600     INSPECT WS-KDINVKAT-REDIG REPLACING LEADING SPACE BY ZERO            
101700     MOVE WS-KDINVKAT-REDIG          TO MID-KDINVKAT-PRINT(IND)           
101800     MOVE WS-KDINVKAT-REDIG        TO 5392-MID-KDINVKAT-PRINT(IND)        
101900     .                                                                    
102000     EJECT                                                                
102100 DC-KOLLA-PRINTKOD-WDH1 SECTION.                                          
102200                                                                          
102300     MOVE MID-IDARTNR-UTSKR(IX)      TO WS-IDARTNR-REDIG                  
102400     INSPECT WS-IDARTNR-REDIG REPLACING LEADING SPACE BY ZERO             
102500     MOVE WS-IDARTNR-REDIG           TO W-IDARTNR                         
102600                                        W-IDARTNR-ARTC                    
102700                                                                          
102800     IF MID-KDINVPRIO-UTSKR(IX) = SPACE                                   
102900        MOVE +2                      TO WS-KDINVPRIO                      
103000     ELSE                                                                 
103100        MOVE MID-KDINVPRIO-UTSKR(IX) TO WS-KDINVPRIO                      
103200     END-IF                                                               
103300     MOVE MID-KDINVKAT-UTSKR(IX)     TO WS-KDINVKAT-REDIG                 
103400     INSPECT WS-KDINVKAT-REDIG REPLACING LEADING SPACE BY ZERO            
103500     MOVE     WS-KDINVKAT-REDIG      TO W-KDINVKAT-MIN                    
103600                                        W-KDINVKAT-MAX                    
103700     PERFORM IMS-GET-INVENTERINGS-ROT                                     
103800     PERFORM IMS-GNP-INVENTERING                                          
103900     .                                                                    
104000     EJECT                                                                
104100 E-LAES-ARTIKEL SECTION.                                                  
104200     MOVE 'E-LEAS-ARTIKEL'         TO WS-SECTION                          
104300     MOVE WS-IDARTNR TO W-IDARTNR                                         
104400     PERFORM IMS-GET-INVENTERINGS-ROT                                     
104500     IF SEGMENT-FINNS                                                     
104600       MOVE ART-IDARTNR TO WS-IDARTNR-SPAR                                
104700       IF WS-KDINVPRIO = ZERO                                             
104800          PERFORM IMS-GNP-INVENTERING                                     
104900          IF SEGMENT-FINNS                                                
105000             IF (INV-KDINVPRIO = +1 OR +2) AND                            
105100                (INV-KDINVKAT NOT = 6)                                    
105200                CONTINUE                                                  
105300             ELSE                                                         
105400                MOVE 'GE' TO STATUS-WS                                    
105500                CONTINUE                                                  
105600             END-IF                                                       
105700          END-IF                                                          
105800       ELSE                                                               
105900         MOVE WS-KDINVPRIO TO W-KDINVPRIO                                 
106000         PERFORM IMS-GNP-INVENTERINGS-PRIO                                
106100       END-IF                                                             
106200     ELSE                                                                 
106300       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
106400       CALL WMEDKONV USING MED-WMEDAREA                                   
106500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
106600       PERFORM MFS-RENSA                                                  
106700     END-IF                                                               
106800                                                                          
106900     IF SEGMENT-FINNS                                                     
107000        MOVE W-IDARTNR TO W-IDARTNR-ARTC                                  
107100        PERFORM S99-KOLLA-URVAL                                           
107200        IF SW-URVAL-OK = NEJ                                              
107300           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
107400           CALL WMEDKONV USING MED-WMEDAREA                               
107500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
107600           PERFORM MFS-RENSA                                              
107700        END-IF                                                            
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 F-LAES-FOERSTA SECTION.                                                  
108200     MOVE 'F-LAES-FOERSTA'         TO WS-SECTION                          
108300     MOVE INF-FIRST-PAGE   TO MED-IDMFSINF                                
108400     CALL WMEDKONV         USING MED-WMEDAREA                             
108500     MOVE MED-MFSINF       TO MOD-TEMFSINF                                
108600     .                                                                    
108700     SKIP2                                                                
108800 G-LAES-NAESTA SECTION.                                                   
108900*                                                                         
109000                                                                          
109100     IF SAVE-IDTRANS = '5302'                                             
109200        MOVE SAVE-IDDC-NEXT        TO W-SEQA-IDDC-MIN                     
109300                                      W-SEQA-IDDC-MAX                     
109400        MOVE SAVE-ADLAGOMR-NEXT    TO W-SEQA-ADLAGOMR-MIN                 
109500        MOVE SAVE-ADGANG-NEXT      TO W-SEQA-ADGANG-MIN                   
109600        MOVE SAVE-ADPLATS-NEXT     TO W-SEQA-ADPLATS-MIN                  
109700        MOVE SAVE-KDINVPRIO-NEXT   TO W-SEQA-KDINVPRIO-MIN                
109800        MOVE SAVE-KDVVKL-NEXT      TO W-SEQA-KDVVKL-MIN                   
109900        MOVE SAVE-IDARTNR-NEXT     TO W-SEQA-IDARTNR-MIN                  
110000        MOVE SAVE-KDINVKAT-NEXT    TO W-SEQA-KDINVKAT-MIN                 
110100     END-IF                                                               
110200     .                                                                    
110300     EJECT                                                                
110400 H-LAES-SAMMA SECTION.                                                    
110500                                                                          
110600     IF SAVE-IDTRANS = '5302'                                             
110700         MOVE SAVE-IDDC-ENTER       TO W-SEQA-IDDC-MIN                    
110800                                       W-SEQA-IDDC-MAX                    
110900         MOVE SAVE-ADLAGOMR-ENTER   TO W-SEQA-ADLAGOMR-MIN                
111000         MOVE SAVE-ADGANG-ENTER     TO W-SEQA-ADGANG-MIN                  
111100         MOVE SAVE-ADPLATS-ENTER    TO W-SEQA-ADPLATS-MIN                 
111200         MOVE SAVE-KDINVPRIO-ENTER  TO W-SEQA-KDINVPRIO-MIN               
111300         MOVE SAVE-KDVVKL-ENTER     TO W-SEQA-KDVVKL-MIN                  
111400         MOVE SAVE-IDARTNR-ENTER    TO W-SEQA-IDARTNR-MIN                 
111500         MOVE SAVE-KDINVKAT-ENTER   TO W-SEQA-KDINVKAT-MIN                
111600         MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                       
111700         CALL WMEDKONV USING MED-WMEDAREA                                 
111800         MOVE MED-MFSINF            TO MOD-TEMFSINF                       
111900     END-IF                                                               
112000     .                                                                    
112100     EJECT                                                                
112200 S01-FIXA-ARTIKEL-NYCKEL SECTION.                                         
112300*                                                                         
112400                                                                          
112500     MOVE SAVE-IDARTNR-NEXT TO WS-IDARTNR-REDIG                           
112600     INSPECT WS-IDARTNR-REDIG REPLACING LEADING SPACE BY ZERO             
112700     MOVE WS-IDARTNR-REDIG TO W-IDARTNR                                   
112800     MOVE SAVE-KDINVKAT-NEXT TO WS-KDINVKAT-REDIG                         
112900     INSPECT WS-KDINVKAT-REDIG REPLACING LEADING SPACE BY ZERO            
113000     MOVE WS-KDINVKAT-REDIG  TO W-KDINVKAT-MIN                            
113100                                W-KDINVKAT-MAX                            
113200     MOVE SAVE-KDINVPRIO-NEXT TO WS-KDINVPRIO                             
113300     MOVE MSGI-IDDC           TO W-IDDC-MIN                               
113400                                W-IDDC-MAX                                
113500     .                                                                    
113600     EJECT                                                                
113700 S02-LAES-VISA-INFO SECTION.                                              
113800*                                                                         
113900     MOVE 'S02-LAES-VISA-INFO' TO WS-SECTION                              
114000     MOVE +1 TO INDX                                                      
114100                                                                          
114200     PERFORM IMS-GET-WDH1-WITH-SECOND-FIRST                               
114300     MOVE JA TO FIRST-SW                                                  
114400     PERFORM UNTIL INDX > MAX-IX                                          
114500        IF SEGMENT-FINNS                                                  
114600          IF KEY-ADLAGOMR = ZERO OR                                       
114700             (KEY-ADLAGOMR = SEQA-ADLAGOMR)                               
114800           IF KEY-ADGANG <= SEQA-ADGANG OR                                
114900             (KEY-ADGANG = ZERO)                                          
115000            IF KEY-ADPLATS <= SEQA-ADPLATS OR                             
115100              (KEY-ADPLATS = ZERO)                                        
115200             IF KEY-KDINVPRIO = SEQA-KDINVPRIO OR                         
115300               (KEY-KDINVPRIO = ZERO)                                     
115400*             IF KEY-KDVVKL = SEQA-KDVVKL OR                              
115500*               (KEY-KDVVKL = ZERO)                                       
115510              IF KEY-KDVVKL = SEQA-KDVVKL OR                              
115520                (WS-KDVVKL = SPACE)                                       
115600               IF KEY-KDINVKAT = SEQA-KDINVKAT OR                         
115700                 (KEY-KDINVKAT = ZERO) AND                                
115800                 (SEQA-KDINVKAT NOT = 6)                                  
115900                IF WS-FLINVSKR = SEQA-FLINVSKR OR                         
116000                  (WS-FLINVSKR = '0')                                     
116100                 IF KEY-KDPRODSL = SEQA-KDPRODSL OR                       
116200                   (KEY-KDPRODSL = ZERO)                                  
116300                  IF KEY-IDFKNGRP = SEQA-IDFKNGRP OR                      
116400                    (KEY-IDFKNGRP = ZERO)                                 
116500                     MOVE SEQA-TISEGKEY   TO WS-FIX-TISEGKEY              
116600                   MOVE KEY-TIREGDAT      TO TMP1-YYMMDD                  
116700                   MOVE WS-TISEGKEY-3-8   TO TMP2-YYMMDD                  
116800                   PERFORM WY2000P1                                       
116900                   IF TMP1-YYMMDD >= TMP2-YYMMDD                          
117000                     OR (KEY-TIREGDAT = ZERO)                             
117100                     IF SEQA-FLINVBEH = NEJ AND                           
117200                        SEQA-KDINVKAT NOT = +8                            
117300                       PERFORM S024-KONTROLLERA-IDPRTINV                  
117400                       MOVE SEQA-IDARTNR TO W-IDARTNR-ARTC                
117500                       PERFORM S99-KOLLA-URVAL                            
117600                       IF ALLT-OK AND (SW-URVAL-OK = JA)                  
119300                         PERFORM S021-SKRIV-RAD                           
119400                         ADD +1 TO INDX                                   
119600                         IF FIRST-TIME                                    
119700                           MOVE '5302'       TO SAVE-IDTRANS              
119800                           MOVE SEQA-IDDC    TO SAVE-IDDC-ENTER           
119900                                                  SAVE-IDDC-NEXT          
120000                           MOVE SEQA-ADLAGOMR TO                          
120100                                            SAVE-ADLAGOMR-ENTER           
120200                                            SAVE-ADLAGOMR-NEXT            
120300                           MOVE SEQA-ADGANG  TO SAVE-ADGANG-ENTER         
120400                                                  SAVE-ADGANG-NEXT        
120500                           MOVE SEQA-ADPLATS TO SAVE-ADPLATS-ENTER        
120600                                                SAVE-ADPLATS-NEXT         
120700                           MOVE SEQA-KDINVPRIO TO                         
120800                                SAVE-KDINVPRIO-ENTER                      
120900                                SAVE-KDINVPRIO-NEXT                       
121000                           MOVE SEQA-KDVVKL   TO SAVE-KDVVKL-ENTER        
121100                                                 SAVE-KDVVKL-NEXT         
121200                           MOVE SEQA-IDARTNR  TO                          
121300                                             SAVE-IDARTNR-ENTER           
121400                                             SAVE-IDARTNR-NEXT            
121500                           MOVE SEQA-KDINVKAT TO                          
121600                                             SAVE-KDINVKAT-ENTER          
121700                                             SAVE-KDINVKAT-NEXT           
121800                           IF MFS-FIRST                                   
121900                             MOVE ZERO        TO SAVE-RAKNARE             
122000                           END-IF                                         
122100                           MOVE '002'         TO MSGI-KDCALL              
122200                           MOVE '5302'        TO MSGI-IDTRANS             
122300                           MOVE SAVE-AREA     TO MSGI-SPAR-AREA           
122400                           CALL W005INIT USING MSGI-WMSGINIT              
122500                                USEA-PCB                                  
122600                           MOVE NEJ TO FIRST-SW                           
122700                         END-IF                                           
122800                       END-IF                                             
122900                     END-IF                                               
123000                   END-IF                                                 
123100                  END-IF                                                  
123200                 END-IF                                                   
123300                END-IF                                                    
123400               END-IF                                                     
123500              END-IF                                                      
123600             END-IF                                                       
123700            END-IF                                                        
123800           END-IF                                                         
123900          END-IF                                                          
124000          PERFORM IMS-GET-WDH1-WITH-SECOND-INDEX                          
124100        ELSE                                                              
124200          IF INDX = 1                                                     
124300            MOVE ERR-PART-MISSING        TO MED-IDMFSFEL                  
124400            CALL WMEDKONV                USING MED-WMEDAREA               
124500            MOVE MED-MFSFEL              TO MOD-TEMFSFEL                  
124600            MOVE MFS-RENSA-FAELT         TO MOD-TEMFSINF                  
124700          END-IF                                                          
124800          PERFORM MFS-RENSA-RAD                                           
124900          ADD +1 TO INDX                                                  
125000        END-IF                                                            
125100     END-PERFORM                                                          
125200                                                                          
125300     MOVE NEJ TO SW-SIDA-FULL                                             
125400     IF SEGMENT-FINNS                                                     
125500        MOVE JA TO SW-SIDA-FULL                                           
125600        PERFORM UNTIL SEGMENT-SAKNAS OR KONTROLL-OK = JA                  
125700          PERFORM S022-KONTROLLERA-VILLKOR                                
125800          IF KONTROLL-OK = NEJ                                            
125900             PERFORM IMS-GET-WDH1-WITH-SECOND-INDEX                       
126000          END-IF                                                          
126100        END-PERFORM                                                       
126200        IF SEGMENT-SAKNAS                                                 
126300           IF MFS-NEXT                                                    
126400               MOVE INF-LAST-PAGE      TO MED-IDMFSINF                    
126500               CALL WMEDKONV           USING MED-WMEDAREA                 
126600               MOVE MED-MFSINF         TO MOD-TEMFSINF                    
126700           END-IF                                                         
126800        ELSE                                                              
126900          IF KONTROLL-OK = JA                                             
127000             MOVE '5302'         TO SAVE-IDTRANS                          
127100             MOVE SEQA-IDDC      TO SAVE-IDDC-NEXT                        
127200             MOVE SEQA-ADLAGOMR  TO SAVE-ADLAGOMR-NEXT                    
127300             MOVE SEQA-ADGANG    TO SAVE-ADGANG-NEXT                      
127400             MOVE SEQA-ADPLATS   TO SAVE-ADPLATS-NEXT                     
127500             MOVE SEQA-KDINVPRIO TO SAVE-KDINVPRIO-NEXT                   
127600             MOVE SEQA-KDVVKL    TO SAVE-KDVVKL-NEXT                      
127700             MOVE SEQA-IDARTNR   TO SAVE-IDARTNR-NEXT                     
127800             MOVE SEQA-KDINVKAT  TO SAVE-KDINVKAT-NEXT                    
127900             IF MFS-ENTER OR MFS-NEXT                                     
128000                MOVE INF-MORE-INFO-PF8  TO MED-IDMFSINF                   
128100                CALL WMEDKONV           USING MED-WMEDAREA                
128200                MOVE MED-MFSINF         TO MOD-TEMFSINF                   
128300             END-IF                                                       
128400             IF MFS-ENTER OR MFS-FIRST                                    
128500               MOVE 10 TO RAKNARE                                         
128600               PERFORM UNTIL SEGMENT-SAKNAS                               
128700                 IF KONTROLL-OK = JA                                      
130000                     ADD +1 TO RAKNARE                                    
130200                 END-IF                                                   
130300                 PERFORM IMS-GET-WDH1-WITH-SECOND-INDEX                   
130400                 PERFORM S022-KONTROLLERA-VILLKOR                         
130500               END-PERFORM                                                
130600               MOVE RAKNARE TO MOD-KVANT-ART                              
130700                               SAVE-RAKNARE                               
130800             ELSE                                                         
130900               MOVE SAVE-RAKNARE TO MOD-KVANT-ART                         
131000             END-IF                                                       
131100             MOVE '002'          TO MSGI-KDCALL                           
131200             MOVE '5302'         TO MSGI-IDTRANS                          
131300             MOVE SAVE-AREA      TO MSGI-SPAR-AREA                        
131400             CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                   
131500          ELSE                                                            
131600             IF MFS-NEXT                                                  
131700                 MOVE SAVE-RAKNARE TO MOD-KVANT-ART                       
131800                 MOVE INF-LAST-PAGE      TO MED-IDMFSINF                  
131900                 CALL WMEDKONV           USING MED-WMEDAREA               
132000                 MOVE MED-MFSINF         TO MOD-TEMFSINF                  
132100             END-IF                                                       
132200          END-IF                                                          
132300        END-IF                                                            
132400     ELSE                                                                 
132500        IF MFS-NEXT                                                       
132600           MOVE SAVE-RAKNARE TO MOD-KVANT-ART                             
132700           MOVE INF-LAST-PAGE      TO MED-IDMFSINF                        
132800           CALL WMEDKONV           USING MED-WMEDAREA                     
132900           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
133000        END-IF                                                            
133100     END-IF                                                               
133200     MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                                 
133300     .                                                                    
133400     EJECT                                                                
133500 S11-LAES-VISA-INFO-ART SECTION.                                          
133600*                                                                         
133700     MOVE 'S11-LAES-VISA-INFO-ART' TO WS-SECTION                          
133800     MOVE JA TO SW-URVAL-OK                                               
133900     IF SEGMENT-FINNS                                                     
134000        MOVE WS-IDARTNR-SPAR TO W-IDARTNR-ARTC                            
134100        PERFORM S99-KOLLA-URVAL                                           
134200     END-IF                                                               
134300                                                                          
134400     MOVE +1 TO INDX                                                      
134500                                                                          
134600     IF SEGMENT-FINNS AND (INV-FLINVBEH = NEJ)                            
134700     AND (SW-URVAL-OK = JA)                                               
134800       MOVE '5302'          TO SAVE-IDTRANS                               
134900       MOVE INV-IDDC        TO SAVE-IDDC-ENTER                            
135000                               SAVE-IDDC-NEXT                             
135100       MOVE INV-ADLAGOMR    TO SAVE-ADLAGOMR-ENTER                        
135200                               SAVE-ADLAGOMR-NEXT                         
135300       MOVE INV-ADGANG      TO SAVE-ADGANG-ENTER                          
135400                               SAVE-ADGANG-NEXT                           
135500       MOVE INV-ADPLATS     TO SAVE-ADPLATS-ENTER                         
135600                               SAVE-ADPLATS-NEXT                          
135700       MOVE INV-KDINVPRIO   TO SAVE-KDINVPRIO-ENTER                       
135800                               SAVE-KDINVPRIO-NEXT                        
135900       MOVE INV-KDVVKL      TO SAVE-KDVVKL-ENTER                          
136000                               SAVE-KDVVKL-NEXT                           
136100       MOVE WS-IDARTNR-SPAR TO SAVE-IDARTNR-ENTER                         
136200                               SAVE-IDARTNR-NEXT                          
136300       MOVE INV-KDINVKAT    TO SAVE-KDINVKAT-ENTER                        
136400                               SAVE-KDINVKAT-NEXT                         
136500       MOVE '002'           TO MSGI-KDCALL                                
136600       MOVE '5302'          TO MSGI-IDTRANS                               
136700       MOVE SAVE-AREA       TO MSGI-SPAR-AREA                             
136800       CALL W005INIT USING MSGI-WMSGINIT                                  
136900            USEA-PCB                                                      
137000       MOVE 'GE' TO STATUS-WS                                             
137100       PERFORM S025-SKRIV-RAD                                             
137200       ADD +1 TO INDX                                                     
137300     ELSE                                                                 
137400       MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                             
137500       CALL WMEDKONV USING MED-WMEDAREA                                   
137600       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
137700       MOVE MFS-RENSA-FAELT   TO MOD-TEMFSINF                             
137800     END-IF                                                               
137900                                                                          
138000     PERFORM UNTIL INDX > MAX-IX                                          
138100        PERFORM MFS-RENSA-RAD                                             
138200        ADD +1 TO INDX                                                    
138300     END-PERFORM                                                          
138400                                                                          
138500     MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                                 
138600*                            MOD-FLAGGA                                   
138700     .                                                                    
138800     EJECT                                                                
138900                                                                          
139000 S021-SKRIV-RAD SECTION.                                                  
139100*                                                                         
139200     MOVE 'S021-SKRIV-RAD'         TO WS-SECTION                          
139300     MOVE SEQA-ADLAGOMR TO WS-ADLAGOMR-NUM                                
139400     MOVE WS-ADLAGOMR-NUM TO MOD-ADLAGOMR-UTSKR  (INDX)                   
139500     MOVE SEQA-ADGANG   TO MOD-ADGANG-UTSKR   (INDX)                      
139600     MOVE SEQA-ADPLATS  TO MOD-ADPLATS-UTSKR  (INDX)                      
139700     IF DCS-CDC                                                           
139800        IF SEQA-KDINVPRIO = +2                                            
139900           MOVE +0 TO MOD-KDINVPRIO-UTSKR (INDX)                          
140000        ELSE                                                              
140100           MOVE SEQA-KDINVPRIO TO MOD-KDINVPRIO-UTSKR (INDX)              
140200        END-IF                                                            
140300     ELSE                                                                 
140400        MOVE SEQA-KDINVPRIO TO MOD-KDINVPRIO-UTSKR (INDX)                 
140500     END-IF                                                               
140600     IF NOT DCS-NDC-NA                                                    
140700       MOVE SEQA-KDVVKL   TO MOD-KDVVKL-UTSKR   (INDX)                    
140800     END-IF                                                               
140900     MOVE SEQA-KDINVKAT TO WS-KDINVKAT-NUM                                
141000     MOVE WS-KDINVKAT-NUM TO MOD-KDINVKAT-UTSKR  (INDX)                   
141100     MOVE SEQA-FLINVSKR TO MOD-FLINVSKR-UTSKR  (INDX)                     
141200     MOVE SEQA-KDPRODSL TO MOD-KDPRODSL-UTSKR  (INDX)                     
141300     MOVE SEQA-IDFKNGRP TO MOD-IDFKNGRP-UTSKR  (INDX)                     
141400     MOVE SEQA-TISEGKEY TO WS-FIX-TISEGKEY                                
141500     MOVE WS-TISEGKEY-3-8 TO MOD-TIREGDAT-UTSKR  (INDX)                   
141600     MOVE SEQA-IDARTNR  TO MOD-IDARTNR-UTSKR   (INDX)                     
141700     MOVE ZERO          TO MOD-KVANT-ART                                  
141800* - -NDC ANPASSNING RS/970128                                             
141900     IF DCS-NDC-NA                                                        
142000       IF SEQA-FLINVSKR = 'J'                                             
142100         MOVE 'Y'  TO MOD-FLINVSKR-UTSKR  (INDX)                          
142200       END-IF                                                             
142300*      MOVE ZERO         TO MOD-KDVVKL-UTSKR   (INDX)                     
142400       MOVE SEQA-IDARTNR TO W-IDARTNR                                     
142500                            W-IDARTNR-ARTC                                
142600       PERFORM IMS-GU-WLARTC01                                            
142700       IF SEGMENT-FINNS                                                   
142800         PERFORM IMS-GNP-WLARTC11                                         
142900         IF SEGMENT-FINNS                                                 
143000           MOVE '  LPC'   TO MOD-HEADING                                  
143100           MOVE ARTC11-CLAG-KDPSLLOC TO MOD-KDPSLLOC-UTSKR (INDX)         
143200          END-IF                                                          
143300       ELSE                                                               
143400         MOVE 00 TO MOD-KDPSLLOC-UTSKR (INDX)                             
143500       END-IF                                                             
143600     END-IF                                                               
143700                                                                          
143800     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-FLER(INDX)                        
143900     IF DCS-CDC                                                           
144000        MOVE SEQA-IDARTNR TO W-IDARTNR-ARTC                               
144100        PERFORM IMS-GU-WLARTC01                                           
144200        IF SEGMENT-FINNS                                                  
144300           PERFORM IMS-GNP-WLARTC11                                       
144400           IF SEGMENT-FINNS                                               
144500              IF (ARTC11-CLAG-ADLAGOMR NOT = ZERO)                        
144600              AND (ARTC11-CLAG-ADLAGOMR-SVS NOT = ZERO)                   
144700                 MOVE 'J' TO MOD-FLAGGA-FLER(INDX)                        
144800              END-IF                                                      
144900           END-IF                                                         
145000        END-IF                                                            
145100     END-IF                                                               
145200     .                                                                    
145300     EJECT                                                                
145400 S022-KONTROLLERA-VILLKOR SECTION.                                        
145500*                                                                         
145600     MOVE 'S022-KONTROLLERA-VILLKOR' TO WS-SECTION                        
145700     MOVE NEJ TO KONTROLL-OK                                              
145800     IF KEY-ADLAGOMR = ZERO OR                                            
145900       (KEY-ADLAGOMR = SEQA-ADLAGOMR)                                     
146000        IF KEY-ADGANG <= SEQA-ADGANG OR                                   
146100          (KEY-ADGANG = ZERO)                                             
146200           IF KEY-ADPLATS <= SEQA-ADPLATS OR                              
146300             (KEY-ADPLATS = ZERO)                                         
146400            IF KEY-KDINVPRIO = SEQA-KDINVPRIO OR                          
146500              (KEY-KDINVPRIO = ZERO)                                      
146600*            IF KEY-KDVVKL = SEQA-KDVVKL OR                               
146700*              (KEY-KDVVKL = ZERO)                                        
146710             IF KEY-KDVVKL = SEQA-KDVVKL OR                               
146720               (WS-KDVVKL = SPACE)                                        
146800              IF KEY-KDINVKAT = SEQA-KDINVKAT OR                          
146900                (KEY-KDINVKAT = ZERO) AND                                 
147000                (SEQA-KDINVKAT NOT = 6)                                   
147100               IF WS-FLINVSKR = SEQA-FLINVSKR OR                          
147200                 (WS-FLINVSKR = '0')                                      
147300                IF KEY-KDPRODSL = SEQA-KDPRODSL OR                        
147400                  (KEY-KDPRODSL = ZERO)                                   
147500                 IF KEY-IDFKNGRP = SEQA-IDFKNGRP OR                       
147600                   (KEY-IDFKNGRP = ZERO)                                  
147700                   MOVE SEQA-TISEGKEY TO WS-FIX-TISEGKEY                  
147800                  MOVE KEY-TIREGDAT      TO TMP1-YYMMDD                   
147900                  MOVE WS-TISEGKEY-3-8   TO TMP2-YYMMDD                   
148000                  PERFORM WY2000P1                                        
148100                  IF TMP1-YYMMDD >= TMP2-YYMMDD                           
148200                     OR (KEY-TIREGDAT = ZERO)                             
148300                    IF SEQA-FLINVBEH = NEJ                                
148400                      MOVE JA TO KONTROLL-OK                              
148500                      MOVE SEQA-IDARTNR TO W-IDARTNR-ARTC                 
148600                      MOVE STATUS-WS TO SPAR-STATUS-WS                    
148700                      PERFORM S99-KOLLA-URVAL                             
148800                      MOVE SPAR-STATUS-WS TO STATUS-WS                    
148900                      IF SW-URVAL-OK = JA                                 
149000                         MOVE JA TO KONTROLL-OK                           
149100                      ELSE                                                
149200                         MOVE NEJ TO KONTROLL-OK                          
149300                      END-IF                                              
149400                    END-IF                                                
149500                  END-IF                                                  
149600                 END-IF                                                   
149700                END-IF                                                    
149800               END-IF                                                     
149900              END-IF                                                      
150000             END-IF                                                       
150100            END-IF                                                        
150200           END-IF                                                         
150300        END-IF                                                            
150400     END-IF                                                               
150500     .                                                                    
150600     EJECT                                                                
150700 S024-KONTROLLERA-IDPRTINV SECTION.                                       
150800     MOVE 'S024-KONTROLLERA-IDPRT' TO WS-SECTION                          
150900     MOVE JA TO ALLT-OK-SW                                                
151000     MOVE SEQA-IDARTNR    TO W-IDARTNR                                    
151100     MOVE SEQA-IDDC       TO W-IDDC-MIN-KY                                
151200                             W-IDDC-MAX-KY                                
151300     MOVE SEQA-KDINVKAT   TO W-KDINVKAT-MIN-KY                            
151400                             W-KDINVKAT-MAX-KY                            
151500     MOVE SEQA-TISEGKEY   TO W-TISEGKEY-MIN-KY                            
151600                             W-TISEGKEY-MAX-KY                            
151700     PERFORM IMS-GET-INVENTERINGS-ROT                                     
151800     IF SEGMENT-FINNS                                                     
151900       PERFORM IMS-GN-WDH111                                              
152000       IF SEGMENT-FINNS                                                   
152100         IF INVA-INV-IDPRTOMG > 0 AND INVA-INV-FLINVSKR = NEJ             
152200           MOVE NEJ TO ALLT-OK-SW                                         
152300         END-IF                                                           
152400       ELSE                                                               
152500         MOVE NEJ TO ALLT-OK-SW                                           
152600       END-IF                                                             
152700     ELSE                                                                 
152800       MOVE NEJ TO ALLT-OK-SW                                             
152900     END-IF                                                               
153000     .                                                                    
153100     EJECT                                                                
153200 S025-SKRIV-RAD SECTION.                                                  
153300     MOVE 'S025-SKRIV-RAD'         TO WS-SECTION                          
153400     MOVE INV-ADLAGOMR  TO WS-ADLAGOMR-NUM                                
153500     MOVE WS-ADLAGOMR-NUM TO MOD-ADLAGOMR-UTSKR  (INDX)                   
153600     MOVE INV-ADGANG    TO MOD-ADGANG-UTSKR   (INDX)                      
153700     MOVE INV-ADPLATS   TO MOD-ADPLATS-UTSKR  (INDX)                      
153800     IF DCS-CDC                                                           
153900        IF INV-KDINVPRIO = +2                                             
154000           MOVE +0 TO MOD-KDINVPRIO-UTSKR (INDX)                          
154100        ELSE                                                              
154200           MOVE INV-KDINVPRIO TO MOD-KDINVPRIO-UTSKR (INDX)               
154300        END-IF                                                            
154400     ELSE                                                                 
154500        MOVE INV-KDINVPRIO TO MOD-KDINVPRIO-UTSKR (INDX)                  
154600     END-IF                                                               
154700     MOVE INV-KDVVKL    TO MOD-KDVVKL-UTSKR   (INDX)                      
154800     MOVE INV-KDINVKAT  TO WS-KDINVKAT-NUM                                
154900     MOVE WS-KDINVKAT-NUM TO MOD-KDINVKAT-UTSKR  (INDX)                   
155000     MOVE INV-FLINVSKR  TO MOD-FLINVSKR-UTSKR   (INDX)                    
155100     MOVE INV-KDPRODSL  TO MOD-KDPRODSL-UTSKR   (INDX)                    
155200     MOVE INV-IDFKNGRP  TO MOD-IDFKNGRP-UTSKR   (INDX)                    
155300     MOVE INV-TISEGKEY  TO WS-FIX-TISEGKEY                                
155400     MOVE WS-TISEGKEY-3-8 TO MOD-TIREGDAT-UTSKR (INDX)                    
155500     MOVE WS-IDARTNR-SPAR TO MOD-IDARTNR-UTSKR  (INDX)                    
155600     MOVE ZERO          TO MOD-KVANT-ART                                  
155700* - -NDC ANPASSNING RS/970128                                             
155800     IF DCS-NDC-NA                                                        
155900       IF INV-FLINVSKR = 'J'                                              
156000         MOVE 'Y'  TO MOD-FLINVSKR-UTSKR  (INDX)                          
156100       END-IF                                                             
156200       MOVE WS-IDARTNR-SPAR TO W-IDARTNR                                  
156300                               W-IDARTNR-ARTC                             
156400       PERFORM IMS-GU-WLARTC01                                            
156500       IF SEGMENT-FINNS                                                   
156600         PERFORM IMS-GNP-WLARTC11                                         
156700         IF SEGMENT-FINNS                                                 
156800           MOVE 'LPC  '   TO MOD-HEADING                                  
156900           MOVE ARTC11-CLAG-KDPSLLOC TO MOD-KDPSLLOC-UTSKR (INDX)         
157000          END-IF                                                          
157100       ELSE                                                               
157200         MOVE 00 TO MOD-KDPSLLOC-UTSKR (INDX)                             
157300       END-IF                                                             
157400     END-IF                                                               
157500                                                                          
157600     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-FLER(INDX)                        
157700     IF DCS-CDC                                                           
157800        MOVE WS-IDARTNR-SPAR TO W-IDARTNR-ARTC                            
157900        PERFORM IMS-GU-WLARTC01                                           
158000        IF SEGMENT-FINNS                                                  
158100           PERFORM IMS-GNP-WLARTC11                                       
158200           IF SEGMENT-FINNS                                               
158300              IF (ARTC11-CLAG-ADLAGOMR NOT = ZERO)                        
158400              AND (ARTC11-CLAG-ADLAGOMR-SVS NOT = ZERO)                   
158500                 MOVE 'J' TO MOD-FLAGGA-FLER(INDX)                        
158600              END-IF                                                      
158700          END-IF                                                          
158800        END-IF                                                            
158900     END-IF                                                               
159000     .                                                                    
159100     EJECT                                                                
159200 S99-KOLLA-URVAL SECTION.                                                 
159300     MOVE 'S99-KOLLA-UTVAL'        TO WS-SECTION                          
159400     MOVE JA TO SW-URVAL-OK                                               
159500                                                                          
159600     IF DCS-CDC                                                           
159700        PERFORM IMS-GU-WLARTC01                                           
159800        IF SEGMENT-FINNS                                                  
159900           PERFORM IMS-GNP-WLARTC11                                       
160000           IF SEGMENT-FINNS                                               
160100              IF (ARTC11-CLAG-ADLAGOMR NOT = ZERO)                        
160200              AND (ARTC11-CLAG-ADLAGOMR-SVS NOT = ZERO)                   
160300                 IF WS-FLURVAL = NEJ                                      
160400                    MOVE NEJ TO SW-URVAL-OK                               
160500                 END-IF                                                   
160600              ELSE                                                        
160700                 IF WS-FLURVAL = JA                                       
160800                    MOVE NEJ TO SW-URVAL-OK                               
160900                 END-IF                                                   
161000              END-IF                                                      
161100           END-IF                                                         
161200        END-IF                                                            
161300     END-IF                                                               
161400     .                                                                    
161500     EJECT                                                                
161600 MFS-RENSA SECTION.                                                       
161700*                                                                         
161800     MOVE +1 TO INDX                                                      
161900                                                                          
162000     PERFORM 10 TIMES                                                     
162100        MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR-UTSKR (INDX)               
162200                                  MOD-ADGANG-UTSKR (INDX)                 
162300                                  MOD-ADPLATS-UTSKR (INDX)                
162400                                  MOD-KDINVPRIO-UTSKR (INDX)              
162500                                  MOD-KDVVKL-UTSKR (INDX)                 
162600                                  MOD-IDARTNR-UTSKR (INDX)                
162700                                  MOD-KDINVKAT-UTSKR (INDX)               
162800                                  MOD-FLINVSKR-UTSKR (INDX)               
162900                                  MOD-KDPRODSL-UTSKR (INDX)               
163000                                  MOD-IDFKNGRP-UTSKR (INDX)               
163100                                  MOD-TIREGDAT-UTSKR (INDX)               
163200                                  MOD-KDPSLLOC-UTSKR (INDX)               
163300                                  MOD-FLAGGA-FLER(INDX)                   
163400        ADD +1 TO INDX                                                    
163500     END-PERFORM                                                          
163600     .                                                                    
163700     SKIP2                                                                
163800 MFS-RENSA-RAD SECTION.                                                   
163900*                                                                         
164000        MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR-UTSKR (INDX)               
164100                                  MOD-ADGANG-UTSKR (INDX)                 
164200                                  MOD-ADPLATS-UTSKR (INDX)                
164300                                  MOD-KDINVPRIO-UTSKR (INDX)              
164400                                  MOD-KDVVKL-UTSKR (INDX)                 
164500                                  MOD-IDARTNR-UTSKR (INDX)                
164600                                  MOD-KDINVKAT-UTSKR (INDX)               
164700                                  MOD-FLINVSKR-UTSKR (INDX)               
164800                                  MOD-KDPRODSL-UTSKR (INDX)               
164900                                  MOD-IDFKNGRP-UTSKR (INDX)               
165000                                  MOD-TIREGDAT-UTSKR (INDX)               
165100                                  MOD-KDPSLLOC-UTSKR (INDX)               
165200                                  MOD-FLAGGA-FLER (INDX)                  
165300     .                                                                    
165400     SKIP2                                                                
165500 MFS-ROER-EJ-MOD-RAD SECTION.                                             
165600*                                                                         
165700*    MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR-NEXT                        
165800*                                 MOD-KDINVPRIO-NEXT                      
165900*                                 MOD-KDINVKAT-NEXT                       
166000*                                 MOD-KVINVSKR                            
166100*                                 MOD-FLAGGA                              
166200     MOVE MFS-ROER-EJ-FAELT    TO MOD-KVANT-ART                           
166300     MOVE +1 TO INDX                                                      
166400                                                                          
166500     PERFORM 10 TIMES                                                     
166600        MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-UTSKR (INDX)               
166700                                  MOD-ADGANG-UTSKR (INDX)                 
166800                                  MOD-ADPLATS-UTSKR (INDX)                
166900                                  MOD-KDINVPRIO-UTSKR (INDX)              
167000                                  MOD-KDVVKL-UTSKR (INDX)                 
167100                                  MOD-IDARTNR-UTSKR (INDX)                
167200                                  MOD-KDINVKAT-UTSKR (INDX)               
167300                                  MOD-FLINVSKR-UTSKR (INDX)               
167400                                  MOD-KDPRODSL-UTSKR (INDX)               
167500                                  MOD-IDFKNGRP-UTSKR (INDX)               
167600                                  MOD-TIREGDAT-UTSKR (INDX)               
167700                                  MOD-KDPSLLOC-UTSKR (INDX)               
167800                                  MOD-FLAGGA-FLER (INDX)                  
167900        ADD +1 TO INDX                                                    
168000     END-PERFORM                                                          
168100     .                                                                    
168200     EJECT                                                                
168300* IMS SEKTIONER                                                           
168400     SKIP3                                                                
168500 IMS-GET-MSG SECTION.                                                     
168600     MOVE '  QC' TO GODK-STATUSKODER                                      
168700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
168800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168900     PERFORM IMS-STATUSKONTROLL                                           
169000     .                                                                    
169100     SKIP3                                                                
169200 IMS-INSERT-MSG SECTION.                                                  
169300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
169400     MOVE SPACE TO GODK-STATUSKODER                                       
169500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
169600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
169700     PERFORM IMS-STATUSKONTROLL                                           
169800     .                                                                    
169900     SKIP3                                                                
170000 IMS-INSERT-ALT-MSG SECTION.                                              
170100     MOVE SPACE TO GODK-STATUSKODER                                       
170200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
170300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
170400     PERFORM IMS-STATUSKONTROLL                                           
170500     .                                                                    
170600     SKIP3                                                                
170700 IMS-INSERT-ALT2-MSG SECTION.                                             
170800     MOVE SPACE TO GODK-STATUSKODER                                       
170900     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW2                  
171000     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
171100     PERFORM IMS-STATUSKONTROLL                                           
171200     .                                                                    
171300     SKIP3                                                                
171400 IMS-PURGE-ALT-MSG SECTION.                                               
171500     MOVE SPACE TO GODK-STATUSKODER                                       
171600     CALL CBLTDLI USING PURG ALT-PCB                                      
171700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     EJECT                                                                
172100 IMS-PURGE-ALT2-MSG SECTION.                                              
172200     MOVE SPACE TO GODK-STATUSKODER                                       
172300     CALL CBLTDLI USING PURG ALT2-PCB                                     
172400     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
172500     PERFORM IMS-STATUSKONTROLL                                           
172600     .                                                                    
172700     EJECT                                                                
172800 IMS-GN-WDH111  SECTION.                                                  
172900     MOVE 'IMS-GN-WDH111'          TO WS-SECTION                          
173000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
173100              DELIMITED BY SIZE INTO SSA1                                 
173200     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN                          
173300                    '&WDH111KY<=' W-WDH111KY-MAX ')'                      
173400              DELIMITED BY SIZE INTO SSA2                                 
173500     MOVE '  GE' TO GODK-STATUSKODER                                      
173600     CALL CBLTDLI USING GN INVA-PCB DLI-IO-WDH111 SSA1 SSA2               
173700     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
173800     PERFORM IMS-STATUSKONTROLL                                           
173900     .                                                                    
174000     SKIP2                                                                
174100 IMS-GET-INVENTERINGS-ROT  SECTION.                                       
174200     MOVE 'IMS-GET-INVENTRINGS-ROT' TO WS-SECTION                         
174300                                                                          
174400     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
174500              DELIMITED BY SIZE INTO SSA1                                 
174600     MOVE '  GE' TO GODK-STATUSKODER                                      
174700     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA SSA1                      
174800     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100     SKIP2                                                                
175200 IMS-GNP-INVENTERING  SECTION.                                            
175300     MOVE 'IMS-GNP-INVENTERING'    TO WS-SECTION                          
175400     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
175500                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
175600                    '&FLINVBEH =' NEJ ')'                                 
175700              DELIMITED BY SIZE INTO SSA1                                 
175800     MOVE '  GE' TO GODK-STATUSKODER                                      
175900     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA SSA1                     
176000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
176100     PERFORM IMS-STATUSKONTROLL                                           
176200     .                                                                    
176300     SKIP2                                                                
176400 IMS-GNP-INVENTERINGS-PRIO SECTION.                                       
176500     MOVE 'IMS-GNP-INVENTRINGS-PRIO' TO WS-SECTION                        
176600     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
176700                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
176800                    '&KDINVPRI =' W-KDINVPRIO-X                           
176900                    '&FLINVBEH =' NEJ ')'                                 
177000              DELIMITED BY SIZE INTO SSA1                                 
177100     MOVE '  GE' TO GODK-STATUSKODER                                      
177200     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA SSA1                     
177300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
177400     PERFORM IMS-STATUSKONTROLL                                           
177500     .                                                                    
177600     EJECT                                                                
177700 IMS-GET-WDH1-WITH-SECOND-FIRST  SECTION.                                 
177800     MOVE 'IMS-GET-WDH1-SECOND-FIRST' TO WS-SECTION                       
177900     STRING 'WDH1A1  *F(WDH1A1KY>=' W-WDH1A1KY-MIN-X                      
178000                      '&WDH1A1KY<=' W-WDH1A1KY-MAX-X ')'                  
178100              DELIMITED BY SIZE INTO SSA1                                 
178200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
178300     CALL CBLTDLI USING GU INVB-PCB DLI-IO-AREA3 SSA1                     
178400     MOVE INVB-STATUS-CODE TO STATUS-WS                                   
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700     SKIP2                                                                
178800 IMS-GET-WDH1-WITH-SECOND-INDEX  SECTION.                                 
178900     MOVE 'IMS-GET-WDH1-SECOND-INDEX' TO WS-SECTION                       
179000                                                                          
179100     STRING 'WDH1A1  (WDH1A1KY>=' W-WDH1A1KY-MIN-X                        
179200                    '&WDH1A1KY<=' W-WDH1A1KY-MAX-X ')'                    
179300              DELIMITED BY SIZE INTO SSA1                                 
179400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
179500     CALL CBLTDLI USING GN INVB-PCB DLI-IO-AREA3 SSA1                     
179600     MOVE INVB-STATUS-CODE TO STATUS-WS                                   
179700     PERFORM IMS-STATUSKONTROLL                                           
179800     .                                                                    
179900     SKIP2                                                                
180000 IMS-GU-WLARTC01 SECTION.                                                 
180100                                                                          
180200     STRING 'WLARTC01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
180300            DELIMITED BY SIZE INTO SSA1                                   
180400     MOVE '  GE' TO GODK-STATUSKODER                                      
180500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
180600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
180700     PERFORM IMS-STATUSKONTROLL                                           
180800     .                                                                    
180900     SKIP2                                                                
181000 IMS-GNP-WLARTC11 SECTION.                                                
181100                                                                          
181200     STRING 'WLARTC01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
181300            DELIMITED BY SIZE INTO SSA1                                   
181400     MOVE 'WLARTC11 '           TO SSA2                                   
181500     MOVE '  GE' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1 SSA2               
181700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     SKIP2                                                                
182100 IMS-GU-WDB601    SECTION.                                                
182200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     MOVE '  ' TO GODK-STATUSKODER                                        
182500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
182600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     SKIP2                                                                
183000 IMS-STATUSKONTROLL SECTION.                                              
183100     SET STATUS-IX TO 1                                                   
183200     SEARCH GODK-STATUS AT END CALL FELLOG                                
183300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
183400     END-SEARCH                                                           
183500     .                                                                    
183600     EJECT                                                                
183700*    -COPY WY2000P1                                                       
