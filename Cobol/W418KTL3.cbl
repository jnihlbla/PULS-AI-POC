000101*COMPOPT STDSUB=YES                                                       
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.     W418KTL3.                                                
000401 AUTHOR.         OLSSON SUSANNE.                                          
000501 DATE-WRITTEN.   09/11/05.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    FUNKTION:                                                            
000901*        PROGRAMMET ÄR ETT SUBPROGRAM SOM KONTROLLERAR OM DET ÄR          
001001*        OK ATT RETURNERA BEGÄRDA RETURARTIKLAR PÅ EN LEV.ANM.            
001101*        MOT EN RETURMATRIS (WDP8).                                       
001201*                                                                         
001300*    SVARSKODER:                                                          
001401*        KDSVAR = Y - OK ARTIKELN FINNS I MATRISEN.                       
001501*                     RETUREN SKALL BEHANDLAS ENLIGT BILD 4752.           
001601*                     A=NORMAL RETURBEHANDLING                            
001701*                     S=STOPPAD RETUR, POSTEN NEKAS.                      
001801*                     Q=UPPKÖAD RETURBEGÄRAN FÖR EXTRA KONTROLL.          
001802*                     P=Q DETTA GÄLLER BARA FARLIGT GODS                  
001803*                         OCH BARA OM DET ÄR ETT PSN NR. SOM FÅR          
001804*                         TRANSPORTERAS MED BIL.                          
001805*                         MAN SER FORTFARANDE Q PÅ RADEN MEN FÅR          
001806*                         ETT ANNAT MEDDELANDE I DETTA FALL.              
001901*                                                                         
002001*        KDSVAR = N - ARTIKELN SAKNAS I MATRISEN.                         
002101*                     RETUREN BEHANDLAS SOM VANLIGT.                      
002201*                                                                         
002301*        KDSVAR = S - FEL/ DC,ARTIKEL ELLER KUND SAKNAS PÅ BASEN.         
002401*                     RETUREN BEHANDLAS VIA VANLIGA KONTROLLEN.           
002501*                                                                         
002601*                                                                         
003000*        PROGRAMMET LÄSER      WDA8 (PROCSEQ=WDA8A)                       
003100*        PROGRAMMET LÄSER      WDB2                                       
003200*        PROGRAMMET LÄSER      WDK6                                       
003300*        PROGRAMMET LÄSER      WDK7                                       
003400*        PROGRAMMET LÄSER      WDB6                                       
003500*                                                                         
003600*    ABENDKODER:                                                          
003700*        U0016 -  . . . .                                                 
003800*        U1000 -  . . . .                                                 
003900*                                                                         
003910* CHANGE LOG:                                                             
003920*        E-TRACKER 8635407  20091021 RETURN CODES MATRIX                  
003930*        E-TRACKER 9436342  20100505 ERROR CORRECTIONS                    
003940*        E-TRACKER 9494057  20100527 EXCEPTION FOR USA/CAN KOD 98         
003950*                                    OCH FG 8616 PAINTED BUMPERS          
003951*        E-TRACKER 9822116  20101014 DISCRAPANCY/RETURNS HAZ.MAT.         
003952*        E-TRACKER 10268175 20160222 OTHER LANG.ON DG-DOK.                
003960*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300                                                                          
004400 DATA DIVISION.                                                           
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W418KTL3'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
005300 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
005400 77  WC-IDARTNR          PIC X(16)   VALUE 'IDARTNR         '.            
005500 77  WC-KDFARLIG         PIC X(16)   VALUE 'KDFARLIG        '.            
005600 77  WC-KDSORT           PIC X(16)   VALUE 'KDSORT          '.            
005700 77  WC-IDLEVNR          PIC X(16)   VALUE 'IDLEVNR         '.            
005800 77  WC-IDFKNGRP         PIC X(16)   VALUE 'IDFKNGRP        '.            
005900 77  WC-KDPRODSL         PIC X(16)   VALUE 'KDPRODSL        '.            
006000 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
006100 77  MAX-IX                      PIC S9(3)  VALUE +14  COMP-3.            
006200                                                                          
006300 77  INDATA-SW                   PIC X      VALUE 'J'.                    
006400     88 INDATA-OK                           VALUE 'J'.                    
006500     88 INDATA-FEL                          VALUE 'N'.                    
006600                                                                          
006701 77  WDK711-SW                   PIC X      VALUE 'J'.                    
006801     88 WDK711-FINNS                        VALUE 'J'.                    
006901     88 WDK711-SAKNAS                       VALUE 'N'.                    
007001                                                                          
007002 77  FG-ADR-SW                   PIC X      VALUE 'N'.                    
007003     88 FG-ADR-FINNS                        VALUE 'J'.                    
007004     88 FG-ADR-FINNS-EJ                     VALUE 'N'.                    
007005                                                                          
007006*01    -COPY WWDC99                                                       
007007                                                                          
007100     EJECT                                                                
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     SKIP2                                                                
008400*    --- PARAMETRAR TILL ABEND                                            
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  FELTEXT.                                                             
009100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-WDA8ASEQ-X.                                                    
010100         05  W-TEELMT            PIC X(16)    VALUE SPACE.                
010200         05  W-IDELMT            PIC X(16)    VALUE SPACE.                
010301         05  W-URET-IDARTNR-FILLER REDEFINES W-IDELMT.                    
010401           07  W-URET-IDARTNR    PIC 9(9).                                
010501           07  FILLER            PIC X(7).                                
010601         05  W-URET-IDLEVNR-FILLER REDEFINES W-IDELMT.                    
010701           07  W-URET-IDLEVNR    PIC X(5).                                
010801           07  FILLER            PIC X(11).                               
010901         05  W-URET-IDFKNGRP-FILLER REDEFINES W-IDELMT.                   
011001           07  W-URET-IDFKNGRP   PIC 9(4).                                
011101           07  FILLER            PIC X(12).                               
011201         05  W-URET-KDFARLIG-FILLER REDEFINES W-IDELMT.                   
011301           07  W-URET-KDFARLIG   PIC 9.                                   
011401           07  FILLER            PIC X(15).                               
011501         05  W-URET-KDSORT-FILLER REDEFINES W-IDELMT.                     
011601           07  W-URET-KDSORT     PIC X(2).                                
011701           07  FILLER            PIC X(14).                               
011801         05  W-URET-KDPRODSL-FILLER REDEFINES W-IDELMT.                   
011901           07  W-URET-KDPRODSL   PIC 9(2).                                
012001           07  FILLER            PIC X(14).                               
012101                                                                          
012201                                                                          
012301     03  W-BESORTRT-X.                                                    
012401         05  W-BESORTRT          PIC X(20)    VALUE SPACE.                
012501                                                                          
012601     03  W-IDGMT-X.                                                       
012701         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012801         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
012901                                                                          
013001     03  W-IDARTNR-X.                                                     
013101         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013201                                                                          
013301     03  W-KDSEGKEY-X.                                                    
013401         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013501                                                                          
013601     03  W-IDDC-X.                                                        
013701         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013702                                                                          
013703     03  W-1165KEY-X.                                                     
013704         05  W-1165              PIC X(4)    VALUE '1165'.                
013705         05  W-IDPSN             PIC 9(3)    VALUE ZERO.                  
013706         05  W-IDSPRAK           PIC X(2)    VALUE 'SE'.                  
013707         05  FILLER              PIC X(21)   VALUE LOW-VALUE.             
013708     03  W-1168KEY-X.                                                     
013709         05  W-KDFGTRP           PIC 9(02)   VALUE ZERO.                  
013801                                                                          
013901     SKIP2                                                                
014001*    --- STATUS-KOD FRÅN IMS                                              
014101 01  STATUS-WS                   PIC XX.                                  
014201     88  SEGMENT-FINNS                       VALUE '  '.                  
014301     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014401     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014501     SKIP2                                                                
014601 01  GODK-STATUSKODER.                                                    
014701     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014801     SKIP3                                                                
014901 01  SSA1                        PIC X(64).                               
015001 01  SSA2                        PIC X(64).                               
015101     EJECT                                                                
015201*    --- IMS FUNKTIONSKODER                                               
015301*01  -COPY W0003                                                          
015401     EJECT                                                                
015501*    ---  DLI INPUT-OUTPUT AREA                                           
015601                                                                          
015701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA801'.                      
015801 01  DLI-IO-WDA801.                                                       
015901*    03  -COPY WDA801                                                     
016001     EJECT                                                                
016101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA811'.                      
016201 01  DLI-IO-WDA811.                                                       
016301*    03  -COPY WDA811                                                     
016401     EJECT                                                                
016501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
016601 01  DLI-IO-WDB201.                                                       
016701*    03  -COPY WDB201                                                     
016801     EJECT                                                                
016901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017001 01  DLI-IO-WDK601.                                                       
017101*    03  -COPY WDK601                                                     
017201     EJECT                                                                
017301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017401 01  DLI-IO-WDK611.                                                       
017501*    03  -COPY WDK611                                                     
017601     EJECT                                                                
017701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
017801 01  DLI-IO-WDK701.                                                       
017901*    03  -COPY WDK701                                                     
018001     EJECT                                                                
018101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018201 01  DLI-IO-WDK711.                                                       
018301*    03  -COPY WDK711                                                     
018401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
018501 01  DLI-IO-WDB601.                                                       
018601*    03  -COPY WDB601                                                     
018701     EJECT                                                                
018702 01  FILLER         PIC X(16) VALUE 'DLI-IO-1168'.                        
018703 01  DLI-IO-1168.                                                         
018704*    03  -COPY WDGX1168                                                   
018705     EJECT                                                                
018801 LINKAGE SECTION.                                                         
018901                                                                          
019001*   -COPY W418KTL3                                                        
019101                                                                          
019201     EJECT                                                                
019401*01  -COPY W0008  -PRE WDA8-                                              
019501     05  FILLER                  PIC X.                                   
019601                                                                          
019701*01  -COPY W0008  -PRE WDB2-                                              
019801     05  FILLER                  PIC X.                                   
019901                                                                          
020001*01  -COPY W0008  -PRE WDK6-                                              
020101     05  FILLER                  PIC X.                                   
020201                                                                          
020301*01  -COPY W0008  -PRE WDK7-                                              
020401     05  FILLER                  PIC X.                                   
020501                                                                          
020601*01  -COPY W0008  -PRE WDB6-                                              
020701     05  FILLER                  PIC X.                                   
020702                                                                          
020703*01  -COPY W0008   -PRE 1165-                                             
020704     05  FILLER                  PIC X.                                   
020801     EJECT                                                                
020901 PROCEDURE DIVISION  USING KTL3-W418KTL3 WDA8-PCB WDB2-PCB                
021001                           WDK6-PCB WDK7-PCB WDB6-PCB 1165-PCB.           
021101 MAIN SECTION.                                                            
021201     ENTRY 'DLITCBL' USING KTL3-W418KTL3 WDA8-PCB WDB2-PCB                
021301                           WDK6-PCB WDK7-PCB WDB6-PCB 1165-PCB.           
021401                                                                          
021501                                                                          
021601     PERFORM A-INIT                                                       
021701                                                                          
021801     PERFORM B-KOLLA-INDATA                                               
021901                                                                          
022001     IF INDATA-OK                                                         
022101       PERFORM C-LAES-RETUR-MATRIS                                        
022201     END-IF                                                               
022301                                                                          
022401                                                                          
022501     MOVE ZERO TO RETURN-CODE                                             
022601     GOBACK                                                               
022701     .                                                                    
022801     EJECT                                                                
022901 A-INIT SECTION.                                                          
023001                                                                          
023101     ACCEPT DAGENS-DATUM  FROM DATE                                       
023201     .                                                                    
023301     EJECT                                                                
023401 B-KOLLA-INDATA   SECTION.                                                
023501     MOVE 'B-KOLLA-INDATA'           TO CURRENT-SECTION                   
023601                                                                          
023701     MOVE JA               TO INDATA-SW                                   
023801     MOVE JA               TO WDK711-SW                                   
023901     MOVE SPACE            TO KTL3-KDRETBEH                               
024001                                                                          
024101     MOVE KTL3-IDDC        TO W-IDDC                                      
024201     PERFORM IMS-GU-WDB601                                                
024301     IF SEGMENT-SAKNAS                                                    
024401       MOVE NEJ            TO INDATA-SW                                   
024501       MOVE 'S'            TO KTL3-KDSVAR                                 
024601     ELSE                                                                 
024701       IF DCS-CDC                                                         
024801         CONTINUE                                                         
024901       ELSE                                                               
025001         MOVE KTL3-IDARTNR TO W-IDARTNR                                   
025101         PERFORM IMS-GU-WDK711                                            
025201         IF SEGMENT-SAKNAS                                                
025301           MOVE NEJ        TO WDK711-SW                                   
025401         END-IF                                                           
025501       END-IF                                                             
025601     END-IF                                                               
025701                                                                          
025801     MOVE KTL3-IDDISTR     TO W-IDDISTR                                   
025901     MOVE KTL3-IDKUNDNR    TO W-IDKUNDNR                                  
026001     PERFORM IMS-GU-WDB201                                                
026101     IF SEGMENT-SAKNAS                                                    
026201       MOVE NEJ            TO INDATA-SW                                   
026301       MOVE 'S'            TO KTL3-KDSVAR                                 
026401     END-IF                                                               
026501                                                                          
026601     MOVE KTL3-IDARTNR     TO W-IDARTNR                                   
026701     PERFORM IMS-GU-WDK601                                                
026801     IF SEGMENT-FINNS                                                     
026901       PERFORM IMS-GNP-WDK611                                             
027001       IF SEGMENT-SAKNAS                                                  
027101         MOVE NEJ          TO INDATA-SW                                   
027201         MOVE 'S'          TO KTL3-KDSVAR                                 
027301       END-IF                                                             
027401     ELSE                                                                 
027501       MOVE NEJ            TO INDATA-SW                                   
027601       MOVE 'S'            TO KTL3-KDSVAR                                 
027701     END-IF                                                               
027801                                                                          
027901     .                                                                    
028001     EJECT                                                                
028101 C-LAES-RETUR-MATRIS    SECTION.                                          
028201     MOVE 'C-LAES-RETUR-MATRIS'      TO CURRENT-SECTION                   
028301                                                                          
028401     MOVE JA               TO INDATA-SW                                   
028501     MOVE NEJ              TO KTL3-KDSVAR                                 
028601                                                                          
028701     MOVE WC-IDARTNR       TO W-TEELMT                                    
028801     MOVE SPACE            TO W-IDELMT                                    
028901     MOVE KTL3-IDARTNR     TO W-URET-IDARTNR                              
029001                                                                          
029101     PERFORM IMS-GU-WDA811-ASEQ                                           
029201     IF SEGMENT-FINNS                                                     
029301       MOVE NEJ            TO INDATA-SW                                   
029401       PERFORM IMS-GNP-WDA801                                             
029501       MOVE +1   TO IX                                                    
029601       PERFORM UNTIL IX > 14                                              
029701         IF SORT-KDANMORS (IX) = KTL3-KDANMORS                            
029801           MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                       
029901           MOVE YES                TO KTL3-KDSVAR                         
030001           MOVE MAX-IX TO IX                                              
030101         END-IF                                                           
030201         ADD +1  TO IX                                                    
030301       END-PERFORM                                                        
030401     END-IF                                                               
030501                                                                          
030601     IF INDATA-OK                                                         
030701       MOVE WC-KDFARLIG      TO W-TEELMT                                  
030801       MOVE SPACE            TO W-IDELMT                                  
030901       MOVE CLAG-KDFARLIG    TO W-URET-KDFARLIG                           
030902       MOVE CLAG-IDPSN       TO W-IDPSN                                   
031001                                                                          
031101       PERFORM IMS-GU-WDA811-ASEQ                                         
031201       IF SEGMENT-FINNS                                                   
031301         MOVE NEJ            TO INDATA-SW                                 
031401         IF GMT-FLRETFG = 'N'                                             
031501           PERFORM IMS-GNP-WDA801                                         
031601           MOVE +1   TO IX                                                
031701           PERFORM UNTIL IX > 14                                          
031801             IF SORT-KDANMORS (IX) = KTL3-KDANMORS                        
031901               MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                   
032001               MOVE YES                TO KTL3-KDSVAR                     
032101               MOVE MAX-IX TO IX                                          
032201             END-IF                                                       
032301             ADD +1  TO IX                                                
032401           END-PERFORM                                                    
032402                                                                          
032403*UNDANTAG FÖR ALLA PSN-ER SOM INTE ÄR FARLIGA FÖR BILTRANSP = ADR         
032405           PERFORM CA-LAS-1168-ADR                                        
032406           IF FG-ADR-FINNS-EJ                                             
032407             IF KTL3-KDANMORS NOT = '98'                                  
032408               IF KTL3-KDRETBEH = 'S'                                     
032409                 MOVE 'P'      TO KTL3-KDRETBEH                           
032410               END-IF                                                     
032411             END-IF                                                       
032412           END-IF                                                         
032420*SLUT UNDANTAG                                                            
032430                                                                          
032501         ELSE                                                             
032601           MOVE 'A'            TO KTL3-KDRETBEH                           
032701           MOVE YES            TO KTL3-KDSVAR                             
032801         END-IF                                                           
032901       END-IF                                                             
033001     END-IF                                                               
033101                                                                          
033201     IF INDATA-OK                                                         
033301       MOVE WC-KDSORT        TO W-TEELMT                                  
033401       MOVE SPACE            TO W-IDELMT                                  
033501       MOVE ART-KDSORT       TO W-URET-KDSORT                             
033601                                                                          
033701       PERFORM IMS-GU-WDA811-ASEQ                                         
033801       IF SEGMENT-FINNS                                                   
033901         MOVE NEJ            TO INDATA-SW                                 
034001         PERFORM IMS-GNP-WDA801                                           
034101         MOVE +1   TO IX                                                  
034201         PERFORM UNTIL IX > 14                                            
034301           IF SORT-KDANMORS (IX) = KTL3-KDANMORS                          
034401             MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                     
034501             MOVE YES                TO KTL3-KDSVAR                       
034601             MOVE MAX-IX TO IX                                            
034701           END-IF                                                         
034801           ADD +1  TO IX                                                  
034901         END-PERFORM                                                      
035001       END-IF                                                             
035101     END-IF                                                               
035201                                                                          
035301     IF INDATA-OK                                                         
035401       MOVE WC-IDLEVNR        TO W-TEELMT                                 
035501       IF DCS-CDC                                                         
035601         MOVE SPACE           TO W-IDELMT                                 
035701         MOVE ART-IDLEVNR     TO W-URET-IDLEVNR                           
035801       ELSE                                                               
035901         IF WDK711-FINNS                                                  
036001           MOVE SPACE         TO W-IDELMT                                 
036101           MOVE SLAG-IDLEVNR  TO W-URET-IDLEVNR                           
036201         ELSE                                                             
036301           MOVE SPACE         TO W-IDELMT                                 
036401           MOVE ART-IDLEVNR   TO W-URET-IDLEVNR                           
036501         END-IF                                                           
036601       END-IF                                                             
036701                                                                          
036801       PERFORM IMS-GU-WDA811-ASEQ                                         
036901       IF SEGMENT-FINNS                                                   
037001         MOVE NEJ            TO INDATA-SW                                 
037101         PERFORM IMS-GNP-WDA801                                           
037201         MOVE +1   TO IX                                                  
037301         PERFORM UNTIL IX > 14                                            
037401           IF SORT-KDANMORS (IX) = KTL3-KDANMORS                          
037501             MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                     
037601             MOVE YES                TO KTL3-KDSVAR                       
037701             MOVE MAX-IX TO IX                                            
037801           END-IF                                                         
037901           ADD +1  TO IX                                                  
038001         END-PERFORM                                                      
038101       END-IF                                                             
038201     END-IF                                                               
038301                                                                          
038401     IF INDATA-OK                                                         
038501       MOVE WC-IDFKNGRP      TO W-TEELMT                                  
038601       MOVE SPACE            TO W-IDELMT                                  
038701       MOVE ART-IDFKNGRP     TO W-URET-IDFKNGRP                           
038801                                                                          
038901       PERFORM IMS-GU-WDA811-ASEQ                                         
039001       IF SEGMENT-FINNS                                                   
039002         MOVE KTL3-IDDC TO WS-IDDC                                        
039003         IF ART-IDFKNGRP = +08616 AND                                     
039004            KTL3-KDANMORS = '98'  AND                                     
039005            NDC-NA                                                        
039006                                                                          
039007           MOVE 'A'                TO KTL3-KDRETBEH                       
039008           MOVE YES                TO KTL3-KDSVAR                         
039009         ELSE                                                             
039101           MOVE NEJ            TO INDATA-SW                               
039201           PERFORM IMS-GNP-WDA801                                         
039301           MOVE +1   TO IX                                                
039401           PERFORM UNTIL IX > 14                                          
039501             IF SORT-KDANMORS (IX) = KTL3-KDANMORS                        
039601               MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                   
039701               MOVE YES                TO KTL3-KDSVAR                     
039801               MOVE MAX-IX TO IX                                          
039901             END-IF                                                       
040001             ADD +1  TO IX                                                
040101           END-PERFORM                                                    
040102         END-IF                                                           
040201       END-IF                                                             
040301     END-IF                                                               
040401                                                                          
040501     IF INDATA-OK                                                         
040601       MOVE WC-KDPRODSL      TO W-TEELMT                                  
040701       MOVE SPACE            TO W-IDELMT                                  
040801       MOVE ART-KDPRODSL     TO W-URET-KDPRODSL                           
040901                                                                          
041001       PERFORM IMS-GU-WDA811-ASEQ                                         
041101       IF SEGMENT-FINNS                                                   
041201         MOVE NEJ            TO INDATA-SW                                 
041301         PERFORM IMS-GNP-WDA801                                           
041401         MOVE +1   TO IX                                                  
041501         PERFORM UNTIL IX > 14                                            
041601           IF SORT-KDANMORS (IX) = KTL3-KDANMORS                          
041701             MOVE SORT-KDRETBEH (IX) TO KTL3-KDRETBEH                     
041801             MOVE YES                TO KTL3-KDSVAR                       
041901             MOVE MAX-IX TO IX                                            
042001           END-IF                                                         
042101           ADD +1  TO IX                                                  
042201         END-PERFORM                                                      
042301       END-IF                                                             
042401     END-IF                                                               
042501                                                                          
042601     .                                                                    
042701     EJECT                                                                
042702 CA-LAS-1168-ADR SECTION.                                                 
042703                                                                          
042706     MOVE NEJ    TO FG-ADR-SW                                             
042707     MOVE 04     TO W-KDFGTRP                                             
042708     PERFORM IMS-GU-1168                                                  
042709     IF SEGMENT-FINNS                                                     
042710       MOVE +1 TO IX                                                      
042711       PERFORM UNTIL IX > 3                                               
042712         IF 1168-BEPSN(IX) = SPACE                                        
042713           CONTINUE                                                       
042714         ELSE                                                             
042715            MOVE JA                 TO FG-ADR-SW                          
042720         END-IF                                                           
042730         ADD +1 TO IX                                                     
042740       END-PERFORM                                                        
042801     END-IF                                                               
042805     .                                                                    
042806     EJECT                                                                
042807 S99-ABEND SECTION.                                                       
042901                                                                          
043001     CALL ABEND USING RKOD-ABEND                                          
043101     .                                                                    
043201     EJECT                                                                
043301* --- IMS SEKTIONER ---                                                   
043401                                                                          
043501     EJECT                                                                
043601 IMS-GU-WDA811-ASEQ SECTION.                                              
043701     MOVE 'IMS-GU-WDA811-ASEQ'      TO DBS-SECTION                        
043801                                                                          
043901     STRING 'WDA811  (WDA8ASEQ =' W-WDA8ASEQ-X ')'                        
044001          DELIMITED BY SIZE INTO SSA1                                     
044101     MOVE '  GE' TO GODK-STATUSKODER                                      
044201     CALL CBLTDLI USING GU WDA8-PCB DLI-IO-WDA811 SSA1                    
044301     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
044401     PERFORM IMS-STATUSKONTROLL                                           
044501     .                                                                    
044601     EJECT                                                                
044701 IMS-GNP-WDA801 SECTION.                                                  
044801     MOVE 'IMS-GNP-WDA801'   TO DBS-SECTION                               
044901                                                                          
045001     MOVE 'WDA801   ' TO SSA1                                             
045101     MOVE '  ' TO GODK-STATUSKODER                                        
045201     CALL CBLTDLI USING GNP WDA8-PCB DLI-IO-WDA801 SSA1                   
045301     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
045401     PERFORM IMS-STATUSKONTROLL                                           
045501     .                                                                    
045601     EJECT                                                                
045701 IMS-GU-WDB201 SECTION.                                                   
045801     MOVE 'IMS-GU-WDB201'    TO DBS-SECTION                               
046001                                                                          
046101     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
046201          DELIMITED BY SIZE INTO SSA1                                     
046301     MOVE '  GE' TO GODK-STATUSKODER                                      
046401     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
046501     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
046601     PERFORM IMS-STATUSKONTROLL                                           
046701     .                                                                    
046801     EJECT                                                                
046901 IMS-GU-WDK601 SECTION.                                                   
047001     MOVE 'IMS-GU-WDK601'    TO DBS-SECTION                               
047101                                                                          
047201     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
047301          DELIMITED BY SIZE INTO SSA1                                     
047401     MOVE '  GE' TO GODK-STATUSKODER                                      
047501     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
047601     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
047701     PERFORM IMS-STATUSKONTROLL                                           
047801     .                                                                    
047901     EJECT                                                                
048001 IMS-GNP-WDK611 SECTION.                                                  
048101     MOVE 'IMS-GNP-WDK611'   TO DBS-SECTION                               
048201                                                                          
048301     MOVE 'WDK611   ' TO SSA1                                             
048401     MOVE '  GE' TO GODK-STATUSKODER                                      
048501     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
048601     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
048701     PERFORM IMS-STATUSKONTROLL                                           
048801     .                                                                    
048901     EJECT                                                                
049001 IMS-GU-WDK711 SECTION.                                                   
050001     MOVE 'IMS-GU-WDK711'    TO DBS-SECTION                               
051001                                                                          
052001     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
053001          DELIMITED BY SIZE INTO SSA1                                     
053101     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
053201          DELIMITED BY SIZE INTO SSA2                                     
053301     MOVE '  GE' TO GODK-STATUSKODER                                      
053401     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
053501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
053601     PERFORM IMS-STATUSKONTROLL                                           
053701     .                                                                    
053801     EJECT                                                                
053901 IMS-GU-WDB601 SECTION.                                                   
054001     MOVE 'IMS-GU-WDB601'    TO DBS-SECTION                               
054101                                                                          
054201     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
054301          DELIMITED BY SIZE INTO SSA1                                     
054401     MOVE '  GE' TO GODK-STATUSKODER                                      
054501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
054601     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
054701     PERFORM IMS-STATUSKONTROLL                                           
054801     .                                                                    
054901     EJECT                                                                
054913 IMS-GU-1168 SECTION.                                                     
054914     MOVE 'IMS-GU-1168   '    TO DBS-SECTION                              
054915                                                                          
054916     STRING 'WDR201  (WDGXKEY  =' W-1165KEY-X ')'                         
054917          DELIMITED BY SIZE INTO SSA1                                     
054918     STRING 'WDGX1168(KDFGTRP  =' W-1168KEY-X ')'                         
054919          DELIMITED BY SIZE INTO SSA2                                     
054920     MOVE '  GE' TO GODK-STATUSKODER                                      
054921     CALL CBLTDLI USING GU 1165-PCB DLI-IO-1168 SSA1 SSA2                 
054922     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
054923     PERFORM IMS-STATUSKONTROLL                                           
054924     .                                                                    
054930     SKIP3                                                                
055001 IMS-STATUSKONTROLL SECTION.                                              
055101                                                                          
055201     SET STATUS-IX TO 1                                                   
055301     SEARCH GODK-STATUS                                                   
055401       AT END                                                             
055501         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055601           DELIMITED BY SIZE INTO FELTEXT                                 
055701         DISPLAY FELTEXT                                                  
055801         CALL FELLOG                                                      
055901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056001         CONTINUE                                                         
056101     END-SEARCH                                                           
056201     .                                                                    
