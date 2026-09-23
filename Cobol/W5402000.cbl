000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.       W5402000.                                              
000300 AUTHOR.           KARL JOHAN HANSSON.                                    
000400                                                                          
000500 DATE-WRITTEN.     DEC 1996.                                              
000600*    REMARKS.                                                             
000700*           - INPUT-PROCEDURE KOMPLETTERAR INPOSTERNA MED                 
000800*             RANDOMISERAD NYCKEL, DÄREFTER SORTERAS PÅ DENNA.            
000900*           - OUTPUT-PROCEDURE UPPDATERAR SEDAN WDK6 MED PRINK,           
001000*             PRARTSTD, PRDIRLON, PRDMTRL, PROVRPAL OCH PRARTSJK.         
001100*             OM INPOSTENS PRINK ÄR NOLL HÄMTAS DENNA                     
001200*             FRÅN ARTIKELREGISTRET.                                      
001300*             OM ARTIKELN HAR SALDO EJ NOLL PÅ NÅGOT DC SKAPAS            
001400*             PEDAL-TRANS PÅ DETTA ÖVER STANDARDPRISFÖRÄNDRINGEN          
001500*             GÅNGER LAGERSALDOT. (GÄLLER EJ DC 41 - 51)                  
001600*                                                                         
001700*    UTFIL:  W54020 EN POST PER DC MED SALDOT EJ NOLL SKAPAS FÖR          
001800*                   PEDAL. POSTENS SUMMAFÄLT =                            
001900*                   KVANTITET * STDPRIS-FÖRÄNDRING.                       
002000*    RETURKODER:                                                          
002100*                16     ABEND VID FEL I SORT                              
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600     SELECT  W55138      ASSIGN  TO  UT-S-W54020D1.                       
002700                                                                          
002710*    ---- XRST FILE: COPY OF OUTFILE (+0) ENTERED AS INPUT FILE           
002720     SELECT  XRST-FIL    ASSIGN      W54020D2.                            
002800*                        PEDALPOSTER, FÖR BOKFÖRING I SAP                 
003100     SELECT  SORTFIL     ASSIGN  TO  UT-S-W54020DS.                       
003110                                                                          
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W55138                                                               
003700     RECORDING F                                                          
003800     BLOCK 0 RECORDS.                                                     
003900                                                                          
004000*01  IN-054    -COPY W55138      -L.                                      
004100     SKIP3                                                                
004110 FD  XRST-FIL                                                             
004120     LABEL RECORD STANDARD                                                
004130     RECORDING  V                                                         
004140     BLOCK CONTAINS 0.                                                    
004150                                                                          
004151 01  FILLER                  PIC X(998).                                  
004160 01  POST   -COPY W51060    -PRE XRST-                                    
004180     EJECT                                                                
004800 SD  SORTFIL.                                                             
004900                                                                          
005000 01  SORT-RECORD.                                                         
005100     03  SORT-RANDOMKEY   PIC X(4).                                       
005200*    03  -COPY W55138   -PRE SORT-                                        
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                    PIC X(8)    VALUE 'W5402000'.               
005800 77  JA                       PIC X       VALUE 'J'.                      
005900 77  NEJ                      PIC X       VALUE 'N'.                      
006000 77  RKOD                     PIC S9(4)   VALUE ZERO  COMP SYNC.          
006100 01  W-KDPRODSL               PIC 9(3)    VALUE ZERO.                     
006200 01  DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                     
006300 01  WS-PRARTBES-PR           PIC S9(7)V9(2)  COMP-3.                     
006400 01  FL-PRARTBES              PIC X       VALUE 'N'.                      
006500 01  DATABASE                 PIC X(4)    VALUE 'WDK6'.                   
006510 77  W-CHKP-RAKNARE           PIC S9(5)   VALUE +0    COMP-3.             
006520 77  W-CHKP-MAX               PIC S9(5)   VALUE +50   COMP-3.             
006530 77  CHKP-ID                  PIC X(8)    VALUE 'W54020  '.               
006540 77  MSG-IO-AREA-LENGTH-1     PIC S9(9)   VALUE +32  COMP SYNC.           
006550 77  MSG-IO-AREA-1            PIC X(32)   VALUE SPACE.                    
006560 77  CHKP-AREA-1-LENGTH       PIC S9(9)   VALUE +32  COMP SYNC.           
006570 77  CHKP-AREA-1              PIC X(32)   VALUE SPACE.                    
006580 77  W-ANT-POSTER-FORBI       PIC S9(7)   VALUE +0    COMP-3.             
006590 77  XRST-FIL-EOF             PIC X       VALUE 'N'.                      
006591 77  MAX-GSAM                 PIC S9(4)   COMP    VALUE 267.              
006592 01  WW-IDARTNR-IDDC.                                                     
006593   03  WW-IDARTNR             PIC X(9)    VALUE SPACE.                    
006594   03  WW-IDDC                PIC X(2)    VALUE SPACE.                    
006600                                                                          
006601 01  WS-IDARTNR-IDDC-N.                                                   
006602   03  WS-IDARTNR-N          PIC 9(9)    VALUE ZERO.                      
006603   03  WS-IDDC-X             PIC X(2)    VALUE SPACE.                     
006604                                                                          
006605 01  WSX-IDARTNR-IDDC.                                                    
006606   03  WSX-IDARTNR            PIC X(9)    VALUE SPACE.                    
006607   03  WSX-IDDC               PIC X(2)    VALUE SPACE.                    
006608                                                                          
006610 01  WS-FLD.                                                              
006630   03  WS-ANT-POST-GSAM       PIC S9(5)   COMP-3  VALUE ZERO.             
006640   03  WS-ANT-POST-XRST       PIC S9(5)   COMP-3  VALUE ZERO.             
006650   03  WS-ANT-SORTFIL         PIC S9(5)   COMP-3  VALUE ZERO.             
006700 01  SWITCH.                                                              
006800     03  W55138-EOF           PIC X       VALUE 'N'.                      
006900     03  SORTFIL-EOF          PIC X       VALUE 'N'.                      
007000                                                                          
007100 01  W-ARBETS-AREOR.                                                      
007200     03  W-DIFF-PRARTSTD      PIC S9(11)V99   VALUE ZERO COMP-3.          
007300     03  W-OLD-PRARTSTD       PIC S9(11)V99   VALUE ZERO COMP-3.          
007400     03  FEL-ANT-UTG-ART      PIC S9(9)      VALUE ZERO COMP-3.           
007500     03  FEL-ANT-SAK-ART      PIC S9(9)      VALUE ZERO COMP-3.           
007600                                                                          
007700*  FÖR BERÄKNING AV DIFFERENS MELLAN NYTT OCH GAMMALT STANDARDPRIS        
007800     03  W-DIFF-LAGERVARDE    PIC S9(11)V99   VALUE ZERO COMP-3.          
007900                                                                          
008000     SKIP3                                                                
008100 01  SUBMODULER.                                                          
008200     03  ABEND           PIC X(8)    VALUE 'ABEND'.                       
008300     03  POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                    
008400     03  CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
008500     03  FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
008600     03  DATKORT         PIC X(8)    VALUE 'DATKORT '.                    
008700     03  W015RAND        PIC X(8)    VALUE 'W015RAND'.                    
008800     EJECT                                                                
008900*    ---  VALID IDDC CODES                                                
009000*01  -COPY WWDC99                                                         
009100*01  -COPY WWDCKONS                                                       
009200     EJECT                                                                
009300 01  WDATUM               PIC X(6) VALUE 'WDATUM'.                        
009400                                                                          
009500*    -COPY WDATKORT                                                       
009600     EJECT                                                                
009700*    -COPY W0005  -PRE POSTSUM-                                           
009800     EJECT                                                                
009900 01  FILLER               PIC X(16)  VALUE 'PRISÄNDRINGSPOST'.            
010000                                                                          
010100*01  POST    -COPY W55138     -PRE IN-.                                   
010200     EJECT                                                                
010300 01  FILLER               PIC X(16)  VALUE 'PEDAL-POST'.                  
010400                                                                          
010500 01  PEDALAREA.                                                           
010600*    03  -COPY W51060.                                                    
010700     EJECT                                                                
010800*         ARBETSAREOR TILL IMS-SEKTIONERNA                                
010900*                                                                         
011000     SKIP3                                                                
011100 01  FILLER              PIC X(16)   VALUE 'IMS-WS'.                      
011200     SKIP3                                                                
011300*   - - - -  STATUSKOD FRÅN IMS                                           
011400                                                                          
011500 01  STATUS-WS            PIC XX.                                         
011600     88  SEGMENT-FINNS             VALUE '  '.                            
011700     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
011800                                                                          
011900                                                                          
012000                                                                          
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS   OCCURS 5   INDEXED BY STATUS-IX   PIC XX.          
012300                                                                          
012400 01  SSA1                 PIC X(64).                                      
012500 01  SSA2                 PIC X(64).                                      
012600                                                                          
012700*   - - - -  NYCKLAR OCH SÖKFÄLT TILL DLI                                 
012800                                                                          
012900 01  W-IDDC-MIN-X.                                                        
013000     03  W-IDDC-MIN       PIC X(2)    VALUE '1A'.                         
013100                                                                          
013200 01  W-IDDC-MAX-X.                                                        
013300     03  W-IDDC-MAX       PIC X(2)    VALUE '62'.                         
013400                                                                          
013500 01  W-IDARTNR-X.                                                         
013600     03  W-IDARTNR        PIC S9(9)   COMP-3.                             
013700                                                                          
013800                                                                          
013900 01  W-DAPRLIST-X.                                                        
014000     03  W-DAPRLIST       PIC   9(8)  VALUE ZERO.                         
014100                                                                          
014200 01  W-IDDC-B6-X.                                                         
014300     03 W-IDDC-B6         PIC X(2).                                       
014400                                                                          
014410 01  W-WDGXKEY-X.                                                         
014420     03  W-IDHTYP            PIC X(4)     VALUE '4579'.                   
014430     03  W-IDPGM             PIC X(8)     VALUE 'W5402000'.               
014440     03  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.                
014450*SKIP2                                                                    
014500     EJECT                                                                
014600*01            -COPY W0003                                                
014700     EJECT                                                                
014800 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K601'.               
014900 01  DLI-IO-K601.                                                         
015000     03  -COPY WDK601                                                     
015100     EJECT                                                                
015200 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K611'.               
015300 01  DLI-IO-K611.                                                         
015400     03  -COPY WDK611                                                     
015500     EJECT                                                                
015600 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K611SKRAP'.          
015700 01  DLI-IO-K611-GHUSKRAP.                                                
015800     03  -COPY WDK611  -L                                                 
015900     EJECT                                                                
016000 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K621'.               
016100 01  DLI-IO-K621.                                                         
016200     03  -COPY WDK621                                                     
016300     EJECT                                                                
016400 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K701'.               
016500 01  DLI-IO-K701.                                                         
016600     03  -COPY WDK701                                                     
016700     EJECT                                                                
016800 01  FILLER               PIC X(16)    VALUE 'DLI-IO-K711'.               
016900 01  DLI-IO-K711.                                                         
017000     03  -COPY WDK711                                                     
017100                                                                          
017200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017300 01   DLI-IO-AREA-B601.                                                   
017400*     03  -COPY WDB601                                                    
017500                                                                          
017510 01  FILLER               PIC X(16)   VALUE 'WDR4 AREA'.                  
017520 01   DLI-IO-AREA-WDR4.                                                   
017530*     03  -COPY WDGX4580                                                  
017540                                                                          
017550 01  FILLER                    PIC X(16) VALUE 'GSAMFIL-IO-AREA'.         
017560 01  GSAMFIL-IO-AREA.                                                     
017570     03  GSAM-LRECL            PIC S9(4) COMP.                            
017580*    03 -COPY W51060          -PRE  GSAM-                                 
017600     EJECT                                                                
017700 LINKAGE SECTION.                                                         
017710                                                                          
017720*01  -COPY W0009  -PRE MSG-                                               
017730     EJECT                                                                
017740                                                                          
017800*    -COPY W0008  -PRE ARTC-                                              
017900         05  FILLER      PIC X(10).                                       
018000                                                                          
018100*    -COPY W0008  -PRE ARTS-                                              
018200         05  FILLER      PIC X(10).                                       
018300                                                                          
018400*01  -COPY W0008  -PRE WDB6-                                              
018500     05  FILLER          PIC X.                                           
018600                                                                          
018610*01  -COPY W0008  -PRE WDR4-                                              
018620     05  FILLER          PIC X.                                           
018621                                                                          
018630*01  -COPY W0008  -PRE GSAMFIL-                                           
018640       05  FILLER        PIC X.                                           
018700     EJECT                                                                
018800 PROCEDURE DIVISION USING MSG-PCB ARTC-PCB ARTS-PCB WDB6-PCB              
018810                          WDR4-PCB GSAMFIL-PCB.                           
018900 MAIN SECTION.                                                            
019000     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB ARTS-PCB WDB6-PCB             
019010                           WDR4-PCB GSAMFIL-PCB.                          
019100                                                                          
019200                                                                          
019300     PERFORM A-INITIERA                                                   
019400                                                                          
019500     SORT SORTFIL  ASCENDING SORT-RANDOMKEY                               
019600                             SORT-IDARTNR                                 
019700                                                                          
019800             INPUT PROCEDURE  B-BESTAM-RANDOMKEY                          
019900             OUTPUT PROCEDURE C-BEHANDLA-UPPDATERA                        
020000                                                                          
020100     IF SORT-RETURN NOT = ZERO                                            
020200       DISPLAY 'FEL I SORTEN'                                             
020300       MOVE +16 TO RKOD                                                   
020400       CALL ABEND USING RKOD                                              
020500     END-IF                                                               
020600                                                                          
020700     PERFORM Z-AVSLUTA                                                    
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INITIERA    SECTION.                                                   
021300                                                                          
021400     OPEN INPUT  W55138                                                   
021600                                                                          
021610     PERFORM IMS-RESTART                                                  
021620                                                                          
021700     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
021800     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
021900     MOVE SPACE           TO GSAM-EKHT-W51060                             
021910     MOVE ZERO            TO WS-ANT-SORTFIL                               
022000     .                                                                    
022100     EJECT                                                                
022200 B-BESTAM-RANDOMKEY SECTION.                                              
022220                                                                          
022230     PERFORM S01-LAES-INFIL                                               
022240     PERFORM UNTIL W55138-EOF = JA                                        
022250       MOVE IN-POST    TO SORT-W55138                                     
022260       CALL W015RAND USING SORT-IDARTNR SORT-RANDOMKEY DATABASE           
022270                                                                          
022280       RELEASE SORT-RECORD                                                
022290       PERFORM S01-LAES-INFIL                                             
022291     END-PERFORM                                                          
022292     .                                                                    
022293     EJECT                                                                
022294                                                                          
023420 C-BEHANDLA-UPPDATERA SECTION.                                            
023500                                                                          
023511     PERFORM CA-MOVE-RESTART-GSAM                                         
023520     PERFORM CB-SKIP-TILL-CHKPOINT                                        
023591     PERFORM CC-BEHANDLA-UPPDATERA                                        
023593     .                                                                    
023594     EJECT                                                                
023595                                                                          
023596 CA-MOVE-RESTART-GSAM SECTION.                                            
023597                                                                          
023598     MOVE NEJ                TO XRST-FIL-EOF                              
023599     MOVE ZERO               TO WS-ANT-POST-XRST                          
023600                                WS-ANT-POST-GSAM                          
023601                                                                          
023602                                                                          
023604     PERFORM IMS-LAS-ATERSTART                                            
023605                                                                          
023606*----- XRST FILE COPIED TO GSAM FILE                                      
023609                                                                          
023611     IF 4580-KVPOST > ZERO                                                
023612                                                                          
023613*--------------------------- XRST-FIL KOPIERAS TILL GSAMFILEN             
023614*                            FRAM TILL CHECKPOINT-LÄGE                    
023615       PERFORM IMS-GSAMFIL-OPEN                                           
023616       OPEN INPUT  XRST-FIL                                               
023617                                                                          
023618       PERFORM CAA-LAS-XRST                                               
023619                                                                          
023621       MOVE 4580-IDSEGKEY TO WS-IDARTNR-IDDC-N                            
023622       PERFORM UNTIL (XRST-FIL-EOF = JA                                   
023623                  OR  WSX-IDARTNR-IDDC = 4580-IDSEGKEY)                   
023625         MOVE MAX-GSAM TO GSAM-LRECL                                      
023626         MOVE XRST-POST  TO GSAM-EKHT-W51060                              
023628         PERFORM S07-SKRIV-GSAM                                           
023629         PERFORM CAA-LAS-XRST                                             
023630       END-PERFORM                                                        
023631                                                                          
023632*----- IF XRST FILE DOESNOT HAVE CORRECT RESTART DATA                     
023633                                                                          
023634*      IF  XRST-FIL-EOF      = JA                                         
023635*      AND WS-ANT-POST-XRST  NOT = 4580-KVPOST                            
023636*        DISPLAY 'W5402000: ERROR IN RESTART, RESTART FILE'               
023637*        CALL ABEND USING    RKOD                                         
023638*      END-IF                                                             
023639                                                                          
023640       MOVE MAX-GSAM TO GSAM-LRECL                                        
023641       MOVE XRST-POST  TO GSAM-EKHT-W51060                                
023642       PERFORM S07-SKRIV-GSAM                                             
023645                                                                          
023646       CLOSE XRST-FIL                                                     
023647                                                                          
023648     END-IF                                                               
023649                                                                          
023676     .                                                                    
023677     EJECT                                                                
023678 CAA-LAS-XRST SECTION.                                                    
023679                                                                          
023680     READ XRST-FIL                                                        
023681          AT END MOVE JA TO XRST-FIL-EOF                                  
023682     END-READ                                                             
023683     IF XRST-FIL-EOF = NEJ                                                
023684        MOVE 'W54020'   TO POSTSUM-FDNAMN                                 
023685        MOVE 'W54020D2'  TO POSTSUM-DDNAMN2                               
023686        MOVE 'XRST'      TO POSTSUM-TRANSTYP                              
023687        CALL POSTSUM USING POSTSUM-PARM                                   
023688        ADD +1                   TO WS-ANT-POST-XRST                      
023689        MOVE XRST-EKHT-IDARTNR   TO WSX-IDARTNR                           
023690        MOVE XRST-EKHT-IDDC-SEND TO WSX-IDDC                              
023692     END-IF                                                               
023693     .                                                                    
023694     EJECT                                                                
023695 CB-SKIP-TILL-CHKPOINT SECTION.                                           
023696     MOVE +0              TO W-ANT-POSTER-FORBI                           
023697     PERFORM S11-RETURN-SORTFIL                                           
023698                                                                          
023699     PERFORM UNTIL SORTFIL-EOF = JA                                       
023700                OR W-ANT-POSTER-FORBI = 4580-KVPOST                       
023701                OR SORT-IDARTNR       = WS-IDARTNR-N                      
023702        ADD +1  TO W-ANT-POSTER-FORBI                                     
023703        PERFORM S11-RETURN-SORTFIL                                        
023705     END-PERFORM                                                          
023706     IF SORTFIL-EOF = JA                                                  
023707     AND W-ANT-POSTER-FORBI < 4580-KVPOST                                 
023708       DISPLAY 'W55138-EOF = JA    VID ÅTERSTART'                         
023709       MOVE +16 TO RKOD                                                   
023710       CALL ABEND USING RKOD                                              
023711     END-IF                                                               
023712                                                                          
023713     .                                                                    
023714     EJECT                                                                
023715                                                                          
023716 CC-BEHANDLA-UPPDATERA SECTION.                                           
023717                                                                          
023718     PERFORM S11-RETURN-SORTFIL                                           
023720     PERFORM UNTIL SORTFIL-EOF = JA                                       
023730       PERFORM CCA-CHECKPOINT                                             
023800       MOVE IN-IDARTNR         TO W-IDARTNR                               
023900       PERFORM IMS-GET-ARTIKELSEG-ARTC01                                  
024000       IF SEGMENT-SAKNAS                                                  
024100         ADD +1                TO FEL-ANT-SAK-ART                         
024200       ELSE                                                               
024300         IF ART-KDERS-UTG > +0                                            
024400           ADD +1              TO FEL-ANT-UTG-ART                         
024500         ELSE                                                             
024600           MOVE ART-KDPRODSL TO W-KDPRODSL                                
024700                                                                          
024800           PERFORM IMS-GET-EKONSEG-ARTC11                                 
024900           PERFORM CCB-UPPD-EKONSEG-ARTC11                                
025000           PERFORM CCD-SKAPA-PEDAL-POSTER                                 
025100         END-IF                                                           
025200       END-IF                                                             
025300       PERFORM S11-RETURN-SORTFIL                                         
025400     END-PERFORM                                                          
025500     .                                                                    
025600     EJECT                                                                
025610                                                                          
025620 CCA-CHECKPOINT SECTION.                                                  
025800                                                                          
025801     IF W-CHKP-RAKNARE       >  W-CHKP-MAX                                
025802*    UPDATE RESTART DB                                                    
025803       PERFORM IMS-LAS-ATERSTART                                          
025804       MOVE WS-ANT-SORTFIL   TO 4580-KVPOST                               
025808       MOVE WW-IDARTNR-IDDC  TO 4580-IDSEGKEY                             
025809       ACCEPT 4580-TIUPPDAT FROM DATE                                     
025810       ACCEPT 4580-TIUPPTID FROM TIME                                     
025811                                                                          
025813       PERFORM IMS-REPL-ATERSTART                                         
025814                                                                          
025815*    TAG CHECKPOINT                                                       
025816       PERFORM IMS-CHECKPOINT                                             
025817       MOVE ZERO        TO W-CHKP-RAKNARE                                 
025818     END-IF                                                               
025819     .                                                                    
025820     EJECT                                                                
025821 CCB-UPPD-EKONSEG-ARTC11 SECTION.                                         
025830                                                                          
025900     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
026000     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
026100     PERFORM IMS-GNP-WDK621                                               
026200     IF SEGMENT-SAKNAS                                                    
026300       MOVE CLAG-PRARTSTD       TO WS-PRARTBES-PR                         
026400     ELSE                                                                 
026500       MOVE NEJ                 TO FL-PRARTBES                            
026600       PERFORM UNTIL  SEGMENT-SAKNAS                                      
026700         IF PRL-SUINLEV-PR > ZERO                                         
026800           MOVE PRL-PRARTBES-PR  TO WS-PRARTBES-PR                        
026900           SET SEGMENT-SAKNAS TO TRUE                                     
027000         ELSE                                                             
027100           IF FL-PRARTBES = NEJ                                           
027200             MOVE PRL-PRARTBES-PR TO WS-PRARTBES-PR                       
027300             MOVE JA              TO FL-PRARTBES                          
027400           END-IF                                                         
027500           PERFORM IMS-GNP-WDK621                                         
027600         END-IF                                                           
027700       END-PERFORM                                                        
027800     END-IF                                                               
027900                                                                          
028000     COMPUTE CLAG-PRARTSJK ROUNDED =                                      
028100         IN-PRDIRLON + IN-PRDMTRL + IN-PROVRPAL + WS-PRARTBES-PR          
028200     IF IN-PRINK NOT = ZERO                                               
028300       MOVE IN-PRINK           TO CLAG-PRINK                              
028400     END-IF                                                               
028500     MOVE CLAG-PRARTSTD        TO W-OLD-PRARTSTD                          
028600     COMPUTE CLAG-PRARTSTD ROUNDED =                                      
028700         IN-PRDIRLON + IN-PRDMTRL + IN-PROVRPAL + CLAG-PRINK              
028800     MOVE IN-PRDIRLON          TO CLAG-PRDIRLON                           
028900     MOVE IN-PRDMTRL           TO CLAG-PRDMTRL                            
029000     MOVE IN-PROVRPAL          TO CLAG-PROVRPAL                           
029100     PERFORM IMS-GHU-ARTIKELSEG-WDK611                                    
029200     PERFORM IMS-REPL-ARTC                                                
029210     ADD +1 TO W-CHKP-RAKNARE                                             
029300     .                                                                    
029400     EJECT                                                                
029500 CCD-SKAPA-PEDAL-POSTER   SECTION.                                        
029600                                                                          
029700     COMPUTE W-DIFF-PRARTSTD =                                            
029800       CLAG-PRARTSTD - W-OLD-PRARTSTD                                     
029900                                                                          
030000***  LAGERVÄRDESFÖRÄNDRING CDC (IDDC = 11)                                
030100     COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                        
030200       (CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC +                        
030300                                  CLAG-KVAKS-PAV)                         
030400     IF W-DIFF-LAGERVARDE NOT = 0                                         
030500       MOVE WC-CDC-SE                TO EKHT-IDDC-SEND                    
030600                                        EKHT-IDDC-REC                     
030700       MOVE W-DIFF-LAGERVARDE        TO EKHT-SUBEL                        
030800       PERFORM CCDA-SKAPA-SKRIV-PEDAL-POST                                
030900     END-IF                                                               
031000                                                                          
031100***  LAGERVÄRDESFÖRÄNDRING TERMINAL (IDDC = 12)                           
031200     COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                        
031300                                 CLAG-KVAKS-T                             
031400     IF W-DIFF-LAGERVARDE NOT = 0                                         
031500       MOVE WC-CDC-TR                TO EKHT-IDDC-SEND                    
031600                                        EKHT-IDDC-REC                     
031700       MOVE W-DIFF-LAGERVARDE        TO EKHT-SUBEL                        
031800       PERFORM CCDA-SKAPA-SKRIV-PEDAL-POST                                
031900     END-IF                                                               
032000                                                                          
032100***  LAGERVÄRDESFÖRÄNDRING SDC (IDDC = 21 - 26/61 - 62)                   
032200     PERFORM IMS-GU-SLAGERROT                                             
032300     IF SEGMENT-FINNS                                                     
032400       PERFORM IMS-GNP-SLAGERINFO                                         
032500       PERFORM UNTIL NOT SEGMENT-FINNS                                    
032600         IF SEGMENT-FINNS                                                 
032700           MOVE SLAG-IDDC              TO WS-IDDC                         
032800                                          W-IDDC-B6                       
032900           PERFORM IMS-GU-WDB601                                          
033000                                                                          
033100           IF DCS-SDC OR DCS-NDC-PF                                       
033200             IF XDC-NON-VCC-OWNED                                         
033300               CONTINUE                                                   
033400             ELSE                                                         
033500               MOVE SLAG-IDDC          TO EKHT-IDDC-SEND                  
033600                                          EKHT-IDDC-REC                   
033700               COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *              
033800                 (SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-SDC +              
033900                                            SLAG-KVAKS-PAV)               
034000               IF W-DIFF-LAGERVARDE NOT = 0                               
034100                 MOVE W-DIFF-LAGERVARDE TO EKHT-SUBEL                     
034200                 PERFORM CCDA-SKAPA-SKRIV-PEDAL-POST                      
034300               END-IF                                                     
034400             END-IF                                                       
034500           END-IF                                                         
034600         PERFORM IMS-GNP-SLAGERINFO                                       
034700         END-IF                                                           
034800       END-PERFORM                                                        
034900     END-IF                                                               
035000     .                                                                    
035100 CCDA-SKAPA-SKRIV-PEDAL-POST SECTION.                                     
035200                                                                          
035300     MOVE IDPGM                      TO EKHT-IDPGM                        
035400     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
035500                                        EKHT-DAVERDAT                     
035600     ACCEPT EKHT-TIKLOCK FROM TIME                                        
035700     MOVE IN-IDARTNR                 TO EKHT-IDARTNR                      
035800     MOVE +1                         TO EKHT-IDSEKVNR                     
035900     MOVE '401'                      TO EKHT-KDEKHHT                      
036000     MOVE '401'                      TO EKHT-KDEKSHT                      
036100     MOVE 'SUM  '                    TO EKHT-KDEKNIVA                     
036200     MOVE +0                         TO EKHT-IDDISTR                      
036300                                        EKHT-IDKUNDNR                     
036400     MOVE EKHT-IDDC-SEND             TO EKHT-IDVERGL(1:2)                 
036500     MOVE W-KDPRODSL(2:2)            TO EKHT-IDVERGL(3:2)                 
036600     MOVE SPACE                      TO EKHT-IDVERGL(5:6)                 
036700     MOVE W-KDPRODSL                 TO EKHT-KDPRODSL                     
036800     MOVE ZERO                       TO EKHT-KDPSLLOC                     
037000     MOVE ' '                        TO EKHT-FLLSBOK                      
037100     MOVE 'SEK'                      TO EKHT-KDVALISO                     
037200     MOVE +1.00                      TO EKHT-PRKURS                       
037300     MOVE +0                         TO EKHT-PRARTNTO                     
037400                                        EKHT-PRARTSJK                     
037500                                        EKHT-PRHEMTAG                     
037600     MOVE ZERO                       TO EKHT-PRARTSTD                     
037700                                        EKHT-PRLANDCO                     
037800                                        EKHT-PRINK                        
037900                                        EKHT-PRDIRLON                     
038000                                        EKHT-PRDMTRL                      
038100                                        EKHT-PROVRPAL                     
038200                                        EKHT-KVANTAL                      
038300     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
038400     MOVE SPACE                      TO EKHT-IDTRANS                      
038500                                        EKHT-IDKST                        
038600     MOVE ZERO                       TO EKHT-BEVAT                        
038700                                        EKHT-IDANALYS                     
038800                                        EKHT-IDKONTO                      
038900                                        EKHT-KDANMORS                     
039000                                        EKHT-KDFRAKT                      
039100                                        EKHT-SUVAT                        
039200     MOVE ZERO                       TO EKHT-DAAVIDAT                     
039300                                        EKHT-IDAVINR                      
039400                                        EKHT-KDAVVTYP                     
039500                                        EKHT-KDRT                         
039600                                        EKHT-KVANTMOT                     
039700                                        EKHT-KVAVIS                       
039800     MOVE SPACE                      TO EKHT-KDSORT                       
039900                                        EKHT-IDLEVNR                      
040000                                        EKHT-KDTRADP                      
040100     MOVE SPACE                      TO EKHT-FLDCET                       
040200     MOVE SPACE                      TO EKHT-IDKUNDRF                     
040210     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
040300     IF EKHT-SUBEL NOT = +0                                               
040400       PERFORM S05-SKRIV-UTPOST                                           
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 Z-AVSLUTA SECTION.                                                       
040900                                                                          
041000     CLOSE W55138                                                         
041200     DISPLAY 'ANTAL UTGÅGNA ARTIKLAR: ' FEL-ANT-UTG-ART                   
041300     DISPLAY 'ANTAL SAKNADE ARTIKLAR: ' FEL-ANT-SAK-ART                   
041310*    NOLLA ÅTERSTARTINFORMATIONEN                                         
041320     PERFORM IMS-LAS-ATERSTART                                            
041330     MOVE +0                   TO 4580-KVPOST                             
041331     MOVE SPACE                TO 4580-IDSEGKEY                           
041340     ACCEPT 4580-TIUPPDAT FROM DATE                                       
041350     ACCEPT 4580-TIUPPTID FROM TIME                                       
041360                                                                          
041370     PERFORM IMS-REPL-ATERSTART                                           
041380     PERFORM IMS-GSAMFIL-CLOSE                                            
041400                                                                          
041500     MOVE 'S'        TO POSTSUM-OPKOD                                     
041600     CALL POSTSUM USING POSTSUM-PARM                                      
041700     .                                                                    
041800     EJECT                                                                
041900 S01-LAES-INFIL SECTION.                                                  
042000                                                                          
042100     READ W55138 INTO IN-POST                                             
042200     AT END                                                               
042300       MOVE JA TO W55138-EOF                                              
042400     NOT AT END                                                           
042500       MOVE 'W54020D1'    TO POSTSUM-DDNAMN2                              
042600       MOVE 'W55138'      TO POSTSUM-FDNAMN                               
042700       MOVE 'IN '         TO POSTSUM-TRANSTYP                             
042800       CALL POSTSUM USING POSTSUM-PARM                                    
042900     END-READ                                                             
043000     .                                                                    
043100     EJECT                                                                
044101 S05-SKRIV-UTPOST SECTION.                                                
044102                                                                          
044104     MOVE MAX-GSAM       TO GSAM-LRECL                                    
044106     MOVE PEDALAREA      TO GSAM-EKHT-W51060                              
044107     MOVE GSAM-EKHT-IDARTNR    TO WS-IDARTNR-N                            
044108     MOVE GSAM-EKHT-IDDC-SEND  TO WS-IDDC-X                               
044109     MOVE WS-IDARTNR-IDDC-N TO WW-IDARTNR-IDDC                            
044113     PERFORM S07-SKRIV-GSAM                                               
044114     .                                                                    
044115     EJECT                                                                
044116 S07-SKRIV-GSAM SECTION.                                                  
044117                                                                          
044130     PERFORM IMS-GSAMFIL-ISRT                                             
044140     ADD +1              TO WS-ANT-POST-GSAM                              
044141     .                                                                    
044142     EJECT                                                                
044200 S11-RETURN-SORTFIL SECTION.                                              
044300                                                                          
044400     RETURN SORTFIL                                                       
044500     AT END                                                               
044600       MOVE JA TO SORTFIL-EOF                                             
044700     NOT AT END                                                           
044800       MOVE SORT-W55138   TO IN-POST                                      
044900       MOVE 'W54020DS'    TO POSTSUM-DDNAMN2                              
045000       MOVE 'SORTUT'      TO POSTSUM-FDNAMN                               
045100       MOVE 'SRT'         TO POSTSUM-TRANSTYP                             
045200       CALL POSTSUM USING POSTSUM-PARM                                    
045210       ADD +1              TO WS-ANT-SORTFIL                              
045300     END-RETURN                                                           
045400     .                                                                    
045500     EJECT                                                                
045600 IMS-GET-ARTIKELSEG-ARTC01 SECTION.                                       
045700                                                                          
045800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045900         DELIMITED BY SIZE INTO SSA1                                      
046000     MOVE '  GE'             TO GODK-STATUSKODER                          
046100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-K601 SSA1                      
046200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     .                                                                    
046500     SKIP2                                                                
046600 IMS-GET-EKONSEG-ARTC11 SECTION.                                          
046700                                                                          
046800     MOVE 'WLARTC11'         TO SSA1                                      
046900     MOVE '  '               TO GODK-STATUSKODER                          
047000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-K611 SSA1                    
047100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400     SKIP3                                                                
047500 IMS-GHU-ARTIKELSEG-WDK611 SECTION.                                       
047600                                                                          
047700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
047800         DELIMITED BY SIZE INTO SSA1                                      
047900     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
048000     MOVE '  '             TO GODK-STATUSKODER                            
048100     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-K611-GHUSKRAP                 
048200          SSA1 SSA2                                                       
048300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
048400     PERFORM IMS-STATUSKONTROLL                                           
048500     .                                                                    
048600     SKIP2                                                                
048700 IMS-GNP-WDK621 SECTION.                                                  
048800                                                                          
048900     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
049000         DELIMITED BY SIZE INTO SSA1                                      
049100     MOVE '  GE'               TO GODK-STATUSKODER                        
049200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K621 SSA1                     
049300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600     SKIP3                                                                
049700 IMS-REPL-ARTC SECTION.                                                   
049800                                                                          
049900     MOVE '  ' TO GODK-STATUSKODER                                        
050000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-K611                         
050100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     SKIP2                                                                
050500 IMS-GU-SLAGERROT SECTION.                                                
050600                                                                          
050700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
050800            DELIMITED BY SIZE INTO SSA1                                   
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-K701 SSA1                      
051100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     SKIP2                                                                
051500 IMS-GNP-SLAGERINFO SECTION.                                              
051600                                                                          
051700     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN-X                            
051800                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
051900            DELIMITED BY SIZE INTO SSA1                                   
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-K711 SSA1                     
052200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSKONTROLL                                           
052400     .                                                                    
052500     SKIP2                                                                
052600 IMS-GU-WDB601    SECTION.                                                
052700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
052800          DELIMITED BY SIZE INTO SSA1                                     
052900     MOVE '  ' TO GODK-STATUSKODER                                        
053000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
053100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     .                                                                    
053400     SKIP2                                                                
053401 IMS-LAS-ATERSTART SECTION.                                               
053402                                                                          
053403     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
053404          DELIMITED BY SIZE INTO SSA1                                     
053405     MOVE 'WDR470   '    TO SSA2                                          
053406     MOVE '  '         TO GODK-STATUSKODER                                
053407     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-AREA-WDR4 SSA1 SSA2           
053408     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
053409     PERFORM IMS-STATUSKONTROLL                                           
053410     .                                                                    
053411     SKIP2                                                                
053412 IMS-GSAMFIL-OPEN SECTION.                                                
053413                                                                          
053414     MOVE 'OUT' TO GSAMFIL-IO-AREA                                        
053415     MOVE '  ' TO GODK-STATUSKODER                                        
053416     CALL CBLTDLI USING OPEN-GSAM GSAMFIL-PCB GSAMFIL-IO-AREA             
053417     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
053418     PERFORM IMS-STATUSKONTROLL                                           
053419     .                                                                    
053420     SKIP2                                                                
053421 IMS-REPL-ATERSTART SECTION.                                              
053422                                                                          
053423     MOVE '  '             TO GODK-STATUSKODER                            
053424     CALL CBLTDLI USING REPL WDR4-PCB DLI-IO-AREA-WDR4                    
053425     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
053426     PERFORM IMS-STATUSKONTROLL                                           
053427     .                                                                    
053428     SKIP2                                                                
053429 IMS-GSAMFIL-ISRT SECTION.                                                
053430                                                                          
053431     MOVE '  '                TO GODK-STATUSKODER                         
053432     CALL CBLTDLI          USING ISRT GSAMFIL-PCB                         
053433                                 GSAMFIL-IO-AREA                          
053434     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
053435     PERFORM IMS-STATUSKONTROLL                                           
053436     .                                                                    
053437     SKIP2                                                                
053438 IMS-GSAMFIL-CLOSE SECTION.                                               
053439                                                                          
053440     MOVE '  ' TO GODK-STATUSKODER                                        
053441     CALL CBLTDLI USING CLSE-GSAM GSAMFIL-PCB                             
053442     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
053443     PERFORM IMS-STATUSKONTROLL                                           
053444     .                                                                    
053445     EJECT                                                                
053446 IMS-RESTART  SECTION.                                                    
053447                                                                          
053448     MOVE SPACE TO MSG-IO-AREA-1                                          
053449     MOVE '  ' TO GODK-STATUSKODER                                        
053450     CALL CBLTDLI USING XRST MSG-PCB                                      
053460                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
053470                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
053480     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053490     PERFORM IMS-STATUSKONTROLL                                           
053491     .                                                                    
053492     EJECT                                                                
053493 IMS-CHECKPOINT  SECTION.                                                 
053494                                                                          
053495     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
053496     MOVE '  XD' TO GODK-STATUSKODER                                      
053497     CALL CBLTDLI USING CHKP MSG-PCB                                      
053498                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
053499                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
053500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053501     PERFORM IMS-STATUSKONTROLL                                           
053502     .                                                                    
053503     EJECT                                                                
053510 IMS-STATUSKONTROLL SECTION.                                              
053600                                                                          
053700     SET STATUS-IX TO 1                                                   
053800     SEARCH GODK-STATUS AT END CALL FELLOG                                
053900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
054000     END-SEARCH                                                           
054100     .                                                                    
