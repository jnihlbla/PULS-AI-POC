000100*COMPOPT STDSUB=YES                                                       
000120 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W416PTID.                                                
000400 AUTHOR.         GUNILLA JOHANSSON.                                       
000500 DATE-WRITTEN.   91/01/30.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001010*        DETTA ÄR ETT SUBPROGRAM SOM ANROPAS NÄR EN                       
001100*        SATSORDER BLIR BYGGBAR.                                          
001200*        SPECIFIKT FÖR SATSORDER ÄR ATT PRC FÖR ORDERN ÄR GIVEN,          
001300*        I INTERVALLET 9980-998Z.                                         
001400*        I EN TABELL STYRS VARJE PRC TILL EN PRODUKTIONSTIDS-             
001500*        TABELL, INOM INTERVALLET 90-99.                                  
001600*        PLOCKTIDEN FÖR ORDERNA BERÄKNAS ENL WOPS-REGLER.                 
001700*        BYGGNATIONSTIDEN BERÄKNAS.                                       
001800*        BYGGNATIONSTID OCH PLOCKTID SUMMERAS TILL PRODUKTIONSTID.        
001900*                                                                         
002000*                                                                         
002100*        PROGRAMMET LÄSER     WLSATB (WDJ1)                               
002110*        PROGRAMMET LÄSER     WLXXKH (WDR1)                               
002200*        PROGRAMMET LÄSER     WLXXKI (WDR1)                               
002300*                                                                         
002400*    LÄNKAREA:  W416PTIDCO                                                
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 01  FILLER                      PIC X(16)     VALUE 'KONSTANTER'.        
004100 01  IDPGM                       PIC X(08)      VALUE 'W416PTID'.         
004200 01  JA                          PIC X          VALUE 'J'.                
004300 01  NEJ                         PIC X          VALUE 'N'.                
004400 01  OKAY                        PIC X          VALUE '0'.                
004500 01  EJ-OKAY                     PIC X          VALUE '1'.                
004600 01  KILO                        PIC X(2)       VALUE 'KG'.               
004700 01  VOLYM                       PIC X(2)       VALUE 'M3'.               
004800 01  RADER                       PIC X(2)       VALUE 'RA'.               
004900 01  STYCK                       PIC X(2)       VALUE 'ST'.               
005000 01  MEDELSTYCK                  PIC X(2)       VALUE 'MS'.               
005100     EJECT                                                                
005200*                                                                         
005300 01  FILLER                      PIC X(16)      VALUE 'WS-VÄRDEN'.        
005400 01  WS-IDSYSTEM                 PIC X(4).                                
005500 01  WS-IDARTNR                  PIC S9(9)      COMP-3.                   
005510 01  WS-IDORDNSB                 PIC S9(5)      COMP-3.                   
005600 01  WS-IDORDNSS                 PIC S9(1)      COMP-3.                   
005700 01  WS-IDPKLTAB                 PIC X(2).                                
005800 01  WS-IDPRC                    PIC X(4).                                
005900 01  WS-IDPRCTAB                 PIC 9(2).                                
006000 01  WS-KDCLAGER                 PIC S9(1)      COMP-3.                   
006100 01  WS-KVANTART                 PIC S9(5)      COMP-3.                   
006200 01  WS-KVBYGGB                  PIC S9(7)      COMP-3.                   
006300 01  WS-KVRADER                  PIC S9(5)      COMP-3.                   
006400 01  WS-SUHANTTI                 PIC S9(5)V9(2) COMP-3.                   
006600 01  WS-SUORDV                   PIC S9(9)V9(2) COMP-3.                   
006700 01  WS-TILST-O                  PIC S9(11)     COMP-3.                   
006800 01  WS-TIREGDAT                 PIC S9(7)      COMP-3.                   
006900 01  WS-TIREGTID                 PIC S9(7)      COMP-3.                   
007000 01  WS-VKORDNTO                 PIC S9(6)V9(5) COMP-3.                   
007100 01  WS-VLORDNTO                 PIC S9(4)V9(5) COMP-3.                   
007200     EJECT                                                                
007300*                                                                         
007400 01  FILLER             PIC X(15)  VALUE 'PRC-PTID-TABELL'.               
007500*                                                                         
007600 01  WS-TAB-PRC-IDPTIDTAB.                                                
007700     03  FILLER         PIC X(18)  VALUE '998090998191998292'.            
007710     03  FILLER         PIC X(6)   VALUE '998993'.                        
007800     03  FILLER         PIC X(18)  VALUE '998A94998B94998C94'.            
007900     03  FILLER         PIC X(18)  VALUE '998D94998E94998F94'.            
008000     03  FILLER         PIC X(18)  VALUE '998G94998H94998I94'.            
008100*                                                                         
008200 01  FILLER REDEFINES WS-TAB-PRC-IDPTIDTAB.                               
008300*                                                                         
008400     03 WS-TAB-RAD-PRC-IDPTIDTAB  OCCURS 13.                              
008500        05  WS-TAB-RAD-PRC        PIC X(4).                               
008600        05  WS-TAB-RAD-IDPTIDTAB  PIC 9(2).                               
008700*                                                                         
008800 01  FILLER                      PIC X(16)      VALUE 'DEFAULTID'.        
008900 01  DEFAULT-IDPTIDTAB-1         PIC 9(2)       VALUE 1.                  
009000 01  DEFAULT-IDPRC               PIC X(4)       VALUE '9980'.             
009100 01  IDUSER-NOLL                 PIC X(8)       VALUE '00000000'.         
009200*                                                                         
009300 01  FILLER                      PIC X(16)      VALUE 'JFR-FÄLT'.         
009400 01  X-KVPTSORT-JFR              PIC S9(4)V9    VALUE +0 COMP-3.          
009500 01  Y-KVPTSORT-JFR              PIC S9(4)V9    VALUE +0 COMP-3.          
009600     EJECT                                                                
009700*                                                                         
009800 01  FILLER                      PIC X(16)      VALUE 'SWITCHAR'.         
009900 01  SW-TRAEFF                   PIC X.                                   
010000 01  SW-X-SORT-FUNNEN            PIC X.                                   
010100 01  SW-Y-SORT-FUNNEN            PIC X.                                   
010200 01  SW-PTID                     PIC X.                                   
010300     EJECT                                                                
010400*                                                                         
010500 01  FILLER                      PIC X(16)      VALUE 'INDEX'.            
010900 01  LO-IX                       PIC S9(3)      VALUE +0 COMP-3.          
010910 01  PRC-IX                      PIC S9(3)      VALUE +0 COMP-3.          
010920 01  TAB-IX                      PIC S9(3)      VALUE +0 COMP-3.          
010930 01  LAGO-IX                     PIC S9(3)      VALUE +0 COMP-3.          
011000 01  W-4452-Y-IX                 PIC S9(3)      VALUE +0 COMP-3.          
011100 01  W-4452-X-IX                 PIC S9(3)      VALUE +0 COMP-3.          
011200     EJECT                                                                
011300*                                                                         
011400 01  TEMP-SUPMIN-PLOCK           PIC S9(7)      COMP-3.                   
011410 01  TEMP-SUPMIN-BYGG            PIC S9(7)      COMP-3.                   
011420 01  TEMP-SUPMIN-TOT             PIC S9(7)      COMP-3.                   
011500 01  TEMP-SUPTIM-PLOCK           PIC S9(5)      COMP-3.                   
011510 01  TEMP-SUPTIM-TOT             PIC S9(5)      COMP-3.                   
011600 01  TEMP-SUPTID-PLOCK           PIC S9(3)V9(2) COMP-3.                   
011610 01  TEMP-SUPTID-TOT             PIC S9(3)V9(2) COMP-3.                   
011700*                                                                         
011800     EJECT                                                                
011810*      --- VALID IDDC CODES                                               
011820*                                                                         
011830*01    -COPY WWDCKONS                                                     
011840       EJECT                                                              
011900*                                                                         
012000 01  GENERELLA-SUBPROGRAM.                                                
012100     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
012200     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
012300     EJECT                                                                
012400                                                                          
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                      PIC X(16)      VALUE 'IMS-WS'.           
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                          VALUE '  '.               
013200     88  BASEN-SLUT                             VALUE 'GB'.               
013300     88  SEGMENT-SAKNAS                         VALUE 'GE'.               
013400     88  SEGMENT-FINNS-REDAN                    VALUE 'II'.               
013500     SKIP2                                                                
013600 01  GODK-STATUSKODER.                                                    
013700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013800     SKIP2                                                                
013900 01  SSA1                        PIC X(300).                              
014000 01  SSA2                        PIC X(128).                              
014100 01  SSA3                        PIC X(64).                               
014200     EJECT                                                                
014300                                                                          
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*                                                                         
014900 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
015000 01  NYCKLAR-TILL-DLI.                                                    
015100                                                                          
015200     03  W-KDCLAGER-X.                                                    
015300         05  W-KDCLAGER          PIC  S9        COMP-3.                   
015400                                                                          
015500     03  W-KDSEGKEY-X.                                                    
015600         05  W-KDSEGKEY          PIC  X         VALUE '1'.                
015700                                                                          
015800     03  W-WDGXKEY-4447-X.                                                
015900         05  FILLER              PIC  X(4)   VALUE '4447'.                
016000         05  W-4447-IDDC         PIC  X(2)   VALUE SPACE.                 
016100         05  FILLER              PIC  X(24)  VALUE LOW-VALUE.             
016200                                                                          
016300     03  W-WDGXKEY-4448-X.                                                
016400         05  W-4448-IDPRC        PIC  X(4).                               
016500         05  FILLER              PIC  X(1)   VALUE LOW-VALUE.             
016600                                                                          
016700     03  W-WDGXKEY-4451-X.                                                
016800         05  FILLER              PIC  X(4)   VALUE '4451'.                
016900         05  W-4451-IDDC         PIC  X(2)   VALUE SPACE.                 
017000         05  W-4451-IDPTIDTAB    PIC  9(2).                               
017100         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
017110                                                                          
017120     03  W-WDJ1-IDARTNR-X.                                                
017130         05  W-WDJ1-IDARTNR      PIC  S9(9)  COMP-3.                      
017200                                                                          
017300     EJECT                                                                
017400*                                                                         
017500 01  FILLER                      PIC  X(16)  VALUE 'LOTABELL'.            
017600 01  INTERNTAB.                                                           
017700     03  INT-LO  OCCURS 99.                                               
017800       05  INT-KVANTART          PIC  S9(5)      COMP-3.                  
017900       05  INT-KVRADER           PIC  S9(5)      COMP-3.                  
018000       05  INT-SUHANTTI          PIC  S9(5)V9(2) COMP-3.                  
018100       05  INT-SUORDV            PIC  S9(9)V9(2) COMP-3.                  
018200       05  INT-VKORDNTO          PIC  S9(6)V9(5) COMP-3.                  
018300       05  INT-VLORDNTO          PIC  S9(4)V9(5) COMP-3.                  
018400       05  INT-IDPRC             PIC  X(4).                               
018500     EJECT                                                                
018600                                                                          
018700*                                                                         
018800 01  FILLER                      PIC  X(16)  VALUE 'PRCTABELL'.           
018900 01  PRCTAB.                                                              
019000     03  PRC OCCURS 99.                                                   
019100       05  PRC-IDPRC             PIC  X(4).                               
019200       05  PRC-IDPTIDTAB         PIC  9(2).                               
019300       05  PRC-KVVTID            PIC  S9(3)V9(2) COMP-3.                  
019400       05  PRC-TILST-OD          PIC  S9(11)     COMP-3.                  
019500       05  PRC-KVANTART-PT       PIC  S9(5)      COMP-3.                  
019600       05  PRC-KVRADER-PT        PIC  S9(5)      COMP-3.                  
019700       05  PRC-VKORDNTO-PT       PIC  S9(6)V9(5) COMP-3.                  
019800       05  PRC-VLORDNTO-PT       PIC  S9(4)V9(5) COMP-3.                  
019900       05  PRC-SUHANTTI-PT       PIC  S9(5)V9(2) COMP-3.                  
020000       05  PRC-SUORDV-PT         PIC  S9(9)V9(2) COMP-3.                  
020100       05  PRC-SUPTID            PIC  S9(3)V9(2) COMP-3.                  
020200       05  PRC-KVPTID            PIC  S9(2)V9(1) COMP-3.                  
020300       05  PRC-KVANTART          PIC  S9(5)      COMP-3.                  
020400       05  PRC-KVRADER           PIC  S9(5)      COMP-3.                  
020500       05  PRC-VKORDNTO          PIC  S9(6)V9(5) COMP-3.                  
020600       05  PRC-VLORDNTO          PIC  S9(4)V9(5) COMP-3.                  
020700       05  PRC-SUHANTTI          PIC  S9(5)V9(2) COMP-3.                  
020800       05  PRC-SUORDV            PIC  S9(9)V9(2) COMP-3.                  
020900     EJECT                                                                
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021200 01  DLI-IO-AREA.                                                         
021300     03  IO-AREA                 PIC X(310) VALUE SPACE.                  
021400     SKIP3                                                                
021500     03  WLXXKH11         REDEFINES IO-AREA.                              
021600*        05 -COPY WDGX4448                                                
021700     EJECT                                                                
021800                                                                          
021900     03  WLXXKI11         REDEFINES IO-AREA.                              
022000*        05 -COPY WDGX4452                                                
022010                                                                          
022020     03  WLSATB01         REDEFINES IO-AREA.                              
022030*        05 -COPY WDJ101     -PRE SATB-                                   
022100     EJECT                                                                
022200                                                                          
022300                                                                          
022400 LINKAGE SECTION.                                                         
022500*                                                                         
022600*01  -COPY W416PTID                                                       
022800     EJECT                                                                
022900*01  -COPY W0008    -PRE XXKH-                                            
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008    -PRE XXKI-                                            
023500     05  FILLER                  PIC X.                                   
023501     EJECT                                                                
023510*01  -COPY W0008    -PRE SATB-                                            
023520     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING PTID-W416PTID                                  
023800                           XXKH-PCB XXKI-PCB SATB-PCB.                    
024000                                                                          
024100     PERFORM A-INIT                                                       
024200           PERFORM S01-NOLLA-PRC-TAB                                      
024300           PERFORM B-BYGGTID                                              
024310           PERFORM C-PLOCKTID                                             
024400           PERFORM D-UPPDATERA-LNK                                        
024500     GOBACK                                                               
024600     .                                                                    
024700     EJECT                                                                
024800                                                                          
024900 A-INIT                        SECTION.                                   
025000                                                                          
025100     MOVE PTID-IDSYSTEM         TO WS-IDSYSTEM                            
025200     MOVE PTID-IDORDNSB         TO WS-IDORDNSB                            
025300     MOVE PTID-IDORDNSS         TO WS-IDORDNSS                            
025400     MOVE PTID-IDPRC            TO WS-IDPRC                               
025500     MOVE PTID-IDARTNR          TO WS-IDARTNR                             
025510     MOVE PTID-KDCLAGER         TO WS-KDCLAGER                            
025600     MOVE PTID-KVBYGGB          TO WS-KVBYGGB                             
025700     MOVE PTID-KVRADER          TO WS-KVRADER                             
025800     MOVE PTID-KVANTART         TO WS-KVANTART                            
025900     MOVE PTID-VKORDNTO         TO WS-VKORDNTO                            
026000     MOVE PTID-VLORDNTO         TO WS-VLORDNTO                            
026100     MOVE '1'                   TO PTID-KDSVAR                            
026200     MOVE +0                    TO WS-SUORDV                              
026300     MOVE +0                    TO WS-SUHANTTI                            
026410     MOVE +0                    TO TEMP-SUPTID-PLOCK                      
026411     MOVE +0                    TO TEMP-SUPTID-TOT                        
026420     MOVE +0                    TO TEMP-SUPTIM-PLOCK                      
026421     MOVE +0                    TO TEMP-SUPTIM-TOT                        
026430     MOVE +0                    TO TEMP-SUPMIN-PLOCK                      
026440     MOVE +0                    TO TEMP-SUPMIN-BYGG                       
026450     MOVE +0                    TO TEMP-SUPMIN-TOT                        
026500     .                                                                    
026600     EJECT                                                                
026700                                                                          
026710 B-BYGGTID                     SECTION.                                   
026720                                                                          
026730     MOVE WS-IDARTNR     TO W-WDJ1-IDARTNR                                
026740     PERFORM IMS-GU-WDJ1-SATB                                             
026760                                                                          
026795     IF SATB-STR-KVBYGMIN > 0                                             
026796        COMPUTE TEMP-SUPMIN-BYGG =                                        
026797                      WS-KVBYGGB *  SATB-STR-KVBYGMIN                     
026798        ADD     TEMP-SUPMIN-BYGG TO TEMP-SUPMIN-TOT                       
026799     END-IF                                                               
026800     .                                                                    
026801     EJECT                                                                
026810 C-PLOCKTID                    SECTION.                                   
026900                                                                          
027000     PERFORM CA-LAES-IN-ARBTAB                                            
027100                                                                          
027200                                                                          
027300     PERFORM CC-RAEKNA-PTID                                               
027400     .                                                                    
027500     EJECT                                                                
027600                                                                          
027700 CA-LAES-IN-ARBTAB             SECTION.                                   
027800                                                                          
027900     MOVE +1             TO LO-IX                                         
028000     MOVE WS-IDPRC       TO INT-IDPRC    (LO-IX)                          
028100     MOVE WS-KVANTART    TO INT-KVANTART (LO-IX)                          
028200     MOVE WS-KVRADER     TO INT-KVRADER  (LO-IX)                          
028300     MOVE WS-SUHANTTI    TO INT-SUHANTTI (LO-IX)                          
028400     MOVE WS-SUORDV      TO INT-SUORDV   (LO-IX)                          
028500     MOVE WS-VKORDNTO    TO INT-VKORDNTO (LO-IX)                          
028600     MOVE WS-VLORDNTO    TO INT-VLORDNTO (LO-IX)                          
028700                                                                          
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 CC-RAEKNA-PTID                SECTION.                                   
029200                                                                          
029300     MOVE +1                      TO PRC-IX                               
029410     IF WS-KDCLAGER               =  +1                                   
029420        MOVE WC-CDC-SE            TO W-4447-IDDC                          
029421                                     W-4451-IDDC                          
029430     ELSE                                                                 
029440        CALL FELLOG                                                       
029450     END-IF                                                               
029500     PERFORM UNTIL PRC-IX > 99                                            
029600       MOVE +1                    TO LO-IX                                
029700       PERFORM UNTIL (LO-IX > 99) OR (INT-IDPRC(LO-IX) > ZERO)            
029800         ADD +1                   TO LO-IX                                
029900       END-PERFORM                                                        
030000       IF LO-IX < 100                                                     
030100        MOVE INT-IDPRC (LO-IX)        TO PRC-IDPRC       (PRC-IX)         
030200                                         W-4448-IDPRC                     
030400                                                                          
030500        PERFORM IMS-GU-XXKH11                                             
030600                                                                          
030700        MOVE 4448-KVVTID              TO PRC-KVVTID      (PRC-IX)         
030800        PERFORM UNTIL LO-IX > 99                                          
030900         IF INT-IDPRC (LO-IX)         =  PRC-IDPRC       (PRC-IX)         
031000          PERFORM CCA-PTID-I-LO                                           
031100          MOVE '0000'                 TO INT-IDPRC       (LO-IX)          
031200          PERFORM CCB-MED-PTID                                            
031300         END-IF                                                           
031400         ADD +1                       TO LO-IX                            
031500        END-PERFORM                                                       
031600       ELSE                                                               
031700        MOVE DEFAULT-IDPRC            TO PRC-IDPRC       (PRC-IX)         
031800        MOVE +99                      TO PRC-IX                           
031900       END-IF                                                             
032000       ADD +1                         TO PRC-IX                           
032100     END-PERFORM                                                          
032200     PERFORM CCF-PTIDSBERAEKNING                                          
032300     .                                                                    
032400     EJECT                                                                
032500                                                                          
032600 CCA-PTID-I-LO                 SECTION.                                   
032700                                                                          
032800     MOVE +1 TO                   LAGO-IX                                 
032900     MOVE JA                      TO SW-PTID                              
033000     PERFORM UNTIL LAGO-IX > 10                                           
033100       IF 4448-ADLAGOMR (LAGO-IX)  > ZERO                                 
033200         IF 4448-ADLAGOMR(LAGO-IX) = LO-IX                                
033300           MOVE JA                TO SW-PTID                              
033400           MOVE +10               TO LAGO-IX                              
033500         ELSE                                                             
033600           MOVE NEJ               TO SW-PTID                              
033700         END-IF                                                           
033800       END-IF                                                             
033900       ADD +1                     TO LAGO-IX                              
034000     END-PERFORM                                                          
034100     .                                                                    
034200     EJECT                                                                
034300                                                                          
034400 CCB-MED-PTID                  SECTION.                                   
034500                                                                          
034600     ADD INT-KVANTART   (LO-IX) TO PRC-KVANTART-PT (PRC-IX)               
034700     ADD INT-KVRADER    (LO-IX) TO PRC-KVRADER-PT  (PRC-IX)               
034800     ADD INT-VKORDNTO   (LO-IX) TO PRC-VKORDNTO-PT (PRC-IX)               
034900     ADD INT-VLORDNTO   (LO-IX) TO PRC-VLORDNTO-PT (PRC-IX)               
035000     ADD INT-SUHANTTI   (LO-IX) TO PRC-SUHANTTI-PT (PRC-IX)               
035100     ADD INT-SUORDV     (LO-IX) TO PRC-SUORDV-PT   (PRC-IX)               
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500 CCF-PTIDSBERAEKNING           SECTION.                                   
035600                                                                          
035700     MOVE +1                      TO PRC-IX                               
035900     PERFORM UNTIL (PRC-IX > 99) OR (PRC-IDPRC (PRC-IX) = ZERO)           
036000       IF PRC-KVRADER-PT (PRC-IX)      > ZERO                             
036100         PERFORM S02-MATCHA-PRC-IDPTIDTAB                                 
036200         MOVE PRC-IDPTIDTAB (PRC-IX)  TO W-4451-IDPTIDTAB                 
036300         PERFORM IMS-GU-XXKI11                                            
036400         IF SEGMENT-SAKNAS                                                
036500           MOVE DEFAULT-IDPTIDTAB-1   TO W-4451-IDPTIDTAB                 
036600           PERFORM IMS-GU-XXKI11                                          
036700         END-IF                                                           
036800         PERFORM CCFA-PTID-PER-RAD                                        
036900       END-IF                                                             
037000                                                                          
037100       ADD +1                     TO PRC-IX                               
037200     END-PERFORM                                                          
037300     .                                                                    
037400     EJECT                                                                
037500                                                                          
037600 CCFA-PTID-PER-RAD             SECTION.                                   
037700                                                                          
037800     IF 4452-KDSORT-Y                  = KILO                             
037900       COMPUTE Y-KVPTSORT-JFR  =                                          
038000        PRC-VKORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)                
038100     ELSE                                                                 
038200       IF 4452-KDSORT-Y                = VOLYM                            
038300         COMPUTE Y-KVPTSORT-JFR  =                                        
038400          PRC-VLORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)              
038500       ELSE                                                               
038600         IF 4452-KDSORT-Y              = RADER                            
038700           MOVE PRC-KVRADER-PT(PRC-IX)     TO Y-KVPTSORT-JFR              
038800         ELSE                                                             
038900           IF 4452-KDSORT-Y            = STYCK                            
039000             MOVE PRC-KVANTART-PT(PRC-IX)  TO Y-KVPTSORT-JFR              
039100           ELSE                                                           
039200             IF 4452-KDSORT-Y          = MEDELSTYCK                       
039300               COMPUTE Y-KVPTSORT-JFR  =                                  
039400                PRC-KVANTART-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)        
039500             END-IF                                                       
039600           END-IF                                                         
039700         END-IF                                                           
039800       END-IF                                                             
039900     END-IF                                                               
040000*                                                                         
040100     IF 4452-KDSORT-X                  = KILO                             
040200       COMPUTE X-KVPTSORT-JFR  =                                          
040300        PRC-VKORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)                
040400     ELSE                                                                 
040500       IF 4452-KDSORT-X                = VOLYM                            
040600         COMPUTE X-KVPTSORT-JFR  =                                        
040700          PRC-VLORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)              
040800       ELSE                                                               
040900         IF 4452-KDSORT-X              = RADER                            
041000           MOVE PRC-KVRADER-PT(PRC-IX)     TO X-KVPTSORT-JFR              
041100         ELSE                                                             
041200           IF 4452-KDSORT-X            = STYCK                            
041300             MOVE PRC-KVANTART-PT(PRC-IX)  TO X-KVPTSORT-JFR              
041400           ELSE                                                           
041500             IF 4452-KDSORT-X          = MEDELSTYCK                       
041600               COMPUTE X-KVPTSORT-JFR  =                                  
041700               PRC-KVANTART-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)         
041800             END-IF                                                       
041900           END-IF                                                         
042000         END-IF                                                           
042100       END-IF                                                             
042200     END-IF                                                               
042300*                                                                         
042400     MOVE +1                          TO W-4452-Y-IX                      
042500     MOVE NEJ                         TO SW-Y-SORT-FUNNEN                 
042600     PERFORM UNTIL (W-4452-Y-IX > 9) OR (SW-Y-SORT-FUNNEN = JA)           
042700       IF 4452-KVPTSORT-Y (W-4452-Y-IX) >=  Y-KVPTSORT-JFR                
042800         MOVE JA                      TO SW-Y-SORT-FUNNEN                 
042900       ELSE                                                               
043000         ADD +1                       TO W-4452-Y-IX                      
043100       END-IF                                                             
043200     END-PERFORM                                                          
043300*                                                                         
043400     MOVE +1                          TO W-4452-X-IX                      
043500     MOVE NEJ                         TO SW-X-SORT-FUNNEN                 
043600     PERFORM UNTIL (W-4452-X-IX > 9) OR (SW-X-SORT-FUNNEN = JA)           
043700       IF 4452-KVPTSORT-X (W-4452-X-IX) >=  X-KVPTSORT-JFR                
043800         MOVE JA                      TO SW-X-SORT-FUNNEN                 
043900       ELSE                                                               
044000         ADD  +1                      TO W-4452-X-IX                      
044100       END-IF                                                             
044200     END-PERFORM                                                          
044300                                                                          
044400*                                                                         
044500     MOVE 4452-KVPTID (W-4452-Y-IX, W-4452-X-IX) TO                       
044600                         PRC-KVPTID (PRC-IX)                              
044700                                                                          
044800     COMPUTE TEMP-SUPMIN-PLOCK ROUNDED     =                              
044900      (PRC-KVPTID (PRC-IX) * PRC-KVRADER-PT (PRC-IX))                     
044910     ADD     TEMP-SUPMIN-PLOCK         TO TEMP-SUPMIN-TOT                 
044920                                                                          
045100     DIVIDE  TEMP-SUPMIN-PLOCK BY 60  GIVING TEMP-SUPTIM-PLOCK            
045200     COMPUTE TEMP-SUPMIN-PLOCK =                                          
045210             TEMP-SUPMIN-PLOCK - (TEMP-SUPTIM-PLOCK * 60)                 
045300                                                                          
045400     DIVIDE  TEMP-SUPMIN-PLOCK BY 100  GIVING  TEMP-SUPTID-PLOCK          
045500     ADD     TEMP-SUPTIM-PLOCK         TO TEMP-SUPTID-PLOCK               
045600                                                                          
045800     MOVE    TEMP-SUPTID-PLOCK         TO PRC-SUPTID (PRC-IX)             
046000                                                                          
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 D-UPPDATERA-LNK               SECTION.                                   
046500                                                                          
046510     IF TEMP-SUPMIN-TOT        > 0                                        
046511        MOVE '0'             TO PTID-KDSVAR                               
046520        DIVIDE TEMP-SUPMIN-TOT BY 60  GIVING TEMP-SUPTIM-TOT              
046530        COMPUTE TEMP-SUPMIN-TOT =                                         
046540                TEMP-SUPMIN-TOT - (TEMP-SUPTIM-TOT * 60)                  
046550                                                                          
046560        DIVIDE TEMP-SUPMIN-TOT BY 100 GIVING  TEMP-SUPTID-TOT             
046570        ADD  TEMP-SUPTIM-TOT   TO      TEMP-SUPTID-TOT                    
046593        MOVE TEMP-SUPTID-TOT TO PTID-SUSATPTI                             
046594     END-IF                                                               
046700     MOVE WS-IDPRC           TO PTID-IDPRC                                
046800     MOVE WS-IDORDNSB        TO PTID-IDORDNSB                             
046900     MOVE WS-IDORDNSS        TO PTID-IDORDNSS                             
047000     MOVE WS-KDCLAGER        TO PTID-KDCLAGER                             
047100     MOVE WS-KVBYGGB         TO PTID-KVBYGGB                              
047200     MOVE WS-KVRADER         TO PTID-KVRADER                              
047300     MOVE WS-VKORDNTO        TO PTID-VKORDNTO                             
047400     MOVE WS-VLORDNTO        TO PTID-VLORDNTO                             
047800     .                                                                    
047900     EJECT                                                                
048000 S01-NOLLA-PRC-TAB             SECTION.                                   
048100                                                                          
048200     MOVE +1                 TO PRC-IX                                    
048300     PERFORM UNTIL PRC-IX     > 99                                        
048400       MOVE ZERO             TO PRC-IDPTIDTAB   (PRC-IX)                  
048500                                PRC-KVVTID      (PRC-IX)                  
048600                                PRC-KVANTART-PT (PRC-IX)                  
048700                                PRC-KVRADER-PT  (PRC-IX)                  
048800                                PRC-VKORDNTO-PT (PRC-IX)                  
048900                                PRC-VLORDNTO-PT (PRC-IX)                  
049000                                PRC-SUHANTTI-PT (PRC-IX)                  
049100                                PRC-SUORDV-PT   (PRC-IX)                  
049200                                PRC-SUPTID      (PRC-IX)                  
049300                                PRC-KVPTID      (PRC-IX)                  
049400                                PRC-KVANTART    (PRC-IX)                  
049500                                PRC-KVRADER     (PRC-IX)                  
049600                                PRC-VKORDNTO    (PRC-IX)                  
049700                                PRC-VLORDNTO    (PRC-IX)                  
049800                                PRC-SUHANTTI    (PRC-IX)                  
049900                                PRC-SUORDV      (PRC-IX)                  
050000       MOVE '0000'           TO PRC-IDPRC       (PRC-IX)                  
050100       ADD +1                TO PRC-IX                                    
050200     END-PERFORM                                                          
050300     MOVE +99999999999       TO WS-TILST-O                                
050310     MOVE NEJ                TO SW-TRAEFF                                 
050400     .                                                                    
050500                                                                          
050600                                                                          
050700 S02-MATCHA-PRC-IDPTIDTAB   SECTION.                                      
050800                                                                          
051000     MOVE +1 TO TAB-IX                                                    
051100     PERFORM UNTIL (TAB-IX > 13)                                          
051200       OR (PRC-IDPRC (PRC-IX) = WS-TAB-RAD-PRC (TAB-IX))                  
051300       ADD +1 TO TAB-IX                                                   
051400     END-PERFORM                                                          
051500                                                                          
051600     IF TAB-IX < 14                                                       
051700       MOVE WS-TAB-RAD-IDPTIDTAB (TAB-IX) TO                              
051800                       PRC-IDPTIDTAB (PRC-IX)                             
051900     END-IF                                                               
052000     .                                                                    
052100 EJECT                                                                    
052200 IMS-GU-WDJ1-SATB              SECTION.                                   
052300                                                                          
052400     STRING 'WLSATB01(IDARTNR  =' W-WDJ1-IDARTNR-X ')'                    
052500            DELIMITED BY SIZE INTO SSA1                                   
052800     MOVE '    '              TO GODK-STATUSKODER                         
052900     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
053000     MOVE SATB-STATUS-CODE      TO STATUS-WS                              
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053310 IMS-GU-XXKH11                 SECTION.                                   
053320                                                                          
053330     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
053340            DELIMITED BY SIZE INTO SSA1                                   
053350     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
053360            DELIMITED BY SIZE INTO SSA2                                   
053370     MOVE '    '              TO GODK-STATUSKODER                         
053380     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
053390     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
053391     PERFORM IMS-STATUSKONTROLL                                           
053392     .                                                                    
053393     EJECT                                                                
053400                                                                          
053500 IMS-GU-XXKI11                 SECTION.                                   
053600                                                                          
053700     STRING 'WLXXKI01(WDGXKEY  =' W-WDGXKEY-4451-X ')'                    
053800            DELIMITED BY SIZE INTO SSA1                                   
053900     STRING 'WLXXKI11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
054000            DELIMITED BY SIZE INTO SSA2                                   
054100     MOVE '  GE'              TO GODK-STATUSKODER                         
054200     CALL CBLTDLI USING GU XXKI-PCB DLI-IO-AREA SSA1 SSA2                 
054300     MOVE XXKI-STATUS-CODE      TO STATUS-WS                              
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600     EJECT                                                                
054700                                                                          
054800 IMS-STATUSKONTROLL            SECTION.                                   
054900                                                                          
055000     SET STATUS-IX             TO 1                                       
055100     SEARCH GODK-STATUS AT END CALL FELLOG                                
055200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
055300     END-SEARCH                                                           
055400     .                                                                    
