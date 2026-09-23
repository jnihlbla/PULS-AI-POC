000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1022200.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   JUNI 1990.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        MED DETTA PROGRAM KAN MAN TA BORT/BYTA UT ARTIKEL I              
001100*        STRUKTURER SOM VALTS FRÅN BILD 1221 (INGÅR I STRUKTUR).          
001200*        TILLKOMMANDE ARTIKLAR LÄGGS PÅ WDR5. NÄR BEARBETNINGEN           
001300*        KLAR STARTAS ETT BAKGRUNDSPGM (W10294) SOM TAR HAND OM           
001400*        UPPDATERINGEN/OMNUMRERINGEN AV RADER SAMT RENSAR BORT            
001500*        UPPGIFTERNA PÅ WDR5 OCH DE KONVERTERADE (VALDA)                  
001600*        STRUKTURERNA PÅ WDJ1.                                            
001700*                                                                         
001800*        ÄT SPLIT 930404 BL                                               
001900*          - KONTROLL KDPRODSL FÖRPACKNING ÄNDRAT                         
002000*                                                                         
002100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002200*        PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
002300*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
002400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002600*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
002700*                                                                         
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W1T222                                              
003100*        MID:         W1I22201                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W1O22201                                            
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100*    -COPY WY2000W3                                                       
004200     SKIP3                                                                
004300*    -COPY WY2000W1                                                       
004400     SKIP3                                                                
004500 77  IDPGM                       PIC X(08)   VALUE 'W1022200'.            
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +2    COMP SYNC.        
005300                                                                          
005400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005500                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005900 77  WS-BELEVART                 PIC X(25)   VALUE SPACE.                 
006000                                                                          
006100*    --- ARBETSFÄLT FÖR ATT KUNNA SPARA NYCKELVÄRDEN                      
006200*    --- NÄR MAN KOMMER FRÅN BILD 1221                                    
006300 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
006400 77  WS-1002-SATS                PIC X(1)    VALUE SPACE.                 
006500 77  WS-KDPRODSL                 PIC X(2)    VALUE SPACE.                 
006600                                                                          
006700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006800     88  INDATA-OK                           VALUE 'J'.                   
006900     88  INDATA-FEL                          VALUE 'N'.                   
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007600     88  ALLT-OK                             VALUE 'J'.                   
007700                                                                          
007800 77  FINNS-SOM-RAD-SW            PIC X       VALUE SPACE.                 
007900     88 FINNS-SOM-RAD                        VALUE 'J'.                   
008000                                                                          
008100 77  STRUKTURNR-FINNS-I-TABELL-SW PIC X      VALUE SPACE.                 
008200     88 STRUKTURNR-FINNS-I-TABELL            VALUE 'J'.                   
008300                                                                          
008400 77  ERSATTNINGS-SW               PIC X      VALUE SPACE.                 
008500     88  FINNS-SOM-ERSATTNING         VALUE 'J'.                          
008600                                                                          
008700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008800     88  EGEN-MID                            VALUE '1222'.                
008900     88  GODK-MID                            VALUE '1221' '1222'.         
009000                                                                          
009100 77  WS-KDSORT-GODK              PIC X(2)    VALUE SPACE.                 
009200     88  KDSORT-GODK                         VALUE 'ST' 'SA' 'KG'         
009300                                                   'M ' ' M' 'L '         
009400                                                   ' L' 'MM' 'G '         
009500                                                   ' G' 'C2' 'M2'         
009600                                                  'ML' 'TM' 'PA'.         
009700                                                                          
009800 77  WS-BEART-SVE                PIC X(25)   VALUE SPACE.                 
009900 77  WS-BEART-GB                 PIC X(25)   VALUE SPACE.                 
010000                                                                          
010100     EJECT                                                                
010200*                                                                         
010300*01    -COPY WWPRODSL                                                     
010400*      --- VALID IDDC CODES                                               
010500*                                                                         
010600*01    -COPY WWDCKONS                                                     
010700*01    -COPY WWDC99                                                       
010800       EJECT                                                              
010900********************************************                              
011000*           UPPDATERINGS-TYPER             *                              
011100********************************************                              
011200                                                                          
011300 77  UPDATE-TYP-SW               PIC X       VALUE SPACE.                 
011400     88  UPDATE-TILLKOMMANDE-ARTIKLAR        VALUE 'T'.                   
011500     88  UPDATE-BYTE-ARTIKLAR                VALUE 'B'.                   
011600     88  UPDATE-BORTTAG-ARTIKLAR             VALUE 'D'.                   
011700     88  UPDATE-AANGRA-ARTIKLAR              VALUE 'A'.                   
011800     88  UPDATE-ENDAST-IDAO-TIAAVV           VALUE 'E'.                   
011900                                                                          
012000     EJECT                                                                
012100********************************************                              
012200*           SOEKNYCKEL-TYPER               *                              
012300********************************************                              
012400                                                                          
012500 77  SOEKNYCKELTYP-SW            PIC X       VALUE SPACE.                 
012600     88  SOEKNYCKEL-IDARTNR                  VALUE 'A'.                   
012700     88  SOEKNYCKEL-IDARTNR-O-BELEV          VALUE 'I'.                   
012800                                                                          
012900 77  SOEKNYCKEL-LAAST-AV-EGET-ID-SW PIC X    VALUE SPACE.                 
013000     88  SOEKNYCKEL-LAAST-AV-EGET-ID         VALUE 'J'.                   
013100                                                                          
013200     EJECT                                                                
013300********************************************                              
013400*    TYP AV NYCKEL OM NYCKEL ÄR ARTIKELNR  *                              
013500********************************************                              
013600                                                                          
013700 77  IDARTNR-FINNS-PAA-ARTREG-SW    PIC X    VALUE SPACE.                 
013800     88  IDARTNR-FINNS-PAA-ARTREG            VALUE 'J'.                   
013900                                                                          
014000 77  IDARTNR-FORPACKNING-SW         PIC X    VALUE SPACE.                 
014100     88  IDARTNR-FORPACKNING                 VALUE 'J'.                   
014200                                                                          
014300 77  IDARTNR-ALT-ERS-PAA-ARTREG-SW  PIC X    VALUE SPACE.                 
014400     88  IDARTNR-ALT-ERSATT-PAA-ARTREG       VALUE 'J'.                   
014500                                                                          
014600 77  EN-AV-VALD-STRUKT-1002-SATS-SW PIC X    VALUE SPACE.                 
014700     88 EN-AV-VALDA-STRUKT-1002-SATS         VALUE 'J'.                   
014800                                                                          
014900     EJECT                                                                
015000********************************************                              
015100*       TESTER AV VAD SOM ÄR IFYLLT        *                              
015200********************************************                              
015300                                                                          
015400 77  MID-RAD-IFYLLD-SW             PIC X      VALUE SPACE.                
015500     88 MID-RAD-IFYLLD                        VALUE 'J'.                  
015600                                                                          
015700 77  MID-RAD-1-IFYLLD-SW           PIC X      VALUE SPACE.                
015800     88 MID-RAD-1-IFYLLD                      VALUE 'J'.                  
015900                                                                          
016000 77  MID-RAD-2-IFYLLD-SW           PIC X      VALUE SPACE.                
016100     88 MID-RAD-2-IFYLLD                      VALUE 'J'.                  
016200                                                                          
016300 77  SVARSFLAGGA-IFYLLD-SW         PIC X      VALUE SPACE.                
016400     88 SVARSFLAGGA-IFYLLD                    VALUE 'J'.                  
016500                                                                          
016600 77  IDAO-OCH-TIAAVV-AENDRAD-SW    PIC X      VALUE SPACE.                
016700     88 IDAO-OCH-TIAAVV-AENDRAD               VALUE 'J'.                  
016800     EJECT                                                                
016900                                                                          
017000*    --- DIVERSE VARIABLER                                                
017100                                                                          
017200 01  TILLKOMMANDE                PIC X     VALUE 'T'.                     
017300 01  BYTE                        PIC X     VALUE 'B'.                     
017400 01  BORTTAG                     PIC X     VALUE 'D'.                     
017500 01  AANGRA                      PIC X     VALUE 'A'.                     
017600 01  ENDAST-AENDRING-IDAO-TIAAVV PIC X     VALUE 'E'.                     
017700                                                                          
017800 01  IDARTNR                     PIC X     VALUE 'A'.                     
017900 01  IDLEVNR-O-BELEVART          PIC X     VALUE 'I'.                     
018000                                                                          
018100 01  DAGENS-DATUM                PIC 9(6).                                
018200 01  DAGENS-DATUM-AAVV           PIC 9(4).                                
018300 01  WS-TIAAVV-GRP               PIC 9(4).                                
018400 01  WS-TISTODAT                 PIC 9(6).                                
018500 01  WS-ANTAL                    PIC 9(3).                                
018600 01  WS-ANTAL-RADER              PIC 9(3).                                
018700 01  WS-ANTAL-ERS-RADER          PIC 9(3).                                
018800 01  WS-IDARTNR-OKONV            PIC 9(9).                                
018900 01  WS-IDARTNR-NUM              PIC 9(9).                                
019000 01  WS-IDARTNR-SPAR             PIC 9(9).                                
019100 01  WS-KDBENHOM-NUM             PIC 9(1).                                
019200 01  WS-KDBENHOM                 PIC 9(1).                                
019300 01  WS-MIN-IDARTNR-KONV         PIC S9(9) VALUE +100000000.              
019400 01  WS-MAX-IDARTNR-OKONV        PIC S9(9) VALUE +099999999.              
019500                                                                          
019600 01  INDX2                       PIC S9(3) VALUE ZERO.                    
019700 01  TAB-INDX                    PIC S9(3) VALUE ZERO.                    
019800 01  TAB-INDX2                   PIC S9(3) VALUE ZERO.                    
019900 01  KONTROLL-TAB-INDX           PIC S9(3) VALUE ZERO.                    
020000 01  MAX-TABELL-LAENGD           PIC S9(3) VALUE +100.                    
020100                                                                          
020200 01  TRANS-TILL-1294             PIC X     VALUE SPACE.                   
020300                                                                          
020400     EJECT                                                                
020500*    ---------------- TABELLER                                            
020600                                                                          
020700 01  STRUKTURNRTABELL.                                                    
020800     03 STRUKTURNR  OCCURS 100   PIC S9(9) COMP-3.                        
020900                                                                          
021000     EJECT                                                                
021100*    ---------------- MEDDELANDEN                                         
021200                                                                          
021300 01  MEDDELANDEN.                                                         
021400     03 FEL-1.                                                            
021500        05 FILLER                   PIC X(40)   VALUE                     
021600           'TILLKOMMANDE ARTIKLAR SAKNAS'.                                
021700        05 FILLER                   PIC X(40)   VALUE                     
021800            'NO NEW PARTS ARE REGISTRED'.                                 
021900     03  FILLER  REDEFINES FEL-1.                                         
022000        05 FEL1 OCCURS 2            PIC X(40).                            
022100                                                                          
022200     03 FEL-2.                                                            
022300        05 FILLER                   PIC X(40)   VALUE                     
022400           'TILLKOMMANDE ARTIKLAR FINNS REGISTRERADE'.                    
022500        05 FILLER                   PIC X(40)   VALUE                     
022600           'NEW PART NO. REGISTRED'.                                      
022700     03  FILLER  REDEFINES FEL-2.                                         
022800        05 FEL2 OCCURS 2            PIC X(40).                            
022900                                                                          
023000     03 FEL-3.                                                            
023100        05 FILLER                   PIC X(40)   VALUE                     
023200           'FÖRPACKNING KAN EJ TAS BORT'.                                 
023300        05 FILLER                   PIC X(40)   VALUE                     
023400           'PACKAGE LINE. DELETE NOT POSSIBLE'.                           
023500     03  FILLER  REDEFINES FEL-3.                                         
023600        05 FEL3 OCCURS 2            PIC X(40).                            
023700                                                                          
023800     03 FEL-4.                                                            
023900        05 FILLER                   PIC X(60)   VALUE                     
024000           'VALDA RADER EJ ERSÄTTNINGSMÄRKTA I RASA '.                    
024100        05 FILLER                   PIC X(60)   VALUE                     
024200           'PART NO. NOT SUPERSEDED IN STRUCTURE'.                        
024300     03  FILLER  REDEFINES FEL-4.                                         
024400        05 FEL4 OCCURS 2            PIC X(60).                            
024500                                                                          
024600     03 MED-1.                                                            
024700        05 FILLER                   PIC X(40)   VALUE                     
024800           'REGISTRERING UTFÖRD. STRUKTUR EJ KLAR   '.                    
024900        05 FILLER                   PIC X(40)   VALUE                     
025000           'UPDATED. STRUCTURE NOT COMPLETE'.                             
025100     03  FILLER  REDEFINES MED-1.                                         
025200        05 MED1 OCCURS 2            PIC X(40).                            
025300                                                                          
025400     03 MED-2.                                                            
025500        05 FILLER                   PIC X(60)   VALUE                     
025600           'TILLKOMMANDE ARTIKLAR MÅSTE FINNAS PÅ ART.REG.'.              
025700        05 FILLER                   PIC X(60)   VALUE                     
025800           'PART NO. IS MISSING'.                                         
025900     03  FILLER  REDEFINES MED-2.                                         
026000        05 MED2 OCCURS 2            PIC X(60).                            
026100                                                                          
026200     03 MED-3.                                                            
026300        05 FILLER                   PIC X(40)   VALUE                     
026400           'ARTIKEL EJ TILLKOMMANDE'.                                     
026500        05 FILLER                   PIC X(40)   VALUE                     
026600           'NOT A SUPERSEDING PART'.                                      
026700     03  FILLER  REDEFINES MED-3.                                         
026800        05 MED3 OCCURS 2            PIC X(40).                            
026900                                                                          
027000     03 MED-4.                                                            
027100        05 FILLER                   PIC X(40)   VALUE                     
027200           'ARTIKEL EJ FÖRPACKNING                  '.                    
027300        05 FILLER                   PIC X(40)   VALUE                     
027400           'NOT A PACKAGE LINE'.                                          
027500     03  FILLER  REDEFINES MED-4.                                         
027600        05 MED4 OCCURS 2            PIC X(40).                            
027700                                                                          
027800     03 MED-5.                                                            
027900        05 FILLER                   PIC X(40)   VALUE                     
028000           'STRUKTUR KOMMER ATT INGÅ I SIG SJÄLV    '.                    
028100        05 FILLER                   PIC X(40)   VALUE                     
028200           'UPDATE NOT POSSIBLE'.                                         
028300     03  FILLER  REDEFINES MED-5.                                         
028400        05 MED5 OCCURS 2            PIC X(40).                            
028500                                                                          
028600     03 MED-6.                                                            
028700        05 FILLER                   PIC X(40)   VALUE                     
028800           'ARTIKEL RENSAD PÅ ARTIKELREGISTRET      '.                    
028900        05 FILLER                   PIC X(40)   VALUE                     
029000           'PART NUMBER DELETED'.                                         
029100     03  FILLER  REDEFINES MED-6.                                         
029200        05 MED6 OCCURS 2            PIC X(40).                            
029300                                                                          
029400     03 MED-7.                                                            
029500        05 FILLER                   PIC X(40)   VALUE                     
029600           'STRUKTUR BORTTAGSMÄRKT'.                                      
029700        05 FILLER                   PIC X(40)   VALUE                     
029800           'STRUCTURE IS MARKED TO BE DELETED'.                           
029900     03  FILLER  REDEFINES MED-7.                                         
030000        05 MED7 OCCURS 2            PIC X(40).                            
030100                                                                          
030200     03 MED-8.                                                            
030300        05 FILLER                   PIC X(60)   VALUE                     
030400           'ARTIKEL ERSÄTTNINGSMÄRKT PÅ ARTIKELREGISTRET'.                
030500        05 FILLER                   PIC X(60)   VALUE                     
030600           'THIS PART IS SUPERSEDED'.                                     
030700     03  FILLER  REDEFINES MED-8.                                         
030800        05 MED8 OCCURS 2            PIC X(60).                            
030900     EJECT                                                                
031000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
031100 01  GENERELLA-SUBPROGRAM.                                                
031200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
031300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
031400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
031500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
031600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
031700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
031800     EJECT                                                                
031900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
032000*   -COPY WMEDAREA                                                        
032100     SKIP3                                                                
032200*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
032300*   -COPY WDECAREA                                                        
032400     SKIP3                                                                
032500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
032600*   -COPY WDATAREA                                                        
032700     SKIP3                                                                
032800*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
032900*   -COPY WORKAREA                                                        
033000     SKIP3                                                                
033100 01  MESSAGE-CODES.                                                       
033200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
033300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
033400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
033500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
033600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
033700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
033800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
033900     EJECT                                                                
034000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
034100*                                                                         
034200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034300     SKIP3                                                                
034400*01  MID -COPY W1I22201                                                   
034500     EJECT                                                                
034600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
034700     SKIP3                                                                
034800*01  -COPY WMSGAREA                                                       
034900     EJECT                                                                
035000     03  MOD REDEFINES MSG-AREA.                                          
035100*      05  -COPY W1O22201                                                 
035200     EJECT                                                                
035300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035400     SKIP3                                                                
035500*01  -COPY WMFSAREA                                                       
035600     EJECT                                                                
035700*    --------------------- ALT-AREA                                       
035800*                                                                         
035900 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
036000     SKIP3                                                                
036100 01  W-PROG-TO-PROG-SW.                                                   
036200     03  M-SW-LL                 PIC S9(4)   VALUE +571                   
036300                                             COMP SYNC.                   
036400     03  M-SW-Z1-Z2              PIC  X(2)   VALUE LOW-VALUE.             
036500     03  M-SW-KDTRANS            PIC  X(8)   VALUE 'W1T294X '.            
036600     03  M-SW-IDTRANS            PIC  X(4)   VALUE '1294'.                
036700     03  M-SW-KDMFSTYP           PIC  X(1)   VALUE '1'.                   
036800     03  MID-W1I29401.                                                    
036900*        05 MID -COPY W1I29401   -PRE R1294-                              
037000                                                                          
037100     EJECT                                                                
037200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
037300*                                                                         
037400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037500     SKIP3                                                                
037600 01  NYCKLAR-TILL-DLI.                                                    
037700                                                                          
037800     03  W-WDJ111KY-X.                                                    
037900         05  W-KDSTRRAD          PIC X(1)    VALUE SPACE.                 
038000         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
038100                                                                          
038200     03  W-IDLEVNR-X.                                                     
038300         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
038400                                                                          
038500     03  W-BELEVART-X.                                                    
038600         05  W-BELEVART          PIC X(30)   VALUE SPACE.                 
038700                                                                          
038800     03  W-IDARTNR-X.                                                     
038900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
039000                                                                          
039100     03  W-IDARTNR-2-X.                                                   
039200         05  W-IDARTNR-2         PIC S9(9)   VALUE ZERO COMP-3.           
039300                                                                          
039400     03  W-MIN-IDARTNR-KONV-X.                                            
039500         05  W-MIN-IDARTNR-KONV  PIC S9(9) VALUE                          
039600                                             +100000000 COMP-3.           
039700                                                                          
039800     03  W-MAX-IDARTNR-KONV-X.                                            
039900         05  W-MAX-IDARTNR-KONV  PIC S9(9) VALUE                          
040000                                             +999999999 COMP-3.           
040100                                                                          
040200     03  W-IDUSER-X.                                                      
040300         05  W-IDUSER            PIC  X(8)   VALUE SPACE.                 
040400                                                                          
040500     03  W-IDSKYLT-X.                                                     
040600         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
040700                                                                          
040800     03  W-BEART-X.                                                       
040900         05 W-BEART              PIC X(25)   VALUE SPACE.                 
041000                                                                          
041100     03  W-IDBENR-X.                                                      
041200         05  W-IDBENR            PIC S9(5)   VALUE ZERO COMP-3.           
041300                                                                          
041400     03  W-KDCLAGER-X.                                                    
041500         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
041600                                                                          
041700     03  W-KDSEGKEY-X.                                                    
041800         05  W-KDSEGKEY          PIC  X(1)   VALUE SPACE.                 
041900                                                                          
042000     03  W-IDHTYP-X.                                                      
042100         05  W-IDHTYP            PIC  X(4)   VALUE SPACE.                 
042200                                                                          
042300     03  W-LOW-VALUE-2-X.                                                 
042400         05  W-LOW-VALUE-2       PIC X(26)   VALUE SPACE.                 
042500                                                                          
042600     03  W-LOW-VALUE-X.                                                   
042700         05  W-LOW-VALUE         PIC  X(4)   VALUE SPACE.                 
042800                                                                          
042900*    --- STATUS-KOD FRÅN IMS                                              
043000 01  STATUS-WS                   PIC XX.                                  
043100     88  SEGMENT-FINNS                       VALUE '  '.                  
043200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
043300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
043400     SKIP2                                                                
043500*    --- STATUS-KODER SOM ANVÄNDS VISSA TESTER                            
043600 01  STATUS-RAD-WS               PIC XX.                                  
043700     88  RADSEGMENT-SAKNAS                   VALUE 'GE'.                  
043800                                                                          
043900 01  STATUS-KONV-WS              PIC XX.                                  
044000     88  KONVERTERADE-SEGMENT-SAKNAS         VALUE 'GE'.                  
044100     SKIP2                                                                
044200 01  GODK-STATUSKODER.                                                    
044300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044400     SKIP3                                                                
044500 01  SSA1                        PIC X(96).                               
044600 01  SSA2                        PIC X(96).                               
044700 01  SSA3                        PIC X(64).                               
044800     EJECT                                                                
044900*    --- IMS FUNKTIONSKODER                                               
045000*01  -COPY W0003                                                          
045100     EJECT                                                                
045200*    ---  DLI INPUT-OUTPUT AREA                                           
045300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
045400     SKIP3                                                                
045500 01  DLI-IO-AREA.                                                         
045600     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
045700     SKIP3                                                                
045800     03  WLSATB01 REDEFINES IO-AREA.                                      
045900*        05  -COPY WDJ101     -PRE SATB01-                                
046000     EJECT                                                                
046100     03  WLSATB11 REDEFINES IO-AREA.                                      
046200*        05  -COPY WDJ111     -PRE SATB11-                                
046300     SKIP3                                                                
046400     03  WLSATB-CSEQ REDEFINES IO-AREA.                                   
046500         05  WLSATB11.                                                    
046600*            07 -COPY WDJ111     -PRE SATB11C-                            
046700         05  WLSATB01.                                                    
046800*            07 -COPY WDJ101     -PRE SATB01C-                            
046900     SKIP3                                                                
047000     03  WLARTC01 REDEFINES IO-AREA.                                      
047100*        05  -COPY WDK601                                                 
047200     SKIP3                                                                
047300     03  WLARTC11 REDEFINES IO-AREA.                                      
047400*        05  -COPY WDK611                                                 
047500     SKIP3                                                                
047600     03  WLBENA01 REDEFINES IO-AREA.                                      
047700*        05  -COPY WDD301     -PRE BENA01-                                
047800     SKIP3                                                                
047900     03  WLBENA11 REDEFINES IO-AREA.                                      
048000*        05  -COPY WDD311     -PRE BENA11-                                
048100     SKIP3                                                                
048200     03  WLERSA01 REDEFINES IO-AREA.                                      
048300*        05  -COPY WDD701     -PRE ERSA01-                                
048400     SKIP3                                                                
048500     03  WLERSA11 REDEFINES IO-AREA.                                      
048600*        05  -COPY WDD702     -PRE ERSA11-                                
048700     SKIP3                                                                
048800     03  WLXXAZ11 REDEFINES IO-AREA.                                      
048900*        05  -COPY WDGX1152   -PRE XXAZ11-                                
049000     EJECT                                                                
049100*    ---  DLI INPUT-OUTPUT AREA 2                                         
049200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
049300     SKIP3                                                                
049400 01  DLI-IO-AREA2.                                                        
049500     03  IO-AREA2                PIC X(256)  VALUE SPACE.                 
049600     SKIP3                                                                
049700     03  WLXXAZ21 REDEFINES IO-AREA2.                                     
049800*        05  -COPY WDGX1154   -PRE XXAZ21-                                
049900     EJECT                                                                
050000 LINKAGE SECTION.                                                         
050100                                                                          
050200*01  -COPY W0009      -PRE MSG-                                           
050300     EJECT                                                                
050400*01  -COPY W0009      -PRE ALT-                                           
050500     EJECT                                                                
050600*01  -COPY W0008      -PRE SATB-                                          
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008      -PRE SATB-C-                                        
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008      -PRE SATB-D-                                        
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008      -PRE ARTC-                                          
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008      -PRE BENA-A-                                        
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008      -PRE BENA-B-                                        
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008      -PRE ERSA-                                          
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008      -PRE XXAZ-                                          
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB SATB-PCB SATB-C-PCB            
053100                           SATB-D-PCB ARTC-PCB BENA-A-PCB                 
053200                           BENA-B-PCB ERSA-PCB XXAZ-PCB.                  
053300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB SATB-PCB SATB-C-PCB            
053400                           SATB-D-PCB ARTC-PCB BENA-A-PCB                 
053500                           BENA-B-PCB ERSA-PCB XXAZ-PCB.                  
053600                                                                          
053700     PERFORM IMS-GET-MSG                                                  
053800     IF SEGMENT-FINNS                                                     
053900       PERFORM A-INIT                                                     
054000       PERFORM B-KOLLA-NYCKLAR                                            
054100       IF NYCKLAR-OK                                                      
054200         IF MFS-UPDATE                                                    
054300           PERFORM G-KOLLA-INPUT                                          
054400           IF INDATA-OK                                                   
054500             PERFORM H-UPPDATERA                                          
054600           END-IF                                                         
054700         ELSE                                                             
054800           IF MFS-FIRST                                                   
054900             PERFORM C-FOERSTA-SIDA                                       
055000           ELSE                                                           
055100             IF MFS-NEXT                                                  
055200               PERFORM D-NAESTA-SIDA                                      
055300             ELSE                                                         
055400               PERFORM E-SAMMA-SIDA                                       
055500             END-IF                                                       
055600           END-IF                                                         
055700         END-IF                                                           
055800         IF (ALLT-OK) AND (TRANS-TILL-1294 NOT = JA)                      
055900           PERFORM F-LAES-VISA-INFO                                       
056000         END-IF                                                           
056100       END-IF                                                             
056200       IF (TRANS-TILL-1294 = JA)                                          
056300         MOVE MID-W1I22201      TO MID-W1I29401                           
056400         MOVE MSG-SIGNON-USERID TO R1294-MID-IDUSER                       
056500         PERFORM IMS-INSERT-ALT-MSG                                       
056600       ELSE                                                               
056700         MOVE LENGTH OF MOD-W1O22201 TO MSG-KVLL                          
056800         ADD          +4             TO MSG-KVLL                          
056900         PERFORM IMS-INSERT-MSG                                           
057000       END-IF                                                             
057100     END-IF                                                               
057200                                                                          
057300     MOVE ZERO TO RETURN-CODE                                             
057400     GOBACK                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 A-INIT SECTION.                                                          
057800                                                                          
057900     IF MSG-DUBBLA-TRANSKODER                                             
058000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I22201                 
058100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
058200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
058300     ELSE                                                                 
058400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I22201                  
058500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
058600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
058700     END-IF                                                               
058800                                                                          
058900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
059000     MOVE MSG-IDPFK     TO MFS-IDPFK                                      
059100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
059200                                                                          
059300     MOVE LOW-VALUE  TO MSG-AREA                                          
059400     MOVE 'W1O222N1' TO MFS-IDMOD                                         
059500     MOVE '1222'     TO MOD-IDTRANS                                       
059600     MOVE SPACE      TO MOD-TEMFSFEL                                      
059700                        MOD-TEMFSINF                                      
059800                                                                          
059900     IF NOT EGEN-MID                                                      
060000       MOVE SPACE TO MFS-KDTRTYP                                          
060100       MOVE '7'   TO MFS-IDPFK                                            
060200     END-IF                                                               
060300                                                                          
060400     IF ENGLISH-TEXT                                                      
060500       MOVE +2    TO SPRAK-IX                                             
060600                     M-SW-KDMFSTYP                                        
060700       MOVE 'GB ' TO MED-IDSKYLT                                          
060800     ELSE                                                                 
060900       MOVE +1    TO SPRAK-IX                                             
061000                     M-SW-KDMFSTYP                                        
061100       MOVE 'S  ' TO MED-IDSKYLT                                          
061200     END-IF                                                               
061300                                                                          
061400     ACCEPT DAGENS-DATUM FROM DATE                                        
061500                                                                          
061600     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
061700     MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                                 
061800     CALL WDATKONV USING DAT-KDDATFORM                                    
061900                         DAT-I-TIDATUM                                    
062000                         DAT-O-TIDATUM                                    
062100                         DAT-KDSVAR                                       
062200     MOVE DAT-TIAAVV-GRP TO DAGENS-DATUM-AAVV                             
062300     .                                                                    
062400     EJECT                                                                
062500 B-KOLLA-NYCKLAR SECTION.                                                 
062600                                                                          
062700     MOVE JA TO NYCKLAR-SW                                                
062800                                                                          
062900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
063000                             MOD-BELEVART-IN                              
063100                                                                          
063200***** DESSA TRE NYCKLAR ANVÄNDS ENDAST FÖR ATT VÄRDEN                     
063300***** FRÅN BILD 1221 SKALL LIGGA KVAR VID ÅTERHOPP.                       
063400***** ANVÄNDS EJ I DENNA 'BILD'                                           
063500                             MOD-IDSKYLT-IN-DOLT                          
063600                             MOD-1002-SATS-IN-DOLT                        
063700                             MOD-KDPRODSL-IN-DOLT                         
063800                                                                          
063900     IF MID-IDLEVNR-IN = ALL '+'                                          
064000       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
064100     ELSE                                                                 
064200       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
064300       MOVE '7'         TO MFS-IDPFK                                      
064400       MOVE SPACE       TO MFS-KDTRTYP                                    
064500     END-IF                                                               
064600                                                                          
064700     IF MID-BELEVART-IN = ALL '+'                                         
064800****** KAN ANTINGEN VARA EN ARTIKELBENÄMNING                              
064900****** ELLER ETT ARTIKELNUMMER                                            
065000       MOVE MID-BELEVART-UT TO WS-BELEVART                                
065100       MOVE MID-IDARTNR-UT  TO WS-IDARTNR                                 
065200       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
065300     ELSE                                                                 
065400       MOVE MID-BELEVART-IN TO WS-BELEVART                                
065500       MOVE MID-IDARTNR-IN  TO WS-IDARTNR                                 
065600       MOVE '7'         TO MFS-IDPFK                                      
065700       MOVE SPACE       TO MFS-KDTRTYP                                    
065800     END-IF                                                               
065900                                                                          
066000     IF MID-IDSKYLT-IN-DOLT = ALL '+'                                     
066100       IF MID-IDSKYLT-UT-DOLT = SPACE                                     
066200         MOVE 'S  ' TO WS-IDSKYLT                                         
066300       ELSE                                                               
066400         MOVE MID-IDSKYLT-UT-DOLT TO WS-IDSKYLT                           
066500       END-IF                                                             
066600     ELSE                                                                 
066700       MOVE MID-IDSKYLT-IN-DOLT TO WS-IDSKYLT                             
066800       MOVE '7'         TO MFS-IDPFK                                      
066900       MOVE SPACE       TO MFS-KDTRTYP                                    
067000     END-IF                                                               
067100                                                                          
067200     IF MID-1002-SATS-IN-DOLT = ALL '+'                                   
067300       MOVE MID-1002-SATS-UT-DOLT TO WS-1002-SATS                         
067400     ELSE                                                                 
067500       MOVE MID-1002-SATS-IN-DOLT TO WS-1002-SATS                         
067600       MOVE '7'         TO MFS-IDPFK                                      
067700       MOVE SPACE       TO MFS-KDTRTYP                                    
067800     END-IF                                                               
067900                                                                          
068000     IF MID-KDPRODSL-IN-DOLT = ALL '+'                                    
068100       MOVE MID-KDPRODSL-UT-DOLT TO WS-KDPRODSL                           
068200       INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO                
068300     ELSE                                                                 
068400       MOVE MID-KDPRODSL-IN-DOLT TO WS-KDPRODSL                           
068500       MOVE '7'         TO MFS-IDPFK                                      
068600       MOVE SPACE       TO MFS-KDTRTYP                                    
068700     END-IF                                                               
068800                                                                          
068900     IF WS-IDLEVNR(1:1) NOT = '0' AND '+'                                 
069000       IF WS-IDLEVNR NOT = SPACE                                          
069100****   OM LEVNR > ' ', ÄR 'SÖKNYCKEL' IDLEVNR TILLS. MED BELEVART         
069200         MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                      
069300         MOVE WS-IDLEVNR         TO W-IDLEVNR                             
069400         MOVE WS-BELEVART        TO W-BELEVART                            
069500         MOVE ZERO               TO W-IDARTNR                             
069600                                                                          
069700         MOVE ZERO               TO WS-IDARTNR                            
069800       ELSE                                                               
069900         IF WS-IDLEVNR = SPACE                                            
070000******   OM IDLEVNR = BLANK, ÄR SÖKNYCKEL IDARTNR                         
070100           MOVE IDARTNR TO SOEKNYCKELTYP-SW                               
070200           IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                    
070300             MOVE WS-IDARTNR TO W-IDARTNR                                 
070400             MOVE SPACE      TO WS-IDLEVNR                                
070500                                W-IDLEVNR                                 
070600                                WS-BELEVART                               
070700                                W-BELEVART                                
070800           ELSE                                                           
070900             MOVE NEJ TO NYCKLAR-SW                                       
071000           END-IF                                                         
071100         ELSE                                                             
071200           MOVE NEJ TO NYCKLAR-SW                                         
071300         END-IF                                                           
071400       END-IF                                                             
071500     ELSE                                                                 
071600       MOVE NEJ                TO NYCKLAR-SW                              
071700       MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                        
071800     END-IF                                                               
071900                                                                          
072000     IF GODK-MID OR NYCKLAR-OK                                            
072100       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
072200       IF SOEKNYCKEL-IDARTNR                                              
072300         MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                
072400                            MOD-IDARTNR-SPAR                              
072500         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
072600       INSPECT MOD-IDARTNR-SPAR REPLACING LEADING ZERO BY SPACE           
072700       ELSE                                                               
072800         MOVE WS-BELEVART TO MOD-BELEVART-UT                              
072900       END-IF                                                             
073000                                                                          
073100       MOVE WS-IDSKYLT   TO MOD-IDSKYLT-UT-DOLT                           
073200       MOVE WS-1002-SATS TO MOD-1002-SATS-UT-DOLT                         
073300       MOVE WS-KDPRODSL  TO MOD-KDPRODSL-UT-DOLT                          
073400                                                                          
073500       PERFORM BA-KOLLA-SOEKNYCKEL-IDUSER                                 
073600                                                                          
073700       IF SOEKNYCKEL-LAAST-AV-EGET-ID                                     
073800         IF SOEKNYCKEL-IDARTNR                                            
073900           PERFORM BB-KOLLA-IDARTNR-TYP                                   
074000           PERFORM BC-KOLLA-OM-VALD-STRUKT-1002                           
074100         END-IF                                                           
074200       END-IF                                                             
074300                                                                          
074400     ELSE                                                                 
074500       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                             
074600                               MOD-BELEVART-UT                            
074700                               MOD-IDARTNR-SPAR                           
074800     END-IF                                                               
074900                                                                          
075000     IF NYCKLAR-FEL                                                       
075100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
075200       CALL WMEDKONV USING MED-WMEDAREA                                   
075300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
075400       PERFORM MFS-RENSA-FAELT-IN                                         
075500       PERFORM MFS-RENSA-FAELT-UT                                         
075600       PERFORM MFS-STAENG-FAELT-IN                                        
075700     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000 BA-KOLLA-SOEKNYCKEL-IDUSER SECTION.                                      
076100******************************************************                    
076200* KONTROLL ATT MSG-SIGNON-USERID HAR LÅST SÖKNYCKELN *                    
076300* (ARTIKELN). (FRÅN BILD 1221)                       *                    
076400******************************************************                    
076500                                                                          
076600     MOVE NEJ TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                           
076700                                                                          
076800     MOVE '1151'    TO W-IDHTYP                                           
076900     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
077000     PERFORM IMS-GET-XXAZ-XXAZ01                                          
077100                                                                          
077200     MOVE LOW-VALUE         TO W-LOW-VALUE                                
077300     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
077400     PERFORM IMS-GET-XXAZ-XXAZ11                                          
077500     IF SEGMENT-FINNS                                                     
077600       MOVE 001                  TO WORK-KDCALL                           
077700       MOVE WC-CDC-SE            TO WORK-IDDC                             
077800       MOVE XXAZ11-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                     
077900       MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                     
078000       CALL WORKDAY USING WORK-KDCALL                                     
078100                          WORK-DATE-AREA                                  
078200                          WORK-KDSVAR                                     
078300       IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
078400*******  OM LÅSNINGEN ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                   
078500*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
078600         PERFORM IMS-DLET-XXAZ11                                          
078700         MOVE NEJ TO NYCKLAR-SW                                           
078800       ELSE                                                               
078900         MOVE JA TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                        
079000       END-IF                                                             
079100     ELSE                                                                 
079200       MOVE NEJ TO NYCKLAR-SW                                             
079300     END-IF                                                               
079400                                                                          
079500     .                                                                    
079600     EJECT                                                                
079700 BB-KOLLA-IDARTNR-TYP SECTION.                                            
079800*******************************************************                   
079900* KOLL OM INMATAT IDARTNR (NYCKEL) FINNS PÅ ARTREG ,  *                   
080000* OM DET ÄR EN FÖRPACKNING OCH OM ALTERNATIVT ERSATT  *                   
080100*******************************************************                   
080200                                                                          
080300     MOVE NEJ TO IDARTNR-FORPACKNING-SW                                   
080400                 IDARTNR-FINNS-PAA-ARTREG-SW                              
080500                 IDARTNR-ALT-ERS-PAA-ARTREG-SW                            
080600                                                                          
080700     PERFORM IMS-GET-ARTC-ARTC01                                          
080800     IF SEGMENT-FINNS                                                     
080900       MOVE ART-KDPRODSL          TO TEST-KDPRODSL                        
081000       IF KDPRODSL-VOLVO-EMB                                              
081100         MOVE JA TO IDARTNR-FORPACKNING-SW                                
081200       END-IF                                                             
081300       MOVE JA TO IDARTNR-FINNS-PAA-ARTREG-SW                             
081400       PERFORM IMS-GET-ARTC-ARTC11                                        
081500       IF SEGMENT-FINNS                                                   
081600         IF CLAG-KDERS = 04 OR 05 OR 06                                   
081700********   ARTIKELNR ALTERNATIVT ERSATT                                   
081800           MOVE JA TO IDARTNR-ALT-ERS-PAA-ARTREG-SW                       
081900         END-IF                                                           
082000       END-IF                                                             
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 BC-KOLLA-OM-VALD-STRUKT-1002 SECTION.                                    
082500**********************************************************                
082600* KOLL OM NÅGON AV DE VALDA STRUKTURERNA ÄR EN 1002-SATS *                
082700**********************************************************                
082800                                                                          
082900     MOVE NEJ TO            EN-AV-VALD-STRUKT-1002-SATS-SW                
083000     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
083100                                                                          
083200     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
083300     PERFORM UNTIL (SEGMENT-SAKNAS) OR                                    
083400                    (EN-AV-VALDA-STRUKT-1002-SATS)                        
083500       IF SEGMENT-FINNS                                                   
083600         MOVE 001                 TO WORK-KDCALL                          
083700         MOVE WC-CDC-SE           TO WORK-IDDC                            
083800         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
083900         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
084000         CALL WORKDAY USING WORK-KDCALL                                   
084100                            WORK-DATE-AREA                                
084200                            WORK-KDSVAR                                   
084300         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
084400*********  OM MAN TRÄFFAR PÅ EN KONV. STRUKTUR                            
084500*********  ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                                 
084600*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
084700           MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
084800           PERFORM IMS-GET-SATB-SATB01                                    
084900           PERFORM IMS-DLET-SATB                                          
085000                                                                          
085100           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
085200         ELSE                                                             
085300           IF SATB01-STR-IDLEVNR = '1002 '                                
085400             MOVE JA TO EN-AV-VALD-STRUKT-1002-SATS-SW                    
085500           ELSE                                                           
085600             PERFORM IMS-GET-SATB-DSEQ-NEXT                               
085700           END-IF                                                         
085800         END-IF                                                           
085900       END-IF                                                             
086000     END-PERFORM                                                          
086100     .                                                                    
086200     EJECT                                                                
086300 C-FOERSTA-SIDA SECTION.                                                  
086400                                                                          
086500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
086600     CALL WMEDKONV USING MED-WMEDAREA                                     
086700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
086800                                                                          
086900*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
087000     MOVE ZERO TO WS-ANTAL                                                
087100                  MOD-ANTAL-SEGMENT-ENTER                                 
087200                  MOD-ANTAL-SEGMENT-NEXT                                  
087300     MOVE JA   TO ALLT-SW                                                 
087400     .                                                                    
087500     EJECT                                                                
087600 D-NAESTA-SIDA SECTION.                                                   
087700                                                                          
087800     IF MID-ANTAL-SEGMENT-NEXT > 0                                        
087900       MOVE MID-ANTAL-SEGMENT-NEXT TO WS-ANTAL                            
088000     ELSE                                                                 
088100       MOVE MID-ANTAL-SEGMENT-ENTER TO WS-ANTAL                           
088200       ADD 2                        TO WS-ANTAL                           
088300     END-IF                                                               
088400                                                                          
088500     MOVE JA TO ALLT-SW                                                   
088600     .                                                                    
088700     EJECT                                                                
088800 E-SAMMA-SIDA SECTION.                                                    
088900                                                                          
089000     PERFORM S05-KOLLA-OM-MID-RADER-IFYLLDA                               
089100     PERFORM S10-KOLLA-OM-SVARSFLAG-IFYLLDA                               
089200     PERFORM S11-KOLLA-OM-IDAO-TIAA-AENDRAD                               
089300                                                                          
089400     IF (MID-RAD-IFYLLD) OR                                               
089500         (SVARSFLAGGA-IFYLLD) OR                                          
089600          (IDAO-OCH-TIAAVV-AENDRAD)                                       
089700       MOVE NEJ            TO ALLT-SW                                     
089800       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
089900       CALL WMEDKONV USING MED-WMEDAREA                                   
090000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
090100       PERFORM MFS-ROR-EJ-FAELT-UT                                        
090200       IF (SVARSFLAGGA-IFYLLD) AND                                        
090300            (NOT IDAO-OCH-TIAAVV-AENDRAD) AND                             
090400             (NOT MID-RAD-IFYLLD)                                         
090500******** OM BARA SVARSFLAGGOR IFYLLDA                                     
090600         MOVE JA TO ALLT-SW                                               
090700       ELSE                                                               
090800         PERFORM S13-RENSA-ELLER-LAES-RAD-IGEN                            
090900       END-IF                                                             
091000     ELSE                                                                 
091100       MOVE MID-ANTAL-SEGMENT-ENTER TO WS-ANTAL                           
091200       MOVE JA                      TO ALLT-SW                            
091300     END-IF                                                               
091400     .                                                                    
091500     SKIP2                                                                
091600 F-LAES-VISA-INFO SECTION.                                                
091700                                                                          
091800     MOVE WS-IDLEVNR TO W-IDLEVNR                                         
091900     MOVE WS-BELEVART TO W-BELEVART                                       
092000     MOVE WS-IDARTNR TO W-IDARTNR                                         
092100                                                                          
092200     PERFORM FA-LAES-GRUNDDATA                                            
092300                                                                          
092400     MOVE '1151'    TO W-IDHTYP                                           
092500     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
092600     PERFORM IMS-GET-XXAZ-XXAZ01                                          
092700                                                                          
092800     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
092900     MOVE LOW-VALUE         TO W-LOW-VALUE                                
093000     PERFORM IMS-GET-XXAZ-XXAZ11                                          
093100                                                                          
093200     IF XXAZ11-1152-IDAO = SPACE                                          
093300       MOVE MFS-RENSA-FAELT TO MOD-IDAO                                   
093400     ELSE                                                                 
093500       MOVE XXAZ11-1152-IDAO TO MOD-IDAO                                  
093600     END-IF                                                               
093700                                                                          
093800     IF XXAZ11-1152-TISTODAT = ZERO                                       
093900       MOVE MFS-RENSA-FAELT TO MOD-TIAAVV                                 
094000     ELSE                                                                 
094100       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
094200       MOVE XXAZ11-1152-TISTODAT TO DAT-I-TIDATUM                         
094300       CALL WDATKONV USING DAT-KDDATFORM                                  
094400                           DAT-I-TIDATUM                                  
094500                           DAT-O-TIDATUM                                  
094600                           DAT-KDSVAR                                     
094700       MOVE DAT-TIAAVV-GRP TO MOD-TIAAVV                                  
094800     END-IF                                                               
094900                                                                          
095000     IF WS-ANTAL > 0                                                      
095100****** LÄS FRAM TILL SEGMENTET FÖRE DET SOM SKALL VISAS                   
095200       MOVE +2 TO INDX                                                    
095300       PERFORM IMS-GET-XXAZ-XXAZ21                                        
095400       PERFORM UNTIL INDX > WS-ANTAL                                      
095500         IF SEGMENT-FINNS                                                 
095600           PERFORM IMS-GET-XXAZ-XXAZ21                                    
095700         END-IF                                                           
095800         ADD 1 TO INDX                                                    
095900       END-PERFORM                                                        
096000     END-IF                                                               
096100                                                                          
096200     MOVE +1 TO INDX                                                      
096300     MOVE WS-ANTAL TO MOD-ANTAL-SEGMENT-ENTER                             
096400                                                                          
096500     PERFORM IMS-GET-XXAZ-XXAZ21                                          
096600                                                                          
096700     PERFORM UNTIL INDX > MAX-INDX                                        
096800       IF SEGMENT-FINNS                                                   
096900         PERFORM FB-REDIGERA-RAD-UT                                       
097000                                                                          
097100         MOVE WS-IDLEVNR  TO W-IDLEVNR                                    
097200         MOVE WS-BELEVART TO W-BELEVART                                   
097300         MOVE WS-IDARTNR  TO W-IDARTNR                                    
097400         PERFORM IMS-GET-XXAZ-XXAZ21                                      
097500       ELSE                                                               
097600         PERFORM MFS-RENSA-RAD-FAELT-IN                                   
097700         PERFORM MFS-FORMATETS-ATTR-RAD                                   
097800       END-IF                                                             
097900       ADD 1 TO INDX                                                      
098000     END-PERFORM                                                          
098100                                                                          
098200     IF SEGMENT-FINNS                                                     
098300       ADD 2 TO WS-ANTAL                                                  
098400       MOVE WS-ANTAL             TO MOD-ANTAL-SEGMENT-NEXT                
098500       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
098600       CALL WMEDKONV USING MED-WMEDAREA                                   
098700       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
098800     ELSE                                                                 
098900       MOVE ZERO TO MOD-ANTAL-SEGMENT-NEXT                                
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300 FA-LAES-GRUNDDATA SECTION.                                               
099400*****************************************************                     
099500* HÄR LÄGGS BENÄMNINGEN PÅ DEN UTGÅENDE ARTIKELN UT *                     
099600*****************************************************                     
099700                                                                          
099800     IF IDARTNR-FINNS-PAA-ARTREG                                          
099900       PERFORM IMS-GET-BENA-BENA01-BSEQ                                   
100000       IF SEGMENT-FINNS                                                   
100100         MOVE WS-IDSKYLT TO W-IDSKYLT                                     
100200         PERFORM IMS-GET-BENA-BENA11-BSEQ                                 
100300         IF SEGMENT-FINNS                                                 
100400           MOVE BENA11-TEXT-BEART TO MOD-BEART-UTG-ART                    
100500         ELSE                                                             
100600           MOVE MFS-RENSA-FAELT TO MOD-BEART-UTG-ART                      
100700         END-IF                                                           
100800       ELSE                                                               
100900         MOVE MFS-RENSA-FAELT TO MOD-BEART-UTG-ART                        
101000       END-IF                                                             
101100     ELSE                                                                 
101200       PERFORM IMS-GET-SATB-CSEQ-UNIK                                     
101300       IF WS-IDSKYLT = 'S  '                                              
101400          MOVE SATB11C-RAD-BEART-SVE TO MOD-BEART-UTG-ART                 
101500       ELSE                                                               
101600          MOVE SATB11C-RAD-BEART-SVE TO W-BEART                           
101700          MOVE SATB11C-RAD-KDBENHOM  TO WS-KDBENHOM                       
101800          PERFORM S18-LAS-BEART-GB                                        
101900          MOVE WS-BEART-GB        TO MOD-BEART-UTG-ART                    
102000       END-IF                                                             
102100                                                                          
102200     END-IF                                                               
102300     .                                                                    
102400     EJECT                                                                
102500 FB-REDIGERA-RAD-UT SECTION.                                              
102600                                                                          
102700     IF XXAZ21-1154-IDLEVNR = SPACE                                       
102800       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(INDX)                          
102900       IF XXAZ21-1154-IDARTNR = ZERO                                      
103000         MOVE MFS-RENSA-FAELT       TO MOD-BELEVART(INDX)                 
103100         IF WS-IDSKYLT = 'S  '                                            
103200            MOVE XXAZ21-1154-BEART-SVE TO MOD-BEART(INDX)                 
103300         ELSE                                                             
103400            MOVE XXAZ21-1154-BEART-SVE TO W-BEART                         
103500            MOVE XXAZ21-1154-KDHOM     TO WS-KDBENHOM                     
103600            PERFORM S18-LAS-BEART-GB                                      
103700            MOVE WS-BEART-GB        TO MOD-BEART(INDX)                    
103800         END-IF                                                           
103900         MOVE XXAZ21-1154-KDSORT    TO MOD-KDSORT(INDX)                   
104000       ELSE                                                               
104100         MOVE XXAZ21-1154-IDARTNR TO W-IDARTNR                            
104200                                     WS-IDARTNR-NUM                       
104300         MOVE WS-IDARTNR-NUM      TO MOD-BELEVART(INDX) (1:9)             
104400         INSPECT MOD-BELEVART(INDX) REPLACING                             
104500                                     LEADING ZERO BY SPACE                
104600         PERFORM IMS-GET-ARTC-ARTC01                                      
104700         IF SEGMENT-FINNS                                                 
104800           PERFORM S09-VISA-BEART-KDBENHOM-KDSORT                         
104900         ELSE                                                             
105000           IF WS-IDSKYLT = 'S  '                                          
105100              MOVE XXAZ21-1154-BEART-SVE TO MOD-BEART(INDX)               
105200           ELSE                                                           
105300              MOVE XXAZ21-1154-BEART-SVE TO W-BEART                       
105400              MOVE XXAZ21-1154-KDHOM     TO WS-KDBENHOM                   
105500              PERFORM S18-LAS-BEART-GB                                    
105600              MOVE WS-BEART-GB        TO MOD-BEART(INDX)                  
105700           END-IF                                                         
105800           MOVE XXAZ21-1154-KDHOM     TO MOD-KDBENHOM(INDX)               
105900           MOVE XXAZ21-1154-KDSORT    TO MOD-KDSORT(INDX)                 
106000         END-IF                                                           
106100       END-IF                                                             
106200     ELSE                                                                 
106300       MOVE XXAZ21-1154-IDLEVNR   TO MOD-IDLEVNR(INDX)                    
106400       MOVE XXAZ21-1154-BELEVART  TO MOD-BELEVART(INDX)                   
106500       IF WS-IDSKYLT = 'S  '                                              
106600          MOVE XXAZ21-1154-BEART-SVE TO MOD-BEART(INDX)                   
106700       ELSE                                                               
106800          MOVE XXAZ21-1154-BEART-SVE TO W-BEART                           
106900          MOVE XXAZ21-1154-KDHOM     TO WS-KDBENHOM                       
107000          PERFORM S18-LAS-BEART-GB                                        
107100          MOVE WS-BEART-GB        TO MOD-BEART(INDX)                      
107200       END-IF                                                             
107300       MOVE XXAZ21-1154-KDHOM     TO MOD-KDBENHOM(INDX)                   
107400       MOVE XXAZ21-1154-KDSORT    TO MOD-KDSORT(INDX)                     
107500     END-IF                                                               
107600                                                                          
107700     MOVE XXAZ21-1154-REANTPSA    TO MOD-REANTPSA(INDX)                   
107800     MOVE XXAZ21-1154-IDSTRTYP    TO MOD-IDSTRTYP(INDX)                   
107900     MOVE XXAZ21-1154-TESTRNOT(1) TO MOD-TESTRNOT(INDX, 1)                
108000     MOVE XXAZ21-1154-TESTRNOT(2) TO MOD-TESTRNOT(INDX, 2)                
108100                                                                          
108200     PERFORM MFS-STAENG-RAD-FAELT-IN                                      
108300     .                                                                    
108400     EJECT                                                                
108500 G-KOLLA-INPUT SECTION.                                                   
108600                                                                          
108700     MOVE JA TO INDATA-SW                                                 
108800                                                                          
108900     IF (IDARTNR-ALT-ERSATT-PAA-ARTREG) AND                               
109000        (EN-AV-VALDA-STRUKT-1002-SATS)                                    
109100*****  OM ARTIKLE ALT ERSATT PÅ ARTIKELREGISTRET                          
109200*****  OCH DE VALDA STRUKTURERNA ÄR 1002-SATSER                           
109300       PERFORM S16-KOLLA-ATT-RADER-E-MAERKTA                              
109400     END-IF                                                               
109500                                                                          
109600     IF INDATA-OK                                                         
109700       PERFORM S05-KOLLA-OM-MID-RADER-IFYLLDA                             
109800       PERFORM S10-KOLLA-OM-SVARSFLAG-IFYLLDA                             
109900       PERFORM S11-KOLLA-OM-IDAO-TIAA-AENDRAD                             
110000                                                                          
110100       IF (MID-RAD-IFYLLD) OR                                             
110200           (SVARSFLAGGA-IFYLLD) OR                                        
110300            (IDAO-OCH-TIAAVV-AENDRAD)                                     
110400         MOVE NEJ TO ALLT-SW                                              
110500                                                                          
110600         PERFORM GA-KOLLA-UPPDAT-TYP                                      
110700         IF INDATA-OK                                                     
110800           IF UPDATE-BYTE-ARTIKLAR                                        
110900             PERFORM GB-KOLLA-BYTE-INPUT                                  
111000           ELSE                                                           
111100             IF UPDATE-BORTTAG-ARTIKLAR                                   
111200               PERFORM GC-KOLLA-BORTTAG-INPUT                             
111300             ELSE                                                         
111400               IF UPDATE-TILLKOMMANDE-ARTIKLAR                            
111500                 IF MID-RAD-IFYLLD                                        
111600                   PERFORM S03-KOLLA-TILLK-ARTIKLAR-INPUT                 
111700                 ELSE                                                     
111800                   MOVE NEJ                TO INDATA-SW                   
111900                   MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL              
112000                   CALL WMEDKONV USING MED-WMEDAREA                       
112100                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
112200                 END-IF                                                   
112300               ELSE                                                       
112400                 IF UPDATE-AANGRA-ARTIKLAR                                
112500                   PERFORM GD-KOLLA-AANGRA-INPUT                          
112600                 ELSE                                                     
112700***************    ENDAST IDAO OCH/ELLER TIAAVV ÄNDRAD                    
112800                   PERFORM S01-KOLLA-IDAO-O-TIAAVV-EJ-OBL                 
112900                 END-IF                                                   
113000               END-IF                                                     
113100             END-IF                                                       
113200           END-IF                                                         
113300         END-IF                                                           
113400                                                                          
113500         IF INDATA-FEL                                                    
113600           IF MOD-TEMFSFEL = SPACE                                        
113700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
113800             CALL WMEDKONV USING MED-WMEDAREA                             
113900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
114000           END-IF                                                         
114100             PERFORM MFS-ROR-EJ-FAELT-UT                                  
114200         END-IF                                                           
114300                                                                          
114400       ELSE                                                               
114500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
114600         CALL WMEDKONV USING MED-WMEDAREA                                 
114700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
114800         MOVE NEJ TO INDATA-SW                                            
114900         MOVE JA TO ALLT-SW                                               
115000       END-IF                                                             
115100     ELSE                                                                 
115200       MOVE NEJ  TO ALLT-SW                                               
115300       MOVE FEL4(SPRAK-IX) TO MOD-TEMFSFEL                                
115400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
115500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
115600       PERFORM S17-LYS-UPP-INMATADE-FAELT                                 
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 GA-KOLLA-UPPDAT-TYP SECTION.                                             
116100**************************************                                    
116200* KOLLAR TYP AV UPPDATERING VID PF11 *                                    
116300**************************************                                    
116400                                                                          
116500     IF (IDAO-OCH-TIAAVV-AENDRAD) AND                                     
116600         (NOT MID-RAD-IFYLLD) AND                                         
116700          (NOT SVARSFLAGGA-IFYLLD)                                        
116800*****  BARA IDAO OCH/ELLER TIAAVV ÄNDRAD                                  
116900       MOVE ENDAST-AENDRING-IDAO-TIAAVV TO UPDATE-TYP-SW                  
117000     ELSE                                                                 
117100       IF NOT SVARSFLAGGA-IFYLLD                                          
117200         MOVE TILLKOMMANDE         TO UPDATE-TYP-SW                       
117300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-AANGRA-ATTR                     
117400                                      MOD-BORTTAG-ATTR                    
117500                                      MOD-KLAR-ATTR                       
117600       ELSE                                                               
117700         IF MID-AANGRA = ALL '+' OR 'N' OR SPACE                          
117800           IF MID-KLAR = ALL '+' OR SPACE                                 
117900             MOVE NEJ                TO INDATA-SW                         
118000             MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-ATTR                     
118100           ELSE                                                           
118200             IF MID-KLAR = 'J' OR 'Y'                                     
118300               MOVE BYTE                 TO UPDATE-TYP-SW                 
118400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KLAR-ATTR                 
118500             ELSE                                                         
118600               MOVE NEJ TO INDATA-SW                                      
118700               MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-ATTR                   
118800             END-IF                                                       
118900           END-IF                                                         
119000                                                                          
119100           IF MID-BORTTAG = ALL '+' OR 'N' OR SPACE                       
119200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORTTAG-ATTR                
119300           ELSE                                                           
119400             IF MID-BORTTAG = 'J' OR 'Y'                                  
119500               MOVE BORTTAG              TO UPDATE-TYP-SW                 
119600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORTTAG-ATTR              
119700             ELSE                                                         
119800               MOVE NEJ TO INDATA-SW                                      
119900               MOVE MFS-ALFA-FAELT-FEL TO MOD-BORTTAG-ATTR                
120000             END-IF                                                       
120100           END-IF                                                         
120200                                                                          
120300         ELSE                                                             
120400           IF MID-AANGRA = 'J' OR 'Y'                                     
120500             MOVE AANGRA               TO UPDATE-TYP-SW                   
120600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-AANGRA-ATTR                 
120700                                                                          
120800**********   KONTROLL ATT INTE BORTTAG ELLER KLAR VALD                    
120900             IF MID-BORTTAG = ALL '+' OR 'N' OR SPACE                     
121000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORTTAG-ATTR              
121100             ELSE                                                         
121200               MOVE NEJ                TO INDATA-SW                       
121300               MOVE MFS-ALFA-FAELT-FEL TO MOD-BORTTAG-ATTR                
121400             END-IF                                                       
121500                                                                          
121600             IF MID-KLAR = ALL '+' OR 'N' OR SPACE                        
121700               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KLAR-ATTR                 
121800             ELSE                                                         
121900               MOVE NEJ                TO INDATA-SW                       
122000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-ATTR                   
122100             END-IF                                                       
122200           ELSE                                                           
122300             MOVE NEJ                TO INDATA-SW                         
122400             MOVE MFS-ALFA-FAELT-FEL TO MOD-AANGRA-ATTR                   
122500           END-IF                                                         
122600         END-IF                                                           
122700       END-IF                                                             
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 GB-KOLLA-BYTE-INPUT SECTION.                                             
123200****************************************************                      
123300* OM MID-RADER IFYLLDA GÖRS INDATA-KONTROLL PÅ DEM,*                      
123400* ANNARS KOLLAS ATT MINST EN TILLKOMMANDE ARTIKEL  *                      
123500* FINNS REGISTRERAD PÅ WDR5 (WLXXAZ).              *                      
123600****************************************************                      
123700                                                                          
123800     IF MID-RAD-IFYLLD                                                    
123900       PERFORM S03-KOLLA-TILLK-ARTIKLAR-INPUT                             
124000     ELSE                                                                 
124100       PERFORM GBA-KOLLA-ATT-TILLK-ART-FINNS                              
124200     END-IF                                                               
124300     .                                                                    
124400     SKIP3                                                                
124500 GBA-KOLLA-ATT-TILLK-ART-FINNS SECTION.                                   
124600                                                                          
124700     MOVE '1151'    TO W-IDHTYP                                           
124800     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
124900     PERFORM IMS-GET-XXAZ-XXAZ01                                          
125000                                                                          
125100     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
125200     MOVE WS-BELEVART       TO W-BELEVART                                 
125300     MOVE WS-IDARTNR        TO W-IDARTNR                                  
125400     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
125500     MOVE LOW-VALUE         TO W-LOW-VALUE                                
125600     PERFORM IMS-GET-XXAZ-XXAZ11                                          
125700                                                                          
125800     IF IDAO-OCH-TIAAVV-AENDRAD                                           
125900       PERFORM S08-KOLLA-IDAO-O-TIAAVV-OBL                                
126000     END-IF                                                               
126100                                                                          
126200     IF INDATA-OK                                                         
126300       PERFORM IMS-GET-XXAZ-XXAZ21                                        
126400       IF SEGMENT-FINNS                                                   
126500         CONTINUE                                                         
126600       ELSE                                                               
126700         MOVE NEJ              TO INDATA-SW                               
126800         MOVE FEL1(SPRAK-IX)   TO MOD-TEMFSFEL                            
126900         MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-ATTR                         
127000       END-IF                                                             
127100     END-IF                                                               
127200     .                                                                    
127300     EJECT                                                                
127400 GC-KOLLA-BORTTAG-INPUT SECTION.                                          
127500                                                                          
127600     IF IDARTNR-FORPACKNING                                               
127700       MOVE NEJ                TO INDATA-SW                               
127800       MOVE FEL3(SPRAK-IX)     TO MOD-TEMFSFEL                            
127900       MOVE MFS-ALFA-FAELT-FEL TO MOD-BORTTAG-ATTR                        
128000     ELSE                                                                 
128100       PERFORM S08-KOLLA-IDAO-O-TIAAVV-OBL                                
128200                                                                          
128300       IF MID-RAD-IFYLLD                                                  
128400         MOVE NEJ                  TO INDATA-SW                           
128500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
128600         CALL WMEDKONV USING MED-WMEDAREA                                 
128700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
128800         PERFORM S02-LYS-UPP-INMATADE-RADFAELT                            
128900       END-IF                                                             
129000                                                                          
129100       IF INDATA-OK                                                       
129200         MOVE '1' TO W-KDSEGKEY                                           
129300         PERFORM IMS-GET-XXAZ-XXAZ21                                      
129400         IF SEGMENT-FINNS                                                 
129500******     TILLKOMMANDE ARTIKLAR FINNS REGISTRERADE                       
129600           MOVE NEJ                TO INDATA-SW                           
129700           MOVE FEL2(SPRAK-IX)     TO MOD-TEMFSFEL                        
129800           MOVE MFS-ALFA-FAELT-FEL TO MOD-BORTTAG-ATTR                    
129900         END-IF                                                           
130000                                                                          
130100       END-IF                                                             
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500 GD-KOLLA-AANGRA-INPUT SECTION.                                           
130600************************************************                          
130700* KONTROLL ATT INTE ARTIKEL-FÄLTEN ÄR IFYLLDA  *                          
130800************************************************                          
130900                                                                          
131000     IF MID-RAD-IFYLLD                                                    
131100       MOVE NEJ TO INDATA-SW                                              
131200       PERFORM S02-LYS-UPP-INMATADE-RADFAELT                              
131300     END-IF                                                               
131400     .                                                                    
131500     EJECT                                                                
131600 H-UPPDATERA SECTION.                                                     
131700                                                                          
131800     MOVE NEJ TO TRANS-TILL-1294                                          
131900                                                                          
132000     IF UPDATE-BYTE-ARTIKLAR                                              
132100       PERFORM HA-BYT-UT-ARTIKELRAD                                       
132200     ELSE                                                                 
132300       IF UPDATE-BORTTAG-ARTIKLAR                                         
132400         PERFORM HB-TABORT-ARTIKELRAD                                     
132500       ELSE                                                               
132600         IF UPDATE-TILLKOMMANDE-ARTIKLAR                                  
132700           PERFORM S04-LAEGG-UPP-TILLKOMMANDE-ART                         
132800         ELSE                                                             
132900           IF UPDATE-AANGRA-ARTIKLAR                                      
133000             PERFORM HC-AANGRA-TILLKOMMANDE-ART                           
133100           ELSE                                                           
133200**********   ÄNDRA IDAO OCH/ELLER TIAAVV                                  
133300             PERFORM S14-AENDRA-IDAO-O-TIAAVV                             
133400           END-IF                                                         
133500         END-IF                                                           
133600       END-IF                                                             
133700     END-IF                                                               
133800                                                                          
133900     IF TRANS-TILL-1294 = NEJ                                             
134000       IF UPDATE-TILLKOMMANDE-ARTIKLAR                                    
134100         MOVE MED1(SPRAK-IX)  TO MOD-TEMFSINF                             
134200       ELSE                                                               
134300         MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                             
134400         CALL WMEDKONV USING MED-WMEDAREA                                 
134500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
134600       END-IF                                                             
134700       PERFORM MFS-ROR-EJ-FAELT-UT                                        
134800       MOVE MFS-RENSA-FAELT TO MOD-AANGRA                                 
134900                               MOD-BORTTAG                                
135000                               MOD-KLAR                                   
135100       IF UPDATE-AANGRA-ARTIKLAR                                          
135200         MOVE MFS-RENSA-FAELT TO MOD-IDAO                                 
135300                                 MOD-TIAAVV                               
135400       END-IF                                                             
135500       MOVE MFS-ADD-SAETT-CURSOR TO MOD-KLAR-ATTR                         
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900 HA-BYT-UT-ARTIKELRAD SECTION.                                            
136000*********************************************************                 
136100* BYTE AV VALD RAD I DE VALDA STRUKTURERNA MOT          *                 
136200* DE TILLKOMMANDE ARTIKLARNA PÅ WDR5 (WLXXAZ) SAMT      *                 
136300* OMNUMRERING AV RADERNA. DETTA SKÖTS AV PGM W10294     *                 
136400*********************************************************                 
136500                                                                          
136600     IF MID-RAD-IFYLLD                                                    
136700****** OM TILLK. ARTIKLAR IFYLLDA LÄGGS DE UPP                            
136800       PERFORM S04-LAEGG-UPP-TILLKOMMANDE-ART                             
136900     ELSE                                                                 
137000       IF IDAO-OCH-TIAAVV-AENDRAD                                         
137100*******  OM BARA IDAO O TIAAVV ÄNDRAD                                     
137200         PERFORM S14-AENDRA-IDAO-O-TIAAVV                                 
137300       END-IF                                                             
137400     END-IF                                                               
137500                                                                          
137600     MOVE JA TO TRANS-TILL-1294                                           
137700     .                                                                    
137800     EJECT                                                                
137900 HB-TABORT-ARTIKELRAD SECTION.                                            
138000*********************************************************                 
138100* BORTTAG AV DEN VALDA RADEN I DE VALDA STRUKTURERNA    *                 
138200* SAMT OMNUMRERING AV RADERNA. DETTA SKÖTS AV PGM W10294*                 
138300*********************************************************                 
138400                                                                          
138500     MOVE '1151'    TO W-IDHTYP                                           
138600     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
138700     PERFORM IMS-GET-XXAZ-XXAZ01                                          
138800                                                                          
138900     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
139000     MOVE WS-BELEVART       TO W-BELEVART                                 
139100     MOVE WS-IDARTNR        TO W-IDARTNR                                  
139200     MOVE LOW-VALUE         TO W-LOW-VALUE                                
139300     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
139400     PERFORM IMS-GET-XXAZ-XXAZ11                                          
139500     MOVE MID-IDAO    TO XXAZ11-1152-IDAO                                 
139600     MOVE WS-TISTODAT TO XXAZ11-1152-TISTODAT                             
139700     PERFORM IMS-REPL-XXAZ                                                
139800                                                                          
139900     MOVE JA TO TRANS-TILL-1294                                           
140000     .                                                                    
140100     EJECT                                                                
140200 HC-AANGRA-TILLKOMMANDE-ART SECTION.                                      
140300********************************************                              
140400* RENSNING AV ALLA UPPLAGDA (TILLKOMMANDE) *                              
140500* ARTIKLAR PÅ WDR5 (WLXXAZ)                *                              
140600********************************************                              
140700                                                                          
140800     MOVE '1151'    TO W-IDHTYP                                           
140900     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
141000     PERFORM IMS-GET-XXAZ-XXAZ01                                          
141100                                                                          
141200     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
141300     MOVE WS-BELEVART       TO W-BELEVART                                 
141400     MOVE WS-IDARTNR        TO W-IDARTNR                                  
141500     MOVE LOW-VALUE         TO W-LOW-VALUE                                
141600     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
141700     PERFORM IMS-GET-XXAZ-XXAZ11                                          
141800     MOVE SPACE TO XXAZ11-1152-IDAO                                       
141900     MOVE ZERO  TO XXAZ11-1152-TISTODAT                                   
142000     PERFORM IMS-REPL-XXAZ                                                
142100                                                                          
142200     PERFORM IMS-GET-XXAZ-XXAZ21                                          
142300     PERFORM UNTIL SEGMENT-SAKNAS                                         
142400       IF SEGMENT-FINNS                                                   
142500         PERFORM IMS-DLET-XXAZ21                                          
142600         PERFORM IMS-GET-XXAZ-XXAZ21                                      
142700       END-IF                                                             
142800     END-PERFORM                                                          
142900     .                                                                    
143000     EJECT                                                                
143100 S01-KOLLA-IDAO-O-TIAAVV-EJ-OBL SECTION.                                  
143200                                                                          
143300     IF MID-IDAO = ALL '+' OR SPACE                                       
143400       CONTINUE                                                           
143500     ELSE                                                                 
143600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                         
143700       IF MID-TIAAVV = ALL '+'                                            
143800         MOVE NEJ TO INDATA-SW                                            
143900         MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                        
144000       END-IF                                                             
144100     END-IF                                                               
144200                                                                          
144300     IF MID-TIAAVV = ALL '+'                                              
144400       CONTINUE                                                           
144500     ELSE                                                                 
144600       PERFORM S12-KOLLA-TIAAVV                                           
144700       IF MID-IDAO = ALL '+' OR SPACE                                     
144800         MOVE NEJ TO INDATA-SW                                            
144900         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                         
145000       END-IF                                                             
145100     END-IF                                                               
145200     .                                                                    
145300     EJECT                                                                
145400 S02-LYS-UPP-INMATADE-RADFAELT SECTION.                                   
145500                                                                          
145600     MOVE +1 TO INDX                                                      
145700     PERFORM UNTIL INDX > MAX-INDX                                        
145800                                                                          
145900       IF MID-IDLEVNR(INDX) NOT = ALL '+'                                 
146000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(INDX)                
146100       END-IF                                                             
146200                                                                          
146300       IF MID-BELEVART(INDX) = ALL '+' OR                                 
146400          MID-BELEVART(INDX) = SPACE                                      
146500         CONTINUE                                                         
146600       ELSE                                                               
146700         MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)               
146800       END-IF                                                             
146900                                                                          
147000       IF MID-IDARTNR(INDX) = ALL '+' OR                                  
147100          MID-IDARTNR(INDX) = ZERO                                        
147200         CONTINUE                                                         
147300       ELSE                                                               
147400         MOVE MFS-NUM-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)                
147500       END-IF                                                             
147600                                                                          
147700       IF MID-REANTPSA(INDX) NOT = ALL '+'                                
147800         MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)                
147900       END-IF                                                             
148000                                                                          
148100       IF MID-KDSORT(INDX) = ALL '+' OR                                   
148200          MID-KDSORT(INDX) = SPACE                                        
148300         CONTINUE                                                         
148400       ELSE                                                               
148500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)                 
148600       END-IF                                                             
148700                                                                          
148800       IF MID-BEART(INDX) = ALL '+' OR                                    
148900          MID-BEART(INDX) = SPACE                                         
149000         CONTINUE                                                         
149100       ELSE                                                               
149200         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR(INDX)                  
149300       END-IF                                                             
149400                                                                          
149500       IF MID-KDBENHOM(INDX) NOT = ALL '+'                                
149600         MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)                
149700       END-IF                                                             
149800                                                                          
149900       IF MID-IDSTRTYP(INDX) = ALL '+' OR                                 
150000          MID-IDSTRTYP(INDX) = SPACE                                      
150100         CONTINUE                                                         
150200       ELSE                                                               
150300         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-ATTR(INDX)               
150400       END-IF                                                             
150500                                                                          
150600       IF MID-TESTRNOT(INDX, 1) = ALL '+' OR                              
150700          MID-TESTRNOT(INDX, 1) = SPACE                                   
150800         CONTINUE                                                         
150900       ELSE                                                               
151000         MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(INDX, 1)            
151100       END-IF                                                             
151200                                                                          
151300       IF MID-TESTRNOT(INDX, 2) = ALL '+' OR                              
151400          MID-TESTRNOT(INDX, 2) = SPACE                                   
151500         CONTINUE                                                         
151600       ELSE                                                               
151700         MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(INDX, 2)            
151800       END-IF                                                             
151900                                                                          
152000       ADD 1 TO INDX                                                      
152100     END-PERFORM                                                          
152200                                                                          
152300     .                                                                    
152400     EJECT                                                                
152500 S03-KOLLA-TILLK-ARTIKLAR-INPUT SECTION.                                  
152600****************************************************                      
152700* KONTROLL AV TILLKOMMANDE ARTIKEL                 *                      
152800****************************************************                      
152900                                                                          
153000     IF MID-RAD-1-IFYLLD                                                  
153100       MOVE +1 TO INDX                                                    
153200       PERFORM S031-KOLLA-TILLK-ARTIKEL                                   
153300     END-IF                                                               
153400                                                                          
153500     IF INDATA-OK                                                         
153600       IF MID-RAD-2-IFYLLD                                                
153700         MOVE +2 TO INDX                                                  
153800         PERFORM S031-KOLLA-TILLK-ARTIKEL                                 
153900       END-IF                                                             
154000     ELSE                                                                 
154100       MOVE +2 TO INDX                                                    
154200       PERFORM MFS-LAES-IN-RAD-IGEN                                       
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
154600 S031-KOLLA-TILLK-ARTIKEL SECTION.                                        
154700****************************************************                      
154800* TILLKOMMANDE ARTIKEL KAN VARA AV 3 OLIKA SLAG :  *                      
154900*     ¤ IDLEVNR + BELEVART                         *                      
155000*     ¤ ENDAST IDARTNR                             *                      
155100*     ¤ ENDAST BEART                               *                      
155200****************************************************                      
155300                                                                          
155400     IF (IDARTNR-FORPACKNING) OR                                          
155500        (EN-AV-VALDA-STRUKT-1002-SATS)                                    
155600*****  OM ARTIKEL SOM SKALL BYTAS UT (NYCKEL) ÄR EN FÖRPACKNING           
155700*****  MÅSTE TILLKOMMANDE ARTIKEL VARA ETT ARTIKELNR                      
155800*****  OM EN AV DE VALDA STRUKTURERNA ÄR EN 1002-SATS MÅSTE TILL-         
155900*****  KOMMANDE ARTIKEL VARA ETT ARTIKELNR (SOM FINNS PÅ ARTREG)          
156000                                                                          
156100       IF (MID-IDLEVNR(INDX) = ALL '+') OR                                
156200          (MID-IDLEVNR(INDX) = SPACE)                                     
156300         CONTINUE                                                         
156400       ELSE                                                               
156500         MOVE NEJ               TO INDATA-SW                              
156600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(INDX)                
156700       END-IF                                                             
156800                                                                          
156900       PERFORM S0312-KOLLA-INPUT-IDARTNR                                  
157000                                                                          
157100     ELSE                                                                 
157200       IF (MID-IDLEVNR(INDX) = ALL '+') OR                                
157300          (MID-IDLEVNR(INDX) = SPACE)                                     
157400                                                                          
157500         IF (MID-IDARTNR(INDX) = ALL '+') OR                              
157600            (MID-IDARTNR(INDX) = ZERO)                                    
157700                                                                          
157800           IF (MID-BEART(INDX) = ALL '+') OR                              
157900              (MID-BEART(INDX) = SPACE)                                   
158000             MOVE NEJ                TO INDATA-SW                         
158100             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR(INDX)              
158200           ELSE                                                           
158300             PERFORM S0311-KOLLA-INPUT-BEART                              
158400           END-IF                                                         
158500         ELSE                                                             
158600           PERFORM S0312-KOLLA-INPUT-IDARTNR                              
158700         END-IF                                                           
158800       ELSE                                                               
158900         PERFORM S0313-KOLLA-INPUT-IDLEV-BELEVA                           
159000       END-IF                                                             
159100     END-IF                                                               
159200     .                                                                    
159300     EJECT                                                                
159400 S0311-KOLLA-INPUT-BEART SECTION.                                         
159500****************************************************                      
159600* KONTROLL NÄR TILLKOMMANDE ARTIKEL ÄR ENBART      *                      
159700* BEART                                            *                      
159800****************************************************                      
159900                                                                          
160000     PERFORM S08-KOLLA-IDAO-O-TIAAVV-OBL                                  
160100                                                                          
160200     IF MID-REANTPSA(INDX) = ALL '+'                                      
160300       MOVE NEJ               TO INDATA-SW                                
160400       MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)                  
160500     ELSE                                                                 
160600       PERFORM S15-KOLLA-REANTPSA                                         
160700     END-IF                                                               
160800                                                                          
160900     IF MID-KDSORT(INDX) = ALL '+'                                        
161000       MOVE NEJ                TO INDATA-SW                               
161100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)                   
161200     ELSE                                                                 
161300       IF MID-KDSORT(INDX) = 'SA' OR 'TM'                                 
161400******** KDSORT = 'SA' ENDAST TILLÅTEN VID ETT ARTIKELNR                  
161500******** SOM FINNS SOM ROT PÅ RASA MED IDSTRTYP = 'S'                     
161600         MOVE NEJ                TO INDATA-SW                             
161700         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)                 
161800       ELSE                                                               
161900         MOVE MID-KDSORT(INDX) TO WS-KDSORT-GODK                          
162000         IF KDSORT-GODK                                                   
162100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR(INDX)             
162200         ELSE                                                             
162300           MOVE NEJ                TO INDATA-SW                           
162400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)               
162500         END-IF                                                           
162600       END-IF                                                             
162700     END-IF                                                               
162800                                                                          
162900     PERFORM S06-KOLLA-BEART-KDBENHOM                                     
163000                                                                          
163100     IF MID-IDSTRTYP(INDX) = ALL '+' OR MID-IDSTRTYP(INDX) = SPACE        
163200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-ATTR(INDX)               
163300     ELSE                                                                 
163400       MOVE NEJ                TO INDATA-SW                               
163500       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-ATTR(INDX)                 
163600     END-IF                                                               
163700                                                                          
163800     IF MID-TESTRNOT(INDX, 1) = ALL '+' OR SPACE                          
163900       CONTINUE                                                           
164000     ELSE                                                                 
164100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(INDX, 1)            
164200     END-IF                                                               
164300                                                                          
164400     IF MID-TESTRNOT(INDX, 2) = ALL '+' OR SPACE                          
164500       CONTINUE                                                           
164600     ELSE                                                                 
164700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(INDX, 2)            
164800     END-IF                                                               
164900     .                                                                    
165000     EJECT                                                                
165100 S0312-KOLLA-INPUT-IDARTNR SECTION.                                       
165200****************************************************                      
165300* KONTROLL NÄR TILLKOMMANDE ARTIKEL ÄR ENBART      *                      
165400* IDARTNR                                          *                      
165500****************************************************                      
165600                                                                          
165700     INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
165800                                                                          
165900     IF (MID-IDARTNR(INDX) NUMERIC) AND                                   
166000        (MID-IDARTNR(INDX) > 0 AND < WS-MIN-IDARTNR-KONV)                 
166100                                                                          
166200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEVART-ATTR(INDX)               
166300       MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-NUM                           
166400       MOVE WS-IDARTNR-NUM    TO W-IDARTNR                                
166500                                                                          
166600       PERFORM IMS-GET-SATB-SATB01                                        
166700       IF SEGMENT-FINNS                                                   
166800         IF SATB01-STR-TIBORT > 0                                         
166900           MOVE NEJ                TO INDATA-SW                           
167000           MOVE MED7(SPRAK-IX)     TO MOD-TEMFSINF                        
167100           MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)             
167200         END-IF                                                           
167300       ELSE                                                               
167400         PERFORM IMS-GET-ARTC-ARTC01                                      
167500         IF SEGMENT-FINNS                                                 
167600           IF ART-KDERS-UTG > 0                                           
167700             MOVE NEJ                TO INDATA-SW                         
167800             MOVE MED6(SPRAK-IX)     TO MOD-TEMFSINF                      
167900             MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)           
168000           ELSE                                                           
168100             PERFORM IMS-GET-ARTC-ARTC11                                  
168200             IF SEGMENT-FINNS                                             
168300               IF CLAG-KDERS > 0                                          
168400                 MOVE NEJ                TO INDATA-SW                     
168500                 MOVE MED8(SPRAK-IX)     TO MOD-TEMFSINF                  
168600                 MOVE MFS-ALFA-FAELT-FEL TO                               
168700                                         MOD-BELEVART-ATTR(INDX)          
168800               END-IF                                                     
168900             END-IF                                                       
169000           END-IF                                                         
169100         END-IF                                                           
169200       END-IF                                                             
169300                                                                          
169400       IF INDATA-OK                                                       
169500         PERFORM S03121-KOLLA-TILLK-EJ-UTVALD                             
169600       END-IF                                                             
169700                                                                          
169800       IF INDATA-OK                                                       
169900                                                                          
170000         IF (IDARTNR-ALT-ERSATT-PAA-ARTREG) AND                           
170100             (EN-AV-VALDA-STRUKT-1002-SATS)                               
170200*******    ARTIKELNR SOM SKALL BYTAS UT (NYCKEL) ALERNATIVT ERSATT        
170300*******    OCH EN AV VALDA STRUKTURER 1002-SATS (=ALLA ÄR 1002)           
170400*******    INMATAT ARTIKELNR MÅSTE VARA EN AV DE TILLKOMMANDE             
170500                                                                          
170600*******    FLYTTA NYCKEL                                                  
170700           MOVE WS-IDARTNR TO W-IDARTNR                                   
170800                                                                          
170900           MOVE NEJ TO ERSATTNINGS-SW                                     
171000           PERFORM IMS-GET-ERSA-ERSA01                                    
171100           PERFORM IMS-GET-ERSA-ERSA11                                    
171200           PERFORM UNTIL SEGMENT-SAKNAS OR FINNS-SOM-ERSATTNING           
171300             IF ERSA11-FLTEXT = JA                                        
171400               CONTINUE                                                   
171500             ELSE                                                         
171600               IF ERSA11-IDARTNR-TILLK = WS-IDARTNR-NUM                   
171700                 MOVE JA TO ERSATTNINGS-SW                                
171800               END-IF                                                     
171900             END-IF                                                       
172000             PERFORM IMS-GET-ERSA-ERSA11                                  
172100           END-PERFORM                                                    
172200                                                                          
172300           IF FINNS-SOM-ERSATTNING                                        
172400             CONTINUE                                                     
172500           ELSE                                                           
172600             MOVE NEJ                TO INDATA-SW                         
172700             MOVE MED3(SPRAK-IX)     TO MOD-TEMFSINF                      
172800             MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)           
172900           END-IF                                                         
173000         END-IF                                                           
173100                                                                          
173200         MOVE WS-IDARTNR-NUM TO W-IDARTNR                                 
173300                                                                          
173400         IF EN-AV-VALDA-STRUKT-1002-SATS                                  
173500*******    ARTIKELNR SOM SKALL BYTAS UT (NYCKEL) INGÅR I EN 1002-         
173600*******    SATS TILLKOMMANDE ARTIKELNR MÅSTE FINNAS PÅ ARTREG             
173700           PERFORM IMS-GET-ARTC-ARTC01                                    
173800           IF SEGMENT-FINNS                                               
173900             CONTINUE                                                     
174000           ELSE                                                           
174100             MOVE NEJ                TO INDATA-SW                         
174200             MOVE MED2(SPRAK-IX)     TO MOD-TEMFSINF                      
174300             MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)           
174400           END-IF                                                         
174500         END-IF                                                           
174600                                                                          
174700         IF IDARTNR-FORPACKNING                                           
174800*******    ARTIKELNR SOM SKALL BYTAS UT (NYCKEL) ÄR EN FÖRPACKNING        
174900*******    INMATAT ARTIKELNR MÅSTE FINNAS PÅ ARTREG MED PRODSL 19         
175000           PERFORM IMS-GET-ARTC-ARTC01                                    
175100           IF SEGMENT-FINNS                                               
175200             MOVE ART-KDPRODSL       TO TEST-KDPRODSL                     
175300             IF KDPRODSL-VOLVO-EMB                                        
175400               CONTINUE                                                   
175500             ELSE                                                         
175600               MOVE NEJ                TO INDATA-SW                       
175700               MOVE MED4(SPRAK-IX)     TO MOD-TEMFSINF                    
175800               MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)         
175900             END-IF                                                       
176000           ELSE                                                           
176100             MOVE NEJ                TO INDATA-SW                         
176200             MOVE MED4(SPRAK-IX)     TO MOD-TEMFSINF                      
176300             MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)           
176400           END-IF                                                         
176500         END-IF                                                           
176600                                                                          
176700         IF INDATA-OK                                                     
176800           IF (IDARTNR-ALT-ERSATT-PAA-ARTREG) AND                         
176900               (EN-AV-VALDA-STRUKT-1002-SATS)                             
177000             PERFORM S01-KOLLA-IDAO-O-TIAAVV-EJ-OBL                       
177100           ELSE                                                           
177200             PERFORM S08-KOLLA-IDAO-O-TIAAVV-OBL                          
177300           END-IF                                                         
177400                                                                          
177500           IF MID-REANTPSA(INDX) = ALL '+'                                
177600             MOVE NEJ               TO INDATA-SW                          
177700             MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)            
177800           ELSE                                                           
177900             PERFORM S15-KOLLA-REANTPSA                                   
178000           END-IF                                                         
178100                                                                          
178200           MOVE MID-IDARTNR(INDX) TO W-IDARTNR                            
178300                                                                          
178400           PERFORM IMS-GET-ARTC-ARTC01                                    
178500           IF SEGMENT-FINNS                                               
178600*******      TILLKOMMANDE ARTIKEL FINNS PÅ ARTREG                         
178700                                                                          
178800             IF MID-KDSORT(INDX) = ALL '+' OR SPACE                       
178900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR(INDX)         
179000             ELSE                                                         
179100               IF MID-KDSORT(INDX) = ART-KDSORT                           
179200                 MOVE MFS-ALFA-FAELT-RAETT TO                             
179300                                            MOD-KDSORT-ATTR(INDX)         
179400               ELSE                                                       
179500                 MOVE NEJ                TO INDATA-SW                     
179600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)         
179700               END-IF                                                     
179800             END-IF                                                       
179900                                                                          
180000             IF (MID-BEART(INDX) = ALL '+' OR SPACE) AND                  
180100                 (MID-KDBENHOM(INDX) = ALL '+')                           
180200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR(INDX)          
180300               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-ATTR(INDX)        
180400             ELSE                                                         
180500                                                                          
180600               MOVE MID-IDARTNR(INDX) TO W-IDARTNR                        
180700               PERFORM IMS-GET-BENA-BENA01-BSEQ                           
180800               IF SEGMENT-FINNS                                           
180900                                                                          
181000                 IF MID-KDBENHOM(INDX) = ALL '+'                          
181100                   CONTINUE                                               
181200                 ELSE                                                     
181300                   IF MID-KDBENHOM(INDX) NUMERIC                          
181400                     MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM-NUM           
181500                     IF WS-KDBENHOM-NUM = BENA01-BEN-KDHOMONYM            
181600                       MOVE MFS-NUM-FAELT-RAETT TO                        
181700                                          MOD-KDBENHOM-ATTR(INDX)         
181800                     ELSE                                                 
181900                       MOVE NEJ               TO INDATA-SW                
182000                       MOVE MFS-NUM-FAELT-FEL TO                          
182100                                          MOD-KDBENHOM-ATTR(INDX)         
182200                     END-IF                                               
182300                   ELSE                                                   
182400                     MOVE NEJ               TO INDATA-SW                  
182500                     MOVE MFS-NUM-FAELT-FEL TO                            
182600                                          MOD-KDBENHOM-ATTR(INDX)         
182700                   END-IF                                                 
182800                 END-IF                                                   
182900                                                                          
183000                 IF MID-BEART(INDX) = ALL '+' OR SPACE                    
183100                   CONTINUE                                               
183200                 ELSE                                                     
183300                   MOVE WS-IDSKYLT TO W-IDSKYLT                           
183400                   PERFORM IMS-GET-BENA-BENA11-BSEQ                       
183500                   IF MID-BEART(INDX) = BENA11-TEXT-BEART                 
183600                     MOVE MFS-ALFA-FAELT-RAETT TO                         
183700                                             MOD-BEART-ATTR(INDX)         
183800                   ELSE                                                   
183900                     MOVE NEJ                TO INDATA-SW                 
184000                     MOVE MFS-ALFA-FAELT-FEL TO                           
184100                                             MOD-BEART-ATTR(INDX)         
184200                   END-IF                                                 
184300                 END-IF                                                   
184400               ELSE                                                       
184500                 MOVE NEJ                TO INDATA-SW                     
184600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR(INDX)          
184700                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)        
184800               END-IF                                                     
184900             END-IF                                                       
185000                                                                          
185100             PERFORM IMS-GET-SATB-SATB01                                  
185200             IF SEGMENT-FINNS                                             
185300*********      TILLKOMMANDE ARTIKEL FINNS PÅ ARTREG OCH RASA              
185400*********      JÄMFÖR STRUKTURTYP MED DEN PÅ RASA                         
185500               IF MID-IDSTRTYP(INDX) = ALL '+' OR SPACE                   
185600                 MOVE NEJ                TO INDATA-SW                     
185700                 MOVE MFS-ALFA-FAELT-FEL TO                               
185800                                         MOD-IDSTRTYP-ATTR(INDX)          
185900               ELSE                                                       
186000                 IF MID-IDSTRTYP(INDX) = SATB01-STR-IDSTRTYP              
186100                   MOVE MFS-ALFA-FAELT-RAETT TO                           
186200                                         MOD-IDSTRTYP-ATTR(INDX)          
186300                 ELSE                                                     
186400                   MOVE NEJ                TO INDATA-SW                   
186500                   MOVE MFS-ALFA-FAELT-FEL TO                             
186600                                         MOD-IDSTRTYP-ATTR(INDX)          
186700                 END-IF                                                   
186800               END-IF                                                     
186900                                                                          
187000               IF INDATA-OK                                               
187100                 IF MID-IDSTRTYP(INDX) = 'S' OR 'K'                       
187200                   PERFORM S03122-KOLLA-UTVALD-EJ-I-TILLK                 
187300*                  IF INDATA-OK                                           
187400*                    PERFORM S03123-KOLLA-STRUKT-EJ-I-SJALV               
187500*                  END-IF                                                 
187600                 END-IF                                                   
187700               END-IF                                                     
187800                                                                          
187900             ELSE                                                         
188000*********      TILLKOMMANDE ARTIKEL FINNS PÅ ARTREG MEN SAKNAS            
188100*********      PÅ RASA. STRUKTURTYP KAN ENDAST VARA BLANK                 
188200               IF MID-IDSTRTYP(INDX) = ALL '+' OR SPACE                   
188300                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP(INDX)          
188400               ELSE                                                       
188500                 MOVE NEJ                TO INDATA-SW                     
188600                 MOVE MFS-ALFA-FAELT-FEL TO                               
188700                                         MOD-IDSTRTYP-ATTR(INDX)          
188800               END-IF                                                     
188900                                                                          
189000             END-IF                                                       
189100                                                                          
189200           ELSE                                                           
189300             PERFORM IMS-GET-SATB-SATB01                                  
189400             IF SEGMENT-FINNS                                             
189500*********      TILLKOMMANDE ARTIKEL SAKNAS PÅ ARTREG                      
189600*********      MEN FINNS PÅ RASA JÄMFÖR EV. INMATADE                      
189700*********      UPPGIFTER MED RASA (MÅSTE STÄMMA)                          
189800                                                                          
189900               IF MID-KDSORT(INDX) = ALL '+' OR SPACE                     
190000                 CONTINUE                                                 
190100               ELSE                                                       
190200                 IF SATB01-STR-IDSTRTYP = 'S'                             
190300                   IF MID-KDSORT(INDX) = 'SA' OR 'TM'                     
190400                     MOVE MFS-ALFA-FAELT-RAETT TO                         
190500                                            MOD-KDSORT-ATTR(INDX)         
190600                   ELSE                                                   
190700                     MOVE NEJ                TO INDATA-SW                 
190800                     MOVE MFS-ALFA-FAELT-FEL TO                           
190900                                            MOD-KDSORT-ATTR(INDX)         
191000                   END-IF                                                 
191100                 ELSE                                                     
191200*************      STRUKTURTYP ÄR 'R' ELLER 'K' PÅ RASA                   
191300                   IF MID-KDSORT(INDX) = 'ST'                             
191400                     MOVE MFS-ALFA-FAELT-RAETT TO                         
191500                                            MOD-KDSORT-ATTR(INDX)         
191600                   ELSE                                                   
191700                     MOVE NEJ                TO INDATA-SW                 
191800                     MOVE MFS-ALFA-FAELT-FEL TO                           
191900                                            MOD-KDSORT-ATTR(INDX)         
192000                   END-IF                                                 
192100                 END-IF                                                   
192200               END-IF                                                     
192300                                                                          
192400               IF MID-BEART(INDX) = ALL '+' OR SPACE                      
192500                 CONTINUE                                                 
192600               ELSE                                                       
192700                 IF MID-BEART(INDX) = SATB01-STR-BEART-SVE                
192800                   MOVE MFS-ALFA-FAELT-RAETT TO                           
192900                                            MOD-BEART-ATTR(INDX)          
193000                 ELSE                                                     
193100                   IF WS-IDSKYLT = 'GB '                                  
193200                      MOVE SATB01-STR-BEART-SVE TO W-BEART                
193300                      MOVE SATB01-STR-KDBENHOM  TO WS-KDBENHOM            
193400                      PERFORM S18-LAS-BEART-GB                            
193500                      IF MID-BEART(INDX) = WS-BEART-GB                    
193600                         MOVE MFS-ALFA-FAELT-RAETT TO                     
193700                                            MOD-BEART-ATTR(INDX)          
193800                      ELSE                                                
193900                          MOVE NEJ TO INDATA-SW                           
194000                          MOVE MFS-ALFA-FAELT-FEL TO                      
194100                                     MOD-BEART-ATTR(INDX)                 
194200                      END-IF                                              
194300                   ELSE                                                   
194400                      MOVE NEJ                TO INDATA-SW                
194500                      MOVE MFS-ALFA-FAELT-FEL TO                          
194600                                           MOD-BEART-ATTR(INDX)           
194700                   END-IF                                                 
194800                 END-IF                                                   
194900               END-IF                                                     
195000                                                                          
195100               IF MID-KDBENHOM(INDX) = ALL '+'                            
195200                 CONTINUE                                                 
195300               ELSE                                                       
195400                 IF MID-KDBENHOM(INDX) NUMERIC                            
195500                   MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM-NUM             
195600                   IF WS-KDBENHOM-NUM = SATB01-STR-KDBENHOM               
195700                     MOVE MFS-NUM-FAELT-RAETT TO                          
195800                                        MOD-KDBENHOM-ATTR(INDX)           
195900                   ELSE                                                   
196000                     MOVE NEJ               TO INDATA-SW                  
196100                     MOVE MFS-NUM-FAELT-FEL TO                            
196200                                        MOD-KDBENHOM-ATTR(INDX)           
196300                   END-IF                                                 
196400                 ELSE                                                     
196500                   MOVE NEJ               TO INDATA-SW                    
196600                   MOVE MFS-NUM-FAELT-FEL TO                              
196700                                        MOD-KDBENHOM-ATTR(INDX)           
196800                 END-IF                                                   
196900               END-IF                                                     
197000                                                                          
197100               IF MID-IDSTRTYP(INDX) = ALL '+' OR SPACE                   
197200                 MOVE NEJ                TO INDATA-SW                     
197300                 MOVE MFS-ALFA-FAELT-FEL TO                               
197400                                        MOD-IDSTRTYP-ATTR(INDX)           
197500               ELSE                                                       
197600                 IF MID-IDSTRTYP(INDX) = SATB01-STR-IDSTRTYP              
197700                   MOVE MFS-ALFA-FAELT-RAETT TO                           
197800                                        MOD-IDSTRTYP-ATTR(INDX)           
197900                 ELSE                                                     
198000                   MOVE NEJ                TO INDATA-SW                   
198100                   MOVE MFS-ALFA-FAELT-FEL TO                             
198200                                       MOD-IDSTRTYP-ATTR(INDX)            
198300                 END-IF                                                   
198400               END-IF                                                     
198500                                                                          
198600               IF INDATA-OK                                               
198700                 IF MID-IDSTRTYP(INDX) = 'S' OR 'K'                       
198800                   PERFORM S03122-KOLLA-UTVALD-EJ-I-TILLK                 
198900*                  IF INDATA-OK                                           
199000*                    PERFORM S03123-KOLLA-STRUKT-EJ-I-SJALV               
199100*                  END-IF                                                 
199200                 END-IF                                                   
199300               END-IF                                                     
199400                                                                          
199500             ELSE                                                         
199600*********      TILLKOMMANDE ARTIKEL SAKNAS PÅ ARTREG OCH PÅ RASA          
199700                                                                          
199800               IF MID-KDSORT(INDX) = ALL '+'                              
199900                 MOVE NEJ                TO INDATA-SW                     
200000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)         
200100               ELSE                                                       
200200                 IF MID-KDSORT(INDX) = 'SA' OR 'TM'                       
200300                   MOVE NEJ                TO INDATA-SW                   
200400                   MOVE MFS-ALFA-FAELT-FEL TO                             
200500                                            MOD-KDSORT-ATTR(INDX)         
200600                 ELSE                                                     
200700                   MOVE MID-KDSORT(INDX) TO WS-KDSORT-GODK                
200800                   IF KDSORT-GODK                                         
200900                     MOVE MFS-ALFA-FAELT-RAETT TO                         
201000                                            MOD-KDSORT-ATTR(INDX)         
201100                   ELSE                                                   
201200                     MOVE NEJ                TO INDATA-SW                 
201300                     MOVE MFS-ALFA-FAELT-FEL TO                           
201400                                            MOD-KDSORT-ATTR(INDX)         
201500                   END-IF                                                 
201600                 END-IF                                                   
201700               END-IF                                                     
201800                                                                          
201900               PERFORM S06-KOLLA-BEART-KDBENHOM                           
202000                                                                          
202100               IF (MID-IDSTRTYP(INDX) = ALL '+') OR                       
202200                  (MID-IDSTRTYP(INDX) = SPACE)                            
202300                 MOVE MFS-ALFA-FAELT-RAETT TO                             
202400                                         MOD-IDSTRTYP-ATTR(INDX)          
202500               ELSE                                                       
202600                 MOVE NEJ                TO INDATA-SW                     
202700                 MOVE MFS-ALFA-FAELT-FEL TO                               
202800                                         MOD-IDSTRTYP-ATTR(INDX)          
202900               END-IF                                                     
203000                                                                          
203100             END-IF                                                       
203200           END-IF                                                         
203300                                                                          
203400           IF MID-TESTRNOT(INDX, 1) = ALL '+' OR SPACE                    
203500             CONTINUE                                                     
203600           ELSE                                                           
203700             MOVE MFS-ALFA-FAELT-RAETT TO                                 
203800                                       MOD-TESTRNOT-ATTR(INDX, 1)         
203900           END-IF                                                         
204000                                                                          
204100           IF MID-TESTRNOT(INDX, 2) = ALL '+' OR SPACE                    
204200             CONTINUE                                                     
204300           ELSE                                                           
204400             MOVE MFS-ALFA-FAELT-RAETT TO                                 
204500                                       MOD-TESTRNOT-ATTR(INDX, 2)         
204600           END-IF                                                         
204700         ELSE                                                             
204800           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
204900                                      MOD-REANTPSA-ATTR(INDX)             
205000                                      MOD-KDSORT-ATTR(INDX)               
205100                                      MOD-BEART-ATTR(INDX)                
205200                                      MOD-KDBENHOM-ATTR(INDX)             
205300                                      MOD-IDSTRTYP-ATTR(INDX)             
205400                                      MOD-TESTRNOT-ATTR(INDX, 1)          
205500                                      MOD-TESTRNOT-ATTR(INDX, 2)          
205600         END-IF                                                           
205700       ELSE                                                               
205800         IF MOD-TEMFSINF = SPACE                                          
205900           IF (IDARTNR-FORPACKNING) OR                                    
206000               (EN-AV-VALDA-STRUKT-1002-SATS)                             
206100             MOVE MED2(SPRAK-IX) TO MOD-TEMFSINF                          
206200           END-IF                                                         
206300         END-IF                                                           
206400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REANTPSA-ATTR(INDX)            
206500                                       MOD-KDSORT-ATTR(INDX)              
206600                                       MOD-BEART-ATTR(INDX)               
206700                                       MOD-KDBENHOM-ATTR(INDX)            
206800                                       MOD-IDSTRTYP-ATTR(INDX)            
206900                                       MOD-TESTRNOT-ATTR(INDX, 1)         
207000                                       MOD-TESTRNOT-ATTR(INDX, 2)         
207100       END-IF                                                             
207200                                                                          
207300     ELSE                                                                 
207400       MOVE NEJ                   TO INDATA-SW                            
207500       MOVE MFS-ALFA-FAELT-FEL    TO MOD-BELEVART-ATTR(INDX)              
207600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REANTPSA-ATTR(INDX)              
207700                                     MOD-KDSORT-ATTR(INDX)                
207800                                     MOD-BEART-ATTR(INDX)                 
207900                                     MOD-KDBENHOM-ATTR(INDX)              
208000                                     MOD-IDSTRTYP-ATTR(INDX)              
208100                                     MOD-TESTRNOT-ATTR(INDX, 1)           
208200                                     MOD-TESTRNOT-ATTR(INDX, 2)           
208300       IF (IDARTNR-FORPACKNING) OR (EN-AV-VALDA-STRUKT-1002-SATS)         
208400         MOVE MED2(SPRAK-IX) TO MOD-TEMFSINF                              
208500       END-IF                                                             
208600     END-IF                                                               
208700     .                                                                    
208800     EJECT                                                                
208900 S03121-KOLLA-TILLK-EJ-UTVALD SECTION.                                    
209000****************************************************                      
209100* KONTROLL ATT TILLKOMMANDE ARTIKEL INTE ÄR SAMMA  *                      
209200* SOM NÅGON AV DE VALDA STRUKTURERNA (FÅR EJ INGÅ  *                      
209300* I SIG SJÄLV)                                     *                      
209400****************************************************                      
209500                                                                          
209600     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
209700     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
209800     PERFORM UNTIL SEGMENT-SAKNAS                                         
209900       IF SEGMENT-FINNS                                                   
210000         MOVE 001                 TO WORK-KDCALL                          
210100         MOVE WC-CDC-SE           TO WORK-IDDC                            
210200         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
210300         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
210400         CALL WORKDAY USING WORK-KDCALL                                   
210500                            WORK-DATE-AREA                                
210600                            WORK-KDSVAR                                   
210700         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
210800*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
210900           MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
211000           PERFORM IMS-GET-SATB-SATB01                                    
211100           PERFORM IMS-DLET-SATB                                          
211200                                                                          
211300           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
211400         ELSE                                                             
211500********** 'GÄLLANDE' KONVERTERAD STRUKTUR                                
211600           MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-NUM                       
211700           COMPUTE WS-IDARTNR-OKONV = 999999999 -                         
211800                                      SATB01-STR-IDARTNR                  
211900           IF WS-IDARTNR-NUM = WS-IDARTNR-OKONV                           
212000             MOVE 'GE'               TO STATUS-WS                         
212100             MOVE NEJ                TO INDATA-SW                         
212200             MOVE MED5(SPRAK-IX)     TO MOD-TEMFSINF                      
212300             MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)           
212400           ELSE                                                           
212500             PERFORM IMS-GET-SATB-DSEQ-NEXT                               
212600           END-IF                                                         
212700         END-IF                                                           
212800       END-IF                                                             
212900     END-PERFORM                                                          
213000     .                                                                    
213100     EJECT                                                                
213200 S03122-KOLLA-UTVALD-EJ-I-TILLK SECTION.                                  
213300****************************************************                      
213400* KONTROLL ATT VALDA STRUKTURER EJ INGÅR I TILLKO- *                      
213500* MMANDE ARTIKEL , (FÅR EJ INGÅ I SIG SJÄLV).      *                      
213600* VID PÅTRÄFFANDE AV EN RAD SOM ÄR EN SATS ELLER   *                      
213700* KOMPLETTENHET LÄGGS DETTA ARTIKELNR I EN TABELL  *                      
213800* DÄR DENNA STRUKTUR SEDAN KONTROLLERAS.           *                      
213900****************************************************                      
214000                                                                          
214100     PERFORM S07-NOLLSTAELL-STRUKTURTABELL                                
214200                                                                          
214300     MOVE 1 TO TAB-INDX                                                   
214400                                                                          
214500     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
214600     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
214700     MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                            
214800     PERFORM UNTIL (KONVERTERADE-SEGMENT-SAKNAS) OR                       
214900                   (INDATA-FEL)                                           
215000       IF SEGMENT-FINNS                                                   
215100         MOVE 001                 TO WORK-KDCALL                          
215200         MOVE WC-CDC-SE           TO WORK-IDDC                            
215300         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
215400         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
215500         CALL WORKDAY USING WORK-KDCALL                                   
215600                            WORK-DATE-AREA                                
215700                            WORK-KDSVAR                                   
215800         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
215900*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
216000           MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
216100           PERFORM IMS-GET-SATB-SATB01                                    
216200           PERFORM IMS-DLET-SATB                                          
216300                                                                          
216400           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
216500           MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                      
216600         ELSE                                                             
216700********** 'GÄLLANDE' KONVERTERAD STRUKTUR                                
216800           COMPUTE WS-IDARTNR-OKONV = 999999999 -                         
216900                                      SATB01-STR-IDARTNR                  
217000           MOVE WS-IDARTNR-OKONV TO WS-IDARTNR-SPAR                       
217100                                                                          
217200           MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-NUM                       
217300           MOVE WS-IDARTNR-NUM TO W-IDARTNR                               
217400           PERFORM IMS-GET-SATB-SATB01                                    
217500           PERFORM IMS-GET-SATB-SATB11-OKVAL                              
217600           PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
217700             IF SEGMENT-FINNS                                             
217800               MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                  
217900               MOVE DAGENS-DATUM          TO TMP2-YYMMDD                  
218000               PERFORM WY2000P1                                           
218100               IF TMP1-YYMMDD > TMP2-YYMMDD                               
218200**************** GÄLLANDE RAD                                             
218300                 IF SATB11-RAD-IDARTNR = WS-IDARTNR-SPAR                  
218400                   MOVE NEJ                TO INDATA-SW                   
218500                   MOVE MED5(SPRAK-IX)     TO MOD-TEMFSINF                
218600                   MOVE MFS-ALFA-FAELT-FEL TO                             
218700                                          MOD-BELEVART-ATTR(INDX)         
218800                 ELSE                                                     
218900                   IF SATB11-RAD-IDSTRTYP = 'S' OR 'K'                    
219000                     PERFORM S031221-LAEGG-UPP-RAD-I-TABELL               
219100                   END-IF                                                 
219200                 END-IF                                                   
219300               END-IF                                                     
219400               IF INDATA-OK                                               
219500                 PERFORM IMS-GET-SATB-SATB11-OKVAL                        
219600               END-IF                                                     
219700             END-IF                                                       
219800           END-PERFORM                                                    
219900         END-IF                                                           
220000         IF INDATA-OK                                                     
220100           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
220200           MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                      
220300         END-IF                                                           
220400       END-IF                                                             
220500     END-PERFORM                                                          
220600                                                                          
220700     IF INDATA-OK                                                         
220800       IF TAB-INDX > 1                                                    
220900******** STRUKTURNR FINNS I TABELLEN                                      
221000         MOVE 1 TO TAB-INDX2                                              
221100         PERFORM UNTIL (TAB-INDX2 > (TAB-INDX - 1) ) OR                   
221200                       (INDATA-FEL)                                       
221300           MOVE STRUKTURNR(TAB-INDX2) TO W-IDARTNR                        
221400                                                                          
221500           PERFORM IMS-GET-SATB-SATB01                                    
221600           PERFORM IMS-GET-SATB-SATB11-OKVAL                              
221700           PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
221800             IF SEGMENT-FINNS                                             
221900               MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                  
222000               MOVE DAGENS-DATUM          TO TMP2-YYMMDD                  
222100               PERFORM WY2000P1                                           
222200               IF TMP1-YYMMDD > TMP2-YYMMDD                               
222300**************** GÄLLANDE RAD                                             
222400                 IF SATB11-RAD-IDARTNR = WS-IDARTNR-SPAR                  
222500                   MOVE NEJ                TO INDATA-SW                   
222600                   MOVE MED5(SPRAK-IX)     TO MOD-TEMFSINF                
222700                   MOVE MFS-ALFA-FAELT-FEL TO                             
222800                                          MOD-BELEVART-ATTR(INDX)         
222900                 ELSE                                                     
223000                   IF SATB11-RAD-IDSTRTYP = 'S' OR 'K'                    
223100                     PERFORM S031221-LAEGG-UPP-RAD-I-TABELL               
223200                   END-IF                                                 
223300                 END-IF                                                   
223400               END-IF                                                     
223500               IF INDATA-OK                                               
223600                 PERFORM IMS-GET-SATB-SATB11-OKVAL                        
223700               END-IF                                                     
223800             END-IF                                                       
223900           END-PERFORM                                                    
224000           ADD 1 TO TAB-INDX2                                             
224100         END-PERFORM                                                      
224200       END-IF                                                             
224300     END-IF                                                               
224400     .                                                                    
224500     EJECT                                                                
224600 S031221-LAEGG-UPP-RAD-I-TABELL SECTION.                                  
224700***********************************************************               
224800* OM STRUKTURNUMMER (RAD) EJ FINNS I TABELL LÄGGS DET UPP *               
224900***********************************************************               
225000                                                                          
225100     MOVE 1   TO KONTROLL-TAB-INDX                                        
225200     MOVE NEJ TO STRUKTURNR-FINNS-I-TABELL-SW                             
225300                                                                          
225400     PERFORM UNTIL KONTROLL-TAB-INDX > TAB-INDX                           
225500       IF STRUKTURNR(KONTROLL-TAB-INDX) = SATB11-RAD-IDARTNR              
225600         MOVE JA  TO STRUKTURNR-FINNS-I-TABELL-SW                         
225700         MOVE 999 TO KONTROLL-TAB-INDX                                    
225800       ELSE                                                               
225900         ADD 1 TO KONTROLL-TAB-INDX                                       
226000       END-IF                                                             
226100     END-PERFORM                                                          
226200                                                                          
226300     IF STRUKTURNR-FINNS-I-TABELL                                         
226400       CONTINUE                                                           
226500     ELSE                                                                 
226600       MOVE SATB11-RAD-IDARTNR TO STRUKTURNR(TAB-INDX)                    
226700       ADD 1 TO TAB-INDX                                                  
226800     END-IF                                                               
226900     .                                                                    
227000     EJECT                                                                
227100*S03123-KOLLA-STRUKT-EJ-I-SJALV SECTION.                                  
227200****************************************************                      
227300* KONTROLL OM VALDA STRUKTURER INGÅR SOM RAD I     *                      
227400* NÅGON ANNAN STRUKTUR. I SÅDANA FALL FÅR INTE     *                      
227500* TILLKOMMANDE ARTIKELNR VARA SAMMA SOM DETTA      *                      
227600* STRUKTURNR (ROTEN). SEDAN KOLLAS OM STRUKTURNUM- *                      
227700* RET (ROTEN) INGÅR SOM RAD O.S.V .  DETTA FÖR ATT *                      
227800* FÖRHINDRA ATT NGN STRUKTUR INGÅR I SIG SJÄLV.    *                      
227900****************************************************                      
228000*                                                                         
228100*    PERFORM S07-NOLLSTAELL-STRUKTURTABELL                                
228200*                                                                         
228300*    MOVE 1                 TO TAB-INDX                                   
228400*    MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-SPAR                            
228500*                                                                         
228600*    MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
228700*    PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
228800*    MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                            
228900*    PERFORM UNTIL (KONVERTERADE-SEGMENT-SAKNAS) OR                       
229000*                  (INDATA-FEL)                                           
229100*      IF SEGMENT-FINNS                                                   
229200*        MOVE 001                 TO WORK-KDCALL                          
229300*        MOVE WC-CDC-SE           TO WORK-IDDC                            
229400*        MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
229500*        MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
229600*        CALL WORKDAY USING WORK-KDCALL                                   
229700*                           WORK-DATE-AREA                                
229800*                           WORK-KDSVAR                                   
229900*        IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
230000*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
230100*          MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
230200*          PERFORM IMS-GET-SATB-SATB01                                    
230300*          PERFORM IMS-DLET-SATB                                          
230400*                                                                         
230500*          PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
230600*          MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                      
230700*        ELSE                                                             
230800********** 'GÄLLANDE' KONVERTERAD STRUKTUR                                
230900*          MOVE SPACE            TO W-IDLEVNR                             
231000*                                   W-BELEVART                            
231100*          COMPUTE WS-IDARTNR-OKONV = 999999999 -                         
231200*                                     SATB01-STR-IDARTNR                  
231300*          MOVE WS-IDARTNR-OKONV TO W-IDARTNR                             
231400*          PERFORM IMS-GET-SATB-CSEQ-UNIK                                 
231500*          PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
231600*            IF SEGMENT-FINNS                                             
231700*              IF SATB01C-STR-IDARTNR > WS-MAX-IDARTNR-OKONV              
231800*                MOVE 'GE' TO STATUS-WS                                   
231900*              ELSE                                                       
232000*                IF SATB01C-STR-TIBORT > 0                                
232100*****************  STRUKTUR BORTTAGSMÄRKT                                 
232200*                  PERFORM IMS-GET-SATB-CSEQ-NEXT                         
232300*                ELSE                                                     
232400*                  MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD             
232500*                  MOVE DAGENS-DATUM           TO TMP2-YYMMDD             
232600*                  PERFORM WY2000P1                                       
232700*                  IF TMP1-YYMMDD > TMP2-YYMMDD                           
232800*******************  FINNS SOM GÄLLANDE RAD                               
232900*                    IF SATB01C-STR-IDARTNR = WS-IDARTNR-SPAR             
233000*                      MOVE NEJ                TO INDATA-SW               
233100*                      MOVE MED5(SPRAK-IX)     TO MOD-TEMFSINF            
233200*                      MOVE MFS-ALFA-FAELT-FEL TO                         
233300*                                         MOD-BELEVART-ATTR(INDX)         
233400*                    ELSE                                                 
233500*                      IF SATB01-STR-IDSTRTYP = 'S' OR 'K'                
233600*********************  LÄGG UPP I TABELL FÖR SENARE KOLL                  
233700*                        PERFORM S031231-LAEGG-UPP-ROT-I-TABELL           
233800*                      END-IF                                             
233900*                      PERFORM IMS-GET-SATB-CSEQ-NEXT                     
234000*                    END-IF                                               
234100*                  ELSE                                                   
234200*                    PERFORM IMS-GET-SATB-CSEQ-NEXT                       
234300*                  END-IF                                                 
234400*                END-IF                                                   
234500*              END-IF                                                     
234600*            END-IF                                                       
234700*          END-PERFORM                                                    
234800*        END-IF                                                           
234900*        IF INDATA-OK                                                     
235000*          PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
235100*          MOVE SATB-D-STATUS-CODE TO STATUS-KONV-WS                      
235200*        END-IF                                                           
235300*      END-IF                                                             
235400*    END-PERFORM                                                          
235500*                                                                         
235600*    IF INDATA-OK                                                         
235700*      IF TAB-INDX > 1                                                    
235800******** STRUKTURNR FINNS I TABELLEN                                      
235900*        MOVE 1 TO TAB-INDX2                                              
236000*        PERFORM UNTIL (TAB-INDX2 > (TAB-INDX - 1) ) OR                   
236100*                      (INDATA-FEL)                                       
236200*          MOVE STRUKTURNR(TAB-INDX2) TO W-IDARTNR                        
236300*          PERFORM IMS-GET-SATB-CSEQ-UNIK                                 
236400*          PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
236500*            IF SEGMENT-FINNS                                             
236600*              IF SATB01C-STR-IDARTNR > WS-MAX-IDARTNR-OKONV              
236700*                MOVE 'GE' TO STATUS-WS                                   
236800*              ELSE                                                       
236900*                IF SATB01C-STR-TIBORT > 0                                
237000*****************  STRUKTUR BORTTAGSMÄRKT                                 
237100*                  PERFORM IMS-GET-SATB-CSEQ-NEXT                         
237200*                ELSE                                                     
237300*                  MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD             
237400*                  MOVE DAGENS-DATUM           TO TMP2-YYMMDD             
237500*                  PERFORM WY2000P1                                       
237600*                  IF TMP1-YYMMDD > TMP2-YYMMDD                           
237700*******************  FINNS SOM GÄLLANDE RAD                               
237800*                    IF SATB01C-STR-IDARTNR = WS-IDARTNR-SPAR             
237900*                      MOVE NEJ                TO INDATA-SW               
238000*                      MOVE MED5(SPRAK-IX)     TO MOD-TEMFSINF            
238100*                      MOVE MFS-ALFA-FAELT-FEL TO                         
238200*                                         MOD-BELEVART-ATTR(INDX)         
238300*                    ELSE                                                 
238400*                      IF SATB01-STR-IDSTRTYP = 'S' OR 'K'                
238500*********************  LÄGG UPP I TABELL FÖR SENARE KOLL                  
238600*                        PERFORM S031231-LAEGG-UPP-ROT-I-TABELL           
238700*                      END-IF                                             
238800*                      PERFORM IMS-GET-SATB-CSEQ-NEXT                     
238900*                    END-IF                                               
239000*                  ELSE                                                   
239100*                    PERFORM IMS-GET-SATB-CSEQ-NEXT                       
239200*                  END-IF                                                 
239300*                END-IF                                                   
239400*              END-IF                                                     
239500*            END-IF                                                       
239600*          END-PERFORM                                                    
239700*          ADD 1 TO TAB-INDX2                                             
239800*        END-PERFORM                                                      
239900*      END-IF                                                             
240000*    END-IF                                                               
240100*    .                                                                    
240200     EJECT                                                                
240300*S031231-LAEGG-UPP-ROT-I-TABELL SECTION.                                  
240400***********************************************************               
240500* OM STRUKTURNUMMER (ROT) EJ FINNS I TABELL LÄGGS DET UPP *               
240600***********************************************************               
240700*                                                                         
240800*    MOVE 1   TO KONTROLL-TAB-INDX                                        
240900*    MOVE NEJ TO STRUKTURNR-FINNS-I-TABELL-SW                             
241000*                                                                         
241100*    PERFORM UNTIL KONTROLL-TAB-INDX > TAB-INDX                           
241200*      IF STRUKTURNR(KONTROLL-TAB-INDX) = SATB01-STR-IDARTNR              
241300*        MOVE JA  TO STRUKTURNR-FINNS-I-TABELL-SW                         
241400*        MOVE 999 TO KONTROLL-TAB-INDX                                    
241500*      ELSE                                                               
241600*        ADD 1 TO KONTROLL-TAB-INDX                                       
241700*      END-IF                                                             
241800*    END-PERFORM                                                          
241900*                                                                         
242000*    IF STRUKTURNR-FINNS-I-TABELL                                         
242100*      CONTINUE                                                           
242200*    ELSE                                                                 
242300*      MOVE SATB01-STR-IDARTNR TO STRUKTURNR(TAB-INDX)                    
242400*      ADD 1 TO TAB-INDX                                                  
242500*    END-IF                                                               
242600*    .                                                                    
242700     EJECT                                                                
242800 S0313-KOLLA-INPUT-IDLEV-BELEVA SECTION.                                  
242900****************************************************                      
243000* KONTROLL NÄR TILLKOMMANDE ARTIKEL ÄR IDLEVNR +   *                      
243100* BELEVART                                         *                      
243200****************************************************                      
243300                                                                          
243400     IF MID-IDLEVNR(INDX)(1:1) NOT = '0' AND  ' ' AND '+'                 
243500       CONTINUE                                                           
243600     ELSE                                                                 
243700       MOVE NEJ               TO INDATA-SW                                
243800       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(INDX)                  
243900     END-IF                                                               
244000                                                                          
244100     IF MID-BELEVART(INDX) = ALL '+'                                      
244200       MOVE NEJ                TO INDATA-SW                               
244300       MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-ATTR(INDX)                 
244400     END-IF                                                               
244500                                                                          
244600     PERFORM S08-KOLLA-IDAO-O-TIAAVV-OBL                                  
244700                                                                          
244800     IF MID-REANTPSA(INDX) = ALL '+'                                      
244900       MOVE NEJ               TO INDATA-SW                                
245000       MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)                  
245100     ELSE                                                                 
245200       PERFORM S15-KOLLA-REANTPSA                                         
245300     END-IF                                                               
245400                                                                          
245500     IF MID-KDSORT(INDX) = ALL '+'                                        
245600       MOVE NEJ                TO INDATA-SW                               
245700       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)                   
245800     ELSE                                                                 
245900       IF MID-KDSORT(INDX) = 'SA' OR 'TM'                                 
246000******** KDSORT = 'SA' ENDAST TILLÅTEN VID ETT ARTIKELNR                  
246100******** SOM FINNS SOM ROT PÅ RASA MED IDSTRTYP = 'S'                     
246200         MOVE NEJ                TO INDATA-SW                             
246300         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)                 
246400       ELSE                                                               
246500         MOVE MID-KDSORT(INDX) TO WS-KDSORT-GODK                          
246600         IF KDSORT-GODK                                                   
246700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-ATTR(INDX)             
246800         ELSE                                                             
246900           MOVE NEJ                TO INDATA-SW                           
247000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-ATTR(INDX)               
247100         END-IF                                                           
247200       END-IF                                                             
247300     END-IF                                                               
247400                                                                          
247500     PERFORM S06-KOLLA-BEART-KDBENHOM                                     
247600                                                                          
247700     IF (MID-IDSTRTYP(INDX) = ALL '+') OR                                 
247800        (MID-IDSTRTYP(INDX) = SPACE)                                      
247900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-ATTR(INDX)               
248000     ELSE                                                                 
248100       MOVE NEJ                TO INDATA-SW                               
248200       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-ATTR(INDX)                 
248300     END-IF                                                               
248400                                                                          
248500     IF MID-TESTRNOT(INDX, 1) = ALL '+' OR SPACE                          
248600       CONTINUE                                                           
248700     ELSE                                                                 
248800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(INDX, 1)            
248900     END-IF                                                               
249000                                                                          
249100     IF MID-TESTRNOT(INDX, 2) = ALL '+' OR SPACE                          
249200       CONTINUE                                                           
249300     ELSE                                                                 
249400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(INDX, 2)            
249500     END-IF                                                               
249600     .                                                                    
249700     EJECT                                                                
249800 S04-LAEGG-UPP-TILLKOMMANDE-ART SECTION.                                  
249900*********************************************************                 
250000* UPPLÄGGNING AV TILLKOMMANDE ARTIKLAR PÅ WDR5 (WLXXAZ) *                 
250100*********************************************************                 
250200                                                                          
250300     MOVE '1151'    TO W-IDHTYP                                           
250400     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
250500     PERFORM IMS-GET-XXAZ-XXAZ01                                          
250600                                                                          
250700     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
250800     MOVE WS-BELEVART       TO W-BELEVART                                 
250900     MOVE WS-IDARTNR        TO W-IDARTNR                                  
251000     MOVE LOW-VALUE         TO W-LOW-VALUE                                
251100     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
251200     PERFORM IMS-GET-XXAZ-XXAZ11                                          
251300                                                                          
251400     IF (MID-IDAO NOT = XXAZ11-1152-IDAO) OR                              
251500        (WS-TISTODAT NOT = XXAZ11-1152-TISTODAT)                          
251600       IF MID-IDAO = ALL '+'                                              
251700         MOVE SPACE      TO XXAZ11-1152-IDAO                              
251800       ELSE                                                               
251900         MOVE MID-IDAO   TO XXAZ11-1152-IDAO                              
252000       END-IF                                                             
252100                                                                          
252200       IF MID-TIAAVV = ALL '+'                                            
252300         MOVE ZERO        TO XXAZ11-1152-TISTODAT                         
252400       ELSE                                                               
252500         MOVE WS-TISTODAT TO XXAZ11-1152-TISTODAT                         
252600       END-IF                                                             
252700       PERFORM IMS-REPL-XXAZ                                              
252800     END-IF                                                               
252900                                                                          
253000     IF MID-RAD-1-IFYLLD                                                  
253100       MOVE 1 TO INDX                                                     
253200       PERFORM S041-LAEGG-UPP-PAA-WDR5                                    
253300       PERFORM MFS-STAENG-RAD-FAELT-IN                                    
253400     END-IF                                                               
253500                                                                          
253600     IF MID-RAD-2-IFYLLD                                                  
253700       MOVE 2 TO INDX                                                     
253800       PERFORM S041-LAEGG-UPP-PAA-WDR5                                    
253900       PERFORM MFS-STAENG-RAD-FAELT-IN                                    
254000     END-IF                                                               
254100     .                                                                    
254200     EJECT                                                                
254300 S041-LAEGG-UPP-PAA-WDR5 SECTION.                                         
254400                                                                          
254500     INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
254600                                                                          
254700     MOVE '1' TO XXAZ21-1154-KDSEGKEY                                     
254800                                                                          
254900     IF (MID-IDLEVNR(INDX) = ALL '+') OR                                  
255000        (MID-IDLEVNR(INDX) = SPACE)                                       
255100       MOVE SPACE TO XXAZ21-1154-IDLEVNR                                  
255200                     XXAZ21-1154-BELEVART                                 
255300       IF (MID-IDARTNR(INDX) = ALL '+') OR                                
255400          (MID-IDARTNR(INDX) = ZERO)                                      
255500         MOVE ZERO               TO XXAZ21-1154-IDARTNR                   
255600         MOVE MID-BEART(INDX)    TO XXAZ21-1154-BEART-SVE                 
255700         IF WS-IDSKYLT = 'GB '                                            
255800            MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM                        
255900            MOVE MID-BEART(INDX)    TO W-BEART                            
256000            PERFORM S19-LAS-BEART-SVE                                     
256100            MOVE WS-BEART-SVE    TO XXAZ21-1154-BEART-SVE                 
256200         END-IF                                                           
256300         MOVE MID-KDSORT(INDX)   TO XXAZ21-1154-KDSORT                    
256400         IF MID-KDBENHOM(INDX) = ALL '+'                                  
256500           MOVE ZERO             TO XXAZ21-1154-KDHOM                     
256600                                    MOD-KDBENHOM(INDX)                    
256700         ELSE                                                             
256800           MOVE MID-KDBENHOM(INDX) TO XXAZ21-1154-KDHOM                   
256900         END-IF                                                           
257000       ELSE                                                               
257100         MOVE MID-IDARTNR(INDX) TO XXAZ21-1154-IDARTNR                    
257200                                   W-IDARTNR                              
257300         PERFORM IMS-GET-ARTC-ARTC01                                      
257400         IF SEGMENT-FINNS                                                 
257500           PERFORM S09-VISA-BEART-KDBENHOM-KDSORT                         
257600           MOVE SPACE TO XXAZ21-1154-BEART-SVE                            
257700                         XXAZ21-1154-KDSORT                               
257800           MOVE ZERO  TO XXAZ21-1154-KDHOM                                
257900         ELSE                                                             
258000           PERFORM IMS-GET-SATB-SATB01                                    
258100           IF SEGMENT-FINNS                                               
258200************ ARTIKELNR FINNS BARA PÅ RASA                                 
258300             MOVE SATB01-STR-BEART-SVE TO XXAZ21-1154-BEART-SVE           
258400             IF WS-IDSKYLT = 'S  '                                        
258500                MOVE SATB01-STR-BEART-SVE TO MOD-BEART(INDX)              
258600             ELSE                                                         
258700                MOVE SATB01-STR-BEART-SVE TO W-BEART                      
258800                MOVE SATB01-STR-KDBENHOM  TO WS-KDBENHOM                  
258900                PERFORM S18-LAS-BEART-GB                                  
259000                MOVE WS-BEART-GB          TO MOD-BEART(INDX)              
259100             END-IF                                                       
259200             IF SATB01-STR-IDSTRTYP = 'S'                                 
259300               MOVE 'SA' TO XXAZ21-1154-KDSORT                            
259400                            MOD-KDSORT(INDX)                              
259500             ELSE                                                         
259600************** IDSTRTYP = 'R' ELLER 'K'                                   
259700               MOVE 'ST' TO XXAZ21-1154-KDSORT                            
259800                            MOD-KDSORT(INDX)                              
259900             END-IF                                                       
260000             MOVE SATB01-STR-KDBENHOM TO XXAZ21-1154-KDHOM                
260100                                         MOD-KDBENHOM(INDX)               
260200           ELSE                                                           
260300************ ARTIKELNR SAKNAS PÅ ARTREG OCH RASA                          
260400             IF WS-IDSKYLT = 'S  '                                        
260500                MOVE MID-BEART(INDX) TO XXAZ21-1154-BEART-SVE             
260600             ELSE                                                         
260700                MOVE MID-BEART(INDX)    TO W-BEART                        
260800                MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM                    
260900                PERFORM S19-LAS-BEART-SVE                                 
261000                MOVE WS-BEART-SVE     TO XXAZ21-1154-BEART-SVE            
261100             END-IF                                                       
261200             MOVE MID-KDSORT(INDX)   TO XXAZ21-1154-KDSORT                
261300             IF MID-KDBENHOM(INDX) = ALL '+'                              
261400               MOVE ZERO           TO XXAZ21-1154-KDHOM                   
261500                                      MOD-KDBENHOM(INDX)                  
261600             ELSE                                                         
261700               MOVE MID-KDBENHOM(INDX) TO XXAZ21-1154-KDHOM               
261800             END-IF                                                       
261900           END-IF                                                         
262000         END-IF                                                           
262100       END-IF                                                             
262200                                                                          
262300     ELSE                                                                 
262400       MOVE MID-IDLEVNR(INDX)  TO XXAZ21-1154-IDLEVNR                     
262500       MOVE MID-BELEVART(INDX) TO XXAZ21-1154-BELEVART                    
262600       MOVE ZERO               TO XXAZ21-1154-IDARTNR                     
262700       IF WS-IDSKYLT = 'S  '                                              
262800          MOVE MID-BEART(INDX) TO XXAZ21-1154-BEART-SVE                   
262900       ELSE                                                               
263000          MOVE MID-BEART(INDX)    TO W-BEART                              
263100          MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM                          
263200          PERFORM S19-LAS-BEART-SVE                                       
263300          MOVE WS-BEART-SVE     TO XXAZ21-1154-BEART-SVE                  
263400       END-IF                                                             
263500       MOVE MID-KDSORT(INDX)   TO XXAZ21-1154-KDSORT                      
263600       IF MID-KDBENHOM(INDX) = ALL '+'                                    
263700         MOVE ZERO             TO XXAZ21-1154-KDHOM                       
263800                                  MOD-KDBENHOM(INDX)                      
263900       ELSE                                                               
264000         MOVE MID-KDBENHOM(INDX) TO XXAZ21-1154-KDHOM                     
264100       END-IF                                                             
264200     END-IF                                                               
264300                                                                          
264400     MOVE MID-REANTPSA(INDX) TO DEC-IDFRIDATA                             
264500     MOVE +2                 TO DEC-KVHELTAL                              
264600     MOVE +3                 TO DEC-KVDECIMAL                             
264700     CALL WDECEDIT USING DEC-WDECAREA                                     
264800     MOVE DEC-IDEDITDATA TO XXAZ21-1154-REANTPSA                          
264900                                                                          
265000     IF MID-IDSTRTYP(INDX) = ALL '+'                                      
265100       MOVE SPACE TO XXAZ21-1154-IDSTRTYP                                 
265200     ELSE                                                                 
265300       MOVE MID-IDSTRTYP(INDX) TO XXAZ21-1154-IDSTRTYP                    
265400     END-IF                                                               
265500                                                                          
265600     IF MID-TESTRNOT(INDX, 1 ) = ALL '+'                                  
265700       MOVE SPACE TO XXAZ21-1154-TESTRNOT(1)                              
265800     ELSE                                                                 
265900       MOVE MID-TESTRNOT(INDX, 1) TO XXAZ21-1154-TESTRNOT(1)              
266000     END-IF                                                               
266100                                                                          
266200     IF MID-TESTRNOT(INDX, 2 ) = ALL '+'                                  
266300       MOVE SPACE TO XXAZ21-1154-TESTRNOT(2)                              
266400     ELSE                                                                 
266500       MOVE MID-TESTRNOT(INDX, 2) TO XXAZ21-1154-TESTRNOT(2)              
266600     END-IF                                                               
266700                                                                          
266800     MOVE '1151'            TO W-IDHTYP                                   
266900     MOVE LOW-VALUE         TO W-LOW-VALUE-2                              
267000                                                                          
267100     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
267200     MOVE WS-BELEVART       TO W-BELEVART                                 
267300     MOVE WS-IDARTNR        TO W-IDARTNR                                  
267400     MOVE LOW-VALUE         TO W-LOW-VALUE                                
267500     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
267600                                                                          
267700     PERFORM IMS-ISRT-XXAZ-XXAZ21                                         
267800                                                                          
267900     .                                                                    
268000     EJECT                                                                
268100 S05-KOLLA-OM-MID-RADER-IFYLLDA SECTION.                                  
268200*********************************************************                 
268300* KOLL OM MID-RADER IFYLLDA OCH I SÅ FALL OCKSÅ VILKA   *                 
268400*********************************************************                 
268500                                                                          
268600     MOVE NEJ TO MID-RAD-1-IFYLLD-SW                                      
268700                 MID-RAD-2-IFYLLD-SW                                      
268800                                                                          
268900     MOVE +1 TO INDX                                                      
269000     PERFORM UNTIL INDX > MAX-INDX                                        
269100                                                                          
269200       MOVE NEJ TO MID-RAD-IFYLLD-SW                                      
269300                                                                          
269400       IF MID-IDLEVNR(INDX) = ALL '+' OR                                  
269500          MID-IDLEVNR(INDX) = SPACE                                       
269600         CONTINUE                                                         
269700       ELSE                                                               
269800         MOVE JA TO MID-RAD-IFYLLD-SW                                     
269900       END-IF                                                             
270000                                                                          
270100       IF MID-BELEVART(INDX) = ALL  '+' OR                                
270200          MID-BELEVART(INDX) = SPACE                                      
270300         CONTINUE                                                         
270400       ELSE                                                               
270500         MOVE JA TO MID-RAD-IFYLLD-SW                                     
270600       END-IF                                                             
270700                                                                          
270800       IF MID-REANTPSA(INDX) = ALL  '+'                                   
270900         CONTINUE                                                         
271000       ELSE                                                               
271100         MOVE JA TO MID-RAD-IFYLLD-SW                                     
271200       END-IF                                                             
271300                                                                          
271400       IF MID-KDSORT(INDX) = ALL  '+' OR                                  
271500          MID-KDSORT(INDX) = SPACE                                        
271600         CONTINUE                                                         
271700       ELSE                                                               
271800         MOVE JA TO MID-RAD-IFYLLD-SW                                     
271900       END-IF                                                             
272000                                                                          
272100       IF MID-BEART(INDX) = ALL  '+' OR                                   
272200          MID-BEART(INDX) = SPACE                                         
272300         CONTINUE                                                         
272400       ELSE                                                               
272500         MOVE JA TO MID-RAD-IFYLLD-SW                                     
272600       END-IF                                                             
272700                                                                          
272800       IF MID-KDBENHOM(INDX) = ALL  '+'                                   
272900         CONTINUE                                                         
273000       ELSE                                                               
273100         MOVE JA TO MID-RAD-IFYLLD-SW                                     
273200       END-IF                                                             
273300                                                                          
273400       IF MID-IDSTRTYP(INDX) = ALL  '+' OR                                
273500          MID-IDSTRTYP(INDX) = SPACE                                      
273600         CONTINUE                                                         
273700       ELSE                                                               
273800         MOVE JA TO MID-RAD-IFYLLD-SW                                     
273900       END-IF                                                             
274000                                                                          
274100       IF MID-TESTRNOT(INDX, 1) = ALL  '+' OR                             
274200          MID-TESTRNOT(INDX, 1) = SPACE                                   
274300         CONTINUE                                                         
274400       ELSE                                                               
274500         MOVE JA TO MID-RAD-IFYLLD-SW                                     
274600       END-IF                                                             
274700                                                                          
274800       IF MID-TESTRNOT(INDX, 2) = ALL  '+' OR                             
274900          MID-TESTRNOT(INDX, 2) = SPACE                                   
275000         CONTINUE                                                         
275100       ELSE                                                               
275200         MOVE JA TO MID-RAD-IFYLLD-SW                                     
275300       END-IF                                                             
275400                                                                          
275500       IF MID-RAD-IFYLLD                                                  
275600         IF INDX = 1                                                      
275700           MOVE JA TO MID-RAD-1-IFYLLD-SW                                 
275800         ELSE                                                             
275900           MOVE JA TO MID-RAD-2-IFYLLD-SW                                 
276000         END-IF                                                           
276100       END-IF                                                             
276200                                                                          
276300       ADD 1 TO INDX                                                      
276400     END-PERFORM                                                          
276500                                                                          
276600     IF (MID-RAD-1-IFYLLD) OR (MID-RAD-2-IFYLLD)                          
276700       MOVE JA TO MID-RAD-IFYLLD-SW                                       
276800     END-IF                                                               
276900                                                                          
277000     IF MID-RAD-1-IFYLLD                                                  
277100       MOVE +1 TO INDX                                                    
277200       PERFORM MFS-ROR-EJ-RAD-FAELT-IN                                    
277300       PERFORM MFS-LAES-IN-RAD-IGEN                                       
277400     ELSE                                                                 
277500       MOVE +1 TO INDX                                                    
277600       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
277700       PERFORM MFS-FORMATETS-ATTR-RAD                                     
277800     END-IF                                                               
277900                                                                          
278000     IF MID-RAD-2-IFYLLD                                                  
278100       MOVE +2 TO INDX                                                    
278200       PERFORM MFS-ROR-EJ-RAD-FAELT-IN                                    
278300       PERFORM MFS-LAES-IN-RAD-IGEN                                       
278400     ELSE                                                                 
278500       MOVE +2 TO INDX                                                    
278600       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
278700       PERFORM MFS-FORMATETS-ATTR-RAD                                     
278800     END-IF                                                               
278900     .                                                                    
279000     EJECT                                                                
279100 S06-KOLLA-BEART-KDBENHOM SECTION.                                        
279200**********************************************************                
279300* KOLL ATT BEART I KOMB. MED HOMONYMKOD FINNS PÅ BEN.REG *                
279400* OM ENDAST BEART INMATAT FÅR ARTIKELBENÄMNINGEN BARA    *                
279500* FINNAS MED EN HOMONYMKOD ANNARS MÅSTE HOMONYMKOD ANGES *                
279600**********************************************************                
279700                                                                          
279800     IF MID-BEART(INDX) = ALL '+'                                         
279900       MOVE NEJ                TO INDATA-SW                               
280000       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-ATTR(INDX)                    
280100                                                                          
280200       IF MID-KDBENHOM(INDX) = ALL '+'                                    
280300         CONTINUE                                                         
280400       ELSE                                                               
280500         IF MID-KDBENHOM(INDX) NUMERIC                                    
280600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-ATTR(INDX)            
280700         ELSE                                                             
280800           MOVE NEJ               TO INDATA-SW                            
280900           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)              
281000         END-IF                                                           
281100       END-IF                                                             
281200     ELSE                                                                 
281300       MOVE MID-BEART(INDX)  TO W-BEART                                   
281400       MOVE WS-IDSKYLT       TO W-IDSKYLT                                 
281500       PERFORM IMS-GET-BENA-BENA01-ASEQ                                   
281600       IF SEGMENT-FINNS                                                   
281700         IF BENA01-BEN-KDBENSTAT < 2                                      
281800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR(INDX)              
281900                                                                          
282000           IF MID-KDBENHOM(INDX) = ALL '+'                                
282100             PERFORM IMS-GET-BENA-BENA01-ASEQ                             
282200             IF SEGMENT-FINNS                                             
282300*************  BENÄMNING FINNS MED FLERA HOMONYMKODER                     
282400*************  HOMONYMKOD MÅSTE ANGES                                     
282500               MOVE NEJ               TO INDATA-SW                        
282600               MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)          
282700             ELSE                                                         
282800               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-ATTR(INDX)        
282900             END-IF                                                       
283000           ELSE                                                           
283100             IF MID-KDBENHOM(INDX) NUMERIC                                
283200               MOVE MID-KDBENHOM(INDX) TO WS-KDBENHOM-NUM                 
283300               PERFORM UNTIL (SEGMENT-SAKNAS) OR                          
283400                       (BENA01-BEN-KDHOMONYM = WS-KDBENHOM-NUM)           
283500                 IF SEGMENT-FINNS                                         
283600                   IF BENA01-BEN-KDHOMONYM = WS-KDBENHOM-NUM              
283700                     CONTINUE                                             
283800                   ELSE                                                   
283900                     PERFORM IMS-GET-BENA-BENA01-ASEQ                     
284000                   END-IF                                                 
284100                 END-IF                                                   
284200               END-PERFORM                                                
284300                                                                          
284400               IF SEGMENT-FINNS                                           
284500                 MOVE MFS-NUM-FAELT-RAETT TO                              
284600                                           MOD-KDBENHOM-ATTR(INDX)        
284700               ELSE                                                       
284800                 MOVE NEJ               TO INDATA-SW                      
284900                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)        
285000               END-IF                                                     
285100             ELSE                                                         
285200               MOVE NEJ               TO INDATA-SW                        
285300               MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-ATTR(INDX)          
285400             END-IF                                                       
285500           END-IF                                                         
285600         ELSE                                                             
285700           MOVE NEJ                   TO INDATA-SW                        
285800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-ATTR(INDX)             
285900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-ATTR(INDX)          
286000         END-IF                                                           
286100                                                                          
286200       ELSE                                                               
286300         MOVE NEJ                   TO INDATA-SW                          
286400         MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-ATTR(INDX)               
286500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-ATTR(INDX)            
286600       END-IF                                                             
286700     END-IF                                                               
286800     .                                                                    
286900     EJECT                                                                
287000 S07-NOLLSTAELL-STRUKTURTABELL SECTION.                                   
287100                                                                          
287200     MOVE +1 TO TAB-INDX                                                  
287300     PERFORM UNTIL TAB-INDX > MAX-TABELL-LAENGD                           
287400       MOVE +0 TO STRUKTURNR(TAB-INDX)                                    
287500       ADD  +1 TO TAB-INDX                                                
287600     END-PERFORM                                                          
287700     .                                                                    
287800     EJECT                                                                
287900 S08-KOLLA-IDAO-O-TIAAVV-OBL SECTION.                                     
288000*********************************************                             
288100* KONTROLL ATT IDAO OCH TIAAVV ÄR IFYLLDA   *                             
288200*********************************************                             
288300                                                                          
288400     IF MID-IDAO = ALL '+' OR SPACE                                       
288500       MOVE NEJ                TO INDATA-SW                               
288600       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                           
288700     ELSE                                                                 
288800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-ATTR                         
288900     END-IF                                                               
289000                                                                          
289100     IF MID-TIAAVV = ALL '+'                                              
289200       MOVE NEJ               TO INDATA-SW                                
289300       MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                          
289400     ELSE                                                                 
289500       IF MID-TIAAVV NUMERIC AND MID-TIAAVV > 0                           
289600         PERFORM S12-KOLLA-TIAAVV                                         
289700       ELSE                                                               
289800         MOVE NEJ               TO INDATA-SW                              
289900         MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                        
290000       END-IF                                                             
290100     END-IF                                                               
290200     .                                                                    
290300     EJECT                                                                
290400 S09-VISA-BEART-KDBENHOM-KDSORT SECTION.                                  
290500                                                                          
290600**** HÄMTA KDSORT FRÅN ARTIKELREGISTRET                                   
290700     MOVE ART-KDSORT TO MOD-KDSORT(INDX)                                  
290800                                                                          
290900**** HÄMTA BENÄMNING + HOMONYMKOD FRÅN BENÄMNINGSREGISTRET                
291000     PERFORM IMS-GET-BENA-BENA01-BSEQ                                     
291100     IF SEGMENT-FINNS                                                     
291200       MOVE BENA01-BEN-KDHOMONYM TO MOD-KDBENHOM(INDX)                    
291300       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
291400       PERFORM IMS-GET-BENA-BENA11-BSEQ                                   
291500       IF SEGMENT-FINNS                                                   
291600         MOVE BENA11-TEXT-BEART TO MOD-BEART(INDX)                        
291700       ELSE                                                               
291800         MOVE MFS-RENSA-FAELT TO MOD-BEART(INDX)                          
291900       END-IF                                                             
292000     ELSE                                                                 
292100       MOVE MFS-RENSA-FAELT TO MOD-BEART(INDX)                            
292200                               MOD-KDBENHOM(INDX)                         
292300     END-IF                                                               
292400                                                                          
292500     .                                                                    
292600     EJECT                                                                
292700 S10-KOLLA-OM-SVARSFLAG-IFYLLDA SECTION.                                  
292800                                                                          
292900     MOVE NEJ TO SVARSFLAGGA-IFYLLD-SW                                    
293000                                                                          
293100     IF MID-AANGRA = ALL '+'  OR SPACE                                    
293200       CONTINUE                                                           
293300     ELSE                                                                 
293400       MOVE MFS-ROER-EJ-FAELT     TO MOD-AANGRA                           
293500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-AANGRA-ATTR                      
293600       IF MID-AANGRA = 'N'                                                
293700         CONTINUE                                                         
293800       ELSE                                                               
293900         MOVE JA                  TO SVARSFLAGGA-IFYLLD-SW                
294000       END-IF                                                             
294100     END-IF                                                               
294200                                                                          
294300     IF MID-BORTTAG = ALL '+' OR SPACE                                    
294400       CONTINUE                                                           
294500     ELSE                                                                 
294600       MOVE MFS-ROER-EJ-FAELT     TO MOD-BORTTAG                          
294700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BORTTAG-ATTR                     
294800       IF MID-BORTTAG = 'N'                                               
294900         CONTINUE                                                         
295000       ELSE                                                               
295100         MOVE JA                  TO SVARSFLAGGA-IFYLLD-SW                
295200       END-IF                                                             
295300     END-IF                                                               
295400                                                                          
295500     IF MID-KLAR = ALL '+' OR SPACE                                       
295600       CONTINUE                                                           
295700     ELSE                                                                 
295800       MOVE MFS-ROER-EJ-FAELT     TO MOD-KLAR                             
295900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KLAR-ATTR                        
296000       IF MID-KLAR = 'N'                                                  
296100         CONTINUE                                                         
296200       ELSE                                                               
296300         MOVE JA                  TO SVARSFLAGGA-IFYLLD-SW                
296400       END-IF                                                             
296500     END-IF                                                               
296600     .                                                                    
296700     EJECT                                                                
296800 S11-KOLLA-OM-IDAO-TIAA-AENDRAD SECTION.                                  
296900***************************************************************           
297000* KONTROLL OM IDAO OCH TIAAVV ÄNDRADE (DE ÄR ALLTID MODIFIED) *           
297100* ISÅDNA FALL SÄTT DE TILL 'ÄNDRADE'                          *           
297200***************************************************************           
297300                                                                          
297400     MOVE NEJ TO IDAO-OCH-TIAAVV-AENDRAD-SW                               
297500                                                                          
297600     MOVE '1151'    TO W-IDHTYP                                           
297700     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
297800     PERFORM IMS-GET-XXAZ-XXAZ01                                          
297900                                                                          
298000     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
298100     MOVE WS-BELEVART       TO W-BELEVART                                 
298200     MOVE WS-IDARTNR        TO W-IDARTNR                                  
298300     MOVE LOW-VALUE         TO W-LOW-VALUE                                
298400     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
298500     PERFORM IMS-GET-XXAZ-XXAZ11                                          
298600                                                                          
298700     IF (MID-IDAO = ALL '+')                                              
298800       CONTINUE                                                           
298900     ELSE                                                                 
299000       IF MID-IDAO = XXAZ11-1152-IDAO                                     
299100         CONTINUE                                                         
299200       ELSE                                                               
299300         MOVE JA TO IDAO-OCH-TIAAVV-AENDRAD-SW                            
299400       END-IF                                                             
299500     END-IF                                                               
299600                                                                          
299700     IF (MID-TIAAVV = ALL '+')                                            
299800       CONTINUE                                                           
299900     ELSE                                                                 
300000       IF MID-TIAAVV NUMERIC AND MID-TIAAVV > 0                           
300100         MOVE 'AAVV'     TO DAT-KDDATFORM                                 
300200         MOVE MID-TIAAVV TO DAT-I-TIDATUM                                 
300300         CALL WDATKONV USING DAT-KDDATFORM                                
300400                             DAT-I-TIDATUM                                
300500                             DAT-O-TIDATUM                                
300600                             DAT-KDSVAR                                   
300700         IF DAT-KDSVAR-OK                                                 
300800           IF DAT-TIAAMMDD = XXAZ11-1152-TISTODAT                         
300900             CONTINUE                                                     
301000           ELSE                                                           
301100             MOVE JA TO IDAO-OCH-TIAAVV-AENDRAD-SW                        
301200           END-IF                                                         
301300         ELSE                                                             
301400           MOVE JA TO IDAO-OCH-TIAAVV-AENDRAD-SW                          
301500         END-IF                                                           
301600       ELSE                                                               
301700         MOVE JA TO IDAO-OCH-TIAAVV-AENDRAD-SW                            
301800       END-IF                                                             
301900     END-IF                                                               
302000     .                                                                    
302100     EJECT                                                                
302200 S12-KOLLA-TIAAVV SECTION.                                                
302300                                                                          
302400     MOVE 'AAVV'         TO DAT-KDDATFORM                                 
302500     MOVE MID-TIAAVV     TO DAT-I-TIDATUM                                 
302600     CALL WDATKONV USING DAT-KDDATFORM                                    
302700                         DAT-I-TIDATUM                                    
302800                         DAT-O-TIDATUM                                    
302900                         DAT-KDSVAR                                       
303000     IF DAT-KDSVAR-OK                                                     
303100       MOVE DAT-TIAAVV-GRP      TO WS-TIAAVV-GRP                          
303200       MOVE WS-TIAAVV-GRP       TO TMP1-YYWW                              
303300       MOVE DAGENS-DATUM-AAVV   TO TMP2-YYWW                              
303400       PERFORM WY2000P3                                                   
303500       IF TMP1-YYWW < TMP2-YYWW                                           
303600         MOVE NEJ               TO INDATA-SW                              
303700         MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                        
303800       ELSE                                                               
303900         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-ATTR                      
304000         IF DAT-TIAAVV-GRP = DAGENS-DATUM-AAVV                            
304100           MOVE DAGENS-DATUM     TO WS-TISTODAT                           
304200         ELSE                                                             
304300           MOVE DAT-TIAAMMDD-GRP TO WS-TISTODAT                           
304400         END-IF                                                           
304500       END-IF                                                             
304600     ELSE                                                                 
304700       MOVE NEJ               TO INDATA-SW                                
304800       MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                          
304900     END-IF                                                               
305000     .                                                                    
305100     EJECT                                                                
305200 S13-RENSA-ELLER-LAES-RAD-IGEN SECTION.                                   
305300********************************************************                  
305400* OM RAD EJ IFYLLD RENSAS DEN ANNARS LÄSES DEN IN IGEN *                  
305500********************************************************                  
305600                                                                          
305700     IF MID-RAD-1-IFYLLD                                                  
305800       MOVE +1 TO INDX                                                    
305900       PERFORM MFS-LAES-IN-RAD-IGEN                                       
306000     ELSE                                                                 
306100       MOVE +1 TO INDX                                                    
306200       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
306300       PERFORM MFS-FORMATETS-ATTR-RAD                                     
306400     END-IF                                                               
306500                                                                          
306600     IF MID-RAD-2-IFYLLD                                                  
306700       MOVE +2 TO INDX                                                    
306800       PERFORM MFS-LAES-IN-RAD-IGEN                                       
306900     ELSE                                                                 
307000       MOVE +2 TO INDX                                                    
307100       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
307200       PERFORM MFS-FORMATETS-ATTR-RAD                                     
307300     END-IF                                                               
307400     .                                                                    
307500     EJECT                                                                
307600 S14-AENDRA-IDAO-O-TIAAVV SECTION.                                        
307700********************************************                              
307800*   ÄNDRING AV IDAO OCH/ELLER TIAAVV       *                              
307900********************************************                              
308000                                                                          
308100     MOVE '1151'    TO W-IDHTYP                                           
308200     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
308300     PERFORM IMS-GET-XXAZ-XXAZ01                                          
308400                                                                          
308500     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
308600     MOVE WS-BELEVART       TO W-BELEVART                                 
308700     MOVE WS-IDARTNR        TO W-IDARTNR                                  
308800     MOVE LOW-VALUE         TO W-LOW-VALUE                                
308900     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
309000     PERFORM IMS-GET-XXAZ-XXAZ11                                          
309100     IF MID-IDAO = ALL '+'                                                
309200       MOVE SPACE    TO XXAZ11-1152-IDAO                                  
309300     ELSE                                                                 
309400       MOVE MID-IDAO TO XXAZ11-1152-IDAO                                  
309500     END-IF                                                               
309600                                                                          
309700     IF MID-TIAAVV = ALL '+'                                              
309800       MOVE ZERO        TO XXAZ11-1152-TISTODAT                           
309900     ELSE                                                                 
310000       MOVE WS-TISTODAT TO XXAZ11-1152-TISTODAT                           
310100     END-IF                                                               
310200     PERFORM IMS-REPL-XXAZ                                                
310300     .                                                                    
310400     EJECT                                                                
310500 S15-KOLLA-REANTPSA SECTION.                                              
310600                                                                          
310700     MOVE MID-REANTPSA(INDX) TO DEC-IDFRIDATA                             
310800     MOVE +2                 TO DEC-KVHELTAL                              
310900     MOVE +3                 TO DEC-KVDECIMAL                             
311000     CALL WDECEDIT USING DEC-WDECAREA                                     
311100     IF DEC-KDSVAR-OK                                                     
311200       IF DEC-IDEDITDATA > 0                                              
311300         MOVE MFS-NUM-FAELT-RAETT TO MOD-REANTPSA-ATTR(INDX)              
311400       ELSE                                                               
311500         MOVE NEJ               TO INDATA-SW                              
311600         MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)                
311700       END-IF                                                             
311800     ELSE                                                                 
311900       MOVE NEJ               TO INDATA-SW                                
312000       MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-ATTR(INDX)                  
312100     END-IF                                                               
312200     .                                                                    
312300     EJECT                                                                
312400 S16-KOLLA-ATT-RADER-E-MAERKTA SECTION.                                   
312500************************************                                      
312600* KONTROLL ATT VALDA RADER PÅ RASA *                                      
312700* E-MÄRKTA OM ARTIKEL ALT ERSATT   *                                      
312800************************************                                      
312900                                                                          
313000     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
313100                                                                          
313200     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
313300     PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                       
313400       IF SEGMENT-FINNS                                                   
313500         MOVE 001                 TO WORK-KDCALL                          
313600         MOVE WC-CDC-SE           TO WORK-IDDC                            
313700         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
313800         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
313900         CALL WORKDAY USING WORK-KDCALL                                   
314000                            WORK-DATE-AREA                                
314100                            WORK-KDSVAR                                   
314200         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
314300*********  OM MAN TRÄFFAR PÅ EN KONV. STRUKTUR                            
314400*********  ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                                 
314500*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
314600           MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
314700           PERFORM IMS-GET-SATB-SATB01                                    
314800           PERFORM IMS-DLET-SATB                                          
314900                                                                          
315000           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
315100         ELSE                                                             
315200           MOVE SATB01-STR-IDARTNR TO W-IDARTNR                           
315300           PERFORM IMS-GET-SATB-SATB01                                    
315400           PERFORM IMS-GET-SATB-SATB11-OKVAL                              
315500           IF SATB11-RAD-KDISATS = 'E'                                    
315600             PERFORM IMS-GET-SATB-DSEQ-NEXT                               
315700           ELSE                                                           
315800             MOVE NEJ TO INDATA-SW                                        
315900           END-IF                                                         
316000         END-IF                                                           
316100       END-IF                                                             
316200     END-PERFORM                                                          
316300     .                                                                    
316400     EJECT                                                                
316500 S17-LYS-UPP-INMATADE-FAELT SECTION.                                      
316600                                                                          
316700     IF MID-AANGRA = ALL '+'                                              
316800       CONTINUE                                                           
316900     ELSE                                                                 
317000       MOVE MFS-NUM-FAELT-FEL TO MOD-AANGRA-ATTR                          
317100     END-IF                                                               
317200                                                                          
317300     IF MID-BORTTAG = ALL '+' OR SPACE                                    
317400       CONTINUE                                                           
317500     ELSE                                                                 
317600       MOVE MFS-ALFA-FAELT-FEL TO MOD-BORTTAG-ATTR                        
317700     END-IF                                                               
317800                                                                          
317900     IF MID-KLAR = ALL '+'                                                
318000       CONTINUE                                                           
318100     ELSE                                                                 
318200       MOVE MFS-NUM-FAELT-FEL TO MOD-KLAR-ATTR                            
318300     END-IF                                                               
318400                                                                          
318500     IF MID-IDAO = ALL '+' OR SPACE                                       
318600       CONTINUE                                                           
318700     ELSE                                                                 
318800       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR                           
318900     END-IF                                                               
319000                                                                          
319100     IF MID-TIAAVV = ALL '+'                                              
319200       CONTINUE                                                           
319300     ELSE                                                                 
319400       MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-ATTR                          
319500     END-IF                                                               
319600                                                                          
319700     PERFORM S02-LYS-UPP-INMATADE-RADFAELT                                
319800     .                                                                    
319900     EJECT                                                                
320000 S18-LAS-BEART-GB SECTION.                                                
320100                                                                          
320200     MOVE SPACE                 TO WS-BEART-GB                            
320300     MOVE 'S  '                 TO W-IDSKYLT                              
320400     PERFORM IMS-GET-BENA-BENA01-ASEQ-UNIK                                
320500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
320600             WS-KDBENHOM = BENA01-BEN-KDHOMONYM                           
320700        IF SEGMENT-FINNS                                                  
320800           IF WS-KDBENHOM = BENA01-BEN-KDHOMONYM                          
320900              CONTINUE                                                    
321000           ELSE                                                           
321100              PERFORM IMS-GET-BENA-BENA01-ASEQ                            
321200           END-IF                                                         
321300        END-IF                                                            
321400     END-PERFORM                                                          
321500                                                                          
321600     IF SEGMENT-FINNS                                                     
321700        MOVE 'GB ' TO W-IDSKYLT                                           
321800        PERFORM IMS-GET-BENA-BENA11-ASEQ                                  
321900        IF SEGMENT-FINNS                                                  
322000           MOVE BENA11-TEXT-BEART TO WS-BEART-GB                          
322100        END-IF                                                            
322200     END-IF                                                               
322300     .                                                                    
322400     EJECT                                                                
322500 S19-LAS-BEART-SVE SECTION.                                               
322600                                                                          
322700     MOVE SPACE                 TO WS-BEART-SVE                           
322800     MOVE 'GB '                 TO W-IDSKYLT                              
322900     PERFORM IMS-GET-BENA-BENA01-ASEQ-UNIK                                
323000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
323100             WS-KDBENHOM = BENA01-BEN-KDHOMONYM                           
323200        IF SEGMENT-FINNS                                                  
323300           IF WS-KDBENHOM = BENA01-BEN-KDHOMONYM                          
323400              CONTINUE                                                    
323500           ELSE                                                           
323600              PERFORM IMS-GET-BENA-BENA01-ASEQ                            
323700           END-IF                                                         
323800        END-IF                                                            
323900     END-PERFORM                                                          
324000                                                                          
324100     IF SEGMENT-FINNS                                                     
324200        MOVE 'S  ' TO W-IDSKYLT                                           
324300        PERFORM IMS-GET-BENA-BENA11-ASEQ                                  
324400        IF SEGMENT-FINNS                                                  
324500           MOVE BENA11-TEXT-BEART TO WS-BEART-SVE                         
324600        END-IF                                                            
324700     END-IF                                                               
324800     .                                                                    
324900     EJECT                                                                
325000 MFS-RENSA-FAELT-UT SECTION.                                              
325100                                                                          
325200*    --- ALLA UTDATA-FÄLT                                                 
325300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
325400     MOVE MFS-RENSA-FAELT TO MOD-BEART-UTG-ART                            
325500                             MOD-ANTAL-SEGMENT-ENTER                      
325600                             MOD-ANTAL-SEGMENT-NEXT                       
325700     .                                                                    
325800     SKIP2                                                                
325900 MFS-RENSA-FAELT-IN SECTION.                                              
326000                                                                          
326100*    --- ALLA INDATA-FÄLT                                                 
326200     MOVE MFS-RENSA-FAELT TO MOD-AANGRA                                   
326300                             MOD-IDAO                                     
326400                             MOD-BORTTAG                                  
326500                             MOD-TIAAVV                                   
326600                             MOD-KLAR                                     
326700                                                                          
326800     MOVE +1 TO INDX                                                      
326900     PERFORM UNTIL INDX > MAX-INDX                                        
327000       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
327100       ADD 1 TO INDX                                                      
327200     END-PERFORM                                                          
327300     .                                                                    
327400     EJECT                                                                
327500 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
327600                                                                          
327700*    --- INDATA-FÄLT PÅ BLÄDDRINGSRADEN                                   
327800                                                                          
327900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (INDX)                           
328000                             MOD-BELEVART (INDX)                          
328100                             MOD-REANTPSA (INDX)                          
328200                             MOD-KDSORT (INDX)                            
328300                             MOD-BEART (INDX)                             
328400                             MOD-KDBENHOM (INDX)                          
328500                             MOD-IDSTRTYP (INDX)                          
328600                             MOD-TESTRNOT (INDX, 1)                       
328700                             MOD-TESTRNOT (INDX, 2)                       
328800     .                                                                    
328900     EJECT                                                                
329000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
329100                                                                          
329200*    --- ALLA UTDATA-FÄLT                                                 
329300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
329400     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-UTG-ART                          
329500                               MOD-ANTAL-SEGMENT-ENTER                    
329600                               MOD-ANTAL-SEGMENT-NEXT                     
329700                               MOD-IDAO                                   
329800                               MOD-TIAAVV                                 
329900                                                                          
330000     .                                                                    
330100     SKIP2                                                                
330200 MFS-ROR-EJ-FAELT-IN SECTION.                                             
330300     MOVE MFS-ROER-EJ-FAELT TO MOD-AANGRA                                 
330400                               MOD-IDAO                                   
330500                               MOD-BORTTAG                                
330600                               MOD-TIAAVV                                 
330700                               MOD-KLAR                                   
330800     MOVE +1 TO INDX                                                      
330900     PERFORM UNTIL INDX > MAX-INDX                                        
331000       PERFORM MFS-ROR-EJ-RAD-FAELT-IN                                    
331100       ADD 1 TO INDX                                                      
331200     END-PERFORM                                                          
331300     .                                                                    
331400     SKIP2                                                                
331500 MFS-ROR-EJ-RAD-FAELT-IN  SECTION.                                        
331600                                                                          
331700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR (INDX)                         
331800                               MOD-BELEVART (INDX)                        
331900                               MOD-REANTPSA (INDX)                        
332000                               MOD-KDSORT (INDX)                          
332100                               MOD-BEART (INDX)                           
332200                               MOD-KDBENHOM (INDX)                        
332300                               MOD-IDSTRTYP (INDX)                        
332400                               MOD-TESTRNOT (INDX, 1)                     
332500                               MOD-TESTRNOT (INDX, 2)                     
332600     .                                                                    
332700     EJECT                                                                
332800 MFS-STAENG-FAELT-IN SECTION.                                             
332900                                                                          
333000     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-AANGRA-ATTR                       
333100                                    MOD-IDAO-ATTR                         
333200                                    MOD-BORTTAG-ATTR                      
333300                                    MOD-TIAAVV-ATTR                       
333400                                    MOD-KLAR-ATTR                         
333500     MOVE +1 TO INDX                                                      
333600     PERFORM UNTIL INDX > MAX-INDX                                        
333700       PERFORM MFS-STAENG-RAD-FAELT-IN                                    
333800       ADD 1 TO INDX                                                      
333900     END-PERFORM                                                          
334000     .                                                                    
334100     SKIP2                                                                
334200 MFS-STAENG-RAD-FAELT-IN  SECTION.                                        
334300                                                                          
334400     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDLEVNR-ATTR (INDX)               
334500                                    MOD-BELEVART-ATTR (INDX)              
334600                                    MOD-REANTPSA-ATTR (INDX)              
334700                                    MOD-KDSORT-ATTR (INDX)                
334800                                    MOD-BEART-ATTR (INDX)                 
334900                                    MOD-KDBENHOM-ATTR (INDX)              
335000                                    MOD-IDSTRTYP-ATTR (INDX)              
335100                                    MOD-TESTRNOT-ATTR (INDX, 1)           
335200                                    MOD-TESTRNOT-ATTR (INDX, 2)           
335300     .                                                                    
335400     EJECT                                                                
335500 MFS-FORMATETS-ATTR-RAD SECTION.                                          
335600                                                                          
335700     MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-ATTR (INDX)                   
335800                                MOD-BELEVART-ATTR (INDX)                  
335900                                MOD-REANTPSA-ATTR (INDX)                  
336000                                MOD-KDSORT-ATTR (INDX)                    
336100                                MOD-BEART-ATTR (INDX)                     
336200                                MOD-KDBENHOM-ATTR (INDX)                  
336300                                MOD-IDSTRTYP-ATTR (INDX)                  
336400                                MOD-TESTRNOT-ATTR (INDX, 1)               
336500                                MOD-TESTRNOT-ATTR (INDX, 2)               
336600     .                                                                    
336700     SKIP2                                                                
336800 MFS-LAES-IN-RAD-IGEN SECTION.                                            
336900                                                                          
337000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR (INDX)                
337100                                   MOD-BELEVART-ATTR (INDX)               
337200                                   MOD-REANTPSA-ATTR (INDX)               
337300                                   MOD-KDSORT-ATTR (INDX)                 
337400                                   MOD-BEART-ATTR (INDX)                  
337500                                   MOD-KDBENHOM-ATTR (INDX)               
337600                                   MOD-IDSTRTYP-ATTR (INDX)               
337700                                   MOD-TESTRNOT-ATTR (INDX, 1)            
337800                                   MOD-TESTRNOT-ATTR (INDX, 2)            
337900     .                                                                    
338000     EJECT                                                                
338100* --- IMS SEKTIONER ---                                                   
338200     SKIP3                                                                
338300 IMS-GET-MSG SECTION.                                                     
338400                                                                          
338500     MOVE '  QC' TO GODK-STATUSKODER                                      
338600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
338700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
338800     PERFORM IMS-STATUSKONTROLL                                           
338900     .                                                                    
339000     SKIP3                                                                
339100 IMS-INSERT-MSG SECTION.                                                  
339200                                                                          
339300     IF NOT ENGLISH-TEXT                                                  
339400       MOVE '0' TO MFS-KDHUVOMR                                           
339500     END-IF                                                               
339600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
339700     MOVE SPACE TO GODK-STATUSKODER                                       
339800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
339900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
340000     PERFORM IMS-STATUSKONTROLL                                           
340100     .                                                                    
340200     SKIP3                                                                
340300 IMS-INSERT-ALT-MSG SECTION.                                              
340400                                                                          
340500     MOVE SPACE TO GODK-STATUSKODER                                       
340600     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
340700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
340800     PERFORM IMS-STATUSKONTROLL                                           
340900     .                                                                    
341000     EJECT                                                                
341100 IMS-DLET-SATB SECTION.                                                   
341200                                                                          
341300     MOVE '  ' TO GODK-STATUSKODER                                        
341400     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
341500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
341600     PERFORM IMS-STATUSKONTROLL                                           
341700     .                                                                    
341800     SKIP3                                                                
341900 IMS-GET-SATB-CSEQ-UNIK SECTION.                                          
342000                                                                          
342100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
342200                                    W-BELEVART-X                          
342300                                    W-IDARTNR-X ')'                       
342400          DELIMITED BY SIZE INTO SSA1                                     
342500     MOVE '  GE' TO GODK-STATUSKODER                                      
342600     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA SSA1                    
342700     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
342800     PERFORM IMS-STATUSKONTROLL                                           
342900     .                                                                    
343000     SKIP3                                                                
343100*IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
343200*                                                                         
343300*    STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
343400*                                   W-BELEVART-X                          
343500*                                   W-IDARTNR-X ')'                       
343600*         DELIMITED BY SIZE INTO SSA1                                     
343700*    MOVE '  GE' TO GODK-STATUSKODER                                      
343800*    CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA SSA1                    
343900*    MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
344000*    PERFORM IMS-STATUSKONTROLL                                           
344100*    .                                                                    
344200     SKIP3                                                                
344300 IMS-GET-SATB-DSEQ-UNIK SECTION.                                          
344400**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
344500                                                                          
344600     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
344700                                  W-MIN-IDARTNR-KONV-X                    
344800                    '&WDJ1DSEQ<=' W-IDUSER-X                              
344900                                  W-MAX-IDARTNR-KONV-X ')'                
345000          DELIMITED BY SIZE INTO SSA1                                     
345100     MOVE '  GE' TO GODK-STATUSKODER                                      
345200     CALL CBLTDLI USING GU SATB-D-PCB DLI-IO-AREA SSA1                    
345300     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
345400     PERFORM IMS-STATUSKONTROLL                                           
345500     .                                                                    
345600     SKIP3                                                                
345700 IMS-GET-SATB-DSEQ-NEXT SECTION.                                          
345800**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
345900                                                                          
346000     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
346100                                  W-MIN-IDARTNR-KONV-X                    
346200                    '&WDJ1DSEQ<=' W-IDUSER-X                              
346300                                  W-MAX-IDARTNR-KONV-X ')'                
346400          DELIMITED BY SIZE INTO SSA1                                     
346500     MOVE '  GE' TO GODK-STATUSKODER                                      
346600     CALL CBLTDLI USING GN SATB-D-PCB DLI-IO-AREA SSA1                    
346700     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
346800     PERFORM IMS-STATUSKONTROLL                                           
346900     .                                                                    
347000     SKIP3                                                                
347100 IMS-GET-SATB-SATB01 SECTION.                                             
347200                                                                          
347300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
347400          DELIMITED BY SIZE INTO SSA1                                     
347500     MOVE '  GE' TO GODK-STATUSKODER                                      
347600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
347700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
347800     PERFORM IMS-STATUSKONTROLL                                           
347900     .                                                                    
348000     SKIP3                                                                
348100 IMS-GET-SATB-SATB11-OKVAL SECTION.                                       
348200                                                                          
348300     MOVE 'WLSATB11 ' TO SSA1                                             
348400     MOVE '  GE' TO GODK-STATUSKODER                                      
348500     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
348600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
348700     PERFORM IMS-STATUSKONTROLL                                           
348800     .                                                                    
348900     EJECT                                                                
349000 IMS-GET-ARTC-ARTC01 SECTION.                                             
349100                                                                          
349200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
349300          DELIMITED BY SIZE INTO SSA1                                     
349400     MOVE '  GE' TO GODK-STATUSKODER                                      
349500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
349600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
349700     PERFORM IMS-STATUSKONTROLL                                           
349800     .                                                                    
349900     SKIP3                                                                
350000 IMS-GET-ARTC-ARTC11 SECTION.                                             
350100                                                                          
350200     MOVE 'WLARTC11 ' TO SSA1                                             
350300     MOVE '  GE' TO GODK-STATUSKODER                                      
350400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
350500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
350600     PERFORM IMS-STATUSKONTROLL                                           
350700     .                                                                    
350800     EJECT                                                                
350900 IMS-GET-ERSA-ERSA01 SECTION.                                             
351000                                                                          
351100     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
351200          DELIMITED BY SIZE INTO SSA1                                     
351300     MOVE '  GE' TO GODK-STATUSKODER                                      
351400     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
351500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
351600     PERFORM IMS-STATUSKONTROLL                                           
351700     .                                                                    
351800     EJECT                                                                
351900 IMS-GET-ERSA-ERSA11 SECTION.                                             
352000                                                                          
352100     MOVE 'WLERSA11' TO SSA1                                              
352200     MOVE '  GE' TO GODK-STATUSKODER                                      
352300     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
352400     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
352500     PERFORM IMS-STATUSKONTROLL                                           
352600     .                                                                    
352700     EJECT                                                                
352800 IMS-GET-BENA-BENA01-ASEQ SECTION.                                        
352900                                                                          
353000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
353100                                  W-BEART-X ')'                           
353200          DELIMITED BY SIZE INTO SSA1                                     
353300     MOVE '  GE' TO GODK-STATUSKODER                                      
353400     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
353500     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
353600     PERFORM IMS-STATUSKONTROLL                                           
353700     .                                                                    
353800     SKIP3                                                                
353900 IMS-GET-BENA-BENA01-ASEQ-UNIK SECTION.                                   
354000                                                                          
354100     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
354200                                  W-BEART-X ')'                           
354300          DELIMITED BY SIZE INTO SSA1                                     
354400     MOVE '  GE' TO GODK-STATUSKODER                                      
354500     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
354600     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
354700     PERFORM IMS-STATUSKONTROLL                                           
354800     .                                                                    
354900     SKIP3                                                                
355000 IMS-GET-BENA-BENA11-ASEQ SECTION.                                        
355100                                                                          
355200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
355300          DELIMITED BY SIZE INTO SSA1                                     
355400     MOVE '  GE' TO GODK-STATUSKODER                                      
355500     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
355600     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
355700     PERFORM IMS-STATUSKONTROLL                                           
355800     .                                                                    
355900     SKIP3                                                                
356000 IMS-GET-BENA-BENA01-BSEQ SECTION.                                        
356100                                                                          
356200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
356300          DELIMITED BY SIZE INTO SSA1                                     
356400     MOVE '  GE' TO GODK-STATUSKODER                                      
356500     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
356600     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
356700     PERFORM IMS-STATUSKONTROLL                                           
356800     .                                                                    
356900     SKIP3                                                                
357000 IMS-GET-BENA-BENA11-BSEQ SECTION.                                        
357100                                                                          
357200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
357300          DELIMITED BY SIZE INTO SSA1                                     
357400     MOVE '  GE' TO GODK-STATUSKODER                                      
357500     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
357600     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
357700     PERFORM IMS-STATUSKONTROLL                                           
357800     .                                                                    
357900     EJECT                                                                
358000 IMS-DLET-XXAZ11 SECTION.                                                 
358100                                                                          
358200     MOVE '  ' TO GODK-STATUSKODER                                        
358300     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA                         
358400     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
358500     PERFORM IMS-STATUSKONTROLL                                           
358600     .                                                                    
358700     SKIP3                                                                
358800 IMS-DLET-XXAZ21 SECTION.                                                 
358900                                                                          
359000     MOVE '  ' TO GODK-STATUSKODER                                        
359100     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA2                        
359200     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
359300     PERFORM IMS-STATUSKONTROLL                                           
359400     .                                                                    
359500     SKIP3                                                                
359600 IMS-REPL-XXAZ SECTION.                                                   
359700                                                                          
359800     MOVE '  ' TO GODK-STATUSKODER                                        
359900     CALL CBLTDLI USING REPL XXAZ-PCB DLI-IO-AREA                         
360000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
360100     PERFORM IMS-STATUSKONTROLL                                           
360200     .                                                                    
360300     SKIP3                                                                
360400 IMS-GET-XXAZ-XXAZ01 SECTION.                                             
360500                                                                          
360600     STRING 'WLXXAZ01(WDGXKEY  =' W-IDHTYP-X                              
360700                                  W-LOW-VALUE-2 ')'                       
360800          DELIMITED BY SIZE INTO SSA1                                     
360900     MOVE '    ' TO GODK-STATUSKODER                                      
361000     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA SSA1                      
361100     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
361200     PERFORM IMS-STATUSKONTROLL                                           
361300     .                                                                    
361400     SKIP3                                                                
361500 IMS-GET-XXAZ-XXAZ11 SECTION.                                             
361600                                                                          
361700     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
361800                                  W-BELEVART-X                            
361900                                  W-IDARTNR-X                             
362000                                  W-IDUSER-X                              
362100                                  W-LOW-VALUE-X ')'                       
362200          DELIMITED BY SIZE INTO SSA1                                     
362300     MOVE '  GE' TO GODK-STATUSKODER                                      
362400     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
362500     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
362600     PERFORM IMS-STATUSKONTROLL                                           
362700     .                                                                    
362800     SKIP3                                                                
362900 IMS-GET-XXAZ-XXAZ21 SECTION.                                             
363000                                                                          
363100     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
363200                                  W-BELEVART-X                            
363300                                  W-IDARTNR-X                             
363400                                  W-IDUSER-X                              
363500                                  W-LOW-VALUE-X ')'                       
363600          DELIMITED BY SIZE INTO SSA1                                     
363700     MOVE 'WLXXAZ21 ' TO SSA2                                             
363800     MOVE '  GE' TO GODK-STATUSKODER                                      
363900     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA2 SSA1 SSA2              
364000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
364100     PERFORM IMS-STATUSKONTROLL                                           
364200     .                                                                    
364300     SKIP3                                                                
364400 IMS-ISRT-XXAZ-XXAZ21 SECTION.                                            
364500                                                                          
364600     STRING 'WLXXAZ01(WDGXKEY  =' W-IDHTYP-X                              
364700                                  W-LOW-VALUE-2 ')'                       
364800          DELIMITED BY SIZE INTO SSA1                                     
364900     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
365000                                  W-BELEVART-X                            
365100                                  W-IDARTNR-X                             
365200                                  W-IDUSER-X                              
365300                                  W-LOW-VALUE-X ')'                       
365400          DELIMITED BY SIZE INTO SSA2                                     
365500     MOVE 'WLXXAZ21 ' TO SSA3                                             
365600     MOVE '  II' TO GODK-STATUSKODER                                      
365700     CALL CBLTDLI USING ISRT XXAZ-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3         
365800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
365900     PERFORM IMS-STATUSKONTROLL                                           
366000     .                                                                    
366100     EJECT                                                                
366200 IMS-STATUSKONTROLL SECTION.                                              
366300                                                                          
366400     SET STATUS-IX TO 1                                                   
366500     SEARCH GODK-STATUS                                                   
366600       AT END CALL FELLOG                                                 
366700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
366800     END-SEARCH                                                           
366900     .                                                                    
367000     EJECT                                                                
367100*    -COPY WY2000P1                                                       
367200     EJECT                                                                
367300*    -COPY WY2000P3                                                       
