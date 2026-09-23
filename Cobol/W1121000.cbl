000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W1121000.                                                 
000300 AUTHOR.        THOMAS LARSSON.                                           
000400 DATE-WRITTEN.  JUNI -90.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*               UPPDATERAR DAGLIGEN RASA ( 1002-SATSER )                  
000900*                                                                         
001000*               MED HJÄLP AV                                              
001100*                                                                         
001200*               TRANS 2303-OR FRÅN HÄNDELSEREG VARVID                     
001300*               ERSÄTTNINGAR HÄMTAS FRÅN ERSÄTTNINGSREG                   
001400*                                                                         
001500*               TRANS 2301-OR FRÅN HÄNDELSEREG LEVERANTÖRSBYTE            
001600*                                                                         
001700*               SAMT SKAPAR TRANS 2234 ( SATSSTRUKTUR UPPDATERAD          
001800*               MAP INGÅENDE ARTIKEL )                                    
001900*                                                                         
002000*               LÄSER :                                                   
002100*                     RASA                 WLSATB  (WDJ1)                 
002200*                     ARTIKELREGISTRET     WLARTC  (WDK6)                 
002300*                     ERSÄTTNINGSREGISTRET WLERSA  (WDD7)                 
002400*                     HÄNDELSEREGISTRET    WLXX    (WDG3)                 
002500*                                          WLINLB  (WDD9)                 
002600*                     NYPONREGISTRET       WLARTG  (WDD2)                 
002700*                                                                         
002800*               UPPDATERAR :                                              
002900*                     RASA                 WLSATB  (WDJ1)                 
003000*                     ARTIKELREGISTRET     WLARTC  (WDK6)                 
003100*                     HÄNDELSEBAS          WLXXBY  (WDR5)                 
003200*                                                                         
003300*                     UTFIL : W11201 FEL- OCH VARNINGSLISTA               
003400*                                          WLFILC  (WDR3)                 
003500*                                                                         
003600*     ÄNDRING: I SAMBAND MED EVEREST-PROJEKTET (IDLEVNR) ÄNDRAS           
003700*              OCKSÅ SEG-NAMN OCH CTX-NAMN FÖR.                           
003800*              GAMLA 2301-TRANSEN (WLXXBT)                                
003900*                  BYTER TILL HTYP-2303 MED DATA PÅ WDGX2304              
004000*              GAMLA 2302-TRANSEN (WLXXBO)                                
004100*                  BYTER TILL HTYP-2301 MED DATA PÅ WDGX2302              
004200*                                                                         
004300*              MAJ 2012  NYCKEL WDD901 UTÖKAD MED IDDC                    
004400*                                                                         
004500*                                                                         
004600*************************** KOMMENTAR  ************************           
004700*                                                                         
004800*                ANLEDNING TILL ATT DET FINNS FLERA LIKADANA              
004900*                PCB:ER BEROR PÅ ATT DET ÄR NÖDVÄNDIGT FÖR ATT            
005000*                MAN INTE SKA FÖRLORA POSITIONEN DÄR MAN BE-              
005100*                FINNER SIG VID NÄSTA LÄSNING                             
005200*                                                                         
005300                                                                          
005400 ENVIRONMENT DIVISION.                                                    
005500                                                                          
005600 INPUT-OUTPUT SECTION.                                                    
005700                                                                          
005800 FILE-CONTROL.                                                            
005900                                                                          
006000 DATA DIVISION.                                                           
006100                                                                          
006200 FILE SECTION.                                                            
006300                                                                          
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600*    -COPY WY2000W3                                                       
006700                                                                          
006800*    -COPY WY2000W2                                                       
006900                                                                          
007000*    -COPY WY2000W1                                                       
007100                                                                          
007200*    -COPY WY2000W9                                                       
007300     SKIP3                                                                
007400* - - - - - - - - - - - - - - - - - - KONSTANTER                          
007500*                                                                         
007600                                                                          
007700 77  IDPGM                   PIC X(8)        VALUE 'W1121100'.            
007800 77  JA                      PIC X           VALUE 'J'.                   
007900 77  NEJ                     PIC X           VALUE 'N'.                   
008000 77  MASKINELL               PIC X(9)        VALUE 'MASKINELL'.           
008100 77  EK01-IX                 PIC S9(3)       VALUE +0.                    
008200 01  CHKP-VAR.                                                            
008300 03  CHKP-MSG-IO-AREA-LENGTH PIC S9(9)   VALUE +32 COMP SYNC.             
008400 03  CHKP-MSG-IO-AREA        PIC X(32)   VALUE SPACE.                     
008500 03  CHKP-AREA-LENGTH        PIC S9(9)   VALUE +32 COMP SYNC.             
008600 03  CHKP-AREA               PIC X(32)   VALUE SPACE.                     
008700 03  CHKP-ANT                PIC S9(5)   VALUE +0.                        
008800 03  CHKP-MAX                PIC S9(5)   VALUE +5.                        
008900     SKIP2                                                                
009000 01  FELTEXT.                                                             
009100     03  FILLER              PIC X(8)    VALUE 'FELTEXT'.                 
009200     03  FELTEXT-STR         PIC X(72)   VALUE SPACE.                     
009300 01  W-TID                   PIC 9(8)   VALUE ZERO.                       
009400                                                                          
009500 01  KOD-SW                  PIC X.                                       
009600     88  BEHANDLINGSBAR-KOD                  VALUE 'J'.                   
009700                                                                          
009800 01  TIDBER-SW               PIC X.                                       
009900     88  TIDBER-KLAR                         VALUE 'J'.                   
010000     88  TIDBER-EJ-KLAR                      VALUE 'N'.                   
010100                                                                          
010200 01  RAD-SW                  PIC X.                                       
010300     88  RAD-FINNS                           VALUE 'J'.                   
010400     88  RAD-FINNS-EJ                        VALUE 'N'.                   
010500                                                                          
010600 01  STRUKTURNR-FINNS-I-TABELL-SW PIC X.                                  
010700     88  STRUKTURNR-FINNS-I-TABELL           VALUE 'J'.                   
010800                                                                          
010900 01  BEARBETNING-SW          PIC X.                                       
011000     88  BEARBETNING-OK                      VALUE 'J'.                   
011100     88  BEARBETNING-EJ-OK                   VALUE 'N'.                   
011200                                                                          
011300 01  BEARBETNINGS-KOLL-SW    PIC X.                                       
011400     88  BEARBETNING-KOLL-OK                 VALUE 'J'.                   
011500     88  BEARBETNING-KOLL-EJ-OK              VALUE 'N'.                   
011600                                                                          
011700 01  INDATA-SW               PIC X.                                       
011800     88  INDATA-OK                           VALUE 'J'.                   
011900     88  INDATA-FEL                          VALUE 'N'.                   
012000                                                                          
012100 01  BORTTAGNINGS-SW         PIC X.                                       
012200     88  BORTTAGNING-OK                      VALUE 'J'.                   
012300     88  BORTTAGNING-EJ-OK                   VALUE 'N'.                   
012400                                                                          
012500 01  BEARB-SW                PIC X.                                       
012600     88  BEARB-OK                            VALUE 'J'.                   
012700     88  BEARB-EJ-OK                         VALUE 'N'.                   
012800                                                                          
012900 01  KONV-SW                 PIC X.                                       
013000     88  KONV-FINNS                          VALUE 'J'.                   
013100     88  KONV-FINNS-EJ                       VALUE 'N'.                   
013200                                                                          
013300 01  ERS-ART-SW              PIC X.                                       
013400     88  ERS-TILLK-FINNS                     VALUE 'J'.                   
013500     88  ERS-TILLK-FINNS-EJ                  VALUE 'N'.                   
013600                                                                          
013700 01  TILLK-ARTIKEL-FORPACKNING-SW PIC X.                                  
013800     88  TILLK-ARTIKEL-FORPACKNING           VALUE 'J'.                   
013900     88  TILLK-ARTIKEL-EJ-FORPACKNING        VALUE 'N'.                   
014000                                                                          
014100 01  UPPDATERING-SW          PIC X.                                       
014200     88  UPPDATERING-OK                      VALUE 'J'.                   
014300     88  UPPDATERING-EJ-OK                   VALUE 'N'.                   
014400                                                                          
014500 01  BACKNING-SW             PIC X.                                       
014600     88  E-BACKNING                          VALUE 'J'.                   
014700     88  EJ-E-BACKNING                       VALUE 'N'.                   
014800                                                                          
014900 01  UPPDATERING-KONV-SW     PIC X.                                       
015000     88  UPPDATERING-KONV-OK                 VALUE 'J'.                   
015100     88  UPPDATERING-KONV-EJ-OK              VALUE 'N'.                   
015200                                                                          
015300 01  WS-EK01-OK              PIC X           VALUE 'N'.                   
015400 01  WS-EK01-FINNS           PIC X           VALUE 'N'.                   
015500 01  WS-SPAR-IDARTNR-STR     PIC S9(9) VALUE ZERO COMP-3.                 
015600 01  WS-SPAR-IDRADNR         PIC S9(5) VALUE ZERO COMP-3.                 
015700 01  WS-IDRADNR-CHECK        PIC S9(5) VALUE ZERO COMP-3.                 
015800 01  WS-KDISATS-CHECK        PIC X VALUE SPACE.                           
015900 01  WS-TISTODAT-CHECK       PIC S9(7) VALUE ZERO COMP-3.                 
016000                                                                          
016100*      --- VALID IDDC CODES                                               
016200*                                                                         
016300*01    -COPY WWDCKONS                                                     
016400*01    -COPY WWDC99                                                       
016500       EJECT                                                              
016600                                                                          
016700*01    -COPY WWPRODSL                                                     
016800       EJECT                                                              
016900*                            *** GENERELLA SUBRUTINER***                  
017000*                                                                         
017100 01  SUBPROGRAM.                                                          
017200     03   CBLTDLI            PIC X(8)        VALUE 'CBLTDLI '.            
017300     03   FELLOG             PIC X(8)        VALUE 'FELLOG  '.            
017400     03   WDATKONV           PIC X(8)        VALUE 'WDATKONV'.            
017500     03   WORKDAY            PIC X(8)        VALUE 'WORKDAY '.            
017600     03   POSTSUM            PIC X(8)        VALUE 'POSTSUM'.             
017700     03   W2222200           PIC X(8)        VALUE 'W22222'.              
017800                                                                          
017900 01  W-DATUM-X.                                                           
018000     03 W-AAR                PIC 9(2).                                    
018100     03 W-VECKA              PIC 9(2).                                    
018200 01  W-DATUM REDEFINES W-DATUM-X PIC S9(4).                               
018300                                                                          
018400 01  WFELKOD-R07             PIC X(3)  VALUE ZERO.                        
018500 01  WSATSNR                 PIC S9(9) COMP-3.                            
018600 01  WFELTEXT.                                                            
018700     03  FILLER              PIC X(43).                                   
018800     03  WARTNR              PIC Z(9).                                    
018900     03  FILLER              PIC X(12).                                   
019000     03  WMASKINELL          PIC X(9).                                    
019100     03  FILLER              PIC X(2).                                    
019200     03  WVARNING            PIC X.                                       
019300     03  FILLER              PIC X(4).                                    
019400 01  TEXTER.                                                              
019500     03  TEXT1               PIC X(43)                                    
019600         VALUE 'FEL VID ERSÄTTNING AV ARTIKEL ENT. ARTNR='.               
019700     03  TEXT2               PIC X(43)                                    
019800         VALUE 'FEL VID ERSÄTTNINGSDATUMBER.       ARTNR='.               
019900     03  TEXT3               PIC X(43)                                    
020000         VALUE 'INGÅENDE ART. SAKNAS EL.MÄRKT ALT. ARTNR='.               
020100     03  TEXT4               PIC X(43)                                    
020200         VALUE 'VID SLUTGILTIG ERSÄTTNING SAKNADES ARTNR='.               
020300     03  TEXT5               PIC X(43)                                    
020400         VALUE 'VID BACKNING SAKNADES ARTIKEL      ARTNR='.               
020500     03  TEXT6               PIC X(43)                                    
020600         VALUE 'ALTERNATIVERSATT REGISTRERAD       ARTNR='.               
020700     03  TEXT7               PIC X(43)                                    
020800         VALUE 'BACKNING ALTERNATIVERSATT REG.     ARTNR='.               
020900     03  TEXT10              PIC X(43)                                    
021000         VALUE 'ING ARTIKELNR = SATS               ARTNR='.               
021100     03  TEXT11              PIC X(43)                                    
021200         VALUE 'FINNS EJ PÅ ARTIKELREGISTRET       ARTNR='.               
021300     03  TEXT12              PIC X(43)                                    
021400         VALUE 'TILLK ARTNR HAR ING SATS =    STRUKTURNR='.               
021500     03  TEXT13              PIC X(43)                                    
021600         VALUE 'TILLK ARTIKEL = STRNR SOM SATS INGÅR I  ='.               
021700     03  TEXT14              PIC X(43)                                    
021800         VALUE 'INGAENDE ARTIKEL ÄR ERSATT         ARTNR='.               
021900     03  TEXT15              PIC X(43)                                    
022000         VALUE 'INGAENDE ARTIKEL ÄR RENSAD         ARTNR='.               
022100     03  TEXT16              PIC X(43)                                    
022200         VALUE 'INGAENDE ARTIKEL ÄR PASSIVMÄRKT    ARTNR='.               
022300                                                                          
022400                                                                          
022500*   -COPY WDATAREA                                                        
022600                                                                          
022700*   -COPY WORKAREA                                                        
022800                                                                          
022900*01 AREA -COPY W11201   -PRE U01FOV-                                      
023000 EJECT                                                                    
023100                                                                          
023200 01  DAGENS-DATUM            PIC 9(6).                                    
023300*                                                                         
023400 01  FILLER REDEFINES DAGENS-DATUM.                                       
023500     03  DAGENS-DATUM-AAR    PIC 9(2).                                    
023600     03  DAGENS-DATUM-MANAD  PIC 9(2).                                    
023700     03  DAGENS-DATUM-DAG    PIC 9(2).                                    
023800                                                                          
023900 01  WS-DATUM-X.                                                          
024000     03  WS-AAR              PIC 9(2).                                    
024100     03  WS-VECKA            PIC 9(2).                                    
024200 01  WS-DATUM REDEFINES WS-DATUM-X                                        
024300                             PIC 9(4).                                    
024400                                                                          
024500 01  TRANS-DATUM             PIC 9(4).                                    
024600*                                                                         
024700 01  FILLER REDEFINES TRANS-DATUM.                                        
024800     03  TRANS-AAR           PIC 9(2).                                    
024900     03  TRANS-VECKA         PIC 9(2).                                    
025000                                                                          
025100 01  W-DAGENS-DATUM          PIC 9(4).                                    
025200*                                                                         
025300 01  FILLER REDEFINES W-DAGENS-DATUM.                                     
025400     03  W-DAGENS-DATUM-AAR   PIC 9(2).                                   
025500     03  W-DAGENS-DATUM-VECKA PIC 9(2).                                   
025600                                                                          
025700 01  WS-DAGENS-DATUM-VECKA   PIC 9(2).                                    
025800 01  KONTROLL-DATUM          PIC 9(4).                                    
025900*                                                                         
026000 01  FILLER REDEFINES KONTROLL-DATUM.                                     
026100     03  KONTROLL-AAR        PIC 9(2).                                    
026200     03  KONTROLL-VECKA      PIC 9(2).                                    
026300                                                                          
026400 01  SPAR-TIFINLV-DATUM      PIC 9(5).                                    
026500*                                                                         
026600 01  FILLER REDEFINES SPAR-TIFINLV-DATUM.                                 
026700     03  SPAR-TIFINLV-AAR    PIC 9(2).                                    
026800     03  SPAR-TIFINLV-VECKA  PIC 9(2).                                    
026900     03  SPAR-TIFINLV-DAG    PIC 9(1).                                    
027000                                                                          
027100 01  PUBL-DAGENS-DATUM       PIC 9(5).                                    
027200*                                                                         
027300 01  FILLER REDEFINES PUBL-DAGENS-DATUM.                                  
027400     03  PUBL-AAR            PIC 9(2).                                    
027500     03  PUBL-VECKA          PIC 9(2).                                    
027600     03  PUBL-DAG            PIC 9(1).                                    
027700                                                                          
027800 01  LAEGG-TILL-EN-VE-DAT    PIC 9(5).                                    
027900*                                                                         
028000 01  FILLER REDEFINES LAEGG-TILL-EN-VE-DAT.                               
028100     03  LAEGG-AAR           PIC 9(2).                                    
028200     03  LAEGG-VECKA         PIC 9(2).                                    
028300     03  LAEGG-DAG           PIC 9(1).                                    
028400                                                                          
028500 01  SPAR-DATUM              PIC 9(6).                                    
028600*                                                                         
028700 01  FILLER REDEFINES SPAR-DATUM.                                         
028800     03  SPAR-AAR            PIC 9(2).                                    
028900     03  SPAR-MAN            PIC 9(2).                                    
029000     03  SPAR-DAG            PIC 9(2).                                    
029100                                                                          
029200 01  ERSAETTNINGS-DATUM      PIC 9(6).                                    
029300*                                                                         
029400 01  FILLER REDEFINES ERSAETTNINGS-DATUM.                                 
029500     03  ERS-AAR             PIC 9(2).                                    
029600     03  ERS-MAN             PIC 9(2).                                    
029700     03  ERS-DAG             PIC 9(2).                                    
029800                                                                          
029900 01  TIERSDAT-DATUM          PIC 9(6).                                    
030000*                                                                         
030100 01  FILLER REDEFINES TIERSDAT-DATUM.                                     
030200     03  TIERSDAT-AAR        PIC 9(2).                                    
030300     03  TIERSDAT-MAN        PIC 9(2).                                    
030400     03  TIERSDAT-DAG        PIC 9(2).                                    
030500                                                                          
030600 01  UPPD-DATUM              PIC 9(6).                                    
030700*                                                                         
030800 01  FILLER REDEFINES UPPD-DATUM.                                         
030900     03  UPPD-AAR            PIC 9(2).                                    
031000     03  UPPD-MAN            PIC 9(2).                                    
031100     03  UPPD-DAG            PIC 9(2).                                    
031200                                                                          
031300 01  INDX                         PIC S9(9) VALUE +0 COMP SYNC.           
031400 01  WDISP-LAGER                  PIC S9(9)V9(2)  COMP-3.                 
031500 01  SPAR-IDARTNR-SATS            PIC S9(9)       COMP-3.                 
031600 01  SPAR-2234-IDARTNR-RAD        PIC S9(9)       COMP-3.                 
031700 01  SPAR-IDARTNR-ING1            PIC S9(9)       COMP-3.                 
031800 01  SPAR-SATSNR                  PIC S9(9)       COMP-3.                 
031900 01  SPAR-IDARTNR                 PIC S9(9)       COMP-3.                 
032000 01  SPAR-RAD-IDARTNR             PIC S9(9)       COMP-3.                 
032100 01  SPAR-STR-IDARTNR             PIC S9(9)       COMP-3.                 
032200 01  KOLLA-IDARTNR                PIC S9(9)       COMP-3.                 
032300 01  KOLL-HIST-IDARTNR            PIC S9(9)       COMP-3.                 
032400 01  KOLL-DEF-IDARTNR             PIC S9(9)       COMP-3.                 
032500 01  KOLL-BACK-IDARTNR            PIC S9(9)       COMP-3.                 
032600 01  SPAR-KOLL-FLER-ARTNR         PIC S9(9)       COMP-3.                 
032700                                                                          
032800**************************************************************            
032900*******  AREOR FÖR KONTROLL AV KONVERTERING ELLER EJ *********            
033000**************************************************************            
033100                                                                          
033200 01  KOLL-KONV-ING                PIC S9(9)       COMP-3.                 
033300 01  KOLL-KONV-SATS               PIC S9(9)       COMP-3.                 
033400 01  WS-IDARTNR                   PIC S9(9)       COMP-3.                 
033500 01  IDARTNR-KONVERTERAD-WS       PIC S9(9)       COMP-3.                 
033600 01  IDARTNR-KONVERTERAD-SATS     PIC S9(9)       COMP-3.                 
033700 01  W-IDRADNR-FORPACKNING        PIC S9(5)       COMP-3.                 
033800 01  WS-STR-IDARTNR               PIC S9(9)       COMP-3.                 
033900 01  WS-IDRADNR                   PIC S9(5)       COMP-3.                 
034000 01  WS-KDSTRRAD                  PIC X.                                  
034100 01  WS-IDRADNR-FORPACKNING       PIC S9(5)       COMP-3.                 
034200 01  KONV-TISTODAT                PIC S9(7)       COMP-3.                 
034300 01  KONV-KDISATS                 PIC X.                                  
034400                                                                          
034500******************************************************************        
034600                                                                          
034700 01  SPAR-KVBEHOV                 PIC S9(7)V9(2)  COMP-3.                 
034800 01  SPAR-KDERS                   PIC S9(3)       COMP-3.                 
034900                                                                          
035000                                                                          
035100 01  SPAR-KVLS-C1                 PIC S9(7)       COMP-3.                 
035200 01  SPAR-KVAKS                   PIC S9(7)       COMP-3.                 
035300 01  SPAR-KVRESS-C1               PIC S9(7)       COMP-3.                 
035400 01  SPAR-KVROS-C1                PIC S9(7)       COMP-3.                 
035500 01  SPAR-UPPD-DATUM              PIC S9(7)       COMP-3.                 
035600 01  SPAR-REANTPSA                PIC S9(2)V9(3)  COMP-3.                 
035700 01  SPAR-KDISATS                 PIC X.                                  
035800 01  SPAR-TISTADAT                PIC S9(7) COMP-3.                       
035900 01  SPAR-TISTODAT                PIC S9(7) COMP-3.                       
036000 01  SPAR-ERSATT-TISTODAT         PIC S9(7) COMP-3.                       
036100 01  SPAR-IDARTNR-ING             PIC S9(9) COMP-3.                       
036200 01  SPAR-KDSTRRAD                PIC X.                                  
036300 01  SPAR-IDRADNR                 PIC S9(5) COMP-3.                       
036400 01  SPAR-IDRADNR2                PIC S9(5) COMP-3.                       
036500 01  SPAR-OMNUM-IDRADNR           PIC S9(5) COMP-3.                       
036600 01  SPAR-RADNR                   PIC S9(5) COMP-3.                       
036700 01  SPAR-IDSTRTYP                PIC X.                                  
036800                                                                          
036900 01  W-PUBL-DAT-X.                                                        
037000     03  W-PUBL-AA                PIC 9(2).                               
037100     03  W-PUBL-VV                PIC 9(2).                               
037200     03  W-PUBL-D                 PIC 9.                                  
037300 01  W-PUBLDAT REDEFINES W-PUBL-DAT-X                                     
037400                                  PIC 9(5).                               
037500 01  W-VECKOREST                  PIC S9(5).                              
037600                                                                          
037700 01  IDEX                         PIC S9(4)       COMP.                   
037800 01  BEHIDEX                      PIC S9(4)       COMP.                   
037900 01  VARV-TIDBER                  PIC S9(4)       COMP.                   
038000                                                                          
038100 01  ERS-DATUM-X.                                                         
038200     03 W-ERS-DATUM-AAR-VV        PIC  9(4).                              
038300     03 W-ERS-DATUM-DAG           PIC  9(1).                              
038400 01  ERS-DATUM-KOLL REDEFINES ERS-DATUM-X PIC S9(5).                      
038500                                                                          
038600 01  TAB-INDX                     PIC S9(3)  VALUE ZERO.                  
038700 01  TAB-INDX2                    PIC S9(3)  VALUE ZERO.                  
038800 01  KONTROLL-TAB-INDX            PIC S9(3)  VALUE ZERO.                  
038900 01  MAX-TABELL-LAENGD            PIC S9(3)  VALUE +100.                  
039000                                                                          
039100 01  SPAR-IDARTNR-TILLK           PIC S9(9)  COMP-3.                      
039200 01  SPAR-IDARTNR-STRUKTUR        PIC S9(9)  COMP-3.                      
039300                                                                          
039400                                                                          
039500 01  STRUKTURNRTABELL.                                                    
039600     03 STRUKTURNR OCCURS 100     PIC S9(9)  COMP-3.                      
039700                                                                          
039800 01  EK01TABELL.                                                          
039900     03 EK01-IDARTNR OCCURS 100   PIC S9(9)  COMP-3.                      
040000                                                                          
040100******************************************  ANTAL FÖR 2234-OR             
040200                                                                          
040300 01  NEW-REANTPSA                 PIC S9(2)V9(3)  COMP-3.                 
040400 01  OLD-REANTPSA                 PIC S9(2)V9(3)  COMP-3.                 
040500                                                                          
040600*                  ********************************************           
040700*                  *** TAB MED BEHANDLINGSBARA ERSÄTTNINGS- ***           
040800*                  *** KODER I STIGANDE ORDNING, GAMMAL +   ***           
040900*                  *** NY-KOD                               ***           
041000*                  ********************************************           
041100                                                                          
041200 01  BEHANDLINGSBARA-KODER.                                               
041300     03  FILLER     PIC X(20) VALUE '00010002000300040005'.               
041400     03  FILLER     PIC X(20) VALUE '00060052010001110200'.               
041500     03  FILLER     PIC X(20) VALUE '02220300032304000414'.               
041600     03  FILLER     PIC X(16) VALUE '0500052506000626'.                   
041700                                                                          
041800 01  TAB-BEHBARA-KODER REDEFINES BEHANDLINGSBARA-KODER.                   
041900     03  INGANG  OCCURS 19 ASCENDING KEY IS GODKAND-KOD                   
042000                           INDEXED BY KODTAB-IDEX.                        
042100         05  GODKAND-KOD      PIC X(4).                                   
042200                                                                          
042300 01  ERSKOD-TRANS.                                                        
042400     03  OLD-ERSKOD           PIC 9(2).                                   
042500     03  NEW-ERSKOD           PIC 9(2).                                   
042600                                                                          
042700*                  ********************************************           
042800*                  *** TABELL FÖR ERSÄTTNINGS-ARTNR FÖRSTA  ***           
042900*                  *** INGÅNG = ART SOM SKALL ERSÄTTAS      ***           
043000*                  ********************************************           
043100                                                                          
043200 01  ERS-TAB.                                                             
043300     03  ERS-TABANT              PIC S9(3)       COMP-3.                  
043400     03  ERS-INGANG  OCCURS 99.                                           
043500         05  ERS-IDARTNR         PIC S9(9)       COMP-3.                  
043600         05  ERS-DIERS           PIC S9(4)V9(3)  COMP-3.                  
043700     03  ERS-DATUM               PIC S9(5)       COMP-3.                  
043800     03  ERS-TIERSDAT-REG        PIC S9(5).                               
043900     03  FILLER REDEFINES ERS-TIERSDAT-REG.                               
044000         05  ERS-TIERSDAT-REG4   PIC 9(4).                                
044100         05  FILLER              PIC X.                                   
044200     03  ERS-TIERSDAT-PREL       PIC S9(5).                               
044300     03  FILLER REDEFINES ERS-TIERSDAT-PREL.                              
044400         05  ERS-TIERSDAT-PREL4  PIC 9(4).                                
044500         05  FILLER              PIC X.                                   
044600     03  ERS-KVANT-I-SATS        PIC S9(2)V9(3)  COMP-3.                  
044700                                                                          
044800**************************************** NYCKLAR TILL DLI ******          
044900*                                                                         
045000 01  NYCKLAR-TILL-DLI.                                                    
045100     03  W-WDJ1CSEQ-X.                                                    
045200         05 W-IDLEVNR-C-X.                                                
045300           07 W-IDLEVNR      PIC X(5)   VALUE SPACE.                      
045400         05 W-BELEVART-X.                                                 
045500           07 W-BELEVART     PIC X(30)  VALUE SPACE.                      
045600         05 W-IDARTNR-C-X.                                                
045700           07 W-IDARTNRC     PIC S9(9)  VALUE ZERO COMP-3.                
045800                                                                          
045900     03  W-IDARTNR-2-X.                                                   
046000         05 W-IDARTNR-2      PIC S9(9)  VALUE ZERO COMP-3.                
046100                                                                          
046200     03  W-IDARTNR-X.                                                     
046300         05 W-IDARTNR        PIC S9(9)       COMP-3.                      
046400                                                                          
046500     03  W-WDJ111KY-X.                                                    
046600         05 W-IDKDSTRRAD-X.                                               
046700           07 W-IDKDSTRRAD   PIC X       VALUE SPACE.                     
046800         05 W-IDRADNR-X.                                                  
046900           07 W-IDRADNR      PIC S9(5)   VALUE ZERO COMP-3.               
047000                                                                          
047100     03  W-IDARTNR-KONV-X.                                                
047200         05 W-IDARTNR-KONV   PIC S9(9)       COMP-3.                      
047300                                                                          
047400     03  W-WDJ111KY-KONV-X.                                               
047500         05 W-KDSTRRAD-KONV     PIC X       VALUE SPACE.                  
047600         05 W-IDRADNR-KONV      PIC S9(5)   VALUE ZERO COMP-3.            
047700                                                                          
047800     03  W-IDARTNR-OKONV-X.                                               
047900         05 W-IDARTNR-OKONV  PIC S9(9)       COMP-3.                      
048000                                                                          
048100     03  W-WDJ111KY-OKONV-X.                                              
048200         05 W-KDSTRRAD-OKONV    PIC X       VALUE SPACE.                  
048300         05 W-IDRADNR-OKONV     PIC S9(5)   VALUE ZERO COMP-3.            
048400                                                                          
048500     03  W-WDD901KY-X.                                                    
048600         05 W-IDARTNR-INLB      PIC S9(9)   VALUE ZERO COMP-3.            
048700         05 W-IDDC-INLB         PIC X(2)    VALUE SPACE.                  
048800                                                                          
048900     03  W-IDLEVNR-X.                                                     
049000         05 W-IDLEVNR-INLB   PIC X(5)         VALUE SPACE.                
049100                                                                          
049200     03  W-IDLEVNR-OKVAL-X.                                               
049300         05 W-IDLEVNR-OKVAL  PIC X(5)         VALUE '1002 '.              
049400                                                                          
049500     03  W-TIBEHOV-X.                                                     
049600         05 W-TIBEHOV        PIC S9(5)        COMP-3.                     
049700                                                                          
049800*                  03  GAMLA W-2301-KEY-X. GAMLA WLXXBT                   
049900*                    05 FILLER GAMLA PIC X(4)       VALUE '2301'.         
050000     03  W-2303-KEY-X.                                                    
050100         05 FILLER           PIC X(4)         VALUE '2303'.               
050200         05 FILLER           PIC X(26)        VALUE LOW-VALUE.            
050300                                                                          
050400*                  03  GAMLA W-2302-KEY-X. GAMLA WLXXBO                   
050500*                    05 FILLER GAMLA PIC X(4)       VALUE '2302'.         
050600     03  W-2301-KEY-X.                                                    
050700         05 FILLER           PIC X(4)         VALUE '2301'.               
050800         05 FILLER           PIC X(26)        VALUE LOW-VALUE.            
050900                                                                          
051000     03  W-2233-KEY-X.                                                    
051100         05 FILLER           PIC X(4)         VALUE '2233'.               
051200         05 FILLER           PIC X(26)        VALUE LOW-VALUE.            
051300                                                                          
051400******************************** ARBETSAREOR TILL IMS ***                 
051500*                                                                         
051600*                                                                         
051700 01  IMS-WS.                                                              
051800     03  FILLER              PIC X(8)        VALUE 'IMS-WS  '.            
051900                                                                          
052000*                            *** STATUSKOD FRÅN IMS ***                   
052100     03  STATUS-WS           PIC XX.                                      
052200         88  SEGMENT-FINNS                   VALUE '  '.                  
052300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
052400         88  IMS-EJ-OK                       VALUE 'XD'.                  
052500         88  BAS-SLUT                        VALUE 'GB'.                  
052600                                                                          
052700     03  GODK-STATUSKODER.                                                
052800         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
052900                                                                          
053000     03  SSA1                PIC X(64).                                   
053100     03  SSA2                PIC X(64).                                   
053200     03  SSA3                PIC X(64).                                   
053300                                                                          
053400     EJECT                                                                
053500*01  -COPY W0003                                                          
053600     EJECT                                                                
053700*                            *** DLI-IO-AREA ***                          
053800*                                                                         
053900 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA'.            
054000 01  DLI-IO-AREA.                                                         
054100     03 IO-AREA             PIC X(250)   VALUE SPACE.                     
054200                                                                          
054300*    03 WLARTC01 -COPY WDK601    -PRE ART2-     -RED IO-AREA.             
054400     EJECT                                                                
054500                                                                          
054600*    03 WLSATB01 -COPY WDJ101   -PRE SATB-      -RED IO-AREA.             
054700     EJECT                                                                
054800                                                                          
054900*    03 WLSATB11 -COPY  WDJ111   -PRE SATB-     -RED IO-AREA.             
055000     EJECT                                                                
055100                                                                          
055200                                                                          
055300 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-A'.          
055400 01  DLI-IO-AREA-A.                                                       
055500     03 IO-AREA-A           PIC X(250)   VALUE SPACE.                     
055600                                                                          
055700*    03 WLERSA01 -COPY  WDD701   -PRE ERSA-     -RED IO-AREA-A.           
055800 EJECT                                                                    
055900                                                                          
056000*    03 WLERSA11 -COPY  WDD702   -PRE ERSA-     -RED IO-AREA-A.           
056100 EJECT                                                                    
056200                                                                          
056300*    03 WLERSA13 -COPY  WDD704   -PRE ERSA-     -RED IO-AREA-A.           
056400 EJECT                                                                    
056500                                                                          
056600*    03 WLINLB01 -COPY  WDD901   -PRE INLB-     -RED IO-AREA-A.           
056700 EJECT                                                                    
056800                                                                          
056900*    03 WLINLB11 -COPY  WDD902   -PRE INLB-     -RED IO-AREA-A.           
057000 EJECT                                                                    
057100                                                                          
057200 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-1'.          
057300 01  DLI-IO-AREA-1.                                                       
057400     03 IO-AREA-1           PIC X(900)   VALUE SPACE.                     
057500                                                                          
057600*    03 WLARTC01 -COPY  WDK601                  -RED IO-AREA-1.           
057700 EJECT                                                                    
057800                                                                          
057900*    03 WLARTC11 -COPY  WDK611                  -RED IO-AREA-1.           
058000 EJECT                                                                    
058100 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-2'.          
058200 01  DLI-IO-AREA-2.                                                       
058300     03 IO-AREA-2            PIC X(500)  VALUE SPACE.                     
058400*    GAMLA XXBT11  ERSÄTTNINGAR                                           
058500*    03 WDGX2304 -COPY  WDGX2304               -RED IO-AREA-2.            
058600 EJECT                                                                    
058700*    GAMLA XXBO11  SATSER LEVNUM                                          
058800*    03 WDGX2302 -COPY  WDGX2302                -RED IO-AREA-2.           
058900 EJECT                                                                    
059000                                                                          
059100 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-3'.          
059200 01  DLI-IO-AREA-3.                                                       
059300     03 IO-AREA-3           PIC X(350)   VALUE SPACE.                     
059400                                                                          
059500     03 WLSATB-CSEQ REDEFINES IO-AREA-3.                                  
059600*       05 WLSATB11 -COPY WDJ111     -PRE SATB11C-                        
059700     SKIP2                                                                
059800*       05 WLSATB01 -COPY WDJ101     -PRE SATB01C-                        
059900 EJECT                                                                    
060000                                                                          
060100 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-4'.          
060200 01  DLI-IO-AREA-4.                                                       
060300     03 IO-AREA-4           PIC X(250)   VALUE SPACE.                     
060400                                                                          
060500*    03 WLXXBY01 -COPY  WDGX2234                -RED IO-AREA-4.           
060600 EJECT                                                                    
060700 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-AREA-5'.          
060800 01  DLI-IO-AREA-5.                                                       
060900     03 IO-AREA-5            PIC X(600)  VALUE SPACE.                     
061000                                                                          
061100*    03 WLARTG01 -COPY  WDD201   -PRE ARTG-     -RED IO-AREA-5.           
061200 EJECT                                                                    
061300                                                                          
061400 01  FILLER                PIC X(17)  VALUE 'DLI-IO-AREA-OKONV'.          
061500 01  DLI-IO-AREA-OKONV.                                                   
061600     03 IO-AREA-OKONV       PIC X(250)   VALUE SPACE.                     
061700                                                                          
061800*    03 WLSATB01 -COPY WDJ101  -PRE SATB01O- -RED IO-AREA-OKONV.          
061900 EJECT                                                                    
062000                                                                          
062100*    03 WLSATB11 -COPY  WDJ111 -PRE SATB11O- -RED IO-AREA-OKONV.          
062200 EJECT                                                                    
062300                                                                          
062400 01  FILLER                PIC X(16)  VALUE 'DLI-IO-AREA-KONV'.           
062500 01  DLI-IO-AREA-KONV.                                                    
062600     03 IO-AREA-KONV       PIC X(250)   VALUE SPACE.                      
062700                                                                          
062800*    03 WLSATB01 -COPY WDJ101  -PRE SATB01K- -RED IO-AREA-KONV.           
062900 EJECT                                                                    
063000                                                                          
063100*    03 WLSATB11 -COPY  WDJ111 -PRE SATB11K- -RED IO-AREA-KONV.           
063200 EJECT                                                                    
063300                                                                          
063400 01  FILLER                PIC X(16)  VALUE 'DLI-IO-AREA-FILC'.           
063500 01  DLI-IO-AREA-FILC.                                                    
063600     03 IO-AREA-FILC       PIC X(250)   VALUE SPACE.                      
063700                                                                          
063800*    03 WLFILC01 -COPY WDR301   -RED IO-AREA-FILC.                        
063900     EJECT                                                                
064000                                                                          
064100* - - - - - - - - - - - - - - - - - - POSTSUM                             
064200*                                                                         
064300*01  -COPY W0005      -PRE POSTSUM-                                       
064400     EJECT                                                                
064500                                                                          
064600*01 AREA -COPY W222L222 -PRE LINK-                                        
064700                                                                          
064800     EJECT                                                                
064900 LINKAGE SECTION.                                                         
065000                                                                          
065100*01  -COPY W0009   -PRE MSG-                                              
065200     EJECT                                                                
065300*01  -COPY W0008    -PRE ART2-                                            
065400         05  FILLER          PIC X(1).                                    
065500     EJECT                                                                
065600*01  -COPY W0008    -PRE SATB-                                            
065700         05  FILLER          PIC X(1).                                    
065800     EJECT                                                                
065900*01  -COPY W0008    -PRE SATB-O-                                          
066000         05  FILLER          PIC X(1).                                    
066100     EJECT                                                                
066200*01  -COPY W0008    -PRE SATB-K-                                          
066300         05  FILLER          PIC X(1).                                    
066400     EJECT                                                                
066500*01  -COPY W0008    -PRE SATB-C-                                          
066600         05  FILLER          PIC X(1).                                    
066700     EJECT                                                                
066800*01  -COPY W0008    -PRE SATB-C1-                                         
066900         05  FILLER          PIC X(1).                                    
067000     EJECT                                                                
067100*01  -COPY W0008    -PRE SATB-C2-                                         
067200         05  FILLER          PIC X(1).                                    
067300     EJECT                                                                
067400*01  -COPY W0008    -PRE SATB-C3-                                         
067500         05  FILLER          PIC X(1).                                    
067600     EJECT                                                                
067700*01  -COPY W0008    -PRE ARTC-                                            
067800         05  FILLER          PIC X(1).                                    
067900     EJECT                                                                
068000*01  -COPY W0008    -PRE ERSA-                                            
068100         05  FILLER          PIC X(1).                                    
068200     EJECT                                                                
068300*01  -COPY W0008    -PRE INLB-                                            
068400         05  FILLER          PIC X(1).                                    
068500     EJECT                                                                
068600*01  -COPY W0008    -PRE 2303-                                            
068700         05  FILLER          PIC X(1).                                    
068800     EJECT                                                                
068900*01  -COPY W0008    -PRE 2301-                                            
069000         05  FILLER          PIC X(1).                                    
069100     EJECT                                                                
069200*01  -COPY W0008    -PRE 2234-                                            
069300         05  FILLER          PIC X(1).                                    
069400     EJECT                                                                
069500 01  W222-WDK6-PCB           PIC X(1).                                    
069600 01  W222-ARTM-PCB           PIC X(1).                                    
069700     EJECT                                                                
069800*01  -COPY W0008    -PRE SATB2-                                           
069900         05  FILLER          PIC X(1).                                    
070000     EJECT                                                                
070100 01  W222-WDK7-PCB           PIC X(1).                                    
070200     EJECT                                                                
070300*01  -COPY W0008    -PRE FILC-                                            
070400         05  FILLER          PIC X(1).                                    
070500     EJECT                                                                
070600 01  W222-2501-PCB           PIC X(1).                                    
070700 01  W222-WDB6R-PCB          PIC X(1).                                    
070800 01  W222-WDK7R-PCB          PIC X(1).                                    
070900 01  W222-WDB6-PCB           PIC X(1).                                    
071000 01  W222-WDD7-PCB           PIC X(1).                                    
071100 01  W222-WDK7E-PCB          PIC X(1).                                    
071200 01  W222-UTIL-WDK6-PCB      PIC X(1).                                    
071300 01  W222-UTIL-WDK7-PCB      PIC X(1).                                    
071400 01  W222-UTIL-WDB6-PCB      PIC X(1).                                    
071500 01  W222-UTUP-WDK7-PCB      PIC X(1).                                    
071600 01  W222-UTUP-WDB6-PCB      PIC X(1).                                    
071700 01  W222-UTUP-UTIL-WDK6-PCB PIC X(1).                                    
071800 01  W222-UTUP-UTIL-WDK7-PCB PIC X(1).                                    
071900 01  W222-UTUP-UTIL-WDB6-PCB PIC X(1).                                    
072000     EJECT                                                                
072100 PROCEDURE DIVISION USING  MSG-PCB ART2-PCB SATB-PCB SATB-O-PCB           
072200                 SATB-K-PCB SATB-C-PCB                                    
072300                 SATB-C1-PCB SATB-C2-PCB SATB-C3-PCB ARTC-PCB             
072400                 ERSA-PCB INLB-PCB 2303-PCB 2301-PCB                      
072500                 2234-PCB W222-WDK6-PCB SATB2-PCB W222-WDK7-PCB           
072600                 W222-ARTM-PCB FILC-PCB W222-2501-PCB                     
072700                 W222-WDB6R-PCB W222-WDK7R-PCB                            
072800                 W222-WDB6-PCB W222-WDD7-PCB W222-WDK7E-PCB               
072900                 W222-UTIL-WDK6-PCB W222-UTIL-WDK7-PCB                    
073000                 W222-UTIL-WDB6-PCB W222-UTUP-WDK7-PCB                    
073100                 W222-UTUP-WDB6-PCB W222-UTUP-UTIL-WDK6-PCB               
073200                 W222-UTUP-UTIL-WDK7-PCB W222-UTUP-UTIL-WDB6-PCB          
073300                 .                                                        
073400                                                                          
073500 MAIN SECTION.                                                            
073600     ENTRY 'DLITCBL' USING MSG-PCB ART2-PCB SATB-PCB SATB-O-PCB           
073700                 SATB-K-PCB SATB-C-PCB                                    
073800                 SATB-C1-PCB SATB-C2-PCB SATB-C3-PCB ARTC-PCB             
073900                 ERSA-PCB INLB-PCB 2303-PCB 2301-PCB                      
074000                 2234-PCB W222-WDK6-PCB SATB2-PCB W222-WDK7-PCB           
074100                 W222-ARTM-PCB FILC-PCB W222-2501-PCB                     
074200                 W222-WDB6R-PCB W222-WDK7R-PCB                            
074300                 W222-WDB6-PCB W222-WDD7-PCB W222-WDK7E-PCB               
074400                 W222-UTIL-WDK6-PCB W222-UTIL-WDK7-PCB                    
074500                 W222-UTIL-WDB6-PCB W222-UTUP-WDK7-PCB                    
074600                 W222-UTUP-WDB6-PCB W222-UTUP-UTIL-WDK6-PCB               
074700                 W222-UTUP-UTIL-WDK7-PCB W222-UTUP-UTIL-WDB6-PCB          
074800                 .                                                        
074900     SKIP2                                                                
075000     PERFORM A-INIT                                                       
075100                                                                          
075200****** BEHANDLA MASKINELLA ERSÄTTNINGAR        ******************         
075300                                                                          
075400     PERFORM IMS-GET-2303-ROT                                             
075500     IF SEGMENT-FINNS                                                     
075600       PERFORM IMS-GETNEXT-2304                                           
075700       IF SEGMENT-FINNS                                                   
075800                                                                          
075900         PERFORM UNTIL SEGMENT-SAKNAS                                     
076000****************** 970704                                                 
076100*          DISPLAY '2304-IDARTNR-SAT = ' 2304-IDARTNR-SATS                
076200*          DISPLAY '2304-IDARTNR-ING = ' 2304-IDARTNR-ING                 
076300****************** 970704                                                 
076400           IF CHKP-ANT > CHKP-MAX                                         
076500             PERFORM X-TAG-CHECKPOINT-2304                                
076600           END-IF                                                         
076700           MOVE JA TO BEARBETNINGS-KOLL-SW                                
076800           MOVE 2304-IDARTNR-SATS TO KOLL-KONV-SATS                       
076900           MOVE 2304-IDARTNR-ING  TO KOLL-KONV-ING                        
077000           IF KOLL-KONV-SATS NOT = ZERO                                   
077100             COMPUTE IDARTNR-KONVERTERAD-WS = +999999999 -                
077200                     KOLL-KONV-SATS                                       
077300             MOVE IDARTNR-KONVERTERAD-WS TO W-IDARTNR-KONV                
077400             PERFORM IMS-GET-SATB01-KONV                                  
077500             IF SEGMENT-FINNS                                             
077600                                                                          
077700               MOVE 001                  TO WORK-KDCALL                   
077800               MOVE WC-CDC-SE            TO WORK-IDDC                     
077900               MOVE SATB01K-STR-TIREGDAT TO WORK-TIAAMMDD-FOM             
078000               MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM             
078100               CALL WORKDAY USING WORK-KDCALL                             
078200                                  WORK-DATE-AREA                          
078300                                  WORK-KDSVAR                             
078400************ TEST OM ARB-KDSVAR-FEL GÖRS FÖR ATT                          
078500*            OM ARB-KVARBDAG TARR BARA +999 OM                            
078600*            DÅ ANTAL DAGAR ÄR STÖRRE TESTAS DET                          
078700*            MED KDSVAR-FEL                                               
078800                                                                          
078900               IF WORK-KVWORKD > 2 OR WORK-KDSVAR-FEL                     
079000                 PERFORM IMS-DLET-SATB-KONV                               
079100                 MOVE JA TO BEARBETNINGS-KOLL-SW                          
079200               ELSE                                                       
079300                 MOVE NEJ TO BORTTAGNINGS-SW                              
079400                 MOVE NEJ TO BEARBETNINGS-KOLL-SW                         
079500               END-IF                                                     
079600             ELSE                                                         
079700               MOVE JA TO BEARBETNINGS-KOLL-SW                            
079800             END-IF                                                       
079900           END-IF                                                         
080000                                                                          
080100           IF KOLL-KONV-ING NOT = ZERO                                    
080200             MOVE KOLL-KONV-ING TO W-IDARTNRC                             
080300             PERFORM IMS-GET-SATB-CSEQ-UNIK1                              
080400             MOVE NEJ TO KONV-SW                                          
080500             IF SEGMENT-FINNS                                             
080600               PERFORM UNTIL (SEGMENT-SAKNAS) OR (KONV-FINNS)             
080700                 IF SATB01C-STR-IDARTNR < 100000000                       
080800                   MOVE SATB01C-STR-IDARTNR TO WS-IDARTNR                 
080900                   COMPUTE IDARTNR-KONVERTERAD-WS = +999999999            
081000                         - WS-IDARTNR                                     
081100                   MOVE IDARTNR-KONVERTERAD-WS TO W-IDARTNR-KONV          
081200                   PERFORM IMS-GET-SATB01-KONV                            
081300                   IF SEGMENT-FINNS                                       
081400                                                                          
081500                     MOVE 001          TO WORK-KDCALL                     
081600                     MOVE WC-CDC-SE    TO WORK-IDDC                       
081700                     MOVE SATB01K-STR-TIREGDAT                            
081800                                       TO WORK-TIAAMMDD-FOM               
081900                     MOVE DAGENS-DATUM TO WORK-TIAAMMDD-TOM               
082000                     CALL WORKDAY USING WORK-KDCALL                       
082100                                        WORK-DATE-AREA                    
082200                                        WORK-KDSVAR                       
082300                                                                          
082400************   TEST OM ARB-KDSVAR-FEL GÖRS FÖR ATT                        
082500*              OM ARB-KVARBDAG TARR BARA +999 OM                          
082600*              DÅ ANTAL DAGAR ÄR STÖRRE TESTAS DET                        
082700*              MED KDSVAR-FEL                                             
082800                                                                          
082900                                                                          
083000                     IF WORK-KVWORKD > 2 OR WORK-KDSVAR-FEL               
083100                       PERFORM IMS-DLET-SATB-KONV                         
083200                       MOVE JA TO BEARBETNINGS-KOLL-SW                    
083300                     ELSE                                                 
083400                       MOVE JA TO KONV-SW                                 
083500                       MOVE NEJ TO BORTTAGNINGS-SW                        
083600                       MOVE NEJ TO BEARBETNINGS-KOLL-SW                   
083700                     END-IF                                               
083800                   ELSE                                                   
083900                     PERFORM IMS-GET-SATB-CSEQ-NEXT                       
084000                   END-IF                                                 
084100                 ELSE                                                     
084200                   PERFORM IMS-GET-SATB-CSEQ-NEXT                         
084300                 END-IF                                                   
084400               END-PERFORM                                                
084500             END-IF                                                       
084600           END-IF                                                         
084700                                                                          
084800           IF BEARBETNING-KOLL-OK                                         
084900             MOVE 2304-IDARTNR-SATS TO SPAR-IDARTNR-SATS                  
085000                                       WSATSNR                            
085100             MOVE 2304-IDARTNR-ING TO SPAR-IDARTNR-ING1                   
085200             PERFORM B-BEHANDLA-2304                                      
085300             IF BORTTAGNING-OK                                            
085400               PERFORM IMS-TA-BORT-2304                                   
085500             END-IF                                                       
085600             PERFORM IMS-GETNEXT-2304                                     
085700           ELSE                                                           
085800             PERFORM IMS-GETNEXT-2304                                     
085900           END-IF                                                         
086000         END-PERFORM                                                      
086100       END-IF                                                             
086200     END-IF                                                               
086300                                                                          
086400***** BEHANDLA MASKINELLA ÄNDRINGAR AV LEVNR   ******************         
086500                                                                          
086600     PERFORM IMS-GET-2301-ROT                                             
086700     IF SEGMENT-FINNS                                                     
086800       PERFORM IMS-GETNEXT-2302                                           
086900       IF SEGMENT-FINNS                                                   
087000         PERFORM UNTIL SEGMENT-SAKNAS                                     
087100****************** 970704                                                 
087200*          DISPLAY '2302-IDARTNR-SAT = ' 2302-IDARTNR-SATS                
087300*          DISPLAY '2302-IDLEVNR     = ' 2302-IDLEVNR                     
087400****************** 970704                                                 
087500           IF CHKP-ANT > CHKP-MAX                                         
087600             PERFORM X-TAG-CHECKPOINT-2302                                
087700           END-IF                                                         
087800           MOVE JA TO BORTTAGNINGS-SW                                     
087900           MOVE 2302-IDARTNR-SATS TO KOLL-KONV-SATS                       
088000           COMPUTE IDARTNR-KONVERTERAD-WS = +999999999 -                  
088100                   KOLL-KONV-SATS                                         
088200           MOVE IDARTNR-KONVERTERAD-WS TO W-IDARTNR-KONV                  
088300           PERFORM IMS-GET-SATB01-KONV                                    
088400           IF SEGMENT-FINNS                                               
088500                                                                          
088600             MOVE 001                  TO WORK-KDCALL                     
088700             MOVE WC-CDC-SE            TO WORK-IDDC                       
088800             MOVE SATB01K-STR-TIREGDAT TO WORK-TIAAMMDD-FOM               
088900             MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM               
089000             CALL WORKDAY USING WORK-KDCALL                               
089100                                WORK-DATE-AREA                            
089200                                WORK-KDSVAR                               
089300                                                                          
089400************ TEST OM ARB-KDSVAR-FEL GÖRS FÖR ATT                          
089500*            OM ARB-KVARBDAG TARR BARA +999 OM                            
089600*            DÅ ANTAL DAGAR ÄR STÖRRE TESTAS DET                          
089700*            MED KDSVAR-FEL                                               
089800                                                                          
089900             IF WORK-KVWORKD > 2 OR WORK-KDSVAR-FEL                       
090000               PERFORM IMS-DLET-SATB-KONV                                 
090100               MOVE JA TO BEARBETNINGS-KOLL-SW                            
090200             ELSE                                                         
090300               MOVE JA TO KONV-SW                                         
090400               MOVE NEJ TO BORTTAGNINGS-SW                                
090500               MOVE NEJ TO BEARBETNINGS-KOLL-SW                           
090600             END-IF                                                       
090700           ELSE                                                           
090800             MOVE JA TO BEARBETNINGS-KOLL-SW                              
090900           END-IF                                                         
091000                                                                          
091100           IF BEARBETNING-KOLL-OK                                         
091200             MOVE 2302-IDARTNR-SATS TO SPAR-SATSNR                        
091300                                       WSATSNR                            
091400             PERFORM C-BEHANDLA-2302                                      
091500                                                                          
091600             IF BORTTAGNING-OK                                            
091700               PERFORM IMS-TA-BORT-2302                                   
091800             END-IF                                                       
091900                                                                          
092000           END-IF                                                         
092100           PERFORM IMS-GETNEXT-2302                                       
092200         END-PERFORM                                                      
092300       END-IF                                                             
092400     END-IF                                                               
092500                                                                          
092600     PERFORM D-OMRAKNING-STOPPDATUM                                       
092700     PERFORM Z-FINIT                                                      
092800     MOVE +0 TO RETURN-CODE                                               
092900     GOBACK                                                               
093000     .                                                                    
093100     EJECT                                                                
093200                                                                          
093300 A-INIT SECTION.                                                          
093400     SKIP2                                                                
093500     ACCEPT DAGENS-DATUM FROM DATE                                        
093600                                                                          
093700     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
093800     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
093900     CALL WDATKONV USING DAT-KDDATFORM                                    
094000                         DAT-I-TIDATUM                                    
094100                         DAT-O-TIDATUM                                    
094200                         DAT-KDSVAR                                       
094300     IF DAT-KDSVAR-OK                                                     
094400       MOVE DAT-TIAA-VECKA TO W-DAGENS-DATUM-AAR                          
094500       MOVE DAT-TIVV       TO W-DAGENS-DATUM-VECKA                        
094600       MOVE DAT-TIAA-VECKA TO PUBL-AAR                                    
094700       MOVE DAT-TIVV       TO PUBL-VECKA                                  
094800       MOVE DAT-TID        TO PUBL-DAG                                    
094900     END-IF                                                               
095000                                                                          
095100     MOVE 'W1121000'       TO FIL-IDPGM                                   
095200     MOVE DAGENS-DATUM     TO FIL-TIREGDAT                                
095300     MOVE 'W11201  '       TO FIL-IDCPYTXT                                
095400     MOVE ZERO             TO FIL-TIKLOCK                                 
095500                                                                          
095600     PERFORM IMS-RESTART                                                  
095700     .                                                                    
095800     EJECT                                                                
095900                                                                          
096000 B-BEHANDLA-2304 SECTION.                                                 
096100     SKIP2                                                                
096200                                                                          
096300*                *********************************************            
096400*                *** BEHANDLAR 2304-SEGMENT FRÅN HÄNDELSE- ***            
096500*                *** REGISTRET                             ***            
096600*                *********************************************            
096700     MOVE SPACE TO WFELTEXT                                               
096800     MOVE JA TO BEARBETNING-SW                                            
096900     MOVE JA TO BORTTAGNINGS-SW                                           
097000     MOVE JA TO INDATA-SW                                                 
097100     IF (2304-IDARTNR-SATS NOT = ZERO) AND                                
097200        (2304-IDARTNR-ING NOT = ZERO)                                     
097300        PERFORM BA-BEHANDLA-ERSAETTNING-SATS                              
097400        PERFORM BB-BEHANDLA-ERSAETTNING-ART                               
097500     ELSE                                                                 
097600       IF (2304-IDARTNR-SATS NOT = ZERO) AND                              
097700          (2304-IDARTNR-ING = ZERO)                                       
097800          PERFORM BA-BEHANDLA-ERSAETTNING-SATS                            
097900       ELSE                                                               
098000         IF (2304-IDARTNR-SATS = ZERO) AND                                
098100            (2304-IDARTNR-ING NOT = ZERO)                                 
098200               PERFORM BB-BEHANDLA-ERSAETTNING-ART                        
098300         END-IF                                                           
098400       END-IF                                                             
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800                                                                          
098900 BA-BEHANDLA-ERSAETTNING-SATS SECTION.                                    
099000   SKIP2                                                                  
099100******************************************************************        
099200* VID ERSÄTTNING AV SATS BEHANDLAS NYREG TILL EK 01 04 09 52     *        
099300*                    SAMT NÄR ERSÄTTNINGEN GÅR VIDARE TILL > 20  *        
099400* VID RIVNING AV ERSÄTTNING TILL 00 BEHANDLAS EK 01 04 09        *        
099500*                    PREL EK OCH EK > 20                         *        
099600* VID BACKNING AV ERSÄTTNING BEHANDLAS EK > 20 TILL 0X           *        
099700******************************************************************        
099800                                                                          
099900     MOVE 2304-IDARTNR-SATS TO W-IDARTNR                                  
100000                               W-IDARTNR-INLB                             
100100     PERFORM IMS-GET-SATB01-ART                                           
100200     IF SEGMENT-FINNS                                                     
100300       IF SATB-STR-IDLEVNR = '1002 '                                      
100400          MOVE SATB-STR-IDARTNR TO SPAR-STR-IDARTNR                       
100500          MOVE SATB-STR-IDLEVNR TO W-IDLEVNR-INLB                         
100600          MOVE WC-CDC-SE        TO W-IDDC-INLB                            
100700          IF 2304-KDERS-NEW > 2304-KDERS-OLD                              
100800             PERFORM BAD-ERSATTNING-SATS                                  
100900          ELSE                                                            
101000             PERFORM BAE-BACKNING-EK-SATS                                 
101100          END-IF                                                          
101200       END-IF                                                             
101300     ELSE                                                                 
101400       MOVE '005' TO WFELKOD-R07                                          
101500       PERFORM S10-SKRIV-FOV                                              
101600     END-IF                                                               
101700     .                                                                    
101800     EJECT                                                                
101900 BAA-BEHANDLA-RADER SECTION.                                              
102000     SKIP2                                                                
102100******************************************************************        
102200* 2234-TRANS SKICKAS PÅ SAMTLIGA INGÅENDE GÄLLANDE ARTIKLAR      *        
102300* FLIART (WDK601) KONTROLLERAS                                   *        
102400******************************************************************        
102500                                                                          
102600     PERFORM IMS-GET-SATB11                                               
102700     IF SEGMENT-FINNS                                                     
102800       PERFORM UNTIL SEGMENT-SAKNAS                                       
102900         MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                          
103000         MOVE DAGENS-DATUM        TO TMP2-YYMMDD                          
103100         PERFORM WY2000P1                                                 
103200         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
103300           MOVE ZERO             TO 2234-KVPB-SEP-TOT                     
103400                                    2234-REANTPSA-NY                      
103500                                    2234-REANTPSA-GAMMAL                  
103600                                    2234-TIBEHDAT                         
103700           MOVE SPACE            TO 2234-KDISATS                          
103800           MOVE SPAR-STR-IDARTNR TO 2234-IDARTNR-SATS                     
103900           MOVE SATB-RAD-IDARTNR TO 2234-IDARTNR-ING                      
104000           PERFORM IMS-INSERT-2234                                        
104100                                                                          
104200           IF (2304-KDERS-NEW > 20) AND (2304-KDERS-OLD < 20)             
104300              MOVE SATB-RAD-IDARTNR TO W-IDARTNRC                         
104400              PERFORM S04-LAES-CSEQ                                       
104500           END-IF                                                         
104600                                                                          
104700           IF 2304-KDERS-NEW < 20 AND 2304-KDERS-OLD > 20                 
104800              MOVE SATB-RAD-IDARTNR TO W-IDARTNR                          
104900              PERFORM IMS-GET-ROT-601                                     
105000              IF SEGMENT-FINNS                                            
105100                 MOVE JA TO ART-FLIART                                    
105200                 PERFORM IMS-UPPD-601                                     
105300              END-IF                                                      
105400           END-IF                                                         
105500         END-IF                                                           
105600         PERFORM IMS-GET-SATB11                                           
105700       END-PERFORM                                                        
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100                                                                          
106200 BAB-BEARBETA-2234-HUVUD SECTION.                                         
106300     SKIP2                                                                
106400     MOVE SPAR-STR-IDARTNR        TO 2234-IDARTNR-SATS                    
106500                                                                          
106600     IF BEARB-OK                                                          
106700       MOVE SPAR-2234-IDARTNR-RAD TO 2234-IDARTNR-ING                     
106800     ELSE                                                                 
106900       MOVE SATB-RAD-IDARTNR      TO 2234-IDARTNR-ING                     
107000     END-IF                                                               
107100                                                                          
107200     MOVE NEJ TO BEARB-SW                                                 
107300                                                                          
107400     IF BEARBETNING-OK                                                    
107500        MOVE ZERO                 TO 2234-KVPB-SEP-TOT                    
107600     END-IF                                                               
107700     .                                                                    
107800     EJECT                                                                
107900 BABB-BEARBETA-2234 SECTION.                                              
108000     SKIP2                                                                
108100     MOVE ZERO  TO 2234-KVPB-SEP-TOT                                      
108200     MOVE ZERO  TO 2234-REANTPSA-NY                                       
108300     MOVE ZERO  TO 2234-REANTPSA-GAMMAL                                   
108400     MOVE SPACE TO 2234-KDISATS                                           
108500     MOVE ZERO  TO 2234-TIBEHDAT                                          
108600     .                                                                    
108700     EJECT                                                                
108800 BAD-ERSATTNING-SATS SECTION.                                             
108900*                                                                         
109000     IF 2304-KDERS-OLD = ZERO                                             
109100       IF 2304-KDERS-NEW < 21                                             
109200         IF 2304-KDERS-NEW = 02 OR 03 OR 05 OR 06                         
109300                         OR 07 OR 08                                      
109400            CONTINUE                                                      
109500         ELSE                                                             
109600           PERFORM IMS-GET-WDD9-KVBR                                      
109700           IF SEGMENT-FINNS                                               
109800             IF INLB-KVBR = ZERO                                          
109900               PERFORM BAA-BEHANDLA-RADER                                 
110000             ELSE                                                         
110100               MOVE NEJ TO BORTTAGNINGS-SW                                
110200             END-IF                                                       
110300           ELSE                                                           
110400             PERFORM BAA-BEHANDLA-RADER                                   
110500           END-IF                                                         
110600         END-IF                                                           
110700       ELSE                                                               
110800         IF 2304-KDERS-NEW > 20                                           
110900           MOVE DAGENS-DATUM TO SATB-STR-TIBORT                           
111000           PERFORM IMS-UPPD-ART                                           
111100           PERFORM BAA-BEHANDLA-RADER                                     
111200         END-IF                                                           
111300       END-IF                                                             
111400     ELSE                                                                 
111500       IF 2304-KDERS-OLD < 21                                             
111600         IF 2304-KDERS-NEW > 20                                           
111700           MOVE DAGENS-DATUM TO SATB-STR-TIBORT                           
111800           PERFORM IMS-UPPD-ART                                           
111900           PERFORM BAA-BEHANDLA-RADER                                     
112000         END-IF                                                           
112100       END-IF                                                             
112200     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112500 BAE-BACKNING-EK-SATS SECTION.                                            
112600*                                                                         
112700     IF 2304-KDERS-NEW = ZERO                                             
112800        IF 2304-KDERS-OLD = 02 OR 03 OR 05 OR 06 OR 07 OR 08              
112900           CONTINUE                                                       
113000        ELSE                                                              
113100           IF 2304-KDERS-OLD > 20                                         
113200              MOVE ZERO TO SATB-STR-TIBORT                                
113300              PERFORM IMS-UPPD-ART                                        
113400              PERFORM BAA-BEHANDLA-RADER                                  
113500           ELSE                                                           
113600              PERFORM IMS-GET-WDD9-KVBR                                   
113700              IF SEGMENT-FINNS                                            
113800                IF INLB-KVBR = ZERO                                       
113900                   PERFORM BAA-BEHANDLA-RADER                             
114000                END-IF                                                    
114100              ELSE                                                        
114200                PERFORM BAA-BEHANDLA-RADER                                
114300              END-IF                                                      
114400            END-IF                                                        
114500         END-IF                                                           
114600     ELSE                                                                 
114700        IF (2304-KDERS-NEW < 10) AND (2304-KDERS-OLD > 20)                
114800           MOVE ZERO TO SATB-STR-TIBORT                                   
114900           PERFORM IMS-UPPD-ART                                           
115000           PERFORM BAA-BEHANDLA-RADER                                     
115100        END-IF                                                            
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500                                                                          
115600 BB-BEHANDLA-ERSAETTNING-ART SECTION.                                     
115700     SKIP2                                                                
115800*                           **********************************            
115900*                           *** KOLLA BEHANDLINGSBAR KOD   ***            
116000*                           **********************************            
116100                                                                          
116200     MOVE 2304-KDERS-OLD TO OLD-ERSKOD                                    
116300     MOVE 2304-KDERS-NEW TO NEW-ERSKOD                                    
116400                                                                          
116500     MOVE NEJ TO KOD-SW                                                   
116600     SEARCH ALL INGANG                                                    
116700     WHEN GODKAND-KOD(KODTAB-IDEX) = ERSKOD-TRANS                         
116800     MOVE JA TO KOD-SW                                                    
116900     END-SEARCH                                                           
117000                                                                          
117100     IF BEHANDLINGSBAR-KOD                                                
117200       MOVE 2304-IDARTNR-ING TO W-IDARTNRC                                
117300                                W-IDARTNR                                 
117400       PERFORM IMS-GET-SATB-CSEQ-UNIK1                                    
117500       IF SEGMENT-FINNS                                                   
117600         PERFORM UNTIL SEGMENT-SAKNAS                                     
117700           MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                     
117800           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
117900           MOVE SATB01C-STR-IDARTNR TO WS-SPAR-IDARTNR-STR                
118000           MOVE SATB11C-RAD-IDRADNR TO WS-SPAR-IDRADNR                    
118100           PERFORM WY2000P1                                               
118200           IF SATB01C-STR-TIBORT = ZERO       AND                         
118300              SATB01C-STR-IDLEVNR = '1002 '   AND                         
118400              SATB01C-STR-IDARTNR < 100000000 AND                         
118500              TMP1-YYMMDD >= TMP2-YYMMDD                                  
118600              IF 2304-KDERS-NEW > 2304-KDERS-OLD                          
118700                MOVE 2304-IDARTNR-ING TO W-IDARTNR                        
118800                PERFORM BBA-ERSAETT-ARTIKEL                               
118900              ELSE                                                        
119000                PERFORM BBB-BACKA-ERSAETTNING-I-SATS                      
119100              END-IF                                                      
119200           ELSE                                                           
119300              IF 2304-KDERS-NEW = 23 OR 26                                
119400                 MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD               
119500                 MOVE DAGENS-DATUM           TO TMP2-YYMMDD               
119600                 PERFORM WY2000P1                                         
119700                 IF SATB01C-STR-TIBORT = ZERO        AND                  
119800                    SATB01C-STR-IDLEVNR = '1002 '    AND                  
119900                    SATB01C-STR-IDARTNR < 100000000  AND                  
120000                    TMP1-YYMMDD <= TMP2-YYMMDD                            
120100                      MOVE 2304-IDARTNR-ING TO W-IDARTNR                  
120200                      PERFORM BBA-ERSAETT-ARTIKEL                         
120300                  END-IF                                                  
120400              END-IF                                                      
120500           END-IF                                                         
120600           MOVE 2304-IDARTNR-ING TO W-IDARTNRC                            
120700           PERFORM X-TAG-CHECKPOINT-WDJ1                                  
120800*******020607                                                             
120900           IF SEGMENT-SAKNAS                                              
121000              CALL FELLOG                                                 
121100           END-IF                                                         
121200*******020607                                                             
121300           PERFORM IMS-GET-SATB-CSEQ-NEXT                                 
121400         END-PERFORM                                                      
121500       END-IF                                                             
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
122000 BBA-ERSAETT-ARTIKEL SECTION.                                             
122100     SKIP2                                                                
122200                                                                          
122300*                     ****************************************            
122400*                     *** ERSÄTTNING AV INGÅENDE ARTIKEL   ***            
122500*                     ****************************************            
122600                                                                          
122700*************************************** ENTYDIG ERSÄTTNING                
122800                                                                          
122900     IF 2304-KDERS-NEW = 01 OR 02 OR 03                                   
123000       PERFORM BBAA-LAES-ERSAETTNINGS-REG                                 
123100       IF ERS-TILLK-FINNS                                                 
123200         PERFORM BBAB-BERAKNA-TID                                         
123300         PERFORM BBAC-UPPD-ERSAETTNING-ENTYDIG                            
123400         PERFORM BBACD-KOPIERA-KONV-TILL-OKONV                            
123500       END-IF                                                             
123600     END-IF                                                               
123700                                                                          
123800*************************************** ALTERNATIV ERSÄTTNING             
123900                                                                          
124000     IF 2304-KDERS-NEW = 04 OR 05 OR 06                                   
124100       PERFORM BBAA-LAES-ERSAETTNINGS-REG                                 
124200       IF ERS-TILLK-FINNS                                                 
124300         PERFORM BBAB-BERAKNA-TID                                         
124400         PERFORM BBAD-UPPD-ERSAETT-ALTERNATIV                             
124500       END-IF                                                             
124600     END-IF                                                               
124700                                                                          
124800*************************************** DEFINITIV ERSÄTTNING              
124900                                                                          
125000     IF 2304-KDERS-NEW = 52                                               
125100       PERFORM BBAE-UPPD-ERSAETTNING-52                                   
125200     ELSE                                                                 
125300       IF 2304-KDERS-NEW > 10                                             
125400         PERFORM BBAA-LAES-ERSAETTNINGS-REG                               
125500         IF ERS-TILLK-FINNS                                               
125600           PERFORM BBAF-UPPD-ERSAETT-DEFINITIV                            
125700         END-IF                                                           
125800       END-IF                                                             
125900     END-IF                                                               
126000     .                                                                    
126100     EJECT                                                                
126200                                                                          
126300 BBAA-LAES-ERSAETTNINGS-REG SECTION.                                      
126400     SKIP2                                                                
126500                                                                          
126600*                        ************************************             
126700*                        *** LÄS ERSÄTTNINGSREGISTER    ****              
126800*                        ************************************             
126900                                                                          
127000     PERFORM IMS-GET-ERSAETTNING-ROT                                      
127100     IF SEGMENT-FINNS                                                     
127200       MOVE JA TO ERS-ART-SW                                              
127300       MOVE ERSA-IDARTNR TO ERS-IDARTNR(1)                                
127400       MOVE ERSA-DIERS-ERS TO ERS-DIERS(1)                                
127500       MOVE +1 TO INDX                                                    
127600       PERFORM IMS-GET-ERSAETTNING-TILLK                                  
127700       IF SEGMENT-FINNS                                                   
127800         PERFORM UNTIL SEGMENT-SAKNAS                                     
127900           IF ERSA-FLTEXT = JA                                            
128000             PERFORM IMS-GET-ERSAETTNING-TILLK                            
128100           ELSE                                                           
128200             ADD +1 TO INDX                                               
128300             MOVE ERSA-IDARTNR-TILLK TO ERS-IDARTNR(INDX)                 
128400             MOVE ERSA-DIERS-TILLK TO ERS-DIERS(INDX)                     
128500             PERFORM IMS-GET-ERSAETTNING-TILLK                            
128600           END-IF                                                         
128700         END-PERFORM                                                      
128800         MOVE INDX TO ERS-TABANT                                          
128900         PERFORM IMS-GET-ERSAETTNING-TID                                  
129000         IF SEGMENT-FINNS                                                 
129100           MOVE ERSA-TIERSDAT-REG TO ERS-TIERSDAT-REG                     
129200           MOVE ERSA-TIERSDAT-PREL-C1 TO ERS-TIERSDAT-PREL                
129300         ELSE                                                             
129400           MOVE ZERO                  TO ERS-TIERSDAT-REG                 
129500                                         ERS-TIERSDAT-PREL                
129600         END-IF                                                           
129700       END-IF                                                             
129800     ELSE                                                                 
129900       MOVE NEJ TO ERS-ART-SW                                             
130000     END-IF                                                               
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 BBAB-BERAKNA-TID SECTION.                                                
130500     SKIP2                                                                
130600*                     ****************************************            
130700*                     *** BERÄKNA TID FÖR ERSÄTTNING       ***            
130800*                     ****************************************            
130900                                                                          
131000     MOVE ERS-TIERSDAT-PREL TO W-PUBLDAT                                  
131100     IF 2304-KDERS-NEW = 01 OR 04                                         
131200       PERFORM BBABA-TID-ENL-FORMEL                                       
131300     ELSE                                                                 
131400       MOVE ERS-TIERSDAT-PREL4 TO ERS-DATUM-X                             
131500       MOVE '1' TO W-ERS-DATUM-DAG                                        
131600     END-IF                                                               
131700     .                                                                    
131800     EJECT                                                                
131900                                                                          
132000 BBABA-TID-ENL-FORMEL SECTION.                                            
132100     SKIP2                                                                
132200*                    *****************************************            
132300*                    *** ERSÄTTNINGS-DATUM BERÄKNAS ENLIGT ***            
132400*                    *** FORMEL. GÄLLER ERSKOD 1 OCH 4     ***            
132500*                    *** BEHOV FRÅN BEHOVSTABELL           ***            
132600*                    *****************************************            
132700                                                                          
132800     MOVE ERS-IDARTNR (1) TO W-IDARTNR                                    
132900     PERFORM IMS-GET-ROT-601                                              
133000     IF SEGMENT-FINNS                                                     
133100       PERFORM IMS-GET-ARTC11                                             
133200       IF SEGMENT-FINNS                                                   
133300         MOVE CLAG-KVLS    TO SPAR-KVLS-C1                                
133400         MOVE CLAG-KVRESS  TO SPAR-KVRESS-C1                              
133500         MOVE CLAG-KVROS   TO SPAR-KVROS-C1                               
133600         COMPUTE SPAR-KVAKS ROUNDED =                                     
133700              CLAG-KVAKS-CDC + CLAG-KVAKS-PAV                             
133800                          + CLAG-KVAKS-T                                  
133900       END-IF                                                             
134000     END-IF                                                               
134100     COMPUTE WDISP-LAGER ROUNDED = SPAR-KVLS-C1 - SPAR-KVRESS-C1          
134200     + SPAR-KVAKS - SPAR-KVROS-C1                                         
134300                                                                          
134400*************************** BEHOVSTABELLEN LÄSES ETT HALVÅR               
134500*                           I STÖTEN FÖR ATT SE HUR LÄNGE                 
134600*                           DISPONIBELT LAGER RÄCKER                      
134700                                                                          
134800     MOVE NEJ             TO TIDBER-SW                                    
134900     MOVE ERS-IDARTNR (1) TO LINK-IDARTNR                                 
135000     MOVE SPACE           TO LINK-IDDC                                    
135100     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
135200     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
135300     CALL WDATKONV USING  DAT-KDDATFORM                                   
135400                          DAT-I-TIDATUM                                   
135500                          DAT-O-TIDATUM                                   
135600                          DAT-KDSVAR                                      
135700     IF DAT-KDSVAR-OK                                                     
135800       MOVE DAT-TIAA-VECKA TO WS-AAR                                      
135900       MOVE DAT-TIVV       TO WS-VECKA                                    
136000     END-IF                                                               
136100                                                                          
136200     MOVE ZERO             TO LINK-TID-AKTUELL                            
136300     MOVE WS-DATUM         TO LINK-TIAAVV-AKTUELL                         
136400     MOVE WS-DATUM         TO LINK-TIBEHOV-START                          
136500     MOVE +26              TO LINK-KVVECKOR-BEHOV                         
136600     MOVE '17'             TO LINK-KDBEHOV                                
136700     MOVE NEJ              TO LINK-FLINKLDIRLEV                           
136800     MOVE ZERO             TO VARV-TIDBER                                 
136900                                                                          
137000     PERFORM UNTIL TIDBER-KLAR                                            
137100       CALL W2222200 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB          
137200                           W222-ARTM-PCB W222-2501-PCB                    
137300                           W222-WDB6R-PCB W222-WDK7R-PCB                  
137400                           W222-WDB6-PCB W222-WDD7-PCB                    
137500                           W222-WDK7E-PCB                                 
137600                           W222-UTIL-WDK6-PCB                             
137700                           W222-UTIL-WDK7-PCB                             
137800                           W222-UTIL-WDB6-PCB                             
137900                           W222-UTUP-WDK7-PCB                             
138000                           W222-UTUP-WDB6-PCB                             
138100                           W222-UTUP-UTIL-WDK6-PCB                        
138200                           W222-UTUP-UTIL-WDK7-PCB                        
138300                           W222-UTUP-UTIL-WDB6-PCB                        
138400                                                                          
138500       IF LINK-ANROP-OK                                                   
138600         ADD +1           TO VARV-TIDBER                                  
138700         MOVE ZERO        TO BEHIDEX                                      
138800         PERFORM UNTIL TIDBER-KLAR OR BEHIDEX > 25                        
138900           ADD +1         TO BEHIDEX                                      
139000           SUBTRACT LINK-KVBEHOV-VECKA (BEHIDEX)                          
139100           FROM WDISP-LAGER                                               
139200           IF WDISP-LAGER <= ZERO                                         
139300             ADD BEHIDEX -1 TO WS-VECKA                                   
139400             IF WS-VECKA > 52                                             
139500               ADD  1       TO WS-AAR                                     
139600               SUBTRACT  52 FROM WS-VECKA                                 
139700             END-IF                                                       
139800             MOVE JA      TO TIDBER-SW                                    
139900           END-IF                                                         
140000         END-PERFORM                                                      
140100         IF TIDBER-EJ-KLAR                                                
140200           ADD  26          TO WS-VECKA                                   
140300           IF WS-VECKA > 52                                               
140400             ADD  1         TO WS-AAR                                     
140500             SUBTRACT  52   FROM WS-VECKA                                 
140600           END-IF                                                         
140700           MOVE WS-DATUM    TO LINK-TIBEHOV-START                         
140800         END-IF                                                           
140900         IF VARV-TIDBER = 3                                               
141000           MOVE JA          TO TIDBER-SW                                  
141100         END-IF                                                           
141200       ELSE                                                               
141300         MOVE '015'            TO WFELKOD-R07                             
141400         MOVE TEXT2            TO WFELTEXT                                
141500         MOVE 2304-IDARTNR-ING TO WARTNR                                  
141600         MOVE MASKINELL        TO WMASKINELL                              
141700         PERFORM S10-SKRIV-FOV                                            
141800         MOVE WS-AAR           TO W-AAR                                   
141900         MOVE WS-VECKA         TO W-VECKA                                 
142000         MOVE JA               TO TIDBER-SW                               
142100       END-IF                                                             
142200     END-PERFORM                                                          
142300                                                                          
142400**************************** TEST MOT PUBLICERINGSDAT OM KDERS            
142500*                            = 1 OCH OM PUBLDAT - ERSDAT > 6              
142600*                            SÄTT ERSDAT TILL PUBLDAT - 6                 
142700                                                                          
142800     IF 2304-KDERS-NEW = 1                                                
142900       MOVE W-PUBL-AA    TO TMP1-YY                                       
143000       MOVE WS-AAR       TO TMP2-YY                                       
143100       PERFORM WY2000P9                                                   
143200       COMPUTE W-VECKOREST =                                              
143300               (TMP1-YY - TMP2-YY) * 52 + W-PUBL-VV - WS-VECKA            
143400       IF W-VECKOREST > 6                                                 
143500         IF W-PUBL-VV < 7                                                 
143600           IF W-PUBL-AA = 00                                              
143700             MOVE 99 TO W-PUBL-AA                                         
143800           ELSE                                                           
143900             SUBTRACT 1 FROM W-PUBL-AA                                    
144000           END-IF                                                         
144100           ADD  46      TO W-PUBL-VV                                      
144200         ELSE                                                             
144300           SUBTRACT 6 FROM W-PUBL-VV                                      
144400         END-IF                                                           
144500         MOVE W-PUBL-AA   TO WS-AAR                                       
144600         MOVE W-PUBL-VV   TO WS-VECKA                                     
144700         MOVE WS-DATUM    TO ERS-DATUM-X                                  
144800       ELSE                                                               
144900         MOVE WS-DATUM    TO ERS-DATUM-X                                  
145000       END-IF                                                             
145100     ELSE                                                                 
145200       MOVE WS-DATUM        TO ERS-DATUM-X                                
145300     END-IF                                                               
145400                                                                          
145500     MOVE '1' TO W-ERS-DATUM-DAG                                          
145600     .                                                                    
145700     EJECT                                                                
145800                                                                          
145900 BBAC-UPPD-ERSAETTNING-ENTYDIG SECTION.                                   
146000     SKIP2                                                                
146100                                                                          
146200*                           **********************************            
146300*                           *** UPPDATERA ERSÄTTNING ENTYDIG *            
146400*                           **********************************            
146500                                                                          
146600     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR                                
146700                                 SPAR-STR-IDARTNR                         
146800                                 WS-STR-IDARTNR                           
146900                                 SPAR-IDARTNR-STRUKTUR                    
147000     MOVE SATB11C-RAD-IDARTNR TO KOLLA-IDARTNR                            
147100     COMPUTE IDARTNR-KONVERTERAD-WS = +999999999 -                        
147200             WS-STR-IDARTNR                                               
147300     MOVE W-IDARTNR TO W-IDARTNR-OKONV                                    
147400     PERFORM IMS-GET-SATB01-OKONV                                         
147500     IF SEGMENT-FINNS                                                     
147600       MOVE IO-AREA-OKONV TO IO-AREA-KONV                                 
147700       MOVE IDARTNR-KONVERTERAD-WS TO SATB01K-STR-IDARTNR                 
147800                                      W-IDARTNR-KONV                      
147900       MOVE 'W1121000' TO SATB01K-STR-IDUSER                              
148000       MOVE DAGENS-DATUM TO SATB01K-STR-TIREGDAT                          
148100       PERFORM IMS-ISRT-SATB01-KONV                                       
148200     END-IF                                                               
148300                                                                          
148400******* 020607                                                            
148500     MOVE ZERO  TO WS-IDRADNR-CHECK                                       
148600                   WS-TISTODAT-CHECK                                      
148700     MOVE SPACE TO WS-KDISATS-CHECK                                       
148800******* 020607                                                            
148900                                                                          
149000     MOVE +10 TO WS-IDRADNR                                               
149100     MOVE +9999 TO WS-IDRADNR-FORPACKNING                                 
149200     PERFORM IMS-GET-SATB11-OKONV                                         
149300     PERFORM UNTIL SEGMENT-SAKNAS                                         
149400       IF SEGMENT-FINNS                                                   
149500         MOVE SATB11O-RAD-KDSTRRAD TO W-KDSTRRAD-OKONV                    
149600                                      W-KDSTRRAD-KONV                     
149700                                      WS-KDSTRRAD                         
149800         MOVE SATB11O-RAD-IDRADNR  TO W-IDRADNR-OKONV                     
149900         MOVE WS-IDRADNR           TO W-IDRADNR-KONV                      
150000         MOVE SATB11O-RAD-TISTODAT   TO TMP1-YYMMDD                       
150100         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
150200         PERFORM WY2000P1                                                 
150300         IF (SATB11O-RAD-KDISATS = 'N' OR 'T' OR ' ') AND                 
150400            SATB11O-RAD-IDARTNR = KOLLA-IDARTNR      AND                  
150500            TMP1-YYMMDD > TMP2-YYMMDD                                     
150600            MOVE SATB11O-RAD-REANTPSA TO ERS-KVANT-I-SATS                 
150700            MOVE JA TO UPPDATERING-KONV-SW                                
150800            PERFORM BBACC-BEARBETA-ERSAETT-RAD                            
150900            IF UPPDATERING-KONV-OK                                        
151000              PERFORM BBACB-KOPIERA-RAD-BEH-NOT                           
151100            END-IF                                                        
151200            ADD +10 TO WS-IDRADNR                                         
151300            PERFORM BBACA-LAEGG-UPP-TILLK-ART                             
151400         ELSE                                                             
151500           MOVE NEJ TO UPPDATERING-KONV-SW                                
151600           PERFORM BBACB-KOPIERA-RAD-BEH-NOT                              
151700           ADD +10 TO WS-IDRADNR                                          
151800         END-IF                                                           
151900         PERFORM IMS-GET-SATB11-OKONV                                     
152000       END-IF                                                             
152100     END-PERFORM                                                          
152200     .                                                                    
152300     EJECT                                                                
152400                                                                          
152500 BBACA-LAEGG-UPP-TILLK-ART SECTION.                                       
152600     SKIP2                                                                
152700     MOVE +2 TO INDX                                                      
152800     PERFORM UNTIL INDX > ERS-TABANT                                      
152900       COMPUTE SPAR-REANTPSA ROUNDED =                                    
153000       ERS-KVANT-I-SATS * ERS-DIERS (INDX)                                
153100       / ERS-DIERS (1)                                                    
153200       MOVE 'T' TO SPAR-KDISATS                                           
153300                                                                          
153400       MOVE ERS-DATUM-KOLL TO DAT-I-TIDATUM                               
153500       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
153600       CALL WDATKONV USING DAT-KDDATFORM                                  
153700                           DAT-I-TIDATUM                                  
153800                           DAT-O-TIDATUM                                  
153900                           DAT-KDSVAR                                     
154000       IF DAT-KDSVAR-OK                                                   
154100         MOVE DAT-TIAA TO SPAR-AAR                                        
154200         MOVE DAT-TIMM TO SPAR-MAN                                        
154300         MOVE DAT-TIDD TO SPAR-DAG                                        
154400       END-IF                                                             
154500                                                                          
154600       MOVE SPAR-DATUM TO SPAR-TISTADAT                                   
154700       MOVE ERS-IDARTNR (INDX) TO SPAR-IDARTNR-ING                        
154800       MOVE SPAR-IDARTNR-ING TO SPAR-IDARTNR-TILLK                        
154900                                                                          
155000       IF SPAR-IDARTNR-ING = SPAR-IDARTNR-STRUKTUR                        
155100         MOVE NEJ TO INDATA-SW                                            
155200         MOVE '015' TO WFELKOD-R07                                        
155300         MOVE TEXT10 TO WFELTEXT                                          
155400         MOVE SPAR-IDARTNR-STRUKTUR TO WARTNR                             
155500         PERFORM S10-SKRIV-FOV                                            
155600       END-IF                                                             
155700                                                                          
155800       IF INDATA-OK                                                       
155900         MOVE SPAR-IDARTNR-ING TO W-IDARTNRC                              
156000         PERFORM S13-KOLL-SATS-EJ-I-TILLK                                 
156100         IF INDATA-OK                                                     
156200           MOVE SPAR-IDARTNR-STRUKTUR TO W-IDARTNRC                       
156300           PERFORM S14-KOLL-STRNR-EJ-I-EGEN                               
156400                                                                          
156500           IF INDATA-OK                                                   
156600             MOVE ERS-IDARTNR (INDX) TO W-IDARTNR                         
156700                                        SPAR-2234-IDARTNR-RAD             
156800             MOVE JA TO BEARB-SW                                          
156900             PERFORM BBACE-FLYTTA-DATA-TILL-ARTIKEL                       
157000             MOVE NEJ TO BEARBETNING-SW                                   
157100             PERFORM BAB-BEARBETA-2234-HUVUD                              
157200             PERFORM BABB-BEARBETA-2234                                   
157300             PERFORM IMS-INSERT-2234                                      
157400             MOVE SPAR-IDARTNR-ING TO W-IDARTNR                           
157500             PERFORM IMS-GET-ROT-601                                      
157600             IF SEGMENT-FINNS                                             
157700                MOVE JA TO ART-FLIART                                     
157800                PERFORM IMS-UPPD-601                                      
157900             END-IF                                                       
158000           END-IF                                                         
158100         END-IF                                                           
158200       END-IF                                                             
158300       ADD +1 TO INDX                                                     
158400     END-PERFORM                                                          
158500     .                                                                    
158600     EJECT                                                                
158700                                                                          
158800 BBACB-KOPIERA-RAD-BEH-NOT SECTION.                                       
158900     SKIP2                                                                
159000                                                                          
159100     MOVE IO-AREA-OKONV TO IO-AREA-KONV                                   
159200                                                                          
159300     IF UPPDATERING-KONV-OK                                               
159400       MOVE KONV-TISTODAT TO SATB11K-RAD-TISTODAT                         
159500       MOVE KONV-KDISATS  TO SATB11K-RAD-KDISATS                          
159600     END-IF                                                               
159700                                                                          
159800     MOVE WS-IDRADNR TO SATB11K-RAD-IDRADNR                               
159900                                                                          
160000******** 020607 NYA RADNR I SATSEN SPARAS                                 
160100     IF UPPDATERING-KONV-OK                                               
160200       MOVE SATB11K-RAD-IDRADNR  TO WS-IDRADNR-CHECK                      
160300       MOVE SATB11K-RAD-TISTODAT TO WS-TISTODAT-CHECK                     
160400       MOVE SATB11K-RAD-KDISATS  TO WS-KDISATS-CHECK                      
160500     END-IF                                                               
160600******** 020607                                                           
160700                                                                          
160800     PERFORM IMS-ISRT-SATB11-KONV                                         
160900*************************** BODIL 970813                                  
161000*    DISPLAY 'BBACB-' SATB11K-RAD-IDARTNR                                 
161100*************************** BODIL 970813                                  
161200                                                                          
161300     PERFORM S21-KOPIERA-NOTERINGSEGMENT                                  
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700 BBACC-BEARBETA-ERSAETT-RAD SECTION.                                      
161800     SKIP2                                                                
161900     MOVE ERS-DATUM-KOLL TO DAT-I-TIDATUM                                 
162000     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
162100     CALL WDATKONV USING DAT-KDDATFORM                                    
162200                         DAT-I-TIDATUM                                    
162300                         DAT-O-TIDATUM                                    
162400                         DAT-KDSVAR                                       
162500     IF DAT-KDSVAR-OK                                                     
162600       MOVE DAT-TIAA TO ERS-AAR                                           
162700       MOVE DAT-TIMM TO ERS-MAN                                           
162800       MOVE DAT-TIDD TO ERS-DAG                                           
162900     END-IF                                                               
163000                                                                          
163100     MOVE NEJ TO BEARBETNING-SW                                           
163200     MOVE KOLLA-IDARTNR TO SPAR-2234-IDARTNR-RAD                          
163300     MOVE JA TO BEARB-SW                                                  
163400     PERFORM BAB-BEARBETA-2234-HUVUD                                      
163500     PERFORM BABB-BEARBETA-2234                                           
163600     PERFORM IMS-INSERT-2234                                              
163700                                                                          
163800     MOVE ERSAETTNINGS-DATUM TO KONV-TISTODAT                             
163900     MOVE 'E' TO KONV-KDISATS                                             
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300 BBACD-KOPIERA-KONV-TILL-OKONV SECTION.                                   
164400     SKIP2                                                                
164500     MOVE ZERO TO WS-IDRADNR                                              
164600     PERFORM IMS-GET-SATB01-OKONV                                         
164700     PERFORM IMS-DLET-SATB-OKONV                                          
164800     MOVE DAGENS-DATUM TO SATB01O-STR-TIUPPDAT                            
164900     IF ERS-TABANT > 2                                                    
165000        MOVE 'J'       TO SATB01O-STR-FLFORPQ                             
165100     END-IF                                                               
165200     PERFORM IMS-ISRT-SATB01-OKONV                                        
165300                                                                          
165400     PERFORM IMS-GET-SATB01-KONV                                          
165500     PERFORM IMS-GET-SATB11-KONV                                          
165600     PERFORM UNTIL SEGMENT-SAKNAS                                         
165700       IF SATB11K-RAD-KDSTRRAD = '9'                                      
165800******* ÄNDRA RADNUMMER                                                   
165900         MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                     
166000                                      W-KDSTRRAD-OKONV                    
166100         MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                      
166200         MOVE IO-AREA-KONV TO IO-AREA-OKONV                               
166300         MOVE WS-IDRADNR-FORPACKNING TO SATB11O-RAD-IDRADNR               
166400                                        W-IDRADNR-OKONV                   
166500*******  020607                                                           
166600         IF SATB11K-RAD-IDARTNR = 2304-IDARTNR-ING                        
166700         AND SATB11K-RAD-IDRADNR  = WS-IDRADNR-CHECK                      
166800         AND SATB11K-RAD-KDISATS  = WS-KDISATS-CHECK                      
166900         AND SATB11K-RAD-TISTODAT = WS-TISTODAT-CHECK                     
167000            MOVE WS-IDRADNR-FORPACKNING TO WS-SPAR-IDRADNR                
167100         END-IF                                                           
167200*******  020607                                                           
167300         ADD +10 TO WS-IDRADNR-FORPACKNING                                
167400       ELSE                                                               
167500*******  020607                                                           
167600         IF SATB11K-RAD-IDARTNR = 2304-IDARTNR-ING                        
167700         AND SATB11K-RAD-IDRADNR  = WS-IDRADNR-CHECK                      
167800         AND SATB11K-RAD-KDISATS  = WS-KDISATS-CHECK                      
167900         AND SATB11K-RAD-TISTODAT = WS-TISTODAT-CHECK                     
168000            MOVE WS-IDRADNR-CHECK TO WS-SPAR-IDRADNR                      
168100         END-IF                                                           
168200*******  020607                                                           
168300         MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                     
168400                                      W-KDSTRRAD-OKONV                    
168500         MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                      
168600                                      W-IDRADNR-OKONV                     
168700         MOVE IO-AREA-KONV TO IO-AREA-OKONV                               
168800                                                                          
168900********** NÄSTA RADNR SPARAS UNDAN FÖR ATT KUNNA NUMRERA                 
169000********** OM EVENTUELLA FÖRPACKNINGSRADER                                
169100         MOVE ZERO TO WS-IDRADNR-FORPACKNING                              
169200         COMPUTE WS-IDRADNR-FORPACKNING = SATB11K-RAD-IDRADNR             
169300                 + 10                                                     
169400       END-IF                                                             
169500                                                                          
169600       PERFORM IMS-ISRT-SATB11-OKONV                                      
169700                                                                          
169800                                                                          
169900       PERFORM IMS-GET-SATB22-KONV                                        
170000       PERFORM UNTIL SEGMENT-SAKNAS                                       
170100         IF SEGMENT-FINNS                                                 
170200           MOVE IO-AREA-KONV TO IO-AREA-OKONV                             
170300           PERFORM IMS-ISRT-SATB22-OKONV                                  
170400           PERFORM IMS-GET-SATB22-KONV                                    
170500         END-IF                                                           
170600       END-PERFORM                                                        
170700       PERFORM IMS-GET-SATB11-KONV                                        
170800     END-PERFORM                                                          
170900                                                                          
171000     PERFORM IMS-GET-SATB01-KONV                                          
171100     PERFORM IMS-DLET-SATB-KONV                                           
171200     MOVE KOLLA-IDARTNR TO W-IDARTNRC                                     
171300     .                                                                    
171400     EJECT                                                                
171500                                                                          
171600 BBACE-FLYTTA-DATA-TILL-ARTIKEL SECTION.                                  
171700     SKIP2                                                                
171800     MOVE NEJ TO TILLK-ARTIKEL-FORPACKNING-SW                             
171900     PERFORM IMS-GET-ARTC01-2                                             
172000     IF SEGMENT-FINNS                                                     
172100       MOVE ART2-ART-KDPRODSL                                             
172200                            TO TEST-KDPRODSL                              
172300       IF KDPRODSL-VOLVO-EMB                                              
172400         MOVE JA TO TILLK-ARTIKEL-FORPACKNING-SW                          
172500       END-IF                                                             
172600     END-IF                                                               
172700                                                                          
172800     IF TILLK-ARTIKEL-FORPACKNING                                         
172900       MOVE '9'             TO SATB11K-RAD-KDSTRRAD                       
173000                               W-KDSTRRAD-KONV                            
173100       MOVE WS-IDRADNR-FORPACKNING TO SATB11K-RAD-IDRADNR                 
173200                                      W-IDRADNR-KONV                      
173300     ELSE                                                                 
173400       MOVE ZERO            TO SATB11K-RAD-KDSTRRAD                       
173500                               W-KDSTRRAD-KONV                            
173600       MOVE WS-IDRADNR      TO SATB11K-RAD-IDRADNR                        
173700                               W-IDRADNR-KONV                             
173800     END-IF                                                               
173900                                                                          
174000     MOVE SPACE             TO SATB11K-RAD-IDLEVNR                        
174100                               SATB11K-RAD-BELEVART                       
174200     MOVE SPAR-IDARTNR-ING  TO SATB11K-RAD-IDARTNR                        
174300     MOVE SPACE             TO SATB11K-RAD-BEART-SVE                      
174400                               SATB11K-RAD-IDAO-STA                       
174500                               SATB11K-RAD-IDAO-STO                       
174600                                                                          
174700     MOVE SPAR-IDARTNR-ING TO W-IDARTNR                                   
174800     PERFORM IMS-GET-SATB01-ART                                           
174900     IF SEGMENT-FINNS                                                     
175000       MOVE SATB-STR-IDSTRTYP TO SATB11K-RAD-IDSTRTYP                     
175100     ELSE                                                                 
175200       MOVE SPACE             TO SATB11K-RAD-IDSTRTYP                     
175300     END-IF                                                               
175400                                                                          
175500     MOVE ZERO              TO SATB11K-RAD-KDBENHOM                       
175600     MOVE SPAR-KDISATS      TO SATB11K-RAD-KDISATS                        
175700     MOVE SPACE             TO SATB11K-RAD-KDSORT                         
175800     MOVE SPAR-REANTPSA     TO SATB11K-RAD-REANTPSA                       
175900     MOVE DAGENS-DATUM      TO SATB11K-RAD-TIREGDAT                       
176000     MOVE SPAR-TISTADAT     TO SATB11K-RAD-TISTADAT                       
176100     MOVE  +999999          TO SATB11K-RAD-TISTODAT                       
176200                                                                          
176300     IF TILLK-ARTIKEL-FORPACKNING                                         
176400       SUBTRACT 1 FROM WS-IDRADNR-FORPACKNING                             
176500     ELSE                                                                 
176600       ADD 10 TO WS-IDRADNR                                               
176700     END-IF                                                               
176800*************************** BODIL 970813                                  
176900*    DISPLAY 'BBACE-' SATB11K-RAD-IDARTNR                                 
177000*************************** BODIL 970813                                  
177100                                                                          
177200     PERFORM IMS-ISRT-SATB11-KONV                                         
177300     .                                                                    
177400     EJECT                                                                
177500                                                                          
177600                                                                          
177700 BBAD-UPPD-ERSAETT-ALTERNATIV SECTION.                                    
177800     SKIP2                                                                
177900                                                                          
178000*                           **********************************            
178100*                           *** UPPDATERA ERSÄTTNING       ***            
178200*                           *** ALTERNATIV                 ***            
178300*                           **********************************            
178400                                                                          
178500     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR                                
178600                                 SPAR-STR-IDARTNR                         
178700     PERFORM IMS-GET-SATB01-ART                                           
178800     IF SEGMENT-FINNS                                                     
178900       MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                             
179000       MOVE 'J' TO SATB-STR-FLFORPQ                                       
179100       PERFORM IMS-UPPD-ART                                               
179200       PERFORM IMS-GET-SATB11                                             
179300       IF SEGMENT-FINNS                                                   
179400         PERFORM UNTIL SEGMENT-SAKNAS                                     
179500           MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                        
179600           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
179700           PERFORM WY2000P1                                               
179800           IF SATB-RAD-KDISATS = 'N' OR 'T' OR ' '   AND                  
179900              SATB-RAD-IDARTNR = SPAR-IDARTNR-ING1   AND                  
180000              TMP1-YYMMDD > TMP2-YYMMDD                                   
180100             MOVE ERS-DATUM-KOLL TO DAT-I-TIDATUM                         
180200             MOVE 'AAVVD' TO DAT-KDDATFORM                                
180300             CALL WDATKONV USING DAT-KDDATFORM                            
180400                                 DAT-I-TIDATUM                            
180500                                 DAT-O-TIDATUM                            
180600                                 DAT-KDSVAR                               
180700             IF DAT-KDSVAR-OK                                             
180800               MOVE DAT-TIAA TO ERS-AAR                                   
180900               MOVE DAT-TIMM TO ERS-MAN                                   
181000               MOVE DAT-TIDD TO ERS-DAG                                   
181100             END-IF                                                       
181200                                                                          
181300             MOVE ERSAETTNINGS-DATUM TO SATB-RAD-TISTODAT                 
181400             MOVE 'E' TO SATB-RAD-KDISATS                                 
181500             PERFORM IMS-UPPD-RAD                                         
181600             MOVE NEJ TO BEARBETNING-SW                                   
181700             PERFORM BAB-BEARBETA-2234-HUVUD                              
181800             PERFORM BABB-BEARBETA-2234                                   
181900             PERFORM IMS-INSERT-2234                                      
182000                                                                          
182100             MOVE '015' TO WFELKOD-R07                                    
182200             MOVE TEXT6 TO WFELTEXT                                       
182300             MOVE 2304-IDARTNR-ING TO WARTNR                              
182400             MOVE MASKINELL TO WMASKINELL                                 
182500             MOVE 'W' TO WVARNING                                         
182600             PERFORM S10-SKRIV-FOV                                        
182700           END-IF                                                         
182800           PERFORM IMS-GET-SATB11                                         
182900         END-PERFORM                                                      
183000       ELSE                                                               
183100         MOVE '015' TO WFELKOD-R07                                        
183200         MOVE TEXT3 TO WFELTEXT                                           
183300         MOVE 2304-IDARTNR-ING TO WARTNR                                  
183400         MOVE MASKINELL TO WMASKINELL                                     
183500         PERFORM S10-SKRIV-FOV                                            
183600       END-IF                                                             
183700     END-IF                                                               
183800     .                                                                    
183900     EJECT                                                                
184000                                                                          
184100 BBAE-UPPD-ERSAETTNING-52 SECTION.                                        
184200     SKIP2                                                                
184300*                            *********************************            
184400*                            *** ARTIKEL RAD BLIR HISTORIKRAD*            
184500*                            *********************************            
184600     MOVE SATB11C-RAD-IDARTNR TO KOLL-HIST-IDARTNR                        
184700     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR                                
184800                                 SPAR-STR-IDARTNR                         
184900     PERFORM IMS-GET-SATB01-ART                                           
185000     IF SEGMENT-FINNS                                                     
185100       MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                             
185200       MOVE 'J' TO SATB-STR-FLFORPQ                                       
185300       PERFORM IMS-UPPD-ART                                               
185400       PERFORM IMS-GET-SATB11                                             
185500       IF SEGMENT-FINNS                                                   
185600         PERFORM UNTIL SEGMENT-SAKNAS                                     
185700           IF SATB-RAD-IDARTNR = KOLL-HIST-IDARTNR                        
185800             MOVE 2304-TIERSDAT-PREL TO DAT-I-TIDATUM                     
185900             MOVE 'AAVVD' TO DAT-KDDATFORM                                
186000             CALL WDATKONV USING DAT-KDDATFORM                            
186100                                 DAT-I-TIDATUM                            
186200                                 DAT-O-TIDATUM                            
186300                                 DAT-KDSVAR                               
186400             IF DAT-KDSVAR-OK                                             
186500               MOVE DAT-TIAA TO TIERSDAT-AAR                              
186600               MOVE DAT-TIMM TO TIERSDAT-MAN                              
186700               MOVE DAT-TIDD TO TIERSDAT-DAG                              
186800             END-IF                                                       
186900                                                                          
187000             PERFORM BAB-BEARBETA-2234-HUVUD                              
187100             MOVE ZERO           TO 2234-REANTPSA-NY                      
187200             IF SATB-RAD-KDISATS = 'U'                                    
187300               MOVE ZERO         TO 2234-REANTPSA-GAMMAL                  
187400             ELSE                                                         
187500               MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-GAMMAL             
187600             END-IF                                                       
187700             MOVE 'D'            TO 2234-KDISATS                          
187800             PERFORM S02-BEARBETA-STOPPDATUM                              
187900             PERFORM IMS-INSERT-2234                                      
188000                                                                          
188100             MOVE TIERSDAT-DATUM TO SATB-RAD-TISTODAT                     
188200             MOVE SPACE          TO SATB-RAD-KDISATS                      
188300             PERFORM IMS-UPPD-RAD                                         
188400             PERFORM IMS-GET-SATB11                                       
188500           ELSE                                                           
188600             PERFORM IMS-GET-SATB11                                       
188700           END-IF                                                         
188800         END-PERFORM                                                      
188900       END-IF                                                             
189000     END-IF                                                               
189100                                                                          
189200     PERFORM IMS-GET-ROT-601                                              
189300     IF SEGMENT-FINNS                                                     
189400       MOVE NEJ TO ART-FLIART                                             
189500       PERFORM IMS-UPPD-601                                               
189600     END-IF                                                               
189700                                                                          
189800     .                                                                    
189900     EJECT                                                                
190000                                                                          
190100 BBAF-UPPD-ERSAETT-DEFINITIV SECTION.                                     
190200     SKIP2                                                                
190300*                              *******************************            
190400*                              *** UPPDATERING DEFINITIV   ***            
190500*                              *** ERSÄTTNING              ***            
190600*                              *** E-MÄRKT BLIR HISTORIKRAD***            
190700*                              *** T-MÄRKT UPPDATERAS      ***            
190800*                              *******************************            
190900                                                                          
191000     MOVE SATB11C-RAD-IDARTNR TO KOLL-DEF-IDARTNR                         
191100     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR                                
191200                                 SPAR-STR-IDARTNR                         
191300     PERFORM IMS-GET-SATB01-ART                                           
191400     IF SEGMENT-FINNS                                                     
191500       PERFORM IMS-GET-SATB11                                             
191600       IF SEGMENT-FINNS                                                   
191700         PERFORM UNTIL SEGMENT-SAKNAS                                     
191800           IF SATB-RAD-IDARTNR = KOLL-DEF-IDARTNR                         
191900             MOVE 2304-TIERSDAT-PREL TO DAT-I-TIDATUM                     
192000             MOVE 'AAVVD'      TO DAT-KDDATFORM                           
192100             CALL WDATKONV USING DAT-KDDATFORM                            
192200                                 DAT-I-TIDATUM                            
192300                                 DAT-O-TIDATUM                            
192400                                 DAT-KDSVAR                               
192500             IF DAT-KDSVAR-OK                                             
192600               MOVE DAT-TIAA TO TIERSDAT-AAR                              
192700               MOVE DAT-TIMM TO TIERSDAT-MAN                              
192800               MOVE DAT-TIDD TO TIERSDAT-DAG                              
192900             END-IF                                                       
193000                                                                          
193100             PERFORM BAB-BEARBETA-2234-HUVUD                              
193200             MOVE ZERO      TO 2234-REANTPSA-NY                           
193300                                                                          
193400             IF SATB-RAD-KDISATS = 'U'                                    
193500               MOVE ZERO    TO 2234-REANTPSA-GAMMAL                       
193600             ELSE                                                         
193700               MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-GAMMAL             
193800             END-IF                                                       
193900                                                                          
194000             MOVE 'D'       TO 2234-KDISATS                               
194100             PERFORM S02-BEARBETA-STOPPDATUM                              
194200             PERFORM IMS-INSERT-2234                                      
194300                                                                          
194400             MOVE SATB-RAD-TISTODAT TO SPAR-TISTODAT                      
194500             MOVE TIERSDAT-DATUM TO SATB-RAD-TISTODAT                     
194600             MOVE SPACE          TO SATB-RAD-KDISATS                      
194700             PERFORM IMS-UPPD-RAD                                         
194800               MOVE SATB-RAD-IDARTNR TO W-IDARTNR                         
194900               PERFORM IMS-GET-ROT-601                                    
195000               IF SEGMENT-FINNS                                           
195100                 MOVE NEJ TO ART-FLIART                                   
195200                 PERFORM IMS-UPPD-601                                     
195300               END-IF                                                     
195400             PERFORM IMS-GET-SATB11                                       
195500           ELSE                                                           
195600             PERFORM IMS-GET-SATB11                                       
195700           END-IF                                                         
195800         END-PERFORM                                                      
195900       END-IF                                                             
196000     END-IF                                                               
196100                                                                          
196200     MOVE SPAR-STR-IDARTNR TO W-IDARTNR                                   
196300     PERFORM IMS-GET-SATB01-ART                                           
196400     IF SEGMENT-FINNS                                                     
196500        MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                            
196600        PERFORM IMS-UPPD-ART                                              
196700                                                                          
196800        PERFORM IMS-GET-SATB11                                            
196900        PERFORM UNTIL SEGMENT-SAKNAS                                      
197000           IF (SATB-RAD-KDISATS = 'T') AND                                
197100              (SATB-RAD-TISTADAT = SPAR-TISTODAT)                         
197200               MOVE +2 TO INDX                                            
197300               PERFORM UNTIL INDX > ERS-TABANT                            
197400                  IF ERS-IDARTNR(INDX) = SATB-RAD-IDARTNR                 
197500                                                                          
197600                     MOVE 2304-TIERSDAT-PREL TO DAT-I-TIDATUM             
197700                     MOVE 'AAVVD' TO DAT-KDDATFORM                        
197800                     CALL WDATKONV USING DAT-KDDATFORM                    
197900                                         DAT-I-TIDATUM                    
198000                                         DAT-O-TIDATUM                    
198100                                         DAT-KDSVAR                       
198200                     IF DAT-KDSVAR-OK                                     
198300                       MOVE DAT-TIAA TO TIERSDAT-AAR                      
198400                       MOVE DAT-TIMM TO TIERSDAT-MAN                      
198500                       MOVE DAT-TIDD TO TIERSDAT-DAG                      
198600                     END-IF                                               
198700                                                                          
198800                     PERFORM BAB-BEARBETA-2234-HUVUD                      
198900                     MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-NY           
199000                     MOVE ZERO       TO 2234-REANTPSA-GAMMAL              
199100                     MOVE 'T'        TO 2234-KDISATS                      
199200                     PERFORM S03-BEARBETA-STARTDATUM                      
199300                     PERFORM IMS-INSERT-2234                              
199400                                                                          
199500                     MOVE TIERSDAT-DATUM TO SATB-RAD-TISTADAT             
199600                     MOVE SPACE TO SATB-RAD-KDISATS                       
199700                     PERFORM IMS-UPPD-RAD                                 
199800                 END-IF                                                   
199900                 ADD +1 TO INDX                                           
200000              END-PERFORM                                                 
200100           END-IF                                                         
200200           PERFORM IMS-GET-SATB11                                         
200300        END-PERFORM                                                       
200400     END-IF                                                               
200500     .                                                                    
200600     EJECT                                                                
200700                                                                          
200800 BBB-BACKA-ERSAETTNING-I-SATS SECTION.                                    
200900     SKIP2                                                                
201000*               **********************************************            
201100*               *** BACKNING AV ALT-ERS OCH DEF-ERS        ***            
201200*               *** E-MÄRKT UPPDATERAS SPACE               ***            
201300*               *** ALTERNATIV : VARNINGSLISTA SKRIVS      ***            
201400*               **********************************************            
201500                                                                          
201600     IF 2304-KDERS-NEW = ZERO AND                                         
201700       2304-KDERS-OLD = 1 OR 2 OR 3 OR 4 OR 5 OR 6                        
201800         MOVE SATB01C-STR-IDARTNR TO W-IDARTNR                            
201900                                     SPAR-STR-IDARTNR                     
202000         MOVE SATB11C-RAD-IDARTNR TO KOLL-BACK-IDARTNR                    
202100       IF 2304-KDERS-OLD = 1 OR 2 OR 3                                    
202200         PERFORM IMS-GET-SATB01-ART                                       
202300         MOVE NEJ TO BACKNING-SW                                          
202400         IF SEGMENT-FINNS                                                 
202500           PERFORM IMS-GET-SATB11                                         
202600           IF SEGMENT-FINNS                                               
202700             PERFORM UNTIL SEGMENT-SAKNAS                                 
202800               IF (SATB-RAD-IDARTNR = KOLL-BACK-IDARTNR) AND              
202900                  (SATB-RAD-KDISATS = 'E')                                
203000                 MOVE SATB-RAD-TISTODAT TO SPAR-ERSATT-TISTODAT           
203100                 MOVE SPACE TO SATB-RAD-KDISATS                           
203200                 MOVE +999999 TO SATB-RAD-TISTODAT                        
203300                 PERFORM IMS-UPPD-RAD                                     
203400                 MOVE JA TO BACKNING-SW                                   
203500                                                                          
203600                 MOVE NEJ TO BEARBETNING-SW                               
203700                 PERFORM BAB-BEARBETA-2234-HUVUD                          
203800                 PERFORM BABB-BEARBETA-2234                               
203900                 PERFORM IMS-INSERT-2234                                  
204000                 PERFORM IMS-GET-SATB11                                   
204100               ELSE                                                       
204200                 PERFORM IMS-GET-SATB11                                   
204300               END-IF                                                     
204400             END-PERFORM                                                  
204500           END-IF                                                         
204600         END-IF                                                           
204700                                                                          
204800                                                                          
204900         IF E-BACKNING                                                    
205000           MOVE ZERO TO W-IDKDSTRRAD                                      
205100                        W-IDRADNR                                         
205200           PERFORM IMS-GET-SATB-RAD-FIRST                                 
205300           IF SEGMENT-FINNS                                               
205400             PERFORM UNTIL SEGMENT-SAKNAS                                 
205500               IF (SATB-RAD-KDISATS = 'T') AND                            
205600                 (SATB-RAD-TISTADAT = SPAR-ERSATT-TISTODAT)               
205700                 MOVE SATB-RAD-IDARTNR TO W-IDARTNR                       
205800                 MOVE SATB-RAD-IDARTNR TO W-IDARTNRC                      
205900                 MOVE DAGENS-DATUM TO SATB-RAD-TISTODAT                   
206000                 MOVE 'U' TO SATB-RAD-KDISATS                             
206100                 PERFORM IMS-UPPD-RAD                                     
206200                 MOVE NEJ TO BEARBETNING-SW                               
206300                 PERFORM BAB-BEARBETA-2234-HUVUD                          
206400                 PERFORM BABB-BEARBETA-2234                               
206500                 PERFORM IMS-INSERT-2234                                  
206600                 PERFORM S04-LAES-CSEQ                                    
206700               END-IF                                                     
206800               PERFORM IMS-GET-SATB11                                     
206900             END-PERFORM                                                  
207000           END-IF                                                         
207100         END-IF                                                           
207200       ELSE                                                               
207300         PERFORM IMS-GET-SATB01-ART                                       
207400         IF SEGMENT-FINNS                                                 
207500           MOVE NEJ TO BACKNING-SW                                        
207600           PERFORM IMS-GET-SATB11                                         
207700           IF SEGMENT-FINNS                                               
207800             PERFORM UNTIL SEGMENT-SAKNAS                                 
207900               IF (SATB-RAD-IDARTNR = KOLL-BACK-IDARTNR) AND              
208000                  (SATB-RAD-KDISATS = 'E')                                
208100                 MOVE SPACE TO SATB-RAD-KDISATS                           
208200                 MOVE +999999 TO SATB-RAD-TISTODAT                        
208300                 PERFORM IMS-UPPD-RAD                                     
208400                 MOVE JA TO BACKNING-SW                                   
208500                                                                          
208600                 MOVE NEJ TO BEARBETNING-SW                               
208700                 PERFORM BAB-BEARBETA-2234-HUVUD                          
208800                 PERFORM BABB-BEARBETA-2234                               
208900                 PERFORM IMS-INSERT-2234                                  
209000                 MOVE '015' TO WFELKOD-R07                                
209100                 MOVE TEXT7 TO WFELTEXT                                   
209200                 MOVE 2304-IDARTNR-ING TO WARTNR                          
209300                 MOVE MASKINELL TO WMASKINELL                             
209400                 PERFORM S10-SKRIV-FOV                                    
209500                 PERFORM IMS-GET-SATB11                                   
209600               ELSE                                                       
209700                 PERFORM IMS-GET-SATB11                                   
209800               END-IF                                                     
209900             END-PERFORM                                                  
210000           END-IF                                                         
210100         END-IF                                                           
210200       END-IF                                                             
210300       IF E-BACKNING                                                      
210400         MOVE SPAR-STR-IDARTNR TO W-IDARTNR                               
210500         PERFORM IMS-GET-SATB01-ART                                       
210600         IF SEGMENT-FINNS                                                 
210700           MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                         
210800           MOVE 'J'          TO SATB-STR-FLFORPQ                          
210900           PERFORM IMS-UPPD-ART                                           
211000         END-IF                                                           
211100       END-IF                                                             
211200     END-IF                                                               
211300     .                                                                    
211400     EJECT                                                                
211500                                                                          
211600 C-BEHANDLA-2302 SECTION.                                                 
211700     SKIP2                                                                
211800*                       **************************************            
211900*                       *** BEHANDLA 2302-SEGMENT FRÅN     ***            
212000*                       *** HÄNDELSE-REGISTRET             ***            
212100*                       *** ÄT JAN 92 01-ERSATTA ARTIKLAR  ***            
212200*                       ***           TILLÅTS I 1002-SATS  ***            
212300*                       **************************************            
212400                                                                          
212500     MOVE JA TO UPPDATERING-SW                                            
212600     MOVE ZERO TO KOLLA-IDARTNR                                           
212700                                                                          
212800     MOVE NEJ TO WS-EK01-FINNS                                            
212900     MOVE +1 TO EK01-IX                                                   
213000     PERFORM UNTIL EK01-IX > 100                                          
213100        MOVE ZERO TO EK01-IDARTNR(EK01-IX)                                
213200        ADD +1 TO EK01-IX                                                 
213300     END-PERFORM                                                          
213400     MOVE +1 TO EK01-IX                                                   
213500                                                                          
213600     IF 2302-IDLEVNR = '1002 '                                            
213700       MOVE 2302-IDARTNR-SATS TO W-IDARTNR                                
213800       PERFORM IMS-GET-SATB01-ART                                         
213900       IF SEGMENT-FINNS                                                   
214000*        IF SATB-STR-IDLEVNR NOT = '1002 '                                
214100           PERFORM IMS-GET-SATB11                                         
214200           IF SEGMENT-FINNS                                               
214300             PERFORM UNTIL SEGMENT-SAKNAS                                 
214400               MOVE SATB-RAD-IDARTNR TO W-IDARTNR                         
214500               PERFORM IMS-GET-ROT-601                                    
214600               IF SEGMENT-FINNS                                           
214700                 IF ART-KDERS-UTG = ZERO                                  
214800                   PERFORM IMS-GET-ARTC11                                 
214900                   IF SEGMENT-FINNS                                       
215000                     IF CLAG-KDUART NOT = 'P'                             
215100                         IF CLAG-KDERS = ZERO                             
215200                           CONTINUE                                       
215300                         ELSE                                             
215400                           IF CLAG-KDERS = 01                             
215500                             MOVE SATB-RAD-TISTODAT TO TMP1-YYMMDD        
215600                             MOVE DAGENS-DATUM      TO TMP2-YYMMDD        
215700                             PERFORM WY2000P1                             
215800                             IF TMP1-YYMMDD > TMP2-YYMMDD                 
215900                                MOVE W-IDARTNR TO KOLLA-IDARTNR           
216000                                PERFORM CC-KOLLA-EK01                     
216100                                IF WS-EK01-OK = JA                        
216200                                   MOVE KOLLA-IDARTNR TO                  
216300                                     EK01-IDARTNR(EK01-IX)                
216400                                   ADD +1 TO EK01-IX                      
216500                                   MOVE JA TO WS-EK01-FINNS               
216600                                ELSE                                      
216700                                  MOVE DAGENS-DATUM                       
216800                                           TO SATB-RAD-TISTODAT           
216900                                  PERFORM IMS-UPPD-RAD                    
217000                                  MOVE '015' TO WFELKOD-R07               
217100                                  MOVE TEXT14 TO WFELTEXT                 
217200                                  MOVE SATB-RAD-IDARTNR TO WARTNR         
217300                                  PERFORM S10-SKRIV-FOV                   
217400                                END-IF                                    
217500                              END-IF                                      
217600                            ELSE                                          
217700                              MOVE DAGENS-DATUM                           
217800                                         TO SATB-RAD-TISTODAT             
217900                              PERFORM IMS-UPPD-RAD                        
218000                              MOVE '015' TO WFELKOD-R07                   
218100                              MOVE TEXT14 TO WFELTEXT                     
218200                              MOVE SATB-RAD-IDARTNR TO WARTNR             
218300                              PERFORM S10-SKRIV-FOV                       
218400                           END-IF                                         
218500                         END-IF                                           
218600                     ELSE                                                 
218700                       MOVE DAGENS-DATUM TO SATB-RAD-TISTODAT             
218800                       PERFORM IMS-UPPD-RAD                               
218900                       MOVE '015' TO WFELKOD-R07                          
219000                       MOVE TEXT16 TO WFELTEXT                            
219100                       MOVE SATB-RAD-IDARTNR TO WARTNR                    
219200                       PERFORM S10-SKRIV-FOV                              
219300                    END-IF                                                
219400                  ELSE                                                    
219500                    MOVE DAGENS-DATUM TO SATB-RAD-TISTODAT                
219600                    PERFORM IMS-UPPD-RAD                                  
219700                    MOVE '015' TO WFELKOD-R07                             
219800                    MOVE TEXT15 TO WFELTEXT                               
219900                    MOVE SATB-RAD-IDARTNR TO WARTNR                       
220000                    PERFORM S10-SKRIV-FOV                                 
220100                  END-IF                                                  
220200                ELSE                                                      
220300                  MOVE DAGENS-DATUM TO SATB-RAD-TISTODAT                  
220400                  PERFORM IMS-UPPD-RAD                                    
220500                  MOVE '015' TO WFELKOD-R07                               
220600                  MOVE TEXT15 TO WFELTEXT                                 
220700                   MOVE SATB-RAD-IDARTNR TO WARTNR                        
220800                   PERFORM S10-SKRIV-FOV                                  
220900                 END-IF                                                   
221000               ELSE                                                       
221100                 MOVE DAGENS-DATUM TO SATB-RAD-TISTODAT                   
221200                 PERFORM IMS-UPPD-RAD                                     
221300                 MOVE '015' TO WFELKOD-R07                                
221400                 MOVE TEXT11 TO WFELTEXT                                  
221500                 MOVE SATB-RAD-IDARTNR TO WARTNR                          
221600                 PERFORM S10-SKRIV-FOV                                    
221700               END-IF                                                     
221800               PERFORM IMS-GET-SATB11                                     
221900             END-PERFORM                                                  
222000           END-IF                                                         
222100                                                                          
222200                                                                          
222300         IF UPPDATERING-OK                                                
222400                                                                          
222500           IF WS-EK01-FINNS = JA                                          
222600              MOVE +1 TO EK01-IX                                          
222700              PERFORM UNTIL EK01-IX > 100                                 
222800                 IF EK01-IDARTNR(EK01-IX) > ZERO                          
222900                    MOVE EK01-IDARTNR(EK01-IX) TO KOLLA-IDARTNR           
223000                                                  W-IDARTNR               
223100                    PERFORM CC-KOLLA-EK01                                 
223200                    PERFORM CD-BEHANDLA-EK01                              
223300                 END-IF                                                   
223400                 ADD +1 TO EK01-IX                                        
223500              END-PERFORM                                                 
223600           END-IF                                                         
223700                                                                          
223800           MOVE 2302-IDARTNR-SATS TO W-IDARTNR                            
223900           PERFORM IMS-GET-SATB01-ART                                     
224000           MOVE 2302-IDLEVNR TO SATB-STR-IDLEVNR                          
224100           MOVE 'J'          TO SATB-STR-FLFORPQ                          
224200           PERFORM IMS-UPPD-ART                                           
224300                                                                          
224400           PERFORM IMS-GET-SATB11                                         
224500           IF SEGMENT-FINNS                                               
224600             PERFORM UNTIL SEGMENT-SAKNAS                                 
224700               MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                    
224800               MOVE DAGENS-DATUM        TO TMP2-YYMMDD                    
224900               PERFORM WY2000P1                                           
225000               IF TMP1-YYMMDD > TMP2-YYMMDD                               
225100                 MOVE SATB-RAD-IDARTNR TO W-IDARTNR                       
225200                 MOVE SATB-RAD-TISTADAT   TO TMP1-YYMMDD                  
225300                 MOVE DAGENS-DATUM        TO TMP2-YYMMDD                  
225400                 PERFORM WY2000P1                                         
225500                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
225600                    AND SATB-RAD-KDISATS = SPACE                          
225700                       MOVE 'N' TO SATB-RAD-KDISATS                       
225800                       PERFORM IMS-UPPD-ART                               
225900                  ELSE                                                    
226000                    PERFORM CB-BEHANDLA-2234-HUVUD-2302                   
226100                    MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-NY            
226200                    MOVE ZERO TO 2234-REANTPSA-GAMMAL                     
226300                    MOVE 'N' TO 2234-KDISATS                              
226400                    PERFORM S03-BEARBETA-STARTDATUM                       
226500                    PERFORM IMS-INSERT-2234                               
226600                  END-IF                                                  
226700                                                                          
226800                 PERFORM IMS-GET-ROT-601                                  
226900                 IF SEGMENT-FINNS                                         
227000                   MOVE JA TO ART-FLIART                                  
227100                   PERFORM IMS-UPPD-601                                   
227200                 END-IF                                                   
227300              ELSE                                                        
227400                 IF SATB-RAD-TISTODAT = DAGENS-DATUM AND                  
227500                    SATB-RAD-KDISATS = 'E'                                
227600                    MOVE SATB-RAD-IDARTNR TO W-IDARTNR                    
227700                    PERFORM CB-BEHANDLA-2234-HUVUD-2302                   
227800                    MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-NY            
227900                    MOVE ZERO       TO 2234-REANTPSA-GAMMAL               
228000                    MOVE 'N'        TO 2234-KDISATS                       
228100                    PERFORM S03-BEARBETA-STARTDATUM                       
228200                    PERFORM IMS-INSERT-2234                               
228300                                                                          
228400                    PERFORM IMS-GET-ROT-601                               
228500                    IF SEGMENT-FINNS                                      
228600                       MOVE JA TO ART-FLIART                              
228700                       PERFORM IMS-UPPD-601                               
228800                    END-IF                                                
228900                  END-IF                                                  
229000                                                                          
229100               END-IF                                                     
229200               PERFORM IMS-GET-SATB11                                     
229300             END-PERFORM                                                  
229400           END-IF                                                         
229500         END-IF                                                           
229600*       END-IF                                                            
229700                                                                          
229800       ELSE                                                               
229900                                                                          
230000***********   STRUKTUR SAKNAS WDJ1                                        
230100                                                                          
230200         PERFORM IMS-GET-ROT-601                                          
230300         IF SEGMENT-FINNS                                                 
230400           PERFORM IMS-GET-ARTC11                                         
230500           IF SEGMENT-FINNS                                               
230600             IF CLAG-KDERS = ZERO                                         
230700               PERFORM CA-NYUPPLAEGG-STRUKTUR                             
230800             END-IF                                                       
230900           END-IF                                                         
231000         END-IF                                                           
231100       END-IF                                                             
231200                                                                          
231300     ELSE                                                                 
231400                                                                          
231500***********   BYTE FRÅN LEVNR '1002 '                                     
231600                                                                          
231700       MOVE 2302-IDARTNR-SATS TO W-IDARTNR                                
231800                                 SPAR-KOLL-FLER-ARTNR                     
231900       PERFORM IMS-GET-SATB01-ART                                         
232000       IF SEGMENT-FINNS                                                   
232100         IF SATB-STR-TIBORT = ZERO                                        
232200           MOVE SPACE TO SATB-STR-IDLEVNR                                 
232300           PERFORM IMS-UPPD-ART                                           
232400           PERFORM IMS-GET-SATB11                                         
232500           IF SEGMENT-FINNS                                               
232600             PERFORM UNTIL SEGMENT-SAKNAS                                 
232700               MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                    
232800               MOVE DAGENS-DATUM        TO TMP2-YYMMDD                    
232900               PERFORM WY2000P1                                           
233000               IF TMP1-YYMMDD > TMP2-YYMMDD                               
233100                 PERFORM CB-BEHANDLA-2234-HUVUD-2302                      
233200                 MOVE ZERO TO 2234-REANTPSA-NY                            
233300                 IF SATB-RAD-KDISATS = 'U'                                
233400                   MOVE ZERO TO 2234-REANTPSA-GAMMAL                      
233500                 ELSE                                                     
233600                  MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-GAMMAL          
233700                 END-IF                                                   
233800                 MOVE 'U' TO 2234-KDISATS                                 
233900                 MOVE DAGENS-DATUM TO DAT-I-TIDATUM                       
234000                 MOVE 'AAMMDD'     TO DAT-KDDATFORM                       
234100                 CALL WDATKONV USING DAT-KDDATFORM                        
234200                                     DAT-I-TIDATUM                        
234300                                     DAT-O-TIDATUM                        
234400                                     DAT-KDSVAR                           
234500                 IF DAT-KDSVAR-OK                                         
234600                   MOVE DAT-TIAA-VECKA TO TRANS-AAR                       
234700                   MOVE DAT-TIVV       TO TRANS-VECKA                     
234800                 END-IF                                                   
234900                 MOVE TRANS-DATUM TO 2234-TIBEHDAT                        
235000                 PERFORM IMS-INSERT-2234                                  
235100               END-IF                                                     
235200                                                                          
235300               MOVE SATB-RAD-IDARTNR TO W-IDARTNRC                        
235400               PERFORM S06-LAES-CSEQ                                      
235500               PERFORM IMS-GET-SATB11                                     
235600             END-PERFORM                                                  
235700           END-IF                                                         
235800         END-IF                                                           
235900       END-IF                                                             
236000     END-IF                                                               
236100     .                                                                    
236200     EJECT                                                                
236300                                                                          
236400 CA-NYUPPLAEGG-STRUKTUR SECTION.                                          
236500     SKIP2                                                                
236600     MOVE SPAR-SATSNR       TO SATB-STR-IDARTNR                           
236700     MOVE SPACE             TO SATB-STR-BEART-SVE                         
236800     MOVE SPACE             TO SATB-STR-FLEXFORP                          
236900     MOVE 'N'               TO SATB-STR-FLFORPQ                           
237000     MOVE ZERO              TO SATB-STR-IDFKNGRP                          
237100     MOVE 2302-IDLEVNR      TO SATB-STR-IDLEVNR                           
237200     MOVE 'S'               TO SATB-STR-IDSTRTYP                          
237300     MOVE SPACE             TO SATB-STR-IDTSPEC                           
237400     MOVE SPACE             TO SATB-STR-IDUSER                            
237500     MOVE ZERO              TO SATB-STR-KDBENHOM                          
237600     MOVE ZERO              TO SATB-STR-KDPRODSL                          
237700     MOVE ZERO              TO SATB-STR-TIBORT                            
237800     MOVE DAGENS-DATUM      TO SATB-STR-TIREGDAT                          
237900     MOVE ZERO              TO SATB-STR-TIUPPDAT                          
238000     MOVE ZERO              TO SATB-STR-KVBYGMIN                          
238100     MOVE +1 TO INDX                                                      
238200     PERFORM UNTIL INDX > 2                                               
238300       MOVE SPACE TO SATB-STR-TESTRNOT(INDX)                              
238400       ADD +1 TO INDX                                                     
238500     END-PERFORM                                                          
238600     PERFORM IMS-NYUPPLAEGG-ART-I-SATS                                    
238700     .                                                                    
238800     EJECT                                                                
238900                                                                          
239000 CB-BEHANDLA-2234-HUVUD-2302 SECTION.                                     
239100     SKIP2                                                                
239200     MOVE 2302-IDARTNR-SATS TO 2234-IDARTNR-SATS                          
239300     MOVE SATB-RAD-IDARTNR  TO 2234-IDARTNR-ING                           
239400     MOVE ZERO              TO 2234-KVPB-SEP-TOT                          
239500     .                                                                    
239600     EJECT                                                                
239700 CC-KOLLA-EK01 SECTION.                                                   
239800*****************************************************************         
239900*  KONTROLL ATT TILLKOMMANDE ARTIKLAR EJ ÄR ERSATTA - OM INTE   *         
240000*  SPARAS TILLKOMMANDE ARTIKLAR I TABELL                        *         
240100*****************************************************************         
240200                                                                          
240300     MOVE JA TO WS-EK01-OK                                                
240400     PERFORM IMS-GET-ERSAETTNING-ROT                                      
240500     IF SEGMENT-FINNS                                                     
240600        MOVE ERSA-IDARTNR   TO ERS-IDARTNR(1)                             
240700        MOVE ERSA-DIERS-ERS TO ERS-DIERS(1)                               
240800        MOVE +1 TO INDX                                                   
240900        PERFORM IMS-GET-ERSAETTNING-TILLK                                 
241000        IF SEGMENT-FINNS                                                  
241100           PERFORM UNTIL SEGMENT-SAKNAS OR WS-EK01-OK = NEJ               
241200              IF ERSA-FLTEXT = JA                                         
241300                 PERFORM IMS-GET-ERSAETTNING-TILLK                        
241400              ELSE                                                        
241500                 MOVE ERSA-IDARTNR-TILLK TO W-IDARTNR                     
241600                 PERFORM IMS-GET-ROT-601                                  
241700                 IF SEGMENT-FINNS                                         
241800                    PERFORM IMS-GET-ARTC11                                
241900                    IF SEGMENT-FINNS                                      
242000                       IF CLAG-KDERS = 0                                  
242100                          ADD +1 TO INDX                                  
242200                          MOVE ERSA-IDARTNR-TILLK                         
242300                                          TO ERS-IDARTNR(INDX)            
242400                          MOVE ERSA-DIERS-TILLK                           
242500                                          TO ERS-DIERS(INDX)              
242600                       ELSE                                               
242700                          MOVE NEJ TO WS-EK01-OK                          
242800                       END-IF                                             
242900                    ELSE                                                  
243000                       MOVE NEJ TO WS-EK01-OK                             
243100                    END-IF                                                
243200                 ELSE                                                     
243300                    MOVE NEJ TO WS-EK01-OK                                
243400                 END-IF                                                   
243500                 PERFORM IMS-GET-ERSAETTNING-TILLK                        
243600              END-IF                                                      
243700           END-PERFORM                                                    
243800           MOVE INDX TO ERS-TABANT                                        
243900        END-IF                                                            
244000     ELSE                                                                 
244100        MOVE NEJ TO WS-EK01-OK                                            
244200     END-IF                                                               
244300     .                                                                    
244400     EJECT                                                                
244500 CD-BEHANDLA-EK01 SECTION.                                                
244600*****************************************************************         
244700* STRUKTUREN KONVERTERAS TILL ARBETS-STRNR. ERSATT ARTIKEL      *         
244800* E-MÄRKS OCH TILLKOMMANDE ARTIKLAR LÄGGS IN I STRUKTUREN EFTER *         
244900* DEN ERSATTA. RADERNA OMNUMRERAS OCH NY STRUKTUR LÄGGS IN.     *         
245000* GAMMAL OCH KONVERTERAD DELEATAS.                              *         
245100*****************************************************************         
245200                                                                          
245300     MOVE NEJ TO UPPDATERING-KONV-SW                                      
245400     MOVE JA TO INDATA-SW                                                 
245500                                                                          
245600     MOVE 2302-IDARTNR-SATS TO W-IDARTNR                                  
245700                               W-IDARTNR-OKONV                            
245800                               WS-STR-IDARTNR                             
245900                               SPAR-IDARTNR-STRUKTUR                      
246000     COMPUTE IDARTNR-KONVERTERAD-WS = +999999999 -                        
246100          WS-STR-IDARTNR                                                  
246200                                                                          
246300     PERFORM IMS-GET-SATB01-OKONV                                         
246400     IF SEGMENT-FINNS                                                     
246500        MOVE IO-AREA-OKONV TO IO-AREA-KONV                                
246600        MOVE IDARTNR-KONVERTERAD-WS TO SATB01K-STR-IDARTNR                
246700                                       W-IDARTNR-KONV                     
246800        MOVE 'W1121000' TO SATB01K-STR-IDUSER                             
246900        MOVE DAGENS-DATUM TO SATB01K-STR-TIREGDAT                         
247000        PERFORM IMS-ISRT-SATB01-KONV                                      
247100                                                                          
247200        MOVE +10   TO WS-IDRADNR                                          
247300        MOVE +9999 TO WS-IDRADNR-FORPACKNING                              
247400                                                                          
247500        PERFORM IMS-GET-SATB11-OKONV                                      
247600        PERFORM UNTIL SEGMENT-SAKNAS                                      
247700           MOVE SATB11O-RAD-KDSTRRAD TO W-KDSTRRAD-OKONV                  
247800                                        W-KDSTRRAD-KONV                   
247900                                        WS-KDSTRRAD                       
248000           MOVE SATB11O-RAD-IDRADNR  TO W-IDRADNR-OKONV                   
248100           MOVE WS-IDRADNR           TO W-IDRADNR-KONV                    
248200           MOVE SATB11O-RAD-TISTODAT   TO TMP1-YYMMDD                     
248300           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
248400           PERFORM WY2000P1                                               
248500           IF (SATB11O-RAD-KDISATS = 'N' OR 'T' OR SPACE)  AND            
248600              (SATB11O-RAD-IDARTNR = KOLLA-IDARTNR)        AND            
248700              (TMP1-YYMMDD > TMP2-YYMMDD)                                 
248800                                                                          
248900              MOVE SATB11O-RAD-REANTPSA TO ERS-KVANT-I-SATS               
249000              MOVE JA TO UPPDATERING-KONV-SW                              
249100              MOVE DAGENS-DATUM TO KONV-TISTODAT                          
249200              MOVE 'E'          TO KONV-KDISATS                           
249300                                                                          
249400              PERFORM CDA-KOPIERA-RAD-NOT                                 
249500              ADD +10 TO WS-IDRADNR                                       
249600              PERFORM CDB-LAEGG-UPP-TILLK                                 
249700           ELSE                                                           
249800              MOVE NEJ TO UPPDATERING-KONV-SW                             
249900              PERFORM CDA-KOPIERA-RAD-NOT                                 
250000              ADD +10 TO WS-IDRADNR                                       
250100           END-IF                                                         
250200           PERFORM IMS-GET-SATB11-OKONV                                   
250300        END-PERFORM                                                       
250400                                                                          
250500        PERFORM CDC-KOPIERA-KONV-TILL-OKONV                               
250600                                                                          
250700     END-IF                                                               
250800     .                                                                    
250900     EJECT                                                                
251000 CDA-KOPIERA-RAD-NOT SECTION.                                             
251100*****************************************************************         
251200*  RADEN FLYTTAS ÖVER TILL KONVERTERAD STRUKTUR                 *         
251300*****************************************************************         
251400                                                                          
251500     MOVE IO-AREA-OKONV TO IO-AREA-KONV                                   
251600     IF UPPDATERING-KONV-OK                                               
251700        MOVE KONV-TISTODAT TO SATB11K-RAD-TISTODAT                        
251800        MOVE KONV-KDISATS  TO SATB11K-RAD-KDISATS                         
251900     END-IF                                                               
252000     MOVE WS-IDRADNR       TO SATB11K-RAD-IDRADNR                         
252100     PERFORM IMS-ISRT-SATB11-KONV                                         
252200     PERFORM S21-KOPIERA-NOTERINGSEGMENT                                  
252300     .                                                                    
252400     EJECT                                                                
252500 CDB-LAEGG-UPP-TILLK SECTION.                                             
252600*****************************************************************         
252700*  KONTROLL ATT TILLK ARTIKEL EJ = ÖVERLIGGANDE STRUKTUR.       *         
252800*  KONTROLL ATT ÖVERLIGGANDE STRUKTUR EJ INGÅR I TILLK ARTIKEL  *         
252900*  RADEN LÄGGS UPP I KONVERTERAD STRUKTUR.                      *         
253000*****************************************************************         
253100     SKIP2                                                                
253200     MOVE +2 TO INDX                                                      
253300     PERFORM UNTIL INDX > ERS-TABANT                                      
253400       COMPUTE SPAR-REANTPSA ROUNDED =                                    
253500       ERS-KVANT-I-SATS * ERS-DIERS(INDX)                                 
253600       / ERS-DIERS(1)                                                     
253700       MOVE 'T'          TO SPAR-KDISATS                                  
253800       MOVE DAGENS-DATUM TO SPAR-TISTADAT                                 
253900       MOVE ERS-IDARTNR (INDX) TO SPAR-IDARTNR-ING                        
254000                                  SPAR-IDARTNR-TILLK                      
254100                                                                          
254200       IF SPAR-IDARTNR-ING = SPAR-IDARTNR-STRUKTUR                        
254300          MOVE NEJ TO INDATA-SW                                           
254400          MOVE '015' TO WFELKOD-R07                                       
254500          MOVE TEXT10 TO WFELTEXT                                         
254600          MOVE SPAR-IDARTNR-STRUKTUR TO WARTNR                            
254700          PERFORM S10-SKRIV-FOV                                           
254800       END-IF                                                             
254900                                                                          
255000       IF INDATA-OK                                                       
255100          MOVE SPAR-IDARTNR-STRUKTUR TO W-IDARTNRC                        
255200          PERFORM S14-KOLL-STRNR-EJ-I-EGEN                                
255300                                                                          
255400          IF INDATA-OK                                                    
255500             MOVE ERS-IDARTNR (INDX) TO W-IDARTNR                         
255600                                                                          
255700             MOVE NEJ TO TILLK-ARTIKEL-FORPACKNING-SW                     
255800             PERFORM IMS-GET-ARTC01-2                                     
255900             IF SEGMENT-FINNS                                             
256000                MOVE ART2-ART-KDPRODSL TO TEST-KDPRODSL                   
256100                IF KDPRODSL-VOLVO-EMB                                     
256200                   MOVE JA TO TILLK-ARTIKEL-FORPACKNING-SW                
256300                END-IF                                                    
256400                IF TILLK-ARTIKEL-FORPACKNING                              
256500                   MOVE +9             TO SATB11K-RAD-KDSTRRAD            
256600                                          W-KDSTRRAD-KONV                 
256700                   MOVE WS-IDRADNR-FORPACKNING TO                         
256800                                       SATB11K-RAD-IDRADNR                
256900                                       W-IDRADNR-KONV                     
257000                ELSE                                                      
257100                   MOVE ZERO           TO SATB11K-RAD-KDSTRRAD            
257200                                          W-KDSTRRAD-KONV                 
257300                   MOVE WS-IDRADNR     TO SATB11K-RAD-IDRADNR             
257400                                          W-IDRADNR-KONV                  
257500                END-IF                                                    
257600                MOVE SPACE             TO SATB11K-RAD-IDLEVNR             
257700                                          SATB11K-RAD-BELEVART            
257800                MOVE SPAR-IDARTNR-ING  TO SATB11K-RAD-IDARTNR             
257900                MOVE SPACE             TO SATB11K-RAD-BEART-SVE           
258000                                          SATB11K-RAD-IDAO-STA            
258100                                          SATB11K-RAD-IDAO-STO            
258200                MOVE SPAR-IDARTNR-ING TO W-IDARTNR                        
258300                PERFORM IMS-GET-SATB01                                    
258400                IF SEGMENT-FINNS                                          
258500                   MOVE SATB-STR-IDSTRTYP TO SATB11K-RAD-IDSTRTYP         
258600                ELSE                                                      
258700                   MOVE SPACE             TO SATB11K-RAD-IDSTRTYP         
258800                END-IF                                                    
258900                MOVE ZERO                 TO SATB11K-RAD-KDBENHOM         
259000                MOVE SPAR-KDISATS     TO SATB11K-RAD-KDISATS              
259100                MOVE SPACE            TO SATB11K-RAD-KDSORT               
259200                MOVE SPAR-REANTPSA    TO SATB11K-RAD-REANTPSA             
259300                MOVE DAGENS-DATUM     TO SATB11K-RAD-TIREGDAT             
259400                MOVE DAGENS-DATUM     TO SATB11K-RAD-TISTADAT             
259500                MOVE +999999          TO SATB11K-RAD-TISTODAT             
259600                                                                          
259700                IF TILLK-ARTIKEL-FORPACKNING                              
259800                   SUBTRACT 1 FROM WS-IDRADNR-FORPACKNING                 
259900                ELSE                                                      
260000                   ADD +10 TO WS-IDRADNR                                  
260100                END-IF                                                    
260200                PERFORM IMS-ISRT-SATB11-KONV                              
260300             END-IF                                                       
260400          END-IF                                                          
260500       END-IF                                                             
260600                                                                          
260700       ADD +1 TO INDX                                                     
260800     END-PERFORM                                                          
260900     .                                                                    
261000     EJECT                                                                
261100 CDC-KOPIERA-KONV-TILL-OKONV SECTION.                                     
261200*****************************************************************         
261300* FÖRPACKN-RADER I KONV STRUKTUR OMNUMRERAS. GAMMAL STRUKTUR    *         
261400* DELEATAS. KONVERTERAD STRUKTUR INSERTAS UNDER RÄTT STRUKTURNR *         
261500* KONVERTERAD STRUKTUR DELEATAS.                                *         
261600*****************************************************************         
261700     SKIP2                                                                
261800     MOVE ZERO TO WS-IDRADNR                                              
261900     PERFORM IMS-GET-SATB01-OKONV                                         
262000     MOVE DAGENS-DATUM TO SATB01O-STR-TIUPPDAT                            
262100     PERFORM IMS-REPL-SATB-OKONV                                          
262200                                                                          
262300     PERFORM IMS-GET-SATB11-OKONV                                         
262400     PERFORM UNTIL SEGMENT-SAKNAS                                         
262500        PERFORM IMS-DLET-SATB-OKONV                                       
262600        PERFORM IMS-GET-SATB11-OKONV                                      
262700     END-PERFORM                                                          
262800                                                                          
262900     PERFORM IMS-GET-SATB01-KONV                                          
263000     PERFORM IMS-GET-SATB11-KONV                                          
263100     PERFORM UNTIL SEGMENT-SAKNAS                                         
263200        IF SATB11K-RAD-KDSTRRAD = '9'                                     
263300           MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                   
263400                                        W-KDSTRRAD-OKONV                  
263500           MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                    
263600           MOVE IO-AREA-KONV TO IO-AREA-OKONV                             
263700           MOVE WS-IDRADNR-FORPACKNING TO SATB11O-RAD-IDRADNR             
263800                                          W-IDRADNR-OKONV                 
263900           ADD +10 TO WS-IDRADNR-FORPACKNING                              
264000        ELSE                                                              
264100           MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                   
264200                                        W-KDSTRRAD-OKONV                  
264300           MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                    
264400                                        W-IDRADNR-OKONV                   
264500           MOVE IO-AREA-KONV TO IO-AREA-OKONV                             
264600                                                                          
264700           MOVE ZERO TO WS-IDRADNR-FORPACKNING                            
264800           COMPUTE WS-IDRADNR-FORPACKNING = SATB11K-RAD-IDRADNR           
264900                   + 10                                                   
265000        END-IF                                                            
265100                                                                          
265200        PERFORM IMS-ISRT-SATB11-OKONV                                     
265300                                                                          
265400        PERFORM IMS-GET-SATB22-KONV                                       
265500        PERFORM UNTIL SEGMENT-SAKNAS                                      
265600           IF SEGMENT-FINNS                                               
265700              MOVE IO-AREA-KONV TO IO-AREA-OKONV                          
265800              PERFORM IMS-ISRT-SATB22-OKONV                               
265900              PERFORM IMS-GET-SATB22-KONV                                 
266000           END-IF                                                         
266100        END-PERFORM                                                       
266200        PERFORM IMS-GET-SATB11-KONV                                       
266300     END-PERFORM                                                          
266400                                                                          
266500     PERFORM IMS-GET-SATB01-KONV                                          
266600     IF SEGMENT-FINNS                                                     
266700        PERFORM IMS-DLET-SATB-KONV                                        
266800     END-IF                                                               
266900     .                                                                    
267000     EJECT                                                                
267100 D-OMRAKNING-STOPPDATUM SECTION.                                          
267200     SKIP2                                                                
267300     MOVE W-DAGENS-DATUM-VECKA TO WS-DAGENS-DATUM-VECKA                   
267400                                                                          
267500     PERFORM IMS-GET-SATB01-ART-OKVAL-UNIK                                
267600     IF SEGMENT-FINNS                                                     
267700       PERFORM UNTIL (SEGMENT-SAKNAS) OR (BAS-SLUT)                       
267800****************** 970704                                                 
267900*          DISPLAY 'SATB-STR-IDARTNR = ' SATB-STR-IDARTNR                 
268000****************** 970704                                                 
268100         IF CHKP-ANT > CHKP-MAX                                           
268200            PERFORM X-TAG-CHECKPOINT-SATB                                 
268300         END-IF                                                           
268400         IF (SATB-STR-TIBORT = ZERO) AND                                  
268500            (SATB-STR-IDARTNR < 100000000)                                
268600                                                                          
268700           MOVE SATB-STR-IDARTNR TO W-IDARTNR-OKONV                       
268800           PERFORM IMS-GET-SATB11                                         
268900           IF SEGMENT-FINNS                                               
269000             PERFORM UNTIL SEGMENT-SAKNAS                                 
269100               IF SATB-RAD-KDISATS = 'E'                                  
269200                 PERFORM DA-BEHANDLA-STOPPDATUM                           
269300               ELSE                                                       
269400                 CONTINUE                                                 
269500               END-IF                                                     
269600               MOVE WS-DAGENS-DATUM-VECKA                                 
269700                              TO W-DAGENS-DATUM-VECKA                     
269800               PERFORM IMS-GET-SATB11                                     
269900             END-PERFORM                                                  
270000           END-IF                                                         
270100         END-IF                                                           
270200         PERFORM IMS-GET-SATB01-ART-OKVAL                                 
270300       END-PERFORM                                                        
270400     END-IF                                                               
270500     .                                                                    
270600     EJECT                                                                
270700                                                                          
270800 DA-BEHANDLA-STOPPDATUM SECTION.                                          
270900     SKIP2                                                                
271000     MOVE SATB-RAD-TISTODAT TO SPAR-UPPD-DATUM                            
271100     MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                              
271200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
271300     CALL WDATKONV USING DAT-KDDATFORM                                    
271400                         DAT-I-TIDATUM                                    
271500                         DAT-O-TIDATUM                                    
271600                         DAT-KDSVAR                                       
271700     IF DAT-KDSVAR-OK                                                     
271800       MOVE DAT-TIAA-VECKA TO KONTROLL-AAR                                
271900       MOVE DAT-TIVV       TO KONTROLL-VECKA                              
272000     END-IF                                                               
272100                                                                          
272200     ADD +1 TO W-DAGENS-DATUM-VECKA                                       
272300     IF W-DAGENS-DATUM-VECKA > 52                                         
272400       ADD +1 TO W-DAGENS-DATUM-AAR                                       
272500       SUBTRACT 52 FROM W-DAGENS-DATUM-VECKA                              
272600     END-IF                                                               
272700                                                                          
272800     MOVE KONTROLL-DATUM TO TMP1-YYWW                                     
272900     MOVE W-DAGENS-DATUM TO TMP2-YYWW                                     
273000     PERFORM WY2000P3                                                     
273100     IF TMP1-YYWW < TMP2-YYWW                                             
273200       MOVE SATB-RAD-IDARTNR TO W-IDARTNR                                 
273300       PERFORM IMS-GET-ROT-601                                            
273400       IF SEGMENT-FINNS                                                   
273500         PERFORM IMS-GET-ARTC11                                           
273600         IF SEGMENT-FINNS                                                 
273700           MOVE CLAG-KDERS TO SPAR-KDERS                                  
273800           IF CLAG-KDERS = 01 OR 02 OR 04 OR 05                           
273900             PERFORM BBAA-LAES-ERSAETTNINGS-REG                           
274000             IF ERS-TILLK-FINNS                                           
274100               IF CLAG-KDERS = 01 OR 04                                   
274200                 PERFORM DAA-TID-ENL-FORMEL                               
274300               ELSE                                                       
274400                 PERFORM DAB-KOLLA-PUBL-VECKA                             
274500               END-IF                                                     
274600               MOVE UPPD-DATUM TO SATB-RAD-TISTODAT                       
274700               PERFORM IMS-UPPD-RAD                                       
274800                                                                          
274900               PERFORM IMS-GET-SATB01-OKONV                               
275000               PERFORM IMS-GET-SATB11-OKONV                               
275100               PERFORM UNTIL SEGMENT-SAKNAS                               
275200                 IF (SATB11O-RAD-KDISATS = 'T') AND                       
275300                    (SATB11O-RAD-TISTADAT = SPAR-UPPD-DATUM)              
275400                    MOVE SATB11O-RAD-IDARTNR TO SPAR-IDARTNR              
275500                    MOVE +2 TO INDX                                       
275600                    PERFORM UNTIL INDX > ERS-TABANT                       
275700                      IF SEGMENT-FINNS                                    
275800                        IF ERS-IDARTNR(INDX) = SPAR-IDARTNR               
275900                          MOVE UPPD-DATUM TO                              
276000                            SATB11O-RAD-TISTADAT                          
276100                          PERFORM IMS-REPL-SATB11O                        
276200                          ADD +1 TO INDX                                  
276300                        ELSE                                              
276400                          ADD +1 TO INDX                                  
276500                        END-IF                                            
276600                      END-IF                                              
276700                    END-PERFORM                                           
276800                 END-IF                                                   
276900                 PERFORM IMS-GET-SATB11-OKONV                             
277000               END-PERFORM                                                
277100             END-IF                                                       
277200           END-IF                                                         
277300         END-IF                                                           
277400       END-IF                                                             
277500     END-IF                                                               
277600     .                                                                    
277700     EJECT                                                                
277800                                                                          
277900 DAA-TID-ENL-FORMEL SECTION.                                              
278000     SKIP2                                                                
278100*                    *****************************************            
278200*                    *** ERSÄTTNINGS-DATUM BERÄKNAS ENLIGT ***            
278300*                    *** FORMEL. GÄLLER ERSKOD 1 OCH 4     ***            
278400*                    *** BEHOV FRÅN BEHOVSTABELL           ***            
278500*                    *****************************************            
278600                                                                          
278700     PERFORM IMS-GET-ROT-601                                              
278800     IF SEGMENT-FINNS                                                     
278900       PERFORM IMS-GET-ARTC11                                             
279000       IF SEGMENT-FINNS                                                   
279100         MOVE CLAG-KVLS    TO SPAR-KVLS-C1                                
279200         MOVE CLAG-KVRESS  TO SPAR-KVRESS-C1                              
279300         MOVE CLAG-KVROS   TO SPAR-KVROS-C1                               
279400         COMPUTE SPAR-KVAKS ROUNDED =                                     
279500            CLAG-KVAKS-CDC + CLAG-KVAKS-T + CLAG-KVAKS-PAV                
279600       END-IF                                                             
279700     END-IF                                                               
279800     COMPUTE WDISP-LAGER ROUNDED = SPAR-KVLS-C1 - SPAR-KVRESS-C1          
279900     + SPAR-KVAKS - SPAR-KVROS-C1                                         
280000                                                                          
280100*************************** BEHOVSTABELLEN LÄSES ETT HALVÅR               
280200*                           I STÖTEN FÖR ATT SE HUR LÄNGE                 
280300*                           DISPONIBELT LAGER RÄCKER                      
280400                                                                          
280500     MOVE NEJ             TO TIDBER-SW                                    
280600     MOVE ERS-IDARTNR (1) TO LINK-IDARTNR                                 
280700     MOVE SPACE           TO LINK-IDDC                                    
280800     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
280900     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
281000     CALL WDATKONV USING  DAT-KDDATFORM                                   
281100                          DAT-I-TIDATUM                                   
281200                          DAT-O-TIDATUM                                   
281300                          DAT-KDSVAR                                      
281400     IF DAT-KDSVAR-OK                                                     
281500       MOVE DAT-TIAA-VECKA TO WS-AAR                                      
281600       MOVE DAT-TIVV       TO WS-VECKA                                    
281700     END-IF                                                               
281800                                                                          
281900     MOVE ZERO             TO LINK-TID-AKTUELL                            
282000     MOVE WS-DATUM         TO LINK-TIAAVV-AKTUELL                         
282100     MOVE WS-DATUM         TO LINK-TIBEHOV-START                          
282200     MOVE +26              TO LINK-KVVECKOR-BEHOV                         
282300     MOVE '17'             TO LINK-KDBEHOV                                
282400     MOVE NEJ              TO LINK-FLINKLDIRLEV                           
282500     MOVE ZERO             TO VARV-TIDBER                                 
282600                                                                          
282700     PERFORM UNTIL TIDBER-KLAR                                            
282800       CALL W2222200 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB          
282900                           W222-ARTM-PCB W222-2501-PCB                    
283000                           W222-WDB6R-PCB W222-WDK7R-PCB                  
283100                           W222-WDB6-PCB W222-WDD7-PCB                    
283200                           W222-WDK7E-PCB                                 
283300                           W222-UTIL-WDK6-PCB                             
283400                           W222-UTIL-WDK7-PCB                             
283500                           W222-UTIL-WDB6-PCB                             
283600                           W222-UTUP-WDK7-PCB                             
283700                           W222-UTUP-WDB6-PCB                             
283800                           W222-UTUP-UTIL-WDK6-PCB                        
283900                           W222-UTUP-UTIL-WDK7-PCB                        
284000                           W222-UTUP-UTIL-WDB6-PCB                        
284100                                                                          
284200       IF LINK-ANROP-OK                                                   
284300         ADD +1           TO VARV-TIDBER                                  
284400         MOVE ZERO        TO BEHIDEX                                      
284500         PERFORM UNTIL TIDBER-KLAR OR BEHIDEX > 25                        
284600           ADD +1         TO BEHIDEX                                      
284700           SUBTRACT LINK-KVBEHOV-VECKA (BEHIDEX)                          
284800           FROM WDISP-LAGER                                               
284900           IF WDISP-LAGER <= ZERO                                         
285000             ADD BEHIDEX -1 TO WS-VECKA                                   
285100             IF WS-VECKA > 52                                             
285200               ADD 1        TO WS-AAR                                     
285300               SUBTRACT 52  FROM WS-VECKA                                 
285400             END-IF                                                       
285500             MOVE JA      TO TIDBER-SW                                    
285600           END-IF                                                         
285700         END-PERFORM                                                      
285800         IF TIDBER-EJ-KLAR                                                
285900           ADD  26          TO WS-VECKA                                   
286000           IF WS-VECKA > 52                                               
286100             ADD  1         TO WS-AAR                                     
286200             SUBTRACT  52   FROM WS-VECKA                                 
286300           END-IF                                                         
286400           MOVE WS-DATUM    TO LINK-TIBEHOV-START                         
286500         END-IF                                                           
286600         IF VARV-TIDBER = 3                                               
286700           MOVE JA          TO TIDBER-SW                                  
286800         END-IF                                                           
286900       ELSE                                                               
287000         MOVE '015'            TO WFELKOD-R07                             
287100         MOVE TEXT2            TO WFELTEXT                                
287200         MOVE 2304-IDARTNR-ING TO WARTNR                                  
287300         MOVE MASKINELL        TO WMASKINELL                              
287400         PERFORM S10-SKRIV-FOV                                            
287500         MOVE WS-AAR           TO W-AAR                                   
287600         MOVE WS-VECKA         TO W-VECKA                                 
287700         MOVE JA               TO TIDBER-SW                               
287800       END-IF                                                             
287900     END-PERFORM                                                          
288000                                                                          
288100     MOVE WS-DATUM          TO ERS-DATUM-X                                
288200                                                                          
288300     MOVE '1' TO W-ERS-DATUM-DAG                                          
288400     MOVE ERS-DATUM-KOLL TO LAEGG-TILL-EN-VE-DAT                          
288500     ADD +1 TO LAEGG-VECKA                                                
288600     IF LAEGG-VECKA > 52                                                  
288700       ADD +1 TO LAEGG-AAR                                                
288800       SUBTRACT 52 FROM LAEGG-VECKA                                       
288900     END-IF                                                               
289000                                                                          
289100     MOVE LAEGG-TILL-EN-VE-DAT TO ERS-DATUM-KOLL                          
289200                                                                          
289300     MOVE ERS-DATUM-KOLL TO DAT-I-TIDATUM                                 
289400     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
289500     CALL WDATKONV USING DAT-KDDATFORM                                    
289600                         DAT-I-TIDATUM                                    
289700                         DAT-O-TIDATUM                                    
289800                         DAT-KDSVAR                                       
289900     MOVE DAT-TIAA TO ERS-AAR                                             
290000     MOVE DAT-TIMM TO ERS-MAN                                             
290100     MOVE DAT-TIDD TO ERS-DAG                                             
290200                                                                          
290300     MOVE ERSAETTNINGS-DATUM TO UPPD-DATUM                                
290400     .                                                                    
290500     EJECT                                                                
290600                                                                          
290700 DAB-KOLLA-PUBL-VECKA SECTION.                                            
290800     SKIP2                                                                
290900     MOVE ZERO TO SPAR-TIFINLV-DATUM                                      
291000     MOVE +2 TO INDX                                                      
291100     PERFORM UNTIL INDX > ERS-TABANT                                      
291200       MOVE ERS-IDARTNR(INDX) TO W-IDARTNR                                
291300       PERFORM IMS-GET-ROT-601                                            
291400       IF SEGMENT-FINNS                                                   
291500         MOVE SPAR-TIFINLV-DATUM   TO TMP1-YYWWD                          
291600         MOVE ART-TIFINLV          TO TMP2-YYWWD                          
291700         PERFORM WY2000P2                                                 
291800         IF TMP1-YYWWD <= TMP2-YYWWD                                      
291900           MOVE ART-TIFINLV TO SPAR-TIFINLV-DATUM                         
292000         END-IF                                                           
292100         ADD +1 TO INDX                                                   
292200       END-IF                                                             
292300     END-PERFORM                                                          
292400                                                                          
292500     MOVE SPAR-TIFINLV-DATUM   TO TMP1-YYWWD                              
292600     MOVE PUBL-DAGENS-DATUM    TO TMP2-YYWWD                              
292700     PERFORM WY2000P2                                                     
292800     IF TMP1-YYWWD < TMP2-YYWWD                                           
292900       ADD +1 TO PUBL-VECKA                                               
293000       IF PUBL-VECKA > 52                                                 
293100         ADD +1 TO PUBL-AAR                                               
293200         SUBTRACT 52 FROM PUBL-VECKA                                      
293300       END-IF                                                             
293400       MOVE PUBL-DAGENS-DATUM TO DAT-I-TIDATUM                            
293500       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
293600       CALL WDATKONV USING DAT-KDDATFORM                                  
293700                           DAT-I-TIDATUM                                  
293800                           DAT-O-TIDATUM                                  
293900                           DAT-KDSVAR                                     
294000       IF DAT-KDSVAR-OK                                                   
294100         MOVE DAT-TIAA TO UPPD-AAR                                        
294200         MOVE DAT-TIMM TO UPPD-MAN                                        
294300         MOVE DAT-TIDD TO UPPD-DAG                                        
294400       END-IF                                                             
294500                                                                          
294600     ELSE                                                                 
294700       ADD +1 TO SPAR-TIFINLV-VECKA                                       
294800       IF SPAR-TIFINLV-VECKA > 52                                         
294900         ADD 1 TO SPAR-TIFINLV-AAR                                        
295000         SUBTRACT 52 FROM SPAR-TIFINLV-VECKA                              
295100       END-IF                                                             
295200       MOVE SPAR-TIFINLV-DATUM TO DAT-I-TIDATUM                           
295300       MOVE 'AAVVD'            TO DAT-KDDATFORM                           
295400       CALL WDATKONV USING DAT-KDDATFORM                                  
295500                           DAT-I-TIDATUM                                  
295600                           DAT-O-TIDATUM                                  
295700                           DAT-KDSVAR                                     
295800       IF DAT-KDSVAR-OK                                                   
295900         MOVE DAT-TIAA TO UPPD-AAR                                        
296000         MOVE DAT-TIMM TO UPPD-MAN                                        
296100         MOVE DAT-TIDD TO UPPD-DAG                                        
296200       END-IF                                                             
296300     END-IF                                                               
296400     .                                                                    
296500     EJECT                                                                
296600                                                                          
296700 Z-FINIT SECTION.                                                         
296800     SKIP2                                                                
296900                                                                          
297000     MOVE 'S' TO POSTSUM-OPKOD                                            
297100     CALL POSTSUM USING POSTSUM-PARM                                      
297200     .                                                                    
297300     EJECT                                                                
297400 X-TAG-CHECKPOINT-2304   SECTION.                                         
297500                                                                          
297600* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
297700* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
297800     PERFORM IMS-CHECKPOINT                                               
297900     MOVE ZERO TO CHKP-ANT                                                
298000* --- LÄS OM DATABAS OM DET BEHÖVS                                        
298100     PERFORM IMS-GET-2303-ROT                                             
298200     PERFORM IMS-GETNEXT-2304                                             
298300     .                                                                    
298400     EJECT                                                                
298500 X-TAG-CHECKPOINT-2302   SECTION.                                         
298600                                                                          
298700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
298800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
298900     PERFORM IMS-CHECKPOINT                                               
299000     MOVE ZERO TO CHKP-ANT                                                
299100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
299200     PERFORM IMS-GET-2301-ROT                                             
299300     PERFORM IMS-GETNEXT-2302                                             
299400     .                                                                    
299500     EJECT                                                                
299600 X-TAG-CHECKPOINT-SATB   SECTION.                                         
299700                                                                          
299800* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
299900* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
300000     MOVE SATB-STR-IDARTNR TO W-IDARTNR                                   
300100                                                                          
300200     PERFORM IMS-CHECKPOINT                                               
300300     MOVE ZERO TO CHKP-ANT                                                
300400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
300500     PERFORM IMS-GET-SATB01-ART                                           
300600     .                                                                    
300700     EJECT                                                                
300800 X-TAG-CHECKPOINT-WDJ1 SECTION.                                           
300900                                                                          
301000     PERFORM IMS-CHECKPOINT                                               
301100     MOVE ZERO TO CHKP-ANT                                                
301200                                                                          
301300     PERFORM IMS-GET-2303-ROT                                             
301400     PERFORM IMS-GETNEXT-2304                                             
301500     PERFORM UNTIL SEGMENT-SAKNAS                                         
301600        OR (2304-IDARTNR-ING = W-IDARTNRC)                                
301700            PERFORM IMS-GETNEXT-2304                                      
301800     END-PERFORM                                                          
301900                                                                          
302000     PERFORM IMS-GET-SATB-CSEQ-UNIK1                                      
302100     PERFORM UNTIL SEGMENT-SAKNAS                                         
302200        OR (SATB01C-STR-IDARTNR = WS-SPAR-IDARTNR-STR                     
302300        AND SATB11C-RAD-IDRADNR = WS-SPAR-IDRADNR)                        
302400            PERFORM IMS-GET-SATB-CSEQ-NEXT                                
302500     END-PERFORM                                                          
302600                                                                          
302700     .                                                                    
302800     EJECT                                                                
302900 S02-BEARBETA-STOPPDATUM SECTION.                                         
303000     SKIP2                                                                
303100*                    ******************************************           
303200*                    *** GÖR OM STOPPDATUM TILL ÅR/VECKA    ***           
303300*                    ******************************************           
303400*                                                                         
303500     MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                              
303600     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
303700     CALL WDATKONV USING DAT-KDDATFORM                                    
303800                         DAT-I-TIDATUM                                    
303900                         DAT-O-TIDATUM                                    
304000                         DAT-KDSVAR                                       
304100     IF DAT-KDSVAR-OK                                                     
304200       MOVE DAT-TIAA-VECKA TO TRANS-AAR                                   
304300       MOVE DAT-TIVV       TO TRANS-VECKA                                 
304400     END-IF                                                               
304500     MOVE TRANS-DATUM TO 2234-TIBEHDAT                                    
304600     .                                                                    
304700     EJECT                                                                
304800                                                                          
304900 S03-BEARBETA-STARTDATUM SECTION.                                         
305000     SKIP2                                                                
305100*                    ******************************************           
305200*                    *** GÖR OM STARTDATUM TILL ÅR/VECKA    ***           
305300*                    ******************************************           
305400*                                                                         
305500     MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                              
305600     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
305700     CALL WDATKONV USING DAT-KDDATFORM                                    
305800                         DAT-I-TIDATUM                                    
305900                         DAT-O-TIDATUM                                    
306000                         DAT-KDSVAR                                       
306100     IF DAT-KDSVAR-OK                                                     
306200       MOVE DAT-TIAA-VECKA TO TRANS-AAR                                   
306300       MOVE DAT-TIVV       TO TRANS-VECKA                                 
306400     END-IF                                                               
306500     MOVE TRANS-DATUM TO 2234-TIBEHDAT                                    
306600     .                                                                    
306700     EJECT                                                                
306800                                                                          
306900 S04-LAES-CSEQ SECTION.                                                   
307000     SKIP2                                                                
307100     MOVE NEJ TO RAD-SW                                                   
307200     PERFORM IMS-GET-SATB-CSEQ-UNIK                                       
307300     PERFORM UNTIL (SEGMENT-SAKNAS) OR (RAD-FINNS)                        
307400       IF SEGMENT-FINNS                                                   
307500         MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                       
307600         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
307700         PERFORM WY2000P1                                                 
307800         IF TMP1-YYMMDD > TMP2-YYMMDD   AND                               
307900            SATB01C-STR-TIBORT = ZERO   AND                               
308000            SATB01C-STR-IDLEVNR = '1002 ' AND                             
308100            SATB01C-STR-IDARTNR < 100000000                               
308200            MOVE JA TO RAD-SW                                             
308300         ELSE                                                             
308400           PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK04                         
308500         END-IF                                                           
308600       ELSE                                                               
308700         MOVE NEJ TO RAD-SW                                               
308800       END-IF                                                             
308900     END-PERFORM                                                          
309000                                                                          
309100     IF RAD-FINNS-EJ                                                      
309200       MOVE SATB-RAD-IDARTNR TO W-IDARTNR                                 
309300       PERFORM IMS-GET-ROT-601                                            
309400       IF SEGMENT-FINNS                                                   
309500         MOVE NEJ TO ART-FLIART                                           
309600         PERFORM IMS-UPPD-601                                             
309700       END-IF                                                             
309800     END-IF                                                               
309900     .                                                                    
310000     EJECT                                                                
310100                                                                          
310200 S05-NOLLSTALL-TABELL SECTION.                                            
310300     SKIP2                                                                
310400     MOVE +1 TO TAB-INDX                                                  
310500     PERFORM UNTIL TAB-INDX > MAX-TABELL-LAENGD                           
310600       MOVE +0 TO STRUKTURNR(TAB-INDX)                                    
310700       ADD +1 TO TAB-INDX                                                 
310800     END-PERFORM                                                          
310900     .                                                                    
311000     EJECT                                                                
311100                                                                          
311200 S06-LAES-CSEQ SECTION.                                                   
311300     SKIP2                                                                
311400     MOVE NEJ TO RAD-SW                                                   
311500     PERFORM IMS-GET-SATB-CSEQ-UNIK                                       
311600     IF SEGMENT-FINNS                                                     
311700       PERFORM UNTIL SEGMENT-SAKNAS OR RAD-FINNS                          
311800         MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                       
311900         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
312000         PERFORM WY2000P1                                                 
312100         IF TMP1-YYMMDD > TMP2-YYMMDD   AND                               
312200            SATB01C-STR-TIBORT = ZERO   AND                               
312300            SATB01C-STR-IDLEVNR = '1002 '  AND                            
312400            SATB01C-STR-IDARTNR < 100000000                               
312500            IF SATB01C-STR-IDARTNR = SPAR-KOLL-FLER-ARTNR                 
312600              PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK04                      
312700            ELSE                                                          
312800              MOVE JA TO RAD-SW                                           
312900            END-IF                                                        
313000         ELSE                                                             
313100           PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK04                         
313200         END-IF                                                           
313300       END-PERFORM                                                        
313400     END-IF                                                               
313500                                                                          
313600     IF RAD-FINNS-EJ                                                      
313700       MOVE SATB-RAD-IDARTNR TO W-IDARTNR                                 
313800       PERFORM IMS-GET-ROT-601                                            
313900       IF SEGMENT-FINNS                                                   
314000         MOVE NEJ TO ART-FLIART                                           
314100         PERFORM IMS-UPPD-601                                             
314200       END-IF                                                             
314300     END-IF                                                               
314400     .                                                                    
314500     EJECT                                                                
314600                                                                          
314700 S10-SKRIV-FOV SECTION.                                                   
314800     SKIP2                                                                
314900*                    ******************************************           
315000*                    *** SKRIVER FEL OCH VARNINGAR          ***           
315100*                    ******************************************           
315200                                                                          
315300     MOVE ZERO  TO U01FOV-W092W001                                        
315400     MOVE SPACE TO U01FOV-FELTEXT                                         
315500     MOVE WSATSNR             TO U01FOV-SORTBGP                           
315600     MOVE WFELKOD-R07         TO U01FOV-IDFELKODX                         
315700     MOVE 'R07'               TO U01FOV-IDPTYP                            
315800     IF WFELKOD-R07 = '015'                                               
315900       MOVE WFELTEXT TO U01FOV-FELTEXT                                    
316000     END-IF                                                               
316100     MOVE U01FOV-W11201 TO FIL-WDR301-DATA                                
316200     ACCEPT W-TID FROM TIME                                               
316300     IF W-TID = FIL-TIKLOCK                                               
316400        ADD +1       TO FIL-IDSEKVNR                                      
316500     ELSE                                                                 
316600        MOVE W-TID   TO FIL-TIKLOCK                                       
316700        MOVE 1       TO FIL-IDSEKVNR                                      
316800     END-IF                                                               
316900                                                                          
317000     PERFORM IMS-ISRT-UTFIL                                               
317100                                                                          
317200     MOVE 'W11201' TO POSTSUM-FDNAMN                                      
317300     MOVE 'W11210D1' TO POSTSUM-DDNAMN2                                   
317400     MOVE 'R07' TO POSTSUM-TRANSTYP                                       
317500     CALL POSTSUM USING POSTSUM-PARM                                      
317600     MOVE SPACE TO WFELTEXT                                               
317700     .                                                                    
317800     EJECT                                                                
317900                                                                          
318000 S13-KOLL-SATS-EJ-I-TILLK SECTION.                                        
318100     SKIP2                                                                
318200******************************************************************        
318300* KONTROLL ATT STRUKTUREN EJ INGÅR I TILLKOMMANDE ARTIKEL ( FÅR           
318400* EJ INGÅ I SIG SJÄLV ) VID PÅTRÄFFANDE AV EN RAD SOM ÄR EN SATS          
318500* SPARAS DETTA ARTIKELNR I EN TABELL DÄR DENNA STRUKTUR SEDAN             
318600* KONTROLLERAS.                                                           
318700******************************************************************        
318800                                                                          
318900     PERFORM S05-NOLLSTALL-TABELL                                         
319000     MOVE +1 TO TAB-INDX                                                  
319100                                                                          
319200     PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK13                               
319300     PERFORM UNTIL SEGMENT-SAKNAS                                         
319400       IF SEGMENT-FINNS                                                   
319500         MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                       
319600         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
319700         PERFORM WY2000P1                                                 
319800         IF TMP1-YYMMDD > TMP2-YYMMDD   AND                               
319900            SATB01C-STR-TIBORT = ZERO   AND                               
320000            SATB01C-STR-IDLEVNR = '1002 '  AND                            
320100            SATB01C-STR-IDARTNR < 100000000                               
320200            IF SATB11C-RAD-IDARTNR = SPAR-IDARTNR-STRUKTUR                
320300              MOVE NEJ TO INDATA-SW                                       
320400              MOVE '015'  TO WFELKOD-R07                                  
320500              MOVE TEXT12 TO WFELTEXT                                     
320600              MOVE SPAR-IDARTNR-STRUKTUR TO WARTNR                        
320700              PERFORM S10-SKRIV-FOV                                       
320800            ELSE                                                          
320900              IF SATB11C-RAD-IDSTRTYP = 'S' OR 'K'                        
321000                MOVE SATB11C-RAD-IDARTNR TO SPAR-RAD-IDARTNR              
321100                PERFORM S15-LAEGG-UPP-RAD-I-TABELL                        
321200              END-IF                                                      
321300            END-IF                                                        
321400         END-IF                                                           
321500         PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK13                           
321600       END-IF                                                             
321700     END-PERFORM                                                          
321800                                                                          
321900 EJECT                                                                    
322000                                                                          
322100     IF INDATA-OK                                                         
322200       IF TAB-INDX > 1                                                    
322300         MOVE 1 TO TAB-INDX2                                              
322400         PERFORM UNTIL (TAB-INDX2 > (TAB-INDX - 1) ) OR                   
322500                       (INDATA-FEL)                                       
322600           MOVE STRUKTURNR(TAB-INDX2) TO W-IDARTNR                        
322700           PERFORM IMS-GET-SATB01-ART                                     
322800           PERFORM IMS-GET-SATB11                                         
322900           PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
323000             IF SEGMENT-FINNS                                             
323100               MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                    
323200               MOVE DAGENS-DATUM        TO TMP2-YYMMDD                    
323300               PERFORM WY2000P1                                           
323400               IF TMP1-YYMMDD > TMP2-YYMMDD                               
323500                 IF SATB-RAD-IDARTNR = SPAR-IDARTNR-STRUKTUR              
323600                   MOVE NEJ TO INDATA-SW                                  
323700                   MOVE '015'  TO WFELKOD-R07                             
323800                   MOVE TEXT12 TO WFELTEXT                                
323900                   MOVE SPAR-IDARTNR-STRUKTUR TO WARTNR                   
324000                   PERFORM S10-SKRIV-FOV                                  
324100                 ELSE                                                     
324200                   IF SATB-RAD-IDSTRTYP = 'S' OR 'K'                      
324300                     MOVE SATB-RAD-IDARTNR TO SPAR-RAD-IDARTNR            
324400                     PERFORM S15-LAEGG-UPP-RAD-I-TABELL                   
324500                   END-IF                                                 
324600                 END-IF                                                   
324700               END-IF                                                     
324800               PERFORM IMS-GET-SATB11                                     
324900             END-IF                                                       
325000           END-PERFORM                                                    
325100           ADD +1 TO TAB-INDX2                                            
325200         END-PERFORM                                                      
325300       END-IF                                                             
325400     END-IF                                                               
325500     .                                                                    
325600     EJECT                                                                
325700                                                                          
325800                                                                          
325900 S14-KOLL-STRNR-EJ-I-EGEN SECTION.                                        
326000     SKIP2                                                                
326100******************************************************************        
326200* KONTROLL OM STRUKTUR INGÅR SOM RAD I EN ANNAN STRUKTUR I SÅ             
326300* FALL FÅR INTE TILLKOMMANDE-ARTIKEL VARA SAMMA SOM DETTA STRUKTUR        
326400* NUMMER (ROTEN) SEDAN KOLLAS OM STRUKTURNUMRET (ROTEN) INGÅR SOM         
326500* RAD O.S.V.. STRUKTUR INGÅR I SIG SJÄLV.                                 
326600******************************************************************        
326700                                                                          
326800     PERFORM S05-NOLLSTALL-TABELL                                         
326900     MOVE +1 TO TAB-INDX                                                  
327000                                                                          
327100     PERFORM IMS-GU-SATB-CSEQ-NEXT-TILLK14                                
327200     PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                       
327300       IF SEGMENT-FINNS                                                   
327400         MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                       
327500         MOVE DAGENS-DATUM           TO TMP2-YYMMDD                       
327600         PERFORM WY2000P1                                                 
327700         IF TMP1-YYMMDD > TMP2-YYMMDD        AND                          
327800            SATB01C-STR-TIBORT = ZERO        AND                          
327900            SATB01C-STR-IDARTNR < 100000000  AND                          
328000            SATB01C-STR-IDLEVNR = '1002 '                                 
328100                                                                          
328200            IF SATB01C-STR-IDARTNR = SPAR-IDARTNR-TILLK                   
328300              MOVE NEJ TO INDATA-SW                                       
328400              MOVE '015'  TO WFELKOD-R07                                  
328500              MOVE TEXT13 TO WFELTEXT                                     
328600              MOVE SATB01C-STR-IDARTNR TO WARTNR                          
328700              PERFORM S10-SKRIV-FOV                                       
328800            ELSE                                                          
328900              IF SATB01C-STR-IDSTRTYP = 'S' OR 'K'                        
329000                PERFORM S16-LAEGG-UPP-ROT-I-TABELL                        
329100              END-IF                                                      
329200              PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK14                      
329300            END-IF                                                        
329400         ELSE                                                             
329500           PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK14                         
329600         END-IF                                                           
329700       END-IF                                                             
329800     END-PERFORM                                                          
329900                                                                          
330000     IF INDATA-OK                                                         
330100       IF TAB-INDX > 1                                                    
330200         MOVE 1 TO TAB-INDX2                                              
330300         PERFORM UNTIL (TAB-INDX2 > (TAB-INDX - 1) ) OR                   
330400                       (INDATA-FEL)                                       
330500           MOVE STRUKTURNR(TAB-INDX2) TO W-IDARTNRC                       
330600           PERFORM IMS-GU-SATB-CSEQ-NEXT-TILLK14                          
330700           PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                 
330800                                                                          
330900             IF SEGMENT-FINNS                                             
331000               MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                 
331100               MOVE DAGENS-DATUM           TO TMP2-YYMMDD                 
331200               PERFORM WY2000P1                                           
331300               IF TMP1-YYMMDD > TMP2-YYMMDD   AND                         
331400                  SATB01C-STR-TIBORT = ZERO   AND                         
331500                  SATB01C-STR-IDLEVNR = '1002 '  AND                      
331600                  SATB01C-STR-IDARTNR < 100000000                         
331700                                                                          
331800                  IF SATB01C-STR-IDARTNR = SPAR-IDARTNR-TILLK             
331900                    MOVE NEJ TO INDATA-SW                                 
332000                    MOVE '015'  TO WFELKOD-R07                            
332100                    MOVE TEXT13 TO WFELTEXT                               
332200                    MOVE SATB01C-STR-IDARTNR TO WARTNR                    
332300                    PERFORM S10-SKRIV-FOV                                 
332400                  ELSE                                                    
332500                    IF SATB01C-STR-IDSTRTYP = 'S' OR 'K'                  
332600                      PERFORM S16-LAEGG-UPP-ROT-I-TABELL                  
332700                    END-IF                                                
332800                  END-IF                                                  
332900                  PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK14                  
333000               ELSE                                                       
333100                 PERFORM IMS-GET-SATB-CSEQ-NEXT-TILLK14                   
333200               END-IF                                                     
333300             END-IF                                                       
333400           END-PERFORM                                                    
333500           ADD +1 TO TAB-INDX2                                            
333600         END-PERFORM                                                      
333700       END-IF                                                             
333800     END-IF                                                               
333900     .                                                                    
334000     EJECT                                                                
334100                                                                          
334200 S15-LAEGG-UPP-RAD-I-TABELL SECTION.                                      
334300     SKIP2                                                                
334400******************************************************************        
334500* OM STRUKTURNUMMER (RAD) EJ FINNS I TABELL LÄGGS DET UPP        *        
334600******************************************************************        
334700                                                                          
334800     MOVE +1 TO KONTROLL-TAB-INDX                                         
334900     MOVE NEJ TO STRUKTURNR-FINNS-I-TABELL-SW                             
335000                                                                          
335100     PERFORM UNTIL KONTROLL-TAB-INDX > TAB-INDX                           
335200       IF STRUKTURNR(KONTROLL-TAB-INDX) = SPAR-RAD-IDARTNR                
335300         MOVE JA TO STRUKTURNR-FINNS-I-TABELL-SW                          
335400         MOVE +999 TO KONTROLL-TAB-INDX                                   
335500       ELSE                                                               
335600         ADD +1 TO KONTROLL-TAB-INDX                                      
335700       END-IF                                                             
335800     END-PERFORM                                                          
335900                                                                          
336000     IF STRUKTURNR-FINNS-I-TABELL                                         
336100       CONTINUE                                                           
336200     ELSE                                                                 
336300       MOVE SPAR-RAD-IDARTNR TO STRUKTURNR(TAB-INDX)                      
336400       ADD +1 TO TAB-INDX                                                 
336500     END-IF                                                               
336600     .                                                                    
336700     EJECT                                                                
336800                                                                          
336900 S16-LAEGG-UPP-ROT-I-TABELL SECTION.                                      
337000     SKIP2                                                                
337100******************************************************************        
337200* OM STRUKTURNUMMER (ROT) EJ FINNS I TABELL LÄGGS DET UPP                 
337300******************************************************************        
337400                                                                          
337500     MOVE +1 TO KONTROLL-TAB-INDX                                         
337600     MOVE NEJ TO STRUKTURNR-FINNS-I-TABELL-SW                             
337700                                                                          
337800     PERFORM UNTIL KONTROLL-TAB-INDX > TAB-INDX                           
337900       IF STRUKTURNR(KONTROLL-TAB-INDX) = SATB01C-STR-IDARTNR             
338000         MOVE JA TO STRUKTURNR-FINNS-I-TABELL-SW                          
338100         MOVE +999 TO KONTROLL-TAB-INDX                                   
338200       ELSE                                                               
338300         ADD +1 TO KONTROLL-TAB-INDX                                      
338400       END-IF                                                             
338500     END-PERFORM                                                          
338600                                                                          
338700     IF STRUKTURNR-FINNS-I-TABELL                                         
338800       CONTINUE                                                           
338900     ELSE                                                                 
339000       MOVE SATB01C-STR-IDARTNR TO STRUKTURNR(TAB-INDX)                   
339100       ADD +1 TO TAB-INDX                                                 
339200     END-IF                                                               
339300     .                                                                    
339400     EJECT                                                                
339500                                                                          
339600                                                                          
339700 S21-KOPIERA-NOTERINGSEGMENT SECTION.                                     
339800     SKIP2                                                                
339900     PERFORM IMS-GET-SATB22-OKONV                                         
340000     PERFORM UNTIL SEGMENT-SAKNAS                                         
340100       IF SEGMENT-FINNS                                                   
340200         MOVE IO-AREA-OKONV TO IO-AREA-KONV                               
340300         PERFORM IMS-ISRT-SATB22-KONV                                     
340400         PERFORM IMS-GET-SATB22-OKONV                                     
340500       END-IF                                                             
340600     END-PERFORM                                                          
340700     .                                                                    
340800     EJECT                                                                
340900                                                                          
341000                                                                          
341100                                                                          
341200*************  IMS-SEKTIONER  ************                                
341300                                                                          
341400 IMS-GET-ARTC01-2 SECTION.                                                
341500     SKIP2                                                                
341600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
341700           DELIMITED BY SIZE INTO SSA1                                    
341800     MOVE '  GE' TO GODK-STATUSKODER                                      
341900     CALL CBLTDLI USING GU ART2-PCB DLI-IO-AREA SSA1                      
342000     MOVE ART2-STATUS-CODE TO STATUS-WS                                   
342100     PERFORM IMS-STATUSKONTROLL                                           
342200     .                                                                    
342300                                                                          
342400 IMS-GET-SATB01-ART SECTION.                                              
342500     SKIP2                                                                
342600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
342700           DELIMITED BY SIZE INTO SSA1                                    
342800     MOVE '  GE' TO GODK-STATUSKODER                                      
342900     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
343000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
343100     PERFORM IMS-STATUSKONTROLL                                           
343200     .                                                                    
343300 IMS-GET-SATB01 SECTION.                                                  
343400     SKIP2                                                                
343500     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
343600           DELIMITED BY SIZE INTO SSA1                                    
343700     MOVE '  GE' TO GODK-STATUSKODER                                      
343800     CALL CBLTDLI USING GHU SATB2-PCB DLI-IO-AREA SSA1                    
343900     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
344000     PERFORM IMS-STATUSKONTROLL                                           
344100     .                                                                    
344200                                                                          
344300 IMS-GET-SATB01-ART-OKVAL-UNIK SECTION.                                   
344400     SKIP2                                                                
344500     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-OKVAL-X ')'                   
344600           DELIMITED BY SIZE INTO SSA1                                    
344700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
344800     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
344900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
345000     PERFORM IMS-STATUSKONTROLL                                           
345100     .                                                                    
345200     EJECT                                                                
345300                                                                          
345400 IMS-GET-SATB01-ART-OKVAL SECTION.                                        
345500     SKIP2                                                                
345600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-OKVAL-X ')'                   
345700           DELIMITED BY SIZE INTO SSA1                                    
345800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
345900     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA SSA1                      
346000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
346100     PERFORM IMS-STATUSKONTROLL                                           
346200     .                                                                    
346300     EJECT                                                                
346400                                                                          
346500 IMS-GET-SATB-RAD-FIRST SECTION.                                          
346600     SKIP2                                                                
346700     STRING 'WLSATB11*F(WDJ111KY=>' W-IDKDSTRRAD-X                        
346800                                    W-IDRADNR-X  ')'                      
346900           DELIMITED BY SIZE INTO SSA1                                    
347000     MOVE '  GE' TO GODK-STATUSKODER                                      
347100     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
347200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
347300     PERFORM IMS-STATUSKONTROLL                                           
347400     .                                                                    
347500                                                                          
347600 IMS-GET-SATB11 SECTION.                                                  
347700     SKIP2                                                                
347800     MOVE 'WLSATB11 ' TO SSA1                                             
347900     MOVE '  GE' TO GODK-STATUSKODER                                      
348000     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
348100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
348200     PERFORM IMS-STATUSKONTROLL                                           
348300     .                                                                    
348400     EJECT                                                                
348500                                                                          
348600 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
348700     SKIP2                                                                
348800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
348900          DELIMITED BY SIZE INTO SSA1                                     
349000     MOVE 'WLSATB01 ' TO SSA2                                             
349100     MOVE '  GE' TO GODK-STATUSKODER                                      
349200     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-3 SSA1 SSA2             
349300     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
349400     PERFORM IMS-STATUSKONTROLL                                           
349500     .                                                                    
349600                                                                          
349700                                                                          
349800 IMS-GET-SATB-CSEQ-NEXT-TILLK13 SECTION.                                  
349900     SKIP2                                                                
350000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
350100          DELIMITED BY SIZE INTO SSA1                                     
350200     MOVE 'WLSATB01 ' TO SSA2                                             
350300     MOVE '  GE' TO GODK-STATUSKODER                                      
350400     CALL CBLTDLI USING GN SATB-C1-PCB DLI-IO-AREA-3 SSA1 SSA2            
350500     MOVE SATB-C1-STATUS-CODE TO STATUS-WS                                
350600     PERFORM IMS-STATUSKONTROLL                                           
350700     .                                                                    
350800                                                                          
350900 IMS-GET-SATB-CSEQ-NEXT-TILLK14 SECTION.                                  
351000     SKIP2                                                                
351100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
351200          DELIMITED BY SIZE INTO SSA1                                     
351300     MOVE 'WLSATB01 ' TO SSA2                                             
351400     MOVE '  GE' TO GODK-STATUSKODER                                      
351500     CALL CBLTDLI USING GN SATB-C2-PCB DLI-IO-AREA-3 SSA1 SSA2            
351600     MOVE SATB-C2-STATUS-CODE TO STATUS-WS                                
351700     PERFORM IMS-STATUSKONTROLL                                           
351800     .                                                                    
351900                                                                          
352000 IMS-GU-SATB-CSEQ-NEXT-TILLK14 SECTION.                                   
352100     SKIP2                                                                
352200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
352300          DELIMITED BY SIZE INTO SSA1                                     
352400     MOVE 'WLSATB01 ' TO SSA2                                             
352500     MOVE '  GE' TO GODK-STATUSKODER                                      
352600     CALL CBLTDLI USING GN SATB-C2-PCB DLI-IO-AREA-3 SSA1 SSA2            
352700     MOVE SATB-C2-STATUS-CODE TO STATUS-WS                                
352800     PERFORM IMS-STATUSKONTROLL                                           
352900     .                                                                    
353000                                                                          
353100 IMS-GET-SATB-CSEQ-NEXT-TILLK04 SECTION.                                  
353200     SKIP2                                                                
353300     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
353400          DELIMITED BY SIZE INTO SSA1                                     
353500     MOVE 'WLSATB01 ' TO SSA2                                             
353600     MOVE '  GE' TO GODK-STATUSKODER                                      
353700     CALL CBLTDLI USING GN SATB-C3-PCB DLI-IO-AREA-3 SSA1 SSA2            
353800     MOVE SATB-C3-STATUS-CODE TO STATUS-WS                                
353900     PERFORM IMS-STATUSKONTROLL                                           
354000     .                                                                    
354100                                                                          
354200 IMS-GET-SATB-CSEQ-UNIK SECTION.                                          
354300     SKIP2                                                                
354400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
354500          DELIMITED BY SIZE INTO SSA1                                     
354600     MOVE 'WLSATB01 ' TO SSA2                                             
354700     MOVE '  GE' TO GODK-STATUSKODER                                      
354800     CALL CBLTDLI USING GU SATB-C3-PCB DLI-IO-AREA-3 SSA1 SSA2            
354900     MOVE SATB-C3-STATUS-CODE TO STATUS-WS                                
355000     PERFORM IMS-STATUSKONTROLL                                           
355100     .                                                                    
355200                                                                          
355300 IMS-GET-SATB-CSEQ-UNIK1 SECTION.                                         
355400     SKIP2                                                                
355500     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
355600          DELIMITED BY SIZE INTO SSA1                                     
355700     MOVE 'WLSATB01 ' TO SSA2                                             
355800     MOVE '  GE' TO GODK-STATUSKODER                                      
355900     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA-3 SSA1 SSA2             
356000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
356100     PERFORM IMS-STATUSKONTROLL                                           
356200     .                                                                    
356300                                                                          
356400*** IMS-ANROP MOT WDJ1 FÖR DEN OKONVERTERADE STRUKTUREN                   
356500                                                                          
356600 IMS-GET-SATB01-OKONV SECTION.                                            
356700     SKIP2                                                                
356800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
356900          DELIMITED BY SIZE INTO SSA1                                     
357000     MOVE '  GE' TO GODK-STATUSKODER                                      
357100     CALL CBLTDLI USING GHU SATB-O-PCB DLI-IO-AREA-OKONV SSA1             
357200     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
357300     PERFORM IMS-STATUSKONTROLL                                           
357400     .                                                                    
357500     EJECT                                                                
357600                                                                          
357700 IMS-GET-SATB11-OKONV SECTION.                                            
357800     SKIP2                                                                
357900     MOVE 'WLSATB11 ' TO SSA1                                             
358000     MOVE '  GE' TO GODK-STATUSKODER                                      
358100     CALL CBLTDLI USING GHNP SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
358200     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
358300     PERFORM IMS-STATUSKONTROLL                                           
358400     .                                                                    
358500                                                                          
358600 IMS-GET-SATB22-OKONV SECTION.                                            
358700     SKIP2                                                                
358800     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-OKONV-X ')'                  
358900          DELIMITED BY SIZE INTO SSA1                                     
359000     MOVE 'WLSATB22 ' TO SSA2                                             
359100     MOVE '  GE' TO GODK-STATUSKODER                                      
359200     CALL CBLTDLI USING GHNP SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
359300                                                          SSA2            
359400     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
359500     PERFORM IMS-STATUSKONTROLL                                           
359600     .                                                                    
359700                                                                          
359800 IMS-ISRT-SATB01-OKONV SECTION.                                           
359900     SKIP2                                                                
360000     MOVE 'WLSATB01 ' TO SSA1                                             
360100     MOVE '  ' TO GODK-STATUSKODER                                        
360200     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
360300     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
360400     PERFORM IMS-STATUSKONTROLL                                           
360500     ADD +1 TO CHKP-ANT                                                   
360600                                                                          
360700     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
360800     MOVE 'SATB01  ' TO POSTSUM-DDNAMN2                                   
360900     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
361000     CALL POSTSUM USING POSTSUM-PARM                                      
361100     .                                                                    
361200     EJECT                                                                
361300                                                                          
361400 IMS-ISRT-SATB11-OKONV SECTION.                                           
361500     SKIP2                                                                
361600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
361700          DELIMITED BY SIZE INTO SSA1                                     
361800     MOVE 'WLSATB11 ' TO SSA2                                             
361900     MOVE '  ' TO GODK-STATUSKODER                                        
362000     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
362100                                                          SSA2            
362200     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
362300     PERFORM IMS-STATUSKONTROLL                                           
362400     ADD +1 TO CHKP-ANT                                                   
362500                                                                          
362600     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
362700     MOVE 'SATB11  ' TO POSTSUM-DDNAMN2                                   
362800     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
362900     CALL POSTSUM USING POSTSUM-PARM                                      
363000     .                                                                    
363100                                                                          
363200 IMS-ISRT-SATB22-OKONV SECTION.                                           
363300     SKIP2                                                                
363400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
363500          DELIMITED BY SIZE INTO SSA1                                     
363600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-OKONV-X ')'                  
363700          DELIMITED BY SIZE INTO SSA2                                     
363800     MOVE 'WLSATB22 ' TO SSA3                                             
363900     MOVE '  ' TO GODK-STATUSKODER                                        
364000     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
364100                                                          SSA2            
364200                                                          SSA3            
364300     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
364400     PERFORM IMS-STATUSKONTROLL                                           
364500     ADD +1 TO CHKP-ANT                                                   
364600                                                                          
364700     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
364800     MOVE 'SATB22  ' TO POSTSUM-DDNAMN2                                   
364900     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
365000     CALL POSTSUM USING POSTSUM-PARM                                      
365100     .                                                                    
365200     EJECT                                                                
365300                                                                          
365400 IMS-DLET-SATB-OKONV SECTION.                                             
365500     SKIP2                                                                
365600     MOVE '  ' TO GODK-STATUSKODER                                        
365700     CALL CBLTDLI USING DLET SATB-O-PCB DLI-IO-AREA-OKONV                 
365800     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
365900     PERFORM IMS-STATUSKONTROLL                                           
366000     ADD +1 TO CHKP-ANT                                                   
366100                                                                          
366200     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
366300     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
366400     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
366500     CALL POSTSUM USING POSTSUM-PARM                                      
366600     .                                                                    
366700                                                                          
366800***IMS-ANROP MOT WDJ1 SOM ÄR KONVERTERAD                                  
366900                                                                          
367000 IMS-GET-SATB01-KONV SECTION.                                             
367100     SKIP2                                                                
367200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
367300          DELIMITED BY SIZE INTO SSA1                                     
367400     MOVE '  GE' TO GODK-STATUSKODER                                      
367500     CALL CBLTDLI USING GHU SATB-K-PCB DLI-IO-AREA-KONV SSA1              
367600     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
367700     PERFORM IMS-STATUSKONTROLL                                           
367800     .                                                                    
367900     EJECT                                                                
368000                                                                          
368100 IMS-GET-SATB11-KONV SECTION.                                             
368200     SKIP2                                                                
368300     MOVE 'WLSATB11 ' TO SSA1                                             
368400     MOVE '  GE' TO GODK-STATUSKODER                                      
368500     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-KONV SSA1             
368600     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
368700     PERFORM IMS-STATUSKONTROLL                                           
368800     .                                                                    
368900                                                                          
369000 IMS-GET-SATB22-KONV SECTION.                                             
369100     SKIP2                                                                
369200     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-KONV-X ')'                   
369300          DELIMITED BY SIZE INTO SSA1                                     
369400     MOVE 'WLSATB22 ' TO SSA2                                             
369500     MOVE '  GE' TO GODK-STATUSKODER                                      
369600     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-KONV SSA1             
369700                                                         SSA2             
369800     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
369900     PERFORM IMS-STATUSKONTROLL                                           
370000     .                                                                    
370100                                                                          
370200 IMS-ISRT-SATB01-KONV SECTION.                                            
370300     SKIP2                                                                
370400     MOVE 'WLSATB01 ' TO SSA1                                             
370500     MOVE '  ' TO GODK-STATUSKODER                                        
370600     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-KONV SSA1             
370700     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
370800     PERFORM IMS-STATUSKONTROLL                                           
370900     ADD +1 TO CHKP-ANT                                                   
371000                                                                          
371100     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
371200     MOVE 'SATB01  ' TO POSTSUM-DDNAMN2                                   
371300     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
371400     CALL POSTSUM USING POSTSUM-PARM                                      
371500     .                                                                    
371600     EJECT                                                                
371700                                                                          
371800 IMS-ISRT-SATB11-KONV SECTION.                                            
371900     SKIP2                                                                
372000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
372100          DELIMITED BY SIZE INTO SSA1                                     
372200     MOVE 'WLSATB11 ' TO SSA2                                             
372300     MOVE '  ' TO GODK-STATUSKODER                                        
372400     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-KONV SSA1             
372500                                                         SSA2             
372600     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
372700     PERFORM IMS-STATUSKONTROLL                                           
372800     ADD +1 TO CHKP-ANT                                                   
372900                                                                          
373000     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
373100     MOVE 'SATB11  ' TO POSTSUM-DDNAMN2                                   
373200     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
373300     CALL POSTSUM USING POSTSUM-PARM                                      
373400     .                                                                    
373500                                                                          
373600 IMS-ISRT-SATB22-KONV SECTION.                                            
373700     SKIP2                                                                
373800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
373900          DELIMITED BY SIZE INTO SSA1                                     
374000     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-KONV-X ')'                   
374100          DELIMITED BY SIZE INTO SSA2                                     
374200     MOVE 'WLSATB22 ' TO SSA3                                             
374300     MOVE '  ' TO GODK-STATUSKODER                                        
374400     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-KONV SSA1             
374500                                                         SSA2             
374600                                                         SSA3             
374700     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
374800     PERFORM IMS-STATUSKONTROLL                                           
374900     ADD +1 TO CHKP-ANT                                                   
375000                                                                          
375100     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
375200     MOVE 'SATB22  ' TO POSTSUM-DDNAMN2                                   
375300     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
375400     CALL POSTSUM USING POSTSUM-PARM                                      
375500     .                                                                    
375600     EJECT                                                                
375700                                                                          
375800 IMS-DLET-SATB-KONV SECTION.                                              
375900     SKIP2                                                                
376000     MOVE '  ' TO GODK-STATUSKODER                                        
376100     CALL CBLTDLI USING DLET SATB-K-PCB DLI-IO-AREA-KONV                  
376200     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
376300     PERFORM IMS-STATUSKONTROLL                                           
376400     ADD +1 TO CHKP-ANT                                                   
376500                                                                          
376600     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
376700     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
376800     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
376900     CALL POSTSUM USING POSTSUM-PARM                                      
377000     .                                                                    
377100                                                                          
377200 IMS-GET-ROT-601 SECTION.                                                 
377300     SKIP2                                                                
377400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
377500          DELIMITED BY SIZE INTO SSA1                                     
377600     MOVE '  GE' TO GODK-STATUSKODER                                      
377700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-1 SSA1                   
377800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
377900     PERFORM IMS-STATUSKONTROLL                                           
378000     .                                                                    
378100     EJECT                                                                
378200 IMS-GET-ARTC11 SECTION.                                                  
378300     SKIP2                                                                
378400     MOVE 'WLARTC11 ' TO SSA1                                             
378500     MOVE '  GE' TO GODK-STATUSKODER                                      
378600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-1 SSA1                   
378700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
378800     PERFORM IMS-STATUSKONTROLL                                           
378900     .                                                                    
379000                                                                          
379100 IMS-GET-ERSAETTNING-ROT SECTION.                                         
379200     SKIP2                                                                
379300     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
379400           DELIMITED BY SIZE INTO SSA1                                    
379500     MOVE '  GE' TO GODK-STATUSKODER                                      
379600     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA-A SSA1                    
379700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
379800     PERFORM IMS-STATUSKONTROLL                                           
379900     .                                                                    
380000                                                                          
380100 IMS-GET-ERSAETTNING-TILLK SECTION.                                       
380200     SKIP2                                                                
380300     MOVE 'WLERSA11' TO SSA1                                              
380400     MOVE '  GE' TO GODK-STATUSKODER                                      
380500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-A SSA1                   
380600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
380700     PERFORM IMS-STATUSKONTROLL                                           
380800     .                                                                    
380900     EJECT                                                                
381000                                                                          
381100 IMS-GET-ERSAETTNING-TID SECTION.                                         
381200     SKIP2                                                                
381300     MOVE 'WLERSA13' TO SSA1                                              
381400     MOVE '  GE' TO GODK-STATUSKODER                                      
381500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-A SSA1                   
381600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
381700     PERFORM IMS-STATUSKONTROLL                                           
381800     .                                                                    
381900                                                                          
382000 IMS-GET-WDD9-KVBR SECTION.                                               
382100     SKIP2                                                                
382200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
382300          DELIMITED BY SIZE INTO SSA1                                     
382400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
382500          DELIMITED BY SIZE INTO SSA2                                     
382600     MOVE '  GE' TO GODK-STATUSKODER                                      
382700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA-A SSA1 SSA2               
382800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
382900     PERFORM IMS-STATUSKONTROLL                                           
383000     .                                                                    
383100                                                                          
383200     EJECT                                                                
383300 IMS-INSERT-2234 SECTION.                                                 
383400     SKIP2                                                                
383500     STRING 'WLXXBY01(WDG3KEY  =' W-2233-KEY-X ')'                        
383600          DELIMITED BY SIZE INTO SSA1                                     
383700     MOVE 'WLXXBY11 ' TO SSA2                                             
383800     MOVE '   ' TO GODK-STATUSKODER                                       
383900     CALL CBLTDLI USING ISRT 2234-PCB DLI-IO-AREA-4 SSA1 SSA2             
384000     MOVE 2234-STATUS-CODE TO STATUS-WS                                   
384100     PERFORM IMS-STATUSKONTROLL                                           
384200     ADD +1 TO CHKP-ANT                                                   
384300                                                                          
384400     MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                    
384500     MOVE '2234    ' TO POSTSUM-DDNAMN2                                   
384600     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
384700     CALL POSTSUM USING POSTSUM-PARM                                      
384800     .                                                                    
384900                                                                          
385000 IMS-GET-2303-ROT SECTION.                                                
385100     SKIP2                                                                
385200     STRING 'WDG301  (WDG3KEY  =' W-2303-KEY-X ')'                        
385300          DELIMITED BY SIZE INTO SSA1                                     
385400     MOVE '  ' TO GODK-STATUSKODER                                        
385500     CALL CBLTDLI USING GU 2303-PCB DLI-IO-AREA-2 SSA1                    
385600     MOVE 2303-STATUS-CODE TO STATUS-WS                                   
385700     PERFORM IMS-STATUSKONTROLL                                           
385800     .                                                                    
385900                                                                          
386000 IMS-GET-2301-ROT SECTION.                                                
386100     SKIP2                                                                
386200     STRING 'WDG301  (WDG3KEY  =' W-2301-KEY-X ')'                        
386300          DELIMITED BY SIZE INTO SSA1                                     
386400     MOVE '  ' TO GODK-STATUSKODER                                        
386500     CALL CBLTDLI USING GU 2301-PCB DLI-IO-AREA-2 SSA1                    
386600     MOVE 2301-STATUS-CODE TO STATUS-WS                                   
386700     PERFORM IMS-STATUSKONTROLL                                           
386800     .                                                                    
386900     EJECT                                                                
387000                                                                          
387100 IMS-GETNEXT-2304 SECTION.                                                
387200     SKIP2                                                                
387300     MOVE 'WDGX2304 ' TO SSA1                                             
387400     MOVE '  GE' TO GODK-STATUSKODER                                      
387500     CALL CBLTDLI USING GHNP 2303-PCB DLI-IO-AREA-2 SSA1                  
387600     MOVE 2303-STATUS-CODE TO STATUS-WS                                   
387700     PERFORM IMS-STATUSKONTROLL                                           
387800     .                                                                    
387900                                                                          
388000 IMS-GETNEXT-2302 SECTION.                                                
388100     SKIP2                                                                
388200     MOVE 'WDGX2302 ' TO SSA1                                             
388300     MOVE '  GE' TO GODK-STATUSKODER                                      
388400     CALL CBLTDLI USING GHNP 2301-PCB DLI-IO-AREA-2 SSA1                  
388500     MOVE 2301-STATUS-CODE TO STATUS-WS                                   
388600     PERFORM IMS-STATUSKONTROLL                                           
388700     .                                                                    
388800                                                                          
388900 IMS-NYUPPLAEGG-ART-I-SATS SECTION.                                       
389000     SKIP2                                                                
389100     MOVE 'WLSATB01' TO SSA1                                              
389200     MOVE '  ' TO GODK-STATUSKODER                                        
389300     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
389400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
389500     PERFORM IMS-STATUSKONTROLL                                           
389600     ADD +1 TO CHKP-ANT                                                   
389700                                                                          
389800     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
389900     MOVE 'SATB01  ' TO POSTSUM-DDNAMN2                                   
390000     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
390100     CALL POSTSUM USING POSTSUM-PARM                                      
390200     .                                                                    
390300     EJECT                                                                
390400                                                                          
390500 IMS-TA-BORT-2302 SECTION.                                                
390600     SKIP2                                                                
390700     MOVE '  ' TO GODK-STATUSKODER                                        
390800     CALL CBLTDLI USING DLET 2301-PCB DLI-IO-AREA-2                       
390900     MOVE 2301-STATUS-CODE TO STATUS-WS                                   
391000     PERFORM IMS-STATUSKONTROLL                                           
391100     ADD +1 TO CHKP-ANT                                                   
391200                                                                          
391300     MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                    
391400     MOVE '2302    ' TO POSTSUM-DDNAMN2                                   
391500     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
391600     CALL POSTSUM USING POSTSUM-PARM                                      
391700     .                                                                    
391800     EJECT                                                                
391900                                                                          
392000 IMS-TA-BORT-2304 SECTION.                                                
392100     SKIP2                                                                
392200     MOVE '  ' TO GODK-STATUSKODER                                        
392300     CALL CBLTDLI USING DLET 2303-PCB DLI-IO-AREA-2                       
392400     MOVE 2303-STATUS-CODE TO STATUS-WS                                   
392500     PERFORM IMS-STATUSKONTROLL                                           
392600     ADD +1 TO CHKP-ANT                                                   
392700                                                                          
392800     MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                    
392900     MOVE '2304    ' TO POSTSUM-DDNAMN2                                   
393000     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
393100     CALL POSTSUM USING POSTSUM-PARM                                      
393200     .                                                                    
393300                                                                          
393400 IMS-UPPD-RAD SECTION.                                                    
393500     SKIP2                                                                
393600     MOVE '  ' TO GODK-STATUSKODER                                        
393700     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
393800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
393900     PERFORM IMS-STATUSKONTROLL                                           
394000     ADD +1 TO CHKP-ANT                                                   
394100                                                                          
394200     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
394300     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
394400     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
394500     CALL POSTSUM USING POSTSUM-PARM                                      
394600     .                                                                    
394700                                                                          
394800 IMS-REPL-SATB11O SECTION.                                                
394900     SKIP2                                                                
395000     MOVE '  ' TO GODK-STATUSKODER                                        
395100     CALL CBLTDLI USING REPL SATB-O-PCB DLI-IO-AREA-OKONV                 
395200     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
395300     PERFORM IMS-STATUSKONTROLL                                           
395400     ADD +1 TO CHKP-ANT                                                   
395500                                                                          
395600     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
395700     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
395800     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
395900     CALL POSTSUM USING POSTSUM-PARM                                      
396000     .                                                                    
396100                                                                          
396200 IMS-REPL-SATB-OKONV SECTION.                                             
396300     SKIP2                                                                
396400     MOVE '  ' TO GODK-STATUSKODER                                        
396500     CALL CBLTDLI USING REPL SATB-O-PCB DLI-IO-AREA-OKONV                 
396600     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
396700     PERFORM IMS-STATUSKONTROLL                                           
396800     ADD +1 TO CHKP-ANT                                                   
396900                                                                          
397000     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
397100     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
397200     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
397300     CALL POSTSUM USING POSTSUM-PARM                                      
397400     .                                                                    
397500 IMS-UPPD-601 SECTION.                                                    
397600     SKIP2                                                                
397700     MOVE '  ' TO GODK-STATUSKODER                                        
397800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-1                       
397900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
398000     PERFORM IMS-STATUSKONTROLL                                           
398100     ADD +1 TO CHKP-ANT                                                   
398200                                                                          
398300     MOVE 'WDK6  '   TO POSTSUM-FDNAMN                                    
398400     MOVE 'ARTC01  ' TO POSTSUM-DDNAMN2                                   
398500     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
398600     CALL POSTSUM USING POSTSUM-PARM                                      
398700     .                                                                    
398800     EJECT                                                                
398900                                                                          
399000 IMS-UPPD-ART SECTION.                                                    
399100     SKIP2                                                                
399200     MOVE '  ' TO GODK-STATUSKODER                                        
399300     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
399400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
399500     PERFORM IMS-STATUSKONTROLL                                           
399600     ADD +1 TO CHKP-ANT                                                   
399700                                                                          
399800     MOVE 'WDJ1  '   TO POSTSUM-FDNAMN                                    
399900     MOVE 'SATB    ' TO POSTSUM-DDNAMN2                                   
400000     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
400100     CALL POSTSUM USING POSTSUM-PARM                                      
400200     .                                                                    
400300     SKIP3                                                                
400400 IMS-ISRT-UTFIL SECTION.                                                  
400500     SKIP2                                                                
400600     STRING 'WLFILC01    '                                                
400700          DELIMITED BY SIZE INTO SSA1                                     
400800     MOVE '   ' TO GODK-STATUSKODER                                       
400900     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
401000     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
401100     PERFORM IMS-STATUSKONTROLL                                           
401200     ADD +1 TO CHKP-ANT                                                   
401300     .                                                                    
401400     EJECT                                                                
401500 IMS-RESTART SECTION.                                                     
401600                                                                          
401700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
401800     MOVE '  ' TO GODK-STATUSKODER                                        
401900     CALL CBLTDLI USING XRST MSG-PCB                                      
402000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
402100                        CHKP-AREA-LENGTH CHKP-AREA                        
402200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
402300     PERFORM IMS-STATUSKONTROLL                                           
402400     .                                                                    
402500                                                                          
402600 IMS-CHECKPOINT SECTION.                                                  
402700                                                                          
402800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
402900     MOVE '  XD' TO GODK-STATUSKODER                                      
403000     CALL CBLTDLI USING CHKP MSG-PCB                                      
403100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
403200                        CHKP-AREA-LENGTH CHKP-AREA                        
403300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
403400     PERFORM IMS-STATUSKONTROLL                                           
403500                                                                          
403600     IF IMS-EJ-OK                                                         
403700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
403800       DISPLAY FELTEXT                                                    
403900       CALL FELLOG                                                        
404000     END-IF                                                               
404100     .                                                                    
404200                                                                          
404300     EJECT                                                                
404400 IMS-STATUSKONTROLL SECTION.                                              
404500                                                                          
404600     SET STATUS-IX TO 1                                                   
404700     SEARCH GODK-STATUS                                                   
404800       AT END                                                             
404900         CALL FELLOG                                                      
405000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
405100       CONTINUE                                                           
405200     END-SEARCH                                                           
405300     .                                                                    
405400     EJECT                                                                
405500*    -COPY WY2000P1                                                       
405600     EJECT                                                                
405700*    -COPY WY2000P2                                                       
405800     EJECT                                                                
405900*    -COPY WY2000P3                                                       
406000     EJECT                                                                
406100*    -COPY WY2000P9                                                       
