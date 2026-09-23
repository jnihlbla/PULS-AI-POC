000100 PROCESS DYNAM                                                            
000200*COMPOPT DB2BIND=YES                                                      
000300 ID DIVISION.                                                             
000400     SKIP2                                                                
000500 PROGRAM-ID.     W4029500.                                                
000600 AUTHOR.         ANNELIE ENGLUND.                                         
000700 DATE-WRITTEN.   MAJ-91.                                                  
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*    FUNKTION.                                                            
001100*        SKRIVER PROFORMA DOKUMENT FRÅN ANTINGEN PROF-SYSTEMET            
001200*        ELLER FRÅN ORDERSYSTEMET.                                        
001300*        OM IDSYSTEM (PROF) SKRIVER MAN FRÅN PROFORMASYSTEMET.            
001400*        OM IDSYSTEM (IMS ) SKRIVER MAN FRÅN ORDERSYSTEMET.               
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T295X                                             
001800*                                                                         
001900*    UTDATA.                                                              
002000*        TRANSAKTION: W4T295X  FÖR OMSTART                                
002100*        DOKUMENT:    PROFORMA-LISTA                                      
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4029500'.            
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600*                                                                         
003700 01  ERRTEXT.                                                             
003800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
003900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004000 77  KDRC-DISPLAY                PIC Z(3)9.                               
004100*                                                                         
004200 77  INDX                        PIC S9(3)  VALUE +100  COMP SYNC.        
004300 77  TAB-INDX                    PIC S9(9)  VALUE ZERO  COMP SYNC.        
004400 77  SPAR-TAB-INDX               PIC S9(9)  VALUE ZERO  COMP SYNC.        
004500 77  MAX-INDEX                   PIC S9(2)  VALUE +99   COMP SYNC.        
004600 77  RAD-IX                      PIC S9(3)  VALUE +34   COMP-3.           
004700 77  MAX-SIDOR                   PIC S9(5)  VALUE +2400 COMP-3.           
004800 77  MAX-RADER                   PIC S9(3)  VALUE +34   COMP-3.           
004900 77  IDSID-RAKN                  PIC 9(3)   VALUE ZERO.                   
005000 77  SID-RAKN                    PIC 9(3)   VALUE ZERO.                   
005100 77  SUORDV-TOT                  PIC 9(9)V99  VALUE ZERO.                 
005200 77  SUORDV-TOT-LOC              PIC 9(9)V99  VALUE ZERO.                 
005300 77  SUORDV-TOT-LOCPREL          PIC 9(9)V99  VALUE ZERO.                 
005400 77  WS-TOTPRIS                  PIC 9(9)V99  VALUE ZERO.                 
005500 77  WS-VKORDNTO                 PIC 9(6)V9(1) VALUE ZERO.                
005600 77  WS-VLORDNTO                 PIC 9(4)V9(3) VALUE ZERO.                
005700 77  WS-SUFKTBEL                 PIC S9(9)V9(4) COMP-3.                   
005800 77  WS-SUFOBV                   PIC S9(9)V9(2) VALUE ZERO.               
005900 77  WS-LEVVIL-ORT               PIC X(48) VALUE SPACE.                   
006000 77  SPAR-KDVALISO               PIC X(3)  VALUE SPACE.                   
006100 77  WS-MID-IDDISTR              PIC 9(4)  VALUE ZERO.                    
006200                                                                          
006300 77  WS-CAR-KOD                  PIC X(2)    VALUE 'C '.                  
006400 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
006500                                                                          
006600*    --- STYRTECKEN PRINTER                                               
006700 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
006800 01  WS-SKIP-BLANK                 PIC X      VALUE ' '.                  
006900 01  WS-SKIP-ZERO                  PIC X      VALUE '0'.                  
007000 01  WS-SKIP-HYPEN                 PIC X      VALUE '-'.                  
007100                                                                          
007200 77  ALLT-SW                     PIC X.                                   
007300     88  ALLT-OK                             VALUE 'J'.                   
007400     88  ALLT-FEL                            VALUE 'N'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  GODK-MID                            VALUE '4266' '4295'          
007800                                                   '4504' '4269'.         
007900 77  PROFORMA-SW                 PIC X.                                   
008000     88  PROFORMA                            VALUE 'J'.                   
008100                                                                          
008200 77  SW-TAB                      PIC X.                                   
008300     88  TABELL-KLAR                         VALUE 'J'.                   
008400                                                                          
008500*    --- VALID IDDC CODES                                                 
008600*                                                                         
008700*01  -COPY WWDCKONS                                                       
008800*01  -COPY WWDC99                                                         
008900                                                                          
009000*-- TABELL FÖR LAGRING AV FÖRSÄKRANTEXTER                                 
009100                                                                          
009200 01  TABELL.                                                              
009300     03  TABELL-RAD              OCCURS 99 TIMES.                         
009400        05  TABELL-BEFORSKN      PIC X(50)   VALUE SPACE.                 
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16) VALUE 'W930VAL'.               
009700*01 -COPY W930VAL                                                         
009800     EJECT                                                                
009900*-- CTEXT FÖR DISTRIKTTEST                                                
010000                                                                          
010100*01  -COPY WWDIST03                                                       
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER                      PIC X(12)  VALUE 'TEST-IDDISTR'.         
010500 01  TEST-IDDISTR                PIC  S9(5)   COMP-3.                     
010600*    ----DISTR-DEALER-PRICE-----                                          
010700*01  FILLER  -COPY WWDIST79     -RED TEST-IDDISTR.                        
010800*                                                                         
010900                                                                          
011000*-- CTEXT FÖR MOMSBERÄKNING                                               
011100                                                                          
011200*01  -COPY W475CONS     -PRE CONS-                                        
011300     EJECT                                                                
011400                                                                          
011500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012000     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
012100     03  WZ04DAP                 PIC X(8)    VALUE 'WZ04DAP '.            
012200     EJECT                                                                
012300*    --- PARAMETERS TO ABEND                                              
012400                                                                          
012500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012800     EJECT                                                                
012900*    --- AREA FÖR SUBPROGRAM W006PRS1                                     
013000*                                                                         
013100 01  FILLER                      PIC X(16)  VALUE 'W006PRS1'.             
013200                                                                          
013300*01  -COPY W006PRAR                                                       
013400                                                                          
013500 01  WS-PRINTER-PARM.                                                     
013600     03 WS-IDPRT                 PIC X(8)    VALUE SPACE.                 
013700     03 WS-RAD.                                                           
013800       05  WS-FILLER             PIC X(10)   VALUE SPACE.                 
013900       05  WS-LISTRAD            PIC X(132)  VALUE SPACE.                 
014000     03 WS-DUMMY                 PIC X(1)    VALUE SPACE.                 
014100*                                                                         
014200 01  FILLER                      PIC X(08)   VALUE 'SENDAREA'.            
014300 01  SEND-RAD-GRP.                                                        
014400     03  STYRTECKEN-RAD          PIC X(01)   VALUE SPACE.                 
014500     03  WS-MARGINAL-X10         PIC X(10)   VALUE SPACE.                 
014600     03  SEND-RAD                PIC X(132)  VALUE SPACE.                 
014700                                                                          
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'DAP-AREA '.           
015000     SKIP3                                                                
015100 01  -COPY WZ04DAP                                                        
015200     EJECT                                                                
015300*                                                                         
015400*    --- AREOR FÖR MSG-HANTERING                                          
015500*                                                                         
015600 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
015700                                                                          
015800*01  -COPY WMSGAREA                                                       
015900 01  P-TO-P-SW1.                                                          
016000     03  PTOP1-LL                PIC S9(4)   VALUE +92 COMP SYNC.         
016100     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
016200     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
016300     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T295X'.             
016400     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
016500     03  FILLER                  PIC  X(4)   VALUE '4295'.                
016600     03  PTOP1-KDMFSFOR          PIC  X(1).                               
016700*    03  MID -COPY W4I29501                                               
016800     EJECT                                                                
016900     EJECT                                                                
017000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300                                                                          
017400 01  NYCKLAR-TILL-DLI.                                                    
017500                                                                          
017600*----> PROFORMAHUVUD WDE8                                                 
017700                                                                          
017800     03  W-WDE801KY-X.                                                    
017900         05  W-IDDISTR-E8        PIC S9(5) VALUE ZERO COMP-3.             
018000         05  W-IDKUNDNR-E8       PIC S9(7) VALUE ZERO COMP-3.             
018100         05  W-IDKUNDRF-E8       PIC X(10) VALUE SPACE.                   
018200                                                                          
018300*----> PROFORMARAD WDE9                                                   
018400                                                                          
018500     03  W-WDE901KY-MIN-X.                                                
018600         05  W-IDORDER-MIN-E9    PIC S9(7) VALUE ZERO COMP-3.             
018700         05  W-IDARTNR-MIN-E9    PIC S9(9) VALUE ZERO COMP-3.             
018800         05  W-IDLOPNR-MIN-E9    PIC S9(3) VALUE ZERO COMP-3.             
018900                                                                          
019000     03  W-WDE901KY-MAX-X.                                                
019100         05  W-IDORDER-MAX-E9    PIC S9(7) VALUE +9999999                 
019200                                                  COMP-3.                 
019300         05  W-IDARTNR-MAX-E9    PIC S9(9) VALUE +999999999               
019400                                                      COMP-3.             
019500         05  W-IDLOPNR-MAX-E9    PIC S9(3) VALUE +999 COMP-3.             
019600                                                                          
019700*----> ARTIKELREGISTER WDD3                                               
019800                                                                          
019900     03  W-IDARTNR-X.                                                     
020000         05  W-IDARTNR-D3        PIC S9(9) COMP-3.                        
020100                                                                          
020200     03  W-IDSKYLT-X.                                                     
020300         05  W-IDSKYLT-D3        PIC X(3)  VALUE SPACE.                   
020400                                                                          
020500*----> KUNDREGISTER WDB1                                                  
020600                                                                          
020700     03  W-WDB101KY-X.                                                    
020800         05  W-WDB1-IDPARTNR     PIC X(9).                                
020900         05  W-WDB1-IDFTG        PIC 9(2).                                
021000                                                                          
021100*----> KUNDREGISTER WDB2                                                  
021200                                                                          
021300     03  W-IDGMT-X.                                                       
021400         05  W-IDDISTR-WDB2      PIC S9(5) COMP-3.                        
021500         05  W-IDKUNDNR-WDB2     PIC S9(7) COMP-3.                        
021600                                                                          
021700*----> KUNDREGISTER WDB3                                                  
021800                                                                          
021900     03  W-WDB301KY-X.                                                    
022000         05  W-IDDC-WDB3         PIC X(2)  VALUE SPACE.                   
022100         05  W-IDDISTR-WDB3      PIC S9(5) COMP-3.                        
022200         05  W-IDKUNDNR-WDB3     PIC S9(7) COMP-3.                        
022300                                                                          
022400     03  W-WDB301KY-DEF-X.                                                
022500         05  W-IDDC-WDB3-DEF     PIC X(2)  VALUE SPACE.                   
022600         05  W-IDDISTR-WDB3-DEF  PIC S9(5) COMP-3.                        
022700         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
022800                                                                          
022900*----> FÖRSÄKRANTEXT.                                                     
023000                                                                          
023100     03  W-KDFORSKN-4731-X.                                               
023200         05  W-4731-IDHTYP       PIC X(4)  VALUE '4731'.                  
023300         05  W-4731-KDFORSKN     PIC S9(3) VALUE ZERO COMP-3.             
023400         05  FILLER              PIC X(24) VALUE LOW-VALUE.               
023500                                                                          
023600*----> FRAKTTEXT.                                                         
023700                                                                          
023800     03  W-4732-IDHTYP-X.                                                 
023900         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
024000         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
024100         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
024200                                                                          
024300*----> LEVERANSVILLKORSTEXT                                               
024400                                                                          
024500     03  W-WDGX4735-X.                                                    
024600         05  W-IDHTYP            PIC  X(4)  VALUE '4735'.                 
024700         05  W-KDLEVVIL          PIC  S9(3) VALUE ZERO COMP-3.            
024800         05  W-LOW-VALUE         PIC  X(24) VALUE LOW-VALUE.              
024900                                                                          
025000     03  W-KDSEGKEY-X.                                                    
025100         05  W-KDSEGKEY          PIC  X    VALUE '1'.                     
025200                                                                          
025300     03  W-WDGXKEY-9301-X.                                                
025400         05  FILLER              PIC X(4)  VALUE '9301'.                  
025500         05  FILLER              PIC X(26) VALUE LOW-VALUE.               
025600     EJECT                                                                
025700*----> ORDERHUVUD WDQ2                                                    
025800                                                                          
025900     03  W-IDORDER-X.                                                     
026000         05  W-IDORDER-Q2        PIC S9(7) VALUE ZERO COMP-3.             
026100                                                                          
026200     03  W-IDGMTREF-X.                                                    
026300         05  W-IDDISTR-Q2        PIC S9(5) VALUE ZERO COMP-3.             
026400         05  W-IDKUNDNR-Q2       PIC S9(7) VALUE ZERO COMP-3.             
026500         05  W-IDKUNDRF-Q2       PIC X(10) VALUE SPACE.                   
026600                                                                          
026700     03  W-IDDC-X.                                                        
026800         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
026900                                                                          
027000*----> ORDERRAD WDQ4                                                      
027100                                                                          
027200     03  W-WDQ401KY-MIN-X.                                                
027300         05  W-IDORDER-MIN-Q4    PIC S9(7) VALUE ZERO COMP-3.             
027400         05  W-IDDC-MIN-Q4       PIC  X(2) VALUE SPACE.                   
027500         05  W-ADLAGOMR-MIN-Q4   PIC S9(3) VALUE ZERO COMP-3.             
027600         05  W-ADGANG-MIN-Q4     PIC S9(3) VALUE ZERO COMP-3.             
027700         05  W-ADPLATS-MIN-Q4    PIC S9(5) VALUE ZERO COMP-3.             
027800         05  W-IDARTNR-MIN-Q4    PIC S9(9) VALUE ZERO COMP-3.             
027900         05  W-IDLOPNR-MIN-Q4    PIC S9(3) VALUE ZERO COMP-3.             
028000                                                                          
028100     03  W-WDQ401KY-MAX-X.                                                
028200         05  W-IDORDER-MAX-Q4    PIC S9(7) VALUE +9999999                 
028300                                                 COMP-3.                  
028400         05  W-IDDC-MAX-Q4       PIC  X(2) VALUE '11'.                    
028500         05  W-ADLAGOMR-MAX-Q4   PIC S9(3) VALUE +999 COMP-3.             
028600         05  W-ADGANG-MAX-Q4     PIC S9(3) VALUE +999 COMP-3.             
028700         05  W-ADPLATS-MAX-Q4    PIC S9(5) VALUE +99999                   
028800                                                 COMP-3.                  
028900         05  W-IDARTNR-MAX-Q4    PIC S9(9) VALUE +999999999               
029000                                                 COMP-3.                  
029100         05  W-IDLOPNR-MAX-Q4    PIC S9(3) VALUE +999 COMP-3.             
029200                                                                          
029300                                                                          
029400     EJECT                                                                
029500*    --- STATUS-KOD FRÅN IMS                                              
029600                                                                          
029700 01  STATUS-WS                   PIC  X(02).                              
029800     88  SEGMENT-FINNS                       VALUE '  '.                  
029900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030000     88  BASEN-SLUT                          VALUE 'GB'.                  
030100     SKIP2                                                                
030200 01  GODK-STATUSKODER.                                                    
030300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030400                                                                          
030500 01  SSA1                        PIC X(128).                              
030600 01  SSA2                        PIC X(128).                              
030700 01  SSA3                        PIC X(128).                              
030800     EJECT                                                                
030900****************************************                                  
031000*  PRINTRADER FÖR PROFORMA-DOKUMENTET  *                                  
031100****************************************                                  
031200                                                                          
031300*----> FÖRSTA SIDAN                                                       
031400                                                                          
031500 01  S1-RAD1.                                                             
031600     03   FILLER               PIC X(11) VALUE 'BUYER    : '.             
031700     03   S1-RAD1-IDDISTR      PIC Z(4).                                  
031800     03   FILLER               PIC X(1)  VALUE SPACE.                     
031900     03   S1-RAD1-IDKUNDNR     PIC Z(5)9.                                 
032000     03   FILLER               PIC X(25) VALUE SPACE.                     
032100     03   FILLER               PIC X(17) VALUE                            
032200                                         'Q U O T A T I O N'.             
032300     03   FILLER               PIC X(10) VALUE SPACE.                     
032400     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
032500     03   S1-RAD1-DATUM        PIC X(6).                                  
032600     03   FILLER               PIC X(5)  VALUE SPACE.                     
032700     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
032800     03   S1-RAD1-IDSID        PIC ZZ9.                                   
032900                                                                          
033000 01  S1-RAD2.                                                             
033100     03   S1-RAD2-KOD          PIC X(2).                                  
033200     03   FILLER               PIC X(9) VALUE SPACE.                      
033300     03   S1-RAD2-BENAMN       PIC X(35).                                 
033400                                                                          
033500 01  S1-RAD3.                                                             
033600     03   FILLER               PIC X(11) VALUE 'CONSIGNEE: '.             
033700     03   S1-RAD3-BENAMN       PIC X(35).                                 
033800                                                                          
033900 01  S1-RAD4.                                                             
034000     03   FILLER               PIC X(11) VALUE SPACE.                     
034100     03   FILLER               PIC X(15) VALUE 'PROFORMA NO. : '.         
034200     03   S1-RAD4-TIUPPDAT     PIC X(6).                                  
034300     03   FILLER               PIC X(1)  VALUE '-'.                       
034400     03   S1-RAD4-TIUPPTID     PIC X(8).                                  
034500     03   FILLER               PIC X(9)  VALUE SPACE.                     
034600     03   FILLER               PIC X(11) VALUE 'VOLVO REF: '.             
034700     03   S1-RAD4-IDORDNR      PIC Z(2)9(5).                              
034800                                                                          
034900 01  S1-RAD5.                                                             
035000     03   FILLER               PIC X(11) VALUE SPACE.                     
035100     03   FILLER               PIC X(15) VALUE 'CUSTOMER REF.: '.         
035200     03   S1-RAD5-BEKUNDRF     PIC X(15).                                 
035300                                                                          
035400 01  S1-RAD6.                                                             
035500     03   FILLER               PIC X(11) VALUE SPACE.                     
035600     03   FILLER               PIC X(15) VALUE '   TRANSPORT : '.         
035700     03   S1-RAD6-BEFRAKT      PIC X(35).                                 
035800                                                                          
035900 01  S1-RAD7.                                                             
036000     03   FILLER               PIC X(11) VALUE SPACE.                     
036100     03   FILLER               PIC X(15) VALUE 'TOTAL VALUE  : '.         
036200     03   S1-RAD7-LEVVIL-ORT   PIC X(48).                                 
036300     03   FILLER               PIC X(1)  VALUE SPACE.                     
036400     03   S1-RAD7-KDVALISO     PIC X(3).                                  
036500     03   FILLER               PIC X(1)  VALUE SPACE.                     
036600     03   S1-RAD7-PRTOTAL      PIC Z(8)9.99 BLANK WHEN ZERO.              
036700                                                                          
036800 01  S1-RAD8.                                                             
036900     03   FILLER               PIC X(11) VALUE SPACE.                     
037000     03   FILLER               PIC X(19)                                  
037100                                   VALUE 'NET WEIGHT       : '.           
037200     03   S1-RAD8-VKORDNTO     PIC Z(5)9.9.                               
037300     03   FILLER               PIC X(4)  VALUE ' KGS'.                    
037400                                                                          
037500 01  S1-RAD9.                                                             
037600     03   FILLER               PIC X(11) VALUE SPACE.                     
037700     03   FILLER               PIC X(19)                                  
037800                                   VALUE 'EST. GROSS WEIGHT: '.           
037900     03   S1-RAD9-VKORDBTO     PIC Z(5)9.9.                               
038000     03   FILLER               PIC X(4)  VALUE ' KGS'.                    
038100                                                                          
038200 01  S1-RAD10.                                                            
038300     03   FILLER               PIC X(11) VALUE SPACE.                     
038400     03   FILLER               PIC X(19)                                  
038500                                   VALUE 'EST. VOLUME      : '.           
038600     03   S1-RAD10-VLORDBTO    PIC Z(3)9.999.                             
038700     03   FILLER               PIC X(4)  VALUE ' CBM'.                    
038800                                                                          
038900 01  S1-RAD11.                                                            
039000     03   FILLER               PIC X(21)                                  
039100                                   VALUE 'BANKING INFORMATION: '.         
039200     03   S1-RAD11-TEBANK      PIC X(72).                                 
039300                                                                          
039400 01  S1-RAD12.                                                            
039500     03   FILLER               PIC X(21) VALUE SPACE.                     
039600     03   S1-RAD12-TEBANK      PIC X(72).                                 
039700                                                                          
039800 01  S1-RAD13.                                                            
039900     03   FILLER               PIC X(21)                                  
040000                                   VALUE '            ACCOUNT: '.         
040100     03   S1-RAD13-TEBANKTO    PIC X(20).                                 
040200     EJECT                                                                
040300*----> ANDRA SIDAN.                                                       
040400                                                                          
040500 01  S2-RAD1.                                                             
040600     03   FILLER               PIC X(33) VALUE SPACE.                     
040700     03   FILLER               PIC X(20)                                  
040800                                VALUE 'TERMS AND CONDITIONS'.             
040900     03   FILLER               PIC X(21) VALUE SPACE.                     
041000     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
041100     03   S2-RAD1-DATUM        PIC X(6).                                  
041200     03   FILLER               PIC X(5)  VALUE SPACE.                     
041300     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
041400     03   S2-RAD1-IDSID        PIC ZZ9.                                   
041500                                                                          
041600 01  S2-RAD2.                                                             
041700     03   FILLER               PIC X(64) VALUE SPACE.                     
041800     03   FILLER               PIC X(13) VALUE 'PROFORMA NO. '.           
041900     03   S2-RAD2-TIUPPDAT     PIC X(6).                                  
042000     03   FILLER               PIC X(1)  VALUE '-'.                       
042100     03   S2-RAD2-TIUPPTID     PIC X(8).                                  
042200                                                                          
042300 01  S2-RAD3.                                                             
042400     03   FILLER               PIC X(24)                                  
042500                               VALUE 'PAYMENT:        '.                  
042600     03   S2-RAD3-TEBETVIL     PIC X(72).                                 
042700                                                                          
042800 01  S2-RAD4.                                                             
042900     03   FILLER               PIC X(24) VALUE SPACE.                     
043000     03   S2-RAD4-VILLKOR      PIC X(72).                                 
043100                                                                          
043200 01  S2-RAD5.                                                             
043300     03   FILLER               PIC X(15)                                  
043400                                 VALUE 'OFFER VALIDITY:'.                 
043500     03   S2-RAD5-TIGILTIG     PIC Z9(6).                                 
043600     03   FILLER               PIC X(2) VALUE SPACE.                      
043700     03   S2-RAD5-TEGILTIG     PIC X(72).                                 
043800                                                                          
043900 01  S2-RAD6.                                                             
044000     03   FILLER               PIC X(24)                                  
044100                               VALUE 'OTHER TERMS:            '.          
044200     03   S2-RAD6-TEFRITT      PIC X(72).                                 
044300                                                                          
044400 01  S2-RAD7.                                                             
044500     03   FILLER               PIC X(24)                                  
044600                               VALUE 'TIME OF DELIVERY:       '.          
044700     03   S2-RAD7-TELEVVIL     PIC X(72).                                 
044800                                                                          
044900 01  S2-RAD8.                                                             
045000     03   FILLER               PIC X(24)                                  
045100                               VALUE 'PACKING:                '.          
045200     03   S2-RAD8-TEPACK       PIC X(72).                                 
045300                                                                          
045400     EJECT                                                                
045500*----> TREDJE SIDAN.                                                      
045600                                                                          
045700 01  S3-RAD1.                                                             
045800     03   FILLER               PIC X(74) VALUE SPACE.                     
045900     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
046000     03   S3-RAD1-DATUM        PIC X(6).                                  
046100     03   FILLER               PIC X(5)  VALUE SPACE.                     
046200     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
046300     03   S3-RAD1-IDSID        PIC ZZ9.                                   
046400                                                                          
046500 01  S3-RAD2.                                                             
046600     03   FILLER               PIC X(70) VALUE SPACE.                     
046700     03   FILLER               PIC X(13) VALUE 'PROFORMA NO. '.           
046800     03   S3-RAD2-TIUPPDAT     PIC X(6).                                  
046900     03   FILLER               PIC X(1)  VALUE '-'.                       
047000     03   S3-RAD2-TIUPPTID     PIC X(8).                                  
047100                                                                          
047200 01  S3-RAD3.                                                             
047300     03   FILLER               PIC X(16) VALUE 'ITEM            '.        
047400     03   FILLER               PIC X(9)  VALUE 'PART NO  '.               
047500     03   FILLER               PIC X(21)                                  
047600                               VALUE 'PART NAME            '.             
047700     03   FILLER               PIC X(8)  VALUE 'QTY REQ '.                
047800     03   FILLER               PIC X(14) VALUE 'UNIT PRICE    '.          
047900     03   FILLER               PIC X(10) VALUE 'TOT PRICE '.              
048000     03   FILLER               PIC X(5)  VALUE 'ORG  '.                   
048100     03   FILLER               PIC X(09) VALUE 'NET WGT  '.               
048200     03   FILLER               PIC X(7)  VALUE 'STAT NO'.                 
048300                                                                          
048400 01  S3-RAD4.                                                             
048500     03   FILLER               PIC X(106) VALUE SPACE.                    
048600                                                                          
048700 01  S3-RAD5.                                                             
048800     03   S3-RAD5-BERADREF     PIC X(10).                                 
048900     03   FILLER               PIC X(2)  VALUE SPACE.                     
049000     03   S3-RAD5-IDARTNR      PIC Z(8)9.                                 
049100     03   FILLER               PIC X     VALUE '-'.                       
049200     03   S3-RAD5-REKSIFFR     PIC 9.                                     
049300     03   FILLER               PIC X(2)  VALUE SPACE.                     
049400     03   S3-RAD5-BEART        PIC X(20).                                 
049500     03   FILLER               PIC X(1)  VALUE SPACE.                     
049600     03   S3-RAD5-KVBEART-Q    PIC Z(6)9.                                 
049700     03   FILLER               PIC X(1)  VALUE SPACE.                     
049800     03   S3-RAD5-PRARTNTO     PIC Z(6)9.99.                              
049900     03   FILLER               PIC X(1)  VALUE SPACE.                     
050000     03   S3-RAD5-TOTPRIS      PIC Z(8)9.99.                              
050100     03   FILLER               PIC X(1)  VALUE SPACE.                     
050200     03   S3-RAD5-KDARTURS     PIC X(2).                                  
050300     03   FILLER               PIC X(3)  VALUE SPACE.                     
050400     03   S3-RAD5-KDURSVKT     PIC Z(7).                                  
050500     03   FILLER               PIC X(2)  VALUE SPACE.                     
050600     03   S3-RAD5-IDSTATNR.                                               
050700        05 S3-RAD5-IDSTATNR-A PIC Z9(3)B9(4) BLANK WHEN ZERO.             
050800        05 S3-RAD5-IDSTATNR-C REDEFINES S3-RAD5-IDSTATNR-A                
050900                              PIC Z9(4)B9(3) BLANK WHEN ZERO.             
051000     EJECT                                                                
051100     EJECT                                                                
051200*----> FJÄRDE SIDAN (ENDAST PROFORMA).                                    
051300                                                                          
051400 01  S4-RAD1.                                                             
051500     03   FILLER               PIC X(74) VALUE SPACE.                     
051600     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
051700     03   S4-RAD1-DATUM        PIC X(6).                                  
051800     03   FILLER               PIC X(5)  VALUE SPACE.                     
051900     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
052000     03   S4-RAD1-IDSID        PIC ZZ9.                                   
052100                                                                          
052200 01  S4-RAD2.                                                             
052300     03   FILLER               PIC X(70) VALUE SPACE.                     
052400     03   FILLER               PIC X(13) VALUE 'PROFORMA NO. '.           
052500     03   S4-RAD2-TIUPPDAT     PIC X(6).                                  
052600     03   FILLER               PIC X(1)  VALUE '-'.                       
052700     03   S4-RAD2-TIUPPTID     PIC X(8).                                  
052800                                                                          
052900 01  S4-RAD3.                                                             
053000     03   FILLER                  PIC X(11) VALUE 'GOODS VALUE'.          
053100     03   FILLER                  PIC X(54) VALUE SPACE.                  
053200     03   S4-RAD3-SUORDV          PIC Z(8)9.99.                           
053300                                                                          
053400 01  S4-RAD4.                                                             
053500     03   FILLER               PIC X(8)  VALUE 'DISCOUNT'.                
053600     03   FILLER               PIC X(59) VALUE SPACE.                     
053700     03   S4-RAD4-PRAVDRAG     PIC Z(6)9.99.                              
053800                                                                          
053900 01  S4-RAD5.                                                             
054000     03   FILLER               PIC X(25) VALUE                            
054100                                   'PACKING AND HANDLING COST'.           
054200     03   FILLER               PIC X(40) VALUE SPACE.                     
054300     03   S4-RAD5-PREMBHNT     PIC Z(8)9.99.                              
054400                                                                          
054500 01  S4-RAD6.                                                             
054600     03   FILLER               PIC X(12) VALUE 'TOTAL VALUE'.             
054700     03   FILLER               PIC X(1)  VALUE SPACE.                     
054800     03   S4-RAD6-BELEVVIL     PIC X(39).                                 
054900     03   FILLER               PIC X(13) VALUE SPACE.                     
055000     03   S4-RAD6-SUFOBV       PIC Z(8)9.99.                              
055100                                                                          
055200 01  S4-RAD7.                                                             
055300     03   FILLER               PIC X(20) VALUE                            
055400                                         'EST FREIGHT COST BY '.          
055500     03   S4-RAD7-BEFRAKT      PIC X(37).                                 
055600     03   FILLER               PIC X(10) VALUE SPACE.                     
055700     03   S4-RAD7-PRFRAKT      PIC Z(6)9.99.                              
055800                                                                          
055900 01  S4-RAD7A.                                                            
056000     03   FILLER               PIC X(20) VALUE                            
056100                                         'LEGALIZATION FEE    '.          
056200     03   FILLER               PIC X(47) VALUE SPACE.                     
056300     03   S4-RAD7A-PRLEGKST    PIC Z(6)9.99.                              
056400                                                                          
056500 01  S4-RAD8.                                                             
056600     03   FILLER               PIC X(17) VALUE                            
056700                                         'INSURANCE PREMIUM'.             
056800     03   FILLER               PIC X(50) VALUE SPACE.                     
056900     03   S4-RAD8-PRFOERS      PIC Z(6)9.99.                              
057000                                                                          
057100 01  S4-RAD9.                                                             
057200     03   FILLER               PIC X(12) VALUE 'TOTAL VALUE '.            
057300     03   FILLER               PIC X(49) VALUE SPACE.                     
057400     03   S4-RAD9-KDVALISO     PIC X(4).                                  
057500     03   S4-RAD9-PRTOTAL      PIC Z(8)9.99.                              
057600                                                                          
057700 01  S4-RAD10.                                                            
057800     03   FILLER               PIC X(9) VALUE 'EQUAL TO '.                
057900     03   S4-RAD10-KDVALISO    PIC X(3).                                  
058000     03   FILLER               PIC X(6) VALUE ' RATE '.                   
058100     03   S4-RAD10-PRKURS      PIC Z(5)9.9(4).                            
058200                                                                          
058300 01  S4-RAD11.                                                            
058400     03   FILLER               PIC X(12) VALUE 'TOTAL VALUE '.            
058500     03   S4-RAD11-LEVVIL-ORT  PIC X(48).                                 
058600     03   FILLER               PIC X(1)  VALUE SPACE.                     
058700     03   S4-RAD11-KDVALISO    PIC X(3).                                  
058800     03   FILLER               PIC X(1)  VALUE SPACE.                     
058900     03   S4-RAD11-TOTVALUE    PIC Z(8)9.99.                              
059000                                                                          
059100 01  S4-RAD12.                                                            
059200     03   S4-RAD12-BEFORSKN   PIC X(50).                                  
059300     EJECT                                                                
059400*----> FEMTE SIDAN (ENDAST PROFORMA).                                     
059500                                                                          
059600 01  S5-RAD1.                                                             
059700     03   FILLER               PIC X(74) VALUE SPACE.                     
059800     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
059900     03   S5-RAD1-DATUM        PIC X(6).                                  
060000     03   FILLER               PIC X(5)  VALUE SPACE.                     
060100     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
060200     03   S5-RAD1-IDSID        PIC ZZ9.                                   
060300                                                                          
060400 01  S5-RAD2.                                                             
060500     03   FILLER               PIC X(70) VALUE SPACE.                     
060600     03   FILLER               PIC X(13) VALUE 'PROFORMA NO. '.           
060700     03   S5-RAD2-TIUPPDAT     PIC X(6).                                  
060800     03   FILLER               PIC X(1)  VALUE '-'.                       
060900     03   S5-RAD2-TIUPPTID     PIC X(8).                                  
061000                                                                          
061100 01  S5-RAD12.                                                            
061200     03   S5-RAD12-BEFORSKN   PIC X(50).                                  
061300     EJECT                                                                
061400*----> SJÄTTE SIDAN.                                                      
061500                                                                          
061600 01  S6-RAD1.                                                             
061700     03   FILLER               PIC X(20) VALUE SPACE.                     
061800     03   FILLER               PIC X(39) VALUE                            
061900               'TOTAL ORDER VALUE PER CENTRAL WAREHOUSE'.                 
062000     03   FILLER               PIC X(15) VALUE SPACE.                     
062100     03   FILLER               PIC X(5)  VALUE 'DATE '.                   
062200     03   S6-RAD1-DATUM        PIC X(6).                                  
062300     03   FILLER               PIC X(5)  VALUE SPACE.                     
062400     03   FILLER               PIC X(5)  VALUE 'PAGE '.                   
062500     03   S6-RAD1-IDSID        PIC ZZ9.                                   
062600                                                                          
062700 01  S6-RAD2.                                                             
062800     03   FILLER               PIC X(70) VALUE SPACE.                     
062900     03   FILLER               PIC X(13) VALUE 'PROFORMA NO. '.           
063000     03   S6-RAD2-TIUPPDAT     PIC X(6).                                  
063100     03   FILLER               PIC X(1)  VALUE '-'.                       
063200     03   S6-RAD2-TIUPPTID     PIC X(8).                                  
063300                                                                          
063400 01  S6-RAD3.                                                             
063500     03   FILLER                  PIC X(4)  VALUE '    '.                 
063600     03   S6-RAD3-SUORDV       PIC Z(8)9.99.                              
063700     03   S6-RAD3-SUORDV-LOC      PIC Z(8)9.99.                           
063800     03   S6-RAD3-SUORDV-LOCPREL  PIC Z(8)9.99.                           
063900     03   FILLER                  PIC X(4)  VALUE ' SEK'.                 
064000                                                                          
064100                                                                          
064200     EJECT                                                                
064300*    --- IMS FUNKTIONSKODER                                               
064400*01  -COPY W0003                                                          
064500     EJECT                                                                
064600*    ---  DLI INPUT-OUTPUT AREA                                           
064700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-E8'.        
064800                                                                          
064900 01  DLI-IO-AREA-E8.                                                      
065000     03  WLPROC01.                                                        
065100*        05  -COPY WDE801                                                 
065200     EJECT                                                                
065300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-E9'.        
065400                                                                          
065500 01  DLI-IO-AREA-E9.                                                      
065600     03  WLPROD01.                                                        
065700*        05  -COPY WDE901                                                 
065800     EJECT                                                                
065900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-D3'.        
066000                                                                          
066100 01  DLI-IO-AREA-D3.                                                      
066200     03  WLBENA11.                                                        
066300*        05  -COPY WDD311                                                 
066400     EJECT                                                                
066500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-D6'.        
066600                                                                          
066700 01  DLI-IO-AREA-D6.                                                      
066800     03  WLARTC11.                                                        
066900*        05  -COPY WDK611                                                 
067000     EJECT                                                                
067100 01  FILLER                      PIC X(16) VALUE 'WDB1-AREA'.             
067200                                                                          
067300 01  WDB1-AREA.                                                           
067400*    03  -COPY WDB101                                                     
067500     EJECT                                                                
067600 01  FILLER                      PIC X(16) VALUE 'WDB201-AREA'.           
067700                                                                          
067800 01  DLI-IO-AREA-GMTA.                                                    
067900     03  WLGMTA01.                                                        
068000*        05  -COPY WDB201   -PRE GMTA-                                    
068100     EJECT                                                                
068200                                                                          
068300 01  FILLER                      PIC X(16) VALUE 'WDB301-AREA'.           
068400                                                                          
068500 01  DLI-IO-AREA-GMTB.                                                    
068600     03  WLGMTB01.                                                        
068700*        05  -COPY WDB301   -PRE GMTB-                                    
068800     EJECT                                                                
068900                                                                          
069000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-Q2'.        
069100                                                                          
069200 01  DLI-IO-AREA-Q2.                                                      
069300     03 WLORQI01.                                                         
069400*        05  -COPY WDQ201                                                 
069500     EJECT                                                                
069600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-Q221'.        
069700                                                                          
069800 01  DLI-IO-AREA-Q221.                                                    
069900     03 WLORQI21.                                                         
070000*        05  -COPY WDQ221                                                 
070100     EJECT                                                                
070200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-Q4'.        
070300                                                                          
070400 01  DLI-IO-AREA-Q4.                                                      
070500     03  WLORQF01.                                                        
070600*        05  -COPY WDQ401                                                 
070700     EJECT                                                                
070800                                                                          
070900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AR-4732'.        
071000 01  DLI-IO-AREA-473211.                                                  
071100     03  WL473211.                                                        
071200*        05 -COPY WDGX4732                                                
071300     EJECT                                                                
071400                                                                          
071500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AR-4735'.        
071600 01  DLI-IO-AREA-473511.                                                  
071700     03  WL473511.                                                        
071800*        05 -COPY WDGX4735                                                
071900     EJECT                                                                
072000                                                                          
072100 01  FILLER                      PIC X(16) VALUE 'IO-AREA-473A'.          
072200 01  DLI-IO-AREA-473A.                                                    
072300     03  WLXXDB01  -COPY WDGX473A                                         
072400     EJECT                                                                
072500                                                                          
072600 01  FILLER                      PIC X(16) VALUE 'IO-AREA-4731'.          
072700 01  DLI-IO-AREA-4731.                                                    
072800     03  WLXXDB11  -COPY WDGX4731                                         
072900     EJECT                                                                
073000 LINKAGE SECTION.                                                         
073100                                                                          
073200*01  -COPY W0009      -PRE MSG-                                           
073300*01  -COPY W0009      -PRE ALT2-                                          
073400     EJECT                                                                
073500*01  -COPY W0008      -PRE PROC-                                          
073600     05  FILLER                  PIC X.                                   
073700                                                                          
073800*01  -COPY W0008      -PRE PROD-                                          
073900     05  FILLER                  PIC X.                                   
074000     EJECT                                                                
074100*01  -COPY W0008      -PRE BENA-                                          
074200     05  FILLER                  PIC X.                                   
074300                                                                          
074400*01  -COPY W0008      -PRE GMTA-                                          
074500     05  FILLER                  PIC X.                                   
074600     EJECT                                                                
074700*01  -COPY W0008      -PRE GMTB-                                          
074800     05  FILLER                  PIC X.                                   
074900     EJECT                                                                
075000*01  -COPY W0008      -PRE ARTC-                                          
075100     05  FILLER                  PIC X.                                   
075200                                                                          
075300*01  -COPY W0008      -PRE 4732-                                          
075400     05  FILLER                  PIC X.                                   
075500     EJECT                                                                
075600*01  -COPY W0008      -PRE 4735-                                          
075700     05  FILLER                  PIC X.                                   
075800                                                                          
075900*01  -COPY W0008      -PRE ORQISEQ-                                       
076000     05  FILLER                  PIC X.                                   
076100     EJECT                                                                
076200*01  -COPY W0008      -PRE ORQF-                                          
076300     05  FILLER                  PIC X.                                   
076400                                                                          
076500*01  -COPY W0008      -PRE WDB1-                                          
076600     05  FILLER                  PIC X.                                   
076700     EJECT                                                                
076800*01  -COPY W0008      -PRE XXDB-                                          
076900     05  FILLER                  PIC X.                                   
077000     EJECT                                                                
077100*01  -COPY W0009      -PRE OUT0-                                          
077200*01  -COPY W0009      -PRE OUT01-                                         
077300*01  -COPY W0009      -PRE OUT02-                                         
077400*01  -COPY W0009      -PRE OUT03-                                         
077500*01  -COPY W0009      -PRE OUT04-                                         
077600*01  -COPY W0009      -PRE OUT05-                                         
077700*01  -COPY W0009      -PRE OUT06-                                         
077800*01  -COPY W0009      -PRE OUT07-                                         
077900*01  -COPY W0009      -PRE OUT08-                                         
078000*01  -COPY W0009      -PRE OUT09-                                         
078100*01  -COPY W0009      -PRE OUT10-                                         
078200*01  -COPY W0009      -PRE OUT11-                                         
078300*01  -COPY W0009      -PRE OUT12-                                         
078400*01  -COPY W0009      -PRE OUT13-                                         
078500*01  -COPY W0009      -PRE OUT14-                                         
078600*01  -COPY W0009      -PRE OUT15-                                         
078700*01  -COPY W0009      -PRE DISTRDOC-                                      
078800     EJECT                                                                
078900 PROCEDURE DIVISION  USING MSG-PCB                                        
079000                           ALT2-PCB                                       
079100                           OUT0-PCB                                       
079200                           OUT01-PCB                                      
079300                           OUT02-PCB                                      
079400                           OUT03-PCB                                      
079500                           OUT04-PCB                                      
079600                           OUT05-PCB                                      
079700                           OUT06-PCB                                      
079800                           OUT07-PCB                                      
079900                           OUT08-PCB                                      
080000                           OUT09-PCB                                      
080100                           OUT10-PCB                                      
080200                           OUT11-PCB                                      
080300                           OUT12-PCB                                      
080400                           OUT13-PCB                                      
080500                           OUT14-PCB                                      
080600                           OUT15-PCB                                      
080700                       DISTRDOC-PCB                                       
080800                           PROC-PCB                                       
080900                           PROD-PCB                                       
081000                           BENA-PCB                                       
081100                           GMTA-PCB                                       
081200                           GMTB-PCB                                       
081300                           ARTC-PCB                                       
081400                           4732-PCB                                       
081500                           4735-PCB                                       
081600                        ORQISEQ-PCB                                       
081700                           ORQF-PCB                                       
081800                           WDB1-PCB                                       
081900                           XXDB-PCB.                                      
082000 MAIN SECTION.                                                            
082100                                                                          
082200     ENTRY 'DLITCBL' USING MSG-PCB                                        
082300                           ALT2-PCB                                       
082400                           OUT0-PCB                                       
082500                           OUT01-PCB                                      
082600                           OUT02-PCB                                      
082700                           OUT03-PCB                                      
082800                           OUT04-PCB                                      
082900                           OUT05-PCB                                      
083000                           OUT06-PCB                                      
083100                           OUT07-PCB                                      
083200                           OUT08-PCB                                      
083300                           OUT09-PCB                                      
083400                           OUT10-PCB                                      
083500                           OUT11-PCB                                      
083600                           OUT12-PCB                                      
083700                           OUT13-PCB                                      
083800                           OUT14-PCB                                      
083900                           OUT15-PCB                                      
084000                       DISTRDOC-PCB                                       
084100                           PROC-PCB                                       
084200                           PROD-PCB                                       
084300                           BENA-PCB                                       
084400                           GMTA-PCB                                       
084500                           GMTB-PCB                                       
084600                           ARTC-PCB                                       
084700                           4732-PCB                                       
084800                           4735-PCB                                       
084900                        ORQISEQ-PCB                                       
085000                           ORQF-PCB                                       
085100                           WDB1-PCB                                       
085200                           XXDB-PCB.                                      
085300     EJECT                                                                
085400     PERFORM IMS-GU-MSG                                                   
085500     IF SEGMENT-FINNS                                                     
085600                                                                          
085700       PERFORM A-INIT                                                     
085800       IF ALLT-OK                                                         
085900         IF PROFORMA                                                      
086000           PERFORM B-FRAN-PROFSYSTEMET                                    
086100         ELSE                                                             
086200           PERFORM C-FRAN-ORDERSYSTEMET                                   
086300         END-IF                                                           
086400       END-IF                                                             
086500       PERFORM Z-FINIT                                                    
086600     END-IF                                                               
086700     MOVE ZERO TO RETURN-CODE                                             
086800     GOBACK                                                               
086900     .                                                                    
087000     EJECT                                                                
087100                                                                          
087200 A-INIT SECTION.                                                          
087300                                                                          
087400     MOVE JA  TO ALLT-SW                                                  
087500     ACCEPT DAGENS-DATUM FROM DATE                                        
087600                                                                          
087700     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
087800     IF NOT GODK-MID                                                      
087900       MOVE NEJ TO ALLT-SW                                                
088000     END-IF                                                               
088100                                                                          
088200                                                                          
088300     IF ALLT-OK                                                           
088400       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I29501                   
088500       MOVE MSG-KDMFSFOR-1              TO PTOP1-KDMFSFOR                 
088600     END-IF                                                               
088700                                                                          
088800     IF MID-IDSYSTEM = 'PROF'                                             
088900       MOVE JA TO PROFORMA-SW                                             
089000     ELSE                                                                 
089100       MOVE NEJ TO PROFORMA-SW                                            
089200     END-IF                                                               
089300     MOVE MID-IDPRT  TO WS-IDPRT                                          
089400                                                                          
089500     MOVE 'W40295  ' TO PRT-PFDEF-OVR                                     
089600                                                                          
089700     PERFORM S02-OPEN-PRINTER                                             
089800                                                                          
089900     .                                                                    
090000     EJECT                                                                
090100                                                                          
090200 B-FRAN-PROFSYSTEMET SECTION.                                             
090300                                                                          
090400     MOVE SPACE            TO DAP-IDOUTREC                                
090500                                                                          
090600     MOVE 'PROFORMA'       TO DAP-IDOUTTYPE                               
090700     MOVE MID-IDPRT        TO DAP-IDOUTREC(1:3)                           
090800                                                                          
090900     MOVE MID-IDDISTR      TO WS-MID-IDDISTR                              
091000     MOVE WS-MID-IDDISTR   TO DAP-IDOUTREC(4:4)                           
091100                                                                          
091200     MOVE WC-CDC-SE        TO DAP-IDOUTREC(8:2)                           
091300     MOVE MID-IDKUNDRF     TO DAP-IDLIST                                  
091400                                                                          
091500     PERFORM S04-DAP-OPEN                                                 
091600                                                                          
091700     MOVE MID-IDDISTR  TO W-IDDISTR-E8                                    
091800                          W-IDDISTR-WDB2                                  
091900                          W-IDDISTR-WDB3                                  
092000                          W-IDDISTR-WDB3-DEF                              
092100     MOVE MID-IDKUNDNR TO W-IDKUNDNR-E8                                   
092200                          W-IDKUNDNR-WDB2                                 
092300                          W-IDKUNDNR-WDB3                                 
092400     MOVE MID-IDKUNDRF TO W-IDKUNDRF-E8                                   
092500     PERFORM IMS-GHU-PROC-WDE8                                            
092600     MOVE PHUV-IDDISTR   TO  TEST-IDDISTR                                 
092700     IF DIST79-DEALER-PRICE                                               
092800                                                                          
092900       IF PHUV-SUORDV-LOCPREL > 0                                         
093000         COMPUTE PHUV-SUORDV-LOC = PHUV-SUORDV-LOC +                      
093100                                     PHUV-SUORDV-LOCPREL                  
093200         MOVE ZERO TO PHUV-SUORDV-LOCPREL                                 
093300         PERFORM IMS-REPL-PROC-WDE8                                       
093400       END-IF                                                             
093500     END-IF                                                               
093600                                                                          
093700     PERFORM IMS-GU-GMTA-WDB2                                             
093800     IF GMTA-GMT-KDSPRAK < 6                                              
093900        ADD 1 TO GMTA-GMT-KDSPRAK                                         
094000     END-IF                                                               
094100                                                                          
094200     MOVE WC-CDC-SE        TO W-IDDC-WDB3                                 
094300                              W-IDDC-WDB3-DEF                             
094400     PERFORM IMS-GU-GMTB-WDB3                                             
094500     MOVE GMTB-DC-KDFORSKN TO W-4731-KDFORSKN                             
094600                                                                          
094700     IF MID-NYCKEL-GRP = LOW-VALUE                                        
094800       PERFORM BA-SKRIV-SID-1                                             
094900       MOVE GMTA-GMT-IDPARTNR   TO W-WDB1-IDPARTNR                        
095000       MOVE GMTA-GMT-IDFTG      TO W-WDB1-IDFTG                           
095100                                                                          
095200       PERFORM IMS-GET-WDB101                                             
095300       IF SEGMENT-FINNS                                                   
095400         MOVE BET-BEBETVIL TO S2-RAD3-TEBETVIL                            
095500       ELSE                                                               
095600         MOVE SPACE        TO S2-RAD3-TEBETVIL                            
095700       END-IF                                                             
095800       PERFORM BB-SKRIV-SID-2                                             
095900     END-IF                                                               
096000                                                                          
096100     PERFORM BC-SKRIV-SID-3                                               
096200                                                                          
096300     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
096400       MOVE SPACE                TO SPAR-KDVALISO                         
096500       MOVE +1                   TO TAB-IX                                
096600       PERFORM UNTIL TAB-IX > TAB-IX-MAX                                  
096700         IF PHUV-KDVALUTA = TAB-KDVALUTA(TAB-IX)                          
096800           MOVE TAB-KDVALISO(TAB-IX) TO SPAR-KDVALISO                     
096900           MOVE TAB-IX-MAX            TO TAB-IX                           
097000         END-IF                                                           
097100         ADD +1 TO TAB-IX                                                 
097200       END-PERFORM                                                        
097300       PERFORM BD-SKRIV-SISTA-TWA-SIDOR                                   
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800 BA-SKRIV-SID-1 SECTION.                                                  
097900                                                                          
098000     MOVE SPACE             TO SEND-RAD                                   
098100     MOVE PHUV-IDDISTR      TO S1-RAD1-IDDISTR                            
098200     MOVE PHUV-IDKUNDNR     TO S1-RAD1-IDKUNDNR                           
098300     MOVE DAGENS-DATUM      TO S1-RAD1-DATUM                              
098400     MOVE +1                TO S1-RAD1-IDSID                              
098500     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
098600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
098700                                                                          
098800     MOVE SPACE             TO SEND-RAD                                   
098900     MOVE WS-SKIP-HYPEN     TO STYRTECKEN-RAD                             
099000     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
099100                                                                          
099200     MOVE SPACE             TO SEND-RAD                                   
099300     MOVE S1-RAD1           TO SEND-RAD                                   
099400     MOVE WS-SKIP-HYPEN     TO STYRTECKEN-RAD                             
099500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
099600                                                                          
099700     PERFORM BAA-HAEMTA-FOERETAGSKOD                                      
099800                                                                          
099900     MOVE SPACE             TO SEND-RAD                                   
100000     MOVE PHUV-BEBETRAD-1   TO S1-RAD2-BENAMN                             
100100     MOVE S1-RAD2           TO SEND-RAD                                   
100200     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
100300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
100400                                                                          
100500     MOVE SPACE             TO S1-RAD2-KOD                                
100600     MOVE PHUV-BEBETRAD-2   TO S1-RAD2-BENAMN                             
100700     MOVE S1-RAD2           TO SEND-RAD                                   
100800     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
100900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
101000                                                                          
101100     MOVE SPACE             TO SEND-RAD                                   
101200     MOVE PHUV-ADBETRAD-1   TO S1-RAD2-BENAMN                             
101300     MOVE S1-RAD2           TO SEND-RAD                                   
101400     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
101500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
101600                                                                          
101700     MOVE SPACE             TO SEND-RAD                                   
101800     MOVE PHUV-ADBETRAD-2   TO S1-RAD2-BENAMN                             
101900     MOVE S1-RAD2           TO SEND-RAD                                   
102000     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
102100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
102200                                                                          
102300     MOVE SPACE             TO SEND-RAD                                   
102400     MOVE PHUV-ADBETRAD-3   TO S1-RAD2-BENAMN                             
102500     MOVE S1-RAD2           TO SEND-RAD                                   
102600     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
102700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
102800                                                                          
102900     MOVE SPACE             TO SEND-RAD                                   
103000     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
103100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
103200                                                                          
103300     MOVE SPACE             TO SEND-RAD                                   
103400     MOVE PHUV-BEGMT-RAD1   TO S1-RAD3-BENAMN                             
103500     MOVE S1-RAD3           TO SEND-RAD                                   
103600     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
103700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
103800                                                                          
103900     MOVE SPACE             TO SEND-RAD                                   
104000     MOVE PHUV-BEGMT-RAD2   TO S1-RAD2-BENAMN                             
104100     MOVE S1-RAD2           TO SEND-RAD                                   
104200     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
104300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
104400                                                                          
104500     MOVE SPACE             TO SEND-RAD                                   
104600     MOVE PHUV-ADGMT-GATA   TO S1-RAD2-BENAMN                             
104700     MOVE S1-RAD2           TO SEND-RAD                                   
104800     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
104900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
105000                                                                          
105100     MOVE SPACE             TO SEND-RAD                                   
105200     MOVE PHUV-ADGMT-PADR   TO S1-RAD2-BENAMN                             
105300     MOVE S1-RAD2           TO SEND-RAD                                   
105400     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
105500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
105600                                                                          
105700     MOVE SPACE             TO SEND-RAD                                   
105800     MOVE PHUV-ADGMT-LAND   TO S1-RAD2-BENAMN                             
105900     MOVE S1-RAD2           TO SEND-RAD                                   
106000     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
106100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
106200                                                                          
106300     MOVE SPACE             TO SEND-RAD                                   
106400     MOVE WS-SKIP-HYPEN     TO STYRTECKEN-RAD                             
106500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
106600                                                                          
106700     MOVE SPACE             TO SEND-RAD                                   
106800     MOVE MID-TIUPPDAT      TO S1-RAD4-TIUPPDAT                           
106900     MOVE MID-TIUPPTID      TO S1-RAD4-TIUPPTID                           
107000     MOVE PHUV-IDORDNR7     TO S1-RAD4-IDORDNR                            
107100     MOVE S1-RAD4           TO SEND-RAD                                   
107200     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
107300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
107400                                                                          
107500     MOVE SPACE             TO SEND-RAD                                   
107600     MOVE PHUV-BEKUNDRF     TO S1-RAD5-BEKUNDRF                           
107700     MOVE S1-RAD5           TO SEND-RAD                                   
107800     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
107900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
108000                                                                          
108100     MOVE SPACE             TO SEND-RAD                                   
108200     MOVE PHUV-KDFRAKT      TO W-4732-KDFRAKT                             
108300     PERFORM IMS-GU-4732-WDG740                                           
108400     IF SEGMENT-FINNS                                                     
108500       MOVE FRAKT-BEFRAKT(GMTA-GMT-KDSPRAK) TO S1-RAD6-BEFRAKT            
108600     ELSE                                                                 
108700       MOVE SPACE                       TO S1-RAD6-BEFRAKT                
108800     END-IF                                                               
108900     MOVE S1-RAD6           TO SEND-RAD                                   
109000     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
109100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
109200                                                                          
109300     PERFORM BAB-SKRIV-RAD7-LEVVIL-TOTAL                                  
109400                                                                          
109500     MOVE SPACE             TO SEND-RAD                                   
109600     MOVE PHUV-VKORDNTO     TO S1-RAD8-VKORDNTO                           
109700     MOVE S1-RAD8           TO SEND-RAD                                   
109800     MOVE WS-SKIP-HYPEN     TO STYRTECKEN-RAD                             
109900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
110000                                                                          
110100     MOVE SPACE             TO SEND-RAD                                   
110200     MOVE PHUV-VKORDBTO     TO S1-RAD9-VKORDBTO                           
110300     MOVE S1-RAD9           TO SEND-RAD                                   
110400     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
110500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
110600                                                                          
110700     MOVE SPACE             TO SEND-RAD                                   
110800     MOVE PHUV-VLORDBTO     TO S1-RAD10-VLORDBTO                          
110900     MOVE S1-RAD10          TO SEND-RAD                                   
111000     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
111100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
111200                                                                          
111300     MOVE SPACE             TO SEND-RAD                                   
111400     MOVE PHUV-TEBANK(1)    TO S1-RAD11-TEBANK                            
111500     MOVE S1-RAD11          TO SEND-RAD                                   
111600     MOVE WS-SKIP-HYPEN     TO STYRTECKEN-RAD                             
111700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
111800                                                                          
111900     MOVE SPACE             TO SEND-RAD                                   
112000     MOVE PHUV-TEBANK(2)    TO S1-RAD12-TEBANK                            
112100     MOVE S1-RAD12          TO SEND-RAD                                   
112200     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
112300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
112400                                                                          
112500     MOVE SPACE             TO SEND-RAD                                   
112600     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
112700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
112800                                                                          
112900     MOVE SPACE             TO SEND-RAD                                   
113000     MOVE PHUV-TEBANKTO     TO S1-RAD13-TEBANKTO                          
113100     MOVE S1-RAD13          TO SEND-RAD                                   
113200     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
113300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
113400                                                                          
113500     .                                                                    
113600     EJECT                                                                
113700                                                                          
113800 BAA-HAEMTA-FOERETAGSKOD SECTION.                                         
113900                                                                          
114000     MOVE WS-CAR-KOD    TO S1-RAD2-KOD                                    
114100                                                                          
114200     .                                                                    
114300     EJECT                                                                
114400                                                                          
114500 BAB-SKRIV-RAD7-LEVVIL-TOTAL SECTION.                                     
114600                                                                          
114700     MOVE SPACE             TO SEND-RAD                                   
114800     PERFORM S06-REDIG-LEVVILLKOR                                         
114900     MOVE WS-LEVVIL-ORT     TO S1-RAD7-LEVVIL-ORT                         
115000     MOVE 'SEK'             TO S1-RAD7-KDVALISO                           
115100                                                                          
115200     PERFORM S07-BERAKNA-TOTVARDE                                         
115300     MOVE WS-SUFKTBEL       TO S1-RAD7-PRTOTAL                            
115400                                                                          
115500     MOVE S1-RAD7           TO SEND-RAD                                   
115600     MOVE WS-SKIP-BLANK     TO STYRTECKEN-RAD                             
115700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
115800     .                                                                    
115900     EJECT                                                                
116000                                                                          
116100 BB-SKRIV-SID-2 SECTION.                                                  
116200                                                                          
116300     MOVE SPACE           TO SEND-RAD                                     
116400     MOVE WS-PAGESKIP     TO STYRTECKEN-RAD                               
116500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
116600                                                                          
116700     MOVE SPACE             TO SEND-RAD                                   
116800     MOVE DAGENS-DATUM    TO S2-RAD1-DATUM                                
116900     MOVE +2              TO S2-RAD1-IDSID                                
117000     MOVE S2-RAD1         TO SEND-RAD                                     
117100     MOVE WS-SKIP-HYPEN   TO STYRTECKEN-RAD                               
117200     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
117300                                                                          
117400     MOVE SPACE            TO SEND-RAD                                    
117500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
117600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
117700                                                                          
117800     MOVE SPACE             TO SEND-RAD                                   
117900     MOVE MID-TIUPPDAT    TO S2-RAD2-TIUPPDAT                             
118000     MOVE MID-TIUPPTID    TO S2-RAD2-TIUPPTID                             
118100     MOVE S2-RAD2         TO SEND-RAD                                     
118200     MOVE WS-SKIP-BLANK   TO STYRTECKEN-RAD                               
118300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
118400                                                                          
118500     MOVE SPACE            TO SEND-RAD                                    
118600     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
118700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
118800                                                                          
118900     MOVE SPACE            TO SEND-RAD                                    
119000     MOVE S2-RAD3          TO SEND-RAD                                    
119100     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
119200     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
119300                                                                          
119400     MOVE SPACE            TO SEND-RAD                                    
119500     MOVE PHUV-TEBETVIL(1) TO S2-RAD4-VILLKOR                             
119600     MOVE S2-RAD4          TO SEND-RAD                                    
119700     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
119800     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
119900                                                                          
120000     MOVE SPACE            TO SEND-RAD                                    
120100     MOVE PHUV-TEBETVIL(2) TO S2-RAD4-VILLKOR                             
120200     MOVE S2-RAD4          TO SEND-RAD                                    
120300     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
120400     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
120500                                                                          
120600     MOVE SPACE            TO SEND-RAD                                    
120700     MOVE PHUV-TEBETVIL(3) TO S2-RAD4-VILLKOR                             
120800     MOVE S2-RAD4          TO SEND-RAD                                    
120900     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
121000     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
121100                                                                          
121200     MOVE SPACE            TO SEND-RAD                                    
121300     MOVE PHUV-TEBETVIL(4) TO S2-RAD4-VILLKOR                             
121400     MOVE S2-RAD4          TO SEND-RAD                                    
121500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
121600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
121700                                                                          
121800     MOVE SPACE            TO SEND-RAD                                    
121900     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
122000     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
122100                                                                          
122200     MOVE SPACE            TO SEND-RAD                                    
122300     MOVE PHUV-TIGILTIG    TO S2-RAD5-TIGILTIG                            
122400     MOVE PHUV-TEGILTIG(1) TO S2-RAD5-TEGILTIG                            
122500     MOVE S2-RAD5          TO SEND-RAD                                    
122600     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
122700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
122800                                                                          
122900     MOVE SPACE            TO SEND-RAD                                    
123000     MOVE PHUV-TEGILTIG(2) TO S2-RAD4-VILLKOR                             
123100     MOVE S2-RAD4          TO SEND-RAD                                    
123200     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
123300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
123400                                                                          
123500     MOVE SPACE            TO SEND-RAD                                    
123600     MOVE PHUV-TEGILTIG(3) TO S2-RAD4-VILLKOR                             
123700     MOVE S2-RAD4          TO SEND-RAD                                    
123800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
123900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
124000                                                                          
124100     MOVE SPACE            TO SEND-RAD                                    
124200     MOVE PHUV-TEGILTIG(4) TO S2-RAD4-VILLKOR                             
124300     MOVE S2-RAD4          TO SEND-RAD                                    
124400     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
124500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
124600                                                                          
124700     MOVE SPACE            TO SEND-RAD                                    
124800     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
124900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
125000                                                                          
125100     MOVE SPACE            TO SEND-RAD                                    
125200     MOVE PHUV-TEFRITT(1)  TO S2-RAD6-TEFRITT                             
125300     MOVE S2-RAD6          TO SEND-RAD                                    
125400     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
125500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
125600                                                                          
125700     MOVE SPACE            TO SEND-RAD                                    
125800     MOVE PHUV-TEFRITT(2)  TO S2-RAD4-VILLKOR                             
125900     MOVE S2-RAD4          TO SEND-RAD                                    
126000     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
126100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
126200                                                                          
126300     MOVE SPACE            TO SEND-RAD                                    
126400     MOVE PHUV-TEFRITT(3)  TO S2-RAD4-VILLKOR                             
126500     MOVE S2-RAD4          TO SEND-RAD                                    
126600     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
126700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
126800                                                                          
126900     MOVE SPACE            TO SEND-RAD                                    
127000     MOVE PHUV-TEFRITT(4)  TO S2-RAD4-VILLKOR                             
127100     MOVE S2-RAD4          TO SEND-RAD                                    
127200     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
127300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
127400                                                                          
127500     MOVE SPACE            TO SEND-RAD                                    
127600     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
127700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
127800                                                                          
127900     MOVE SPACE            TO SEND-RAD                                    
128000     MOVE PHUV-TELEVVIL(1) TO S2-RAD7-TELEVVIL                            
128100     MOVE S2-RAD7          TO SEND-RAD                                    
128200     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
128300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
128400                                                                          
128500     MOVE SPACE            TO SEND-RAD                                    
128600     MOVE PHUV-TELEVVIL(2) TO S2-RAD4-VILLKOR                             
128700     MOVE S2-RAD4          TO SEND-RAD                                    
128800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
128900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
129000                                                                          
129100     MOVE SPACE            TO SEND-RAD                                    
129200     MOVE PHUV-TELEVVIL(3) TO S2-RAD4-VILLKOR                             
129300     MOVE S2-RAD4          TO SEND-RAD                                    
129400     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
129500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
129600                                                                          
129700     MOVE SPACE            TO SEND-RAD                                    
129800     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
129900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
130000                                                                          
130100     MOVE SPACE            TO SEND-RAD                                    
130200     MOVE PHUV-TEPACK      TO S2-RAD8-TEPACK                              
130300     MOVE S2-RAD8          TO SEND-RAD                                    
130400     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
130500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
130600                                                                          
130700     .                                                                    
130800     EJECT                                                                
130900                                                                          
131000 BC-SKRIV-SID-3 SECTION.                                                  
131100                                                                          
131200     IF MID-NYCKEL-GRP NOT = LOW-VALUE                                    
131300       MOVE MID-IDORDER      TO W-IDORDER-MIN-E9                          
131400                                W-IDORDER-MAX-E9                          
131500       MOVE MID-IDARTNR      TO W-IDARTNR-MIN-E9                          
131600       MOVE MID-IDLOPNR      TO W-IDLOPNR-MIN-E9                          
131700       MOVE MID-IDSID        TO IDSID-RAKN                                
131800       MOVE +1               TO SID-RAKN                                  
131900       MOVE MID-SUORDV       TO SUORDV-TOT                                
132000     ELSE                                                                 
132100       MOVE PHUV-IDORDER     TO W-IDORDER-MIN-E9                          
132200                                W-IDORDER-MAX-E9                          
132300       MOVE +3               TO IDSID-RAKN                                
132400                                  SID-RAKN                                
132500     END-IF                                                               
132600     PERFORM IMS-GHU-PROD-WDE9                                            
132700     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
132800       IF RAD-IX = MAX-RADER                                              
132900         IF SID-RAKN NOT = MAX-SIDOR                                      
133000           PERFORM BCA-SKRIV-RAD1-4                                       
133100           ADD +1 TO IDSID-RAKN                                           
133200         END-IF                                                           
133300         MOVE +0 TO RAD-IX                                                
133400         ADD +1 TO SID-RAKN                                               
133500       ELSE                                                               
133600         PERFORM BCB-SKRIV-RAD-5                                          
133700         ADD +1 TO RAD-IX                                                 
133800         PERFORM IMS-GHN-PROD-WDE9                                        
133900       END-IF                                                             
134000     END-PERFORM                                                          
134100     .                                                                    
134200     EJECT                                                                
134300                                                                          
134400 BCA-SKRIV-RAD1-4 SECTION.                                                
134500                                                                          
134600     MOVE SPACE            TO SEND-RAD                                    
134700     MOVE WS-PAGESKIP      TO STYRTECKEN-RAD                              
134800     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
134900                                                                          
135000     MOVE SPACE            TO SEND-RAD                                    
135100     MOVE DAGENS-DATUM     TO S3-RAD1-DATUM                               
135200     MOVE IDSID-RAKN       TO S3-RAD1-IDSID                               
135300     MOVE S3-RAD1          TO SEND-RAD                                    
135400     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
135500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
135600                                                                          
135700     MOVE SPACE            TO SEND-RAD                                    
135800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
135900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
136000                                                                          
136100     MOVE SPACE            TO SEND-RAD                                    
136200     MOVE MID-TIUPPDAT     TO S3-RAD2-TIUPPDAT                            
136300     MOVE MID-TIUPPTID     TO S3-RAD2-TIUPPTID                            
136400     MOVE S3-RAD2          TO SEND-RAD                                    
136500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
136600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
136700                                                                          
136800     MOVE SPACE            TO SEND-RAD                                    
136900     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
137000     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
137100                                                                          
137200     MOVE SPACE            TO SEND-RAD                                    
137300     MOVE S3-RAD3          TO SEND-RAD                                    
137400     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
137500     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
137600                                                                          
137700     MOVE SPACE            TO SEND-RAD                                    
137800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
137900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
138000                                                                          
138100     MOVE SPACE            TO SEND-RAD                                    
138200     MOVE S3-RAD4          TO SEND-RAD                                    
138300     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
138400     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
138500     .                                                                    
138600     EJECT                                                                
138700                                                                          
138800 BCB-SKRIV-RAD-5 SECTION.                                                 
138900                                                                          
139000     MOVE SPACE            TO SEND-RAD                                    
139100                                                                          
139200     MOVE PRAD-IDARTNR     TO W-IDARTNR-D3                                
139300     MOVE PHUV-IDSKYLT     TO W-IDSKYLT-D3                                
139400     MOVE PRAD-BERADREF    TO S3-RAD5-BERADREF                            
139500     MOVE PRAD-IDARTNR     TO S3-RAD5-IDARTNR                             
139600     MOVE PRAD-REKSIFFR    TO S3-RAD5-REKSIFFR                            
139700     IF PRAD-BEART = SPACE                                                
139800       PERFORM IMS-GU-BENA-WDD3                                           
139900       IF SEGMENT-FINNS                                                   
140000         MOVE TEXT-BEART          TO S3-RAD5-BEART                        
140100       ELSE                                                               
140200         MOVE 'PART DESC MISSING' TO S3-RAD5-BEART                        
140300       END-IF                                                             
140400     ELSE                                                                 
140500       MOVE PRAD-BEART     TO S3-RAD5-BEART                               
140600     END-IF                                                               
140700     MOVE PRAD-KVBEART-Q   TO S3-RAD5-KVBEART-Q                           
140800                                                                          
140900     IF DIST79-DEALER-PRICE                                               
141000       IF PRAD-PRARTNTO-LOCPREL > 0                                       
141100         MOVE PRAD-PRARTNTO-LOCPREL  TO PRAD-PRARTNTO-LOC                 
141200         MOVE ZERO                  TO PRAD-PRARTNTO-LOCPREL              
141300         PERFORM IMS-REPL-PROD-WDE9                                       
141400       END-IF                                                             
141500                                                                          
141600       MOVE PRAD-PRARTNTO-LOC TO S3-RAD5-PRARTNTO                         
141700       COMPUTE WS-TOTPRIS = PRAD-KVBEART-Q * PRAD-PRARTNTO-LOC            
141800     ELSE                                                                 
141900       MOVE PRAD-PRARTNTO    TO S3-RAD5-PRARTNTO                          
142000       COMPUTE WS-TOTPRIS = PRAD-KVBEART-Q * PRAD-PRARTNTO                
142100     END-IF                                                               
142200                                                                          
142300     MOVE WS-TOTPRIS       TO S3-RAD5-TOTPRIS                             
142400     MOVE SPACE            TO S3-RAD5-KDARTURS                            
142500     IF GMTA-GMT-FLFAKURS = JA                                            
142600       IF PHUV-IDDISTR < +7800 OR > +7899                                 
142700         MOVE PRAD-KDARTURS    TO S3-RAD5-KDARTURS                        
142800       END-IF                                                             
142900     END-IF                                                               
143000     IF GMTA-GMT-FLFAKVKT = JA                                            
143100       MOVE PRAD-VKART TO S3-RAD5-KDURSVKT                                
143200     ELSE                                                                 
143300       MOVE ZERO           TO S3-RAD5-KDURSVKT                            
143400     END-IF                                                               
143500     IF GMTA-GMT-KDSTATNR > 0 AND < 7                                     
143600       PERFORM IMS-GU-ARTC-WDK611                                         
143700       IF GMTA-GMT-KDSTATNR = 4                                           
143800         MOVE CLAG-IDSTATNR(GMTA-GMT-KDSTATNR) TO                         
143900              S3-RAD5-IDSTATNR-A                                          
144000       ELSE                                                               
144100         MOVE CLAG-IDSTATNR(GMTA-GMT-KDSTATNR) TO                         
144200              S3-RAD5-IDSTATNR-C                                          
144300       END-IF                                                             
144400     END-IF                                                               
144500     ADD WS-TOTPRIS        TO SUORDV-TOT                                  
144600     MOVE S3-RAD5          TO SEND-RAD                                    
144700     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
144800     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
144900                                                                          
145000     .                                                                    
145100     EJECT                                                                
145200                                                                          
145300 BD-SKRIV-SISTA-TWA-SIDOR SECTION.                                        
145400                                                                          
145500     MOVE SPACE            TO SEND-RAD                                    
145600     MOVE WS-PAGESKIP      TO STYRTECKEN-RAD                              
145700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
145800                                                                          
145900     MOVE SPACE            TO SEND-RAD                                    
146000     MOVE DAGENS-DATUM     TO S4-RAD1-DATUM                               
146100     MOVE IDSID-RAKN       TO S4-RAD1-IDSID                               
146200     MOVE S4-RAD1          TO SEND-RAD                                    
146300     MOVE WS-SKIP-HYPEN    TO STYRTECKEN-RAD                              
146400     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
146500     MOVE +2 TO RAD-IX                                                    
146600                                                                          
146700     MOVE SPACE            TO SEND-RAD                                    
146800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
146900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
147000                                                                          
147100     MOVE SPACE            TO SEND-RAD                                    
147200     MOVE MID-TIUPPDAT     TO S4-RAD2-TIUPPDAT                            
147300     MOVE MID-TIUPPTID     TO S4-RAD2-TIUPPTID                            
147400     MOVE S4-RAD2          TO SEND-RAD                                    
147500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
147600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
147700     ADD +2 TO RAD-IX                                                     
147800                                                                          
147900     MOVE SPACE            TO SEND-RAD                                    
148000     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
148100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
148200                                                                          
148300     MOVE SPACE            TO SEND-RAD                                    
148400     IF DIST79-DEALER-PRICE                                               
148500       MOVE PHUV-SUORDV-LOC  TO S4-RAD3-SUORDV                            
148600     ELSE                                                                 
148700       MOVE PHUV-SUORDV      TO S4-RAD3-SUORDV                            
148800     END-IF                                                               
148900     MOVE S4-RAD3          TO SEND-RAD                                    
149000     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
149100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
149200     ADD +2 TO RAD-IX                                                     
149300                                                                          
149400     IF PHUV-PRAVDRAG > ZERO                                              
149500       MOVE SPACE          TO SEND-RAD                                    
149600       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
149700       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
149800                                                                          
149900       MOVE PHUV-PRAVDRAG  TO S4-RAD4-PRAVDRAG                            
150000       MOVE S4-RAD4        TO SEND-RAD                                    
150100       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
150200       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
150300       ADD +2 TO RAD-IX                                                   
150400     END-IF                                                               
150500                                                                          
150600     IF PHUV-PREMBHNT > ZERO                                              
150700       MOVE SPACE          TO SEND-RAD                                    
150800       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
150900       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
151000                                                                          
151100       MOVE PHUV-PREMBHNT  TO S4-RAD5-PREMBHNT                            
151200       MOVE S4-RAD5        TO SEND-RAD                                    
151300       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
151400       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
151500       ADD +2 TO RAD-IX                                                   
151600                                                                          
151700       PERFORM BDA-SKRIV-RAD6-FCA-TOTAL                                   
151800     END-IF                                                               
151900                                                                          
152000     IF PHUV-PRFRAKT > ZERO                                               
152100       PERFORM BDB-SKRIV-RAD7-PRFRAKT                                     
152200     END-IF                                                               
152300                                                                          
152400     IF PHUV-PRLEGKST > ZERO                                              
152500       MOVE SPACE          TO SEND-RAD                                    
152600       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
152700       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
152800                                                                          
152900       MOVE PHUV-PRLEGKST  TO S4-RAD7A-PRLEGKST                           
153000       MOVE S4-RAD7A       TO SEND-RAD                                    
153100       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
153200       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
153300       ADD +2 TO RAD-IX                                                   
153400     END-IF                                                               
153500                                                                          
153600     IF PHUV-PRFOERS > ZERO                                               
153700       MOVE SPACE          TO SEND-RAD                                    
153800       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
153900       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
154000                                                                          
154100       MOVE PHUV-PRFOERS   TO S4-RAD8-PRFOERS                             
154200       MOVE S4-RAD8        TO SEND-RAD                                    
154300       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
154400       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
154500       ADD +2 TO RAD-IX                                                   
154600     END-IF                                                               
154700                                                                          
154800     IF MID-NYCKEL-GRP = LOW-VALUE                                        
154900        CONTINUE                                                          
155000     ELSE                                                                 
155100        PERFORM S07-BERAKNA-TOTVARDE                                      
155200     END-IF                                                               
155300                                                                          
155400     MOVE SPACE            TO SEND-RAD                                    
155500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
155600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
155700                                                                          
155800     MOVE SPACE            TO SEND-RAD                                    
155900     IF DIST79-DEALER-PRICE                                               
156000       MOVE PHUV-KDVALISO  TO S4-RAD9-KDVALISO                            
156100     ELSE                                                                 
156200       MOVE 'SEK'          TO S4-RAD9-KDVALISO                            
156300     END-IF                                                               
156400     MOVE WS-SUFKTBEL      TO S4-RAD9-PRTOTAL                             
156500     MOVE S4-RAD9          TO SEND-RAD                                    
156600     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
156700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
156800     ADD +2 TO RAD-IX                                                     
156900                                                                          
157000     MOVE SPACE            TO SEND-RAD                                    
157100     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
157200     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
157300                                                                          
157400     MOVE SPACE            TO SEND-RAD                                    
157500     IF DIST79-DEALER-PRICE                                               
157600       MOVE 'SEK'          TO S4-RAD10-KDVALISO                           
157700     ELSE                                                                 
157800       MOVE SPAR-KDVALISO  TO S4-RAD10-KDVALISO                           
157900     END-IF                                                               
158000     MOVE PHUV-PRKURS      TO S4-RAD10-PRKURS                             
158100     MOVE S4-RAD10         TO SEND-RAD                                    
158200     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
158300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
158400     ADD +2 TO RAD-IX                                                     
158500                                                                          
158600     PERFORM BDC-SKRIV-RAD11-TOTAL                                        
158700                                                                          
158800     PERFORM BDD-HAEMTA-FORSKN-TEXT                                       
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200 BDA-SKRIV-RAD6-FCA-TOTAL SECTION.                                        
159300                                                                          
159400     MOVE SPACE            TO SEND-RAD                                    
159500     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
159600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
159700                                                                          
159800     MOVE SPACE            TO SEND-RAD                                    
159900     IF DIST79-DEALER-PRICE                                               
160000       MOVE PHUV-SUORDV-LOC TO WS-SUFOBV                                  
160100       MOVE SPACE            TO S4-RAD6-BELEVVIL                          
160200     ELSE                                                                 
160300       COMPUTE WS-SUFOBV = PHUV-SUORDV +                                  
160400                             PHUV-PREMBHNT                                
160500       MOVE 'FCA GOTHENBURG' TO S4-RAD6-BELEVVIL                          
160600     END-IF                                                               
160700     MOVE WS-SUFOBV        TO S4-RAD6-SUFOBV                              
160800     MOVE S4-RAD6          TO SEND-RAD                                    
160900     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
161000     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
161100     ADD +2 TO RAD-IX                                                     
161200     .                                                                    
161300     EJECT                                                                
161400                                                                          
161500 BDB-SKRIV-RAD7-PRFRAKT SECTION.                                          
161600                                                                          
161700     MOVE SPACE            TO SEND-RAD                                    
161800     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
161900     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
162000                                                                          
162100     MOVE SPACE            TO SEND-RAD                                    
162200     IF DIST79-DEALER-PRICE                                               
162300       MOVE SPACE         TO S4-RAD7-BEFRAKT                              
162400     ELSE                                                                 
162500       IF MID-NYCKEL-GRP = LOW-VALUE                                      
162600          MOVE S1-RAD6-BEFRAKT  TO S4-RAD7-BEFRAKT                        
162700       ELSE                                                               
162800          MOVE PHUV-KDFRAKT     TO W-4732-KDFRAKT                         
162900          PERFORM IMS-GU-4732-WDG740                                      
163000          IF SEGMENT-FINNS                                                
163100             MOVE FRAKT-BEFRAKT(GMTA-GMT-KDSPRAK)                         
163200                                TO S4-RAD7-BEFRAKT                        
163300          ELSE                                                            
163400             MOVE SPACE         TO S4-RAD7-BEFRAKT                        
163500          END-IF                                                          
163600       END-IF                                                             
163700     END-IF                                                               
163800                                                                          
163900     MOVE PHUV-PRFRAKT     TO S4-RAD7-PRFRAKT                             
164000     MOVE S4-RAD7          TO SEND-RAD                                    
164100     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
164200     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
164300     ADD +2 TO RAD-IX                                                     
164400     .                                                                    
164500     EJECT                                                                
164600                                                                          
164700 BDC-SKRIV-RAD11-TOTAL SECTION.                                           
164800                                                                          
164900     MOVE SPACE            TO SEND-RAD                                    
165000     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
165100     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
165200                                                                          
165300     IF MID-NYCKEL-GRP = LOW-VALUE                                        
165400        CONTINUE                                                          
165500     ELSE                                                                 
165600         PERFORM S06-REDIG-LEVVILLKOR                                     
165700     END-IF                                                               
165800                                                                          
165900     MOVE WS-LEVVIL-ORT     TO S4-RAD11-LEVVIL-ORT                        
166000                                                                          
166100     IF DIST79-DEALER-PRICE                                               
166200       MOVE 'SEK'             TO S4-RAD11-KDVALISO                        
166300     ELSE                                                                 
166400       MOVE SPAR-KDVALISO     TO S4-RAD11-KDVALISO                        
166500     END-IF                                                               
166600                                                                          
166700     COMPUTE WS-SUFKTBEL ROUNDED =                                        
166800             WS-SUFKTBEL / PHUV-PRKURS                                    
166900                                                                          
167000     MOVE WS-SUFKTBEL       TO S4-RAD11-TOTVALUE                          
167100                                                                          
167200     MOVE S4-RAD11         TO SEND-RAD                                    
167300     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
167400     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
167500     ADD +2 TO RAD-IX                                                     
167600     .                                                                    
167700     EJECT                                                                
167800                                                                          
167900 BDD-HAEMTA-FORSKN-TEXT SECTION.                                          
168000                                                                          
168100     MOVE SPACE            TO SEND-RAD                                    
168200     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
168300     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
168400                                                                          
168500     PERFORM IMS-GU-XXDB-WDG731A                                          
168600     IF SEGMENT-FINNS                                                     
168700       PERFORM IMS-GNP-XXDB-WDG731                                        
168800                                                                          
168900       MOVE ZERO TO TAB-INDX                                              
169000       PERFORM UNTIL SEGMENT-SAKNAS                                       
169100         ADD +1              TO TAB-INDX                                  
169200         MOVE FOERS-BEFORSKN TO TABELL-BEFORSKN(TAB-INDX)                 
169300         PERFORM IMS-GNP-XXDB-WDG731                                      
169400       END-PERFORM                                                        
169500                                                                          
169600       MOVE +99 TO TAB-INDX                                               
169700       MOVE NEJ TO SW-TAB                                                 
169800       PERFORM UNTIL TABELL-KLAR OR TAB-INDX = ZERO                       
169900          IF TABELL-BEFORSKN (TAB-INDX) = SPACE                           
170000             SUBTRACT +1 FROM TAB-INDX                                    
170100          ELSE                                                            
170200             MOVE JA TO SW-TAB                                            
170300          END-IF                                                          
170400       END-PERFORM                                                        
170500                                                                          
170600       MOVE TAB-INDX TO SPAR-TAB-INDX                                     
170700       ADD TAB-INDX  TO RAD-IX                                            
170800       ADD 1         TO RAD-IX                                            
170900       IF RAD-IX > MAX-RADER                                              
171000         PERFORM BDDA-SKRIV-NY-SIDA                                       
171100       END-IF                                                             
171200                                                                          
171300       MOVE +0                          TO TAB-INDX                       
171400       MOVE SPACE          TO SEND-RAD                                    
171500       MOVE WS-SKIP-BLANK  TO STYRTECKEN-RAD                              
171600       PERFORM S04-DAP-PUT-FROM-SEND-AREA                                 
171700                                                                          
171800                                                                          
171900       PERFORM UNTIL TAB-INDX = SPAR-TAB-INDX                             
172000         MOVE SPACE        TO SEND-RAD                                    
172100         ADD +1 TO TAB-INDX                                               
172200         MOVE TABELL-BEFORSKN(TAB-INDX) TO S4-RAD12-BEFORSKN              
172300         MOVE S4-RAD12                  TO SEND-RAD                       
172400         MOVE WS-SKIP-BLANK TO STYRTECKEN-RAD                             
172500         PERFORM S04-DAP-PUT-FROM-SEND-AREA                               
172600       END-PERFORM                                                        
172700                                                                          
172800     END-IF                                                               
172900     .                                                                    
173000     EJECT                                                                
173100                                                                          
173200 BDDA-SKRIV-NY-SIDA SECTION.                                              
173300                                                                          
173400     MOVE SPACE            TO SEND-RAD                                    
173500     MOVE WS-PAGESKIP      TO STYRTECKEN-RAD                              
173600     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
173700                                                                          
173800     MOVE SPACE            TO SEND-RAD                                    
173900     ADD +1                TO IDSID-RAKN                                  
174000     MOVE DAGENS-DATUM     TO S5-RAD1-DATUM                               
174100     MOVE IDSID-RAKN       TO S5-RAD1-IDSID                               
174200     MOVE S5-RAD1          TO SEND-RAD                                    
174300     MOVE WS-SKIP-HYPEN TO STYRTECKEN-RAD                                 
174400     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
174500                                                                          
174600     MOVE SPACE            TO SEND-RAD                                    
174700     MOVE WS-SKIP-BLANK    TO STYRTECKEN-RAD                              
174800     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
174900                                                                          
175000     MOVE SPACE            TO SEND-RAD                                    
175100     MOVE MID-TIUPPDAT     TO S5-RAD2-TIUPPDAT                            
175200     MOVE MID-TIUPPTID     TO S5-RAD2-TIUPPTID                            
175300     MOVE S5-RAD2          TO SEND-RAD                                    
175400     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
175500     PERFORM S05-SKRIV-RAD                                                
175600     MOVE WS-SKIP-BLANK TO STYRTECKEN-RAD                                 
175700     PERFORM S04-DAP-PUT-FROM-SEND-AREA                                   
175800     .                                                                    
175900     EJECT                                                                
176000                                                                          
176100 C-FRAN-ORDERSYSTEMET SECTION.                                            
176200                                                                          
176300     MOVE MID-IDDISTR  TO W-IDDISTR-Q2                                    
176400                          W-IDDISTR-WDB2                                  
176500     MOVE MID-IDKUNDNR TO W-IDKUNDNR-Q2                                   
176600                          W-IDKUNDNR-WDB2                                 
176700     MOVE MID-IDKUNDRF TO W-IDKUNDRF-Q2                                   
176800     MOVE WC-CDC-SE    TO W-IDDC                                          
176900                                                                          
177000     PERFORM IMS-GU-ORQI-WDQ2                                             
177100     PERFORM IMS-GU-GMTA-WDB2                                             
177200     IF GMTA-GMT-KDSPRAK < 6                                              
177300        ADD 1 TO GMTA-GMT-KDSPRAK                                         
177400     END-IF                                                               
177500                                                                          
177600     MOVE OHUV-IDORDER TO W-IDORDER-Q2                                    
177700     IF MID-NYCKEL-GRP = LOW-VALUE                                        
177800       PERFORM CA-SKRIV-SID-1                                             
177900     END-IF                                                               
178000                                                                          
178100     PERFORM CB-SKRIV-SID-2                                               
178200                                                                          
178300     .                                                                    
178400     EJECT                                                                
178500                                                                          
178600 CA-SKRIV-SID-1 SECTION.                                                  
178700                                                                          
178800     MOVE OHUV-IDDISTR    TO S1-RAD1-IDDISTR                              
178900     MOVE OHUV-IDKUNDNR   TO S1-RAD1-IDKUNDNR                             
179000     MOVE DAGENS-DATUM    TO S1-RAD1-DATUM                                
179100     MOVE +1              TO S1-RAD1-IDSID                                
179200     MOVE S1-RAD1         TO WS-LISTRAD                                   
179300     MOVE PRT-NYSIDA-RAD7 TO PRT-RADSKIP                                  
179400     PERFORM S05-SKRIV-RAD                                                
179500                                                                          
179600     MOVE OHUV-BEBETRAD-1 TO S1-RAD2-BENAMN                               
179700     MOVE S1-RAD2         TO WS-LISTRAD                                   
179800     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
179900     PERFORM S05-SKRIV-RAD                                                
180000                                                                          
180100     MOVE OHUV-BEBETRAD-2 TO S1-RAD2-BENAMN                               
180200     MOVE S1-RAD2         TO WS-LISTRAD                                   
180300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
180400     PERFORM S05-SKRIV-RAD                                                
180500                                                                          
180600     MOVE OHUV-ADBETRAD-1 TO S1-RAD2-BENAMN                               
180700     MOVE S1-RAD2         TO WS-LISTRAD                                   
180800     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
180900     PERFORM S05-SKRIV-RAD                                                
181000                                                                          
181100     MOVE OHUV-ADBETRAD-2 TO S1-RAD2-BENAMN                               
181200     MOVE S1-RAD2         TO WS-LISTRAD                                   
181300     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
181400     PERFORM S05-SKRIV-RAD                                                
181500                                                                          
181600     MOVE OHUV-BEGMT-RAD1   TO S1-RAD3-BENAMN                             
181700     MOVE S1-RAD3           TO WS-LISTRAD                                 
181800     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
181900     PERFORM S05-SKRIV-RAD                                                
182000                                                                          
182100     MOVE OHUV-BEGMT-RAD2   TO S1-RAD2-BENAMN                             
182200     MOVE S1-RAD2           TO WS-LISTRAD                                 
182300     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
182400     PERFORM S05-SKRIV-RAD                                                
182500                                                                          
182600     MOVE OHUV-ADGMT-GATA   TO S1-RAD2-BENAMN                             
182700     MOVE S1-RAD2           TO WS-LISTRAD                                 
182800     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
182900     PERFORM S05-SKRIV-RAD                                                
183000                                                                          
183100     MOVE OHUV-ADGMT-PADR   TO S1-RAD2-BENAMN                             
183200     MOVE S1-RAD2           TO WS-LISTRAD                                 
183300     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
183400     PERFORM S05-SKRIV-RAD                                                
183500                                                                          
183600     MOVE MID-TIUPPDAT      TO S1-RAD4-TIUPPDAT                           
183700     MOVE MID-TIUPPTID      TO S1-RAD4-TIUPPTID                           
183800     MOVE MID-IDKUNDRF(1:7) TO S1-RAD4-IDORDNR                            
183900     MOVE S1-RAD4           TO WS-LISTRAD                                 
184000     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
184100     PERFORM S05-SKRIV-RAD                                                
184200                                                                          
184300     MOVE OHUV-BEKUNDRF     TO S1-RAD5-BEKUNDRF                           
184400     MOVE S1-RAD5           TO WS-LISTRAD                                 
184500     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
184600     PERFORM S05-SKRIV-RAD                                                
184700                                                                          
184800     MOVE SPACE             TO S1-RAD6-BEFRAKT                            
184900     MOVE S1-RAD6           TO WS-LISTRAD                                 
185000     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
185100     PERFORM S05-SKRIV-RAD                                                
185200                                                                          
185300     MOVE SPACE             TO S1-RAD7-LEVVIL-ORT                         
185400     MOVE ZERO              TO S1-RAD7-PRTOTAL                            
185500     MOVE S1-RAD7           TO WS-LISTRAD                                 
185600     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
185700     PERFORM S05-SKRIV-RAD                                                
185800                                                                          
185900     PERFORM CAA-SUMMERA-WDQ221                                           
186000                                                                          
186100     MOVE WS-VKORDNTO       TO S1-RAD8-VKORDNTO                           
186200     MOVE S1-RAD8           TO WS-LISTRAD                                 
186300     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
186400     PERFORM S05-SKRIV-RAD                                                
186500                                                                          
186600     MOVE ZERO              TO S1-RAD9-VKORDBTO                           
186700     MOVE S1-RAD9           TO WS-LISTRAD                                 
186800     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
186900     PERFORM S05-SKRIV-RAD                                                
187000                                                                          
187100     MOVE WS-VLORDNTO       TO S1-RAD10-VLORDBTO                          
187200     MOVE S1-RAD10          TO WS-LISTRAD                                 
187300     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
187400     PERFORM S05-SKRIV-RAD                                                
187500                                                                          
187600     MOVE SPACE             TO S1-RAD11-TEBANK                            
187700     MOVE S1-RAD11          TO WS-LISTRAD                                 
187800     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
187900     PERFORM S05-SKRIV-RAD                                                
188000                                                                          
188100     MOVE SPACE             TO S1-RAD12-TEBANK                            
188200     MOVE S1-RAD12          TO WS-LISTRAD                                 
188300     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
188400     PERFORM S05-SKRIV-RAD                                                
188500                                                                          
188600     MOVE SPACE             TO S1-RAD13-TEBANKTO                          
188700     MOVE S1-RAD13          TO WS-LISTRAD                                 
188800     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
188900     PERFORM S05-SKRIV-RAD                                                
189000                                                                          
189100     .                                                                    
189200     EJECT                                                                
189300                                                                          
189400 CAA-SUMMERA-WDQ221    SECTION.                                           
189500                                                                          
189600     PERFORM IMS-GNP-ORQI-WDQ221                                          
189800     PERFORM UNTIL SEGMENT-SAKNAS                                         
189900       COMPUTE WS-VKORDNTO ROUNDED =                                      
190000                                  WS-VKORDNTO + LOR-VKORDNTO              
190100       COMPUTE WS-VLORDNTO ROUNDED =                                      
190200                                  WS-VLORDNTO + LOR-VLORDNTO              
190210                                                                          
190300       PERFORM IMS-GNP-ORQI-WDQ221                                        
190400     END-PERFORM                                                          
190700     .                                                                    
190800     EJECT                                                                
190900                                                                          
191000 CB-SKRIV-SID-2 SECTION.                                                  
191100                                                                          
191200     IF MID-NYCKEL-GRP NOT = LOW-VALUE                                    
191300       MOVE MID-IDORDER              TO W-IDORDER-MAX-Q4                  
191400                                        W-IDORDER-MIN-Q4                  
191500       MOVE WC-CDC-SE                TO W-IDDC-MIN-Q4                     
191600       MOVE MID-IDARTNR              TO W-IDARTNR-MIN-Q4                  
191700       MOVE MID-IDLOPNR              TO W-IDLOPNR-MIN-Q4                  
191800       MOVE MID-ADLAGOMR             TO W-ADLAGOMR-MIN-Q4                 
191900       MOVE MID-ADGANG               TO W-ADGANG-MIN-Q4                   
192000       MOVE MID-ADPLATS              TO W-ADPLATS-MIN-Q4                  
192100       MOVE MID-IDSID                TO IDSID-RAKN                        
192200       MOVE +1                       TO SID-RAKN                          
192300       IF DIST79-DEALER-PRICE                                             
192400         IF MID-SUORDV-LOC > 0                                            
192500           MOVE MID-SUORDV-LOC       TO SUORDV-TOT-LOC                    
192600         ELSE                                                             
192700           IF MID-SUORDV-LOCPREL > 0                                      
192800             MOVE MID-SUORDV-LOCPREL TO SUORDV-TOT-LOCPREL                
192900           END-IF                                                         
193000         END-IF                                                           
193100       ELSE                                                               
193200         MOVE MID-SUORDV             TO SUORDV-TOT                        
193300       END-IF                                                             
193400     ELSE                                                                 
193500       MOVE OHUV-IDORDER             TO W-IDORDER-MIN-Q4                  
193600                                        W-IDORDER-MAX-Q4                  
193700       MOVE +2                       TO IDSID-RAKN                        
193800                                          SID-RAKN                        
193900     END-IF                                                               
194000     PERFORM IMS-GU-ORQF-WDQ4                                             
194100     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
194200       IF RAD-IX = MAX-RADER                                              
194300         IF SID-RAKN NOT = MAX-SIDOR                                      
194400           PERFORM CCA-SKRIV-RAD1-4                                       
194500           ADD +1 TO IDSID-RAKN                                           
194600         END-IF                                                           
194700         MOVE +0 TO RAD-IX                                                
194800         ADD +1 TO SID-RAKN                                               
194900       ELSE                                                               
195000         PERFORM CCB-SKRIV-RAD-5                                          
195100         ADD +1 TO RAD-IX                                                 
195200         PERFORM IMS-GN-ORQF-WDQ4                                         
195300       END-IF                                                             
195400     END-PERFORM                                                          
195500     .                                                                    
195600     EJECT                                                                
195700                                                                          
195800 CCA-SKRIV-RAD1-4 SECTION.                                                
195900                                                                          
196000     MOVE DAGENS-DATUM     TO S3-RAD1-DATUM                               
196100     MOVE IDSID-RAKN       TO S3-RAD1-IDSID                               
196200     MOVE S3-RAD1          TO WS-LISTRAD                                  
196300     MOVE PRT-NYSIDA-RAD4  TO PRT-RADSKIP                                 
196400     PERFORM S05-SKRIV-RAD                                                
196500                                                                          
196600     MOVE MID-TIUPPDAT   TO S3-RAD2-TIUPPDAT                              
196700     MOVE MID-TIUPPTID   TO S3-RAD2-TIUPPTID                              
196800     MOVE S3-RAD2          TO WS-LISTRAD                                  
196900     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
197000     PERFORM S05-SKRIV-RAD                                                
197100                                                                          
197200     MOVE S3-RAD3          TO WS-LISTRAD                                  
197300     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
197400     PERFORM S05-SKRIV-RAD                                                
197500                                                                          
197600     MOVE S3-RAD4          TO WS-LISTRAD                                  
197700     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
197800     PERFORM S05-SKRIV-RAD                                                
197900     .                                                                    
198000     EJECT                                                                
198100                                                                          
198200 CCB-SKRIV-RAD-5 SECTION.                                                 
198300                                                                          
198400     MOVE ORAD-IDARTNR     TO W-IDARTNR-D3                                
198500     MOVE OHUV-IDSKYLT     TO W-IDSKYLT-D3                                
198600     MOVE ORAD-BERADREF    TO S3-RAD5-BERADREF                            
198700     MOVE ORAD-IDARTNR     TO S3-RAD5-IDARTNR                             
198800     MOVE ORAD-REKSIFFR    TO S3-RAD5-REKSIFFR                            
198900     PERFORM IMS-GU-BENA-WDD3                                             
199000     IF SEGMENT-FINNS                                                     
199100       MOVE TEXT-BEART     TO S3-RAD5-BEART                               
199200     ELSE                                                                 
199300       MOVE 'PART DESC MISSING' TO S3-RAD5-BEART                          
199400     END-IF                                                               
199500     MOVE ORAD-KVBEART-Q   TO S3-RAD5-KVBEART-Q                           
199600     MOVE ORAD-PRARTNTO    TO S3-RAD5-PRARTNTO                            
199700     IF DIST79-DEALER-PRICE                                               
199800       IF ORAD-PRARTNTO-LOC > 0                                           
199900           COMPUTE WS-TOTPRIS = ORAD-KVBEART-Q                            
200000                           * ORAD-PRARTNTO-LOC                            
200100       ELSE                                                               
200200         IF ORAD-PRARTNTO-LOCPREL > 0                                     
200300           COMPUTE WS-TOTPRIS = ORAD-KVBEART-Q                            
200400                       * ORAD-PRARTNTO-LOCPREL                            
200500         END-IF                                                           
200600       END-IF                                                             
200700     ELSE                                                                 
200800       COMPUTE WS-TOTPRIS = ORAD-KVBEART-Q                                
200900                           * ORAD-PRARTNTO                                
201000     END-IF                                                               
201100     MOVE WS-TOTPRIS       TO S3-RAD5-TOTPRIS                             
201200     MOVE SPACE            TO S3-RAD5-KDARTURS                            
201300     IF GMTA-GMT-FLFAKURS = JA                                            
201400       IF OHUV-IDDISTR < +7800 OR > +7899                                 
201500         MOVE ORAD-KDARTURS TO S3-RAD5-KDARTURS                           
201600       END-IF                                                             
201700     END-IF                                                               
201800     IF GMTA-GMT-FLFAKVKT = JA                                            
201900       MOVE ORAD-VKART TO S3-RAD5-KDURSVKT                                
202000     ELSE                                                                 
202100       MOVE ZERO           TO S3-RAD5-KDURSVKT                            
202200     END-IF                                                               
202300     IF GMTA-GMT-KDSTATNR > 0 AND < 7                                     
202400       PERFORM IMS-GU-ARTC-WDK611                                         
202500       IF GMTA-GMT-KDSTATNR = 4                                           
202600         MOVE CLAG-IDSTATNR(GMTA-GMT-KDSTATNR) TO                         
202700              S3-RAD5-IDSTATNR-A                                          
202800       ELSE                                                               
202900         MOVE CLAG-IDSTATNR(GMTA-GMT-KDSTATNR) TO                         
203000              S3-RAD5-IDSTATNR-C                                          
203100       END-IF                                                             
203200     END-IF                                                               
203300     IF DIST79-DEALER-PRICE                                               
203400       IF SUORDV-TOT-LOC > 0                                              
203500         ADD WS-TOTPRIS    TO SUORDV-TOT-LOC                              
203600       ELSE                                                               
203700         IF SUORDV-TOT-LOCPREL > 0                                        
203800           ADD WS-TOTPRIS  TO SUORDV-TOT-LOCPREL                          
203900         END-IF                                                           
204000       END-IF                                                             
204100     ELSE                                                                 
204200       ADD WS-TOTPRIS      TO SUORDV-TOT                                  
204300     END-IF                                                               
204400     MOVE S3-RAD5          TO WS-LISTRAD                                  
204500     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
204600     PERFORM S05-SKRIV-RAD                                                
204700                                                                          
204800     .                                                                    
204900     EJECT                                                                
205000                                                                          
205100 Z-FINIT SECTION.                                                         
205200                                                                          
205300     PERFORM S03-CLOSE-PRINTER                                            
205400                                                                          
205500     PERFORM S04-DAP-CLOSE                                                
205600                                                                          
205700     .                                                                    
205800     EJECT                                                                
205900                                                                          
206000 S02-OPEN-PRINTER SECTION.                                                
206100                                                                          
206200     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
206300                         PRT-OPEN                                         
206400                         WS-IDPRT                                         
206500                         ALT2-PCB                                         
206600                         WS-DUMMY                                         
206700                         WS-DUMMY                                         
206800                                                                          
206900     .                                                                    
207000     EJECT                                                                
207100                                                                          
207200 S03-CLOSE-PRINTER SECTION.                                               
207300                                                                          
207400     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
207500                         PRT-CLOSE                                        
207600                         WS-IDPRT                                         
207700                         ALT2-PCB                                         
207800                         WS-DUMMY                                         
207900                         WS-DUMMY                                         
208000     .                                                                    
208100     EJECT                                                                
208200                                                                          
208300 S05-SKRIV-RAD SECTION.                                                   
208400                                                                          
208500     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
208600                         PRT-WRITE                                        
208700                         WS-IDPRT                                         
208800                         ALT2-PCB                                         
208900                         PRT-RADSKIP                                      
209000                         WS-RAD                                           
209100     .                                                                    
209200     EJECT                                                                
209300                                                                          
209400 S06-REDIG-LEVVILLKOR SECTION.                                            
209500                                                                          
209600     IF DIST79-DEALER-PRICE                                               
209700       STRING '          '                                                
209800           PHUV-BELOSORT DELIMITED BY SIZE                                
209900             INTO WS-LEVVIL-ORT                                           
210000     ELSE                                                                 
210100       IF PHUV-KDLEVVIL > ZERO                                            
210200          MOVE PHUV-KDLEVVIL   TO W-KDLEVVIL                              
210300          PERFORM IMS-GU-4735-WDG750                                      
210400          IF SEGMENT-FINNS                                                
210500             STRING LEVVIL-BELEVVIL(GMTA-GMT-KDSPRAK)                     
210600                    ' '                                                   
210700                    PHUV-BELOSORT DELIMITED BY '   '                      
210800                    INTO WS-LEVVIL-ORT                                    
210900          ELSE                                                            
211000             STRING '          '                                          
211100                    PHUV-BELOSORT DELIMITED BY SIZE                       
211200                    INTO WS-LEVVIL-ORT                                    
211300          END-IF                                                          
211400       ELSE                                                               
211500          IF PHUV-PREMBHNT > 0 AND                                        
211600             PHUV-PRFRAKT  = 0 AND                                        
211700             PHUV-PRFOERS  = 0 AND                                        
211800             PHUV-PRLEGKST = 0 AND                                        
211900             PHUV-PRAVDRAG = 0                                            
212000              MOVE 'FCA GOTHENBURG '   TO WS-LEVVIL-ORT                   
212100          ELSE                                                            
212200             IF PHUV-PRFRAKT = ZERO                                       
212300                  MOVE 'FCA GOTHENBURG  (INCOTERMS 2010) '                
212400                                       TO WS-LEVVIL-ORT                   
212500             ELSE                                                         
212600                IF PHUV-PRFOERS = ZERO                                    
212700                   STRING 'CPT  (INCOTERMS 2010) '                        
212800                          PHUV-BELOSORT DELIMITED BY SIZE                 
212900                          INTO WS-LEVVIL-ORT                              
213000                ELSE                                                      
213100                   STRING 'CIP   (INCOTERMS 2010) '                       
213200                          PHUV-BELOSORT DELIMITED BY SIZE                 
213300                          INTO WS-LEVVIL-ORT                              
213400                END-IF                                                    
213500             END-IF                                                       
213600          END-IF                                                          
213700       END-IF                                                             
213800     END-IF                                                               
213900     .                                                                    
214000     EJECT                                                                
214100                                                                          
214200 S07-BERAKNA-TOTVARDE SECTION.                                            
214300                                                                          
214400     MOVE PHUV-IDDISTR TO DIST03-IDDISTR                                  
214500                                                                          
214600     IF DIST79-DEALER-PRICE                                               
214700       MOVE PHUV-SUORDV-LOC  TO WS-SUFKTBEL                               
214800     ELSE                                                                 
214900       COMPUTE     WS-SUFKTBEL = (PHUV-SUORDV                             
215000                                        + PHUV-PREMBHNT                   
215100                                        + PHUV-PRFRAKT                    
215200                                        + PHUV-PRFOERS                    
215300                                        + PHUV-PRLEGKST                   
215400                                        - PHUV-PRAVDRAG)                  
215500     END-IF                                                               
215600                                                                          
215700     IF DIST03-SVERIGE AND PHUV-KDMOMSIN = +1                             
215800        COMPUTE WS-SUFKTBEL = WS-SUFKTBEL  *                              
215900                             (1            +                              
216000                             (CONS-REMOMS  /                              
216100                              100))                                       
216200     END-IF                                                               
216300                                                                          
216400     .                                                                    
216500     EJECT                                                                
216600                                                                          
216700 S04-DAP-OPEN SECTION.                                                    
216800                                                                          
216900     MOVE 'OPEN'                     TO DAP-KDFUNC                        
217000                                                                          
217100     CALL WZ04DAP USING DAP-WZ04DAP                                       
217200                                                                          
217300     IF DAP-KDRC > 0                                                      
217400       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
217500       STRING 'WZ04DAP "OPEN" ERROR RC=' KDRC-DISPLAY                     
217600              '. ' DAP-BEFEL                                              
217700       DELIMITED BY SIZE INTO ERRTEXT                                     
217800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
217900     END-IF                                                               
218000     .                                                                    
218100     SKIP3                                                                
218200 S04-DAP-PUT-FROM-SEND-AREA SECTION.                                      
218300                                                                          
218400     MOVE 'PUT'                           TO DAP-KDFUNC                   
218500                                                                          
218600     MOVE LENGTH OF SEND-RAD-GRP          TO DAP-KVDLEN                   
218700     MOVE SEND-RAD-GRP                    TO DAP-TEOUTDATA                
218800     CALL WZ04DAP USING DAP-WZ04DAP                                       
218900                                                                          
219000     IF DAP-KDRC > 0                                                      
219100       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
219200       STRING 'WZ04DAP "PUT" ERROR RC=' KDRC-DISPLAY                      
219300              '. ' DAP-BEFEL                                              
219400       DELIMITED BY SIZE INTO ERRTEXT                                     
219500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
219600     END-IF                                                               
219700     .                                                                    
219800     SKIP3                                                                
219900 S04-DAP-CLOSE SECTION.                                                   
220000                                                                          
220100     MOVE 'CLOSE'                    TO DAP-KDFUNC                        
220200     CALL WZ04DAP USING DAP-WZ04DAP                                       
220300                                                                          
220400     IF DAP-KDRC > 0                                                      
220500       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
220600       STRING 'WZ04DAP "CLOSE" ERROR RC=' KDRC-DISPLAY                    
220700              '. ' DAP-BEFEL                                              
220800       DELIMITED BY SIZE INTO ERRTEXT                                     
220900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
221000     END-IF                                                               
221100     .                                                                    
221200                                                                          
221300* --- IMS SEKTIONER ---                                                   
221400                                                                          
221500 IMS-GU-MSG SECTION.                                                      
221600                                                                          
221700     MOVE '  QC' TO GODK-STATUSKODER                                      
221800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
221900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200     SKIP2                                                                
222300 IMS-GHU-PROC-WDE8 SECTION.                                               
222400                                                                          
222500     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
222600          DELIMITED BY SIZE INTO SSA1                                     
222700     MOVE '    ' TO GODK-STATUSKODER                                      
222800     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-E8 SSA1                  
222900     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
223000     PERFORM IMS-STATUSKONTROLL                                           
223100     .                                                                    
223200     EJECT                                                                
223300 IMS-REPL-PROC-WDE8 SECTION.                                              
223400                                                                          
223500     MOVE '  '   TO GODK-STATUSKODER                                      
223600     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-E8                      
223700     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000     EJECT                                                                
224100 IMS-GHU-PROD-WDE9 SECTION.                                               
224200                                                                          
224300     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
224400                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
224500          DELIMITED BY SIZE INTO SSA1                                     
224600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
224700     CALL CBLTDLI USING GHU PROD-PCB DLI-IO-AREA-E9 SSA1                  
224800     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
224900     PERFORM IMS-STATUSKONTROLL                                           
225000     .                                                                    
225100     SKIP2                                                                
225200 IMS-GHN-PROD-WDE9 SECTION.                                               
225300                                                                          
225400     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
225500                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
225600          DELIMITED BY SIZE INTO SSA1                                     
225700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
225800     CALL CBLTDLI USING GHN PROD-PCB DLI-IO-AREA-E9 SSA1                  
225900     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
226000     PERFORM IMS-STATUSKONTROLL                                           
226100     .                                                                    
226200     EJECT                                                                
226300 IMS-REPL-PROD-WDE9 SECTION.                                              
226400                                                                          
226500     MOVE '  '   TO GODK-STATUSKODER                                      
226600     CALL CBLTDLI USING REPL PROD-PCB DLI-IO-AREA-E9                      
226700     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
226800     PERFORM IMS-STATUSKONTROLL                                           
226900     .                                                                    
227000 IMS-GU-ORQI-WDQ2 SECTION.                                                
227100                                                                          
227200     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
227300          DELIMITED BY SIZE INTO SSA1                                     
227400     MOVE '    ' TO GODK-STATUSKODER                                      
227500     CALL CBLTDLI USING GU ORQISEQ-PCB DLI-IO-AREA-Q2 SSA1                
227600     MOVE ORQISEQ-STATUS-CODE TO STATUS-WS                                
227700     PERFORM IMS-STATUSKONTROLL                                           
227800     .                                                                    
227900     SKIP2                                                                
228000 IMS-GNP-ORQI-WDQ221 SECTION.                                             
228100                                                                          
228200     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
228300          DELIMITED BY SIZE INTO SSA1                                     
228310     MOVE   'WLORQI21'        TO SSA2                                     
228400     MOVE '  GE' TO GODK-STATUSKODER                                      
228500     CALL CBLTDLI USING GNP ORQISEQ-PCB DLI-IO-AREA-Q221 SSA1 SSA2        
228600     MOVE ORQISEQ-STATUS-CODE TO STATUS-WS                                
228700     PERFORM IMS-STATUSKONTROLL                                           
228800     .                                                                    
228900     EJECT                                                                
229000 IMS-GU-ORQF-WDQ4 SECTION.                                                
229100                                                                          
229200     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
229300                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
229400          DELIMITED BY SIZE INTO SSA1                                     
229500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
229600     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-Q4 SSA1                   
229700     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
229800     PERFORM IMS-STATUSKONTROLL                                           
229900     .                                                                    
230000     SKIP2                                                                
230100 IMS-GN-ORQF-WDQ4 SECTION.                                                
230200                                                                          
230300     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
230400                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
230500          DELIMITED BY SIZE INTO SSA1                                     
230600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
230700     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-Q4 SSA1                   
230800     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
230900     PERFORM IMS-STATUSKONTROLL                                           
231000     .                                                                    
231100     EJECT                                                                
231200 IMS-GU-GMTA-WDB2 SECTION.                                                
231300                                                                          
231400     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
231500          DELIMITED BY SIZE INTO SSA1                                     
231600     MOVE '  ' TO GODK-STATUSKODER                                        
231700     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-GMTA SSA1                 
231800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100     SKIP2                                                                
232200 IMS-GU-GMTB-WDB3 SECTION.                                                
232300                                                                          
232400     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
232500                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
232600          DELIMITED BY SIZE INTO SSA1                                     
232700     MOVE '  ' TO GODK-STATUSKODER                                        
232800     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-GMTB SSA1                 
232900     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
233000     PERFORM IMS-STATUSKONTROLL                                           
233100     .                                                                    
233200     EJECT                                                                
233300 IMS-GU-ARTC-WDK611 SECTION.                                              
233400                                                                          
233500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
233600          DELIMITED BY SIZE INTO SSA1                                     
233700     MOVE 'WLARTC11 ' TO SSA2                                             
233800     MOVE '  '   TO GODK-STATUSKODER                                      
233900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-D6 SSA1 SSA2              
234000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300     SKIP2                                                                
234400 IMS-GU-BENA-WDD3 SECTION.                                                
234500                                                                          
234600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
234700          DELIMITED BY SIZE INTO SSA1                                     
234800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
234900          DELIMITED BY SIZE INTO SSA2                                     
235000     MOVE '  GE' TO GODK-STATUSKODER                                      
235100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-D3 SSA1 SSA2              
235200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
235300     PERFORM IMS-STATUSKONTROLL                                           
235400     .                                                                    
235500     EJECT                                                                
235600 IMS-GU-XXDB-WDG731A SECTION.                                             
235700                                                                          
235800     STRING 'WLXXDB01(WDGXKEY  =' W-KDFORSKN-4731-X ')'                   
235900          DELIMITED BY SIZE INTO SSA1                                     
236000     MOVE '  GE' TO GODK-STATUSKODER                                      
236100     CALL CBLTDLI USING GU XXDB-PCB DLI-IO-AREA-473A SSA1                 
236200     MOVE XXDB-STATUS-CODE TO STATUS-WS                                   
236300     PERFORM IMS-STATUSKONTROLL                                           
236400     .                                                                    
236500     SKIP2                                                                
236600 IMS-GNP-XXDB-WDG731 SECTION.                                             
236700                                                                          
236800     MOVE 'WLXXDB11 ' TO SSA1                                             
236900     MOVE '  GE' TO GODK-STATUSKODER                                      
237000     CALL CBLTDLI USING GNP XXDB-PCB DLI-IO-AREA-4731 SSA1                
237100     MOVE XXDB-STATUS-CODE TO STATUS-WS                                   
237200     PERFORM IMS-STATUSKONTROLL                                           
237300     .                                                                    
237400     EJECT                                                                
237500 IMS-GU-4732-WDG740 SECTION.                                              
237600                                                                          
237700     STRING 'WL473201(WDGXKEY  =' W-4732-IDHTYP-X ')'                     
237800          DELIMITED BY SIZE INTO SSA1                                     
237900     MOVE   'WL473211 ' TO SSA2                                           
238000     MOVE '  GE' TO GODK-STATUSKODER                                      
238100     CALL CBLTDLI USING GU 4732-PCB DLI-IO-AREA-473211 SSA1 SSA2          
238200     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
238300     PERFORM IMS-STATUSKONTROLL                                           
238400     .                                                                    
238500     SKIP2                                                                
238600 IMS-GU-4735-WDG750 SECTION.                                              
238700                                                                          
238800     STRING 'WL473501(WDGXKEY  =' W-WDGX4735-X ')'                        
238900          DELIMITED BY SIZE INTO SSA1                                     
239000     MOVE   'WL473511 ' TO SSA2                                           
239100     MOVE '  GE' TO GODK-STATUSKODER                                      
239200     CALL CBLTDLI USING GU 4735-PCB DLI-IO-AREA-473511 SSA1 SSA2          
239300     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
239400     PERFORM IMS-STATUSKONTROLL                                           
239500     .                                                                    
239600     EJECT                                                                
239700 IMS-GET-WDB101 SECTION.                                                  
239800                                                                          
239900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
240000          DELIMITED BY SIZE INTO SSA1                                     
240100     MOVE '  GE' TO GODK-STATUSKODER                                      
240200     CALL CBLTDLI USING GU WDB1-PCB WDB1-AREA SSA1                        
240300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     .                                                                    
240600     EJECT                                                                
240700 IMS-STATUSKONTROLL SECTION.                                              
240800                                                                          
240900     SET STATUS-IX TO 1                                                   
241000     SEARCH GODK-STATUS                                                   
241100       AT END                                                             
241200         CALL FELLOG                                                      
241300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
241400         CONTINUE                                                         
241500     END-SEARCH                                                           
241600     .                                                                    
