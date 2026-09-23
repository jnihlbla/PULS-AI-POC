000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W476KULB.                                                
000400 AUTHOR.         STINA MOGREN.                                            
000500 DATE-WRITTEN.   04/11/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PGM:ET SKRIVER FÖR NDC:ERNA ?                                    
001100*        - KULLAGERBILAGA                                                 
001200*        -                                                                
001300*                                                                         
001400*                                                                         
001500*        PGM:ET ANVÄNDER:                                                 
001600*            ALT-PCB     ANVÄNDS AV W006PRR1 (PCB FÖR PRINTER)            
001700*            LISB-PCB    ANVÄNDS AV W006PRR1 FÖR LISTÅTERSTART            
001800*            WDE1                                                         
001900*            WDB2                                                         
002000*            WDB1                                                         
002100*            WDG7                                                         
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- LISTFIL TILL VCOM                                          
003500     SELECT W4759N-001                 ASSIGN TO W476KUD2.                
003600                                                                          
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W4759N-001                                                           
004200     LABEL RECORD   STANDARD                                              
004300     RECORDING      F                                                     
004400     BLOCK CONTAINS 0.                                                    
004500 01  W4759N-001-L       PIC X(115).                                       
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W476KULB'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  TAB-IX                      PIC S9(5)   VALUE +0   COMP SYNC.        
005600 77  INDX                        PIC S9(5)   VALUE +0   COMP SYNC.        
005700 77  MAX-IX                      PIC S9(5)   VALUE +100 COMP SYNC.        
005800 77  WS-SIDNR                    PIC S9(3)   VALUE +0   COMP-3.           
005900 77  WS-RADNR                    PIC S9(3)   VALUE +0   COMP-3.           
006000 77  WS-POST-RAEKNARE            PIC S9(5)   VALUE +0   COMP-3.           
006100 77  WS-ANTAL-RADER              PIC S9(3)   VALUE +0   COMP-3.           
006200 77  WS-VKORDBTO                 PIC S9(8)V9(1) VALUE 0 COMP-3.           
006300 77  WS-VKARTNTO-G               PIC S9(4)V9(3) VALUE 0 COMP-3.           
006400 77  WS-VLORDBTO                 PIC S9(6)V9(3) VALUE 0 COMP-3.           
006500 77  WS-PRAVCOST-TOT             PIC S9(7)V9(2) VALUE 0 COMP-3.           
006600 77  MAX-RADER                   PIC S9(3)   VALUE +40  COMP-3.           
006700 01  BER-W-PRPRIS                PIC S9(7)V9(5) COMP-3 VALUE 0.           
006800 01  W-PRPRIS                    PIC S9(07)V9(2)  VALUE ZERO.             
006900 01  W-VKVIKT                    PIC S9(08)V9(3)  VALUE ZERO.             
007000     SKIP2                                                                
007100 01  FELTEXT.                                                             
007200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007400*                                                                         
007500*                                                                         
007600*      --- VALID IDDC CODES                                               
007700*                                                                         
007800*01    -COPY WWDC99                                                       
007900       EJECT                                                              
008000*                                                                         
       01  WS-META                     PIC X(5) VALUE '¤META'.                  
       01  WS-IDDISTR                  PIC Z(4)9.                               
       01  WS-IDSHIPM-Z                PIC Z(6)9.                               
       01  WS-TIMESTAMP.                                                        
           03  FILLER                  PIC X       VALUE 'D'.                   
           03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
           03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
           03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
           03  FILLER                  PIC X       VALUE '_'.                   
           03  FILLER                  PIC X       VALUE 'T'.                   
           03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
           03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
           03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
                                                                                
008100 77  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
008200                                                                          
008300 01  WS-DATUM-AAMMDD.                                                     
008400     03  WS-DATUM-AA             PIC 9(2)          VALUE ZERO.            
008500     03  WS-DATUM-MM             PIC 9(2)          VALUE ZERO.            
008600     03  WS-DATUM-DD             PIC 9(2)          VALUE ZERO.            
008700                                                                          
008800 01 WS-DATUM-DDMMAA.                                                      
008900     03  WS-DD                   PIC 9(2)          VALUE ZERO.            
009000     03  WS-MM                   PIC 9(2)          VALUE ZERO.            
009100     03  WS-AA                   PIC 9(2)          VALUE ZERO.            
009200     EJECT                                                                
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800                                                                          
009900 01  WS-CENTURY-DATUM.                                                    
010000     03 WS-CENTURY               PIC 9(2).                                
010100     03 WS-AAMMDD.                                                        
010200       05 WS-AA2                 PIC 9(2).                                
010300       05 WS-MM2                 PIC 9(2).                                
010400       05 WS-DD2                 PIC 9(2).                                
010500     EJECT                                                                
010600 01  WS-LIST-AREA.                                                        
010700     03  WS-DUMMY                PIC X.                                   
010800     03  WS-IDPRTLST             PIC X(8)    VALUE SPACE.                 
010900     03  WS-LISTRAD.                                                      
011000       05  FILLER                PIC X(4)    VALUE SPACE.                 
011100       05  LIST-RAD              PIC X(113).                              
011200     EJECT                                                                
011300 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
011400     SKIP3                                                                
011500*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
011600     EJECT                                                                
011700*01  FILLER   -COPY WWDIS110    -RED TEST-IDDISTR.                        
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL COPYTEXT WWOMVAND                                
012000*01  -COPY WWOMVAND                                                       
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL COPYTEXT W400ARTU                                
012300*01  -COPY W400ARTU                                                       
012400     EJECT                                                                
012500 01  DYNAMISKA-SUBPROGRAM.                                                
012600*                                                                         
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013000     03  INTSOR                  PIC X(8)    VALUE 'INTSOR  '.            
013100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013200     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
013300     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
013400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013500     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
013600     EJECT                                                                
013700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014000     SKIP2                                                                
014100 77  KDRC-DISPLAY                PIC Z(5).                                
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL POSTSUM                                          
014400*                                                                         
014500*01  -COPY W0005   -PRE  POSTSUM-                                         
014600     EJECT                                                                
014700*- - - - - - - - - - - - - -  PARAMETRAR TILL  INTSOR                     
014800 01  INTSOR-HJAELP-AREA.                                                  
014900     03  INTSOR-POST-ANTAL         PIC S9(3)  VALUE ZERO COMP-3.          
015000     03  INTSOR-POST-LAENGD        PIC S9(3)  VALUE ZERO COMP-3.          
015100     03  INTSOR-SORT-FAELT-LAENGD  PIC S9(3)  VALUE ZERO COMP-3.          
015200     SKIP3                                                                
015300 01  FILLER                      PIC X(16)   VALUE  'SEND-AREA'.          
015400 01  SEND-AREA.                                                           
015500*    03  -COPY WZ01SEND                                                   
015600                                                                          
015700 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
015800 01  WS-SKIP1                      PIC X      VALUE ' '.                  
015900 01  WS-SKIP2                      PIC X      VALUE '0'.                  
016000 01  WS-SKIP3                      PIC X      VALUE '-'.                  
016100                                                                          
016200 01  SEND-RAD-STYRTECKEN.                                                 
016300     03  STYRTECKEN-RAD          PIC X.                                   
016400     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
016500                                                                          
016600*                                                                         
016700*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
016800*                                                                         
016900*01  FILLER   -COPY W006PRAR                                              
017000     EJECT                                                                
017100                                                                          
017200 01  W-IDPRTLST                  PIC X(8).                                
017300 01  WS-PRT.                                                              
017400     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
017500     03 WS-PRT-IDLIST.                                                    
017600        05 WS-IDLIST             PIC X(4)  VALUE 'KULB'.                  
017700        05 WS-PRT-IDDISTR        PIC 9(4).                                
017800        05 FILLER                PIC X(2).                                
017900     03 WS-PRT-LISTRAD.                                                   
018000        05 WS-RAD                PIC X(115).                              
018100     03 WS-PRT-DUMMY             PIC X(1).                                
018200                                                                          
018300 01  FILLER                      PIC X(20)   VALUE SPACE.                 
018400 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
018500*    --- KULLAGER-TABELL                                                  
018600 01  W475W556.                                                            
018700*                                                                         
018800*           KULLAGER BILAGA  TABELL                                       
018900*                            TABELL GRÄNSER                               
019000*                            LEDTEXTER                                    
019100     SKIP3                                                                
019200*                                                                         
019300   03  KULLAGER-BILAGA-LEDTEXTER.                                         
019400    04  KULBIL-RUBRIK.                                                    
019500       05  FILLER  PIC X(50) VALUE                                        
019600          'KULLAGERBILAGA                                    '.           
019700       05  FILLER  PIC X(50) VALUE                                        
019800          'BALL AND ROLLER BEARING ATTACHMENT TO INVOICE     '.           
019900       05  FILLER  PIC X(50) VALUE                                        
020000          'ANNEXE ROULEMENTS A BILLES                        '.           
020100       05  FILLER  PIC X(50) VALUE                                        
020200          'BALL AND ROLLER BEARING ATTACHMENT TO INVOICE     '.           
020300       05  FILLER  PIC X(50) VALUE                                        
020400          'BALL AND ROLLER BEARING ATTACHMENT TO INVOICE     '.           
020500       05  FILLER  PIC X(50) VALUE                                        
020600          'BALL AND ROLLER BEARING ATTACHMENT TO INVOICE     '.           
020700    04  FILLER  REDEFINES  KULBIL-RUBRIK.                                 
020800       05  KULBILAGA-BETECKNING PIC X(50) OCCURS 6 TIMES.                 
020900*                                                                         
021000    04  KUL-IDARTNR-LEDTEXT.                                              
021100       05  FILLER  PIC X(09) VALUE 'VOLVOPART'.                           
021200*                                                                         
021300    04  KUL-BEARTKUL-LEDTEXT.                                             
021400       05  FILLER  PIC X(15) VALUE 'NAME           '.                     
021500*                                                                         
021600    04  KUL-KVLEVART-LEDTEXT.                                             
021700       05  FILLER  PIC X(07) VALUE 'DEL.QTY'.                             
021800*                                                                         
021900    04  KUL-DI-INNER-LEDTEXT.                                             
022000       05  FILLER  PIC X(04) VALUE 'IDIM'.                                
022100*                                                                         
022200    04  KUL-DI-YTTER-LEDTEXT.                                             
022300       05  FILLER  PIC X(04) VALUE 'ODIM'.                                
022400*                                                                         
022500    04   KUL-BELEVKUL-LEDTEXT.                                            
022600       05  FILLER  PIC X(10) VALUE 'SUPPLIER  '.                          
022700*                                                                         
022800    04  KUL-IDLEVKUL-LEDTEXT.                                             
022900       05  FILLER  PIC X(08) VALUE 'SUP.PART'.                            
023000*                                                                         
023100    04  KUL-VKARTNTO-LEDTEXT.                                             
023200       05  FILLER  PIC X(08) VALUE '  WEIGHT'.                            
023300*                                                                         
023400    04  KUL-VKARTBTO-LEDTEXT.                                             
023500       05  FILLER  PIC X(08) VALUE 'GROSS WG'.                            
023600*                                                                         
023700    04  KUL-VOLVO-PRARTNTO-LEDTEXT.                                       
023800       05  FILLER  PIC X(08) VALUE 'VOLVO PR'.                            
023900*                                                                         
024000    04  KUL-USA-PRARTNTO-LEDTEXT.                                         
024100       05  FILLER  PIC X(08) VALUE 'US.PRICE'.                            
024200*                                                                         
024300    04  KUL-VOLVO-PRARTBTO-LEDTEXT.                                       
024400       05  FILLER  PIC X(08) VALUE 'TOT PRCE'.                            
024500*                                                                         
024600    04  KUL-USA-PRARTBTO-LEDTEXT.                                         
024700       05  FILLER  PIC X(08) VALUE 'US.TOT.P'.                            
024800*                                                                         
024900    04  KUL-BEARTURS-LEDTEXT.                                             
025000       05  FILLER  PIC X(03) VALUE 'ORG'.                                 
025100*                                                                         
025200    04  KUL-IDSTAKUL-LEDTEXT.                                             
025300       05  FILLER  PIC X(08) VALUE 'STAT.NO '.                            
025400*                                                                         
025500    04  KUL-BETOTAL-LEDTEXT.                                              
025600       05  FILLER  PIC X(08) VALUE 'TOTAL   '.                            
025700*                                                                         
025800     EJECT                                                                
025900*                                                                         
026000*                                                                         
026100*    *** FÖR TOTAL AV TABELLEN                                            
026200*                                                                         
026300   03  KULTOT.                                                            
026400       07  KULTOT-KVLEVART         PIC S9(7)        COMP-3.               
026500       07  KULTOT-VKARTBTO         PIC S9(6)V9(3)   COMP-3.               
026600       07  KULTOT-PRARTBTO-VOLVO   PIC S9(9)V9(2)   COMP-3.               
026700       07  KULTOT-PRARTBTO-USA     PIC S9(9)V9(2)   COMP-3.               
026800*                                                                         
026900   03  FILLER.                                                            
027000*                                                                         
027100*    *** FÖR BERÄKNING  USA PRIS                                          
027200*                                                                         
027300     05  KUL-USA-KOEFF     PIC S9V9(3)  COMP-3  VALUE +1.000.             
027400*                                                                         
027500*    *** FÖR BEHANDLING AV TABELLEN                                       
027600*                                                                         
027700     05  KULTAB-MAX-ANTAL-POST  PIC S9(4)  COMP VALUE +150.               
027800     05  KULTAB-ANTAL-POST      PIC S9(4)  COMP VALUE ZERO.               
027900*                                                                         
028000*    *** FÖR SORTERING  AV TABELLEN                                       
028100*                                                                         
028200     05  KULTAB-POST-LAENGD     PIC S9(3) COMP-3 VALUE +100.              
028300     05  KULTAB-IDARTNR-LAENGD  PIC S9(3) COMP-3 VALUE +5.                
028400*                                                                         
028500*                                                                         
028600     05  KULLAGER-TABELL  OCCURS  150 TIMES.                              
028700*                                                                         
028800         07 KULTAB-IDARTNR     PIC S9(9)       COMP-3.                    
028900*                      *** ARTIKELNR                                      
029000         07 KULTAB-REKSIFFR    PIC S9(1)       COMP-3.                    
029100*                      *** KONTROLLSIFFRA                                 
029200         07 KULTAB-VKARTNTO    PIC S9(04)V9(3) COMP-3.                    
029300*                      *** NETTOVIKT                                      
029400         07 KULTAB-PRARTNTO    PIC S9(07)V9(2) COMP-3.                    
029500*                      *** NETTOPRIS                                      
029600         07 KULTAB-KVLEVART    PIC S9(09)      COMP-3.                    
029700*                      *** LEVERERAT ANTAL                                
029800         07 KULTAB-KDARTURS    PIC X(2).                                  
029900*                      *** URSPRUNG                                       
030000         07 KULTAB-IDSTAKUL    PIC S9(09)      COMP-3.                    
030100*                      *** STATISTIK NR                                   
030200         07 KULTAB-DI-INNER    PIC S9(04)V9(1) COMP-3.                    
030300*                      *** DIAMETER INNER                                 
030400         07 KULTAB-DI-YTTER    PIC S9(04)V9(1) COMP-3.                    
030500*                      *** DIAMETER YTTER                                 
030600         07 KULTAB-BEARTKUL.                                              
030700          09 KULTAB-BEARTKUL-1 PIC X(15).                                 
030800          09 KULTAB-BEARTKUL-2 PIC X(15).                                 
030900*                      *** KULLAGER BENÄMNING                             
031000         07 KULTAB-BELEVKUL.                                              
031100          09 KULTAB-BELEVKUL-1 PIC X(11).                                 
031200          09 KULTAB-BELEVKUL-2 PIC X(10).                                 
031300*                      *** KULLAGER LEVERANSBENÄMNING                     
031400         07 KULTAB-IDLEVKUL.                                              
031500          09 KULTAB-IDLEVKUL-1 PIC X(08).                                 
031600          09 KULTAB-IDLEVKUL-2 PIC X(08).                                 
031700*                      *** KULLAGER LEVERANS IDENTITET                    
031800     EJECT                                                                
031900 01  FILLER                    PIC X(10)  VALUE 'SPAR-AREA '.             
032000 01  SPAR-AREA.                                                           
032100     03 SPAR-IDKUNDRF          PIC X(10)  VALUE SPACE.                    
032200     03 SPAR-IDSHIPM           PIC 9(7)   VALUE ZERO.                     
032300     03 SPAR-IDFAKT            PIC S9(7)  VALUE ZERO.                     
032400                                                                          
032500 01  FILLER                    PIC X(16)  VALUE 'LIST-RADER'.             
032600 01  LIST-RADER.                                                          
032700                                                                          
032800     03 RUB-DATUM.                                                        
032900       05 FILLER               PIC X(90)  VALUE SPACE.                    
033000       05 DATUM.                                                          
033100         07 RUB-MM             PIC 99.                                    
033200         07 FILLER             PIC X     VALUE '/'.                       
033300         07 RUB-DD             PIC 99.                                    
033400         07 FILLER             PIC X     VALUE '/'.                       
033500         07 RUB-CC             PIC 99.                                    
033600         07 RUB-AA             PIC 99.                                    
033700                                                                          
033800     03 RAD-SIDNR.                                                        
033900       05 FILLER               PIC X(97)  VALUE SPACE.                    
034000       05 SIDNR                PIC Z(2)9.                                 
034100                                                                          
034200 01  RAD-000.                                                             
034300     03  FILLER                    PIC X(1).                              
034400     03  RAD000-TEXT               PIC X(111).                            
034500     SKIP3                                                                
034600 01  RAD-0.                                                               
034700     03  FILLER                    PIC X(60).                             
034800     03  RAD0-TEXT                 PIC X(33).                             
034900     SKIP3                                                                
035000                                                                          
035100 01   RAD-1.                                                              
035200     03  FILLER                    PIC X(1).                              
035300     03  RAD1-BEGMT                PIC X(33).                             
035400     03  FILLER                    PIC X(3).                              
035500     03  RAD1-BEBET                PIC X(33).                             
035600     03  FILLER                    PIC X(2).                              
035700     03  COM-VECKA                 PIC 9(2).                              
035800     03  COM-DAGNR                 PIC 9(1).                              
035900     03  RAD1-TIAAMMDD-X           PIC 9(06).                             
036000     03  RAD1-IDDISTR-X            PIC Z(4)9.                             
036100     03  RAD1-IDKUNDNR             PIC Z(07).                             
036200     03  FILLER                    PIC X(01).                             
036300     03  RAD1-ASTERISK             PIC X(01).                             
036400     03  RAD1-KDFAKTYP             PIC X(01).                             
036500     03  RAD1-FAKT-SEP             PIC X(01).                             
036600     03  RAD1-IDFAKT               PIC 9(07).                             
036700     03  FILLER REDEFINES RAD1-IDFAKT.                                    
036800       05  RAD1-IDFAKT-X           PIC X(07).                             
036900     03  FILLER                    PIC X(05).                             
037000     03  RAD1-SIDNR                PIC Z(03).                             
037100 01  RAD1.                                                                
037200     03  FILLER                    PIC X(30).                             
037300     03  RAD4H-IMPORTER            PIC X(35).                             
037400     03  FILLER                    PIC X(02).                             
037500     03  RAD1-TIAAMMDD             PIC 9(06).                             
037600     03  RAD1-IDDISTR              PIC Z(4)9.                             
037700     03  FILLER                    PIC X(01).                             
037800     03  RAD1-IDSHIPM              PIC Z(06)9.                            
037900     03  FILLER                    PIC X(04).                             
038000     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
038100     03  FILLER                    PIC X(01).                             
038200     03  RAD1-IDLBBET              PIC X(12).                             
038300     03  FILLER                    PIC X(02).                             
038400     03  RAD1-PAGE-NO              PIC Z(03).                             
038500                                                                          
038600 01  RAD-HEAD.                                                            
038700     03  FILLER                    PIC X(15).                             
038800     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
038900     03  FILLER                    PIC X(2).                              
039000     03  RAD1H-IMPORTER            PIC X(35).                             
039100     03  FILLER                    PIC X(2).                              
039200     03  RAD-TYP-IDSHIP            PIC X(50).                             
039300                                                                          
039400 01  RAD2-HEAD.                                                           
039500     03  FILLER                    PIC X(30).                             
039600     03  RAD2H-IMPORTER            PIC X(35).                             
039700                                                                          
039800 01  RAD3-HEAD.                                                           
039900     03  FILLER                    PIC X(30).                             
040000     03  RAD3H-IMPORTER            PIC X(35).                             
040100                                                                          
040200 01  RAD5-HEAD.                                                           
040300     03  FILLER                    PIC X(30).                             
040400     03  RAD5H-IMPORTER            PIC X(35).                             
040500                                                                          
040600*                                                                         
040700*    FÖR UTSKRIFT AV KULLAGER BILAGA                                      
040800*                                                                         
040900 01  ARBETS-RAD.                                                          
041000**   03  FILLER                    PIC X(01).                             
041100     03  RAD-IDARTNR-X.                                                   
041200       05  RAD-IDARTNR             PIC Z(07)9.                            
041300       05  RAD-IDART-STRK          PIC X(01).                             
041400       05  RAD-REKSIFFR            PIC 9(01).                             
041500     03  FILLER                    PIC X(01).                             
041600     03  RAD-BEARTKUL              PIC X(14).                             
041700**   03  FILLER                    PIC X(01).                             
041800     03  RAD-KVLEVART-W.                                                  
041900       05  RAD-KVLEVART            PIC Z(07).                             
042000     03  RAD-KVLEVART-X   REDEFINES RAD-KVLEVART-W                        
042100                                   PIC X(07) JUSTIFIED RIGHT.             
042200     03  FILLER                    PIC X(01).                             
042300     03  RAD-DI-INNER-W.                                                  
042400       05  RAD-DI-INNER            PIC Z(03).9.                           
042500     03  RAD-DI-INNER-X   REDEFINES RAD-DI-INNER-W                        
042600                                   PIC X(05) JUSTIFIED RIGHT.             
042700     03  FILLER                    PIC X(01).                             
042800     03  RAD-DI-YTTER-W.                                                  
042900       05  RAD-DI-YTTER            PIC Z(03).9.                           
043000     03  RAD-DI-YTTER-X   REDEFINES RAD-DI-YTTER-W                        
043100                                   PIC X(05) JUSTIFIED RIGHT.             
043200     03  FILLER                    PIC X(01).                             
043300     03  RAD-BELEVKUL              PIC X(11).                             
043400**   03  FILLER                    PIC X(01).                             
043500     03  RAD-IDLEVKUL              PIC X(08).                             
043600     03  FILLER                    PIC X(01).                             
043700     03  RAD-VKARTNTO-W.                                                  
043800       05  RAD-VKARTNTO            PIC Z(05)V999.                         
043900     03  RAD-VKARTNTO-X   REDEFINES RAD-VKARTNTO-W                        
044000                                   PIC X(08) JUSTIFIED RIGHT.             
044100     03  FILLER                    PIC X(01).                             
044200     03  RAD-VKARTBTO-W.                                                  
044300       05  RAD-VKARTBTO            PIC Z(04)9.99.                         
044400     03  RAD-VKARTBTO-X   REDEFINES RAD-VKARTBTO-W                        
044500                                   PIC X(08) JUSTIFIED RIGHT.             
044600     03  FILLER                    PIC X(01).                             
044700     03  RAD-PRARTNTO-W.                                                  
044800       05  RAD-PRARTNTO            PIC Z(04)9.99.                         
044900     03  RAD-PRARTNTO-X   REDEFINES RAD-PRARTNTO-W                        
045000                                   PIC X(08) JUSTIFIED RIGHT.             
045100     03  FILLER                    PIC X(01).                             
045200     03  RAD-PRARTBTO-W.                                                  
045300       05  RAD-PRARTBTO            PIC Z(04)9.99.                         
045400     03  RAD-PRARTBTO-X   REDEFINES RAD-PRARTBTO-W                        
045500                                   PIC X(08) JUSTIFIED RIGHT.             
045600     03  FILLER                    PIC X(01).                             
045700     03  RAD-KDARTURS              PIC X(02).                             
045800     03  FILLER                    PIC X(01).                             
045900     03  RAD-IDSTAKUL-X.                                                  
046000       05  RAD-IDSTAKUL            PIC 9(4)B9(3) BLANK WHEN ZERO.         
046100     EJECT                                                                
046200                                                                          
046300*                                                                         
046400     EJECT                                                                
046500 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
046600     SKIP2                                                                
046700 01  KEYS-TO-DLI.                                                         
046800     03  W-WDB101KY-X.                                                    
046900        05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                  
047000        05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                   
047100                                                                          
047200     03  W-WDB201KY-X.                                                    
047300        05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
047400        05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
047500                                                                          
047600     03  W-IDSHIPM-X.                                                     
047700        05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                   
047800                                                                          
047900     03  W-WDE111KY-X.                                                    
048000        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
048100        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
048200                                                                          
048300     03  W-WDE111KY-D.                                                    
048400        05  W-WDE111-IDDISTR-D  PIC S9(05)  VALUE ZERO COMP-3.            
048500        05  W-WDE111-IDKUNDNR-D PIC S9(07)  VALUE ZERO COMP-3.            
048600                                                                          
048700     03  W-WDE111KY-MIN.                                                  
048800        05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.           
048900        05  FILLER               PIC X(04)   VALUE LOW-VALUES.            
049000                                                                          
049100     03  W-WDE111KY-MAX.                                                  
049200        05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.           
049300        05  FILLER               PIC X(04)   VALUE HIGH-VALUES.           
049400                                                                          
049500     03  W-WDE121KY-X.                                                    
049600        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
049700        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
049800                                                                          
049900     03  W-IDARTNR-X.                                                     
050000        05  W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
050100     03  W-IDDC-X.                                                        
050200        05  W-IDDC               PIC X(2)    VALUE SPACE.                 
050300     03  W-IDSKYLT-X.                                                     
050400*       05  W-IDSKYLT            PIC X(3)    VALUE 'S  '.                 
050500        05  W-IDSKYLT            PIC X(3)    VALUE 'USA'.                 
050600     03  WDGX01.                                                          
050700*                                 ROTSEGMENT WDGX (WDG7)                  
050800*                                 NYCKEL IDHTYP + 26 BYTES VALFRI         
050900        05 IDHTYP               PIC X(4).                                 
051000*                                 HÄNDELSETYP                             
051100        05 NYCKEL-VALFRI        PIC X(26).                                
051200*                                 KULLAGER-REGISTER                       
051300     05  NYCKEL-4747             REDEFINES NYCKEL-VALFRI.                 
051400       07  IDARTNR-4747          PIC S9(9)         COMP-3.                
051500*                                *** ARTIKELNUMMER                        
051600                                                                          
051700                                                                          
051800*    --- STATUS-KOD FRÅN IMS                                              
051900 01  STATUS-WS                   PIC XX.                                  
052000     88  SEGMENT-FINNS                       VALUE '  '.                  
052100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
052200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
052300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
052400     88  IMS-EJ-OK                           VALUE 'XD'.                  
052500     SKIP2                                                                
052600 01  GOOD-STATUSCODES.                                                    
052700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052800     SKIP3                                                                
052900 01  SSA1                        PIC X(64).                               
053000 01  SSA2                        PIC X(64).                               
053100 01  SSA3                        PIC X(64).                               
053200 01  SSA4                        PIC X(64).                               
053300     EJECT                                                                
053400*    --- IMS FUNKTIONSKODER                                               
053500*01  -COPY W0003                                                          
053600     EJECT                                                                
053700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
053800 01  DLI-IO-WDE101.                                                       
053900*    03  -COPY WDE101                                                     
054000                                                                          
054100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
054200 01  DLI-IO-WDE111.                                                       
054300*    03  -COPY WDE111                                                     
054400                                                                          
054500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
054600 01  DLI-IO-WDE121.                                                       
054700*    03  -COPY WDE121                                                     
054800                                                                          
054900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
055000 01  DLI-IO-WDE131.                                                       
055100*    03  -COPY WDE131                                                     
055200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
055300 01  DLI-IO-WDB201.                                                       
055400*    03  -COPY WDB201                                                     
055500                                                                          
055600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
055700 01  DLI-IO-WDB101.                                                       
055800*    03  -COPY WDB101                                                     
055900                                                                          
056000 01  DLI-IO-KUL.                                                          
056100*  03  FILLER     -COPY WDGX4747                                          
056200     EJECT                                                                
056300                                                                          
056400 LINKAGE SECTION.                                                         
056500*01  -COPY W476TRPD                                                       
056600                                                                          
056700*01  -COPY W0009   -PRE MSG-                                              
056800     EJECT                                                                
056900*01  -COPY W0009   -PRE ALT-                                              
057000     EJECT                                                                
057100*01  -COPY W0008  -PRE WDE1-                                              
057200     05  FILLER                  PIC X.                                   
057300*01  -COPY W0008  -PRE WDB2-                                              
057400     05  FILLER                  PIC X.                                   
057500*01  -COPY W0008  -PRE WDB1-                                              
057600     05  FILLER                  PIC X.                                   
057700*01  -COPY W0008  -PRE WDG7-                                              
057800     05  FILLER                  PIC X.                                   
057900                                                                          
058000*01  -COPY W0008  -PRE LISB-                                              
058100     05  FILLER                  PIC X.                                   
058200     EJECT                                                                
058300 PROCEDURE DIVISION  USING TRPD-W476TRPD                                  
058400*                          MSG-PCB                                        
058500                           ALT-PCB                                        
058600                           WDE1-PCB                                       
058700                           WDB2-PCB                                       
058800                           WDB1-PCB                                       
058900                           WDG7-PCB                                       
059000                           LISB-PCB.                                      
059100 MAIN SECTION.                                                            
059200     ENTRY 'DLITCBL' USING TRPD-W476TRPD                                  
059300*                          MSG-PCB                                        
059400                           ALT-PCB                                        
059500                           WDE1-PCB                                       
059600                           WDB2-PCB                                       
059700                           WDB1-PCB                                       
059800                           WDG7-PCB                                       
059900                           LISB-PCB.                                      
060000                                                                          
060100     PERFORM A-INIT                                                       
060200     PERFORM IMS-GU-WDE101                                                
060300                                                                          
060400     PERFORM B-SPARA-DATA                                                 
060500                                                                          
060600     PERFORM C-INIT-ALLM                                                  
060700                                                                          
060800     PERFORM IMS-GU-WDE101                                                
060900                                                                          
061000     IF SEGMENT-FINNS                                                     
061100       PERFORM IMS-GNP-WDE111                                             
061200                                                                          
061300       MOVE SGMT-IDDISTR                TO W-WDB201-IDDISTR               
061400       MOVE SGMT-IDKUNDNR               TO W-WDB201-IDKUNDNR              
061500       PERFORM IMS-GU-WDB201                                              
061600       MOVE GMT-IDPARTNR                TO W-WDB101-IDPARTNR              
061700       MOVE GMT-IDFTG                   TO W-WDB101-IDFTG                 
061800       PERFORM IMS-GU-WDB101                                              
061900*      PERFORM S03-INIT-PRINTER-ID                                        
062000*      PERFORM S04-OPPNA-PRINTER                                          
062100                                                                          
062200       PERFORM F-SKAPA-KULLAGERBILAGA                                     
062300                                                                          
062400*      PERFORM S06-STAENG-PRINTER                                         
062500     END-IF                                                               
062600                                                                          
062700     PERFORM Z-FINIT                                                      
062800                                                                          
062900     MOVE ZERO TO RETURN-CODE                                             
063000     GOBACK                                                               
063100     .                                                                    
063200     EJECT                                                                
063300 A-INIT SECTION.                                                          
063400     SKIP2                                                                
063500                                                                          
063600*    OPEN OUTPUT W4759N-001                                               
063700                                                                          
063800     MOVE '***---- INIT-KULLAGER ***----'  TO FELTEXT                     
063900*                                                                         
                                                                                
064000     MOVE ZERO TO KULTOT-KVLEVART                                         
064100                  KULTOT-VKARTBTO                                         
064200                  KULTOT-PRARTBTO-VOLVO                                   
064300                  KULTOT-PRARTBTO-USA                                     
064400                  KULTAB-ANTAL-POST                                       
                        WS-IDDISTR                                              
                        WS-IDSHIPM-Z                                            
                                                                                
064500     MOVE +1 TO INDX                                                      
064600     PERFORM UNTIL                                                        
064700      ( INDX > KULTAB-MAX-ANTAL-POST )                                    
064800        MOVE ZERO      TO KULTAB-IDARTNR (INDX)                           
064900        ADD +1    TO INDX                                                 
065000     END-PERFORM                                                          
065100                                                                          
065200     MOVE ZERO     TO TAB-IX                                              
065300                                                                          
065400     ACCEPT DAGENS-DATUM  FROM DATE                                       
065500     MOVE   DAGENS-DATUM  TO WS-AAMMDD                                    
065600                                                                          
065700     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
065800                                                                          
065900     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
066000                              WS-PRT-IDPRTLST                             
066100     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
066200     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
066300     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
066400                              W-WDB201-IDDISTR                            
066500                              W-WDE111-IDDISTR-MIN                        
066600                              W-WDE111-IDDISTR-MAX                        
066700                              TEST-IDDISTR                                
066800     MOVE SPACES          TO RAD-HEAD                                     
066900                             RAD2-HEAD                                    
067000                             RAD3-HEAD                                    
067100                             RAD5-HEAD                                    
067200     .                                                                    
067300     EJECT                                                                
067400 B-SPARA-DATA SECTION.                                                    
067500                                                                          
067600     PERFORM IMS-GNP-WDE111                                               
067700     MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                          
067800     PERFORM UNTIL SEGMENT-SAKNAS                                         
067900       MOVE W-IDSHIPM         TO SPAR-IDSHIPM                             
068000                                                                          
068100       PERFORM IMS-GNP-WDE121                                             
068200       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
068300       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
068400       PERFORM UNTIL SEGMENT-SAKNAS                                       
068500         PERFORM IMS-GNP-WDE131                                           
068600         PERFORM UNTIL SEGMENT-SAKNAS                                     
068700          PERFORM S01-LAES-KULLAGER                                       
068800          IF SEGMENT-FINNS                                                
068900           MOVE 1                  TO INDX                                
069000           PERFORM UNTIL                                                  
069100            ( INDX > KULTAB-MAX-ANTAL-POST )                              
069200             IF SRAD-IDARTNR = KULTAB-IDARTNR (INDX)                      
069300                ADD SRAD-KVLEVART  TO KULTAB-KVLEVART (INDX)              
069400                MOVE KULTAB-MAX-ANTAL-POST TO INDX                        
069500             ELSE                                                         
069600             IF INDX > KULTAB-ANTAL-POST                                  
069700           MOVE SRAD-IDARTNR       TO KULTAB-IDARTNR  (INDX)              
069800           MOVE SRAD-REKSIFFR      TO KULTAB-REKSIFFR (INDX)              
069900           MOVE SRAD-KVLEVART      TO KULTAB-KVLEVART (INDX)              
070000           MOVE SRAD-KDARTURS      TO KULTAB-KDARTURS (INDX)              
070100           MOVE SRAD-PRARTNTO      TO KULTAB-PRARTNTO (INDX)              
070200           IF DIST79-DEALER-PRICE                                         
070300             MOVE SRAD-PRARTNTO-LOC TO KULTAB-PRARTNTO(INDX)              
070400           END-IF                                                         
070500           MOVE KUL-BEARTKUL       TO KULTAB-BEARTKUL (INDX)              
070600           MOVE KUL-BELEVKUL       TO KULTAB-BELEVKUL (INDX)              
070700           MOVE KUL-DIKULLAG-INNER TO KULTAB-DI-INNER (INDX)              
070800           MOVE KUL-DIKULLAG-YTTER TO KULTAB-DI-YTTER (INDX)              
070900           MOVE KUL-IDLEVKUL       TO KULTAB-IDLEVKUL (INDX)              
071000           MOVE KUL-IDSTAKUL       TO KULTAB-IDSTAKUL (INDX)              
071100           MOVE KUL-VKARTNTO       TO KULTAB-VKARTNTO (INDX)              
071200           MOVE INDX               TO KULTAB-ANTAL-POST                   
071300*          END-IF                                                         
071400           MOVE KULTAB-MAX-ANTAL-POST TO INDX                             
071500          ELSE                                                            
071600            IF INDX = KULTAB-MAX-ANTAL-POST                               
071700              MOVE 'KULLAGER TABELL FÖR LITEN ' TO FELTEXT                
071800              CALL FELLOG                                                 
071900            END-IF                                                        
072000          END-IF                                                          
072100          END-IF                                                          
072200           MOVE SRAD-IDARTNR       TO W-IDARTNR                           
072300           MOVE SHIP-IDDC          TO W-IDDC                              
072400                                                                          
072500            ADD +1                 TO INDX                                
072600          END-PERFORM                                                     
072700          END-IF                                                          
072800           PERFORM IMS-GNP-WDE131                                         
072900         END-PERFORM                                                      
073000         PERFORM IMS-GNP-WDE121                                           
073100         MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                     
073200         MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                      
073300       END-PERFORM                                                        
073400                                                                          
073500       PERFORM IMS-GNP-WDE111                                             
073600       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
073700     END-PERFORM                                                          
073800                                                                          
073900     .                                                                    
074000     EJECT                                                                
074100 C-INIT-ALLM SECTION.                                                     
074200                                                                          
074300                                                                          
074400     MOVE ZERO  TO WS-SIDNR                                               
074500                                                                          
074600     MOVE +1    TO TAB-IX                                                 
074700                                                                          
074800     MOVE +45   TO WS-RADNR                                               
074900                                                                          
075000     MOVE SPACE TO SPAR-IDKUNDRF                                          
075100                                                                          
075200     IF WS-AA < 50                                                        
075300       MOVE 20                        TO WS-CENTURY                       
075400     ELSE                                                                 
075500       MOVE 19                        TO WS-CENTURY                       
075600     END-IF                                                               
075700                                                                          
075800     MOVE WS-CENTURY                  TO RUB-CC                           
075900     MOVE DAGENS-DATUM-MAANAD         TO RUB-MM                           
076000     MOVE DAGENS-DATUM-DAG            TO RUB-DD                           
076100     MOVE DAGENS-DATUM-AAR            TO RUB-AA                           
076200                                                                          
076300     MOVE WS-SIDNR                    TO SIDNR                            
076400     .                                                                    
076500     EJECT                                                                
076600 F-SKAPA-KULLAGERBILAGA  SECTION.                                         
076700                                                                          
076800     IF KULTAB-ANTAL-POST NOT = ZERO                                      
076900        IF KULTAB-ANTAL-POST > +1                                         
077000          MOVE KULTAB-ANTAL-POST     TO INTSOR-POST-ANTAL                 
077100          MOVE KULTAB-POST-LAENGD    TO INTSOR-POST-LAENGD                
077200          MOVE KULTAB-IDARTNR-LAENGD TO INTSOR-SORT-FAELT-LAENGD          
077300          CALL INTSOR USING KULLAGER-TABELL (+1)                          
077400                            INTSOR-POST-LAENGD                            
077500                            INTSOR-POST-ANTAL                             
077600                            KULTAB-IDARTNR (+1)                           
077700                            INTSOR-SORT-FAELT-LAENGD                      
077800        END-IF                                                            
077900        MOVE +99 TO WS-RADNR                                              
078000        MOVE +1  TO INDX                                                  
078100        PERFORM UNTIL                                                     
078200        ( INDX > KULTAB-ANTAL-POST )                                      
078300          IF WS-RADNR > MAX-RADER                                         
078400            PERFORM S02-HUVUD-DEL-1                                       
078500            PERFORM FA-KULLAGER-RUBRIK                                    
078600         END-IF                                                           
078700          MOVE SPACES                   TO ARBETS-RAD                     
078800          MOVE KULTAB-IDARTNR    (INDX) TO RAD-IDARTNR                    
078900          MOVE '-'                      TO RAD-IDART-STRK                 
079000          MOVE KULTAB-REKSIFFR   (INDX) TO RAD-REKSIFFR                   
079100          MOVE KULTAB-BEARTKUL-1 (INDX) TO RAD-BEARTKUL                   
079200          MOVE KULTAB-KVLEVART   (INDX) TO RAD-KVLEVART                   
079300          MOVE KULTAB-DI-INNER   (INDX) TO RAD-DI-INNER                   
079400          MOVE KULTAB-DI-YTTER   (INDX) TO RAD-DI-YTTER                   
079500          MOVE KULTAB-BELEVKUL-1 (INDX) TO RAD-BELEVKUL                   
079600          MOVE KULTAB-IDLEVKUL-1 (INDX) TO RAD-IDLEVKUL                   
079700          MOVE KULTAB-VKARTNTO   (INDX) TO RAD-VKARTNTO                   
079800          COMPUTE W-VKVIKT   = KULTAB-VKARTNTO (INDX)                     
079900                             * KULTAB-KVLEVART (INDX)                     
080000          COMPUTE  RAD-VKARTBTO ROUNDED = W-VKVIKT                        
080100          MOVE KULTAB-PRARTNTO   (INDX) TO RAD-PRARTNTO                   
080200          COMPUTE W-PRPRIS   = KULTAB-PRARTNTO (INDX)                     
080300                             * KULTAB-KVLEVART (INDX)                     
080700          IF DIS110-AUD-VALUTA OR DIS110-THB-VALUTA                       
080900            MOVE W-PRPRIS TO BER-W-PRPRIS                                 
081000            COMPUTE W-PRPRIS ROUNDED =                                    
081100            BER-W-PRPRIS / SGMT-PRKURS                                    
081200          END-IF                                                          
081400          MOVE W-PRPRIS                 TO RAD-PRARTBTO                   
081500          MOVE KULTAB-KDARTURS   (INDX) TO RAD-KDARTURS                   
081600          MOVE KULTAB-IDSTAKUL   (INDX) TO RAD-IDSTAKUL                   
081700          MOVE PRT-AFTER-2              TO PRT-RADSKIP                    
081800          MOVE WS-SKIP2                 TO STYRTECKEN-RAD                 
081900          MOVE ARBETS-RAD               TO WS-RAD                         
082000                                           SEND-RAD                       
082100          PERFORM S05-SKRIV-EN-RAD                                        
082200          ADD   W-VKVIKT                TO KULTOT-VKARTBTO                
082300          ADD   W-PRPRIS                TO KULTOT-PRARTBTO-VOLVO          
082400          ADD KULTAB-KVLEVART (INDX)    TO KULTOT-KVLEVART                
082500          MOVE SPACES                   TO ARBETS-RAD                     
082600          MOVE KULTAB-BEARTKUL-2 (INDX) TO RAD-BEARTKUL                   
082700          MOVE KULTAB-BELEVKUL-2 (INDX) TO RAD-BELEVKUL                   
082800          MOVE KULTAB-IDLEVKUL-2 (INDX) TO RAD-IDLEVKUL                   
082900          COMPUTE RAD-PRARTNTO ROUNDED = KULTAB-PRARTNTO (INDX)           
083000                                         * KUL-USA-KOEFF                  
083100          COMPUTE W-PRPRIS       ROUNDED = KULTAB-PRARTNTO (INDX)         
083200                                         * KUL-USA-KOEFF                  
083300          COMPUTE W-PRPRIS       ROUNDED = KULTAB-PRARTNTO (INDX)         
083400                                         * KUL-USA-KOEFF                  
083800          IF DIS110-AUD-VALUTA OR DIS110-THB-VALUTA                       
084000           MOVE W-PRPRIS TO BER-W-PRPRIS                                  
084100           COMPUTE W-PRPRIS ROUNDED =                                     
084200           BER-W-PRPRIS / SGMT-PRKURS                                     
084400          END-IF                                                          
084500          COMPUTE W-PRPRIS    = W-PRPRIS * KULTAB-KVLEVART (INDX)         
084600          MOVE  W-PRPRIS                TO RAD-PRARTBTO                   
084700          ADD   W-PRPRIS                TO KULTOT-PRARTBTO-USA            
084800          MOVE PRT-AFTER-1              TO PRT-RADSKIP                    
084900          MOVE WS-SKIP1                 TO STYRTECKEN-RAD                 
085000          MOVE ARBETS-RAD               TO WS-RAD                         
085100                                           SEND-RAD                       
085200          PERFORM S05-SKRIV-EN-RAD                                        
085300          ADD +1 TO INDX                                                  
085400        END-PERFORM                                                       
085500        IF WS-RADNR + +5 > MAX-RADER                                      
085600          PERFORM S02-HUVUD-DEL-1                                         
085700          PERFORM FA-KULLAGER-RUBRIK                                      
085800        END-IF                                                            
085900        MOVE  SPACES                   TO ARBETS-RAD                      
086000        MOVE KUL-BETOTAL-LEDTEXT       TO RAD-IDARTNR-X                   
086100        MOVE KULTOT-KVLEVART           TO RAD-KVLEVART                    
086200        COMPUTE RAD-VKARTBTO ROUNDED = KULTOT-VKARTBTO                    
086300        MOVE KULTOT-PRARTBTO-VOLVO     TO RAD-PRARTBTO                    
086400        MOVE PRT-AFTER-2               TO PRT-RADSKIP                     
086500        MOVE WS-SKIP2                  TO STYRTECKEN-RAD                  
086600        MOVE ARBETS-RAD                TO WS-RAD                          
086700                                          SEND-RAD                        
086800        PERFORM S05-SKRIV-EN-RAD                                          
086900        MOVE SPACES                    TO ARBETS-RAD                      
087000        MOVE KULTOT-PRARTBTO-USA       TO RAD-PRARTBTO                    
087100        MOVE PRT-AFTER-1               TO PRT-RADSKIP                     
087200        MOVE WS-SKIP1                  TO STYRTECKEN-RAD                  
087300        MOVE ARBETS-RAD                TO WS-RAD                          
087400                                          SEND-RAD                        
087500        PERFORM S05-SKRIV-EN-RAD                                          
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900 FA-KULLAGER-RUBRIK   SECTION.                                            
088000     MOVE '***---- UA-KULLAGER-RUBRIK ***----'  TO FELTEXT                
088100                                                                          
088200     MOVE SPACE                          TO RAD-000                       
088300*    MOVE KULBILAGA-BETECKNING     (2)   TO RAD000-TEXT                   
088400     MOVE RAD-000                        TO WS-RAD                        
088500                                            SEND-RAD                      
088600     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
088700     MOVE WS-SKIP1                       TO STYRTECKEN-RAD                
088800     PERFORM S05-SKRIV-EN-RAD                                             
088900     MOVE SPACES                         TO ARBETS-RAD                    
089000     MOVE KUL-IDARTNR-LEDTEXT            TO RAD-IDARTNR-X                 
089100     MOVE KUL-BEARTKUL-LEDTEXT           TO RAD-BEARTKUL                  
089200     MOVE KUL-KVLEVART-LEDTEXT           TO RAD-KVLEVART-X                
089300     MOVE KUL-DI-INNER-LEDTEXT           TO RAD-DI-INNER-X                
089400     MOVE KUL-DI-YTTER-LEDTEXT           TO RAD-DI-YTTER-X                
089500     MOVE KUL-BELEVKUL-LEDTEXT           TO RAD-BELEVKUL                  
089600     MOVE KUL-IDLEVKUL-LEDTEXT           TO RAD-IDLEVKUL                  
089700     MOVE KUL-VKARTNTO-LEDTEXT           TO RAD-VKARTNTO-X                
089800     MOVE KUL-VKARTBTO-LEDTEXT           TO RAD-VKARTBTO-X                
089900     MOVE KUL-VOLVO-PRARTNTO-LEDTEXT     TO RAD-PRARTNTO-X                
090000     MOVE KUL-VOLVO-PRARTBTO-LEDTEXT     TO RAD-PRARTBTO-X                
090100     MOVE KUL-BEARTURS-LEDTEXT           TO RAD-KDARTURS                  
090200     MOVE KUL-IDSTAKUL-LEDTEXT           TO RAD-IDSTAKUL-X                
090300     MOVE ARBETS-RAD                     TO WS-RAD                        
090400                                            SEND-RAD                      
090500     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
090600     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
090700     PERFORM S05-SKRIV-EN-RAD                                             
090800     MOVE SPACES                         TO ARBETS-RAD                    
090900     MOVE KUL-USA-PRARTNTO-LEDTEXT       TO RAD-PRARTNTO-X                
091000     MOVE KUL-USA-PRARTBTO-LEDTEXT       TO RAD-PRARTBTO-X                
091100     MOVE ARBETS-RAD                     TO WS-RAD                        
091200                                            SEND-RAD                      
091300     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
091400     MOVE WS-SKIP1                       TO STYRTECKEN-RAD                
091500     PERFORM S05-SKRIV-EN-RAD                                             
091600     .                                                                    
091700     EJECT                                                                
091800 Z-FINIT SECTION.                                                         
091900                                                                          
092000*    CLOSE W4759N-001                                                     
092100     SKIP2                                                                
092200     MOVE 'S' TO POSTSUM-OPKOD                                            
092300     CALL POSTSUM USING POSTSUM-PARM                                      
092400     .                                                                    
092500     EJECT                                                                
092600 S01-LAES-KULLAGER SECTION.                                               
092700                                                                          
092800     MOVE '4747'             TO IDHTYP                                    
092900     MOVE LOW-VALUE          TO NYCKEL-VALFRI                             
093000     MOVE SRAD-IDARTNR       TO IDARTNR-4747                              
093100     PERFORM IMS-GU-4747-KUL                                              
093200     .                                                                    
093300     EJECT                                                                
093400 S02-HUVUD-DEL-1     SECTION.                                             
093500     MOVE '***---- S02-HUVUD-DEL-1 ***---'  TO FELTEXT                    
093600*                                                                         
093700*     EDITERING AV ACCOUNT INFO PÅ BMP-FAKT                               
093800*                                                                         
093900     MOVE SHIP-IDDC TO WS-IDDC                                            
094000*    IF (BMP AND CDC-SE) OR                                               
094100*       (BMP AND LDC-SE)                                                  
094200*      IF FAKT-IDFAKT NOT = W-IDFAKT                                      
094300*        MOVE FAKT-IDFAKT         TO W-IDFAKT                             
094400*        MOVE '57'                TO HDR-IDFTG                            
094500*        MOVE 'FAKTHEADER'        TO HDR-FAKTHEADER                       
094600*        MOVE JA                  TO SKRIV-LASER-SID1-SW                  
094700*      END-IF                                                             
094800*    END-IF                                                               
           IF WS-SIDNR = ZERO                                                   
              PERFORM S07-PRINT-META                                            
           END-IF                                                               
094900                                                                          
095000     MOVE ZERO                       TO WS-RADNR                          
095100     MOVE SPACES                     TO ARBETS-RAD                        
095200                                        WS-RAD                            
095300                                        SEND-RAD                          
095400     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
095500     PERFORM S90-PUT-DOC-LINE                                             
095600     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
095700     PERFORM S90-PUT-DOC-LINE                                             
095800     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
095900     PERFORM S90-PUT-DOC-LINE                                             
096000     MOVE BET-BEBETRAD-1             TO RAD1H-IMPORTER                    
096100     MOVE BET-BEBETRAD-2             TO RAD2H-IMPORTER                    
096200     MOVE BET-ADBETRAD-1             TO RAD3H-IMPORTER                    
096300     MOVE BET-ADBETRAD-2             TO RAD4H-IMPORTER                    
096400     MOVE BET-BELAND-SVE             TO RAD5H-IMPORTER                    
096500     MOVE 'IMPORTER REF'             TO RAD1H-IMPORTER-TEXT               
096600     MOVE KULBILAGA-BETECKNING (2)   TO RAD-TYP-IDSHIP                    
096700     MOVE RAD-HEAD                   TO WS-RAD                            
096800                                        SEND-RAD                          
096900     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
097000     ADD 7                           TO WS-RADNR                          
097100     PERFORM S05-SKRIV-EN-RAD                                             
097200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
097300     MOVE RAD2-HEAD                  TO ARBETS-RAD                        
097400                                        WS-RAD                            
097500                                        SEND-RAD                          
097600     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
097700     ADD 1                           TO WS-RADNR                          
097800     PERFORM S05-SKRIV-EN-RAD                                             
097900     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
098000                                                                          
098100     MOVE RAD3-HEAD                  TO ARBETS-RAD                        
098200                                        WS-RAD                            
098300                                        SEND-RAD                          
098400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
098500     ADD 1                           TO WS-RADNR                          
098600     PERFORM S05-SKRIV-EN-RAD                                             
098700*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
098800                                                                          
098900     ADD 1                     TO WS-SIDNR                                
099000     MOVE SPACE                TO RAD1                                    
099100     MOVE BET-ADBETRAD-2       TO RAD4H-IMPORTER                          
099200     MOVE SHIP-TISKEPPN        TO RAD1-TIAAMMDD                           
099300     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
099400     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
099500     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
099600     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
099700     MOVE WS-SIDNR             TO RAD1-PAGE-NO                            
099800     MOVE RAD1                 TO ARBETS-RAD                              
099900                                  WS-RAD                                  
100000                                  SEND-RAD                                
100100     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
100200     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
100300     ADD 1                     TO WS-RADNR                                
100400     PERFORM S05-SKRIV-EN-RAD                                             
100500     MOVE RAD5-HEAD                  TO ARBETS-RAD                        
100600                                        WS-RAD                            
100700                                        SEND-RAD                          
100800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
100900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
101000     ADD 1                           TO WS-RADNR                          
101100     PERFORM S05-SKRIV-EN-RAD                                             
101200     MOVE SPACES           TO RAD-0                                       
101300     MOVE SGMT-IDDISTR     TO TEST-IDDISTR                                
101400     IF SKOLLI-KDFAKTYP = 'P'                                             
101500       MOVE 'PROFORMA'            TO RAD0-TEXT                            
101600     END-IF                                                               
101700     MOVE RAD-0            TO WS-RAD                                      
101800                              SEND-RAD                                    
101900     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
102000*    MOVE WS-SKIP2         TO STYRTECKEN-RAD                              
102100*    PERFORM S05-SKRIV-EN-RAD                                             
102200                                                                          
102300     MOVE SPACES            TO RAD-1                                      
102400     MOVE SGMT-IDDISTR      TO TEST-IDDISTR                               
102500*    IF (FAKT-FLSAMFAK     = JA  AND                                      
102600*        SPAR-KEY-KDTULLVE = 0)                                           
102700*       MOVE SPACE    TO FAKT-BEGMT-RAD1                                  
102800*                        FAKT-BEGMT-RAD2                                  
102900*                        FAKT-ADGMT-GATA                                  
103000*                        FAKT-ADGMT-PADR                                  
103100*                        FAKT-ADGMT-LAND                                  
103200*    END-IF                                                               
103300*     MOVE GMT-BEGMT-RAD1    TO RAD1-BEGMT                                
103400*     MOVE BET-BEBETRAD-1    TO RAD1-BEBET                                
103500     MOVE RAD-1             TO WS-RAD                                     
103600                               SEND-RAD                                   
103700     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
103800*    MOVE WS-SKIP1          TO STYRTECKEN-RAD                             
103900*    PERFORM S05-SKRIV-EN-RAD                                             
104000     MOVE SHIP-IDSHIPM      TO RAD1-IDFAKT                                
104100     MOVE GMT-BEGMT-RAD2    TO RAD1-BEGMT                                 
104200     MOVE BET-BEBETRAD-2    TO RAD1-BEBET                                 
104300     MOVE RAD-1             TO WS-RAD                                     
104400                               SEND-RAD                                   
104500*    PERFORM S05-SKRIV-EN-RAD                                             
104600     MOVE SPACES            TO RAD-1                                      
104700     MOVE GMT-ADGMT-GATA    TO RAD1-BEGMT                                 
104800     MOVE BET-ADBETRAD-1    TO RAD1-BEBET                                 
104900                                                                          
105000*    IF NOT BMP                                                           
105100*       MOVE D-VECKA TO COM-VECKA                                         
105200*       MOVE D-DAGNR TO COM-DAGNR                                         
105300*    END-IF                                                               
105400                                                                          
105500     MOVE SHIP-IDDC      TO WS-IDDC                                       
105600     MOVE SGMT-IDDISTR   TO TEST-IDDISTR                                  
105700     IF SDC-IT                                                            
105800       MOVE SHIP-TISKEPPN   TO WS-DATUM                                   
105900       MOVE WS-DATUM        TO WS-DATUM-AAMMDD                            
106000       MOVE WS-DATUM-AA     TO WS-AA                                      
106100       MOVE WS-DATUM-MM     TO WS-MM                                      
106200       MOVE WS-DATUM-DD     TO WS-DD                                      
106300       MOVE WS-DATUM-DDMMAA TO RAD1-TIAAMMDD                              
106400     ELSE                                                                 
106500       MOVE SHIP-TISKEPPN   TO RAD1-TIAAMMDD                              
106600     END-IF                                                               
106700                                                                          
106800     MOVE SGMT-IDDISTR      TO RAD1-IDDISTR                               
106900     MOVE SGMT-IDKUNDNR     TO RAD1-IDKUNDNR                              
107000*    IF SPAR-KEY-KDTULLVE NOT = ZERO                                      
107100*      MOVE '*'             TO RAD1-ASTERISK                              
107200*    END-IF                                                               
107300     IF SKOLLI-KDFAKTYP NOT = SPACE                                       
107400       MOVE SKOLLI-KDFAKTYP TO RAD1-KDFAKTYP                              
107500       MOVE '-'             TO RAD1-FAKT-SEP                              
107600     END-IF                                                               
107700     MOVE SKOLLI-IDFAKT     TO RAD1-IDFAKT                                
107800***    IDSHIPM  ? ?                                                       
107900*    ADD +1                 TO WS-SIDNR                                   
108000     MOVE WS-SIDNR          TO RAD1-SIDNR                                 
108100     MOVE RAD-1             TO WS-RAD                                     
108200                               SEND-RAD                                   
108300*    PERFORM S05-SKRIV-EN-RAD                                             
108400     MOVE SPACES            TO ARBETS-RAD                                 
108500     MOVE GMT-ADGMT-PADR    TO RAD1-BEGMT                                 
108600     MOVE BET-ADBETRAD-2    TO RAD1-BEBET                                 
108700     MOVE RAD-1             TO WS-RAD                                     
108800                               SEND-RAD                                   
108900*    PERFORM S05-SKRIV-EN-RAD                                             
109000     MOVE SPACES            TO ARBETS-RAD                                 
109100     MOVE GMT-ADGMT-LAND    TO RAD1-BEGMT                                 
109200     MOVE RAD-1             TO WS-RAD                                     
109300                               SEND-RAD                                   
109400*    PERFORM S05-SKRIV-EN-RAD                                             
109500     .                                                                    
109600     EJECT                                                                
109700 S03-INIT-PRINTER-ID SECTION.                                             
109800                                                                          
109900     MOVE SHIP-IDDC                   TO WS-IDDC                          
110000     MOVE SGMT-IDDISTR                TO TEST-IDDISTR                     
110100                                                                          
110200*    EVALUATE TRUE                                                        
110300*      WHEN NDC-US-RU                                                     
110400*        MOVE 'W4750741'              TO WS-PRT-IDPRTLST                  
110500*      WHEN NDC-US-AT                                                     
110600*        MOVE 'W4750742'              TO WS-PRT-IDPRTLST                  
110700*      WHEN NDC-US-LA                                                     
110800*        MOVE 'W4750743'              TO WS-PRT-IDPRTLST                  
110900*      WHEN NDC-CA                                                        
111000*        MOVE 'W4750751'              TO WS-PRT-IDPRTLST                  
111100*    END-EVALUATE                                                         
111200                                                                          
111300     MOVE SGMT-IDDISTR                TO WS-PRT-IDDISTR                   
111400     .                                                                    
111500     EJECT                                                                
111600 S04-OPPNA-PRINTER SECTION.                                               
111700                                                                          
111800     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
111900                         PRT-OPEN                                         
112000                         WS-PRT-IDPRTLST                                  
112100                         ALT-PCB                                          
112200                         LISB-PCB                                         
112300                         WS-PRT-IDLIST                                    
112400                         WS-PRT-DUMMY                                     
112500                         WS-PRT-DUMMY                                     
112600     .                                                                    
112700     EJECT                                                                
112800 S05-SKRIV-EN-RAD  SECTION.                                               
112900                                                                          
113000*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
113100     PERFORM S90-PUT-DOC-LINE                                             
113200                                                                          
113300*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
113400*    -- OR ALWAYS IF WE COME FROM 4622                                    
113500*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
113600*    -- CANNOT BE SET TO J)                                               
113700     IF SHIP-FLSKRIV-NU = YES OR JA                                       
113800     OR TRPD-IDPGM = 'W4062200'                                           
113900     OR TRPD-FLLDCKND = YES                                               
114000       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
114100                          PRT-WRITE                                       
114200                          W-IDPRTLST                                      
114300                          ALT-PCB                                         
114400                          PRT-RADSKIP                                     
114500                          WS-RAD                                          
114600     END-IF                                                               
114700     .                                                                    
114800     EJECT                                                                
114900 S06-STAENG-PRINTER SECTION.                                              
115000                                                                          
115100     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
115200                         PRT-CLOSE                                        
115300                         WS-PRT-IDPRTLST                                  
115400                         ALT-PCB                                          
115500                         LISB-PCB                                         
115600                         WS-PRT-IDLIST                                    
115700                         WS-PRT-DUMMY                                     
115800                         WS-PRT-DUMMY                                     
115900                                                                          
116000     .                                                                    
116100     EJECT                                                                
                                                                                
       S07-PRINT-META SECTION.                                                  
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
           STRING WS-META                                                       
                  'SHIPMENT_NUMBER='                                            
                  WS-IDSHIPM-Z                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           STRING WS-META                                                       
                  'DOCUMENT_TYPE='                                              
                  KULBILAGA-BETECKNING (2)                                      
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-TISKEPPN      TO WS-DATUM                                  
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
                                                                                
           STRING WS-META                                                       
                  'SHIPPING_DATE='                                              
                  WS-YEAR(1:2)                                                  
                  WS-DATUM                                                      
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
           STRING WS-META                                                       
                  'DISTRICT_NUMBER='                                            
                  WS-IDDISTR                                                    
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
                                                                                
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
           MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
           MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
           MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
           MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
           MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
                                                                                
           STRING WS-META                                                       
                  'FILE_NAME='                                                  
                  DELIMITED BY SIZE                                             
                  'SHIPDOC_NAK'                                                 
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (WS-IDSHIPM-Z)                                  
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  FUNCTION TRIM (WS-IDDISTR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
           .                                                                    
                                                                                
116200 S90-PUT-DOC-LINE SECTION.                                                
116210                                                                          
116300     IF (TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1')                 
116310                                 OR                                       
116320        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1' AND              
116330         SHIP-KDFAKSTA-EXP = 2)                                           
116400       IF TRPD-FLSKRIV-ONDEM = YES OR JA                                  
116500         MOVE +1                          TO SEND-IDCOM                   
116600         MOVE 'PUT'                       TO SEND-KDFUNC                  
116700         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
116800         CALL WZ01SEND USING SEND-CONTROL-AREA                            
116900                             SEND-KVDLEN                                  
117000                             SEND-RAD-STYRTECKEN                          
117100         IF SEND-KDRC > ZERO                                              
117200           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
117300           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
117400           DELIMITED BY SIZE INTO FELTEXT-STR                             
117500           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
117600         END-IF                                                           
117700       END-IF                                                             
117800     END-IF                                                               
117900     .                                                                    
118000     EJECT                                                                
118100*****************IMS-SECTIONER*********************                       
118200                                                                          
118300 IMS-GU-WDE101  SECTION.                                                  
118400                                                                          
118500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
118600          DELIMITED BY SIZE INTO SSA1                                     
118700     MOVE '  GE' TO GOOD-STATUSCODES                                      
118800     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
118900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSCHECK                                              
119100     .                                                                    
119200     EJECT                                                                
119300 IMS-GNP-WDE111 SECTION.                                                  
119400                                                                          
119500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
119800                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
119900          DELIMITED BY SIZE INTO SSA2                                     
120000     MOVE '  GE' TO GOOD-STATUSCODES                                      
120100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
120200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
120300     PERFORM IMS-STATUSCHECK                                              
120400     .                                                                    
120500     EJECT                                                                
120600 IMS-GNP-WDE121  SECTION.                                                 
120700                                                                          
120800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
120900          DELIMITED BY SIZE INTO SSA1                                     
121000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
121100          DELIMITED BY SIZE INTO SSA2                                     
121200     MOVE 'WDE121  '          TO SSA3                                     
121300     MOVE '  GE' TO GOOD-STATUSCODES                                      
121400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
121500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
121600     PERFORM IMS-STATUSCHECK                                              
121700     .                                                                    
121800     EJECT                                                                
121900 IMS-GNP-WDE131  SECTION.                                                 
122000                                                                          
122100     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
122200          DELIMITED BY SIZE INTO SSA1                                     
122300     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
122400          DELIMITED BY SIZE INTO SSA2                                     
122500     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
122600          DELIMITED BY SIZE INTO SSA3                                     
122700     MOVE 'WDE131  '          TO SSA4                                     
122800     MOVE '  GE' TO GOOD-STATUSCODES                                      
122900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
123000                                                   SSA3 SSA4              
123100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSCHECK                                              
123300     .                                                                    
123400     EJECT                                                                
123500 IMS-GU-WDB201 SECTION.                                                   
123600                                                                          
123700     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GE' TO GOOD-STATUSCODES                                      
124000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
124100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSCHECK                                              
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-GU-WDB101 SECTION.                                                   
124600                                                                          
124700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
124800          DELIMITED BY SIZE INTO SSA1                                     
124900     MOVE '  GE' TO GOOD-STATUSCODES                                      
125000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
125100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
125200     PERFORM IMS-STATUSCHECK                                              
125300     .                                                                    
125400     EJECT                                                                
125500 IMS-GU-4747-KUL SECTION.                                                 
125600                                                                          
125700      STRING 'WDG701  (WDGXKEY  ='  WDGX01  ')'                           
125800               DELIMITED BY SIZE INTO SSA1                                
125900      MOVE 'WDG740  '         TO SSA2                                     
126000      MOVE '  GE'             TO GOOD-STATUSCODES                         
126100      CALL CBLTDLI  USING  GU  WDG7-PCB  DLI-IO-KUL  SSA1 SSA2            
126200      MOVE WDG7-STATUS-CODE   TO   STATUS-WS                              
126300      PERFORM IMS-STATUSCHECK                                             
126400     .                                                                    
126500     SKIP3                                                                
126600 IMS-STATUSCHECK SECTION.                                                 
126700                                                                          
126800     SET STATUS-IX TO 1                                                   
126900     SEARCH GOOD-STATUS                                                   
127000       AT END                                                             
127100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
127200           DELIMITED BY SIZE INTO FELTEXT                                 
127300         CALL FELLOG                                                      
127400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
127500         CONTINUE                                                         
127600     END-SEARCH                                                           
127700     .                                                                    
127800     EJECT                                                                
