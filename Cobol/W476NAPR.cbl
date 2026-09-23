000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W476NAPR.                                                
000400 AUTHOR.         STINA MOGREN.                                            
000500 DATE-WRITTEN.   04/09/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PGM:ET SKRIVER FÖR NA-NDC:ERNA;                                  
001100*        - PROFORMA FAKTURAN FÖR NDC:ERNA                                 
001200*        - PROFORMA FAKTURANS SIDA 1 PÅ FIL SOM SKICKAS VIA VCOM          
001300*                                                                         
001400*                                                                         
001500*        PGM:ET ANVÄNDER:                                                 
001600*            ALT-PCB     ANVÄNDS AV W006PRR1 (PCB FÖR PRINTER)            
001700*            LISB-PCB    ANVÄNDS AV W006PRR1 FÖR LISTÅTERSTART            
001800*            WDE1                                                         
001900*            WDB2                                                         
002000*            WDK7                                                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP2                                                                
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W476NAPR'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  TAB-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  MAX-IX                      PIC S9(9)   VALUE +100 COMP SYNC.        
004900 77  SHIP-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  WS-SIDNR                    PIC S9(3)   VALUE +0   COMP-3.           
005100 77  WS-RAD-RAEKNARE             PIC S9(3)   VALUE +0   COMP-3.           
005200 77  WS-POST-RAEKNARE            PIC S9(5)   VALUE +0   COMP-3.           
005300 77  WS-ANTAL-RADER              PIC S9(3)   VALUE +0   COMP-3.           
005400 77  WS-VKORDBTO                 PIC S9(8)V9(1) VALUE 0 COMP-3.           
005500 77  WS-VKARTNTO-G               PIC S9(4)V9(3) VALUE 0 COMP-3.           
005600 77  WS-VLORDBTO                 PIC S9(6)V9(3) VALUE 0 COMP-3.           
005700 77  WS-PRAVCOST-TOT             PIC S9(7)V9(2) VALUE 0 COMP-3.           
005800 77  MAX-RADER                   PIC S9(3)   VALUE +40  COMP-3.           
005900 77  W-KDSPRAK                   PIC S9      COMP-3.                      
006000 77  WS-STRECK                   PIC X       VALUE '-'.                   
006100                                                                          
006200                                                                          
       01  WS-META                       PIC X(5) VALUE '¤META'.                
       01  WS-IDDISTR                    PIC Z(4)9.                             
       01  WS-IDSHIPM-Z                  PIC Z(6)9.                             
       01  WS-YYMMDD                     PIC 9(6).                              
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
                                                                                
006300 01  WX-IDDISTR                  PIC 9(4).                                
006400 01  FILLER                      REDEFINES WX-IDDISTR.                    
006500     03  FILLER                  PIC X(2).                                
006600     03  WX-IDDC                 PIC X(2).                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000*                                                                         
007100 77  VCOM-SW                     PIC X       VALUE 'J'.                   
007200 77  RAD-SW                      PIC X       VALUE 'J'.                   
007300                                                                          
007400 77  WEB-OUTPUT-SW               PIC X       VALUE 'N'.                   
007500     88 WEB-OUTPUT                           VALUE 'J'.                   
007600                                                                          
007700*                                                                         
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDC99                                                       
008100       EJECT                                                              
008200*                                                                         
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800                                                                          
008900 01  WS-LIST-AREA.                                                        
009000     03  WS-DUMMY                PIC X.                                   
009100     03  WS-IDPRTLST             PIC X(8)    VALUE SPACE.                 
009200     03  WS-LISTRAD.                                                      
009300       05  FILLER                PIC X(4)    VALUE SPACE.                 
009400       05  LIST-RAD              PIC X(113).                              
009500                                                                          
009600 01   W-IDPRTLST                 PIC X(8)    VALUE SPACE.                 
009700     EJECT                                                                
009800 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
009900     SKIP3                                                                
010000*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
010100     EJECT                                                                
010200*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
010300     EJECT                                                                
010400*01  FILLER   -COPY WWDIS134    -RED TEST-IDDISTR.                        
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16) VALUE 'DIST-DC-TAB'.           
010700*    -COPY WWDIST57                                                       
010800     EJECT                                                                
010900 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
011000 01  FILLER -COPY WWBYT19       -RED TEST-IDARTNR                         
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL COPYTEXT WNDCADRE                                
011300*01  -COPY WNDCADRE                                                       
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL COPYTEXT WWOMVAND                                
011600*01  -COPY WWOMVAND                                                       
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL COPYTEXT W400ARTU                                
011900*01  -COPY W400ARTU                                                       
012000     EJECT                                                                
012100 01  DYNAMISKA-SUBPROGRAM.                                                
012200*                                                                         
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012600     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
012700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
012800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
012900     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
013000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013100     EJECT                                                                
013200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013500     SKIP2                                                                
013600 77  KDRC-DISPLAY                PIC Z(5).                                
013700     EJECT                                                                
013800 01  FILLER                     PIC X(16)  VALUE  'LEDTEXT TABLE'.        
013900*01 -COPY W475W552                                                        
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE  'SEND-AREA'.          
014200 01  SEND-AREA.                                                           
014300*    03  -COPY WZ01SEND                                                   
014400                                                                          
014500 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
014600 01  WS-SKIP1                      PIC X      VALUE ' '.                  
014700 01  WS-SKIP2                      PIC X      VALUE '0'.                  
014800 01  WS-SKIP3                      PIC X      VALUE '-'.                  
014900                                                                          
015000 01  SEND-RAD-STYRTECKEN.                                                 
015100     03  STYRTECKEN-RAD          PIC X.                                   
015200     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
015300                                                                          
015400*    --- PARAMETRAR TILL POSTSUM                                          
015500*                                                                         
015600*01  -COPY W0005   -PRE  POSTSUM-                                         
015700     EJECT                                                                
015800*                                                                         
015900*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
016000*                                                                         
016100*01  FILLER   -COPY W006PRAR                                              
016200     EJECT                                                                
016300                                                                          
016400 01  PRT-AREA.                                                            
016500     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
016600     03 WS-PRT-IDLIST.                                                    
016700        05 WS-IDLIST             PIC X(4)  VALUE 'PROF'.                  
016800        05 WS-PRT-IDDISTR        PIC 9(4).                                
016900        05 FILLER                PIC X(2).                                
017000     03 WS-PRT-LISTRAD.                                                   
017100        05 WS-RAD                PIC X(115).                              
017200     03 WS-PRT-DUMMY             PIC X(1).                                
017300                                                                          
017400 01  FILLER                      PIC X(20)   VALUE SPACES.                
017500 01  SPEC-RAD.                                                            
017600     03  SPEC-STYR               PIC S9(3)   VALUE +0   COMP-3.           
017700     03  SPEC-RADDA              PIC X(120).                              
017800                                                                          
017900 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
018000*    --- FAKTURA-TABELL                                                   
018100 01  FAKTURA-DATA-TABELL.                                                 
018200    03   FAKTURA-DATA OCCURS 100.                                         
018300      05  TAB-IDSHIPM                PIC 9(7).                            
018400      05  TAB-IDFAKT                 PIC S9(7).                           
018500      05  TAB-IDDISTR                PIC S9(5).                           
018600      05  TAB-IDDC                   PIC X(2).                            
018700      05  TAB-ANTAL-KOLLI            PIC S9(3).                           
018800      05  TAB-VKORDBTO-KOLLI         PIC S9(8)V9(1).                      
018900      05  TAB-VLORDBTO-KOLLI         PIC S9(6)V9(3).                      
019000      05  TAB-PRAVCOST-KOLLI         PIC S9(7)V9(2).                      
019100      05  TAB-ANTAL-RADER            PIC S9(3).                           
019200     EJECT                                                                
019300 01  FILLER                       PIC X(10)  VALUE 'SPAR-AREA '.          
019400 01  SPAR-AREA.                                                           
019500     03 SPAR-IDFAKT                 PIC S9(7)   VALUE ZERO.               
019600     03 SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.              
019700     03 SPAR-IDSHIPM                PIC 9(7)    VALUE ZERO.               
019800                                                                          
019900 01  FILLER                    PIC X(16)  VALUE 'LIST-RADER'.             
020000                                                                          
020100 01  RAD1.                                                                
020200     03  FILLER                    PIC X(30).                             
020300* BET-ADBETRAD-2 och DCS-ADGMT-PADR                                       
020400     03  RAD4H-IMPORTER            PIC X(35).                             
020500     03  FILLER                    PIC X(02).                             
020600* SHIP-TISKEPPN                                                           
020700     03  RAD1-TIAAMMDD             PIC 9(06).                             
020800* SGMT-IDDISTR                                                            
020900     03  RAD1-IDDISTR              PIC Z(4)9.                             
021000     03  FILLER                    PIC X(01).                             
021100* SHIP-IDSHIPM                                                            
021200     03  RAD1-IDSHIPM              PIC Z(06)9.                            
021300     03  FILLER                    PIC X(06).                             
021400* SHIP-IDTRPTNR                                                           
021500     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
021600     03  FILLER                    PIC X(01).                             
021700* SHIP-IDLBBET                                                            
021800     03  RAD1-IDLBBET              PIC X(12).                             
021900     03  FILLER                    PIC X(03).                             
022000* WS-SIDNR                                                                
022100     03  RAD1-PAGE-NO              PIC Z(03).                             
022200                                                                          
022300* Special lösning för Canada - C - rad 4 (shipper)                        
022400 01  RAD1-C.                                                              
022500     03  FILLER                    PIC X(15).                             
022600* DCS-ADGMT-PADR                                                          
022700     03  RAD4HC-SHIPPER            PIC X(35).                             
022800     03  FILLER                    PIC X(17).                             
022900* SHIP-TISKEPPN                                                           
023000     03  RAD1C-TIAAMMDD            PIC 9(06).                             
023100* SGMT-IDDISTR                                                            
023200     03  RAD1C-IDDISTR             PIC Z(4)9.                             
023300     03  FILLER                    PIC X(01).                             
023400* SHIP-IDSHIPM                                                            
023500     03  RAD1C-IDSHIPM             PIC Z(06)9.                            
023600     03  FILLER                    PIC X(06).                             
023700* SHIP-IDTRPTNR                                                           
023800     03  RAD1C-IDTRPTNR            PIC Z(02)9.                            
023900     03  FILLER                    PIC X(01).                             
024000* SHIP-IDLBBET                                                            
024100     03  RAD1C-IDLBBET             PIC X(12).                             
024200     03  FILLER                    PIC X(03).                             
024300* WS-SIDNR                                                                
024400     03  RAD1C-PAGE-NO             PIC Z(03).                             
024500* Slut special lösning för Canada - C                                     
024600                                                                          
024700 01  RAD-HEAD.                                                            
024800     03  FILLER                    PIC X(2).                              
024900* 'SHIPPER'                                                               
025000     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
025100     03  FILLER                    PIC X(13).                             
025200     03  FILLER                    PIC X(2).                              
025300* BET-BEBETRAD-1 ELLER DCS-BEGMT-RAD1 ???                                 
025400     03  RAD1H-IMPORTER            PIC X(35).                             
025500     03  FILLER                    PIC X(2).                              
025600* 'PROFORMA INVOICE'                                                      
025700     03  RAD-TYP-IDSHIP            PIC X(50).                             
025800                                                                          
025900 01  RAD2-HEAD.                                                           
026000     03  FILLER                    PIC X(30).                             
026100* BET-BEBETRAD-2 ELLER DCS-BEGMT-RAD2 ???                                 
026200     03  RAD2H-IMPORTER            PIC X(35).                             
026300                                                                          
026400 01  RAD3-HEAD.                                                           
026500     03  FILLER                    PIC X(30).                             
026600* BET-BEBETRAD-3 ELLER DCS-BEGMT-RAD3 ???                                 
026700     03  RAD3H-IMPORTER            PIC X(35).                             
026800                                                                          
026900 01  RAD5-HEAD.                                                           
027000     03  FILLER                    PIC X(30).                             
027100* BET-BELAND-SVE ELLER DCS-ADGMT-LAND ???                                 
027200     03  RAD5H-IMPORTER            PIC X(35).                             
027300                                                                          
027400* Special lösning för Canada - C - rad 1,2,3,5 (shipper)                  
027500 01  RAD-HEAD-C.                                                          
027600     03  FILLER                    PIC X(2).                              
027700* 'SHIPPER'                                                               
027800     03  RAD1HC-SHIPP-TEXT         PIC X(8).                              
027900     03  FILLER                    PIC X.                                 
028000* DCS-BEGMT-RAD1                                                          
028100     03  RAD1HC-SHIPPER            PIC X(35).                             
028200     03  FILLER                    PIC X(21).                             
028300* 'PROFORMA INVOICE'                                                      
028400     03  RAD1HC-TYP-IDSHIP         PIC X(50).                             
028500                                                                          
028600 01  RAD2-HEAD-C.                                                         
028700     03  FILLER                    PIC X(2).                              
028800     03  FILLER                    PIC X(9).                              
028900* DCS-BEGMT-RAD2                                                          
029000     03  RAD2HC-SHIPPER            PIC X(35).                             
029100                                                                          
029200 01  RAD3-HEAD-C.                                                         
029300     03  FILLER                    PIC X(2).                              
029400     03  FILLER                    PIC X(9).                              
029500* DCS-BEGMT-GATA                                                          
029600     03  RAD3HC-SHIPPER            PIC X(35).                             
029700                                                                          
029800 01  RAD5-HEAD-C.                                                         
029900     03  FILLER                    PIC X(2).                              
030000     03  FILLER                    PIC X(9).                              
030100* DCS-ADGMT-LAND                                                          
030200     03  RAD5HC-SHIPPER            PIC X(35).                             
030300* Slut special lösning för Canada - C                                     
030400                                                                          
030500 01  RAD1X.                                                               
030600     03  FILLER                    PIC X(2).                              
030700* 'SHIP TO'                                                               
030800     03  RAD1-DEALER-TEXT          PIC X(12).                             
030900     03  FILLER                    PIC X(3).                              
031000* 'DC'                                                                    
031100     03  RAD1-IDDC-TEXT            PIC X(2).                              
031200     03  FILLER                    PIC X(1).                              
031300* SPACE FAST OM  DIST57-SOK-IDDISTR(DIST57-IX) = SGMT-IDDISTR             
031400* FLYTTAS DIST57-REFILL-TO-DC(DIST57-IX)                                  
031500     03  RAD1-TO-IDDC              PIC X(2).                              
031600     03  FILLER                    PIC X(8).                              
031700* GMT-BEGMT-RAD1                                                          
031800     03  RAD1-BEGMT-1              PIC X(35).                             
031900     03  FILLER                    PIC X(5).                              
032000* GMT-ADGMT-GATA                                                          
032100     03  RAD1-BEGMT-2              PIC X(35).                             
032200                                                                          
032300 01  RAD2X.                                                               
032400     03  FILLER                    PIC X(30).                             
032500* GMT-BEGMT-RAD2                                                          
032600     03  RAD2-BEGMT-3              PIC X(35).                             
032700     03  FILLER                    PIC X(5).                              
032800* GMT-ADGMT-PADR                                                          
032900     03  RAD2-BEGMT-4              PIC X(35).                             
033000                                                                          
033100 01  RAD3X.                                                               
033200     03  FILLER                    PIC X(70).                             
033300* GMT-ADGMT-LAND                                                          
033400     03  RAD3-BEGMT-5              PIC X(35).                             
033500                                                                          
033600* Special lösning för Canada - C - rad 1,2,3,4 (ship to...)               
033700 01  RAD1X-C.                                                             
033800     03  FILLER                    PIC X(2).                              
033900* 'SHIP TO'                                                               
034000     03  RAD1-DEALER-TEXT-C        PIC X(08).                             
034100* GMT-BEGMT-RAD1                                                          
034200     03  RAD1-BEGMT-1C             PIC X(32).                             
034300     03  FILLER                    PIC X(1).                              
034400* 'SUPPLIER'                                                              
034500     03  RAD1-SUPPL-TEXT2-C        PIC X(07).                             
034600* GMT-BGMT-RAD1C                                                          
034700     03  RAD1-BEGMT-2C             PIC X(27).                             
034800     03  FILLER                    PIC X(1).                              
034900* 'SOLD TO'                                                               
035000     03  RAD1-CANADA-TEXT2-C       PIC X(08).                             
035100* GMT-BGMT-RAD1C                                                          
035200     03  RAD1-BEGMT-3C             PIC X(28).                             
035300                                                                          
035400 01  RAD2X-C.                                                             
035500     03  FILLER                    PIC X(10).                             
035600* GMT-ADGMT-GATA                                                          
035700     03  RAD2-GATA-1C              PIC X(32).                             
035800     03  FILLER                    PIC X(8).                              
035900* GMT-ADGMT-GATA                                                          
036000     03  RAD2-GATA-2C              PIC X(27).                             
036100     03  FILLER                    PIC X(9).                              
036200* GMT-ADGMT-GATA                                                          
036300     03  RAD2-GATA-3C              PIC X(28).                             
036400                                                                          
036500 01  RAD3X-C.                                                             
036600     03  FILLER                    PIC X(10).                             
036700* GMT-ADGMT-PADR                                                          
036800     03  RAD3-PADR-1C              PIC X(32).                             
036900     03  FILLER                    PIC X(8).                              
037000* GMT-ADGMT-PADR                                                          
037100     03  RAD3-PADR-2C              PIC X(27).                             
037200     03  FILLER                    PIC X(9).                              
037300* GMT-ADGMT-PADR                                                          
037400     03  RAD3-PADR-3C              PIC X(28).                             
037500                                                                          
037600 01  RAD4X-C.                                                             
037700     03  FILLER                    PIC X(10).                             
037800* GMT-ADGMT-LAND                                                          
037900     03  RAD4-LAND-1C              PIC X(32).                             
038000     03  FILLER                    PIC X(8).                              
038100* GMT-ADGMT-PADR                                                          
038200     03  RAD4-LAND-2C              PIC X(27).                             
038300     03  FILLER                    PIC X(9).                              
038400* GMT-ADGMT-PADR                                                          
038500     03  RAD4-LAND-3C              PIC X(28).                             
038600* Slut special lösning för Canada - C                                     
038700                                                                          
038800 01  LIST-RADER.                                                          
038900                                                                          
039000     03 RAD-SIDNR.                                                        
039100       05 FILLER               PIC X(97)  VALUE SPACE.                    
039200* WS-SIDNR                                                                
039300       05 SIDNR                PIC Z(2)9.                                 
039400                                                                          
039500     03 RUBRIKRAD-4.                                                      
039600       05 FILLER               PIC X(03) VALUE SPACE.                     
039700       05 FILLER               PIC X(11) VALUE 'DC Number: '.             
039800* SHIP-IDDC                                                               
039900       05 RUB4-IDDC            PIC X(02) VALUE SPACE.                     
040000       05 FILLER               PIC X(02) VALUE SPACE.                     
040100       05 FILLER               PIC X(14) VALUE 'Freight Mode: '.          
040200       05 FILLER               PIC X(12) VALUE SPACE.                     
040300       05 FILLER               PIC X(13) VALUE 'Puls ref no: '.           
040400* SHIP-IDSHIPM                                                            
040500       05 RUB4-IDSHIPM         PIC Z(6)9.                                 
040600       05 FILLER               PIC X(02) VALUE SPACE.                     
040700       05 FILLER               PIC X(12) VALUE 'Invoice no: '.            
040800* SKOLLI-IDFAKT                                                           
040900       05 RUB4-IDFAKT          PIC Z(6)9.                                 
041000       05 FILLER               PIC X(02) VALUE SPACE.                     
041100                                                                          
041200* Special lösning för Canada - C - rad 4 - DC Number...                   
041300     03 RUBRIKRAD-4C.                                                     
041400       05 FILLER               PIC X(03) VALUE SPACE.                     
041500       05 FILLER               PIC X(11) VALUE 'DC Number: '.             
041600* SHIP-IDDC                                                               
041700       05 RUB4C-IDDC           PIC X(02) VALUE SPACE.                     
041800       05 FILLER               PIC X(02) VALUE SPACE.                     
041900       05 FILLER               PIC X(14) VALUE 'Freight Mode: '.          
042000       05 FILLER               PIC X(34) VALUE SPACE.                     
042100* SKOLLI-IDFAKT                                                           
042200       05 FILLER               PIC X(12) VALUE 'Invoice no: '.            
042300       05 RUB4C-IDFAKT         PIC Z(6)9.                                 
042400       05 FILLER               PIC X(02) VALUE SPACE.                     
042500                                                                          
042600* Slut special lösning för Canada - C                                     
042700                                                                          
042800     03 RUBRIKRAD-6.                                                      
042900       05 FILLER               PIC X(03) VALUE SPACE.                     
043000       05 FILLER               PIC X(14) VALUE 'District No.: '.          
043100* SGMT-IDDISTR                                                            
043200       05 RUB6-IDDISTR         PIC Z(3)9.                                 
043300       05 FILLER               PIC X(14) VALUE SPACE.                     
043400       05 FILLER               PIC X(13) VALUE 'Order Class: '.           
043500* SGMT-KDORDKL-MAX                                                        
043600       05 RUB6-KDORDKL         PIC 9.                                     
043700       05 FILLER               PIC X(06) VALUE SPACE.                     
043800       05 FILLER               PIC X(13) VALUE 'Total Cases: '.           
043900* TAB-ANTAL-KOLLI(TAB-IX)                                                 
044000       05 RUB6-TOT-KOLLI       PIC Z(2)9.                                 
044100                                                                          
044200* Special lösning för Canada - C - rad 6 - Dealer Code...                 
044300     03 RUBRIKRAD-6C.                                                     
044400       05 FILLER               PIC X(03) VALUE SPACE.                     
044500       05 FILLER               PIC X(14) VALUE 'Dealer Code:  '.          
044600* SGMT-IDDISTR                                                            
044700       05 RUB6C-IDKUNDNR       PIC Z(5)9.                                 
044800       05 FILLER               PIC X(12) VALUE SPACE.                     
044900       05 FILLER               PIC X(13) VALUE 'Order Class: '.           
045000* SGMT-KDORDKL-MAX                                                        
045100       05 RUB6C-KDORDKL        PIC 9.                                     
045200       05 FILLER               PIC X(06) VALUE SPACE.                     
045300       05 FILLER               PIC X(13) VALUE 'Total Cases: '.           
045400* TAB-ANTAL-KOLLI(TAB-IX)                                                 
045500       05 RUB6C-TOT-KOLLI      PIC Z(2)9.                                 
045600* Slut special lösning för Canada - C                                     
045700                                                                          
045800     03 RUBRIKRAD-7.                                                      
045900       05 FILLER               PIC X(03) VALUE SPACE.                     
046000       05 FILLER               PIC X(14) VALUE 'Gross Weight: '.          
046100* OM DIS134-BYTESREN-NA - ZERO ANNARS WS-VKORDBTO - DVS                   
046200* TAB-VKORDBTO-KOLLI (TAB-IX) * CONV-KG-TO-LB                             
046300       05 RUB7-VKORDBTO        PIC ZZ,ZZZ,ZZZ.ZZ.                         
046400       05 FILLER               PIC X(01) VALUE SPACE.                     
046500* 'LB'                                                                    
046600       05 RUB7-SORT            PIC X(2)  VALUE SPACE.                     
046700       05 FILLER               PIC X(02) VALUE SPACE.                     
046800       05 FILLER               PIC X(06) VALUE 'Vol.: '.                  
046900* OM DIS134-BYTESREN-NA - ZERO ANNARS WS-VLORDBTO - DVS                   
047000* TAB-VLORDBTO-KOLLI (TAB-IX) * CONV-M3-TO-FT3                            
047100       05 RUB7-VLORDBTO        PIC ZZZ,ZZZ.ZZZ.                           
047200       05 FILLER               PIC X(03) VALUE SPACE.                     
047300       05 FILLER               PIC X(14) VALUE 'Total. value: '.          
047400* OM DIS134-BYTESREN-NA - ZERO ANNARS TAB-PRAVCOST-KOLLI (TAB-IX)         
047500       05 RUB7-TOT-PRAVCOST    PIC ZZ,ZZZ,ZZ9.99.                         
047600       05 FILLER               PIC X(01) VALUE SPACE.                     
047700* 'USD'                                                                   
047800       05 RUB7-VALUTA          PIC X(03).                                 
047900                                                                          
048000* Special lösning för Canada - C - rad 7 - Gross weight...                
048100     03 RUBRIKRAD-7C.                                                     
048200       05 FILLER               PIC X(03) VALUE SPACE.                     
048300       05 FILLER               PIC X(14) VALUE 'Gross Weight: '.          
048400* TAB-VKORDBTO-KOLLI (TAB-IX) * CONV-KG-TO-LB                             
048500       05 RUB7C-VKORDBTO       PIC ZZ,ZZZ,ZZZ.ZZ.                         
048600       05 FILLER               PIC X(01) VALUE SPACE.                     
048700* 'LB'                                                                    
048800       05 RUB7C-SORT           PIC X(2)  VALUE SPACE.                     
048900       05 FILLER               PIC X(02) VALUE SPACE.                     
049000       05 FILLER               PIC X(06) VALUE 'Vol.: '.                  
049100* TAB-VLORDBTO-KOLLI (TAB-IX) * CONV-M3-TO-FT3                            
049200       05 RUB7C-VLORDBTO       PIC ZZZ,ZZZ.ZZZ.                           
049300       05 FILLER               PIC X(03) VALUE SPACE.                     
049400       05 FILLER               PIC X(14) VALUE 'Total. value: '.          
049500* TAB-PRAVCOST-KOLLI (TAB-IX)                                             
049600       05 RUB7C-TOT-PRAVCOST   PIC ZZ,ZZZ,ZZ9.99.                         
049700       05 FILLER               PIC X(01) VALUE SPACE.                     
049800* 'USD'                                                                   
049900       05 RUB7C-VALUTA         PIC X(03).                                 
050000* Slut special lösning för Canada - C                                     
050100                                                                          
050200     03 RUBRIKLINJE.                                                      
050300       05 FILLER               PIC X(03) VALUE SPACE.                     
050400       05 FILLER               PIC X(112) VALUE ALL '_'.                  
050500                                                                          
050600     03 DETALJRUB.                                                        
050700       05 FILLER               PIC X(06)  VALUE SPACE.                    
050800       05 FILLER               PIC X(08)  VALUE 'Part No.'.               
050900       05 FILLER               PIC X(01)  VALUE SPACE.                    
051000       05 FILLER               PIC X(12)  VALUE 'Part Name   '.           
051100       05 FILLER               PIC X(02)  VALUE SPACE.                    
051200       05 FILLER               PIC X(11)  VALUE ' Tariff no '.            
051300       05 FILLER               PIC X(06)  VALUE SPACE.                    
051400       05 FILLER               PIC X(03)  VALUE 'Qty'.                    
051500       05 FILLER               PIC X(03)  VALUE SPACE.                    
051600       05 FILLER               PIC X(10)  VALUE 'Unit Price'.             
051700       05 FILLER               PIC X(05)  VALUE SPACE.                    
051800       05 FILLER               PIC X(09)  VALUE 'Ext.Price'.              
051900       05 FILLER               PIC X(01)  VALUE SPACE.                    
052000       05 FILLER               PIC X(17)                                  
052100                               VALUE 'Country of origin'.                 
052200       05 FILLER               PIC X(01)  VALUE SPACE.                    
052300       05 FILLER               PIC X(05)  VALUE 'Order'.                  
052400       05 FILLER               PIC X(02)  VALUE SPACE.                    
052500       05 FILLER               PIC X(04)  VALUE 'Case'.                   
052600       05 FILLER               PIC X(04)  VALUE SPACE.                    
052700                                                                          
052800     03 DETALJRAD.                                                        
052900       05 FILLER               PIC X(5)   VALUE SPACE.                    
053000* SRAD-IDARTNR                                                            
053100       05 LISTRAD-IDARTNR      PIC Z(9)   VALUE SPACE.                    
053200       05 FILLER               PIC X      VALUE SPACE.                    
053300* TEXT-BEART ELLER 'CORE PART'                                            
053400       05 LISTRAD-BEART        PIC X(12).                                 
053500       05 FILLER               PIC X(5)   VALUE SPACE.                    
053600* ZERO                                                                    
053700       05 LISTRAD-VKARTNTO-G   PIC Z(8).                                  
053800       05 FILLER               PIC X(2)   VALUE SPACE.                    
053900* SRAD-KVLEVART                                                           
054000       05 LISTRAD-KVLEVART     PIC Z(6)9.                                 
054100       05 FILLER               PIC X      VALUE SPACE.                    
054200* OM DIS134-BYTESREN-NA - ZERO ANNARS SLAG-PRAVCOST                       
054300       05 LISTRAD-UNIT-PRAVCOST PIC Z,ZZZ,ZZ9.99.                         
054400       05 FILLER               PIC X      VALUE SPACE.                    
054500* OM DIS134-BYTESREN-NA - ZERO ANNARS WS-PRAVCOST-TOT DVS.                
054600* SLAG-PRAVCOST * SRAD-KVLEVART                                           
054700       05 LISTRAD-PRAVCOST     PIC ZZ,ZZZ,ZZ9.99.                         
054800       05 FILLER               PIC X      VALUE SPACE.                    
054900* ARTU-BEARTURS-ENG                                                       
055000       05 LISTRAD-BEARTURS-ENG PIC X(15).                                 
055100       05 FILLER               PIC X(3)   VALUE SPACE.                    
055200* SKOLLI-IDKUNDRF(3:5)                                                    
055300       05 LISTRAD-IDKUNDRF     PIC Z(4)9.                                 
055400       05 FILLER               PIC X      VALUE SPACE.                    
055500* SKOLLI-IDKOLLI                                                          
055600       05 LISTRAD-IDKOLLI      PIC Z(4)9.                                 
055700       05 FILLER               PIC X(04)  VALUE SPACE.                    
055800                                                                          
055900* Special lösning för Canada - C - Detaljrad                              
056000* Detta gäller US-> CA                                                    
056100* men också CA -> US (nytt från mars '18)                                 
056200     03 DETALJRAD-C.                                                      
056300       05 FILLER               PIC X(4)   VALUE SPACE.                    
056400* SRAD-IDARTNR                                                            
056500       05 LISTRAD-IDARTNR-C    PIC Z(08).                                 
056600       05 LISTRAD-STRECK-C     PIC X      VALUE SPACE.                    
056700       05 LISTRAD-KONTROLL-C   PIC X      VALUE SPACE.                    
056800       05 FILLER               PIC X      VALUE SPACE.                    
056900* TEXT-BEART                                                              
057000       05 LISTRAD-BEART-C      PIC X(12).                                 
057100       05 FILLER               PIC X(5)   VALUE SPACE.                    
057200* ZERO                                                                    
057300       05 LISTRAD-VKARTNTO-G-C PIC Z(8).                                  
057400       05 FILLER               PIC X(2)   VALUE SPACE.                    
057500* SRAD-KVLEVART                                                           
057600       05 LISTRAD-KVLEVART-C   PIC Z(6)9.                                 
057700       05 FILLER               PIC X      VALUE SPACE.                    
057800* OM DIS134-BYTESREN-NA - ZERO ANNARS SLAG-PRAVCOST                       
057900       05 LISTRAD-UNIT-PRAVCOST-C                                         
058000                                PIC Z,ZZZ,ZZ9.99.                         
058100       05 FILLER               PIC X      VALUE SPACE.                    
058200* OM DIS134-BYTESREN-NA - ZERO ANNARS WS-PRAVCOST-TOT DVS.                
058300* SLAG-PRAVCOST * SRAD-KVLEVART                                           
058400       05 LISTRAD-PRAVCOST-C   PIC ZZ,ZZZ,ZZ9.99.                         
058500       05 FILLER               PIC X      VALUE SPACE.                    
058600* ARTU-BEARTURS-ENG                                                       
058700       05 LISTRAD-BEARTURS-ENG-C                                          
058800                               PIC X(15).                                 
058900       05 FILLER               PIC X(3)   VALUE SPACE.                    
059000* SKOLLI-IDKUNDRF(3:5)                                                    
059100       05 LISTRAD-IDKUNDRF-C   PIC Z(4)9.                                 
059200       05 FILLER               PIC X      VALUE SPACE.                    
059300* SKOLLI-IDKOLLI                                                          
059400       05 LISTRAD-IDKOLLI-C    PIC Z(4)9.                                 
059500       05 FILLER               PIC X(04)  VALUE SPACE.                    
059600* Slut special lösning för Canada - C                                     
059700                                                                          
059800     03 FINALRAD1.                                                        
059900       05 FILLER               PIC X(03) VALUE SPACE.                     
060000       05 FILLER               PIC X(77) VALUE ALL '_'.                   
060100                                                                          
060200     03 FINALRAD2.                                                        
060300       05 FILLER               PIC X(03) VALUE SPACE.                     
060400       05 FILLER               PIC X(29) VALUE                            
060500                               'Shippers Signature and Title '.           
060600*                                                                         
060700     03 TEXTRAD1.                                                         
060800       05 FILLER               PIC X(10) VALUE SPACE.                     
060900       05 FILLER               PIC X(33) VALUE                            
061000          'VOLVO CAR USA,LLC                '.                            
061100       05 FILLER               PIC X(5)  VALUE SPACE.                     
061200* SHIPPER-CITY (SHIP-INDX)                                                
061300       05 TEXTRAD1-ADR         PIC X(30).                                 
061400                                                                          
061500     03 TEXTRAD2.                                                         
061600       05 FILLER               PIC X(48) VALUE SPACE.                     
061700       05 FILLER               PIC X(11) VALUE 'TELEPHONE: '.             
061800* SHIPPER-TEL (SHIP-INDX)                                                 
061900       05 TEXTRAD2-TEL         PIC X(13) VALUE SPACE.                     
062000                                                                          
062100     03 TEXTRAD3.                                                         
062200       05 FILLER               PIC X(48) VALUE SPACE.                     
062300       05 FILLER               PIC X(11) VALUE 'TELEFAX  : '.             
062400* SHIPPER-TELEFAX (SHIP-INDX)                                             
062500       05 TEXTRAD3-TELEFAX     PIC X(13) VALUE SPACE.                     
062600                                                                          
062700     03 TEXTRAD4.                                                         
062800       05 FILLER               PIC X(10) VALUE SPACE.                     
062900       05 FILLER               PIC X(35) VALUE                            
063000          'TO: VOLVO CARS OF CANADA CORP FROM:'.                          
063100* SHIPPER-COMPANY (SHIP-INDX)                                             
063200       05 TEXTRAD4-NAMN        PIC X(32).                                 
063300                                                                          
063400     03 TEXTRAD5.                                                         
063500       05 FILLER               PIC X(10) VALUE SPACE.                     
063600       05 FILLER               PIC X(35) VALUE                            
063700          '    9130 LESLIE STREET, SUITE 101  '.                          
063800* SHIPPER-STREET (SHIP-INDX)                                              
063900       05 TEXTRAD5-GATA        PIC X(32).                                 
064000                                                                          
064100     03 TEXTRAD6.                                                         
064200       05 FILLER               PIC X(10) VALUE SPACE.                     
064300       05 FILLER               PIC X(35) VALUE                            
064400          '    RICHMOND HILL, ON L4B0B9       '.                          
064500* SHIPPER-CITY (SHIP-INDX)                                                
064600       05 TEXTRAD6-ADR         PIC X(32).                                 
064700                                                                          
064800     03 TEXTRAD7.                                                         
064900       05 FILLER               PIC X(10) VALUE SPACE.                     
065000       05 FILLER               PIC X(35) VALUE                            
065100          'COUNTRY OF TRANSPORT:        USA   '.                          
065200                                                                          
065300     03 TEXTRAD8.                                                         
065400       05 FILLER               PIC X(10) VALUE SPACE.                     
065500       05 FILLER               PIC X(50) VALUE                            
065600          'SHIP VIA:                          TRANS SPEED    '.           
065700                                                                          
065800     03 TEXTRAD9.                                                         
065900       05 FILLER               PIC X(10) VALUE SPACE.                     
066000       05 FILLER               PIC X(32) VALUE                            
066100          'THE PRICES ARE OUT OF BOND AND I'.                             
066200       05 FILLER               PIC X(18) VALUE                            
066300          'NCLUDE U.S. DUTIES'.                                           
066400                                                                          
066500     03 TEXTRAD10.                                                        
066600       05 FILLER               PIC X(10) VALUE SPACE.                     
066700       05 FILLER               PIC X(50) VALUE                            
066800          'IN BOND TO ONTARIO. UPON ARRIVAL CONTACT:         '.           
066900                                                                          
067000     03 TEXTRAD11.                                                        
067100       05 FILLER               PIC X(22) VALUE SPACE.                     
067200       05 FILLER               PIC X(28) VALUE                            
067300          'UPS SCS Inc                 '.                                 
067400                                                                          
067500     03 TEXTRAD12.                                                        
067600       05 FILLER               PIC X(22) VALUE SPACE.                     
067700       05 FILLER               PIC X(28) VALUE                            
067800          '1930 Derry Road             '.                                 
067900                                                                          
068000     03 TEXTRAD13.                                                        
068100       05 FILLER               PIC X(22) VALUE SPACE.                     
068200       05 FILLER               PIC X(28) VALUE                            
068300          'Mississauga, ON L5S 1E2     '.                                 
068400                                                                          
068500     03 TEXTRAD14.                                                        
068600       05 FILLER               PIC X(22) VALUE SPACE.                     
068700       05 FILLER               PIC X(28) VALUE                            
068800          'Canada                      '.                                 
068900                                                                          
069000     03 TEXTRAD15.                                                        
069100       05 FILLER               PIC X(10) VALUE SPACE.                     
069200       05 FILLER               PIC X(52) VALUE                            
069300          'ORIGINATOR:_________________________________________'.         
069400                                                                          
069500     03 TEXTRAD16.                                                        
069600       05 FILLER               PIC X(22) VALUE SPACE.                     
069700* SHIPPER-COMPANY (SHIP-INDX)                                             
069800       05 TEXTRAD16-NAMN       PIC X(30).                                 
069900                                                                          
070000     03 TEXTRAD17.                                                        
070100       05 FILLER               PIC X(22) VALUE SPACE.                     
070200* SHIPPER-CITY (SHIP-INDX)                                                
070300       05 TEXTRAD17-ADR        PIC X(30).                                 
070400*                                                                         
070500* Special lösning för Canada - C - Textrader                              
070600     03 TEXTRAD1-C.                                                       
070700       05 FILLER               PIC X(10) VALUE SPACE.                     
070800       05 FILLER               PIC X(30) VALUE                            
070900          'THESE COMMODITIES, TECHNOLOGY '.                               
071000       05 FILLER               PIC X(30) VALUE                            
071100          'OR SOFTWARE WERE EXPORTED FROM'.                               
071200                                                                          
071300     03 TEXTRAD2-C.                                                       
071400       05 FILLER               PIC X(11) VALUE SPACE.                     
071500       05 FILLER               PIC X(32) VALUE                            
071600          'THE UNITED STATES IN ACCORDANCE '.                             
071700       05 FILLER               PIC X(30) VALUE                            
071800          'WITH THE EXPORT ADMINISTRATION'.                               
071900                                                                          
072000     03 TEXTRAD3-C.                                                       
072100       05 FILLER               PIC X(13) VALUE SPACE.                     
072200       05 FILLER               PIC X(32) VALUE                            
072300          'REGULATIONS. DIVERSION CONTRARY '.                             
072400       05 FILLER               PIC X(25) VALUE                            
072500          'TO US LAW PROHIBITED.    '.                                    
072600* Slut special lösning för Canada - C                                     
072700                                                                          
072800                                                                          
072900     EJECT                                                                
073000 01  FILLER                    PIC X(16)  VALUE 'WEB-COPYTEXTS'.          
073100                                                                          
073200*01  -COPY  W476NAPH                                                      
073300                                                                          
073400*01  -COPY  W476NAPS                                                      
073500                                                                          
073600*01  -COPY  W476NAPX                                                      
073700                                                                          
073800*01  -COPY  W476NAPL                                                      
073900                                                                          
074000*01  -COPY  W476NAPT                                                      
074100                                                                          
074200 01  FILLER                    PIC X(16)   VALUE 'WEB-DATA-AREA'.         
074300 01  WEB-DATA-AREA             PIC X(1000) VALUE SPACE.                   
074400                                                                          
074500     EJECT                                                                
074600 01  FILLER                     PIC X(16)   VALUE 'IMS-WS'.               
074700                                                                          
074800 01  KEYS-TO-DLI.                                                         
074900     03  W-WDB101KY-X.                                                    
075000        05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                  
075100        05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                   
075200                                                                          
075300     03  W-WDB201KY-X.                                                    
075400        05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
075500        05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
075600                                                                          
075700     03  W-IDSHIPM-X.                                                     
075800        05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                   
075900                                                                          
076000     03  W-WDE111KY-X.                                                    
076100        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
076200        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
076300                                                                          
076400     03  W-WDE111KY-D.                                                    
076500        05  W-WDE111-IDDISTR-D  PIC S9(05)  VALUE ZERO COMP-3.            
076600        05  W-WDE111-IDKUNDNR-D PIC S9(07)  VALUE ZERO COMP-3.            
076700                                                                          
076800     03  W-WDE111KY-MIN.                                                  
076900        05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.           
077000        05  FILLER               PIC X(04)   VALUE LOW-VALUES.            
077100                                                                          
077200     03  W-WDE111KY-MAX.                                                  
077300        05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.           
077400        05  FILLER               PIC X(04)   VALUE HIGH-VALUES.           
077500                                                                          
077600     03  W-WDE121KY-X.                                                    
077700        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
077800        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
077900                                                                          
078000     03   W-IDARTNR-X.                                                    
078100        05  W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
078200     03   W-IDDC-X.                                                       
078300        05  W-IDDC               PIC X(2)    VALUE SPACE.                 
078400     03  W-IDSKYLT-X.                                                     
078500        05  W-IDSKYLT            PIC X(3)    VALUE 'USA'.                 
078600     03  W-IDDC-WDB6-X.                                                   
078700        05  W-IDDC-WDB6          PIC X(2)    VALUE SPACE.                 
078800                                                                          
078900*    --- STATUS-KOD FRÅN IMS                                              
079000 01  STATUS-WS                   PIC XX.                                  
079100     88  SEGMENT-FINNS                       VALUE '  '.                  
079200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
079300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
079400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
079500     88  IMS-EJ-OK                           VALUE 'XD'.                  
079600     SKIP2                                                                
079700 01  GOOD-STATUSCODES.                                                    
079800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
079900     SKIP3                                                                
080000 01  SSA1                        PIC X(64).                               
080100 01  SSA2                        PIC X(64).                               
080200 01  SSA3                        PIC X(64).                               
080300 01  SSA4                        PIC X(64).                               
080400     EJECT                                                                
080500*    --- IMS FUNKTIONSKODER                                               
080600*01  -COPY W0003                                                          
080700     EJECT                                                                
080800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
080900 01  DLI-IO-WDE101.                                                       
081000*    03  -COPY WDE101                                                     
081100                                                                          
081200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
081300 01  DLI-IO-WDE111.                                                       
081400*    03  -COPY WDE111                                                     
081500                                                                          
081600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
081700 01  DLI-IO-WDE121.                                                       
081800*    03  -COPY WDE121                                                     
081900                                                                          
082000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
082100 01  DLI-IO-WDE131.                                                       
082200*    03  -COPY WDE131                                                     
082300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
082400 01  DLI-IO-WDB201.                                                       
082500*    03  -COPY WDB201                                                     
082600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
082700 01  DLI-IO-WDB101.                                                       
082800*    03  -COPY WDB101                                                     
082900                                                                          
083000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
083100 01  DLI-IO-WDK711.                                                       
083200*    03  -COPY WDK711                                                     
083300                                                                          
083400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
083500 01  DLI-IO-WDD311.                                                       
083600*    03  -COPY WDD311                                                     
083700                                                                          
083800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR701'.                      
083900 01  DLI-IO-WDR701.                                                       
084000*    03  -COPY WDR701                                                     
084100     EJECT                                                                
084200                                                                          
084300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
084400 01  DLI-IO-WDB601.                                                       
084500*    03  -COPY WDB601                                                     
084600     EJECT                                                                
084700                                                                          
084800 LINKAGE SECTION.                                                         
084900*01  -COPY W476TRPD                                                       
085000                                                                          
085100*01  -COPY W0009   -PRE MSG-                                              
085200     EJECT                                                                
085300*01  -COPY W0009   -PRE ALT-                                              
085400     EJECT                                                                
085500*01  -COPY W0008  -PRE WDE1-                                              
085600     05  FILLER                  PIC X.                                   
085700*01  -COPY W0008  -PRE WDB2-                                              
085800     05  FILLER                  PIC X.                                   
085900*01  -COPY W0008  -PRE WDB1-                                              
086000     05  FILLER                  PIC X.                                   
086100*01  -COPY W0008  -PRE WDK7-                                              
086200     05  FILLER                  PIC X.                                   
086300*01  -COPY W0008  -PRE WDD3-                                              
086400     05  FILLER                  PIC X.                                   
086500*01  -COPY W0008  -PRE WDR7-                                              
086600     05  FILLER                  PIC X.                                   
086700*01  -COPY W0008  -PRE WDB6-                                              
086800     05  FILLER                  PIC X.                                   
086900*01  -COPY W0008  -PRE LISB-                                              
087000     05  FILLER                  PIC X.                                   
087100     EJECT                                                                
087200 PROCEDURE DIVISION  USING TRPD-W476TRPD                                  
087300*                          MSG-PCB                                        
087400                           ALT-PCB                                        
087500                           WDE1-PCB                                       
087600                           WDB2-PCB                                       
087700                           WDB1-PCB                                       
087800                           WDK7-PCB                                       
087900                           WDD3-PCB                                       
088000                           WDR7-PCB                                       
088100                           WDB6-PCB                                       
088200                           LISB-PCB.                                      
088300 MAIN SECTION.                                                            
088400     ENTRY 'DLITCBL' USING TRPD-W476TRPD                                  
088500*                          MSG-PCB                                        
088600                           ALT-PCB                                        
088700                           WDE1-PCB                                       
088800                           WDB2-PCB                                       
088900                           WDB1-PCB                                       
089000                           WDK7-PCB                                       
089100                           WDD3-PCB                                       
089200                           WDR7-PCB                                       
089300                           WDB6-PCB                                       
089400                           LISB-PCB.                                      
089500                                                                          
089600     PERFORM A-INIT                                                       
089700                                                                          
089800     PERFORM B-SPARA-DATA                                                 
089900                                                                          
090000     PERFORM C-INIT-ALLM                                                  
090100                                                                          
090200     PERFORM IMS-GU-WDE101                                                
090300                                                                          
090400     IF SEGMENT-FINNS                                                     
090500       PERFORM S03-INIT-PRINTER-ID                                        
090600       PERFORM IMS-GNP-WDE111                                             
090700       PERFORM UNTIL TAB-IX > MAX-IX OR SEGMENT-SAKNAS                    
090800                                                                          
090900         MOVE SGMT-IDKUNDNR   TO W-WDE111-IDKUNDNR                        
091000         PERFORM E-SKAPA-PROFORMA                                         
091100                                                                          
091200         PERFORM IMS-GNP-WDE111                                           
091300       END-PERFORM                                                        
091400                                                                          
091500     END-IF                                                               
091600                                                                          
091700     PERFORM Z-FINIT                                                      
091800                                                                          
091900     MOVE ZERO TO RETURN-CODE                                             
092000     GOBACK                                                               
092100     .                                                                    
092200     EJECT                                                                
092300 A-INIT SECTION.                                                          
092400     SKIP2                                                                
092500                                                                          
092600                                                                          
092700     MOVE +1       TO TAB-IX                                              
092800     PERFORM UNTIL TAB-IX > MAX-IX                                        
092900        MOVE ZERO  TO TAB-IDFAKT(TAB-IX)                                  
093000                      TAB-IDSHIPM(TAB-IX)                                 
093100                      TAB-IDDISTR(TAB-IX)                                 
093200                      TAB-IDDC(TAB-IX)                                    
093300                      TAB-ANTAL-KOLLI(TAB-IX)                             
093400                      TAB-VKORDBTO-KOLLI(TAB-IX)                          
093500                      TAB-VLORDBTO-KOLLI(TAB-IX)                          
093600                      TAB-PRAVCOST-KOLLI(TAB-IX)                          
093700                      TAB-ANTAL-RADER(TAB-IX)                             
093800                                                                          
093900        ADD +1     TO TAB-IX                                              
094000     END-PERFORM                                                          
094100                                                                          
094200     MOVE ZERO     TO TAB-IX                                              
                            WS-IDDISTR                                          
                            WS-IDSHIPM-Z                                        
                            WS-YYMMDD                                           
094300                                                                          
094400     ACCEPT DAGENS-DATUM      FROM DATE                                   
094500     ACCEPT FIL-TIKLOCK       FROM TIME                                   
094600     MOVE ZERO                TO FIL-IDSEKVNR                             
094700                                                                          
094800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
094900                                                                          
095000     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
095100     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
095200     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
095300     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
095400                              W-WDB201-IDDISTR                            
095500                              W-WDE111-IDDISTR-MIN                        
095600                              W-WDE111-IDDISTR-MAX                        
095700                              TEST-IDDISTR                                
095800     MOVE SPACES          TO  RAD-HEAD                                    
095900                              RAD2-HEAD                                   
096000                              RAD3-HEAD                                   
096100                              RAD5-HEAD                                   
096200                              RAD1X                                       
096300                              RAD2X                                       
096400                              RAD3X                                       
096500                              RAD-HEAD-C                                  
096600                              RAD2-HEAD-C                                 
096700                              RAD3-HEAD-C                                 
096800                              RAD5-HEAD-C                                 
096900                              RAD1X-C                                     
097000                              RAD2X-C                                     
097100                              RAD3X-C                                     
097200                                                                          
097300     IF TRPD-FLLDCKND = JA OR YES                                         
097400       MOVE JA             TO WEB-OUTPUT-SW                               
097500     ELSE                                                                 
097600       MOVE NEJ            TO WEB-OUTPUT-SW                               
097700     END-IF                                                               
097800     .                                                                    
097900     EJECT                                                                
098000 B-SPARA-DATA SECTION.                                                    
098100                                                                          
098200     PERFORM IMS-GU-WDE101                                                
098300     MOVE SHIP-IDDC         TO W-IDDC-WDB6                                
098400                                                                          
098500     PERFORM IMS-GNP-WDE111                                               
098600     PERFORM UNTIL SEGMENT-SAKNAS                                         
098700                                                                          
098800       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
098900       MOVE W-IDSHIPM         TO SPAR-IDSHIPM                             
099000                                                                          
099100       ADD +1                 TO TAB-IX                                   
099200       MOVE SHIP-IDSHIPM      TO TAB-IDSHIPM (TAB-IX)                     
099300       MOVE SGMT-IDDISTR      TO TAB-IDDISTR (TAB-IX)                     
099400       MOVE SHIP-IDDC         TO TAB-IDDC    (TAB-IX)                     
099500                                                                          
099600       PERFORM IMS-GNP-WDE121                                             
099700       PERFORM UNTIL SEGMENT-SAKNAS                                       
099800         MOVE SKOLLI-IDPRODNR      TO W-WDE121-IDPRODNR                   
099900         MOVE SKOLLI-IDKOLLI       TO W-WDE121-IDKOLLI                    
100000         ADD +1                    TO TAB-ANTAL-KOLLI (TAB-IX)            
100100         ADD SKOLLI-VKORDBTO-KOLLI TO TAB-VKORDBTO-KOLLI (TAB-IX)         
100200         ADD SKOLLI-VLORDBTO-KOLLI TO TAB-VLORDBTO-KOLLI (TAB-IX)         
100300                                                                          
100400         PERFORM IMS-GNP-WDE131                                           
100500         PERFORM UNTIL SEGMENT-SAKNAS                                     
100600           MOVE SRAD-IDARTNR    TO W-IDARTNR                              
100700           MOVE SHIP-IDDC       TO W-IDDC                                 
100800           PERFORM IMS-GU-WDK711                                          
100900           IF SEGMENT-SAKNAS                                              
101000             MOVE ZERO          TO SLAG-PRAVCOST                          
101100           END-IF                                                         
101200                                                                          
101300           ADD +1               TO TAB-ANTAL-RADER (TAB-IX)               
101400           COMPUTE WS-PRAVCOST-TOT  =  SLAG-PRAVCOST *                    
101500                                       SRAD-KVLEVART                      
101600           ADD WS-PRAVCOST-TOT  TO TAB-PRAVCOST-KOLLI (TAB-IX)            
101700                                                                          
101800           PERFORM IMS-GNP-WDE131                                         
101900         END-PERFORM                                                      
102000                                                                          
102100         PERFORM IMS-GNP-WDE121                                           
102200       END-PERFORM                                                        
102300                                                                          
102400       PERFORM IMS-GNP-WDE111                                             
102500     END-PERFORM                                                          
102600                                                                          
102700     .                                                                    
102800     EJECT                                                                
102900 C-INIT-ALLM SECTION.                                                     
103000                                                                          
103100                                                                          
103200     MOVE ZERO  TO WS-SIDNR                                               
103300                                                                          
103400     MOVE +1    TO TAB-IX                                                 
103500                                                                          
103600     MOVE +45   TO WS-RAD-RAEKNARE                                        
103700                                                                          
103800     MOVE SPACE TO SPAR-IDKUNDRF                                          
103900     .                                                                    
104000     EJECT                                                                
104100 E-SKAPA-PROFORMA SECTION.                                                
104200                                                                          
104300                                                                          
104400     IF SEGMENT-FINNS                                                     
104500       PERFORM S03-INIT-PRINTER-ID                                        
104600     END-IF                                                               
104700                                                                          
104800     PERFORM IMS-GNP-WDE121                                               
104900     MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                         
105000     MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                          
105100     PERFORM UNTIL SEGMENT-SAKNAS                                         
105200       PERFORM IMS-GNP-WDE131                                             
105300       PERFORM UNTIL SEGMENT-SAKNAS                                       
105400         PERFORM EA-SKRIV-PROFORMA-INVOICE                                
105500         PERFORM IMS-GNP-WDE131                                           
105600       END-PERFORM                                                        
105700       PERFORM IMS-GNP-WDE121                                             
105800       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
105900       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
106000     END-PERFORM                                                          
106100                                                                          
106200     .                                                                    
106300     EJECT                                                                
106400 EA-SKRIV-PROFORMA-INVOICE SECTION.                                       
106500                                                                          
106600     MOVE JA                 TO VCOM-SW                                   
106700     MOVE SGMT-IDDC          TO WS-IDDC                                   
106800                                W-IDDC-WDB6                               
106900     MOVE SGMT-IDDISTR       TO TEST-IDDISTR                              
107000     IF DIST35-NA-CDC-RETURN  OR                                          
107100        DIST35-NA-NDC-RETURNS OR                                          
107200        DIST35-NA-TRANSFER    OR                                          
107300        DIST35-REFILL-INOM-NA OR                                          
107400        DIST18-SCRAP-NDC-SC   OR                                          
107500        DIS134-BYTESREN-NA                                                
107600        CONTINUE                                                          
107700     ELSE                                                                 
107800        MOVE NEJ             TO VCOM-SW                                   
107900     END-IF                                                               
108000                                                                          
108100*************************************************************             
108200*** NÄR RAD RÄKNAREN ÄR STÖRRE ÄN 40 BLIR DET EN NY SIDA ****             
108300*** DVS RAD RÄKNAREN ÄR LIKA MED 41                      ****             
108400*************************************************************             
108500*                                                                         
108600*   MOVE DATA TO BOTH PAPER AND WEB REPORTS IN THE "FLYTTA"               
108700*   SECTIONS EVEN IF ONLY ONE SET WILL BE USED                            
108800     IF WS-RAD-RAEKNARE > MAX-RADER                                       
108900                                                                          
109000       PERFORM S07-INIT-SHIP-INDX                                         
109100                                                                          
109200       IF SGMT-IDDISTR = 7674 AND NDC-US                                  
109300*        -- SPECIAL USA TO CANADA DEALER VARIANT                          
109400         PERFORM EAA1-FLYTTA-RUB-DATA-CA                                  
109500         IF WEB-OUTPUT                                                    
109600*          -- WEB REPORT (SPECIAL SUB-HEADER)                             
109700           PERFORM EAF1-SKRIV-WEB-HDR                                     
109800           PERFORM EAF3-SKRIV-WEB-SUBX                                    
109900         ELSE                                                             
110000*          -- PAPER REPORT                                                
110100           PERFORM EAB1-SKRIV-RUBRIK-CA                                   
110200         END-IF                                                           
110300       ELSE                                                               
110400*        -- NORMAL INTRA-COUNTRY OR DC-TO-DC VARIANT                      
110500         PERFORM EAA-FLYTTA-RUB-DATA                                      
110600         IF WEB-OUTPUT                                                    
110700*          -- WEB REPORT (NORMAL SUB-HEADER)                              
110800           PERFORM EAF1-SKRIV-WEB-HDR                                     
110900           PERFORM EAF2-SKRIV-WEB-SUBH                                    
111000         ELSE                                                             
111100*          -- PAPER REPORT                                                
111200           PERFORM EAB-SKRIV-RUBRIK                                       
111300         END-IF                                                           
111400       END-IF                                                             
111500                                                                          
111600       IF WS-SIDNR > +1                                                   
111700         MOVE NEJ      TO VCOM-SW                                         
111800       END-IF                                                             
111900       MOVE +24    TO WS-RAD-RAEKNARE                                     
112000     END-IF                                                               
112100                                                                          
112200*    -- DETAIL (PART-NUMBER) LINE                                         
112300*    -- a special DETAIL LINE for US->CA and CA->US                       
112400     ADD +1      TO WS-ANTAL-RADER                                        
112500     IF (SGMT-IDDISTR = 7674 AND NDC-US) OR                               
112600        (DIST35-US-CAN-TRANSFER)         OR                               
112700        (DIST35-US-CA-RETUR)             OR                               
112800        (DIST35-USA-CA-REFILL)           OR                               
112900        (DIST35-CAN-US-TRANSFER)         OR                               
113000        (DIST35-CA-US-RETUR)             OR                               
113100        (DIST35-CA-USA-REFILL)                                            
113200       PERFORM EAC2-FLYTTA-RAD-DATA-CA                                    
113300       IF NOT WEB-OUTPUT                                                  
113400*        -- USA TO CANADA DEALER LINE (PAPER)                             
113500*        -- USA TO CANADA DC     LINE (PAPER)                             
113600         PERFORM EAD2-SKRIV-RADER-CA                                      
113700       END-IF                                                             
113800     ELSE                                                                 
113900*    -- DETAIL LINE when not special                                      
114000       PERFORM EAC1-FLYTTA-RAD-DATA                                       
114100       IF NOT WEB-OUTPUT                                                  
114200*        -- NORMAL LINE (PAPER)                                           
114300         PERFORM EAD1-SKRIV-RADER                                         
114400       END-IF                                                             
114500     END-IF                                                               
114600     IF WEB-OUTPUT                                                        
114700*      -- WEB DETAIL LINE (ALL VARIANTS)                                  
114800       PERFORM EAF4-SKRIV-WEB-LINE                                        
114900       ADD +1                 TO WS-RAD-RAEKNARE                          
115000     END-IF                                                               
115100                                                                          
115200*    -- EXTRA "CORE PART" DETAIL LINE                                     
115300     MOVE SHIP-IDDC           TO WS-IDDC                                  
115400                                 W-IDDC-WDB6                              
115500     IF (NDC-US) AND                                                      
115600        SLAG-PRAVCOST > 0                                                 
115700       PERFORM EAC3-FLYTTA-RAD-DATA                                       
115800       IF NOT DIS134-BYTESREN-NA AND RAD-SW = JA                          
115900         IF WEB-OUTPUT                                                    
116000           PERFORM EAF4-SKRIV-WEB-LINE                                    
116100           ADD +1             TO WS-RAD-RAEKNARE                          
116200         ELSE                                                             
116300           PERFORM EAD1-SKRIV-RADER                                       
116400         END-IF                                                           
116500       END-IF                                                             
116600     END-IF                                                               
116700                                                                          
116800*    -- FINAL LINES AFTER LAST DETAIL LINE                                
116900     IF WS-ANTAL-RADER = TAB-ANTAL-RADER (TAB-IX)                         
117000       PERFORM EAE-SKRIV-SISTA-SIDAN                                      
117100       ADD +1    TO TAB-IX                                                
117200       MOVE ZERO TO WS-ANTAL-RADER                                        
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
117600 EAA-FLYTTA-RUB-DATA SECTION.                                             
117700                                                                          
117800     MOVE WS-SIDNR              TO SIDNR                                  
117900                                                                          
118000     MOVE SGMT-IDDISTR          TO W-WDB201-IDDISTR                       
118100     MOVE SGMT-IDKUNDNR         TO W-WDB201-IDKUNDNR                      
118200     PERFORM IMS-GU-WDB201                                                
118300                                                                          
118400     MOVE GMT-IDPARTNR          TO W-WDB101-IDPARTNR                      
118500     MOVE GMT-IDFTG             TO W-WDB101-IDFTG                         
118600     PERFORM IMS-GU-WDB101                                                
118700                                                                          
118800     MOVE BET-BEBETRAD-1        TO RAD1H-IMPORTER                         
118900     MOVE BET-BEBETRAD-2        TO RAD2H-IMPORTER                         
119000     MOVE BET-ADBETRAD-1        TO RAD3H-IMPORTER                         
119100     MOVE BET-ADBETRAD-2        TO RAD4H-IMPORTER                         
119200     MOVE BET-BELAND-SVE        TO RAD5H-IMPORTER                         
119300                                                                          
119400     PERFORM IMS-GU-WDB601                                                
119500     IF SEGMENT-FINNS                                                     
119600       MOVE DCS-BEGMT-RAD1      TO RAD1H-IMPORTER                         
119700       MOVE DCS-BEGMT-RAD2      TO RAD2H-IMPORTER                         
119800       MOVE DCS-ADGMT-GATA      TO RAD3H-IMPORTER                         
119900       MOVE DCS-ADGMT-PADR      TO RAD4H-IMPORTER                         
120000       MOVE DCS-ADGMT-LAND      TO RAD5H-IMPORTER                         
120100     END-IF                                                               
120200**                                                                        
120300     MOVE 'SHIPPER     '        TO RAD1H-IMPORTER-TEXT                    
120400     MOVE 'PROFORMA INVOICE '   TO RAD-TYP-IDSHIP                         
120500     MOVE 'DC'                  TO RAD1-IDDC-TEXT                         
120600     SEARCH ALL  DIST57-REFILL-DC                                         
120700       AT END                                                             
120800         MOVE SGMT-IDDISTR      TO WX-IDDISTR                             
120900         MOVE SPACE             TO RAD1-TO-IDDC                           
121000       WHEN DIST57-SOK-IDDISTR(DIST57-IX) = SGMT-IDDISTR                  
121100         MOVE DIST57-REFILL-TO-DC(DIST57-IX)                              
121200                                TO RAD1-TO-IDDC                           
121300     END-SEARCH                                                           
121400                                                                          
121500     MOVE GMT-BEGMT-RAD1        TO RAD1-BEGMT-1                           
121600     MOVE GMT-ADGMT-GATA        TO RAD1-BEGMT-2                           
121700     MOVE GMT-BEGMT-RAD2        TO RAD2-BEGMT-3                           
121800     MOVE GMT-ADGMT-PADR        TO RAD2-BEGMT-4                           
121900     MOVE GMT-ADGMT-LAND        TO RAD3-BEGMT-5                           
122000                                                                          
122100     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
122200       MOVE +2 TO W-KDSPRAK                                               
122300     ELSE                                                                 
122400       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
122500     END-IF                                                               
122600                                                                          
122700     MOVE SHIP-IDDC             TO RUB4-IDDC                              
122800     MOVE SHIP-IDSHIPM          TO RUB4-IDSHIPM                           
122900     MOVE SKOLLI-IDFAKT         TO RUB4-IDFAKT                            
123000     MOVE SGMT-IDDISTR          TO RUB6-IDDISTR                           
123100                                   TEST-IDDISTR                           
123200     MOVE SGMT-KDORDKL-MAX      TO RUB6-KDORDKL                           
123300     MOVE TAB-ANTAL-KOLLI(TAB-IX) TO RUB6-TOT-KOLLI                       
123400                                                                          
123500     COMPUTE WS-VKORDBTO = TAB-VKORDBTO-KOLLI (TAB-IX) *                  
123600                           CONV-KG-TO-LB                                  
123700     COMPUTE WS-VLORDBTO = TAB-VLORDBTO-KOLLI (TAB-IX) *                  
123800                           CONV-M3-TO-FT3                                 
123900                                                                          
124000     IF DIS134-BYTESREN-NA                                                
124100       MOVE ZERO                TO RUB7-VKORDBTO                          
124200                                   RUB7-VLORDBTO                          
124300                                   RUB7-TOT-PRAVCOST                      
124400       MOVE 'LB'                TO RUB7-SORT                              
124500     ELSE                                                                 
124600       MOVE WS-VKORDBTO         TO RUB7-VKORDBTO                          
124700       MOVE 'LB'                TO RUB7-SORT                              
124800       MOVE WS-VLORDBTO         TO RUB7-VLORDBTO                          
124900       MOVE TAB-PRAVCOST-KOLLI (TAB-IX)                                   
125000                                TO RUB7-TOT-PRAVCOST                      
125100     END-IF                                                               
125200                                                                          
125300     MOVE SHIP-IDDC             TO WS-IDDC                                
125400     IF NDC-CA                                                            
125500       MOVE 'CAD'               TO RUB7-VALUTA                            
125600     ELSE                                                                 
125700       MOVE 'USD'               TO RUB7-VALUTA                            
125800     END-IF                                                               
125900                                                                          
126000                                                                          
126100*    -- DATA FOR THE WEB HEADER RECORDS -----------------------           
126200     MOVE SPACE                 TO HDR-W476NAPH                           
126300                                                                          
126400*    -- THE FOLLOWING FIELDS ARE MOVED IN EAB-SKRIV-RUBRIK                
126500*    -- FOR THE PAPER REPORT, BUT HERE FOR THE WEB REPORT                 
126600     MOVE SHIP-TISKEPPN         TO HDR-TISKEPPN                           
126700     MOVE SGMT-IDDISTR          TO HDR-IDDISTR                            
126800     MOVE SHIP-IDSHIPM          TO HDR-IDSHIPM                            
126900     MOVE SHIP-IDTRPTNR         TO HDR-IDTRPTNR                           
127000     MOVE SHIP-IDLBBET          TO HDR-IDLBBET                            
127100                                                                          
127200*    -- THE REST OF THE HEADER FIELDS                                     
127300     MOVE RUB4-IDDC             TO HDR-IDDC                               
127400     MOVE RAD1H-IMPORTER        TO HDR-BEGMT-RAD1                         
127500     MOVE RAD2H-IMPORTER        TO HDR-BEGMT-RAD2                         
127600     MOVE RAD3H-IMPORTER        TO HDR-ADGMT-GATA                         
127700     MOVE RAD4H-IMPORTER        TO HDR-ADGMT-PADR                         
127800     MOVE RAD5H-IMPORTER        TO HDR-ADGMT-LAND                         
127900                                                                          
128000*    -- SUB-HEADER DATA FOR US                                            
128100     MOVE SPACE                 TO SUBH-W476NAPS                          
128200                                                                          
128300     MOVE RAD1-TO-IDDC          TO SUBH-TO-IDDC                           
128400     MOVE RAD1-BEGMT-1          TO SUBH-BEGMT-RAD1                        
128500     MOVE RAD1-BEGMT-2          TO SUBH-adGMT-GATA                        
128600     MOVE RAD2-BEGMT-3          TO SUBH-BEGMT-RAD2                        
128700     MOVE RAD2-BEGMT-4          TO SUBH-ADGMT-PADR                        
128800     MOVE RAD3-BEGMT-5          TO SUBH-ADGMT-LAND                        
128900     MOVE SPACE                 TO SUBH-KDFRAKT                           
129000     MOVE RUB4-IDFAKT           TO SUBH-IDFAKT                            
129100     MOVE RUB6-KDORDKL          TO SUBH-KDORDKL-MAX                       
129200     MOVE RUB6-TOT-KOLLI        TO SUBH-KVKOLLI                           
129300     MOVE RUB7-VKORDBTO         TO SUBH-VKORDBTO                          
129400     MOVE RUB7-SORT             TO SUBH-KDSORT-VK                         
129500     MOVE RUB7-VLORDBTO         TO SUBH-VLORDBTO                          
129600     MOVE 'FT'                  TO SUBH-KDSORT-VL                         
129700     MOVE RUB7-TOT-PRAVCOST     TO SUBH-PRAVCOST                          
129800     MOVE RUB7-VALUTA           TO SUBH-KDVALISO                          
129900     .                                                                    
130000     EJECT                                                                
130100 EAA1-FLYTTA-RUB-DATA-CA SECTION.                                         
130200                                                                          
130300     MOVE WS-SIDNR                TO SIDNR                                
130400     MOVE 'SHIPPER'               TO RAD1HC-SHIPP-TEXT                    
130500     MOVE 'PROFORMA INVOICE '     TO RAD1HC-TYP-IDSHIP                    
130600                                                                          
130700     PERFORM IMS-GU-WDB601                                                
130800     IF SEGMENT-FINNS                                                     
130900       MOVE DCS-BEGMT-RAD1        TO RAD1HC-SHIPPER                       
131000       MOVE DCS-BEGMT-RAD2        TO RAD2HC-SHIPPER                       
131100       MOVE DCS-ADGMT-GATA        TO RAD3HC-SHIPPER                       
131200       MOVE DCS-ADGMT-PADR        TO RAD4HC-SHIPPER                       
131300       MOVE DCS-ADGMT-LAND        TO RAD5HC-SHIPPER                       
131400     END-IF                                                               
131500*                                                                         
131600     MOVE SHIP-IDDC               TO RUB4C-IDDC                           
131700     MOVE SKOLLI-IDFAKT           TO RUB4C-IDFAKT                         
131800     MOVE SGMT-IDKUNDNR           TO RUB6C-IDKUNDNR                       
131900     MOVE SGMT-IDDISTR            TO TEST-IDDISTR                         
132000     MOVE SGMT-KDORDKL-MAX        TO RUB6C-KDORDKL                        
132100     MOVE TAB-ANTAL-KOLLI(TAB-IX) TO RUB6C-TOT-KOLLI                      
132200     COMPUTE WS-VKORDBTO = TAB-VKORDBTO-KOLLI (TAB-IX) *                  
132300                           CONV-KG-TO-LB                                  
132400     COMPUTE WS-VLORDBTO = TAB-VLORDBTO-KOLLI (TAB-IX) *                  
132500                           CONV-M3-TO-FT3                                 
132600                                                                          
132700     MOVE WS-VKORDBTO             TO RUB7C-VKORDBTO                       
132800     MOVE 'LB'                    TO RUB7C-SORT                           
132900     MOVE WS-VLORDBTO             TO RUB7C-VLORDBTO                       
133000     MOVE TAB-PRAVCOST-KOLLI (TAB-IX)                                     
133100                                  TO RUB7C-TOT-PRAVCOST                   
133200                                                                          
133300     MOVE 'USD'                   TO RUB7C-VALUTA                         
133400*                                                                         
133500     MOVE SGMT-IDDISTR            TO W-WDB201-IDDISTR                     
133600     MOVE SGMT-IDKUNDNR           TO W-WDB201-IDKUNDNR                    
133700     PERFORM IMS-GU-WDB201                                                
133800                                                                          
133900     IF SEGMENT-FINNS                                                     
134000       MOVE GMT-BEGMT-RAD1        TO RAD1-BEGMT-1C                        
134100       MOVE GMT-ADGMT-GATA        TO RAD2-GATA-1C                         
134200       MOVE GMT-ADGMT-PADR        TO RAD3-PADR-1C                         
134300       MOVE GMT-ADGMT-LAND        TO RAD4-LAND-1C                         
134400     END-IF                                                               
134500                                                                          
134600     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
134700       MOVE +2 TO W-KDSPRAK                                               
134800     ELSE                                                                 
134900       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
135000     END-IF                                                               
135100                                                                          
135200*    -- DATA FOR THE WEB HEADER RECORDS -----------------------           
135300     MOVE SPACE                 TO HDR-W476NAPH                           
135400                                                                          
135500*    -- THE FOLLOWING FIELDS ARE MOVED IN EAB1-SKRIV-RUBRIK-CA            
135600*    -- FOR THE PAPER REPORT, BUT HERE FOR THE WEB REPORT                 
135700     MOVE SHIP-TISKEPPN         TO HDR-TISKEPPN                           
135800     MOVE SGMT-IDDISTR          TO HDR-IDDISTR                            
135900     MOVE SHIP-IDSHIPM          TO HDR-IDSHIPM                            
136000     MOVE SHIP-IDTRPTNR         TO HDR-IDTRPTNR                           
136100     MOVE SHIP-IDLBBET          TO HDR-IDLBBET                            
136200                                                                          
136300*    -- THE REST OF THE HEADER FIELDS                                     
136400     MOVE RUB4C-IDDC            TO HDR-IDDC                               
136500     MOVE RAD1HC-SHIPPER        TO HDR-BEGMT-RAD1                         
136600     MOVE RAD2HC-SHIPPER        TO HDR-BEGMT-RAD2                         
136700     MOVE RAD3HC-SHIPPER        TO HDR-ADGMT-GATA                         
136800     MOVE RAD4HC-SHIPPER        TO HDR-ADGMT-PADR                         
136900     MOVE RAD5HC-SHIPPER        TO HDR-ADGMT-LAND                         
137000                                                                          
137100*    -- SPECIAL SUB-HEADER DATA FOR USE TO CANADA DEALER                  
137200     MOVE SPACE                 TO SUBX-W476NAPX                          
137300                                                                          
137400     MOVE RAD1-BEGMT-1C         TO SUBX-TO-BEGMT-RAD1                     
137500     MOVE SPACE                 TO SUBX-TO-BEGMT-RAD2                     
137600     MOVE RAD2-GATA-1C          TO SUBX-TO-ADGMT-GATA                     
137700     MOVE RAD3-PADR-1C          TO SUBX-TO-ADGMT-PADR                     
137800     MOVE RAD4-LAND-1C          TO SUBX-TO-ADGMT-LAND                     
137900                                                                          
138000*    -- THE FOLLOWING FIELDS ARE MOVED IN EAB1-SKRIV-RUBRIK-CA            
138100*    -- FOR THE PAPER REPORT, BUT HERE FOR THE WEB REPORT                 
138200     MOVE 'VOLVO CAR USA,LLC          '  TO SUBX-BELEV-RAD1               
138300     MOVE SPACE                          TO SUBX-BELEV-RAD2               
138400     MOVE '1 VOLVO DRIVE'                TO SUBX-ADLEV-GATA               
138500     MOVE 'ROCKLEIGH,N.J. 07647'         TO SUBX-ADLEV-PADR               
138600     MOVE 'USA'                          TO SUBX-ADLEV-LAND               
138700                                                                          
138800     MOVE 'VOLVO CARS OF CANADA CORP'    TO SUBX-BEBETRAD-1               
138900     MOVE SPACE                          TO SUBX-BEBETRAD-2               
139000     MOVE '9130 LESLIE STREET,SUITE 101' TO SUBX-ADBETRAD-GATA            
139100     MOVE 'RICHMOND HILL, ON L4B0B9'     TO SUBX-ADBETRAD-PADR            
139200     MOVE 'ACCOUNT 5R40Y2'               TO SUBX-ADBETRAD-LAND            
139300                                                                          
139400*    --                                                                   
139500     MOVE SPACE                  TO SUBX-KDFRAKT                          
139600     MOVE RUB6C-IDKUNDNR         TO SUBX-TO-IDKUNDNR                      
139700     MOVE RUB6C-KDORDKL          TO SUBX-KDORDKL-MAX                      
139800     MOVE RUB6C-TOT-KOLLI        TO SUBX-KVKOLLI                          
139900     MOVE RUB7C-VKORDBTO         TO SUBX-VKORDBTO                         
140000     MOVE RUB7C-SORT             TO SUBX-KDSORT-VK                        
140100     MOVE RUB7C-VKORDBTO         TO SUBX-VLORDBTO                         
140200     MOVE 'FT'                   TO SUBX-KDSORT-VL                        
140300     MOVE RUB7C-TOT-PRAVCOST     TO SUBX-PRAVCOST                         
140400     MOVE RUB7C-VALUTA           TO SUBX-KDVALISO                         
140500     MOVE SKOLLI-IDFAKT          TO SUBX-IDFAKT                           
140600     .                                                                    
140700                                                                          
140800     EJECT                                                                
140900 EAB-SKRIV-RUBRIK SECTION.                                                
141000                                                                          
           IF WS-SIDNR = ZERO                                                   
              PERFORM S09-PRINT-META                                            
           END-IF                                                               
                                                                                
141100     MOVE SPACE                      TO SEND-RAD                          
141200     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
141300     PERFORM S80-PUT-ONDEM-LINE                                           
141400                                                                          
141500     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
141600     PERFORM S80-PUT-ONDEM-LINE                                           
141700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
141800     PERFORM S80-PUT-ONDEM-LINE                                           
141900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
142000     MOVE RAD-HEAD                   TO WS-RAD                            
142100                                        SEND-RAD                          
142200     MOVE PRT-NYSIDA-RAD5            TO PRT-RADSKIP                       
142300     ADD 5                           TO WS-RAD-RAEKNARE                   
142400     PERFORM S10-PRINT-LINE                                               
142500     MOVE RAD2-HEAD                  TO SEND-RAD                          
142600                                        WS-RAD                            
142700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
142800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
142900     ADD 1                           TO WS-RAD-RAEKNARE                   
143000     PERFORM S10-PRINT-LINE                                               
143100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
143200                                                                          
143300     MOVE RAD3-HEAD                  TO SEND-RAD                          
143400                                        WS-RAD                            
143500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
143600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
143700     ADD 1                           TO WS-RAD-RAEKNARE                   
143800     PERFORM S10-PRINT-LINE                                               
143900                                                                          
144000     ADD 1                     TO WS-SIDNR                                
144100     MOVE SPACE                TO RAD1                                    
144200*    MOVE DCS-ADGMT-PADR       TO RAD4H-IMPORTER                          
144300     MOVE SHIP-TISKEPPN        TO RAD1-TIAAMMDD                           
144400     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
144500     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
144600     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
144700     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
144800     MOVE WS-SIDNR             TO RAD1-PAGE-NO                            
144900     MOVE RAD1                 TO SEND-RAD                                
145000                                  WS-RAD                                  
145100     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
145200     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
145300     ADD 1                     TO WS-RAD-RAEKNARE                         
145400     PERFORM S10-PRINT-LINE                                               
145500     MOVE RAD5-HEAD                  TO SEND-RAD                          
145600                                        WS-RAD                            
145700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
145800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
145900     ADD 1                           TO WS-RAD-RAEKNARE                   
146000     PERFORM S10-PRINT-LINE                                               
146100                                                                          
146200     MOVE 'SHIP TO'                  TO RAD1-DEALER-TEXT                  
146300     MOVE RAD1X                      TO WS-RAD                            
146400                                        SEND-RAD                          
146500     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
146600     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
146700     ADD 2                           TO WS-RAD-RAEKNARE                   
146800     PERFORM S10-PRINT-LINE                                               
146900                                                                          
147000     MOVE RAD2X                      TO WS-RAD                            
147100                                        SEND-RAD                          
147200     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
147300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
147400     ADD 1                           TO WS-RAD-RAEKNARE                   
147500     PERFORM S10-PRINT-LINE                                               
147600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
147700                                                                          
147800     MOVE RAD3X                      TO WS-RAD                            
147900                                        SEND-RAD                          
148000     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
148100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
148200     ADD 1                           TO WS-RAD-RAEKNARE                   
148300     PERFORM S10-PRINT-LINE                                               
148400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
148500                                                                          
148600                                                                          
148700     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
148800     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
148900     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
149000     MOVE RUBRIKRAD-4              TO WS-RAD                              
149100                                      SEND-RAD                            
149200     PERFORM S10-PRINT-LINE                                               
149300                                                                          
149400                                                                          
149500     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
149600     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
149700     MOVE RUBRIKLINJE              TO WS-RAD                              
149800                                      SEND-RAD                            
149900     PERFORM S10-PRINT-LINE                                               
150000                                                                          
150100     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
150200     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
150300     MOVE RUBRIKRAD-6              TO WS-RAD                              
150400                                      SEND-RAD                            
150500     PERFORM S10-PRINT-LINE                                               
150600                                                                          
150700     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
150800     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
150900     MOVE RUBRIKRAD-7              TO WS-RAD                              
151000                                      SEND-RAD                            
151100     PERFORM S10-PRINT-LINE                                               
151200                                                                          
151300     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
151400     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
151500     MOVE RUBRIKLINJE              TO WS-RAD                              
151600                                      SEND-RAD                            
151700     PERFORM S10-PRINT-LINE                                               
151800                                                                          
151900     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
152000     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
152100     MOVE DETALJRUB                TO WS-RAD                              
152200                                      SEND-RAD                            
152300     PERFORM S10-PRINT-LINE                                               
152400                                                                          
152500     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
152600     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
152700     MOVE SPACE                    TO WS-RAD                              
152800                                      SEND-RAD                            
152900     PERFORM S10-PRINT-LINE                                               
153000     .                                                                    
153100     EJECT                                                                
153200 EAB1-SKRIV-RUBRIK-CA SECTION.                                            
153300                                                                          
           IF WS-SIDNR = ZERO                                                   
              PERFORM S09-PRINT-META                                            
           END-IF                                                               
                                                                                
153400     MOVE SPACE                      TO SEND-RAD                          
153500     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
153600     PERFORM S80-PUT-ONDEM-LINE                                           
153700                                                                          
153800     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
153900     PERFORM S80-PUT-ONDEM-LINE                                           
154000     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
154100     PERFORM S80-PUT-ONDEM-LINE                                           
154200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
154300     MOVE RAD-HEAD-C                 TO WS-RAD                            
154400                                        SEND-RAD                          
154500     MOVE PRT-NYSIDA-RAD5            TO PRT-RADSKIP                       
154600     ADD 5                           TO WS-RAD-RAEKNARE                   
154700     PERFORM S10-PRINT-LINE                                               
154800     MOVE RAD2-HEAD-C                TO SEND-RAD                          
154900                                        WS-RAD                            
155000     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
155100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
155200     ADD 1                           TO WS-RAD-RAEKNARE                   
155300     PERFORM S10-PRINT-LINE                                               
155400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
155500                                                                          
155600     MOVE RAD3-HEAD-C                TO SEND-RAD                          
155700                                        WS-RAD                            
155800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
155900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
156000     ADD 1                           TO WS-RAD-RAEKNARE                   
156100     PERFORM S10-PRINT-LINE                                               
156200                                                                          
156300     ADD 1                     TO WS-SIDNR                                
156400     MOVE SPACE                TO RAD1                                    
156500*    MOVE DCS-ADGMT-PADR       TO RAD4HC-SHIPPER                          
156600     MOVE SHIP-TISKEPPN        TO RAD1C-TIAAMMDD                          
156700     MOVE SGMT-IDDISTR         TO RAD1C-IDDISTR                           
156800     MOVE SHIP-IDSHIPM         TO RAD1C-IDSHIPM                           
156900     MOVE SHIP-IDTRPTNR        TO RAD1C-IDTRPTNR                          
157000     MOVE SHIP-IDLBBET         TO RAD1C-IDLBBET                           
157100     MOVE WS-SIDNR             TO RAD1C-PAGE-NO                           
157200     MOVE RAD1-C               TO SEND-RAD                                
157300                                  WS-RAD                                  
157400     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
157500     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
157600     ADD 1                     TO WS-RAD-RAEKNARE                         
157700     PERFORM S10-PRINT-LINE                                               
157800     MOVE RAD5-HEAD-C                TO SEND-RAD                          
157900                                        WS-RAD                            
158000     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
158100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
158200     ADD 1                           TO WS-RAD-RAEKNARE                   
158300     PERFORM S10-PRINT-LINE                                               
158400                                                                          
158500*    -- THE LITERAL ADDRESS VALUES BELOW ARE ALSO MOVED IN                
158600*    -- SECTION EAA1-..  FOR THE WEB REPORT                               
158700     MOVE 'SHIP TO:'                 TO RAD1-DEALER-TEXT-C                
158800     MOVE 'SUPPL.:'                  TO RAD1-SUPPL-TEXT2-C                
158900     MOVE 'VOLVO CAR USA, LLC         '                                   
159000                                     TO RAD1-BEGMT-2C                     
159100     MOVE 'SOLD TO:'                 TO RAD1-CANADA-TEXT2-C               
159200     MOVE 'VOLVO CARS OF CANADA CORP   '                                  
159300                                     TO RAD1-BEGMT-3C                     
159400     MOVE RAD1X-C                    TO WS-RAD                            
159500                                        SEND-RAD                          
159600     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
159700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
159800     ADD 1                           TO WS-RAD-RAEKNARE                   
159900     PERFORM S10-PRINT-LINE                                               
160000                                                                          
160100     MOVE '1800 VOLVO PLACE           '                                   
160200                                    TO RAD2-GATA-2C                       
160300     MOVE '9130 LESLIE STREET,SUITE 101'                                  
160400                                     TO RAD2-GATA-3C                      
160500     MOVE RAD2X-C                    TO WS-RAD                            
160600                                        SEND-RAD                          
160700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
160800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
160900     ADD 1                           TO WS-RAD-RAEKNARE                   
161000     PERFORM S10-PRINT-LINE                                               
161100                                                                          
161200     MOVE 'MAHWAH, NJ 07460           '                                   
161300                                     TO RAD3-PADR-2C                      
161400     MOVE 'RICHMOND HILL, ON L4B0B9    '                                  
161500                                     TO RAD3-PADR-3C                      
161600     MOVE RAD3X-C                    TO WS-RAD                            
161700                                        SEND-RAD                          
161800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
161900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
162000     ADD 1                           TO WS-RAD-RAEKNARE                   
162100     PERFORM S10-PRINT-LINE                                               
162200                                                                          
162300     MOVE 'USA                        '                                   
162400                                     TO RAD4-LAND-2C                      
162500     MOVE 'ACCOUNT 5R40Y2              '                                  
162600                                     TO RAD4-LAND-3C                      
162700     MOVE RAD4X-C                    TO WS-RAD                            
162800                                        SEND-RAD                          
162900     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
163000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
163100     ADD 1                           TO WS-RAD-RAEKNARE                   
163200     PERFORM S10-PRINT-LINE                                               
163300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
163400                                                                          
163500                                                                          
163600     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
163700     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
163800     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
163900     MOVE RUBRIKRAD-4C             TO WS-RAD                              
164000                                      SEND-RAD                            
164100     PERFORM S10-PRINT-LINE                                               
164200                                                                          
164300                                                                          
164400     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
164500     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
164600     MOVE RUBRIKLINJE              TO WS-RAD                              
164700                                      SEND-RAD                            
164800     PERFORM S10-PRINT-LINE                                               
164900                                                                          
165000     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
165100     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
165200     MOVE RUBRIKRAD-6C             TO WS-RAD                              
165300                                      SEND-RAD                            
165400     PERFORM S10-PRINT-LINE                                               
165500                                                                          
165600     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
165700     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
165800     MOVE RUBRIKRAD-7C             TO WS-RAD                              
165900                                      SEND-RAD                            
166000     PERFORM S10-PRINT-LINE                                               
166100                                                                          
166200     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
166300     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
166400     MOVE RUBRIKLINJE              TO WS-RAD                              
166500                                      SEND-RAD                            
166600     PERFORM S10-PRINT-LINE                                               
166700                                                                          
166800     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
166900     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
167000     MOVE DETALJRUB                TO WS-RAD                              
167100                                      SEND-RAD                            
167200     PERFORM S10-PRINT-LINE                                               
167300                                                                          
167400     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
167500     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
167600     MOVE SPACE                    TO WS-RAD                              
167700                                      SEND-RAD                            
167800     PERFORM S10-PRINT-LINE                                               
167900     .                                                                    
168000     EJECT                                                                
168100                                                                          
168200 EAC1-FLYTTA-RAD-DATA SECTION.                                            
168300                                                                          
168400     MOVE SRAD-IDARTNR        TO LISTRAD-IDARTNR                          
168500                                 W-IDARTNR                                
168600     PERFORM IMS-GU-WDD311                                                
168700     IF SEGMENT-SAKNAS                                                    
168800       MOVE SPACE             TO TEXT-BEART                               
168900     END-IF                                                               
169000     MOVE TEXT-BEART          TO LISTRAD-BEART                            
169100                                                                          
169200     MOVE ZERO                TO LISTRAD-VKARTNTO-G                       
169300                                                                          
169400     MOVE SRAD-IDARTNR        TO W-IDARTNR                                
169500     MOVE SHIP-IDDC           TO W-IDDC                                   
169600                                 WS-IDDC                                  
169700     PERFORM IMS-GU-WDK711                                                
169800     IF SEGMENT-SAKNAS                                                    
169900       MOVE ZERO              TO SLAG-PRAVCOST                            
170000     END-IF                                                               
170100                                                                          
170200     MOVE SRAD-KVLEVART       TO LISTRAD-KVLEVART                         
170300     COMPUTE WS-PRAVCOST-TOT  =  SLAG-PRAVCOST *                          
170400                                 SRAD-KVLEVART                            
170500     MOVE SGMT-IDDISTR        TO TEST-IDDISTR                             
170600     IF DIS134-BYTESREN-NA                                                
170700       MOVE ZERO              TO LISTRAD-UNIT-PRAVCOST                    
170800                                 LISTRAD-PRAVCOST                         
170900     ELSE                                                                 
171000       MOVE SLAG-PRAVCOST     TO LISTRAD-UNIT-PRAVCOST                    
171100       MOVE WS-PRAVCOST-TOT   TO LISTRAD-PRAVCOST                         
171200     END-IF                                                               
171300                                                                          
171400     MOVE SRAD-KDARTURS       TO ARTU-KDARTURS                            
171500     MOVE ZERO                TO ARTU-IDDISTR                             
171600     MOVE SPACE               TO ARTU-IDDC                                
171700     IF SGMT-IDDISTR = 7674 AND NDC-US                                    
171800       MOVE SRAD-KDARTURS     TO LISTRAD-BEARTURS-ENG                     
171900     ELSE                                                                 
172000       CALL W400ARTU USING ARTU-W400ARTU                                  
172100       MOVE ARTU-BEARTURS-ENG TO LISTRAD-BEARTURS-ENG                     
172200     END-IF                                                               
172300     MOVE SKOLLI-IDKUNDRF(3:5) TO LISTRAD-IDKUNDRF                        
172400     MOVE SKOLLI-IDKOLLI      TO LISTRAD-IDKOLLI                          
172500                                                                          
172600                                                                          
172700*    -- DATA FOR THE WEB DETAIL LINE RECORDS ------------------           
172800     MOVE SPACE                 TO LINE-W476NAPL                          
172900                                                                          
173000     MOVE LISTRAD-IDARTNR       TO LINE-IDARTNR                           
173100     MOVE SPACE                 TO LINE-REKSIFFR                          
173200     MOVE LISTRAD-BEART         TO LINE-BEART                             
173300     MOVE SRAD-IDLEVNR-ART      TO LINE-IDLEVNR-ART                       
173400     MOVE LISTRAD-KVLEVART      TO LINE-KVLEVART                          
173500     MOVE LISTRAD-UNIT-PRAVCOST TO LINE-PRAVCOST                          
173600     MOVE LISTRAD-PRAVCOST      TO LINE-SUAVCOST                          
173700     MOVE LISTRAD-BEARTURS-ENG  TO LINE-BEARTURS                          
173800     MOVE LISTRAD-IDKUNDRF      TO LINE-IDORDNR5                          
173900     MOVE LISTRAD-IDKOLLI       TO LINE-IDKOLLI                           
174000     .                                                                    
174100     EJECT                                                                
174200 EAC2-FLYTTA-RAD-DATA-CA SECTION.                                         
174300                                                                          
174400     MOVE SRAD-IDARTNR        TO LISTRAD-IDARTNR-C                        
174500                                 W-IDARTNR                                
174600     MOVE WS-STRECK           TO LISTRAD-STRECK-C                         
174700     MOVE SRAD-REKSIFFR       TO LISTRAD-KONTROLL-C                       
174800                                                                          
174900     PERFORM IMS-GU-WDD311                                                
175000     IF SEGMENT-SAKNAS                                                    
175100       MOVE SPACE             TO TEXT-BEART                               
175200     END-IF                                                               
175300     MOVE TEXT-BEART          TO LISTRAD-BEART-C                          
175400                                                                          
175500     MOVE ZERO                TO LISTRAD-VKARTNTO-G-C                     
175600                                                                          
175700     MOVE SRAD-IDARTNR        TO W-IDARTNR                                
175800     MOVE SHIP-IDDC           TO W-IDDC                                   
175900     PERFORM IMS-GU-WDK711                                                
176000     IF SEGMENT-SAKNAS                                                    
176100       MOVE ZERO              TO SLAG-PRAVCOST                            
176200     END-IF                                                               
176300                                                                          
176400     MOVE SRAD-KVLEVART       TO LISTRAD-KVLEVART-C                       
176500     COMPUTE WS-PRAVCOST-TOT  =  SLAG-PRAVCOST *                          
176600                                 SRAD-KVLEVART                            
176700     MOVE SGMT-IDDISTR        TO TEST-IDDISTR                             
176800     IF DIS134-BYTESREN-NA                                                
176900       MOVE ZERO              TO LISTRAD-UNIT-PRAVCOST-C                  
177000                                 LISTRAD-PRAVCOST-C                       
177100     ELSE                                                                 
177200       MOVE SLAG-PRAVCOST     TO LISTRAD-UNIT-PRAVCOST-C                  
177300       MOVE WS-PRAVCOST-TOT   TO LISTRAD-PRAVCOST-C                       
177400     END-IF                                                               
177500                                                                          
177600     MOVE SRAD-KDARTURS       TO LISTRAD-BEARTURS-ENG-C                   
177700     MOVE SKOLLI-IDKUNDRF(3:5) TO LISTRAD-IDKUNDRF-C                      
177800     MOVE SKOLLI-IDKOLLI      TO LISTRAD-IDKOLLI-C                        
177900                                                                          
178000                                                                          
178100*    -- DATA FOR THE WEB DETAIL LINE RECORDS ------------------           
178200     MOVE SPACE                   TO LINE-W476NAPL                        
178300                                                                          
178400     MOVE LISTRAD-IDARTNR-C       TO LINE-IDARTNR                         
178500     MOVE LISTRAD-KONTROLL-C      TO LINE-REKSIFFR                        
178600     MOVE LISTRAD-BEART-C         TO LINE-BEART                           
178700     MOVE SRAD-IDLEVNR-ART        TO LINE-IDLEVNR-ART                     
178800     MOVE LISTRAD-KVLEVART-C      TO LINE-KVLEVART                        
178900     MOVE LISTRAD-UNIT-PRAVCOST-C TO LINE-PRAVCOST                        
179000     MOVE LISTRAD-PRAVCOST-C      TO LINE-SUAVCOST                        
179100     MOVE LISTRAD-BEARTURS-ENG-C  TO LINE-BEARTURS                        
179200     MOVE LISTRAD-IDKUNDRF-C      TO LINE-IDORDNR5                        
179300     MOVE LISTRAD-IDKOLLI-C       TO LINE-IDKOLLI                         
179400     .                                                                    
179500     EJECT                                                                
179600 EAC3-FLYTTA-RAD-DATA SECTION.                                            
179700                                                                          
179800     MOVE SGMT-IDDISTR        TO TEST-IDDISTR                             
179900     MOVE SPACE               TO DETALJRAD                                
180000     MOVE JA                  TO RAD-SW                                   
180100     MOVE SRAD-IDARTNR        TO TEST-IDARTNR                             
180200     IF BYT19-BYTES                                                       
180300       ADD +6000              TO SRAD-IDARTNR                             
180400     ELSE                                                                 
180500       IF BYT19-RADIO                                                     
180600         ADD +1000            TO SRAD-IDARTNR                             
180700       ELSE                                                               
180800         IF DIST35-US-US-TRANSFER     OR                                  
180900            DIST35-US-CAN-TRANSFER    OR                                  
181000            DIST35-US-US-RETUR        OR                                  
181100            DIST35-US-CA-RETUR        OR                                  
181200            DIST35-USA-USA-REFILL     OR                                  
181300            DIST35-USA-CA-REFILL                                          
181400           MOVE NEJ           TO RAD-SW                                   
181500         END-IF                                                           
181600       END-IF                                                             
181700     END-IF                                                               
181800     MOVE SRAD-IDARTNR        TO LISTRAD-IDARTNR                          
181900     MOVE 'CORE PART'         TO LISTRAD-BEART                            
182000                                                                          
182100     MOVE SRAD-IDARTNR        TO W-IDARTNR                                
182200     MOVE SHIP-IDDC           TO W-IDDC                                   
182300     PERFORM IMS-GU-WDK711                                                
182400     IF SEGMENT-SAKNAS                                                    
182500       MOVE ZERO              TO SLAG-PRAVCOST                            
182600     END-IF                                                               
182700                                                                          
182800     COMPUTE WS-PRAVCOST-TOT = SLAG-PRAVCOST *                            
182900                               SRAD-KVLEVART                              
183000     MOVE SGMT-IDDISTR        TO TEST-IDDISTR                             
183010*                                                                         
183020*    -- PRAVCOST AV THE CORE PART (WHICH IS LIKE A LITTLE FEE)--          
183030*    -- SHOULD NOT BE SHOWN BECAUSE IT'S NOT PART OF THE TOTAL--          
183040*    -- 17/1 '22                                                          
183050*                                                                         
183100*    IF DIS134-BYTESREN-NA                                                
183200*      MOVE ZERO              TO LISTRAD-UNIT-PRAVCOST                    
183300*                                LISTRAD-PRAVCOST                         
183400*    ELSE                                                                 
183500*      MOVE SLAG-PRAVCOST     TO LISTRAD-UNIT-PRAVCOST                    
183600*      MOVE WS-PRAVCOST-TOT   TO LISTRAD-PRAVCOST                         
183700*    END-IF                                                               
183800                                                                          
183900     IF SGMT-IDDISTR = 7674 AND NDC-US                                    
184000       MOVE NEJ               TO RAD-SW                                   
184100     END-IF                                                               
184500*                                                                         
184501*                                                                         
184510*    -- DATA FOR THE WEB DETAIL LINE RECORDS ------------------           
184520*    -- (SOME MOVES ARE COMMENTED TO GIVE EMPTY COLUMNS                   
184530*    -- FOR THIS TYPE OF DETAIL LINE)                                     
184600     MOVE SPACE                   TO LINE-W476NAPL                        
184700                                                                          
184800     MOVE LISTRAD-IDARTNR         TO LINE-IDARTNR                         
184900     MOVE SPACE                   TO LINE-REKSIFFR                        
185000     MOVE LISTRAD-BEART           TO LINE-BEART                           
185100     MOVE SRAD-IDLEVNR-ART        TO LINE-IDLEVNR-ART                     
185200*    MOVE LISTRAD-KVLEVART        TO LINE-KVLEVART                        
185300*    MOVE LISTRAD-UNIT-PRAVCOST   TO LINE-PRAVCOST                        
185400*    MOVE LISTRAD-PRAVCOST        TO LINE-SUAVCOST                        
185500*    MOVE LISTRAD-BEARTURS-ENG    TO LINE-BEARTURS                        
185600*    MOVE LISTRAD-IDKUNDRF        TO LINE-IDORDNR5                        
185700*    MOVE LISTRAD-IDKOLLI         TO LINE-IDKOLLI                         
185800     .                                                                    
185900     EJECT                                                                
186000 EAD1-SKRIV-RADER SECTION.                                                
186100                                                                          
186200     IF SKOLLI-IDKUNDRF = SPAR-IDKUNDRF                                   
186300       MOVE PRT-AFTER-1       TO PRT-RADSKIP                              
186400       MOVE WS-SKIP1          TO STYRTECKEN-RAD                           
186500       MOVE DETALJRAD         TO WS-RAD                                   
186600                                 SEND-RAD                                 
186700       PERFORM S10-PRINT-LINE                                             
186800       ADD +1                 TO WS-RAD-RAEKNARE                          
186900                                                                          
187000     ELSE                                                                 
187100       MOVE PRT-AFTER-2       TO PRT-RADSKIP                              
187200       MOVE WS-SKIP2          TO STYRTECKEN-RAD                           
187300       MOVE DETALJRAD         TO WS-RAD                                   
187400                                 SEND-RAD                                 
187500       PERFORM S10-PRINT-LINE                                             
187600       ADD +2                 TO WS-RAD-RAEKNARE                          
187700     END-IF                                                               
187800                                                                          
187900     MOVE SKOLLI-IDKUNDRF     TO SPAR-IDKUNDRF                            
188000     .                                                                    
188100     EJECT                                                                
188200 EAD2-SKRIV-RADER-CA SECTION.                                             
188300                                                                          
188400     IF SKOLLI-IDKUNDRF = SPAR-IDKUNDRF                                   
188500       MOVE PRT-AFTER-1       TO PRT-RADSKIP                              
188600       MOVE WS-SKIP1          TO STYRTECKEN-RAD                           
188700       MOVE DETALJRAD-C       TO WS-RAD                                   
188800                                 SEND-RAD                                 
188900       PERFORM S10-PRINT-LINE                                             
189000       ADD +1                 TO WS-RAD-RAEKNARE                          
189100                                                                          
189200     ELSE                                                                 
189300       MOVE PRT-AFTER-2       TO PRT-RADSKIP                              
189400       MOVE WS-SKIP2          TO STYRTECKEN-RAD                           
189500       MOVE DETALJRAD-C       TO WS-RAD                                   
189600                                 SEND-RAD                                 
189700       PERFORM S10-PRINT-LINE                                             
189800       ADD +2                 TO WS-RAD-RAEKNARE                          
189900     END-IF                                                               
190000                                                                          
190100     MOVE SKOLLI-IDKUNDRF     TO SPAR-IDKUNDRF                            
190200     .                                                                    
190300                                                                          
190400     EJECT                                                                
190500 EAE-SKRIV-SISTA-SIDAN SECTION.                                           
190600                                                                          
190700     IF SGMT-IDDISTR = 7674 AND NDC-US                                    
190800*      -- END-OF-REPORT LINES FOR USA TO CANADA-DEALER                    
190900       MOVE SPACE                  TO SEND-RAD                            
191000                                      WS-RAD                              
191100       MOVE PRT-AFTER-3            TO PRT-RADSKIP                         
191200       MOVE WS-SKIP2               TO STYRTECKEN-RAD                      
191300       PERFORM S80-PUT-ONDEM-LINE                                         
191400       IF WEB-OUTPUT                                                      
191500         PERFORM EAE-S1-SKRIV-TEXT-RAD                                    
191600       END-IF                                                             
191700                                                                          
191800       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
191900       PERFORM S80-PUT-ONDEM-LINE                                         
192000       IF WEB-OUTPUT                                                      
192100         PERFORM EAE-S1-SKRIV-TEXT-RAD                                    
192200       END-IF                                                             
192300                                                                          
192400       MOVE FINALRAD1              TO WS-RAD                              
192500       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
192600       ADD +3                      TO WS-RAD-RAEKNARE                     
192700                                                                          
192800       MOVE PRT-AFTER-1            TO PRT-RADSKIP                         
192900       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
193000       MOVE FINALRAD2              TO WS-RAD                              
193100       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
193200       ADD +1                      TO WS-RAD-RAEKNARE                     
193300                                                                          
193400       IF WS-RAD-RAEKNARE >= 37                                           
193500         PERFORM EAED-SKRIV-HUV-CA                                        
193600       END-IF                                                             
193700       PERFORM EAEC-SKRIV-TEXT-CA                                         
193800                                                                          
193900       MOVE ZERO                   TO WS-SIDNR                            
194000       MOVE +45                    TO WS-RAD-RAEKNARE                     
194100       MOVE SPACE                  TO SPAR-IDKUNDRF                       
194200                                                                          
194300     ELSE                                                                 
194400*      -- NORMAL END-OF-REPORT LINES                                      
194500       MOVE SPACE                  TO SEND-RAD                            
194600                                      WS-RAD                              
194700       MOVE PRT-AFTER-3            TO PRT-RADSKIP                         
194800       MOVE WS-SKIP2               TO STYRTECKEN-RAD                      
194900       PERFORM S80-PUT-ONDEM-LINE                                         
195000       IF WEB-OUTPUT                                                      
195100         PERFORM EAE-S1-SKRIV-TEXT-RAD                                    
195200       END-IF                                                             
195300       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
195400       PERFORM S80-PUT-ONDEM-LINE                                         
195500       IF WEB-OUTPUT                                                      
195600         PERFORM EAE-S1-SKRIV-TEXT-RAD                                    
195700       END-IF                                                             
195800       MOVE FINALRAD1              TO WS-RAD                              
195900       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
196000                                                                          
196100       MOVE PRT-AFTER-1            TO PRT-RADSKIP                         
196200       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
196300       MOVE FINALRAD2              TO WS-RAD                              
196400       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
196500       ADD +1                      TO WS-RAD-RAEKNARE                     
196600                                                                          
196700       MOVE TAB-IDDISTR (TAB-IX)   TO TEST-IDDISTR                        
196800       IF DIST35-US-CAN-TRANSFER   OR                                     
196900          DIST35-US-CA-RETUR       OR                                     
197000          DIST35-USA-NDC51-REFILL                                         
197100         ADD +1 TO WS-SIDNR                                               
197200                                                                          
197300         PERFORM S07-INIT-SHIP-INDX                                       
197400         MOVE NEJ                  TO VCOM-SW                             
197500         PERFORM EAEA-FLYTTA-TEXT-SIDA                                    
197600         PERFORM EAEB-SKRIV-TEXT-SIDA                                     
197700       END-IF                                                             
197800                                                                          
197900       MOVE ZERO                   TO WS-SIDNR                            
198000       MOVE +45                    TO WS-RAD-RAEKNARE                     
198100       MOVE SPACE                  TO SPAR-IDKUNDRF                       
198200     END-IF                                                               
198300     .                                                                    
198400     EJECT                                                                
198500 EAEA-FLYTTA-TEXT-SIDA SECTION.                                           
198600                                                                          
198700     MOVE WS-SIDNR                    TO SIDNR                            
198800                                                                          
198900     MOVE SHIPPER-CITY    (SHIP-INDX) TO TEXTRAD1-ADR                     
199000                                         TEXTRAD6-ADR                     
199100                                         TEXTRAD17-ADR                    
199200                                                                          
199300     MOVE SHIPPER-COMPANY (SHIP-INDX) TO TEXTRAD4-NAMN                    
199400                                         TEXTRAD16-NAMN                   
199500                                                                          
199600     MOVE SHIPPER-STREET  (SHIP-INDX) TO TEXTRAD5-GATA                    
199700                                                                          
199800     MOVE SHIPPER-TEL     (SHIP-INDX) TO TEXTRAD2-TEL                     
199900                                                                          
200000     MOVE SHIPPER-TELEFAX (SHIP-INDX) TO TEXTRAD3-TELEFAX                 
200100     .                                                                    
200200     EJECT                                                                
200300 EAEB-SKRIV-TEXT-SIDA SECTION.                                            
200400                                                                          
200500     IF WEB-OUTPUT                                                        
200600*    -- WRITE A STRIPPED PAGE HEADER RECORD FOR THE WEB REPORT            
200700       MOVE SPACE               TO HDR-W476NAPH                           
200800                                                                          
200900       MOVE RUB4-IDDC           TO HDR-IDDC                               
201000                                                                          
201100       MOVE SHIP-TISKEPPN       TO HDR-TISKEPPN                           
201200       MOVE SGMT-IDDISTR        TO HDR-IDDISTR                            
201300       MOVE SHIP-IDSHIPM        TO HDR-IDSHIPM                            
201400       MOVE SHIP-IDTRPTNR       TO HDR-IDTRPTNR                           
201500       MOVE SHIP-IDLBBET        TO HDR-IDLBBET                            
201600                                                                          
201700       PERFORM EAF1-SKRIV-WEB-HDR                                         
201800                                                                          
201900     ELSE                                                                 
202000*      -- WRITE A STRIPPED PAGE HEADER FOR PAPER TEXT PAGE                
202100       MOVE SPACE              TO RAD1H-IMPORTER-TEXT                     
202200                                  RAD1H-IMPORTER                          
202300                                  RAD4H-IMPORTER                          
202400                                  SEND-RAD                                
202500                                  WS-RAD                                  
202600                                                                          
202700       MOVE PRT-NYSIDA-RAD5    TO PRT-RADSKIP                             
202800       MOVE WS-PAGESKIP        TO STYRTECKEN-RAD                          
202900       PERFORM S80-PUT-ONDEM-LINE                                         
203000       MOVE WS-SKIP3           TO STYRTECKEN-RAD                          
203100       PERFORM S80-PUT-ONDEM-LINE                                         
203200       MOVE WS-SKIP2           TO STYRTECKEN-RAD                          
203300       PERFORM S80-PUT-ONDEM-LINE                                         
203400                                                                          
203500       MOVE WS-SKIP1           TO STYRTECKEN-RAD                          
203600       MOVE RAD-HEAD           TO WS-RAD                                  
203700                                  SEND-RAD                                
203800       ADD 5                   TO WS-RAD-RAEKNARE                         
203900       PERFORM S10-PRINT-LINE                                             
204000                                                                          
204100       MOVE SPACE              TO SEND-RAD                                
204200                                  WS-RAD                                  
204300       MOVE WS-SKIP1           TO STYRTECKEN-RAD                          
204400       PERFORM S80-PUT-ONDEM-LINE                                         
204500                                                                          
204600       MOVE PRT-AFTER-3        TO PRT-RADSKIP                             
204700       MOVE WS-SKIP1           TO STYRTECKEN-RAD                          
204800       PERFORM S80-PUT-ONDEM-LINE                                         
204900                                                                          
205000       MOVE SPACE              TO RAD1                                    
205100       MOVE SHIP-TISKEPPN      TO RAD1-TIAAMMDD                           
205200       MOVE SGMT-IDDISTR       TO RAD1-IDDISTR                            
205300       MOVE SHIP-IDSHIPM       TO RAD1-IDSHIPM                            
205400       MOVE SHIP-IDTRPTNR      TO RAD1-IDTRPTNR                           
205500       MOVE SHIP-IDLBBET       TO RAD1-IDLBBET                            
205600       MOVE WS-SIDNR           TO RAD1-PAGE-NO                            
205700       MOVE RAD1               TO WS-RAD                                  
205800                                  SEND-RAD                                
205900       MOVE WS-SKIP1           TO STYRTECKEN-RAD                          
206000       ADD 3                   TO WS-RAD-RAEKNARE                         
206100       PERFORM S10-PRINT-LINE                                             
206200                                                                          
206300     END-IF                                                               
206400                                                                          
206500     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
206600     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
206700     MOVE TEXTRAD1                 TO WS-RAD                              
206800     IF WEB-OUTPUT                                                        
206900       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
207000     END-IF                                                               
207100     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
207200                                                                          
207300     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
207400     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
207500     MOVE TEXTRAD2                 TO WS-RAD                              
207600     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
207700                                                                          
207800     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
207900     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
208000     MOVE TEXTRAD3                 TO WS-RAD                              
208100     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
208200                                                                          
208300     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
208400     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
208500     MOVE TEXTRAD4                 TO WS-RAD                              
208600     IF WEB-OUTPUT                                                        
208700       MOVE WS-SKIP2               TO STYRTECKEN-RAD                      
208800     END-IF                                                               
208900     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
209000                                                                          
209100     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
209200     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
209300     MOVE TEXTRAD5                 TO WS-RAD                              
209400     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
209500                                                                          
209600     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
209700     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
209800     MOVE TEXTRAD6                 TO WS-RAD                              
209900     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
210000                                                                          
210100     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
210200     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
210300     MOVE TEXTRAD7                 TO WS-RAD                              
210400     IF WEB-OUTPUT                                                        
210500       MOVE WS-SKIP2               TO STYRTECKEN-RAD                      
210600     END-IF                                                               
210700     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
210800                                                                          
210900     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
211000     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
211100     MOVE TEXTRAD8                 TO WS-RAD                              
211200     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
211300                                                                          
211400     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
211500     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
211600     MOVE TEXTRAD9                 TO WS-RAD                              
211700     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
211800                                                                          
211900     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
212000     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
212100     MOVE TEXTRAD10                TO WS-RAD                              
212200     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
212300                                                                          
212400     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
212500     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
212600     MOVE TEXTRAD11                TO WS-RAD                              
212700     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
212800                                                                          
212900     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
213000     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
213100     MOVE TEXTRAD12                TO WS-RAD                              
213200     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
213300                                                                          
213400     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
213500     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
213600     MOVE TEXTRAD13                TO WS-RAD                              
213700     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
213800                                                                          
213900     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
214000     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
214100     MOVE TEXTRAD14                TO WS-RAD                              
214200     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
214300                                                                          
214400     MOVE SPACE                    TO WS-RAD                              
214500                                      SEND-RAD                            
214600     MOVE PRT-AFTER-9              TO PRT-RADSKIP                         
214700     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
214800     PERFORM S80-PUT-ONDEM-LINE                                           
214900     IF WEB-OUTPUT                                                        
215000       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
215100       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
215200     END-IF                                                               
215300     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
215400     PERFORM S80-PUT-ONDEM-LINE                                           
215500     IF WEB-OUTPUT                                                        
215600       MOVE WS-SKIP2               TO STYRTECKEN-RAD                      
215700       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
215800     END-IF                                                               
215900     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
216000     MOVE TEXTRAD15                TO WS-RAD                              
216100     IF WEB-OUTPUT                                                        
216200       MOVE WS-SKIP1               TO STYRTECKEN-RAD                      
216300     END-IF                                                               
216400     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
216500                                                                          
216600     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
216700     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
216800     MOVE TEXTRAD16                TO WS-RAD                              
216900     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
217000                                                                          
217100     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
217200     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
217300     MOVE TEXTRAD17                TO WS-RAD                              
217400     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
217500     .                                                                    
217600                                                                          
217700     EJECT                                                                
217800 EAEC-SKRIV-TEXT-CA SECTION.                                              
217900                                                                          
218000     MOVE SPACE                    TO SEND-RAD                            
218100     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
218200     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
218300     MOVE TEXTRAD1-C               TO WS-RAD                              
218400     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
218500                                                                          
218600     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
218700     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
218800     MOVE TEXTRAD2-C               TO WS-RAD                              
218900     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
219000                                                                          
219100     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
219200     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
219300     MOVE TEXTRAD3-C               TO WS-RAD                              
219400     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
219500     .                                                                    
219600     EJECT                                                                
219700 EAED-SKRIV-HUV-CA SECTION.                                               
219800                                                                          
219900     MOVE SPACE                    TO RAD1HC-SHIPP-TEXT                   
220000                                      RAD1HC-SHIPPER                      
220100                                      RAD2HC-SHIPPER                      
220200                                      RAD3HC-SHIPPER                      
220300                                      RAD4HC-SHIPPER                      
220400                                      RAD5HC-SHIPPER                      
220500                                      SEND-RAD                            
220600                                      WS-RAD                              
220700                                                                          
220800     MOVE WS-PAGESKIP              TO STYRTECKEN-RAD                      
220900     PERFORM S80-PUT-ONDEM-LINE                                           
221000     IF WEB-OUTPUT                                                        
221100       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
221200     END-IF                                                               
221300     MOVE WS-SKIP3                 TO STYRTECKEN-RAD                      
221400     PERFORM S80-PUT-ONDEM-LINE                                           
221500     IF WEB-OUTPUT                                                        
221600       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
221700     END-IF                                                               
221800     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
221900     PERFORM S80-PUT-ONDEM-LINE                                           
222000     IF WEB-OUTPUT                                                        
222100       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
222200     END-IF                                                               
222300     MOVE PRT-NYSIDA-RAD5          TO PRT-RADSKIP                         
222400     ADD 5                         TO WS-RAD-RAEKNARE                     
222500     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
222600     MOVE 'PROFORMA INVOICE '      TO RAD1HC-TYP-IDSHIP                   
222700     MOVE RAD-HEAD-C               TO WS-RAD                              
222800     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
222900                                                                          
223000     ADD +1                        TO WS-SIDNR                            
223100     MOVE WS-SIDNR                 TO SIDNR                               
223200                                      RAD1C-PAGE-NO                       
223300     MOVE SPACE                    TO SEND-RAD                            
223400                                      WS-RAD                              
223500                                                                          
223600     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
223700     PERFORM S80-PUT-ONDEM-LINE                                           
223800     IF WEB-OUTPUT                                                        
223900       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
224000     END-IF                                                               
224100     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
224200     PERFORM S80-PUT-ONDEM-LINE                                           
224300     IF WEB-OUTPUT                                                        
224400       PERFORM EAE-S1-SKRIV-TEXT-RAD                                      
224500     END-IF                                                               
224600     MOVE SPACE                    TO RAD1-C                              
224700                                      RAD4HC-SHIPPER                      
224800     MOVE SHIP-TISKEPPN            TO RAD1C-TIAAMMDD                      
224900     MOVE SGMT-IDDISTR             TO RAD1C-IDDISTR                       
225000     MOVE SHIP-IDSHIPM             TO RAD1C-IDSHIPM                       
225100     MOVE SHIP-IDTRPTNR            TO RAD1C-IDTRPTNR                      
225200     MOVE SHIP-IDLBBET             TO RAD1C-IDLBBET                       
225300     MOVE WS-SIDNR                 TO RAD1C-PAGE-NO                       
225400     MOVE RAD1-C                   TO SEND-RAD                            
225500                                      WS-RAD                              
225600     MOVE PRT-AFTER-3              TO PRT-RADSKIP                         
225700     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
225800     ADD 4                         TO WS-RAD-RAEKNARE                     
225900     PERFORM EAE-S1-SKRIV-TEXT-RAD                                        
226000     .                                                                    
226100                                                                          
226200     EJECT                                                                
226300 EAE-S1-SKRIV-TEXT-RAD SECTION.                                           
226400                                                                          
226500     IF WEB-OUTPUT                                                        
226600       MOVE SPACE          TO TEXT-W476NAPT                               
226700       MOVE STYRTECKEN-RAD TO TEXT-KDCARRCNTL                             
226800       MOVE WS-RAD         TO TEXT-DATA                                   
226900       PERFORM EAF5-SKRIV-WEB-TEXT                                        
227000     ELSE                                                                 
227100       MOVE WS-RAD         TO  SEND-RAD                                   
227200       PERFORM S10-PRINT-LINE                                             
227300     END-IF                                                               
227400     .                                                                    
227500                                                                          
227600     EJECT                                                                
227700 EAF1-SKRIV-WEB-HDR   SECTION.                                            
227800                                                                          
227900     MOVE 'H'                   TO HDR-IDAFPRCD                           
228000     MOVE HDR-W476NAPH  TO  WEB-DATA-AREA                                 
228100     MOVE LENGTH OF HDR-W476NAPH TO SEND-KVDLEN                           
228200     PERFORM S91-PUT-WEB-DATA                                             
228300     .                                                                    
228400                                                                          
228500 EAF2-SKRIV-WEB-SUBH  SECTION.                                            
228600                                                                          
228700     MOVE 'S'                   TO SUBH-IDAFPRCD                          
228800     MOVE SUBH-W476NAPS  TO  WEB-DATA-AREA                                
228900     MOVE LENGTH OF SUBH-W476NAPS TO SEND-KVDLEN                          
229000     PERFORM S91-PUT-WEB-DATA                                             
229100     .                                                                    
229200                                                                          
229300 EAF3-SKRIV-WEB-SUBX  SECTION.                                            
229400                                                                          
229500     MOVE 'X'                   TO SUBX-IDAFPRCD                          
229600     MOVE SUBX-W476NAPX  TO  WEB-DATA-AREA                                
229700     MOVE LENGTH OF SUBX-W476NAPX TO SEND-KVDLEN                          
229800     PERFORM S91-PUT-WEB-DATA                                             
229900     .                                                                    
230000                                                                          
230100 EAF4-SKRIV-WEB-LINE  SECTION.                                            
230200                                                                          
230300     MOVE 'L'                     TO LINE-IDAFPRCD                        
230400     MOVE LINE-W476NAPL  TO  WEB-DATA-AREA                                
230500     MOVE LENGTH OF LINE-W476NAPL TO SEND-KVDLEN                          
230600     PERFORM S91-PUT-WEB-DATA                                             
230700     .                                                                    
230800                                                                          
230900 EAF5-SKRIV-WEB-TEXT  SECTION.                                            
231000                                                                          
231100     MOVE 'T'                     TO TEXT-IDAFPRCD                        
231200     MOVE TEXT-W476NAPT  TO  WEB-DATA-AREA                                
231300     MOVE LENGTH OF TEXT-W476NAPT TO SEND-KVDLEN                          
231400     PERFORM S91-PUT-WEB-DATA                                             
231500     .                                                                    
231600                                                                          
231700     EJECT                                                                
231800 Z-FINIT SECTION.                                                         
231900                                                                          
232000     SKIP2                                                                
232100     MOVE 'S' TO POSTSUM-OPKOD                                            
232200     CALL POSTSUM USING POSTSUM-PARM                                      
232300     .                                                                    
232400     EJECT                                                                
232500 S03-INIT-PRINTER-ID SECTION.                                             
232600                                                                          
232700     MOVE SHIP-IDDC                   TO WS-IDDC                          
232800     MOVE SGMT-IDDISTR                TO TEST-IDDISTR                     
232900                                         WS-PRT-IDDISTR                   
233000     .                                                                    
233100     EJECT                                                                
233200 S07-INIT-SHIP-INDX SECTION.                                              
233300                                                                          
233400     MOVE TAB-IDDC (TAB-IX)           TO WS-IDDC                          
233500                                                                          
233600     EVALUATE TRUE                                                        
233700       WHEN NDC-US-RU                                                     
233800         MOVE +1                      TO SHIP-INDX                        
233900       WHEN NDC-US-BAT                                                    
234000         MOVE +2                      TO SHIP-INDX                        
234100       WHEN NDC-US-LA                                                     
234200         MOVE +3                      TO SHIP-INDX                        
234300       WHEN NDC-US-SE                                                     
234400         MOVE +4                      TO SHIP-INDX                        
234500       WHEN NDC-US-CH                                                     
234600         MOVE +5                      TO SHIP-INDX                        
234700       WHEN NDC-US-JA                                                     
234800         MOVE +6                      TO SHIP-INDX                        
234810       WHEN NDC-US-DA                                                     
234820         MOVE +16                     TO SHIP-INDX                        
234900       WHEN NDC-CA                                                        
235000         MOVE +7                      TO SHIP-INDX                        
235100     END-EVALUATE                                                         
235200     .                                                                    
                                                                                
       S09-PRINT-META SECTION.                                                  
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
           STRING WS-META                                                       
                  'SHIPMENT_NUMBER='                                            
                  WS-IDSHIPM-Z                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S80-PUT-ONDEM-LINE                                           
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           STRING WS-META                                                       
                  'DOCUMENT_TYPE='                                              
                  'PROFORMA INVOICE '                                           
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S80-PUT-ONDEM-LINE                                           
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-TISKEPPN      TO WS-YYMMDD                                 
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
                                                                                
           STRING WS-META                                                       
                  'SHIPPING_DATE='                                              
                  WS-YEAR(1:2)                                                  
                  WS-YYMMDD                                                     
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S80-PUT-ONDEM-LINE                                           
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
           STRING WS-META                                                       
                  'DISTRICT_NUMBER='                                            
                  WS-IDDISTR                                                    
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S80-PUT-ONDEM-LINE                                           
                                                                                
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
                  'SHIPDOC_NAP'                                                 
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
                                                                                
           PERFORM S80-PUT-ONDEM-LINE                                           
           .                                                                    
                                                                                
235300     EJECT                                                                
235400 S10-PRINT-LINE  SECTION.                                                 
235500                                                                          
235600*    -- NO "CLASSIC PRINTING" IF IT IS A WEB DC                           
235700     IF NOT WEB-OUTPUT                                                    
235800*      -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                      
235900       PERFORM S80-PUT-ONDEM-LINE                                         
236000                                                                          
236100*      -- PRINT TO PAPER IF WE ARE TRIGGERED FROM BILL-IT (W40634)        
236200*      -- OR IT IS A REPRINT (W40622)                                     
236300       IF TRPD-IDPGM = 'W4062200'                                         
236400       OR TRPD-IDPGM = 'W4063400'                                         
236500         CALL W006PRS1 USING PRT-SPOOL-OVR                                
236600                             PRT-WRITE                                    
236700                             W-IDPRTLST                                   
236800                             ALT-PCB                                      
236900                             PRT-RADSKIP                                  
237000                             WS-RAD                                       
237100       END-IF                                                             
237200     END-IF                                                               
237300                                                                          
237400     IF VCOM-SW = JA                                                      
237500     AND TRPD-IDPGM = 'W4063400'                                          
237600       PERFORM S11-SKRIV-RAD-WDR701                                       
237700     END-IF                                                               
237800     .                                                                    
237900     EJECT                                                                
238000 S11-SKRIV-RAD-WDR701  SECTION.                                           
238100                                                                          
238200     MOVE 'W476NAPR'             TO FIL-IDPGM                             
238300     MOVE DAGENS-DATUM           TO FIL-TIREGDAT                          
238400     ADD +1                      TO FIL-IDSEKVNR                          
238500     MOVE 'W476'                 TO FIL-CT-IDSYSTEM                       
238600     MOVE 'NAP'                  TO FIL-CT-IDPTYP                         
238700     MOVE SPACE                  TO FIL-CT-IDVTYP                         
238800     MOVE PRT-RADSKIP            TO SPEC-STYR                             
238900     MOVE WS-RAD                 TO SPEC-RADDA                            
239000     MOVE SPEC-RAD               TO FIL-WDR701-DATA                       
239100     PERFORM IMS-ISRT-WDR701                                              
239200     PERFORM S12-WDR7-FINNS                                               
239300     .                                                                    
239400     EJECT                                                                
239500 S12-WDR7-FINNS   SECTION.                                                
239600                                                                          
239700     IF FIL-IDSEKVNR = 999                                                
239800        MOVE ZERO            TO FIL-IDSEKVNR                              
239900        ADD +1               TO FIL-TIKLOCK                               
240000     END-IF                                                               
240100                                                                          
240200     PERFORM UNTIL SEGMENT-FINNS                                          
240300        ADD +1 TO FIL-IDSEKVNR                                            
240400        PERFORM IMS-ISRT-WDR701                                           
240500        IF FIL-IDSEKVNR = 999                                             
240600          MOVE ZERO          TO FIL-IDSEKVNR                              
240700          ADD +1             TO FIL-TIKLOCK                               
240800        END-IF                                                            
240900     END-PERFORM                                                          
241000     .                                                                    
241100     EJECT                                                                
241200                                                                          
241300 S80-PUT-ONDEM-LINE SECTION.                                              
241400                                                                          
241500     IF NOT WEB-OUTPUT                                                    
241600       IF TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1'                 
241700         IF TRPD-FLSKRIV-ONDEM = YES OR JA                                
241800           MOVE +1                        TO SEND-IDCOM                   
241900           MOVE 'PUT'                     TO SEND-KDFUNC                  
242000           MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN              
242100           CALL WZ01SEND USING SEND-CONTROL-AREA                          
242200                               SEND-KVDLEN                                
242300                               SEND-RAD-STYRTECKEN                        
242400           IF SEND-KDRC > ZERO                                            
242500             MOVE SEND-KDRC               TO KDRC-DISPLAY                 
242600             STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                 
242700             DELIMITED BY SIZE INTO FELTEXT-STR                           
242800             CALL ABEND USING RKOD-ABEND-WITH-DUMP                        
242900           END-IF                                                         
243000         END-IF                                                           
243100       END-IF                                                             
243200     END-IF                                                               
243300     .                                                                    
243400     EJECT                                                                
243500                                                                          
243600 S91-PUT-WEB-DATA SECTION.                                                
243700                                                                          
243800     MOVE +1                              TO SEND-IDCOM                   
243900     MOVE 'PUT'                           TO SEND-KDFUNC                  
244000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
244100                         SEND-KVDLEN                                      
244200                         WEB-DATA-AREA                                    
244300     IF SEND-KDRC > ZERO                                                  
244400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
244500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
244600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
244700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
244800     END-IF                                                               
244900     .                                                                    
245000                                                                          
245100                                                                          
245200     EJECT                                                                
245300*****************IMS-SECTIONER*********************                       
245400                                                                          
245500 IMS-GU-WDE101  SECTION.                                                  
245600                                                                          
245700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
245800          DELIMITED BY SIZE INTO SSA1                                     
245900     MOVE '  GE' TO GOOD-STATUSCODES                                      
246000     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
246100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
246200     PERFORM IMS-STATUSCHECK                                              
246300     .                                                                    
246400     EJECT                                                                
246500 IMS-GNP-WDE111 SECTION.                                                  
246600                                                                          
246700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
246800          DELIMITED BY SIZE INTO SSA1                                     
246900     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
247000                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
247100          DELIMITED BY SIZE INTO SSA2                                     
247200     MOVE '  GE' TO GOOD-STATUSCODES                                      
247300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
247400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
247500     PERFORM IMS-STATUSCHECK                                              
247600     .                                                                    
247700     EJECT                                                                
247800 IMS-GNP-WDE121  SECTION.                                                 
247900                                                                          
248000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
248100          DELIMITED BY SIZE INTO SSA1                                     
248200     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
248300          DELIMITED BY SIZE INTO SSA2                                     
248400     MOVE 'WDE121  '          TO SSA3                                     
248500     MOVE '  GE' TO GOOD-STATUSCODES                                      
248600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
248700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
248800     PERFORM IMS-STATUSCHECK                                              
248900     .                                                                    
249000     EJECT                                                                
249100 IMS-GNP-WDE131  SECTION.                                                 
249200                                                                          
249300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
249400          DELIMITED BY SIZE INTO SSA1                                     
249500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
249600          DELIMITED BY SIZE INTO SSA2                                     
249700     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
249800          DELIMITED BY SIZE INTO SSA3                                     
249900     MOVE 'WDE131  '          TO SSA4                                     
250000     MOVE '  GE' TO GOOD-STATUSCODES                                      
250100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
250200                                                   SSA3 SSA4              
250300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
250400     PERFORM IMS-STATUSCHECK                                              
250500     .                                                                    
250600     EJECT                                                                
250700 IMS-GU-WDB201 SECTION.                                                   
250800                                                                          
250900     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
251000          DELIMITED BY SIZE INTO SSA1                                     
251100     MOVE '  GE'              TO GOOD-STATUSCODES                         
251200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
251300     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
251400     PERFORM IMS-STATUSCHECK                                              
251500     .                                                                    
251600     EJECT                                                                
251700 IMS-GU-WDB101 SECTION.                                                   
251800                                                                          
251900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
252000          DELIMITED BY SIZE INTO SSA1                                     
252100     MOVE '  GE'              TO GOOD-STATUSCODES                         
252200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
252300     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
252400     PERFORM IMS-STATUSCHECK                                              
252500     .                                                                    
252600     EJECT                                                                
252700 IMS-GU-WDK711 SECTION.                                                   
252800                                                                          
252900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
253000          DELIMITED BY SIZE INTO SSA1                                     
253100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
253200          DELIMITED BY SIZE INTO SSA2                                     
253300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
253400     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
253500     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
253600     PERFORM IMS-STATUSCHECK                                              
253700     .                                                                    
253800     SKIP3                                                                
253900 IMS-GU-WDD311    SECTION.                                                
254000                                                                          
254100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
254200            DELIMITED BY SIZE INTO SSA1                                   
254300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
254400            DELIMITED BY SIZE INTO SSA2                                   
254500     MOVE '  ' TO GOOD-STATUSCODES                                        
254600     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
254700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
254800     PERFORM IMS-STATUSCHECK                                              
254900     .                                                                    
255000     EJECT                                                                
255100 IMS-ISRT-WDR701 SECTION.                                                 
255200                                                                          
255300     MOVE 'WDR701 '              TO SSA1                                  
255400     MOVE '  II'                 TO GOOD-STATUSCODES                      
255500     CALL CBLTDLI USING ISRT WDR7-PCB DLI-IO-WDR701 SSA1                  
255600     MOVE WDR7-STATUS-CODE       TO STATUS-WS                             
255700     PERFORM IMS-STATUSCHECK                                              
255800     .                                                                    
255900     EJECT                                                                
256000 IMS-GU-WDB601 SECTION.                                                   
256100*                                                                         
256200     STRING 'WDB601  (IDDC     =' W-IDDC-WDB6-X ')'                       
256300          DELIMITED BY SIZE INTO SSA1                                     
256400     MOVE '  GE'              TO GOOD-STATUSCODES                         
256500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
256600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
256700     PERFORM IMS-STATUSCHECK                                              
256800     .                                                                    
256900     EJECT                                                                
257000 IMS-STATUSCHECK SECTION.                                                 
257100                                                                          
257200     SET STATUS-IX TO 1                                                   
257300     SEARCH GOOD-STATUS                                                   
257400       AT END                                                             
257500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
257600           DELIMITED BY SIZE INTO FELTEXT                                 
257700         CALL FELLOG                                                      
257800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
257900         CONTINUE                                                         
258000     END-SEARCH                                                           
259000     .                                                                    
