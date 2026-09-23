000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476KLIS.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/04/24.                                                
000500                                                                          
000600*    FUNCTION:                                                            
000700*        SUBPROGRAM TO WRITE 'CARGO SPECIFICATION' TRANSPORT              
000800*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT SHOW        
000900*        EACH DEALER,ADDRESS AND EACH KOLLI. FOR EACH KOLLI IT            
001000*        SHOWS PACKAGE SIZE,WEIGHT,VOLUME AND IS THAT CONTAINS            
001100*        HAZARDOUS GOODS. AFTER EACH DEALER IT SHOWS TOTAL LINE           
001200*        FOR TOTAL KOLLI,WEIGHT,VOLUME,VALUE.AT THE END OF THE            
001300*        DOCUMENT IT SHOWS THE TOTAL FOR THE FULL SHIPMENT/DIST           
001400*                                                                         
001500*        THE PROGRAM READS     WDE1                                       
001600*        THE PROGRAM READS     WDQ2                                       
001700*        THE PROGRAM READS     WDR1                                       
001800*                                                                         
001900*    ABENDCODES:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*                                                                         
002400*     2019-04                                                             
002500*     FÖR TURKIET  VISAR VI INGA 'VALUE´ (=PENGAR)                        
002600*     VARKEN PÅ DETALJ- ELLER NÅGON AV TOTALRADERNA                       
002700*     SAMMA FÖR KINA REFILL.                                              
002800*     FÖR INDIEN VISAS EJ VALUE PÅ DETALJRAD.                             
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)   VALUE 'W476KLIS'.             
004500 77  YES                         PIC X      VALUE 'J'.                    
004600 77  NOO                         PIC X      VALUE 'N'.                    
004610 77  CROSSDOC-YES                PIC X      VALUE 'Y'.                    
004700 77  WS-IDDC-11                  PIC X(2)   VALUE '11'.                   
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  WS-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  WS-SKLI-IX                  PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  WS-SKLI-IX-MAX              PIC S9(4)  VALUE +500  COMP SYNC.        
005200 77  WS-GMT-IDTFN                PIC X(20)  VALUE SPACE.                  
005300 77  FLSAMFAK-SW                 PIC X(01)  VALUE 'N'.                    
005400     88 FLSAMFAK                            VALUE 'J'.                    
005500 77  WEB-OUTPUT-SW               PIC X(01)  VALUE 'N'.                    
005600     88 WEB-OUTPUT                          VALUE 'J'.                    
005700                                                                          
005800 77  FLLOCCUR-SW                 PIC X(01)  VALUE 'N'.                    
005900     88 FLLOCCUR                            VALUE 'J'.                    
006000                                                                          
006100 77  TRAFF-SKLI-SW               PIC X(01)  VALUE 'N'.                    
006200     88 TRAFF-SKLI                          VALUE 'J'.                    
006300                                                                          
006400 77  VOR-CDC-11                  PIC X(01)  VALUE 'N'.                    
006500     88 VOR-OK                              VALUE 'J'.                    
006600                                                                          
006700 77  W-CS4-NIV                   PIC S9(3)  COMP-3 VALUE ZERO.            
006800 77  W-PAGE-NO                   PIC S9(3)  COMP-3 VALUE ZERO.            
006900 77  WS-LDC-HDR                  PIC S9(3)  COMP-3 VALUE ZERO.            
007000 77  W-DATE-AAMM                 PIC 9(4)   VALUE ZERO.                   
007100 77  WS-KDVALISO-HUV             PIC X(3)   VALUE 'SEK'.                  
007200 77  SHIP-INDX                   PIC S9(9)  VALUE +15.                    
007300                                                                          
007400     EJECT                                                                
007500 01  W-YYMMDD                    PIC 9(06)  VALUE ZERO.                   
007600 01  CURR-SECTION                PIC X(32)  VALUE SPACE.                  
007700                                                                          
007800*--- SAMLINGSKOLLITABELL                                                  
007900 01  WS-SKLI-TABELL.                                                      
008000   03  WS-SKLI-TABSTEG OCCURS 500.                                        
008100     05  WS-IDKOLLI-SAMP      PIC S9(5)  VALUE +0.                        
008200   03  WS-ANT-KOLLI-I-ALLA-SK PIC S9(9)  VALUE +0.                        
008300   03  WS-ANT-SK              PIC S9(5)  VALUE +0.                        
008400                                                                          
008500 01  WS-ANT-SINGLE-KOLLI      PIC S9(9)  VALUE +0.                        
008600                                                                          
008700                                                                          
008800*01  -COPY WWDC99                                                         
008900                                                                          
009000 01  FILLER                        PIC X(16)  VALUE 'WNDCADRE '.          
009100*   -COPY WNDCADRE                                                        
009200     EJECT                                                                
009300*    --- STYRTECKEN PRINTER                                               
009400 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
009500 01  WS-SKIP1                      PIC X      VALUE ' '.                  
009600 01  WS-SKIP2                      PIC X      VALUE '0'.                  
009700 01  WS-SKIP3                      PIC X      VALUE '-'.                  
009800                                                                          
009900 01  WS-TYP-IDSHIP                 PIC X(19) VALUE                        
010000                                   'CARGO SPECIFICATION'.                 
010100 01  W-LINE.                                                              
010200     03  W-LINE-COUNT            PIC 9(02)  VALUE ZERO.                   
010300     03  W-LINE-MAX              PIC 9(02)  VALUE 43.                     
010400     03  W-LINE-HEAD             PIC 9(02)  VALUE 5.                      
010500     03  W-LINE-GRTOTAL          PIC 9(02)  VALUE 2.                      
010600     03  W-LINE-SAMKOLLI         PIC 9(02)  VALUE 6.                      
010700                                                                          
010800 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
010900 01  FILLER REDEFINES TODAYS-DATE.                                        
011000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
011100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
011200     03  TODAYS-DATE-DAY         PIC 9(2).                                
011300     EJECT                                                                
011400                                                                          
011500 01  WS-META                       PIC X(5) VALUE '¤META'.                
011600 01  WS-IDDISTR                    PIC Z(4)9.                             
011700 01  WS-IDSHIPM-Z                  PIC Z(6)9.                             
011800 01  WS-TIMESTAMP.                                                        
011900     03  FILLER                  PIC X       VALUE 'D'.                   
012000     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
012100     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
012200     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
012300     03  FILLER                  PIC X       VALUE '_'.                   
012400     03  FILLER                  PIC X       VALUE 'T'.                   
012500     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
012600     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
012700     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
012800                                                                          
012900 01  ARB-RAD                     PIC X(132)  VALUE SPACE.                 
013000                                                                          
013100 01  RAD-HEAD.                                                            
013200     03  FILLER                    PIC X(15).                             
013300     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
013400     03  FILLER                    PIC X(2).                              
013500     03  RAD1H-IMPORTER            PIC X(35).                             
013600     03  FILLER                    PIC X(2).                              
013700**   03  FILLER                    PIC X(67).                             
013800     03  RAD-TYP-IDSHIP            PIC X(25).                             
013900                                                                          
014000 01  RAD2-HEAD.                                                           
014100     03  FILLER                    PIC X(30).                             
014200     03  RAD2H-IMPORTER            PIC X(35).                             
014300                                                                          
014400 01  RAD3-HEAD.                                                           
014500     03  FILLER                    PIC X(30).                             
014600     03  RAD3H-IMPORTER            PIC X(35).                             
014700                                                                          
014800 01  RAD5-HEAD.                                                           
014900     03  FILLER                    PIC X(30).                             
015000     03  RAD5H-IMPORTER            PIC X(35).                             
015100                                                                          
015200 01  RAD1.                                                                
015300     03  FILLER                    PIC X(30).                             
015400     03  RAD4H-IMPORTER            PIC X(35).                             
015500     03  FILLER                    PIC X(02).                             
015600**   03  FILLER                    PIC X(67).                             
015700     03  RAD1-TIAAMMDD             PIC 9(06).                             
015800     03  RAD1-IDDISTR              PIC Z(4)9.                             
015900     03  FILLER                    PIC X(01).                             
016000     03  RAD1-IDSHIPM              PIC Z(06)9.                            
016100     03  FILLER                    PIC X(04).                             
016200     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
016300     03  FILLER                    PIC X(01).                             
016400     03  RAD1-IDLBBET              PIC X(12).                             
016500     03  FILLER                    PIC X(02).                             
016600     03  RAD1-PAGE-NO              PIC Z(03).                             
016700                                                                          
016800 01  RAD2.                                                                
016900     03 FILLER             PIC X(8)    VALUE SPACE.                       
017000     03 RAD2-IDKUNDNR-LEDTEXT                                             
017100                           PIC X(6).                                      
017200     03 FILLER             PIC X(01).                                     
017300     03 RAD2-IDKUNDNR      PIC Z(7).                                      
017400     03 FILLER             PIC X(2)    VALUE SPACE.                       
017500     03 RAD2-BEGMT1        PIC X(35).                                     
017600     03 FILLER             PIC X(2)    VALUE SPACE.                       
017700     03 RAD2-ADGMT-GATA    PIC X(35).                                     
017800                                                                          
017900 01  RAD3.                                                                
018000     03 FILLER             PIC X(24)   VALUE SPACE.                       
018100     03 RAD3-BEGMT2        PIC X(35).                                     
018200     03 FILLER             PIC X(2)    VALUE SPACE.                       
018300     03 RAD3-ADGMT-PADR    PIC X(35).                                     
018400                                                                          
018500 01  RAD4.                                                                
018600     03 FILLER             PIC X(61)   VALUE SPACE.                       
018700     03 RAD4-ADGMT-LAND    PIC X(35).                                     
018800                                                                          
018900 01  RAD4B.                                                               
019000     03 FILLER              PIC X(49)   VALUE SPACE.                      
019100     03 RAD4B-PHONE-LEDTEXT PIC X(10).                                    
019200     03 FILLER              PIC X(2)    VALUE SPACE.                      
019300     03 RAD4B-IDTFN         PIC X(20).                                    
019400     03 FILLER              PIC X(15)   VALUE SPACE.                      
019500                                                                          
019600 01  RAD5.                                                                
019700     03  FILLER                   PIC X(1).                               
019800     03  RAD5-IDORDNR7-IDKOLLI-X  PIC X(11).                              
019900     03  FILLER                   PIC X(2).                               
020000     03  RAD5-KDORDKL-X           PIC X(2).                               
020100     03  FILLER                   PIC X(2).                               
020200     03  RAD5-BEEMBTYP-X          PIC X(12).                              
020300     03  FILLER                   PIC X(1).                               
020400     03  RAD5-DIKOLLIL-X          PIC X(5).                               
020500     03  FILLER                   PIC X(1).                               
020600     03  RAD5-DIKOLLIB-X          PIC X(3).                               
020700     03  FILLER                   PIC X(1).                               
020800     03  RAD5-DIKOLLIH-X          PIC X(3).                               
020900     03  FILLER                   PIC X(1).                               
021000     03  RAD5-VKORDBTO-X          PIC X(8).                               
021100     03  FILLER                   PIC X(1).                               
021200     03  FILLER                   PIC X(2).                               
021300     03  RAD5-VKORDNTO-X          PIC X(8)  JUSTIFIED RIGHT.              
021400     03  FILLER                   PIC X(1).                               
021500     03  RAD5-VLORDBTO-X          PIC X(8)  JUSTIFIED RIGHT.              
021600     03  FILLER                   PIC X(5).                               
021700     03  RAD5-SUORDV-X            PIC X(8)  JUSTIFIED RIGHT.              
021800     03  FILLER                   PIC X(1).                               
021900     03  RAD5-KDVALISO-X          PIC X(08).                              
022000     03  FILLER                   PIC X(1).                               
022100**   03  FILLER                   PIC X(23).                              
022200     03  FILLER                   PIC X(21).                              
022300                                                                          
022400 01  RAD6.                                                                
022500     03  FILLER                   PIC X(1).                               
022600     03  RAD6-IDORDNR7-IDKOLLI-X.                                         
022700       05  RAD6-IDORDNR7-X.                                               
022800         07  RAD6-IDORDNR7        PIC Z(5)9.                              
022900       05  RAD6-IDKOLLI-SEP       PIC X(1)  VALUE '-'.                    
023000       05  RAD6-IDKOLLI-X.                                                
023100         07  RAD6-IDKOLLI         PIC Z9(4).                              
023200     03  FILLER                   PIC X(2).                               
023300     03  RAD6-KDORDKL             PIC X(1).                               
023400     03  FILLER                   PIC X(2).                               
023500     03  RAD6-BEEMBTYP            PIC X(12).                              
023600     03  FILLER                   PIC X(1).                               
023700     03  RAD6-DIKOLLIL-X.                                                 
023800       05  RAD6-DIKOLLIL          PIC Z(5).                               
023900     03  FILLER                   PIC X(1).                               
024000     03  RAD6-DIKOLLIB-X.                                                 
024100       05  RAD6-DIKOLLIB          PIC Z(3).                               
024200     03  FILLER                   PIC X(1).                               
024300     03  RAD6-DIKOLLIH-X.                                                 
024400       05  RAD6-DIKOLLIH          PIC Z(3).                               
024500     03  FILLER                   PIC X(1).                               
024600     03  RAD6-VKORDBTO-W.                                                 
024700       05  RAD6-VKORDBTO          PIC Z(5)9.9.                            
024800     03  FILLER  REDEFINES RAD6-VKORDBTO-W.                               
024900       05  RAD6-VKORDBTO-X        PIC X(8)  JUSTIFIED RIGHT.              
025000     03  FILLER                   PIC X(1).                               
025100     03  RAD6-VKORDNTO-W.                                                 
025200***    05  RAD6-VKORDNTO          PIC Z(5)9.9.                            
025300       05  RAD6-VKORDNTO          PIC Z(5)9.999.                          
025400     03  FILLER  REDEFINES RAD6-VKORDNTO-W.                               
025500***    05  RAD6-VKORDNTO-X        PIC X(8)  JUSTIFIED RIGHT.              
025600       05  RAD6-VKORDNTO-X        PIC X(10) JUSTIFIED RIGHT.              
025700     03  FILLER                   PIC X(1).                               
025800     03  RAD6-VLORDBTO-W.                                                 
025900       05  RAD6-VLORDBTO          PIC Z(3)9.999.                          
026000     03  FILLER  REDEFINES RAD6-VLORDBTO-W.                               
026100       05  RAD6-VLORDBTO-X        PIC X(8)  JUSTIFIED RIGHT.              
026200     03  FILLER                   PIC X(1).                               
026300     03  RAD6-SUORDV-W.                                                   
026400       05  RAD6-SUORDV            PIC Z(8)9.99.                           
026500     03  FILLER  REDEFINES RAD6-SUORDV-W.                                 
026600       05  FILLER                 PIC X(4).                               
026700       05  RAD6-SUORDV-X          PIC X(8)  JUSTIFIED RIGHT.              
026800     03  FILLER  REDEFINES RAD6-SUORDV-W.                                 
026900       05  RAD6-SUORDV-BLANK      PIC Z(8)9.99 BLANK WHEN ZERO.           
027000     03  FILLER                   PIC X(1).                               
027100     03  RAD6-KDVALISO-X.                                                 
027200       05  RAD6-KDVALISO          PIC X(3).                               
027300     03  FILLER                   PIC X(2).                               
027400     03  RAD6-FARLIG.                                                     
027500       05 RAD6-BEFARLIG           PIC X(17).                              
027600       05 FILLER                  PIC X(1).                               
027700       05 RAD6-KVFLAMP-X          PIC X(6).                               
027800       05 RAD6-KVFLAMP            PIC ZZ9.                                
027900     03  FILLER REDEFINES RAD6-FARLIG.                                    
028000       05 FILLER                  PIC X(13).                              
028100       05 RAD6-BEFARLIG-SDC       PIC X(4).                               
028200       05 FILLER                  PIC X(1).                               
028300       05 RAD6-KVFLAMP-X-SDC      PIC X(6).                               
028400       05 RAD6-KVFLAMP-SDC        PIC ZZ9.                                
028410     03  FILLER                   PIC X(1).                               
028420     03  RAD6-FLCROSS             PIC X(1).                               
028500                                                                          
028600 01  RAD7.                                                                
028700     03  FILLER                   PIC X(01).                              
028800     03  RAD7-TOTAL-TEXT          PIC X(12).                              
028900     03  FILLER                   PIC X(1)     VALUE  ':'.                
029000     03  RAD7-KOLLI-TOTAL-X.                                              
029100         05  RAD7-KOLLI-TOTAL     PIC Z(6)9.                              
029200     03  FILLER                   PIC X(01).                              
029300     03  RAD7-KOLLI-TEXT          PIC X(7).                               
029400     03  FILLER                   PIC X(11).                              
029500     03  RAD7-TOTAL-VKORDBTO-W.                                           
029600       05  RAD7-TOTAL-VKORDBTO    PIC Z(5)9.9.                            
029700     03  FILLER                   PIC X(1).                               
029800     03  RAD7-TOTAL-VKORDNTO-W.                                           
029900*******05  RAD7-TOTAL-VKORDNTO    PIC Z(5)9.9.                            
030000       05  RAD7-TOTAL-VKORDNTO    PIC Z(5)9.999.                          
030100     03  FILLER                   PIC X(1).                               
030200     03  RAD7-TOTAL-VLORDBTO-W.                                           
030300       05  RAD7-TOTAL-VLORDBTO    PIC Z(3)9.999.                          
030400     03  FILLER                   PIC X(1).                               
030500     03  RAD7-TOTAL-SUORDV-W.                                             
030600       05  RAD7-TOTAL-SUORDV      PIC Z(8)9.99.                           
030700     03  FILLER                   PIC X(1).                               
030800     03  RAD7-TOTAL-KDVALISO-X.                                           
030900       05  RAD7-TOTAL-KDVALISO    PIC X(8).                               
031000                                                                          
031100 01  RAD8.                                                                
031200     03  FILLER                   PIC X(01).                              
031300     03  RAD8-TOTAL-TEXT          PIC X(25).                              
031400                                                                          
031500 01  RAD9.                                                                
031600     03  FILLER                   PIC X(01).                              
031700     03  RAD9-DETAIL-TEXT         PIC X(07) VALUE 'DETAILS'.              
031800                                                                          
031900 01  RAD10.                                                               
032000     03  FILLER                   PIC X(01).                              
032100     03  FILLER                   PIC X(13).                              
032200     03  RAD10-ANT-SINGLE-PACK    PIC Z(4)9.                              
032300     03  RAD10-SINGLE-PACK-TEXT   PIC X(15) VALUE                         
032400                                  ' SINGLE PACKAGE'.                      
032500                                                                          
032600 01  RAD11.                                                               
032700     03  FILLER                   PIC X(01).                              
032800     03  FILLER                   PIC X(13).                              
032900     03  RAD11-ANT-MIXED-PACK     PIC Z(4)9.                              
033000     03  RAD11-MIXED-PACK-TEXT    PIC X(25) VALUE                         
033100                                  ' MIXED PACKAGE INCLUDING '.            
033200     03  RAD11-ANT-SINGLE-INCL    PIC Z(4)9.                              
033300     03  RAD11-SINGLE-INCL-TEXT   PIC X(15) VALUE                         
033400                                  ' SINGLE PACKAGE'.                      
033500                                                                          
033600 01  RAD12.                                                               
033700     03  FILLER                   PIC X(01).                              
033800     03  FILLER                   PIC X(18).                              
033900     03  RAD12-MIXED-PACK-TEXT    PIC X(50) VALUE                         
034000       ' MIXED PACKAGE WRAPPING INCLUDED IN DISTRICT TOTAL'.              
034100                                                                          
034200 01  RADY.                                                                
034300     03 FILLER                    PIC X(32).                              
034400     03 RADY-TEXT                 PIC X(30).                              
034500     03 FILLER                    PIC X(5).                               
034600     03 RADY-DIFF                 PIC -(8)9.99.                           
034700                                                                          
034800*          COPYTEXTER  TILL WEB-DC                                        
034900 01  WSKRIV-CS1                   PIC X(01)  VALUE 'J'.                   
035000**                                                                        
035100 01  WEB-AREOR.                                                           
035200     03  DOC-AREA                 PIC X(250)  VALUE SPACES.               
035300*    03  -COPY  W476CS1                                                   
035400                                                                          
035500*    03  -COPY  W476CS2                                                   
035600                                                                          
035700*    03  -COPY  W476CS3                                                   
035800                                                                          
035900*    03  -COPY  W476CS4                                                   
036000                                                                          
036010*    03  -COPY  W476CS5                                                   
036020                                                                          
036100                                                                          
036200 77  W-KDSPRAK                 PIC S9    VALUE +2  COMP-3.                
036300 77  W-SUORDV                  PIC S9(9)V9(2) VALUE ZERO COMP-3.          
036400 77  W-SUORDV-EXP              PIC S9(9)V9(2) VALUE ZERO COMP-3.          
036500 77  W-KDVALISO                PIC X(3)    VALUE SPACE.                   
036600 77  DUMMY-AREA                PIC X(50)   VALUE SPACE.                   
036700                                                                          
036800 01  W-NEW-DOC-FLAG              PIC X(01)   VALUE 'N'.                   
036900     88  W-PRINT-NEW-DOC                     VALUE 'J'.                   
037000                                                                          
037100 01  W-TOTAL-FLAG                PIC X(01)   VALUE 'N'.                   
037200     88  W-PRINT-TOTAL                       VALUE 'J'.                   
037300                                                                          
037400 01  W-GRTOTAL-FLAG              PIC X(01)   VALUE 'N'.                   
037500     88  W-PRINT-GRTOTAL                     VALUE 'J'.                   
037600                                                                          
037700 01  W-PRINT-DEALER-FLAG         PIC X(01)   VALUE 'N'.                   
037800     88  W-PRINT-DEALER                      VALUE 'J'.                   
037900                                                                          
038000 01  WS-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
038100 01  WY-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
038200 01  WS-REVALUTA-LOCCUR          PIC S9(5)      COMP-3 VALUE 1.           
038300 01  WS-PRKURS-LOCCUR            PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
038400 01  WS-PRKURS-EXCH              PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
038500                                                                          
038600 01  W-VKORDNTO-KOLLI            PIC S9(6)V9(3).                          
038700                                                                          
038800 01  W-TOT.                                                               
038900     03  W-TOTAL   OCCURS 2.                                              
039000         05  W-TOTAL-SUORDV            PIC S9(9)V9(2).                    
039100         05  W-TOTAL-SUORDV-EXP        PIC S9(9)V9(2).                    
039200         05  W-TOTAL-VKORDBTO          PIC S9(6)V9(1).                    
039300*********05  W-TOTAL-VKORDNTO          PIC S9(6)V9(1).                    
039400         05  W-TOTAL-VKORDNTO          PIC S9(6)V9(3).                    
039500         05  W-TOTAL-VLORDBTO          PIC S9(6)V9(3).                    
039600         05  W-TOTAL-KOLLI-COUNT       PIC S9(6).                         
039700     03  W-TOTAL-SUORDV-EXCH           PIC S9(9)V9(2).                    
039710*                                                                         
039720 01  W-TOT-CD.                                                            
039730     03  W-TOTAL-SUORDV-CD             PIC S9(9)V9(2).                    
039740     03  W-TOTAL-SUORDV-EXP-CD         PIC S9(9)V9(2).                    
039750     03  W-TOTAL-VKORDNTO-CD           PIC S9(6)V9(3).                    
039760     03  W-TOTAL-VKORDBTO-CD           PIC S9(6)V9(3).                    
039770     03  W-TOTAL-VLORDBTO-CD           PIC S9(6)V9(3).                    
039780     03  W-TOTAL-KOLLI-CNT-CD          PIC S9(6).                         
039790                                                                          
039800*                                                                         
039900 01  W-TOTAL-VLORDBTO-JP               PIC S9(6)V9(3) VALUE ZERO.         
040000*                                                                         
040100 01  W-FARLIG-TEXT-SDC22        PIC X(4)   VALUE 'DANG'.                  
040200 01  W-FARLIG-TEXT-SDC24        PIC X(4)   VALUE 'PER.'.                  
040300 01  W-FARLIG-TEXT-SDC25        PIC X(4)   VALUE 'PER.'.                  
040400*                                                                         
040500 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
040600*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
040700*                                                                         
040800*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
040900*                                                                         
041000*01  FILLER   -COPY WWDIS121    -RED TEST-IDDISTR.                        
041100                                                                          
041200 01  WS-IDDISTR-EXCH             REDEFINES TEST-IDDISTR                   
041300                                           PIC 9(5) COMP-3.               
041400     88  EXCH-IDDISTR-TURKIET             VALUE 5810 5811.                
041500                                                                          
041600     EJECT                                                                
041700                                                                          
041800 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
041900* 01 -COPY W475W552                                                       
042000     EJECT                                                                
042100* 01 -COPY W475W553                                                       
042200     EJECT                                                                
042300* 01 -COPY W476W001                                                       
042400     EJECT                                                                
042500                                                                          
042600 01  GENERAL-SUBPROGRAMS.                                                 
042700*                                                                         
042800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
042900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
043000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
043100     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
043200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
043300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
043400     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
043500     SKIP2                                                                
043600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
043700                                                                          
043800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
043900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
044000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
044100     SKIP2                                                                
044200 01  ERRTEXT.                                                             
044300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
044400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
044500 77  KDRC-DISPLAY                PIC Z(5).                                
044600     EJECT                                                                
044700*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
044800*01  -COPY W510CURR                                                       
044900     EJECT                                                                
045000*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
045100     EJECT                                                                
045200*01  -COPY W006PRAR                                                       
045300 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
045400*                                        WRITE ON NEW LINE 10             
045500 01  W-IDPRTLST                  PIC X(8).                                
045600                                                                          
045700     EJECT                                                                
045800 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
045900                                                                          
046000 01  WS-CP-SVE-EBCDIC            PIC X(5)  VALUE '278  '.                 
046100 01  WS-CP-CHN-EBCDIC            PIC X(5)  VALUE '935  '.                 
046200*01  -COPY WTRAUTF8                                                       
046300                                                                          
046400     EJECT                                                                
046500*    --- AREAS FOR IMS-SECTIONS                                           
046600*                                                                         
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046900     SKIP3                                                                
047000 01  KEYS-TO-DLI.                                                         
047100                                                                          
047200     03  W-IDSHIPM-X.                                                     
047300         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
047400                                                                          
047500     03  W-WDE111KY-X.                                                    
047600         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
047700         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
047800                                                                          
047900     03  W-WDE111KY-MIN.                                                  
048000         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
048100         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
048200                                                                          
048300     03  W-WDE111KY-MAX.                                                  
048400         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
048500         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
048600                                                                          
048700     03  W-WDE121KY-X.                                                    
048800         05  W-WDE121-IDPRODNR   PIC S9(07)  VALUE ZERO COMP-3.           
048900         05  W-WDE121-IDKOLLI    PIC S9(05)  VALUE ZERO COMP-3.           
049000                                                                          
049100                                                                          
049200     03  W-IDORDER-X.                                                     
049300         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
049400                                                                          
049500     03  W-WDGXKEY-4738-X.                                                
049600         05  FILLER              PIC X(4)     VALUE '4738'.               
049700         05  W-KDEMBTYP          PIC S9(3)    VALUE +0  COMP-3.           
049800         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
049900                                                                          
050000     03  W-WDB101KY-X.                                                    
050100         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
050200         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
050300                                                                          
050400     03  W-WDB201KY-X.                                                    
050500         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
050600         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
050700                                                                          
050800     03  W-WDE7ASEQ-X.                                                    
050900         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
051000         05 W-IDKOLLI-SAMP       PIC S9(5)   VALUE ZERO  COMP-3.          
051100                                                                          
051200                                                                          
051300     SKIP2                                                                
051400*    --- STATUS-KOD FRÅN IMS                                              
051500 01  STATUS-WS                   PIC XX.                                  
051600     88  SEGMENT-FOUND                       VALUE '  '.                  
051700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
051800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
051900     SKIP2                                                                
052000 01  GOOD-STATUSCODES.                                                    
052100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052200     SKIP3                                                                
052300 01  SSA1                        PIC X(64).                               
052400 01  SSA2                        PIC X(64).                               
052500 01  SSA3                        PIC X(64).                               
052600 01  SSA4                        PIC X(64).                               
052700     EJECT                                                                
052800                                                                          
052900 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
053000 01  SEND-AREA.                                                           
053100*    03  -COPY WZ01SEND                                                   
053200                                                                          
053300 01  SEND-RAD-STYRTECKEN.                                                 
053400     03  STYRTECKEN-RAD          PIC X.                                   
053500     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
053600                                                                          
053700 01  DAP-AREA-START              PIC X(24)   VALUE                        
053800                                             'DAP-AREA-START'.            
053900                                                                          
054000 01  FILLER                 PIC X(16)   VALUE 'HDR-AREA'.                 
054100 01  HDR-AREA.                                                            
054200*    03  -COPY WZ01REQU  -PRE HDR-                                        
054300*    03  -COPY WZ04HDR                                                    
054400                                                                          
054500*    --- IMS FUNCTION CODES                                               
054600*01  -COPY W0003                                                          
054700     EJECT                                                                
054800*    ---  DLI INPUT-OUTPUT AREA                                           
054900 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
055000 01  DLI-IO-WDE101-11.                                                    
055100     03  DLI-IO-WDE101.                                                   
055200*        05  -COPY WDE101                                                 
055300     03  DLI-IO-WDE111.                                                   
055400*        05  -COPY WDE111                                                 
055500     EJECT                                                                
055600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
055700 01  DLI-IO-WDE121.                                                       
055800*    03  -COPY WDE121                                                     
055900     EJECT                                                                
056000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
056100 01  DLI-IO-WDE131.                                                       
056200*    03  -COPY WDE131                                                     
056300     EJECT                                                                
056400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
056500 01  DLI-IO-WDQ201.                                                       
056600*    03  -COPY WDQ201                                                     
056700     EJECT                                                                
056800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4738'.                    
056900 01  DLI-IO-WDGX4738.                                                     
057000*    03  -COPY WDGX4738                                                   
057100     EJECT                                                                
057200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
057300 01  DLI-IO-WDB101.                                                       
057400*    03  -COPY WDB101                                                     
057500     EJECT                                                                
057600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
057700 01  DLI-IO-WDB201.                                                       
057800*    03  -COPY WDB201                                                     
057900     EJECT                                                                
058000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE711'.                      
058100 01  DLI-IO-WDE711.                                                       
058200*    03  -COPY WDE711                                                     
058300                                                                          
058400     EJECT                                                                
058500                                                                          
058600 LINKAGE SECTION.                                                         
058700                                                                          
058800*01  -COPY W476TRPD                                                       
058900                                                                          
059000 01  ALT-PCB                     PIC X(32).                               
059100                                                                          
059200*01  -COPY W0008  -PRE WDE1-                                              
059300     05  FILLER                  PIC X.                                   
059400                                                                          
059500*01  -COPY W0008  -PRE WDQ2-                                              
059600     05  FILLER                  PIC X.                                   
059700                                                                          
059800*01  -COPY W0008  -PRE WDR1-                                              
059900     05  FILLER                  PIC X.                                   
060000                                                                          
060100*01  -COPY W0008  -PRE WDB1-                                              
060200     05  FILLER                  PIC X.                                   
060300                                                                          
060400*01  -COPY W0008  -PRE WDB2-                                              
060500     05  FILLER                  PIC X.                                   
060600                                                                          
060700*01  -COPY W0008  -PRE WDG2-                                              
060800     05  FILLER                  PIC X.                                   
060900                                                                          
061000*01  -COPY W0008  -PRE WDE7-                                              
061100     05  FILLER                  PIC X.                                   
061200                                                                          
061300     EJECT                                                                
061400 PROCEDURE DIVISION  USING TRPD-W476TRPD ALT-PCB                          
061500                           WDE1-PCB WDQ2-PCB WDR1-PCB                     
061600                           WDB2-PCB WDB1-PCB WDG2-PCB                     
061700                           WDE7-PCB.                                      
061800 MAIN SECTION.                                                            
061900     ENTRY 'DLITCBL' USING TRPD-W476TRPD ALT-PCB                          
062000                           WDE1-PCB WDQ2-PCB WDR1-PCB                     
062100                           WDB2-PCB WDB1-PCB WDG2-PCB                     
062200                           WDE7-PCB.                                      
062300                                                                          
062400     PERFORM A-INIT                                                       
062500                                                                          
062600     PERFORM UNTIL NOT SEGMENT-FOUND                                      
062700       MOVE SGMT-PRKURS           TO WS-PRKURS-LOCCUR                     
062800       IF WS-IX = ZERO                                                    
062900         PERFORM B-IMPORTER                                               
063000       END-IF                                                             
063100       PERFORM B-DEALER-DETAIL                                            
063200       PERFORM IMS-GNP-WDE111-DIST                                        
063300     END-PERFORM                                                          
063400                                                                          
063500     IF W-PRINT-GRTOTAL                                                   
063600       PERFORM S24-PRINT-GRTOTAL                                          
063700     END-IF                                                               
063800                                                                          
063900     MOVE ZERO TO RETURN-CODE                                             
064000     GOBACK                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 A-INIT SECTION.                                                          
064400                                                                          
064500     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
064600     ACCEPT TODAYS-DATE  FROM DATE                                        
064700                                                                          
065500     MOVE TRPD-IDPRTLST   TO W-IDPRTLST                                   
065600     MOVE TRPD-IDSHIPM    TO W-IDSHIPM                                    
065700     MOVE TRPD-PFDEF-OVR  TO PRT-PFDEF-OVR                                
065800     MOVE TRPD-IDDISTR    TO W-WDE111-IDDISTR                             
065900                             W-WDB201-IDDISTR                             
066000                             W-WDE111-IDDISTR-MIN                         
066100                             W-WDE111-IDDISTR-MAX                         
066200                             TEST-IDDISTR                                 
066300                                                                          
066400     MOVE NOO             TO W-NEW-DOC-FLAG                               
066500                             W-PRINT-DEALER-FLAG                          
066600                             W-TOTAL-FLAG                                 
066700                             VOR-CDC-11                                   
066800                                                                          
066900     MOVE 2               TO W-KDSPRAK                                    
067000     MOVE ZERO            TO W-PAGE-NO                                    
067100                             WS-IDDISTR                                   
067200                             WS-IDSHIPM-Z                                 
067210                             W-TOT-CD                                     
067300                                                                          
067400     MOVE SPACE           TO RAD-HEAD                                     
067500                             RAD2-HEAD                                    
067600                             RAD3-HEAD                                    
067700                             RAD5-HEAD                                    
067800                             RAD5                                         
067900                             RADY                                         
068000                                                                          
068100     PERFORM S22-INIT-TOTAL                                               
068200** BUILD HEADER                                                           
068300     MOVE IDKUNDRF-IDKOLLI-LEDTEXT  (W-KDSPRAK)                           
068400                                       TO RAD5-IDORDNR7-IDKOLLI-X         
068500     MOVE 'CL'                         TO RAD5-KDORDKL-X                  
068600     MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD5-BEEMBTYP-X                 
068700     MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIL-X                 
068800     MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIB-X                 
068900     MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIH-X                 
069000     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDBTO-X                 
069100     MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDNTO-X                 
069200     MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VLORDBTO-X                 
069300     MOVE ZERO         TO WS-IX                                           
069400                                                                          
069500*    -- READ WDE1 HERE TO BE ABLE TO DETERMINE TYPE OF DC                 
069600     PERFORM IMS-GU-WDE111-DIST                                           
069700     MOVE SHIP-IDDC       TO WS-IDDC                                      
069800*--- VI GÖR OMRÄKNING TILL LOKAL VALUTA FÖR CNY/TRY/INR                   
069900*--- ÄVEN OM VALUE SKA VISAS ELLER EJ. MAN BYTER OFTA ÅSIKT:)             
070000                                                                          
070100     IF ((CDC-SE OR DDC-SE) AND                                           
070200         (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                      
070300       OR                                                                 
070400        ((CDC-SE OR DDC-SE) AND                                           
070500         (DIST34-KINA-NDC OR DIST35-REFILL-CN))                           
070600       OR                                                                 
070700          DIST35-NDCUS-NDCCN-REFILL                                       
070800       OR                                                                 
070900        ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                     
071000                                                                          
071100       OR                                                                 
071200         DIST35-CDC-TR-REFILL                                             
071300                                                                          
071400       OR                                                                 
071500         DIS121-SOUTH-AFRICA                                              
071600                                                                          
071610       OR                                                                 
071620         DIST35-CDC-ZA-REFILL                                             
071630                                                                          
071700       IF ((CDC-SE OR DDC-SE) AND                                         
071800           (DIST34-KINA-NDC OR DIST35-NDCUS-NDCCN-REFILL))                
071900         MOVE 'CNY'                    TO CURR-KDVALISO-ROW               
072000       ELSE                                                               
072100         IF ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                 
072200           MOVE 'TRY'                  TO CURR-KDVALISO-ROW               
072400         ELSE                                                             
072500           IF ((CDC-SE OR DDC-SE) AND                                     
072510               (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))             
072600             MOVE 'ZAR'                TO CURR-KDVALISO-ROW               
072700           ELSE                                                           
072800             MOVE 'INR'                TO CURR-KDVALISO-ROW               
072900           END-IF                                                         
073000         END-IF                                                           
073100       END-IF                                                             
073200                                                                          
073300       MOVE SPACE                      TO RAD5-SUORDV-X                   
073400                                          RAD5-KDVALISO-X                 
073500                                                                          
073600*      HÄMTA KURS FÖR OMRÄKNING TILL LOKAL VALUTA (INR/CNY)               
073700       MOVE TODAYS-DATE-YEAR           TO W-DATE-AAMM(1:2)                
073800       MOVE TODAYS-DATE-MONTH          TO W-DATE-AAMM(3:2)                
073900       MOVE W-DATE-AAMM                TO CURR-TIAAMM                     
074000       MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV               
074100       MOVE 'M'                        TO CURR-KDVALTYP                   
074200                                                                          
074300       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
074400       IF CURR-KDSVAR = ' '                                               
074500          CONTINUE                                                        
074600       ELSE                                                               
074700          MOVE 1                       TO CURR-PRKURS-NEW                 
074800       END-IF                                                             
074900       COMPUTE WS-PRKURS-EXCH ROUNDED = 1 / CURR-PRKURS-NEW               
075000                                                                          
075100     ELSE                                                                 
075200       MOVE SUORDV-LEDTEXT (W-KDSPRAK)   TO RAD5-SUORDV-X                 
075300       MOVE KDVALISO-LEDTEXT (W-KDSPRAK) TO RAD5-KDVALISO-X               
075400     END-IF                                                               
075500                                                                          
075600     IF TRPD-FLLDCKND = YES                                               
075700*    -- THEN IT IS A WEB DC                                               
075800       MOVE 'CARGO-SPEC'               TO HDR-IDOUTTYPE                   
075900       MOVE 001                        TO HDR-REQU-IDMSGVER               
076000       MOVE SPACE                      TO HDR-REQU-KDPGMACT               
076100       MOVE SPACE                      TO HDR-REQU-IDUSER                 
076200       MOVE YES                        TO WEB-OUTPUT-SW                   
076300       MOVE YES                        TO WSKRIV-CS1                      
076400     END-IF                                                               
076500     MOVE ZERO                         TO WS-LDC-HDR                      
076600                                                                          
076700     IF WEB-OUTPUT                                                        
076800       MOVE SPACE                 TO HDR-IDOUTREC                         
076900       MOVE SHIP-IDDC             TO HDR-IDOUTREC(1:2)                    
077000                                     WS-IDDC                              
077100       MOVE '      '              TO HDR-IDOUTREC(3:8)                    
077200       MOVE SHIP-IDSHIPM          TO HDR-IDLIST                           
077300       MOVE +1                    TO SEND-IDCOM                           
077400                                                                          
077500       IF SHIP-IDDC-EXP > SPACE                                           
077600         IF SHIP-IDDC-EXP NOT = WS-IDDC-11                                
077700           IF SHIP-KDFAKSTA-EXP = 2                                       
077800             MOVE SHIP-IDDC-EXP   TO WS-IDDC                              
077900                                     HDR-IDOUTREC(1:2)                    
078000             MOVE YES             TO VOR-CDC-11                           
078100           END-IF                                                         
078200         END-IF                                                           
078300       END-IF                                                             
078400                                                                          
078500     END-IF                                                               
078600     MOVE YES                     TO  W-NEW-DOC-FLAG                      
078700                                                                          
078800     IF VOR-OK OR NDC-AE                                                  
078900       MOVE SPACE                 TO RAD5-SUORDV-X                        
079000                                     RAD5-KDVALISO-X                      
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400                                                                          
079500 B-IMPORTER SECTION.                                                      
079600                                                                          
079700     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
079800                            W-WDB201-IDKUNDNR                             
079900     PERFORM IMS-GU-WDB201                                                
080000     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
080100     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
080200     PERFORM IMS-GU-WDB101                                                
080300     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
080400     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
080500     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
080600     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
080700     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
080800     MOVE BET-FLLOCCUR   TO FLLOCCUR-SW                                   
080900     ADD +1 TO WS-IX                                                      
081000*                                                                         
081100*OBS: EN ÄNDRING SOM MAN VILL BACKA PÅ MEN VI TAR INTE BORT HELT!         
081200*OBS: INTE ÄN... I FALL DE ÄNDRAR SIG!!! 10/11 '21                        
081300*FÖR IMPORTÖRER SOM FÅR LEVERANS FRÅN DC.87 DUBAI                         
081400*SHIPPER-INFO FINNS I SHIPPER-TAB I PROGRAMMET.                           
081500*    IF SHIP-KDFAKSTA-EXP = 2                                             
081600*      IF NDC-AE                                                          
081700*        MOVE +15                         TO SHIP-INDX                    
081800*                                                                         
081900*        MOVE SHIPPER-COMPANY (SHIP-INDX) TO RAD1H-IMPORTER               
082000*        MOVE SHIPPER-NAME (SHIP-INDX)    TO RAD2H-IMPORTER               
082100*        MOVE SHIPPER-STREET (SHIP-INDX)  TO RAD3H-IMPORTER               
082200*        MOVE SHIPPER-CITY (SHIP-INDX)    TO RAD4H-IMPORTER               
082300*        MOVE SHIPPER-COUNTRY (SHIP-INDX) TO RAD5H-IMPORTER               
082400*      END-IF                                                             
082500*    END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 B-DEALER-DETAIL SECTION.                                                 
083000                                                                          
083100     PERFORM S20-HAMTA-WDB2                                               
083200                                                                          
083300     MOVE SPACE         TO RAD1                                           
083400                           RAD2                                           
083500                           RAD3                                           
083600                           RAD4                                           
083700                           RAD4B                                          
083800                           RAD6                                           
083900                           RAD7                                           
084000                                                                          
084100     MOVE YES           TO W-PRINT-DEALER-FLAG                            
084200     MOVE YES           TO WSKRIV-CS1                                     
084300     MOVE IDKUNDNR-LEDTEXT (W-KDSPRAK)                                    
084400                        TO RAD2-IDKUNDNR-LEDTEXT                          
084500     MOVE SGMT-IDKUNDNR TO RAD2-IDKUNDNR                                  
084600     PERFORM BA-PACKAGE-DETAIL                                            
084700     .                                                                    
084800     EJECT                                                                
084900                                                                          
085000 BA-PACKAGE-DETAIL SECTION.                                               
085100                                                                          
085200     MOVE NOO            TO W-TOTAL-FLAG                                  
085300     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
085400     PERFORM IMS-GNP-WDE121                                               
085500     PERFORM UNTIL NOT SEGMENT-FOUND                                      
085600                                                                          
085700        PERFORM BAA-WRITE-LINE                                            
085800        PERFORM IMS-GNP-WDE121                                            
085900                                                                          
086000     END-PERFORM                                                          
086100                                                                          
086200     IF W-PRINT-TOTAL                                                     
086300        MOVE NOO                    TO W-TOTAL-FLAG                       
086400        MOVE SPACES                 TO RAD7                               
086500        MOVE PRODGRP-SUMMA-LEDTEXT (W-KDSPRAK)                            
086600                                    TO RAD7-TOTAL-TEXT                    
086700        MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                     
086800                                    TO RAD7-KOLLI-TEXT                    
086900                                                                          
087000        MOVE W-TOTAL-VKORDBTO (1)   TO RAD7-TOTAL-VKORDBTO                
087100                                                                          
087200*---    RAD7-TOTAL-SUORDV                                                 
087300                                                                          
087400        IF (NDC-CN AND NOT DIST35-NONVCC-NONVCC-REFILL)                   
087500        OR NDC-IN                                                         
087600          MOVE W-TOTAL-SUORDV-EXP (1)   TO RAD7-TOTAL-SUORDV              
087700        ELSE                                                              
087800          IF ((CDC-SE OR DDC-SE) AND                                      
087900              (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                 
088000            OR                                                            
088100             ((CDC-SE OR DDC-SE) AND                                      
088200              (DIST34-KINA-NDC OR DIST35-REFILL-CN))                      
088300            OR                                                            
088400               DIST35-NDCUS-NDCCN-REFILL                                  
088500            OR                                                            
088600              ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)               
088700            OR                                                            
088800               DIST35-CDC-TR-REFILL                                       
088900            OR                                                            
089000               DIS121-SOUTH-AFRICA                                        
089100                                                                          
089110            OR                                                            
089120               DIST35-CDC-ZA-REFILL                                       
089130                                                                          
089200            COMPUTE WS-MONEY ROUNDED = W-TOTAL-SUORDV(1) *                
089300                                       WS-PRKURS-EXCH                     
089400                                                                          
089500*---        VALUE FÖR CHINA REFILL PÅ TOTALRADERNA VISAS EJ T V           
089600*---        EJ HELLER FÖR TURKIET + REFILL TURKIET                        
089700*---        EJ HELLER FÖR SOUTH AFRICA                                    
089800                                                                          
089900            IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                  
090000              OR                                                          
090100                 DIST35-NDCUS-NDCCN-REFILL                                
090200              OR                                                          
090300               ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)              
090400              OR                                                          
090500                 DIST35-CDC-TR-REFILL                                     
090600              OR                                                          
090700                 DIS121-SOUTH-AFRICA                                      
091000                                                                          
091100              OR                                                          
091200                 DIST35-CDC-ZA-REFILL                                     
091300                                                                          
092000              MOVE SPACE          TO RAD7-TOTAL-SUORDV-W                  
093000            ELSE                                                          
094000              MOVE WS-MONEY       TO RAD7-TOTAL-SUORDV                    
095000            END-IF                                                        
096000                                                                          
097000            ADD WS-MONEY            TO W-TOTAL-SUORDV-EXCH                
097100          ELSE                                                            
097200            MOVE W-TOTAL-SUORDV (1) TO RAD7-TOTAL-SUORDV                  
097300            IF FLLOCCUR                                                   
097400              COMPUTE WS-MONEY ROUNDED = (W-TOTAL-SUORDV(1) *             
097500                WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                    
097600              MOVE WS-MONEY         TO RAD7-TOTAL-SUORDV                  
097700            END-IF                                                        
097800          END-IF                                                          
097900        END-IF                                                            
098000                                                                          
098100        MOVE W-TOTAL-VKORDNTO (1)   TO RAD7-TOTAL-VKORDNTO                
098200        MOVE W-TOTAL-VLORDBTO (1)   TO RAD7-TOTAL-VLORDBTO                
098300*                                                                         
098400*---    JAPAN HAS A UNIQUE MEASUREMENT FOR VOLYM                          
098500        IF NDC-JP                                                         
098600          COMPUTE W-TOTAL-VLORDBTO-JP = W-TOTAL-VLORDBTO (1) *            
098700                                        280                               
098800          MOVE W-TOTAL-VLORDBTO-JP  TO RAD7-TOTAL-VLORDBTO                
098900        END-IF                                                            
099000*                                                                         
099100        MOVE W-TOTAL-KOLLI-COUNT (1)                                      
099200                                    TO RAD7-KOLLI-TOTAL                   
099300                                                                          
099400*---    RAD7-TOTAL-KDVALISO                                               
099500                                                                          
099600        IF (NDC-CN AND NOT DIST35-NONVCC-NONVCC-REFILL)                   
099700          MOVE 'CNY'                TO RAD7-TOTAL-KDVALISO                
099800        ELSE                                                              
099900          IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                    
100000            OR                                                            
110000            DIST35-NDCUS-NDCCN-REFILL                                     
110100            OR                                                            
110200            ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                 
110300            OR                                                            
110400            DIST35-CDC-TR-REFILL                                          
110500            OR                                                            
110600            DIS121-SOUTH-AFRICA                                           
110610            OR                                                            
110620            DIST35-CDC-ZA-REFILL                                          
110700                                                                          
110800            MOVE SPACE              TO RAD7-TOTAL-KDVALISO                
110900          ELSE                                                            
111000            IF NDC-IN OR                                                  
111100              ((CDC-SE OR DDC-SE) AND                                     
111200              (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                 
111300              MOVE 'INR'            TO RAD7-TOTAL-KDVALISO                
111400            ELSE                                                          
111500              MOVE W-KDVALISO       TO RAD7-TOTAL-KDVALISO                
111600              IF FLLOCCUR                                                 
111700                MOVE SGMT-KDVALISO TO RAD7-TOTAL-KDVALISO                 
111800              END-IF                                                      
111900            END-IF                                                        
112000          END-IF                                                          
112100        END-IF                                                            
112200                                                                          
112300*---    FÖR VOR FRÅN DC11 -> CN/IN SKALL DET INTE VISAS NÅGRA             
112400*       TOTALER, VARKEN PÅ DETALJRADEN ELLER PÅ TOTALRADEN                
112500                                                                          
112600        IF VOR-OK OR NDC-AE                                               
112700          MOVE SPACE                TO RAD7-TOTAL-KDVALISO                
112800                                       RAD7-TOTAL-SUORDV-W                
112900        END-IF                                                            
113000                                                                          
113100        MOVE ZERO                   TO W-TOTAL-SUORDV (1)                 
113200                                       W-TOTAL-SUORDV-EXP (1)             
113300                                       W-TOTAL-VKORDBTO (1)               
113400                                       W-TOTAL-VKORDNTO (1)               
113500                                       W-TOTAL-VLORDBTO (1)               
113600                                       W-TOTAL-KOLLI-COUNT (1)            
113700*       MOVE SPACE               TO ARB-RAD                               
113800        MOVE RAD7                TO ARB-RAD                               
113900                                    SEND-RAD                              
114000        ADD 1                    TO W-LINE-COUNT                          
114100        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
114200        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
114300        PERFORM S21-PRINT-LINE                                            
114400        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
114500                                                                          
114600        IF WEB-OUTPUT                                                     
114700          MOVE 1                 TO W-CS4-NIV                             
114800          PERFORM S14-SKAPA-W476CS4                                       
114900        END-IF                                                            
115000                                                                          
115100     END-IF                                                               
115200     .                                                                    
115300     EJECT                                                                
115400                                                                          
115500 BAA-WRITE-LINE SECTION.                                                  
115600                                                                          
115700     IF WEB-OUTPUT                                                        
115800        MOVE SKOLLI-IDORDER      TO W-IDORDER                             
115900        PERFORM IMS-GU-WDQ201                                             
116000        IF SEGMENT-FOUND                                                  
116100          MOVE OHUV-IDUSER       TO HDR-REQU-IDUSER                       
116200          IF OHUV-IDUSER > SPACE                                          
116300            MOVE OHUV-IDUSER     TO HDR-IDOUTREC(3:8)                     
116400          END-IF                                                          
116500        END-IF                                                            
116600     END-IF                                                               
116700                                                                          
116800     IF W-PRINT-NEW-DOC                                                   
116900        MOVE 0              TO W-PAGE-NO                                  
117000        PERFORM S01-PRINT-META                                            
117100        PERFORM S25-PRINT-DISTR                                           
117200     END-IF                                                               
117300                                                                          
117400     IF W-PRINT-DEALER                                                    
117500        MOVE NOO                 TO W-PRINT-DEALER-FLAG                   
117600        MOVE SGMT-IDKUNDNR       TO RAD2-IDKUNDNR                         
117700        MOVE SKOLLI-IDORDER      TO W-IDORDER                             
117800        PERFORM IMS-GU-WDQ201                                             
117900        IF SEGMENT-FOUND                                                  
118000           MOVE OHUV-BEGMT-RAD1  TO RAD2-BEGMT1                           
118100           MOVE OHUV-ADGMT-GATA  TO RAD2-ADGMT-GATA                       
118200           MOVE OHUV-BEGMT-RAD2  TO RAD3-BEGMT2                           
118300           MOVE OHUV-ADGMT-PADR  TO RAD3-ADGMT-PADR                       
118400                                                                          
118500           MOVE OHUV-ADGMT-LAND  TO RAD4-ADGMT-LAND                       
118600        ELSE                                                              
118700           MOVE SPACE            TO RAD2-BEGMT1                           
118800                                    RAD2-ADGMT-GATA                       
118900                                    RAD3-BEGMT2                           
119000                                    RAD3-ADGMT-PADR                       
119100                                    RAD4-ADGMT-LAND                       
119200        END-IF                                                            
119300*       ADDRESS DATA IN NORMAL SWEDISH EBCDIC SO FAR                      
119400        MOVE WS-CP-SVE-EBCDIC   TO  TRAUTF8-KDCP                          
119500                                                                          
119600        IF NDC-CN OR LDC-CN                                               
119700*         -- FIX BAD DATA IN GMT-OVR FIELDS                               
119800          IF GMT-BEGMT-OVR-RAD1 = LOW-VALUE                               
119900             MOVE SPACE TO GMT-BEGMT-OVR-RAD1                             
120000          END-IF                                                          
120100          IF GMT-BEGMT-OVR-RAD2 = LOW-VALUE                               
120200             MOVE SPACE TO GMT-BEGMT-OVR-RAD2                             
120300          END-IF                                                          
120400          IF GMT-ADGMT-OVR-GATA = LOW-VALUE                               
120500             MOVE SPACE TO GMT-ADGMT-OVR-GATA                             
120600          END-IF                                                          
120700          IF GMT-ADGMT-OVR-PADR = LOW-VALUE                               
120800             MOVE SPACE TO GMT-ADGMT-OVR-PADR                             
120900          END-IF                                                          
121000*         -- USE OVR FIELDS IF THEY CONTAIN ANY SIGNIFICANT VALUES        
121100*         -- ELSE KEEP THE ENGLISH ADDRESS                                
121200          IF GMT-BEGMT-OVR-RAD1 NOT = SPACE                               
121300          OR GMT-BEGMT-OVR-RAD2 NOT = SPACE                               
121400          OR GMT-ADGMT-OVR-GATA NOT = SPACE                               
121500          OR GMT-ADGMT-OVR-PADR NOT = SPACE                               
121600            MOVE GMT-BEGMT-OVR-RAD1  TO RAD2-BEGMT1                       
121700            MOVE GMT-BEGMT-OVR-RAD2  TO RAD3-BEGMT2                       
121800            MOVE GMT-ADGMT-OVR-GATA  TO RAD2-ADGMT-GATA                   
121900            MOVE GMT-ADGMT-OVR-PADR  TO RAD3-ADGMT-PADR                   
122000            MOVE GMT-ADGMT-OVR-LAND  TO RAD4-ADGMT-LAND                   
122100*           -- DATA IN CHINESE DOUBLEBYTE EBCDIC                          
122200            MOVE WS-CP-CHN-EBCDIC    TO  TRAUTF8-KDCP                     
122300          END-IF                                                          
122400        END-IF                                                            
122500                                                                          
122600        IF TRPD-FLLDCKND = YES                                            
122700*       -- TEMP SOLUTION FOR CHINA - BETTER TO CHANGE FLLDCKND            
122800*       -- TO FLWEBDC IN TRPD COPYTEXT                                    
122900        OR NDC-CN OR LDC-CN                                               
123000          IF DIST35-NONVCC-NONVCC-REFILL                                  
123100*           -- TRANSP.DOK. FÖR STUDS FLÖDET SKRIVS VID ANDRA              
123200*           -- FAKTURERINGEN, SOM OM MAN HAR SKEPPAT FRÅN SE              
123300            CONTINUE                                                      
123400          ELSE                                                            
123500*           -- CONVERT ADDRESS DATA TO UNICODE UTF8                       
123600            MOVE RAD2-BEGMT1    TO TRAUTF8-TECONV-FROM                    
123700            CALL WTRAUTF8 USING TRAUTF8-AREA                              
123800            MOVE TRAUTF8-TECONV-TO TO RAD2-BEGMT1                         
123900                                                                          
124000            MOVE RAD3-BEGMT2    TO TRAUTF8-TECONV-FROM                    
124100            CALL WTRAUTF8 USING TRAUTF8-AREA                              
124200            MOVE TRAUTF8-TECONV-TO TO RAD3-BEGMT2                         
124300                                                                          
124400            MOVE RAD2-ADGMT-GATA TO TRAUTF8-TECONV-FROM                   
124500            CALL WTRAUTF8 USING TRAUTF8-AREA                              
124600            MOVE TRAUTF8-TECONV-TO TO RAD2-ADGMT-GATA                     
124700                                                                          
124800            MOVE RAD3-ADGMT-PADR TO TRAUTF8-TECONV-FROM                   
124900            CALL WTRAUTF8 USING TRAUTF8-AREA                              
125000            MOVE TRAUTF8-TECONV-TO TO RAD3-ADGMT-PADR                     
125100                                                                          
125200            MOVE RAD4-ADGMT-LAND TO TRAUTF8-TECONV-FROM                   
125300            CALL WTRAUTF8 USING TRAUTF8-AREA                              
125400            MOVE TRAUTF8-TECONV-TO TO RAD4-ADGMT-LAND                     
125500                                                                          
125600          END-IF                                                          
125700        END-IF                                                            
125800        IF W-LINE-COUNT >  W-LINE-MAX - W-LINE-HEAD                       
125900           PERFORM S25-PRINT-DISTR                                        
126000        END-IF                                                            
126100                                                                          
126200        MOVE RAD2                TO ARB-RAD                               
126300                                    SEND-RAD                              
126400        ADD 2                    TO W-LINE-COUNT                          
126500        MOVE PRT-AFTER-2         TO PRT-RADSKIP                           
126600        MOVE WS-SKIP2            TO STYRTECKEN-RAD                        
126700        PERFORM S21-PRINT-LINE                                            
126800        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
126900                                                                          
127000        MOVE RAD3                TO ARB-RAD                               
127100                                    SEND-RAD                              
127200        ADD 1                    TO W-LINE-COUNT                          
127300        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
127400        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
127500        PERFORM S21-PRINT-LINE                                            
127600        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
127700                                                                          
127800        MOVE RAD4                TO ARB-RAD                               
127900                                    SEND-RAD                              
128000        ADD 1                    TO W-LINE-COUNT                          
128100        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
128200        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
128300                                                                          
128400        MOVE TELEF-LEDTEXT (W-KDSPRAK)                                    
128500                                 TO RAD4B-PHONE-LEDTEXT                   
128600        MOVE WS-GMT-IDTFN        TO RAD4B-IDTFN                           
128700        MOVE RAD4B               TO ARB-RAD                               
128800                                    SEND-RAD                              
128900        ADD 1                    TO W-LINE-COUNT                          
129000        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
129100        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
129200                                                                          
129300        PERFORM S21-PRINT-LINE                                            
129400        IF WEB-OUTPUT                                                     
129500          PERFORM S11-SKRIV-CS1                                           
129600          PERFORM S12-SKAPA-W476CS2                                       
129700        END-IF                                                            
129800        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
129900                                                                          
130000        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
130100        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
130200        ADD 1                    TO W-LINE-COUNT                          
130300        PERFORM S23-PRINT-HEADER                                          
130400        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
130500     END-IF                                                               
130600                                                                          
130700     IF W-LINE-COUNT > W-LINE-MAX   -  1                                  
130800        PERFORM S25-PRINT-DISTR                                           
130900        MOVE PRT-AFTER-2         TO PRT-RADSKIP                           
131000        MOVE WS-SKIP2            TO STYRTECKEN-RAD                        
131100        ADD 2                    TO W-LINE-COUNT                          
131200        PERFORM S23-PRINT-HEADER                                          
131300        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
131400     END-IF                                                               
131500     MOVE SPACES             TO RAD6                                      
131600     MOVE SKOLLI-IDORDNR7    TO RAD6-IDORDNR7                             
131700     MOVE '-'                TO RAD6-IDKOLLI-SEP                          
131800     MOVE SKOLLI-IDKOLLI     TO RAD6-IDKOLLI                              
131900     MOVE SKOLLI-KDORDKL     TO RAD6-KDORDKL                              
132000     MOVE SKOLLI-KDEMBTYP    TO W-KDEMBTYP                                
132100     PERFORM IMS-GU-WDGX4738                                              
132200     IF SEGMENT-FOUND                                                     
132300        MOVE EMBTYP-BEEMBTYP(W-KDSPRAK) TO RAD6-BEEMBTYP                  
132400     END-IF                                                               
132500     MOVE SKOLLI-DIKOLLIL    TO RAD6-DIKOLLIL                             
132600     MOVE SKOLLI-DIKOLLIB    TO RAD6-DIKOLLIB                             
132700     MOVE SKOLLI-DIKOLLIH    TO RAD6-DIKOLLIH                             
132800     MOVE SKOLLI-VKORDBTO-KOLLI    TO RAD6-VKORDBTO                       
132900                                                                          
133000     PERFORM BAAC-BERAKNA-KOLLITS-NETTOVIKT                               
133100     MOVE W-VKORDNTO-KOLLI         TO RAD6-VKORDNTO                       
133200***  MOVE SKOLLI-VKORDNTO-KOLLI    TO RAD6-VKORDNTO                       
133300                                                                          
133400     MOVE SKOLLI-VLORDBTO-KOLLI    TO RAD6-VLORDBTO                       
133500                                                                          
133600     IF (NDC-CN AND NOT DIST35-NONVCC-NONVCC-REFILL)                      
133700     OR NDC-IN                                                            
133800       IF SKOLLI-SUORDV-EXP > ZERO                                        
133900         COMPUTE W-SUORDV-EXP = SKOLLI-SUORDV-EXP                         
134000         IF NDC-IN                                                        
134100           MOVE 'INR'         TO RAD6-KDVALISO                            
134200         ELSE                                                             
134300           MOVE 'CNY'         TO RAD6-KDVALISO                            
134400         END-IF                                                           
134500       END-IF                                                             
134600     ELSE                                                                 
134700                                                                          
134800       IF ((CDC-SE OR DDC-SE) AND                                         
134900           (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                    
135000         OR                                                               
135100           ((CDC-SE OR DDC-SE) AND                                        
135200            (DIST34-KINA-NDC OR DIST35-REFILL-CN))                        
135300         OR                                                               
135400             DIST35-NDCUS-NDCCN-REFILL                                    
135500         OR                                                               
135600           ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                  
135700         OR                                                               
135800             DIST35-CDC-TR-REFILL                                         
135900         OR                                                               
136000             DIS121-SOUTH-AFRICA                                          
136100                                                                          
136110         OR                                                               
136120             DIST35-CDC-ZA-REFILL                                         
136130                                                                          
136200         IF SKOLLI-SUORDV > ZERO                                          
136300           COMPUTE W-SUORDV = SKOLLI-SUORDV                               
136400         ELSE                                                             
136500           MOVE ZERO          TO W-SUORDV                                 
136600         END-IF                                                           
136700         MOVE SPACE           TO RAD6-SUORDV-W                            
136800                                 RAD6-KDVALISO                            
136900       ELSE                                                               
137000         IF SKOLLI-SUORDV > ZERO                                          
137100           COMPUTE W-SUORDV = SKOLLI-SUORDV                               
137200           MOVE 'SEK'         TO RAD6-KDVALISO                            
137300                                   W-KDVALISO                             
137400                                                                          
137500         ELSE                                                             
137600           IF SKOLLI-SUORDV-LOCPREL > +0 OR                               
137700              SKOLLI-SUORDV-LOC > +0                                      
137800             COMPUTE W-SUORDV = SKOLLI-SUORDV-LOCPREL +                   
137900                                SKOLLI-SUORDV-LOC                         
138000             MOVE SKOLLI-KDVALISO TO RAD6-KDVALISO                        
138100                                     W-KDVALISO                           
138200           END-IF                                                         
138300         END-IF                                                           
138400                                                                          
138500         MOVE W-SUORDV        TO RAD6-SUORDV                              
138600         IF FLLOCCUR                                                      
138700            MOVE SGMT-KDVALISO TO RAD6-KDVALISO                           
138800            COMPUTE WS-MONEY ROUNDED = (W-SUORDV *                        
138900              WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                      
139000            MOVE WS-MONEY         TO RAD6-SUORDV                          
139100            ADD WS-MONEY          TO WY-MONEY                             
139200         END-IF                                                           
139300       END-IF                                                             
139400     END-IF                                                               
139500     IF NDC-AE                                                            
139600       MOVE SPACE                 TO RAD6-SUORDV-W                        
139700                                     RAD6-KDVALISO                        
139800     END-IF                                                               
139810                                                                          
139820*RC-CD                                                                    
139830     IF SKOLLI-FLCROSS = YES                                              
139840       MOVE CROSSDOC-YES      TO RAD6-FLCROSS                             
139850     ELSE                                                                 
139860       MOVE SPACE             TO RAD6-FLCROSS                             
139870     END-IF                                                               
139900                                                                          
140000     PERFORM BAAA-BUILD-TOTAL                                             
140100     PERFORM BAAB-KDFARLIG-CHECK                                          
140200*    MOVE SPACES              TO ARB-RAD                                  
140300     MOVE RAD6                TO ARB-RAD                                  
140400                                 SEND-RAD                                 
140500     ADD 1                    TO W-LINE-COUNT                             
140600     MOVE PRT-AFTER-1         TO PRT-RADSKIP                              
140700     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
140800     PERFORM S21-PRINT-LINE                                               
140900     IF WEB-OUTPUT                                                        
141000       PERFORM S13-SKAPA-W476CS3                                          
141100     END-IF                                                               
141200     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
141300     .                                                                    
141400     EJECT                                                                
141500                                                                          
141600 BAAA-BUILD-TOTAL SECTION.                                                
141700                                                                          
141800     ADD W-SUORDV               TO W-TOTAL-SUORDV (1)                     
141900                                   W-TOTAL-SUORDV (2)                     
142000     ADD W-SUORDV-EXP           TO W-TOTAL-SUORDV-EXP (1)                 
142100                                   W-TOTAL-SUORDV-EXP (2)                 
142200**** ADD SKOLLI-VKORDNTO-KOLLI  TO W-TOTAL-VKORDNTO (1)                   
142300     ADD W-VKORDNTO-KOLLI       TO W-TOTAL-VKORDNTO (1)                   
142400                                   W-TOTAL-VKORDNTO (2)                   
142500     ADD SKOLLI-VKORDBTO-KOLLI  TO W-TOTAL-VKORDBTO (1)                   
142600     ADD SKOLLI-VLORDBTO-KOLLI  TO W-TOTAL-VLORDBTO (1)                   
142700     ADD 1                      TO W-TOTAL-KOLLI-COUNT (1)                
142800                                   W-TOTAL-KOLLI-COUNT (2)                
142900                                                                          
142910*RC-CD                                                                    
142920     IF SKOLLI-FLCROSS = YES                                              
142930        ADD W-SUORDV               TO W-TOTAL-SUORDV-CD                   
142940        ADD W-SUORDV-EXP           TO W-TOTAL-SUORDV-EXP-CD               
142950        ADD W-VKORDNTO-KOLLI       TO W-TOTAL-VKORDNTO-CD                 
142960        ADD SKOLLI-VKORDBTO-KOLLI  TO W-TOTAL-VKORDBTO-CD                 
142970        ADD SKOLLI-VLORDBTO-KOLLI  TO W-TOTAL-VLORDBTO-CD                 
142980        ADD 1                      TO W-TOTAL-KOLLI-CNT-CD                
142990     END-IF                                                               
143000*---   GÄLLANDE SAMKOLLI:                                                 
143100*---   BRUTTO VIKT O VOLYM FÖR SAMKOLLI HÄMTAS FRÅN WDE7                  
143200*---   DÄREMOT NETTOVIKT FRÅN WDE1 (ING.ARTIKLARS NETTOVIKT)              
143300                                                                          
143400     IF (CDC-SE OR DDC-SE) AND                                            
143500       SKOLLI-IDKOLLI-SAMP > ZERO                                         
143600       PERFORM BAAAA-SAMKOLLI                                             
143700     ELSE                                                                 
143800       ADD SKOLLI-VKORDBTO-KOLLI  TO W-TOTAL-VKORDBTO (2)                 
143900       ADD SKOLLI-VLORDBTO-KOLLI  TO W-TOTAL-VLORDBTO (2)                 
144000     END-IF                                                               
144100                                                                          
144200     MOVE YES                   TO W-TOTAL-FLAG                           
144300                                   W-GRTOTAL-FLAG                         
144400     .                                                                    
144500     EJECT                                                                
144600                                                                          
144700 BAAAA-SAMKOLLI      SECTION.                                             
144800                                                                          
144900*---   KOLLIT INGÅR I ETT SAMKOLLI                                        
145000*---   SUMMERA ANTALET SAMKOLLIN OCH KOLLIN I SAMKOLLI                    
145100*---   HÄMTA/ADDERA VIKT O VOLYM EN GÅNG PER SAMKOLLI                     
145200                                                                          
145300     MOVE 1            TO WS-SKLI-IX                                      
145400     MOVE NOO          TO TRAFF-SKLI-SW                                   
145500                                                                          
145600     PERFORM UNTIL WS-SKLI-IX > WS-SKLI-IX-MAX OR                         
145700       TRAFF-SKLI                                                         
145800       IF SKOLLI-IDKOLLI-SAMP = WS-IDKOLLI-SAMP (WS-SKLI-IX)              
145900         ADD +1        TO WS-ANT-KOLLI-I-ALLA-SK                          
146000         MOVE YES      TO TRAFF-SKLI-SW                                   
146100       ELSE                                                               
146200         IF WS-IDKOLLI-SAMP (WS-SKLI-IX) = ZERO                           
146300           MOVE SKOLLI-IDKOLLI-SAMP                                       
146400                       TO WS-IDKOLLI-SAMP (WS-SKLI-IX)                    
146500           ADD +1      TO WS-ANT-KOLLI-I-ALLA-SK                          
146600                          WS-ANT-SK                                       
146700           MOVE YES    TO TRAFF-SKLI-SW                                   
146800                                                                          
146900           MOVE SHIP-IDDC            TO W-IDDC                            
147000           MOVE SKOLLI-IDKOLLI-SAMP  TO W-IDKOLLI-SAMP                    
147100                                                                          
147200           PERFORM IMS-GU-WDE711-ASEQ                                     
147300                                                                          
147400           ADD SKLI-VKKOLLIB-SAMP    TO W-TOTAL-VKORDBTO (2)              
147500           ADD SKLI-VLKOLLIB-SAMP    TO W-TOTAL-VLORDBTO (2)              
147600         ELSE                                                             
147700           ADD +1      TO WS-SKLI-IX                                      
147800         END-IF                                                           
147900       END-IF                                                             
148000     END-PERFORM                                                          
148100                                                                          
148200                                                                          
148300     .                                                                    
148400     EJECT                                                                
148500                                                                          
148600 BAAB-KDFARLIG-CHECK SECTION.                                             
148700                                                                          
148800     IF SKOLLI-KDFARLIG-KOLLI = +4                                        
148900     OR SKOLLI-KDFARLIG-KOLLI = +7                                        
149000       IF DIST34-FRANKRIKE-SDC OR                                         
149100          DIST34-ITALIEN-SDC OR                                           
149200          DIST34-SPANIEN-SDC                                              
149300         IF SKOLLI-KVFLAMP-KOLLI NOT = ZERO                               
149400           MOVE KVFLAMP-LEDTEXT (W-KDSPRAK)                               
149500                                      TO RAD6-KVFLAMP-X-SDC               
149600           MOVE SKOLLI-KVFLAMP-KOLLI  TO RAD6-KVFLAMP-SDC                 
149700         END-IF                                                           
149800       END-IF                                                             
149900       IF DIST34-FRANKRIKE-SDC                                            
150000         MOVE W-FARLIG-TEXT-SDC22     TO RAD6-BEFARLIG-SDC                
150100       ELSE                                                               
150200         IF DIST34-ITALIEN-SDC                                            
150300           MOVE W-FARLIG-TEXT-SDC25   TO RAD6-BEFARLIG-SDC                
150400         ELSE                                                             
150500           IF DIST34-SPANIEN-SDC                                          
150600             MOVE W-FARLIG-TEXT-SDC24 TO RAD6-BEFARLIG-SDC                
150700           ELSE                                                           
150800             MOVE BEFARLIG-TEXT (W-KDSPRAK) TO RAD6-BEFARLIG              
150900             IF SKOLLI-KVFLAMP-KOLLI  NOT = ZERO                          
151000               MOVE KVFLAMP-LEDTEXT (W-KDSPRAK)                           
151100                                            TO RAD6-KVFLAMP-X             
151200               MOVE SKOLLI-KVFLAMP-KOLLI    TO RAD6-KVFLAMP               
151300             END-IF                                                       
151400           END-IF                                                         
151500         END-IF                                                           
151600       END-IF                                                             
151700     END-IF                                                               
151800     .                                                                    
151900     EJECT                                                                
152000                                                                          
152100 BAAC-BERAKNA-KOLLITS-NETTOVIKT SECTION.                                  
152200                                                                          
152300     MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR                           
152400     MOVE SKOLLI-IDPRODNR  TO W-WDE121-IDPRODNR                           
152500     MOVE SKOLLI-IDKOLLI   TO W-WDE121-IDKOLLI                            
152600     MOVE ZERO             TO W-VKORDNTO-KOLLI                            
152700                                                                          
152800     PERFORM IMS-GNP-WDE131                                               
152900                                                                          
153000     PERFORM UNTIL NOT SEGMENT-FOUND                                      
153100        COMPUTE W-VKORDNTO-KOLLI =                                        
153200                W-VKORDNTO-KOLLI +                                        
153300**             (SRAD-VKARTNTO * SRAD-KVLEVART)                            
153400               (SRAD-VKART-NTO-KG * SRAD-KVLEVART)                        
153500                                                                          
153600        PERFORM IMS-GNP-WDE131                                            
153700     END-PERFORM                                                          
153800     .                                                                    
153900     EJECT                                                                
154000                                                                          
154100 S01-PRINT-META SECTION.                                                  
154200                                                                          
154300     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
154400     MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
154500     STRING WS-META                                                       
154600            'SHIPMENT_NUMBER='                                            
154700            WS-IDSHIPM-Z                                                  
154800            DELIMITED BY SIZE INTO SEND-RAD                               
154900                                                                          
155000     PERFORM S90-PUT-DOC-LINE                                             
155100                                                                          
155200     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
155300     STRING WS-META                                                       
155400            'DOCUMENT_TYPE='                                              
155500            WS-TYP-IDSHIP                                                 
155600            DELIMITED BY SIZE INTO SEND-RAD                               
155700                                                                          
155800     PERFORM S90-PUT-DOC-LINE                                             
155900                                                                          
156000     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
156100     MOVE SHIP-TISKEPPN      TO W-YYMMDD                                  
156200     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
156300                                                                          
156400     STRING WS-META                                                       
156500            'SHIPPING_DATE='                                              
156600            WS-YEAR(1:2)                                                  
156700            W-YYMMDD                                                      
156800            DELIMITED BY SIZE INTO SEND-RAD                               
156900                                                                          
157000     PERFORM S90-PUT-DOC-LINE                                             
157100                                                                          
157200     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
157300     MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
157400     STRING WS-META                                                       
157500            'DISTRICT_NUMBER='                                            
157600            WS-IDDISTR                                                    
157700            DELIMITED BY SIZE INTO SEND-RAD                               
157800                                                                          
157900     PERFORM S90-PUT-DOC-LINE                                             
158000                                                                          
158100     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
158200     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
158300     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
158400     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
158500     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
158600     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
158700     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
158800                                                                          
158900     STRING WS-META                                                       
159000            'FILE_NAME='                                                  
159100            DELIMITED BY SIZE                                             
159200            'SHIPDOC_CS'                                                  
159300            DELIMITED BY SIZE                                             
159400            '_'                                                           
159500            DELIMITED BY SIZE                                             
159600            FUNCTION TRIM (WS-IDSHIPM-Z)                                  
159700            DELIMITED BY SIZE                                             
159800            '_'                                                           
159900            FUNCTION TRIM (WS-IDDISTR)                                    
160000            DELIMITED BY SIZE                                             
160100            '_'                                                           
160200            DELIMITED BY SIZE                                             
160300            WS-TIMESTAMP                                                  
160400            DELIMITED BY SIZE INTO SEND-RAD                               
160500                                                                          
160600     PERFORM S90-PUT-DOC-LINE                                             
160700     .                                                                    
160800                                                                          
160900 S11-SKAPA-W476CS1   SECTION.                                             
161000                                                                          
161100     MOVE '1'                  TO HDR-IDAFPRCD                            
161200     MOVE SHIP-IDDC            TO HDR-IDDC                                
161300     IF VOR-OK                                                            
161400       MOVE SHIP-IDDC-EXP      TO HDR-IDDC                                
161500     END-IF                                                               
161600     MOVE RAD1H-IMPORTER-TEXT  TO HDR-RAD1H-IMPORTER-TEXT                 
161700     MOVE RAD1H-IMPORTER       TO HDR-RAD1H-IMPORTER                      
161800     MOVE RAD2H-IMPORTER       TO HDR-RAD2H-IMPORTER                      
161900     MOVE RAD3H-IMPORTER       TO HDR-RAD3H-IMPORTER                      
162000     MOVE RAD4H-IMPORTER       TO HDR-RAD4H-IMPORTER                      
162100     MOVE RAD5H-IMPORTER       TO HDR-RAD5H-IMPORTER                      
162200     MOVE RAD1-TIAAMMDD        TO HDR-RAD1-TIAAMMDD                       
162300     MOVE RAD1-IDDISTR         TO HDR-RAD1-IDDISTR                        
162400     MOVE RAD1-IDSHIPM         TO HDR-RAD1-IDSHIPM                        
162500     MOVE RAD1-IDTRPTNR        TO HDR-RAD1-IDTRPTNR                       
162600     MOVE RAD1-IDLBBET         TO HDR-RAD1-IDLBBET                        
162700     IF WS-LDC-HDR = 0                                                    
162800       PERFORM S90-PUT-DAP-HEADER                                         
162900       MOVE 1                  TO WS-LDC-HDR                              
163000     END-IF                                                               
163100     .                                                                    
163200     EJECT                                                                
163300 S11-SKRIV-CS1 SECTION.                                                   
163400     MOVE HDR-W476CS1             TO DOC-AREA                             
163500     MOVE LENGTH OF HDR-W476CS1   TO SEND-KVDLEN                          
163600     PERFORM S90-PUT-DOC                                                  
163700     .                                                                    
163800     EJECT                                                                
163900 S12-SKAPA-W476CS2   SECTION.                                             
164000                                                                          
164100     MOVE '2'                  TO SUBHDR-IDAFPRCD                         
164200     MOVE RAD2-IDKUNDNR        TO SUBHDR-RAD2-IDKUNDNR                    
164300     MOVE RAD2-BEGMT1          TO SUBHDR-RAD2-BEGMT1                      
164400     MOVE RAD2-ADGMT-GATA      TO SUBHDR-RAD2-ADGMT-GATA                  
164500     MOVE RAD3-BEGMT2          TO SUBHDR-RAD3-BEGMT2                      
164600     MOVE RAD3-ADGMT-PADR      TO SUBHDR-RAD3-ADGMT-PADR                  
164700     MOVE RAD4-ADGMT-LAND      TO SUBHDR-RAD4-ADGMT-LAND                  
164800     MOVE RAD4B-IDTFN          TO SUBHDR-RAD4B-IDTFN                      
164900     MOVE SUBHDR-W476CS2            TO DOC-AREA                           
165000     MOVE LENGTH OF SUBHDR-W476CS2  TO SEND-KVDLEN                        
165100     PERFORM S90-PUT-DOC                                                  
165200     .                                                                    
165300     EJECT                                                                
165400 S13-SKAPA-W476CS3   SECTION.                                             
165500                                                                          
165600     MOVE '3'                  TO LINE-IDAFPRCD                           
165700     MOVE RAD6-IDORDNR7        TO LINE-RAD6-IDORDNR7                      
165800     MOVE RAD6-IDKOLLI         TO LINE-RAD6-IDKOLLI                       
165900     MOVE RAD6-KDORDKL         TO LINE-RAD6-KDORDKL                       
166000     MOVE RAD6-BEEMBTYP        TO LINE-RAD6-BEEMBTYP                      
166100     MOVE RAD6-DIKOLLIL        TO LINE-RAD6-DIKOLLIL                      
166200     MOVE RAD6-DIKOLLIB        TO LINE-RAD6-DIKOLLIB                      
166300     MOVE RAD6-DIKOLLIH        TO LINE-RAD6-DIKOLLIH                      
166400     MOVE RAD6-VKORDBTO        TO LINE-RAD6-VKORDBTO                      
166500     MOVE RAD6-VKORDNTO        TO LINE-RAD6-VKORDNTO                      
166600     MOVE RAD6-VLORDBTO        TO LINE-RAD6-VLORDBTO                      
166700     MOVE RAD6-SUORDV          TO LINE-RAD6-SUORDV                        
166800     MOVE RAD6-KDVALISO        TO LINE-RAD6-KDVALISO                      
166900     MOVE RAD6-BEFARLIG        TO LINE-RAD6-FARLIG                        
166910     MOVE RAD6-FLCROSS         TO LINE-RAD6-FLCROSS                       
167000     MOVE LINE-W476CS3            TO DOC-AREA                             
167100     MOVE LENGTH OF LINE-W476CS3  TO SEND-KVDLEN                          
167200     PERFORM S90-PUT-DOC                                                  
167300     .                                                                    
167400     EJECT                                                                
167500 S14-SKAPA-W476CS4   SECTION.                                             
167600                                                                          
167700     MOVE '4'                     TO FTR-IDAFPRCD                         
167800     MOVE RAD7-TOTAL-TEXT         TO FTR-RAD7-TOTAL-TEXT                  
167900     IF W-CS4-NIV = 1                                                     
168000       MOVE 'TOTAL DEALER'        TO FTR-RAD7-TOTAL-TEXT                  
168100     ELSE                                                                 
168200       MOVE 'TOTAL DISTR.'        TO FTR-RAD7-TOTAL-TEXT                  
168300     END-IF                                                               
168400     MOVE RAD7-KOLLI-TOTAL        TO FTR-RAD7-KOLLI-TOTAL                 
168500     MOVE RAD7-KOLLI-TEXT         TO FTR-RAD7-KOLLI-TEXT                  
168600     MOVE RAD7-TOTAL-VKORDBTO     TO FTR-RAD7-VKORDBTO                    
168700     MOVE RAD7-TOTAL-VKORDNTO     TO FTR-RAD7-VKORDNTO                    
168800     MOVE RAD7-TOTAL-VLORDBTO     TO FTR-RAD7-VLORDBTO                    
168900     IF NOT (NDC-CN OR LDC-CN)                                            
169000        MOVE RAD7-TOTAL-SUORDV    TO FTR-RAD7-SUORDV                      
169100        MOVE RAD7-TOTAL-KDVALISO  TO FTR-RAD7-KDVALISO                    
169200     END-IF                                                               
169300     MOVE FTR-W476CS4             TO DOC-AREA                             
169400     MOVE LENGTH OF FTR-W476CS4   TO SEND-KVDLEN                          
169500     PERFORM S90-PUT-DOC                                                  
169600     .                                                                    
169700     EJECT                                                                
169710 S15-SKAPA-W476CS5  SECTION.                                              
169720                                                                          
169730     MOVE '5'                     TO SUBFTR-IDAFPRCD                      
169740                                                                          
169750     MOVE 'OF WHICH TOTAL CROSS DOCK.'                                    
169760                                  TO SUBFTR-RAD7-CD-TOTAL-TEXT            
169770     MOVE W-TOTAL-KOLLI-CNT-CD    TO SUBFTR-RAD7-CD-KOLLI-TOTAL           
169780     MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
169790                                  TO SUBFTR-RAD7-CD-KOLLI-TEXT            
169791     MOVE W-TOTAL-VKORDBTO-CD     TO SUBFTR-RAD7-CD-VKORDBTO              
169792     MOVE W-TOTAL-VKORDNTO-CD     TO SUBFTR-RAD7-CD-VKORDNTO              
169793     MOVE W-TOTAL-VLORDBTO-CD     TO SUBFTR-RAD7-CD-VLORDBTO              
169794*    IF NOT (NDC-CN OR LDC-CN)                                            
169795        MOVE W-TOTAL-SUORDV-CD    TO SUBFTR-RAD7-CD-SUORDV                
169796        MOVE RAD7-TOTAL-KDVALISO  TO SUBFTR-RAD7-CD-KDVALISO              
169797*    END-IF                                                               
169798     MOVE SUBFTR-W476CS5          TO DOC-AREA                             
169799     MOVE LENGTH OF SUBFTR-W476CS5                                        
169800                                  TO SEND-KVDLEN                          
169801     PERFORM S90-PUT-DOC                                                  
169802     .                                                                    
169803     EJECT                                                                
169810 S20-HAMTA-WDB2  SECTION.                                                 
169900                                                                          
170000     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
170100     PERFORM IMS-GU-WDB201                                                
170200     MOVE GMT-IDTFN         TO WS-GMT-IDTFN                               
170300     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
170400       MOVE +2 TO W-KDSPRAK                                               
170500     ELSE                                                                 
170600       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
170700     END-IF                                                               
170800     IF W-KDSPRAK NOT = +2                                                
170900       MOVE IDKUNDRF-IDKOLLI-LEDTEXT  (W-KDSPRAK)                         
171000                                       TO RAD5-IDORDNR7-IDKOLLI-X         
171100       MOVE 'CL'                         TO RAD5-KDORDKL-X                
171200       MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD5-BEEMBTYP-X               
171300       MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIL-X               
171400       MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIB-X               
171500       MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIH-X               
171600       MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDBTO-X               
171700       MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDNTO-X               
171800       MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VLORDBTO-X               
171900                                                                          
172000*---   FÖR CHINA REFILL SKA VALUE O KDVALISO EJ VISAS,VARKEN              
172100*---   PÅ DETALJ- ELLER TOTALRADER. DÄRFÖR INGEN RUBRIK.                  
172200*---   GÄLLER ÄVEN TURKIET + REFILL TURKIET                               
172300                                                                          
172400       IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                       
172500         OR                                                               
172600         DIST35-NDCUS-NDCCN-REFILL                                        
172700         OR                                                               
172800         ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                    
172900         OR                                                               
173000         DIST35-CDC-TR-REFILL                                             
173100         OR                                                               
173200         DIS121-SOUTH-AFRICA                                              
173210         OR                                                               
173220         DIST35-CDC-ZA-REFILL                                             
173300                                                                          
173400         MOVE SPACE                      TO RAD5-SUORDV-X                 
173500                                            RAD5-KDVALISO-X               
173600       ELSE                                                               
173700         MOVE SUORDV-LEDTEXT (W-KDSPRAK) TO RAD5-SUORDV-X                 
173800         MOVE KDVALISO-LEDTEXT (W-KDSPRAK) TO RAD5-KDVALISO-X             
173900       END-IF                                                             
174000     END-IF                                                               
174100     .                                                                    
174200     EJECT                                                                
174300                                                                          
174400 S21-PRINT-LINE SECTION.                                                  
174500                                                                          
174600     IF NOT WEB-OUTPUT                                                    
174700                                                                          
174800       PERFORM S90-PUT-DOC-LINE                                           
174900       IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                
175000         CALL W006PRS1 USING PRT-SPOOL-OVR                                
175100                             PRT-WRITE                                    
175200                             W-IDPRTLST                                   
175300                             ALT-PCB                                      
175400                             PRT-RADSKIP                                  
175500                             ARB-RAD                                      
175600       END-IF                                                             
175700     END-IF                                                               
175800     .                                                                    
175900     EJECT                                                                
176000                                                                          
176100 S22-INIT-TOTAL SECTION.                                                  
176200                                                                          
176300     MOVE 1 TO INDX                                                       
176400     PERFORM UNTIL INDX > 2                                               
176500       MOVE ZERO TO W-TOTAL-SUORDV (INDX)                                 
176600                    W-TOTAL-SUORDV-EXP (INDX)                             
176700                    W-TOTAL-VKORDBTO (INDX)                               
176800                    W-TOTAL-VKORDNTO (INDX)                               
176900                    W-TOTAL-VLORDBTO (INDX)                               
177000                    W-TOTAL-KOLLI-COUNT (INDX)                            
177100       ADD 1     TO INDX                                                  
177200     END-PERFORM                                                          
177300     MOVE ZERO TO W-TOTAL-SUORDV-EXCH                                     
177400                                                                          
177500     MOVE 1 TO WS-SKLI-IX                                                 
177600     PERFORM UNTIL WS-SKLI-IX > WS-SKLI-IX-MAX                            
177700       MOVE ZERO TO WS-IDKOLLI-SAMP (WS-SKLI-IX)                          
177800       ADD 1     TO WS-SKLI-IX                                            
177900     END-PERFORM                                                          
178000                                                                          
178100     MOVE ZERO TO WS-ANT-KOLLI-I-ALLA-SK                                  
178200                  WS-ANT-SK                                               
178300                                                                          
178400     .                                                                    
178500     EJECT                                                                
178600                                                                          
178700 S23-PRINT-HEADER SECTION.                                                
178800                                                                          
178900*    MOVE SPACES                       TO ARB-RAD                         
179000     MOVE RAD5                         TO ARB-RAD                         
179100                                          SEND-RAD                        
179200     PERFORM S21-PRINT-LINE                                               
179300*    IF WEB-OUTPUT                                                        
179400*      PERFORM S13-SKAPA-W476CS3                                          
179500*    END-IF                                                               
179600     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
179700     .                                                                    
179800     EJECT                                                                
179900                                                                          
180000 S24-PRINT-GRTOTAL SECTION.                                               
180100                                                                          
180200     MOVE NOO                   TO W-GRTOTAL-FLAG                         
180300     MOVE SPACES                TO RAD8                                   
180400                                   ARB-RAD                                
180500                                   SEND-RAD                               
180600     MOVE IDSHIPM-TOTAL-LEDTEXT (W-KDSPRAK)                               
180700                                TO RAD8-TOTAL-TEXT                        
180800     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
180900     MOVE WS-SKIP2              TO STYRTECKEN-RAD                         
181000     ADD 2                      TO W-LINE-COUNT                           
181100     IF W-LINE-COUNT > W-LINE-MAX - W-LINE-GRTOTAL                        
181200       PERFORM S25-PRINT-DISTR                                            
181300       MOVE PRT-AFTER-1         TO PRT-RADSKIP                            
181400       MOVE WS-SKIP1            TO STYRTECKEN-RAD                         
181500       ADD 1                    TO W-LINE-COUNT                           
181600     END-IF                                                               
181700*    MOVE SPACES                TO ARB-RAD                                
181800     MOVE RAD8                  TO ARB-RAD                                
181900                                   SEND-RAD                               
182000     PERFORM S21-PRINT-LINE                                               
182100     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
182200                                                                          
182300     MOVE SPACES                TO RAD7                                   
182400                                   ARB-RAD                                
182500                                   SEND-RAD                               
182600     MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
182700                                TO RAD7-KOLLI-TEXT                        
182800     MOVE W-TOTAL-VKORDBTO (2)  TO RAD7-TOTAL-VKORDBTO                    
182900                                                                          
183000     IF (NDC-CN AND NOT DIST35-NONVCC-NONVCC-REFILL)                      
183100     OR NDC-IN                                                            
183200       MOVE W-TOTAL-SUORDV-EXP (2) TO RAD7-TOTAL-SUORDV                   
183300     ELSE                                                                 
183400       IF ((CDC-SE OR DDC-SE) AND                                         
183500           (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                    
183600         OR                                                               
183700          ((CDC-SE OR DDC-SE) AND                                         
183800           (DIST34-KINA-NDC OR DIST35-REFILL-CN))                         
183900         OR                                                               
184000            DIST35-NDCUS-NDCCN-REFILL                                     
184100         OR                                                               
184200          ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                   
184300         OR                                                               
184400            DIST35-CDC-TR-REFILL                                          
184500         OR                                                               
184600            DIS121-SOUTH-AFRICA                                           
184610         OR                                                               
184620            DIST35-CDC-ZA-REFILL                                          
184700                                                                          
184800*---     VALUE FÖR CHINA REFILL PÅ TOTALRADERNA VISAS EJ T V              
184900*---     GÄLLER ÄVEN TURKIET + REFILL TURKIET                             
185000*---     GÄLLER ÄVEN SOUTH AFRICA                                         
185100                                                                          
185200         IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                     
185300         OR                                                               
185400            DIST35-NDCUS-NDCCN-REFILL                                     
185500         OR                                                               
185600            ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                 
185700         OR                                                               
185800            DIST35-CDC-TR-REFILL                                          
185900         OR                                                               
186000            DIS121-SOUTH-AFRICA                                           
186010         OR                                                               
186020            DIST35-CDC-ZA-REFILL                                          
186100                                                                          
186200           MOVE SPACE             TO RAD7-TOTAL-SUORDV-W                  
186300         ELSE                                                             
186400           MOVE W-TOTAL-SUORDV-EXCH TO RAD7-TOTAL-SUORDV                  
187000         END-IF                                                           
188000                                                                          
189000*--- FÖR INDIEN SUMMERAS DELTOTALERNA TILL GRANDTOTAL I INR.              
190000*--- ORDERVÄRDE PÅ DETALJRAD VISAS EJ.                                    
200000*--- SAMMA GÄLLER FÖR KINA (BÅDE STUDS OCH ICKE-STUDS)                    
210000                                                                          
220000*---     ALT1 ÄR ATT VISA SUMMERADE INDISKA DELTOTALER (GÄLLER NU)        
220100*---     - DÅ SKA TOTALEN INOM KLIS DOKS DELTOTALER STÄMMA                
220200*---     ALT2 ÄR ATT VISA DEN KONVERTERADE SVENSKA TOTALEN                
220300*---     - DÅ SKA TOTALEN MELLAN OLIKA TRP DOK STÄMMA                     
220400                                                                          
220500*ALT2    COMPUTE WS-MONEY ROUNDED = W-TOTAL-SUORDV(2) *                   
220600*----                               WS-PRKURS-EXCH                        
220700*----    MOVE WS-MONEY            TO RAD7-TOTAL-SUORDV                    
220800                                                                          
220900       ELSE                                                               
221000         MOVE W-TOTAL-SUORDV (2) TO RAD7-TOTAL-SUORDV                     
221100         IF FLLOCCUR                                                      
221200            COMPUTE WS-MONEY ROUNDED = (W-TOTAL-SUORDV(2) *               
221300              WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                      
221400            MOVE WS-MONEY         TO RAD7-TOTAL-SUORDV                    
221500         END-IF                                                           
221600       END-IF                                                             
221700     END-IF                                                               
221800                                                                          
221900     MOVE W-TOTAL-VKORDBTO (2)  TO RAD7-TOTAL-VKORDBTO                    
222000     MOVE W-TOTAL-VKORDNTO (2)  TO RAD7-TOTAL-VKORDNTO                    
222100     MOVE W-TOTAL-VLORDBTO (2)  TO RAD7-TOTAL-VLORDBTO                    
222200*                                                                         
222300*--- JAPAN HAS A UNIQUE MEASUREMENT FOR VOLYM                             
222400     IF NDC-JP                                                            
222500       COMPUTE W-TOTAL-VLORDBTO-JP = W-TOTAL-VLORDBTO (2) *               
222600                                     280                                  
222700       MOVE W-TOTAL-VLORDBTO-JP  TO RAD7-TOTAL-VLORDBTO                   
222800     END-IF                                                               
222900*                                                                         
223000     MOVE W-TOTAL-KOLLI-COUNT (2)                                         
223100                                TO RAD7-KOLLI-TOTAL                       
223200                                                                          
223300     IF (NDC-CN AND NOT DIST35-NONVCC-NONVCC-REFILL)                      
223400       MOVE 'CNY'               TO RAD7-TOTAL-KDVALISO                    
223500     ELSE                                                                 
223600       IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                       
223700         OR                                                               
223800         DIST35-NDCUS-NDCCN-REFILL                                        
223900         OR                                                               
224000         ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                    
224100         OR                                                               
224200         DIST35-CDC-TR-REFILL                                             
224300         OR                                                               
224400         DIS121-SOUTH-AFRICA                                              
224410         OR                                                               
224420         DIST35-CDC-ZA-REFILL                                             
224500                                                                          
224600         MOVE SPACE             TO RAD7-TOTAL-KDVALISO                    
224700       ELSE                                                               
224800         IF NDC-IN OR                                                     
224900           ((CDC-SE OR DDC-SE) AND                                        
225000           (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                    
225100           MOVE 'INR'           TO RAD7-TOTAL-KDVALISO                    
225200         ELSE                                                             
225300           MOVE W-KDVALISO      TO RAD7-TOTAL-KDVALISO                    
225400           IF FLLOCCUR                                                    
225500             MOVE SGMT-KDVALISO TO RAD7-TOTAL-KDVALISO                    
225600           END-IF                                                         
225700         END-IF                                                           
225800       END-IF                                                             
225900     END-IF                                                               
226000                                                                          
226100     IF VOR-OK OR NDC-AE                                                  
226200       MOVE SPACE               TO RAD7-TOTAL-SUORDV-W                    
226300                                   RAD7-TOTAL-KDVALISO                    
226400     END-IF                                                               
226500                                                                          
226600     MOVE RAD7                  TO ARB-RAD                                
226700                                   SEND-RAD                               
226800     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
226900     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
227000     ADD 1                      TO W-LINE-COUNT                           
227100     PERFORM S21-PRINT-LINE                                               
227200                                                                          
227300*--- OM SAMLINGSKOLLI FINNS BLAND KOLLIN,                                 
227400*--- SKRIV UT ANTAL 'SINGEL-' O SAMKOLLIN                                 
227500                                                                          
227600     IF WS-ANT-KOLLI-I-ALLA-SK > ZERO                                     
227700       PERFORM S24A-PRINT-SAMKOLLI                                        
227800     END-IF                                                               
227900*---                                                                      
228000*--- FÖR INDIEN SUMMERAS REDAN I INR, INGEN ADJUSTMENT BEHÖVS             
228100*--- SAMMA FÖR KINA (CNY)                                                 
228200     IF ((CDC-SE OR DDC-SE) AND                                           
228300         (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                      
228400       OR                                                                 
228500        ((CDC-SE OR DDC-SE) AND                                           
228600         (DIST34-KINA-NDC OR DIST35-REFILL-CN))                           
228700       OR                                                                 
228800          DIST35-NDCUS-NDCCN-REFILL                                       
228900       OR                                                                 
229000        ((CDC-SE OR DDC-SE) AND EXCH-IDDISTR-TURKIET)                     
229100       OR                                                                 
229200          DIST35-CDC-TR-REFILL                                            
229300       OR                                                                 
229400          DIS121-SOUTH-AFRICA                                             
229410       OR                                                                 
229420          DIST35-CDC-ZA-REFILL                                            
229500       CONTINUE                                                           
229600     ELSE                                                                 
229700       IF FLLOCCUR                                                        
229800        IF WS-MONEY NOT = WY-MONEY                                        
229900         SUBTRACT WS-MONEY      FROM WY-MONEY                             
230000         IF WY-MONEY NOT = ZERO                                           
230100           MOVE WY-MONEY        TO RADY-DIFF                              
230200           MOVE 'CURRENCY CONVERSION ADJUSTMENT' TO RADY-TEXT             
230300           MOVE RADY            TO ARB-RAD                                
230400                                     SEND-RAD                             
230500           MOVE PRT-AFTER-2     TO PRT-RADSKIP                            
230600           MOVE WS-SKIP2        TO STYRTECKEN-RAD                         
230700           ADD 2                TO W-LINE-COUNT                           
230800           PERFORM S21-PRINT-LINE                                         
230900         END-IF                                                           
231000        END-IF                                                            
231100       END-IF                                                             
231200     END-IF                                                               
231300                                                                          
231400     IF WEB-OUTPUT                                                        
231500       MOVE 2                   TO W-CS4-NIV                              
231600       PERFORM S14-SKAPA-W476CS4                                          
231700     END-IF                                                               
231800     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
231900                                                                          
232000     PERFORM S22-INIT-TOTAL                                               
232100     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
232110*RC-CD WRITE CD TOTALS LINE                                               
232120     IF WEB-OUTPUT AND W-TOTAL-KOLLI-CNT-CD  > 0                          
232130       PERFORM S15-SKAPA-W476CS5                                          
232140     END-IF                                                               
232150     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
232160                                                                          
232170     PERFORM S22-INIT-TOTAL                                               
232180     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
232200     .                                                                    
232300     EJECT                                                                
232400                                                                          
232500 S24A-PRINT-SAMKOLLI SECTION.                                             
232600                                                                          
232700     IF W-LINE-COUNT > W-LINE-MAX - W-LINE-SAMKOLLI                       
232800       PERFORM S25-PRINT-DISTR                                            
232900       MOVE PRT-AFTER-1         TO PRT-RADSKIP                            
233000       MOVE WS-SKIP1            TO STYRTECKEN-RAD                         
233100       ADD 1                    TO W-LINE-COUNT                           
233200     END-IF                                                               
233300                                                                          
233400     COMPUTE WS-ANT-SINGLE-KOLLI =                                        
233500       W-TOTAL-KOLLI-COUNT (2) -                                          
233600       WS-ANT-KOLLI-I-ALLA-SK                                             
233700                                                                          
233800*--- DETAILS                                                              
233900     MOVE PRT-AFTER-3           TO PRT-RADSKIP                            
234000     MOVE WS-SKIP3              TO STYRTECKEN-RAD                         
234100     ADD 3                      TO W-LINE-COUNT                           
234200     MOVE RAD9                  TO ARB-RAD                                
234300                                   SEND-RAD                               
234400     PERFORM S21-PRINT-LINE                                               
234500                                                                          
234600*--- SINGLE PACKAGES                                                      
234700*--- JUST NU HÅRDKODADE LEDTEXTER                                         
234800*    MOVE SPACES                TO RAD10                                  
234900*                                  ARB-RAD                                
235000*                                  SEND-RAD                               
235100*    MOVE XXX-LEDTEXT (W-KDSPRAK)                                         
235200*                               TO RAD10-XXX-TEXT                         
235300     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
235400     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
235500     ADD 1                      TO W-LINE-COUNT                           
235600     MOVE WS-ANT-SINGLE-KOLLI TO RAD10-ANT-SINGLE-PACK                    
235700     MOVE RAD10                 TO ARB-RAD                                
235800                                   SEND-RAD                               
235900     PERFORM S21-PRINT-LINE                                               
236000     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
236100                                                                          
236200*--- MIXED PACKAGES                                                       
236300*--- JUST NU HÅRDKODADE LEDTEXTER                                         
236400*    MOVE SPACES                TO RAD11                                  
236500*                                  ARB-RAD                                
236600*                                  SEND-RAD                               
236700*    MOVE XXX-LEDTEXT (W-KDSPRAK)                                         
236800*                               TO RAD11-XXX-TEXT                         
236900     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
237000     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
237100     ADD 1                      TO W-LINE-COUNT                           
237200     MOVE WS-ANT-SK             TO RAD11-ANT-MIXED-PACK                   
237300     MOVE WS-ANT-KOLLI-I-ALLA-SK TO RAD11-ANT-SINGLE-INCL                 
237400     MOVE RAD11                 TO ARB-RAD                                
237500                                   SEND-RAD                               
237600     PERFORM S21-PRINT-LINE                                               
237700                                                                          
237800     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
237900     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
238000     ADD 1                      TO W-LINE-COUNT                           
238100     MOVE RAD12                 TO ARB-RAD                                
238200                                   SEND-RAD                               
238300     PERFORM S21-PRINT-LINE                                               
238400     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
238500     .                                                                    
238600                                                                          
238700     EJECT                                                                
238800                                                                          
238900 S25-PRINT-DISTR SECTION.                                                 
239000                                                                          
239100     MOVE NOO              TO W-NEW-DOC-FLAG                              
239200                                                                          
239300     MOVE SPACE                      TO SEND-RAD                          
239400     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
239500     PERFORM S90-PUT-DOC-LINE                                             
239600     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
239700     PERFORM S90-PUT-DOC-LINE                                             
239800     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
239900     PERFORM S90-PUT-DOC-LINE                                             
240000                                                                          
240100     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
240200     MOVE WS-TYP-IDSHIP              TO  RAD-TYP-IDSHIP                   
240300     MOVE  RAD-HEAD                  TO  ARB-RAD                          
240400                                         SEND-RAD                         
240500     MOVE WS-SKIP1                   TO  STYRTECKEN-RAD                   
240600     MOVE  PRT-NYSIDA-RAD7           TO  PRT-RADSKIP                      
240700     MOVE  7                         TO  W-LINE-COUNT                     
240800     PERFORM S21-PRINT-LINE                                               
240900                                                                          
241000     MOVE SPACE                      TO SEND-RAD                          
241100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
241200                                                                          
241300     MOVE RAD2-HEAD                  TO ARB-RAD                           
241400                                        SEND-RAD                          
241500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
241600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
241700     ADD 1                           TO W-LINE-COUNT                      
241800     PERFORM S21-PRINT-LINE                                               
241900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
242000                                                                          
242100     MOVE RAD3-HEAD                  TO ARB-RAD                           
242200                                        SEND-RAD                          
242300     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
242400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
242500     ADD 1                           TO W-LINE-COUNT                      
242600     PERFORM S21-PRINT-LINE                                               
242700     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
242800                                                                          
242900     ADD 1                     TO W-PAGE-NO                               
243000     MOVE SPACE                TO RAD1                                    
243100**--   RADEN MÅSTE INITIERAS IGEN                                         
243200                                                                          
243300**--   KOLLA TIDIGARE KOMMENTAR ->NDC-AE                                  
243400*    IF NDC-AE                                                            
243500*      MOVE SHIPPER-CITY (SHIP-INDX)                                      
243600*                              TO RAD4H-IMPORTER                          
243700*    ELSE                                                                 
243800       MOVE BET-ADBETRAD-2     TO RAD4H-IMPORTER                          
243900*    END-IF                                                               
244000     MOVE SHIP-TISKEPPN        TO W-YYMMDD                                
244100     MOVE W-YYMMDD             TO RAD1-TIAAMMDD                           
244200     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
244300     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
244400     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
244500     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
244600     MOVE W-PAGE-NO            TO RAD1-PAGE-NO                            
244700     MOVE RAD1                 TO ARB-RAD                                 
244800                                  SEND-RAD                                
244900     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
245000     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
245100     ADD 1                     TO W-LINE-COUNT                            
245200     PERFORM S21-PRINT-LINE                                               
245300     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
245400                                                                          
245500     MOVE RAD5-HEAD        TO ARB-RAD                                     
245600                              SEND-RAD                                    
245700     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
245800     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
245900     ADD 1                 TO W-LINE-COUNT                                
246000     PERFORM S21-PRINT-LINE                                               
246100     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
246200                                                                          
246300     IF WEB-OUTPUT                                                        
246400         PERFORM S11-SKAPA-W476CS1                                        
246500     END-IF                                                               
246600     .                                                                    
246700     EJECT                                                                
246800                                                                          
246900 S90-PUT-DOC-LINE SECTION.                                                
247000     IF (TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1')                 
247100                                 OR                                       
247200        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1' AND              
247300         SHIP-KDFAKSTA-EXP = 2)                                           
247400                                 OR                                       
247500        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1' AND              
247600         DIST35-CDC-AE-REFILL)                                            
247700       IF TRPD-FLSKRIV-ONDEM = YES AND NOT WEB-OUTPUT                     
247800         MOVE +1                          TO SEND-IDCOM                   
247900         MOVE 'PUT'                       TO SEND-KDFUNC                  
248000         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
248100         CALL WZ01SEND USING SEND-CONTROL-AREA                            
248200                             SEND-KVDLEN                                  
248300                             SEND-RAD-STYRTECKEN                          
248400         IF SEND-KDRC > ZERO                                              
248500           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
248600           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
248700           DELIMITED BY SIZE INTO ERRTEXT-STR                             
248800           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
248900         END-IF                                                           
249000       END-IF                                                             
249100     END-IF                                                               
249200     .                                                                    
249300     EJECT                                                                
249400 S90-PUT-DAP-HEADER SECTION.                                              
249500     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
249600                                                                          
249700     MOVE 'PUT'                           TO SEND-KDFUNC                  
249800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
249900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
250000                         SEND-KVDLEN                                      
250100                         HDR-AREA                                         
250200     IF SEND-KDRC > ZERO                                                  
250300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
250400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
250500       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
250600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
250700     END-IF                                                               
250800     .                                                                    
250900     EJECT                                                                
251000 S90-PUT-DOC      SECTION.                                                
251100     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
251200                                                                          
251300     MOVE 'PUT'                           TO SEND-KDFUNC                  
251400*    MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
251500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
251600                         SEND-KVDLEN                                      
251700                         DOC-AREA                                         
251800     IF SEND-KDRC > ZERO                                                  
251900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
252000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
252100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
252200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
252300     END-IF                                                               
252400     .                                                                    
252500* --- IMS SECTIONS  ---                                                   
252600                                                                          
252700     EJECT                                                                
252800 IMS-GU-WDE111-DIST SECTION.                                              
252900                                                                          
253000     STRING 'WDE101  *PD(IDSHIPM  =' W-IDSHIPM-X ')'                      
253100          DELIMITED BY SIZE INTO SSA1                                     
253200     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
253300                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
253400          DELIMITED BY SIZE INTO SSA2                                     
253500     MOVE '  GE' TO GOOD-STATUSCODES                                      
253600     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101-11 SSA1 SSA2            
253700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
253800     PERFORM IMS-STATUSCHECK                                              
253900     .                                                                    
254000     EJECT                                                                
254100 IMS-GNP-WDE111-DIST SECTION.                                             
254200                                                                          
254300     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
254400                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
254500          DELIMITED BY SIZE INTO SSA1                                     
254600     MOVE '  GE' TO GOOD-STATUSCODES                                      
254700     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
254800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
254900     PERFORM IMS-STATUSCHECK                                              
255000     .                                                                    
255100     EJECT                                                                
255200 IMS-GNP-WDE121 SECTION.                                                  
255300                                                                          
255400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
255500          DELIMITED BY SIZE INTO SSA1                                     
255600     MOVE 'WDE121  '          TO SSA2                                     
255700     MOVE '  GE' TO GOOD-STATUSCODES                                      
255800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
255900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
256000     PERFORM IMS-STATUSCHECK                                              
256100     .                                                                    
256200     EJECT                                                                
256300 IMS-GNP-WDE131 SECTION.                                                  
256400                                                                          
256500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
256600          DELIMITED BY SIZE INTO SSA1                                     
256700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
256800          DELIMITED BY SIZE INTO SSA2                                     
256900     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
257000          DELIMITED BY SIZE INTO SSA3                                     
257100     MOVE 'WDE131  '          TO SSA4                                     
257200     MOVE '  GE' TO GOOD-STATUSCODES                                      
257300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
257400                                                   SSA3 SSA4              
257500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
257600     PERFORM IMS-STATUSCHECK                                              
257700     .                                                                    
257800     EJECT                                                                
257900 IMS-GU-WDQ201 SECTION.                                                   
258000                                                                          
258100     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
258200          DELIMITED BY SIZE INTO SSA1                                     
258300     MOVE '  GE' TO GOOD-STATUSCODES                                      
258400     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
258500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
258600     PERFORM IMS-STATUSCHECK                                              
258700     .                                                                    
258800     EJECT                                                                
258900 IMS-GU-WDGX4738 SECTION.                                                 
259000                                                                          
259100     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4738-X ')'                    
259200          DELIMITED BY SIZE INTO SSA1                                     
259300     MOVE   'WDGX4738'     TO SSA2                                        
259400     MOVE '  GE' TO GOOD-STATUSCODES                                      
259500     CALL CBLTDLI USING GU WDR1-PCB DLI-IO-WDGX4738 SSA1 SSA2             
259600     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
259700     PERFORM IMS-STATUSCHECK                                              
259800     .                                                                    
259900     EJECT                                                                
260000 IMS-GU-WDB101 SECTION.                                                   
260100                                                                          
260200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
260300          DELIMITED BY SIZE INTO SSA1                                     
260400     MOVE '  GE' TO GOOD-STATUSCODES                                      
260500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
260600     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
260700     PERFORM IMS-STATUSCHECK                                              
260800     .                                                                    
260900     EJECT                                                                
261000 IMS-GU-WDB201 SECTION.                                                   
261100                                                                          
261200     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
261300          DELIMITED BY SIZE INTO SSA1                                     
261400     MOVE '  GE' TO GOOD-STATUSCODES                                      
261500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
261600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
261700     PERFORM IMS-STATUSCHECK                                              
261800     .                                                                    
261900 IMS-GU-WDE711-ASEQ  SECTION.                                             
262000                                                                          
262100     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
262200          DELIMITED BY SIZE INTO SSA1                                     
262300                                                                          
262400     MOVE '  ' TO GOOD-STATUSCODES                                        
262500     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE711 SSA1                    
262600     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
262700                                                                          
262800     PERFORM IMS-STATUSCHECK                                              
262900     .                                                                    
263000     EJECT                                                                
263100                                                                          
263200 IMS-STATUSCHECK SECTION.                                                 
263300                                                                          
263400     SET STATUS-IX TO 1                                                   
263500     SEARCH GOOD-STATUS                                                   
263600       AT END                                                             
263700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
263800           DELIMITED BY SIZE INTO ERRTEXT                                 
263900         DISPLAY ERRTEXT                                                  
264000         CALL FELLOG                                                      
264100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
264200         CONTINUE                                                         
264300     END-SEARCH                                                           
264400     .                                                                    
265000                                                                          
