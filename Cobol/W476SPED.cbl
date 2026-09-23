000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476SPED.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*     FUNCTION:                                                           
000900*        SUBPROGRAM TO WRITE 'BOOKING DOCUMENT' TRANSPORT                 
001000*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631.                      
001100*        THIS DOCUMENT SHOWS A VARIOUS INFORMATION.                       
001200*                                                                         
001300*                                                                         
001400*        THE PROGRAM READS     WDE1                                       
001500*        THE PROGRAM READS     WDG7                                       
001600*        THE PROGRAM READS     WDR1                                       
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                         PIC X(8)  VALUE 'W476SPED'.            
002700 77  W-CURRENT                     PIC X(50) VALUE SPACE.                 
002800                                                                          
002900 77  YES                           PIC X     VALUE 'J'.                   
003000 77  NOO                           PIC X     VALUE 'N'.                   
003100 77  IX                            PIC S9(9) VALUE +0 COMP SYNC.          
003200 77  MAX-IX                        PIC S9(9) VALUE +16 COMP SYNC.         
003300 77  WS-IX                         PIC S9(4) VALUE +0 COMP SYNC.          
003400 77  INDX                          PIC S9(4) VALUE +0 COMP SYNC.          
003500 77  INDX2                         PIC S9(4) VALUE +0 COMP SYNC.          
003600 77  INDX3                         PIC S9(4) VALUE +0 COMP SYNC.          
003700 77  INDX4                         PIC S9(4) VALUE +0 COMP SYNC.          
003800 77  INDX5                         PIC S9(4) VALUE +0 COMP SYNC.          
003900 77  INDX6                         PIC S9(4) VALUE +0 COMP SYNC.          
004000 77  KDEMBTYP-MAX                  PIC S9(4) VALUE +15 COMP SYNC.         
004100 77  BOAT-TRP                      PIC S9(3) VALUE +43 COMP-3.            
004200                                                                          
004300 77  WS-PAGE-NO                    PIC 9(3)    VALUE ZERO.                
004400 77  W-KDSPRAK                     PIC S9      COMP-3.                    
004500 77  W-SUORDV                   PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004600 77  W-KDVALISO                    PIC X(3)    VALUE SPACE.               
004700 77  WS-KDVALISO-EXCH              PIC X(3)    VALUE SPACE.               
004800 77  DUMMY-AREA                    PIC X(50)   VALUE SPACE.               
004810 77  SW-ISRAEL-C1-FC17             PIC X       VALUE 'N'.                 
004820     88 ISRAEL-C1-FC17                         VALUE 'J'.                 
004900                                                                          
005000*    --- STYRTECKEN PRINTER                                               
005100 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
005200 01  WS-SKIP1                      PIC X      VALUE ' '.                  
005300 01  WS-SKIP2                      PIC X      VALUE '0'.                  
005400 01  WS-SKIP3                      PIC X      VALUE '-'.                  
005500                                                                          
005600 01  WS-TYP-IDSHIP                 PIC X(16) VALUE                        
005700                                   'BOOKING DOCUMENT'.                    
005800                                                                          
005900 01  WS-META                       PIC X(5) VALUE '¤META'.                
006000 01  WS-IDDISTR                    PIC Z(4)9.                             
006100 01  WS-IDSHIPM-Z                  PIC Z(6)9.                             
006200 01  WS-IDSHIPM                    PIC 9(7)  VALUE ZERO.                  
006300 01  WS-TISKEPPN                   PIC S9(7) VALUE ZERO COMP-3.           
006400 01  WS-YYMMDD                     PIC 9(6)  VALUE ZERO.                  
006500 01  WS-IDLC                       PIC X(15) VALUE SPACE.                 
006600 01  WS-BESLULEV                   PIC X(20) VALUE SPACE.                 
006700 01  WS-IDBOKN                     PIC X(15) VALUE SPACE.                 
006800 01  WS-IDVCERT                    PIC X(16) VALUE SPACE.                 
006900 01  WS-IDLICENS                   PIC X(15) VALUE SPACE.                 
007000 01  WS-BEROUTE                    PIC X(25) VALUE SPACE.                 
007100 01  WS-SKOLLI-KDEMBTYP            PIC S9    VALUE ZERO.                  
007200 01  WS-KDEMBTYP-TAB.                                                     
007300   03 FILLER OCCURS 16.                                                   
007400     05  WS-KDEMBTYP               PIC 9(2)  VALUE ZERO.                  
007500     05  WS-QUANTITY               PIC S9(4) VALUE +0 COMP SYNC.          
007600 01  WS-BELEVVIL-TAB.                                                     
007700   03 FILLER OCCURS 6.                                                    
007800     05 WS-BELEVVIL                PIC X(60) VALUE SPACE.                 
007900 01  TABELL.                                                              
008000   03  FILLER OCCURS 16.                                                  
008100     05  W-TEXT-BEEMBTYP           PIC X(12).                             
008200 01  WS-DELIVERY-TAB.                                                     
008300     03 FILLER OCCURS 6.                                                  
008400       05 WS-DELIVERY-TERMS-TERMS  PIC X(35)      VALUE SPACE.            
008500 01  WS-KDFRAKT                    PIC X(20)      VALUE SPACE.            
008600*01  WS-VKARTNTO-TOT               PIC S9(6)V9(1) VALUE ZERO.             
008700 01  WS-VKARTNTO-TOT               PIC S9(6)V9(3) VALUE ZERO.             
008800 01  WS-PRARTNTO-TOT               PIC S9(6)V9(3) VALUE ZERO.             
008900 01  WS-GOODS-VALUE                PIC S9(9)V9(2) VALUE ZERO.             
009000 01  WS-TOTAL-VALUE                PIC S9(9)V9(2) VALUE ZERO.             
009100 01  WS-TEMP-VALUE                 PIC S9(9)V9(2) VALUE ZERO.             
009200 01  WS-PRFRAKT-VALUE              PIC S9(8)V9(2) VALUE ZERO.             
009300 01  WS-PRFOERS-VALUE              PIC S9(8)V9(2) VALUE ZERO.             
009400 01  WS-PRLEGKST-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
009500 01  WS-PREMBHNT-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
009600 01  WS-PRAVDRAG-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
009700 01  WS-KDVALISO                   PIC X(3)       VALUE SPACE.            
009800 01  WS-KDVALISO-2                 PIC X(3)       VALUE SPACE.            
009900 01  WS-SKOLLI-SUORDV              PIC S9(9)V9(2) VALUE ZERO.             
010000 01  WS-TOTAL-VALUE-PRKURS         PIC S9(9)V9(2) VALUE ZERO.             
010100 01  WS-PRKURS                     PIC S9(6)V9(5) VALUE ZERO.             
010200*01  WS-VKORDNTO-TOTAL             PIC S9(8)V9(2) VALUE ZERO.             
010300 01  WS-VKORDNTO-TOTAL             PIC S9(8)V9(3) VALUE ZERO.             
010400 01  WS-SUORDV-TOTAL               PIC S9(8)V9(2) VALUE ZERO.             
010500 01  WS-VKORDBTO-TOTAL             PIC S9(8)V9(2) VALUE ZERO.             
010600 01  WS-VLORDBTO-TOTAL             PIC S9(8)V9(3) VALUE ZERO.             
010700 01  W-DATE-AAMM                   PIC 9(4)    VALUE ZERO.                
010800 01  WS-KDVALISO-HUV               PIC X(3)    VALUE 'SEK'.               
010900                                                                          
011000 01  W-LINE.                                                              
011100     03  W-LINE-COUNT              PIC 9(02) VALUE ZERO.                  
011200     03  W-LINE-MAX                PIC 9(02) VALUE 40.                    
011300*    03  W-LINE-MAX                PIC 9(02) VALUE 43.                    
011400                                                                          
011500 01  TODAYS-DATE                   PIC 9(6)  VALUE ZERO.                  
011600 01  FILLER REDEFINES TODAYS-DATE.                                        
011700     03  TODAYS-DATE-YEAR          PIC 9(2).                              
011800     03  TODAYS-DATE-MONTH         PIC 9(2).                              
011900     03  TODAYS-DATE-DAY           PIC 9(2).                              
012000     EJECT                                                                
012100                                                                          
012200 01  WS-TIMESTAMP.                                                        
012300     03  FILLER                  PIC X       VALUE 'D'.                   
012400     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
012500     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
012600     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
012700     03  FILLER                  PIC X       VALUE '_'.                   
012800     03  FILLER                  PIC X       VALUE 'T'.                   
012900     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
013000     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
013100     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
013200                                                                          
013300 01  IDFKNGRP-SW                   PIC 9(5).                              
013400     88  CHARIOT                   VALUE 8001 THRU 8002.                  
013500     88  ENGINE-B                  VALUE 2101.                            
013600     88  ENGINE-D                  VALUE 2102.                            
013700     88  TOOLS                     VALUE  800 THRU  899.                  
013800     88  PAINT                     VALUE 1620 THRU 1629,                  
013900                                         1800 THRU 1899,                  
014000                                         8990 THRU 8999.                  
014100     88  SPARE-PARTS               VALUE    1 THRU  799,                  
014200                                          900 THRU 1619,                  
014300                                         1630 THRU 1799,                  
014400                                         1900 THRU 2100,                  
014500                                         2103 THRU 8000,                  
014600                                         8003 THRU 8989,                  
014700                                         9000 THRU 99999.                 
014800                                                                          
014900 77  KOLLI-SW                      PIC X(1)       VALUE SPACE.            
015000     88  KOLLI-END                 VALUE 'J'.                             
015100     88  KOLLI-START               VALUE 'N'.                             
015200                                                                          
015300 77  EMBTYP-FOUND-SW               PIC X(1)       VALUE SPACE.            
015400     88  EMBTYP-FOUND              VALUE 'J'.                             
015500 77  WS-SKLI-IX                  PIC S9(4)  VALUE +0    COMP SYNC.        
015600 77  WS-SKLI-IX-MAX              PIC S9(4)  VALUE +500  COMP SYNC.        
015700                                                                          
015800 77  TRAFF-SKLI-SW               PIC X(01)  VALUE 'N'.                    
015900     88 TRAFF-SKLI                          VALUE 'J'.                    
016000                                                                          
016100 01  SPARE-VKVIKT                  PIC 9(5)V9(3)  VALUE ZERO.             
016200 01  SPARE-PRARTNTO                PIC 9(7)V9(2)  VALUE ZERO.             
016300 01  CHARIOT-VKVIKT                PIC 9(5)V9(3)  VALUE ZERO.             
016400 01  CHARIOT-PRARTNTO              PIC 9(7)V9(2)  VALUE ZERO.             
016500 01  ENGINE-B-VKVIKT               PIC 9(5)V9(3)  VALUE ZERO.             
016600 01  ENGINE-B-PRARTNTO             PIC 9(7)V9(2)  VALUE ZERO.             
016700 01  ENGINE-D-VKVIKT               PIC 9(5)V9(3)  VALUE ZERO.             
016800 01  ENGINE-D-PRARTNTO             PIC 9(7)V9(2)  VALUE ZERO.             
016900 01  TOOLS-VKVIKT                  PIC 9(5)V9(3)  VALUE ZERO.             
017000 01  TOOLS-PRARTNTO                PIC 9(7)V9(2)  VALUE ZERO.             
017100 01  PAINT-VKVIKT                  PIC 9(5)V9(3)  VALUE ZERO.             
017200 01  PAINT-PRARTNTO                PIC 9(7)V9(2)  VALUE ZERO.             
017300                                                                          
017400*--- SAMLINGSKOLLITABELL                                                  
017500 01  WS-SKLI-TABELL.                                                      
017600   03  WS-SKLI-TABSTEG OCCURS 500.                                        
017700     05  WS-IDKOLLI-SAMP      PIC S9(5)  VALUE +0.                        
017800                                                                          
017900 01  ARB-RAD                       PIC X(132) VALUE SPACE.                
018000                                                                          
018100 01  RAD1-HEAD.                                                           
018200     03  FILLER                    PIC X(15).                             
018300     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
018400     03  FILLER                    PIC X(2).                              
018500     03  RAD1H-IMPORTER            PIC X(35).                             
018600     03  FILLER                    PIC X(2).                              
018700**   03  FILLER                    PIC X(67).                             
018800     03  RAD1-TYP-IDSHIP           PIC X(25).                             
018900                                                                          
019000 01  RAD2X-HEAD.                                                          
019100     03  FILLER                    PIC X(30).                             
019200     03  RAD2H-IMPORTER            PIC X(35).                             
019300                                                                          
019400 01  RAD3-HEAD.                                                           
019500     03  FILLER                    PIC X(30).                             
019600     03  RAD3H-IMPORTER            PIC X(35).                             
019700                                                                          
019800 01  RAD5-HEAD.                                                           
019900     03  FILLER                    PIC X(30).                             
020000     03  RAD5H-IMPORTER            PIC X(35).                             
020100                                                                          
020200 01  RAD2-HEAD.                                                           
020300     03  FILLER                    PIC X(30).                             
020400     03  RAD4H-IMPORTER            PIC X(35).                             
020500     03  FILLER                    PIC X(02).                             
020600**   03  FILLER                    PIC X(67).                             
020700     03  RAD2H-TIYYMMDD            PIC 9(06).                             
020800     03  RAD2H-IDDISTR             PIC Z(4)9.                             
020900     03  FILLER                    PIC X(01).                             
021000     03  RAD2H-IDSHIPM             PIC Z(06)9.                            
021100     03  FILLER                    PIC X(04).                             
021200     03  RAD2H-IDTRPTNR            PIC Z(02)9.                            
021300     03  FILLER                    PIC X(01).                             
021400     03  RAD2H-IDLBBET             PIC X(12).                             
021500     03  FILLER                    PIC X(02).                             
021600     03  RAD2H-PAGE-NO             PIC Z(03).                             
021700                                                                          
021800 01  RAD1.                                                                
021900     03  FILLER                    PIC X(1).                              
022000     03  RAD1-FREIGHT-MODE-TEXT    PIC X(30).                             
022100     03  FILLER                    PIC X(1).                              
022200     03  RAD1-IDLC-TEXT            PIC X(30).                             
022300     03  FILLER                    PIC X(1).                              
022400     03  RAD1-BESLULEV-TEXT        PIC X(30).                             
022500                                                                          
022600 01  RAD2.                                                                
022700     03  FILLER                    PIC X(1).                              
022800     03  RAD2-IDTRANSP-NAMN        PIC X(15).                             
022900     03  FILLER                    PIC X(16).                             
023000     03  RAD2-IDLC                 PIC X(15).                             
023100     03  FILLER                    PIC X(16).                             
023200     03  RAD2-BESLULEV             PIC X(20).                             
023300                                                                          
023400 01  RAD3.                                                                
023500     03  FILLER                    PIC X(1).                              
023600     03  RAD3-DELIVERY-TERMS-TEXT  PIC X(30).                             
023700                                                                          
023800 01  RAD4.                                                                
023900     03  FILLER                    PIC X(1).                              
024000     03  RAD4-DELIVERY-TERMS       PIC X(35).                             
024100                                                                          
024200 01  RAD5.                                                                
024300     03  FILLER                    PIC X(1).                              
024400     03  RAD5-IDBOKN-TEXT          PIC X(30).                             
024500     03  FILLER                    PIC X(1).                              
024600     03  RAD5-IDLICENS-TEXT        PIC X(30).                             
024700     03  FILLER                    PIC X(1).                              
024800     03  RAD5-IDVCERT-TEXT         PIC X(30).                             
024900                                                                          
025000 01  RAD6.                                                                
025100     03  FILLER                    PIC X(1).                              
025200     03  RAD6-IDBOKN-VALUE         PIC X(15).                             
025300     03  FILLER                    PIC X(16).                             
025400     03  RAD6-IDLICENS-VALUE       PIC X(15).                             
025500     03  FILLER                    PIC X(16).                             
025600     03  RAD6-IDVCERT-VALUE        PIC X(16).                             
025700                                                                          
025800 01  RAD7.                                                                
025900     03  FILLER                    PIC X(16).                             
026000     03  RAD7-TOTAL-VALUE-TEXT     PIC X(16).                             
026100     03  FILLER                    PIC X(3).                              
026200     03  RAD7-TOTAL-VALUE          PIC Z(8)9.9(2).                        
026300     03  FILLER                    PIC X(8).                              
026400     03  RAD7-KDVALISO             PIC X(3).                              
026500                                                                          
026600 01  RAD8.                                                                
026700     03  FILLER                    PIC X(1).                              
026800     03  RAD8-BEROUTE-TEXT         PIC X(14).                             
026900                                                                          
027000 01  RAD9.                                                                
027100     03  FILLER                    PIC X(1).                              
027200     03  RAD9-BEROUTE-VALUE        PIC X(25).                             
027300                                                                          
027400 01  RAD10.                                                               
027500     03  FILLER                    PIC X(1).                              
027600     03  RAD10-SHIPPING-NO-TEXT    PIC X(12).                             
027700     03  FILLER                    PIC X(19).                             
027800     03  RAD10-IDSKEPPN-TEXT       PIC X(30).                             
027900                                                                          
028000 01  RAD11.                                                               
028100     03  FILLER                    PIC X(1).                              
028200     03  RAD11-IDSHIPM-TEXT        PIC Z(6)9.                             
028300     03  FILLER                    PIC X(24).                             
028400     03  RAD11-TISKEPPN            PIC Z9(6).                             
028500                                                                          
028600 01  RAD12.                                                               
028700     03  FILLER                    PIC X(11).                             
028800     03  RAD12-QUANTITY-TEXT       PIC X(8).                              
028900     03  FILLER                    PIC X(2).                              
029000     03  RAD12-PACKING-TYPE-TEXT   PIC X(12).                             
029100     03  FILLER                    PIC X(33).                             
029200     03  RAD12-NET-WEIGHT-TEXT     PIC X(8).                              
029300     03  FILLER                    PIC X(7).                              
029400     03  RAD12-VALUE-TEXT          PIC X(8).                              
029500                                                                          
029600 01  RAD13.                                                               
029700     03  FILLER                    PIC X(11).                             
029800     03  RAD13-QUANTITY-VALUE      PIC Z(8).                              
029900     03  FILLER                    PIC X(2).                              
030000     03  RAD13-PACKING-TYPE        PIC X(12).                             
030100*****03  FILLER                    PIC X(5).                              
030200     03  FILLER                    PIC X(3).                              
030300     03  RAD13-KVKOLLI-TEXT        PIC X(26).                             
030400     03  FILLER                    PIC X(1).                              
030500*****03  RAD13-NET-WEIGHT-VALUE    PIC Z(6)9.9.                           
030600     03  RAD13-NET-WEIGHT-VALUE    PIC Z(6)9.999.                         
030700     03  FILLER                    PIC X(1).                              
030800     03  RAD13-VALUE-VALUE         PIC Z(10)9.9(2).                       
030900                                                                          
031000 01  RAD14.                                                               
031100     03  FILLER                    PIC X(92).                             
031200     03  RAD14-GROSS-WEIGHT-TEXT   PIC X(8).                              
031300     03  FILLER                    PIC X(2).                              
031400     03  RAD14-VOLUME-TEXT         PIC X(8).                              
031500                                                                          
031600 01  RAD15.                                                               
031700     03  FILLER                    PIC X(37).                             
031800     03  RAD15-BETOTAL-TEXT        PIC X(25).                             
031900**** 03  FILLER                    PIC X(3).                              
032000     03  FILLER                    PIC X(1).                              
032100**** 03  RAD15-VKORDNTO-TOTAL      PIC Z(6)9.9.                           
032200     03  RAD15-VKORDNTO-TOTAL      PIC Z(6)9.999.                         
032300     03  FILLER                    PIC X(1).                              
032400     03  RAD15-SUORDV-TOTAL        PIC Z(10)9.9(2).                       
032500     03  FILLER                    PIC X(3).                              
032600     03  RAD15-VKORDBTO-TOTAL      PIC Z(5)9.9.                           
032700     03  FILLER                    PIC X(2).                              
032800     03  RAD15-VLORDBTO-TOTAL      PIC Z(3)9.9(3).                        
032900                                                                          
033000 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
033100*01  FILLER   -COPY WWDIST25    -RED TEST-IDDISTR.                        
033200     EJECT                                                                
033210*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
033220     EJECT                                                                
033300*01  FILLER   -COPY WWDIST41    -RED TEST-IDDISTR.                        
033400     EJECT                                                                
033500*01  FILLER   -COPY WWDIST76    -RED TEST-IDDISTR.                        
033600     EJECT                                                                
033700*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
033800     EJECT                                                                
033900*01  FILLER   -COPY WWDIS121    -RED TEST-IDDISTR.                        
034000     EJECT                                                                
034100*01    -COPY WWDC99                                                       
034200     EJECT                                                                
034300                                                                          
034400 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
034500* 01 -COPY W475W551                                                       
034600     EJECT                                                                
034700* 01 -COPY W475W552                                                       
034800     EJECT                                                                
034900* 01 -COPY W475W554                                                       
035000     EJECT                                                                
035100* 01 -COPY W475W559                                                       
035200     EJECT                                                                
035300* 01 -COPY W476W001                                                       
035400     EJECT                                                                
035500* 01 -COPY W476W002                                                       
035600     EJECT                                                                
035700                                                                          
035800 01  GENERAL-SUBPROGRAMS.                                                 
035900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
036000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
036200     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
036300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
036400     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
036500     SKIP2                                                                
036600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
036700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
036800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
036900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
037000     SKIP2                                                                
037100 01  ERRTEXT.                                                             
037200     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
037300     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
037400     EJECT                                                                
037500 77  KDRC-DISPLAY                PIC Z(5).                                
037600*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
037700     EJECT                                                                
037800*01  -COPY W510CURR                                                       
037900*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
038000     EJECT                                                                
038100*01  -COPY W006PRAR                                                       
038200 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
038300*                                        WRITE ON NEW LINE 10             
038400 01  W-IDPRTLST                  PIC X(8).                                
038500     SKIP2                                                                
038600*    --- AREAS FOR IMS-SECTIONS                                           
038700*                                                                         
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039000     SKIP3                                                                
039100 01  KEYS-TO-DLI.                                                         
039200                                                                          
039300     03  W-IDSHIPM-X.                                                     
039400         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
039500                                                                          
039600     03  W-WDE111KY-X.                                                    
039700         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
039800         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
039900                                                                          
040000     03  W-WDE111KY-MIN.                                                  
040100         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
040200         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
040300                                                                          
040400     03  W-WDE111KY-MAX.                                                  
040500         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
040600         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
040700                                                                          
040800     03  W-WDE121KY-X.                                                    
040900         05  W-WDE121-IDPRODNR   PIC S9(07)  VALUE ZERO COMP-3.           
041000         05  W-WDE121-IDKOLLI    PIC S9(05)  VALUE ZERO COMP-3.           
041100                                                                          
041200     03  W-WDB201KY-X.                                                    
041300         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
041400         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
041500                                                                          
041600     03  W-WDB101KY-X.                                                    
041700         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
041800         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
041900                                                                          
042000     03  W-WDE7ASEQ-X.                                                    
042100         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
042200         05 W-IDKOLLI-SAMP       PIC S9(5)   VALUE ZERO  COMP-3.          
042300                                                                          
042400                                                                          
042500*    03     -COPY WDGX01                                                  
042600                                                                          
042700*    03     -COPY WDGK4732                                                
042800                                                                          
042900*    03     -COPY WDGK4735                                                
043000                                                                          
043100*    03     -COPY WDGK4738                                                
043200                                                                          
043300     EJECT                                                                
043400*    --- STATUS-KOD FRÅN IMS                                              
043500 01  STATUS-WS                   PIC XX.                                  
043600     88  SEGMENT-FOUND                       VALUE '  '.                  
043700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
043800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
043900                                                                          
044000 01  GOOD-STATUSCODES.                                                    
044100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044200                                                                          
044300 01  SSA1                        PIC X(64).                               
044400 01  SSA2                        PIC X(64).                               
044500 01  SSA3                        PIC X(64).                               
044600 01  SSA4                        PIC X(64).                               
044700     EJECT                                                                
044800                                                                          
044900 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
045000 01  SEND-AREA.                                                           
045100*    03  -COPY WZ01SEND                                                   
045200                                                                          
045300 01  SEND-RAD-STYRTECKEN.                                                 
045400     03  STYRTECKEN-RAD          PIC X.                                   
045500     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
045600                                                                          
045700 01  DAP-AREA-START              PIC X(24)   VALUE                        
045800                                             'DAP-AREA-START'.            
045900                                                                          
046000*    ---  IMS FUNCTION CODES                                              
046100*01  -COPY W0003                                                          
046200     EJECT                                                                
046300                                                                          
046400*    ---  DLI INPUT-OUTPUT AREA                                           
046500 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
046600 01  DLI-IO-WDE101-11.                                                    
046700     03  DLI-IO-WDE101.                                                   
046800*        05  -COPY WDE101                                                 
046900     03  DLI-IO-WDE111.                                                   
047000*        05  -COPY WDE111                                                 
047100     EJECT                                                                
047200                                                                          
047300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
047400 01  DLI-IO-WDE121.                                                       
047500*    03  -COPY WDE121                                                     
047600                                                                          
047700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE122'.                      
047800 01  DLI-IO-WDE122.                                                       
047900*    03  -COPY WDE122                                                     
048000                                                                          
048100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
048200 01  DLI-IO-WDE131.                                                       
048300*    03  -COPY WDE131                                                     
048400                                                                          
048500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4732'.                    
048600 01  DLI-IO-4732.                                                         
048700*    03  -COPY WDGX4732                                                   
048800                                                                          
048900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4735'.                    
049000 01  DLI-IO-4735.                                                         
049100*    03  -COPY WDGX4735                                                   
049200                                                                          
049300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4738'.                    
049400 01  DLI-IO-4738.                                                         
049500*    03 -COPY WDGX4738                                                    
049600                                                                          
049700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
049800 01  DLI-IO-WDB101.                                                       
049900*    03  -COPY WDB101                                                     
050000                                                                          
050100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
050200 01  DLI-IO-WDB201.                                                       
050300*    03  -COPY WDB201                                                     
050400                                                                          
050500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE711'.                      
050600 01  DLI-IO-WDE711.                                                       
050700*    03  -COPY WDE711                                                     
050800                                                                          
050900     EJECT                                                                
051000                                                                          
051100 LINKAGE SECTION.                                                         
051200*01  -COPY W476TRPD                                                       
051300                                                                          
051400 01  ALT-PCB                     PIC X(32).                               
051500                                                                          
051600*01  -COPY W0008  -PRE WDE1-                                              
051700     05  FILLER                  PIC X.                                   
051800                                                                          
051900*01  -COPY W0008  -PRE 4732-                                              
052000     05  FILLER                  PIC X.                                   
052100                                                                          
052200*01  -COPY W0008  -PRE 4735-                                              
052300     05  FILLER                  PIC X.                                   
052400                                                                          
052500*01  -COPY W0008  -PRE 4738-                                              
052600     05  FILLER                  PIC X.                                   
052700                                                                          
052800*01  -COPY W0008  -PRE WDB2-                                              
052900     05  FILLER                  PIC X.                                   
053000                                                                          
053100*01  -COPY W0008  -PRE WDB1-                                              
053200     05  FILLER                  PIC X.                                   
053300                                                                          
053400*01  -COPY W0008  -PRE WDE7-                                              
053500     05  FILLER                  PIC X.                                   
053600                                                                          
053700*01  -COPY W0008  -PRE WDG2-                                              
053800     05  FILLER                  PIC X.                                   
053900                                                                          
054000     EJECT                                                                
054100                                                                          
054200 PROCEDURE DIVISION  USING                                                
054300                                                                          
054400                           TRPD-W476TRPD ALT-PCB WDE1-PCB                 
054500                           4732-PCB 4735-PCB 4738-PCB                     
054600                           WDB2-PCB WDB1-PCB                              
054700                           WDE7-PCB WDG2-PCB.                             
054800                                                                          
054900 MAIN SECTION.                                                            
055000     ENTRY 'DLITCBL' USING                                                
055100                                                                          
055200                           TRPD-W476TRPD ALT-PCB WDE1-PCB                 
055300                           4732-PCB 4735-PCB 4738-PCB                     
055400                           WDB2-PCB WDB1-PCB                              
055500                           WDE7-PCB WDG2-PCB.                             
055600                                                                          
055700     PERFORM A-INIT                                                       
055800                                                                          
055900     PERFORM IMS-GU-WDE101                                                
056000     PERFORM B-SHIP-INFO                                                  
056100                                                                          
056200     PERFORM IMS-GNP-WDE111                                               
056300     PERFORM UNTIL SEGMENT-MISSING                                        
056400       MOVE SGMT-IDDISTR  TO W-WDE111-IDDISTR                             
056500       MOVE SGMT-IDKUNDNR TO W-WDE111-IDKUNDNR                            
056600       PERFORM S20-HAMTA-WDB2                                             
056700       PERFORM C-SHIPPING-DETAIL                                          
056800       PERFORM IMS-GNP-WDE111                                             
056900     END-PERFORM                                                          
057000                                                                          
057100     PERFORM D-COMPUTE-TOTALS                                             
057200     PERFORM E-PRINT-DOCUMENT                                             
057300                                                                          
057400     MOVE ZERO TO RETURN-CODE                                             
057500     GOBACK                                                               
057600     .                                                                    
057700     EJECT                                                                
057800 A-INIT SECTION.                                                          
057900                                                                          
058000     MOVE FUNCTION CURRENT-DATE (3:6)  TO TODAYS-DATE                     
058100                                                                          
058900     MOVE SPACE           TO  RAD1                                        
059000                              RAD2                                        
059100                              RAD3                                        
059200                              RAD4                                        
059300                              RAD5                                        
059400                              RAD6                                        
059500                              RAD7                                        
059600                              RAD8                                        
059700                              RAD9                                        
059800                              RAD10                                       
059900                              RAD11                                       
060000                              RAD12                                       
060100                              RAD13                                       
060200                              RAD14                                       
060300                              RAD15                                       
060400                              RAD1-HEAD                                   
060500                              RAD2-HEAD                                   
060600                              RAD2X-HEAD                                  
060700                              RAD3-HEAD                                   
060800                              RAD5-HEAD                                   
060900                              WS-KDVALISO                                 
061000                                                                          
061100     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
061200     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
061300     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
061400     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
061500                              W-WDB201-IDDISTR                            
061600                              W-WDE111-IDDISTR-MIN                        
061700                              W-WDE111-IDDISTR-MAX                        
061800                              TEST-IDDISTR                                
061900                                                                          
062000     MOVE ZERO            TO  WS-TEMP-VALUE                               
062100                              WS-GOODS-VALUE                              
062200                              WS-TOTAL-VALUE                              
062300                              WS-YYMMDD                                   
062400                              SPARE-VKVIKT                                
062500                              SPARE-PRARTNTO                              
062600                              CHARIOT-VKVIKT                              
062700                              CHARIOT-PRARTNTO                            
062800                              ENGINE-B-VKVIKT                             
062900                              ENGINE-B-PRARTNTO                           
063000                              ENGINE-D-VKVIKT                             
063100                              ENGINE-D-PRARTNTO                           
063200                              TOOLS-VKVIKT                                
063300                              TOOLS-PRARTNTO                              
063400                              PAINT-VKVIKT                                
063500                              PAINT-PRARTNTO                              
063600                              WS-SKOLLI-SUORDV                            
063700                              WS-PRFRAKT-VALUE                            
063800                              WS-PRFOERS-VALUE                            
063900                              WS-PRLEGKST-VALUE                           
064000                              WS-PREMBHNT-VALUE                           
064100                              WS-PRAVDRAG-VALUE                           
064200                              WS-VKORDNTO-TOTAL                           
064300                              WS-SUORDV-TOTAL                             
064400                              WS-VKORDBTO-TOTAL                           
064500                              WS-VLORDBTO-TOTAL                           
064600                              WS-PAGE-NO                                  
064700                              INDX                                        
064800                              INDX2                                       
064900                              INDX3                                       
065000                              INDX4                                       
065100                              INDX5                                       
065200                              INDX6                                       
065300                              WS-IX                                       
065400                              WS-IDDISTR                                  
065500                              WS-IDSHIPM-Z                                
065600                                                                          
065700     MOVE 2                TO W-KDSPRAK                                   
065800     MOVE NOO              TO KOLLI-SW                                    
065900                                                                          
066000     MOVE +1 TO IX                                                        
066100     PERFORM UNTIL IX > MAX-IX                                            
066200       MOVE ZERO TO WS-KDEMBTYP (IX)                                      
066300                    WS-QUANTITY (IX)                                      
066400       ADD +1 TO IX                                                       
066500     END-PERFORM                                                          
066600                                                                          
066700     .                                                                    
066800     EJECT                                                                
066900 B-SHIP-INFO SECTION.                                                     
067000                                                                          
067100     IF SEGMENT-FOUND                                                     
067200       MOVE SHIP-IDSHIPM    TO WS-IDSHIPM                                 
067300       MOVE SHIP-TISKEPPN   TO WS-TISKEPPN                                
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 C-SHIPPING-DETAIL SECTION.                                               
067800                                                                          
067900     PERFORM CA-COMPLEMENTARY-VALUE                                       
068000                                                                          
068100     PERFORM CB-GOODS-VALUE                                               
068200                                                                          
068300     PERFORM CC-TOTAL-VALUE                                               
068400                                                                          
068500     PERFORM CD-DELIVERY-TERMS-TEXT                                       
068600                                                                          
068700     PERFORM CE-EQUAL-TO                                                  
068800     .                                                                    
068900     EJECT                                                                
069000 CA-COMPLEMENTARY-VALUE SECTION.                                          
069100                                                                          
069200     PERFORM IMS-GNP-WDE122                                               
069300     IF SEGMENT-FOUND                                                     
069400       IF TILL-PRFRAKT > ZERO                                             
069500         COMPUTE WS-PRFRAKT-VALUE = WS-PRFRAKT-VALUE +                    
069600                                    TILL-PRFRAKT                          
069700       END-IF                                                             
069800                                                                          
069900       IF TILL-PRFOERS > ZERO                                             
070000         COMPUTE WS-PRFOERS-VALUE = WS-PRFOERS-VALUE +                    
070100                                    TILL-PRFOERS                          
070200       END-IF                                                             
070300                                                                          
070400       IF TILL-PRLEGKST > ZERO                                            
070500         COMPUTE WS-PRLEGKST-VALUE = WS-PRLEGKST-VALUE +                  
070600                                     TILL-PRLEGKST                        
070700       END-IF                                                             
070800                                                                          
070900       IF TILL-PREMBHNT > ZERO                                            
071000         COMPUTE WS-PREMBHNT-VALUE = WS-PREMBHNT-VALUE +                  
071100                                     TILL-PREMBHNT                        
071200       END-IF                                                             
071300                                                                          
071400       IF TILL-PRAVDRAG > ZERO                                            
071500         COMPUTE WS-PRAVDRAG-VALUE = WS-PRAVDRAG-VALUE +                  
071600                                     TILL-PRAVDRAG                        
071700       END-IF                                                             
071800       IF INDX2 = ZERO                                                    
071900         IF TILL-IDLC > SPACE                                             
072000           MOVE TILL-IDLC            TO WS-IDLC                           
072100         END-IF                                                           
072200         IF TILL-BESLULEV > SPACE                                         
072300           MOVE TILL-BESLULEV        TO WS-BESLULEV                       
072400         END-IF                                                           
072500         IF TILL-IDBOKN > SPACE                                           
072600           MOVE TILL-IDBOKN          TO WS-IDBOKN                         
072700         END-IF                                                           
072800         IF TILL-IDLICENS > SPACE                                         
072900           MOVE TILL-IDLICENS        TO WS-IDLICENS                       
073000         END-IF                                                           
073100         IF TILL-IDVCERT > SPACE                                          
073200           MOVE TILL-IDVCERT         TO WS-IDVCERT                        
073300         END-IF                                                           
073400         IF TILL-IDSIGILL > SPACE                                         
073500           MOVE TILL-IDSIGILL        TO WS-BEROUTE                        
073600         END-IF                                                           
073700         IF TILL-TISKEPPN-MAN > ZERO                                      
073800           MOVE TILL-TISKEPPN-MAN    TO WS-TISKEPPN                       
073900         END-IF                                                           
074000         ADD +1                      TO INDX2                             
074100       END-IF                                                             
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 CB-GOODS-VALUE SECTION.                                                  
074600                                                                          
074610     MOVE NOO                       TO SW-ISRAEL-C1-FC17                  
074620                                                                          
074700     PERFORM IMS-GNP-WDE121                                               
074800                                                                          
074900     IF SEGMENT-FOUND                                                     
075000       PERFORM CBA-FREIGHT-MODE-TEXT                                      
075100     END-IF                                                               
075200                                                                          
075300     PERFORM UNTIL SEGMENT-MISSING                                        
075400       PERFORM CBB-COMPUTE-VALUE                                          
075500       PERFORM CBC-PACKAGE-TYPE                                           
075600       PERFORM CBD-PACKAGE-VOLUME-WEIGHT                                  
075610       PERFORM CBF-ISRAEL-CLASS1-FC17                                     
075700                                                                          
075800       MOVE SKOLLI-IDPRODNR         TO W-WDE121-IDPRODNR                  
075900       MOVE SKOLLI-IDKOLLI          TO W-WDE121-IDKOLLI                   
076000       PERFORM IMS-GNP-WDE131                                             
076100                                                                          
076200       PERFORM UNTIL SEGMENT-MISSING                                      
076300         PERFORM CBE-ALL-PACKING                                          
076400         PERFORM IMS-GNP-WDE131                                           
076500       END-PERFORM                                                        
076600                                                                          
076700       PERFORM IMS-GNP-WDE121                                             
076800     END-PERFORM                                                          
076900     .                                                                    
077000     EJECT                                                                
077100 CBA-FREIGHT-MODE-TEXT SECTION.                                           
077200                                                                          
077300     MOVE '4732'                     TO IDHTYP                            
077400     MOVE LOW-VALUE                  TO NYCKEL-VALFRI                     
077500     MOVE SKOLLI-KDFRAKT             TO KDFRAKT-4732                      
077600                                                                          
077700     PERFORM IMS-4732-GET-ROOT                                            
077800                                                                          
077900     IF SEGMENT-FOUND                                                     
078000       PERFORM IMS-4732-GET-SEGMENT                                       
078100       IF SEGMENT-FOUND                                                   
078200         MOVE FRAKT-BEFRAKT (W-KDSPRAK) TO WS-KDFRAKT                     
078300       ELSE                                                               
078400         MOVE SPACE                  TO WS-KDFRAKT                        
078500       END-IF                                                             
078600     ELSE                                                                 
078700       MOVE SPACE                    TO WS-KDFRAKT                        
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100 CBB-COMPUTE-VALUE SECTION.                                               
079200                                                                          
079300     MOVE ZERO                       TO WS-TEMP-VALUE                     
079400     IF SKOLLI-SUORDV > ZERO                                              
079500       MOVE SKOLLI-SUORDV            TO WS-TEMP-VALUE                     
079600                                        WS-SKOLLI-SUORDV                  
079700       MOVE 'SEK'                    TO WS-KDVALISO                       
079800     ELSE                                                                 
079900       COMPUTE WS-TEMP-VALUE =                                            
080000       SKOLLI-SUORDV-LOC + SKOLLI-SUORDV-LOCPREL                          
080100       MOVE SKOLLI-KDVALISO          TO WS-KDVALISO                       
080200     END-IF                                                               
080300                                                                          
080400     COMPUTE WS-GOODS-VALUE = WS-GOODS-VALUE + WS-TEMP-VALUE              
080500     .                                                                    
080600     EJECT                                                                
080700 CBC-PACKAGE-TYPE SECTION.                                                
080800                                                                          
080900     MOVE SKOLLI-KDEMBTYP   TO WS-SKOLLI-KDEMBTYP                         
081000     IF WS-SKOLLI-KDEMBTYP < +1                                           
081100       MOVE +2              TO WS-SKOLLI-KDEMBTYP                         
081200     END-IF                                                               
081300                                                                          
081400     MOVE +1 TO INDX3                                                     
081500     PERFORM UNTIL INDX3 > KDEMBTYP-MAX                                   
081600       IF WS-SKOLLI-KDEMBTYP = INDX3                                      
081700         IF WS-SKOLLI-KDEMBTYP = WS-KDEMBTYP (INDX3)                      
081800           ADD +1 TO WS-QUANTITY (INDX3)                                  
081900           ADD KDEMBTYP-MAX TO INDX3                                      
082000         ELSE                                                             
082100           IF WS-KDEMBTYP (INDX3) = ZERO                                  
082200             MOVE WS-SKOLLI-KDEMBTYP TO WS-KDEMBTYP (INDX3)               
082300             MOVE +1 TO WS-QUANTITY (INDX3)                               
082400             ADD KDEMBTYP-MAX TO INDX3                                    
082500           END-IF                                                         
082600         END-IF                                                           
082700       END-IF                                                             
082800       ADD +1 TO INDX3                                                    
082900     END-PERFORM                                                          
083000                                                                          
083100     IF SKOLLI-KDEMBTYP > KDEMBTYP-MAX                                    
083200       MOVE 'NOT ENOUGH LINES IN KDEMBTYP TABLE' TO ERRTEXT-STR           
083300       DISPLAY ERRTEXT                                                    
083400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083500     END-IF                                                               
083600     .                                                                    
083700     EJECT                                                                
083800 CBD-PACKAGE-VOLUME-WEIGHT SECTION.                                       
083900                                                                          
084000     MOVE SGMT-IDDC    TO WS-IDDC                                         
084100                                                                          
084200     IF (CDC-SE OR DDC-SE) AND                                            
084300       SKOLLI-IDKOLLI-SAMP > ZERO                                         
084400       PERFORM CBDA-SAMKOLLI                                              
084500     ELSE                                                                 
084600       COMPUTE WS-VKORDBTO-TOTAL = WS-VKORDBTO-TOTAL +                    
084700               SKOLLI-VKORDBTO-KOLLI                                      
084800                                                                          
084900       COMPUTE WS-VLORDBTO-TOTAL = WS-VLORDBTO-TOTAL +                    
085000               SKOLLI-VLORDBTO-KOLLI                                      
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400                                                                          
085500 CBDA-SAMKOLLI      SECTION.                                              
085600                                                                          
085700*---   KOLLIT INGÅR I ETT SAMKOLLI                                        
085800*---   HÄMTA/ADDERA VIKT O VOLYM EN GÅNG PER SAMKOLLI                     
085900                                                                          
086000     MOVE 1            TO WS-SKLI-IX                                      
086100     MOVE NOO          TO TRAFF-SKLI-SW                                   
086200                                                                          
086300     PERFORM UNTIL WS-SKLI-IX > WS-SKLI-IX-MAX OR                         
086400       TRAFF-SKLI                                                         
086500       IF SKOLLI-IDKOLLI-SAMP = WS-IDKOLLI-SAMP (WS-SKLI-IX)              
086600         MOVE YES      TO TRAFF-SKLI-SW                                   
086700       ELSE                                                               
086800         IF WS-IDKOLLI-SAMP (WS-SKLI-IX) = ZERO                           
086900           MOVE SKOLLI-IDKOLLI-SAMP                                       
087000                       TO WS-IDKOLLI-SAMP (WS-SKLI-IX)                    
087100           MOVE YES    TO TRAFF-SKLI-SW                                   
087200                                                                          
087300           MOVE SHIP-IDDC            TO W-IDDC                            
087400           MOVE SKOLLI-IDKOLLI-SAMP  TO W-IDKOLLI-SAMP                    
087500                                                                          
087600           PERFORM IMS-GU-WDE711-ASEQ                                     
087700                                                                          
087800           COMPUTE WS-VKORDBTO-TOTAL = WS-VKORDBTO-TOTAL +                
087900                   SKLI-VKKOLLIB-SAMP                                     
088000                                                                          
088100           COMPUTE WS-VLORDBTO-TOTAL = WS-VLORDBTO-TOTAL +                
088200                   SKLI-VLKOLLIB-SAMP                                     
088300         ELSE                                                             
088400           ADD +1      TO WS-SKLI-IX                                      
088500         END-IF                                                           
088600       END-IF                                                             
088700     END-PERFORM                                                          
088800                                                                          
088900                                                                          
089000     .                                                                    
089100 CBE-ALL-PACKING SECTION.                                                 
089200                                                                          
089300     MOVE ZERO             TO WS-PRARTNTO-TOT                             
089400                                                                          
089500*    COMPUTE WS-VKARTNTO-TOT   = SRAD-VKARTNTO * SRAD-KVLEVART            
089600     COMPUTE WS-VKARTNTO-TOT   = SRAD-VKART-NTO-KG * SRAD-KVLEVART        
089700                                                                          
089800     IF SRAD-PRARTNTO > ZERO                                              
089900       COMPUTE WS-PRARTNTO-TOT = SRAD-PRARTNTO * SRAD-KVLEVART            
090000     ELSE                                                                 
090100                                                                          
090200       IF SRAD-PRARTNTO-LOC      > ZERO                                   
090300         COMPUTE WS-PRARTNTO-TOT = SRAD-PRARTNTO-LOC *                    
090400                                   SRAD-KVLEVART                          
090500       ELSE                                                               
090600         IF SRAD-PRARTNTO-LOCPREL > ZERO                                  
090700           COMPUTE WS-PRARTNTO-TOT = SRAD-PRARTNTO-LOCPREL *              
090800                                     SRAD-KVLEVART                        
090900         END-IF                                                           
091000       END-IF                                                             
091100     END-IF                                                               
091200                                                                          
091300     MOVE SRAD-IDFKNGRP    TO IDFKNGRP-SW                                 
091400     IF SPARE-PARTS                                                       
091500       ADD WS-VKARTNTO-TOT TO SPARE-VKVIKT                                
091600       ADD WS-PRARTNTO-TOT TO SPARE-PRARTNTO                              
091700     END-IF                                                               
091800     IF CHARIOT                                                           
091900       ADD WS-VKARTNTO-TOT TO CHARIOT-VKVIKT                              
092000       ADD WS-PRARTNTO-TOT TO CHARIOT-PRARTNTO                            
092100     END-IF                                                               
092200     IF ENGINE-B                                                          
092300       ADD WS-VKARTNTO-TOT TO ENGINE-B-VKVIKT                             
092400       ADD WS-PRARTNTO-TOT TO ENGINE-B-PRARTNTO                           
092500     END-IF                                                               
092600     IF ENGINE-D                                                          
092700       ADD WS-VKARTNTO-TOT TO ENGINE-D-VKVIKT                             
092800       ADD WS-PRARTNTO-TOT TO ENGINE-D-PRARTNTO                           
092900     END-IF                                                               
093000     IF TOOLS                                                             
093100       ADD WS-VKARTNTO-TOT TO TOOLS-VKVIKT                                
093200       ADD WS-PRARTNTO-TOT TO TOOLS-PRARTNTO                              
093300     END-IF                                                               
093400     IF PAINT                                                             
093500       ADD WS-VKARTNTO-TOT TO PAINT-VKVIKT                                
093600       ADD WS-PRARTNTO-TOT TO PAINT-PRARTNTO                              
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
093910*FOR ISRAEL CLASS 1 AND FC 17 , INCOTERM SHOULD BE 'FCA'                  
094000 CBF-ISRAEL-CLASS1-FC17 SECTION.                                          
094100                                                                          
094101     IF DIST25-ISRAEL-FRAKT AND                                           
094102        SKOLLI-KDORDKL  = 1 AND                                           
094103        SKOLLI-KDFRAKT  = 17                                              
094104        MOVE YES                     TO SW-ISRAEL-C1-FC17                 
094105     END-IF                                                               
094110     .                                                                    
094120     EJECT                                                                
094130 CC-TOTAL-VALUE SECTION.                                                  
094140                                                                          
094200     COMPUTE WS-TOTAL-VALUE = WS-GOODS-VALUE    +                         
094300                              WS-PRFRAKT-VALUE  +                         
094400                              WS-PRFOERS-VALUE  +                         
094500                              WS-PRLEGKST-VALUE +                         
094600                              WS-PREMBHNT-VALUE +                         
094700                              WS-PRAVDRAG-VALUE                           
094800     .                                                                    
094900     EJECT                                                                
095000 CD-DELIVERY-TERMS-TEXT SECTION.                                          
095100                                                                          
095200     IF SGMT-KDLEVVIL > ZERO                                              
095300       MOVE '4735'                   TO IDHTYP                            
095400       MOVE LOW-VALUE                TO NYCKEL-VALFRI                     
095500       MOVE SGMT-KDLEVVIL            TO KDLEVVIL-4735                     
095600                                                                          
095700       PERFORM IMS-4735-GET-ROOT                                          
095800                                                                          
095900       IF SEGMENT-FOUND                                                   
096000         PERFORM IMS-4735-GET-SEGMENT                                     
096100         IF SEGMENT-FOUND                                                 
096200           MOVE +1                   TO INDX                              
096300           PERFORM UNTIL INDX > 6                                         
096400             MOVE LEVVIL-BELEVVIL(INDX)                                   
096500                            TO WS-DELIVERY-TERMS-TERMS(INDX)              
096600             ADD +1                  TO INDX                              
096700           END-PERFORM                                                    
096800         ELSE                                                             
096900           MOVE +1                   TO INDX                              
097000           PERFORM UNTIL INDX > 6                                         
097100             MOVE SPACE     TO WS-DELIVERY-TERMS-TERMS(INDX)              
097200             ADD +1                  TO INDX                              
097300           END-PERFORM                                                    
097400         END-IF                                                           
097500       ELSE                                                               
097600         MOVE +1                   TO INDX                                
097700         PERFORM UNTIL INDX > 6                                           
097800           MOVE SPACE     TO WS-DELIVERY-TERMS-TERMS(INDX)                
097900           ADD +1                  TO INDX                                
098000         END-PERFORM                                                      
098100       END-IF                                                             
098200       MOVE WS-DELIVERY-TERMS-TERMS(W-KDSPRAK) TO                         
098300            RAD4-DELIVERY-TERMS                                           
098400     ELSE                                                                 
098500       PERFORM CDA-WRITE-SPEC-LEVVIL                                      
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 CDA-WRITE-SPEC-LEVVIL SECTION.                                           
099000                                                                          
099100     MOVE SGMT-IDDISTR TO TEST-IDDISTR                                    
099200     IF DIST41-DDU-FRAKT                                                  
099300       MOVE         'DDU CONSIGNEE (INCOTERMS 2010) '     TO              
099400                     RAD4-DELIVERY-TERMS                                  
099500     ELSE                                                                 
099600       IF DIST41-CIP-FRAKT                                                
099700         MOVE       'CIP                            '     TO              
099800                     RAD4-DELIVERY-TERMS                                  
099810         IF ISRAEL-C1-FC17                                                
099820            MOVE  'FCA                            '   TO                  
099830                     RAD4-DELIVERY-TERMS                                  
099840         END-IF                                                           
099900       ELSE                                                               
100000         MOVE SGMT-IDDISTR TO TEST-IDDISTR                                
100100         MOVE SGMT-IDDC    TO WS-IDDC                                     
100200         IF TILL-PRFRAKT = ZERO                                           
100300           IF CDC-SE OR DDC-SE                                            
100400             EVALUATE TRUE                                                
100500             WHEN DIST76-ROMANIA                                          
100600               MOVE 'CIP CONSIGNEE                  '     TO              
100700                     RAD4-DELIVERY-TERMS                                  
100800             WHEN DIST76-RYSSLAND                                         
100900               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
101000                     RAD4-DELIVERY-TERMS                                  
101100             WHEN DIST76-RYSSLAND-2606                                    
101200               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
101300                     RAD4-DELIVERY-TERMS                                  
101400             WHEN DIST76-VITRYSSLAND                                      
101500               MOVE 'CIP DNEPROPETROVSK             '     TO              
101600                     RAD4-DELIVERY-TERMS                                  
101700             WHEN DIST76-PERU                                             
101800               MOVE 'FCA GOTHENBURG                 '     TO              
101900                     RAD4-DELIVERY-TERMS                                  
102000             WHEN DIST76-BELARUS                                          
102100               MOVE 'CIP                            '     TO              
102200                     RAD4-DELIVERY-TERMS                                  
102300             WHEN DIST76-MOLDAVIA                                         
102400               MOVE 'DAP CHISINAU                   '     TO              
102500                     RAD4-DELIVERY-TERMS                                  
102600             WHEN DIST76-OMAN                                             
102700               MOVE 'DDP                            '     TO              
102800                     RAD4-DELIVERY-TERMS                                  
102900             WHEN DIST76-COLUMBIA                                         
103000               MOVE 'FCA GOTHENBURG                 '     TO              
103100                     RAD4-DELIVERY-TERMS                                  
103200             WHEN DIST76-CANADA-REF                                       
103300               MOVE 'CIP                            '     TO              
103400                     RAD4-DELIVERY-TERMS                                  
103500             WHEN DIST76-SLOVENIA                                         
103600               MOVE 'CIP DEALER                     '     TO              
103700                     RAD4-DELIVERY-TERMS                                  
103800             WHEN DIST76-BOSNIA                                           
103900               MOVE 'CIP LJUBLJANA                  '     TO              
104000                     RAD4-DELIVERY-TERMS                                  
104100             WHEN DIST76-MACEDONIA                                        
104200               MOVE 'CIP LJUBLJANA                  '     TO              
104300                     RAD4-DELIVERY-TERMS                                  
104400             WHEN DIST76-SERBIA                                           
104500               MOVE 'CIP LJUBLJANA                  '     TO              
104600                     RAD4-DELIVERY-TERMS                                  
104700             WHEN DIST76-GEORGIA                                          
104800               MOVE 'CIP TBILISI                    '     TO              
104900                     RAD4-DELIVERY-TERMS                                  
105000             WHEN DIST76-TUNISIA                                          
105100               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
105200                     RAD4-DELIVERY-TERMS                                  
105300             WHEN OTHER                                                   
105400               MOVE 'FCA GÖTEBORG   (INCOTERMS 2010)'     TO              
105500                     RAD4-DELIVERY-TERMS                                  
105600             END-EVALUATE                                                 
105700                                                                          
105800           ELSE                                                           
105900             EVALUATE TRUE                                                
106000             WHEN DIST76-ROMANIA                                          
106100               MOVE 'CIP CONSIGNEE                  '     TO              
106200                     RAD4-DELIVERY-TERMS                                  
106300             WHEN DIST76-RYSSLAND                                         
106400               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
106500                     RAD4-DELIVERY-TERMS                                  
106600             WHEN DIST76-RYSSLAND-2606                                    
106700               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
106800                     RAD4-DELIVERY-TERMS                                  
106900             WHEN DIST76-VITRYSSLAND                                      
107000               MOVE 'CIP DNEPROPETROVSK             '     TO              
107100                     RAD4-DELIVERY-TERMS                                  
107200             WHEN DIST76-BELARUS                                          
107300               MOVE 'CIP                            '     TO              
107400                     RAD4-DELIVERY-TERMS                                  
107500             WHEN DIST76-MOLDAVIA                                         
107600               MOVE 'DAP CHISINAU                   '     TO              
107700                     RAD4-DELIVERY-TERMS                                  
107800             WHEN DIST76-OMAN                                             
107900               MOVE 'DDP                            '     TO              
108000                     RAD4-DELIVERY-TERMS                                  
108100             WHEN DIST76-SLOVENIA                                         
108200               MOVE 'CIP DEALER                     '     TO              
108300                     RAD4-DELIVERY-TERMS                                  
108400             WHEN DIST76-BOSNIA                                           
108500               MOVE 'CIP LJUBLJANA                  '     TO              
108600                     RAD4-DELIVERY-TERMS                                  
108700             WHEN DIST76-MACEDONIA                                        
108800               MOVE 'CIP LJUBLJANA                  '     TO              
108900                     RAD4-DELIVERY-TERMS                                  
109000             WHEN DIST76-SERBIA                                           
109100               MOVE 'CIP LJUBLJANA                  '     TO              
109200                     RAD4-DELIVERY-TERMS                                  
109300             WHEN DIST76-GEORGIA                                          
109400               MOVE 'CIP TBILISI                    '     TO              
109500                     RAD4-DELIVERY-TERMS                                  
109600             WHEN DIST76-TUNISIA                                          
109700               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
109800                     RAD4-DELIVERY-TERMS                                  
109900             WHEN OTHER                                                   
110000               MOVE 'FCA                            '     TO              
110100                     RAD4-DELIVERY-TERMS                                  
110200             END-EVALUATE                                                 
110300           END-IF                                                         
110400         ELSE                                                             
110500           IF TILL-PRFOERS = ZERO                                         
110600             EVALUATE TRUE                                                
110700             WHEN DIST76-ROMANIA                                          
110800               MOVE 'CIP CONSIGNEE                  '     TO              
110900                     RAD4-DELIVERY-TERMS                                  
111000             WHEN DIST76-RYSSLAND                                         
111100               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
111200                     RAD4-DELIVERY-TERMS                                  
111300             WHEN DIST76-RYSSLAND-2606                                    
111400               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
111500                     RAD4-DELIVERY-TERMS                                  
111600             WHEN DIST76-VITRYSSLAND                                      
111700               MOVE 'CIP DNEPROPETROVSK             '     TO              
111800                     RAD4-DELIVERY-TERMS                                  
111900             WHEN DIST76-PERU                                             
112000               MOVE 'CPT                            '     TO              
112100                     RAD4-DELIVERY-TERMS                                  
112200             WHEN DIST76-BELARUS                                          
112300               MOVE 'CIP                            '     TO              
112400                     RAD4-DELIVERY-TERMS                                  
112500             WHEN DIST76-MOLDAVIA                                         
112600               MOVE 'DAP CHISINAU                   '     TO              
112700                     RAD4-DELIVERY-TERMS                                  
112800             WHEN DIST76-OMAN                                             
112900               MOVE 'DDP                            '     TO              
113000                     RAD4-DELIVERY-TERMS                                  
113100             WHEN DIST76-COLUMBIA                                         
113200               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
113300                     RAD4-DELIVERY-TERMS                                  
113400             WHEN DIST76-CANADA-REF                                       
113500               MOVE 'CIP                            '     TO              
113600                     RAD4-DELIVERY-TERMS                                  
113700             WHEN DIST76-SLOVENIA                                         
113800               MOVE 'CIP DEALER                     '     TO              
113900                     RAD4-DELIVERY-TERMS                                  
114000             WHEN DIST76-BOSNIA                                           
114100               MOVE 'CIP LJUBLJANA                  '     TO              
114200                     RAD4-DELIVERY-TERMS                                  
114300             WHEN DIST76-MACEDONIA                                        
114400               MOVE 'CIP LJUBLJANA                  '     TO              
114500                     RAD4-DELIVERY-TERMS                                  
114600             WHEN DIST76-SERBIA                                           
114700               MOVE 'CIP LJUBLJANA                  '     TO              
114800                     RAD4-DELIVERY-TERMS                                  
114900             WHEN DIST76-GEORGIA                                          
115000               MOVE 'CIP TBILISI                    '     TO              
115100                     RAD4-DELIVERY-TERMS                                  
115200             WHEN DIST76-TUNISIA                                          
115300               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
115400                     RAD4-DELIVERY-TERMS                                  
115500             WHEN OTHER                                                   
115600               MOVE 'CPT            (INCOTERMS 2010)'     TO              
115700                       RAD4-DELIVERY-TERMS                                
115800             END-EVALUATE                                                 
115900           ELSE                                                           
116000             EVALUATE TRUE                                                
116100             WHEN DIST76-ROMANIA                                          
116200               MOVE 'CIP CONSIGNEE                  '     TO              
116300                     RAD4-DELIVERY-TERMS                                  
116400             WHEN DIST76-RYSSLAND                                         
116500               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
116600                     RAD4-DELIVERY-TERMS                                  
116700             WHEN DIST76-RYSSLAND-2606                                    
116800               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
116900                     RAD4-DELIVERY-TERMS                                  
117000             WHEN DIST76-VITRYSSLAND                                      
117100               MOVE 'CIP DNEPROPETROVSK             '     TO              
117200                     RAD4-DELIVERY-TERMS                                  
117300             WHEN DIST76-PERU                                             
117400               MOVE 'CIP                            '     TO              
117500                     RAD4-DELIVERY-TERMS                                  
117600             WHEN DIST76-BELARUS                                          
117700               MOVE 'CIP                            '     TO              
117800                     RAD4-DELIVERY-TERMS                                  
117900             WHEN DIST76-MOLDAVIA                                         
118000               MOVE 'DAP CHISINAU                   '     TO              
118100                     RAD4-DELIVERY-TERMS                                  
118200             WHEN DIST76-OMAN                                             
118300               MOVE 'DDP                            '      TO             
118400                     RAD4-DELIVERY-TERMS                                  
118500             WHEN DIST76-COLUMBIA                                         
118600               MOVE 'CPT BOGOTA - COLOMBIA          '      TO             
118700                     RAD4-DELIVERY-TERMS                                  
118800             WHEN DIST76-SLOVENIA                                         
118900               MOVE 'CIP DEALER                     '     TO              
119000                     RAD4-DELIVERY-TERMS                                  
119100             WHEN DIST76-BOSNIA                                           
119200               MOVE 'CIP LJUBLJANA                  '     TO              
119300                     RAD4-DELIVERY-TERMS                                  
119400             WHEN DIST76-MACEDONIA                                        
119500               MOVE 'CIP LJUBLJANA                  '     TO              
119600                     RAD4-DELIVERY-TERMS                                  
119700             WHEN DIST76-SERBIA                                           
119800               MOVE 'CIP LJUBLJANA                  '     TO              
119900                     RAD4-DELIVERY-TERMS                                  
120000             WHEN DIST76-GEORGIA                                          
120100               MOVE 'CIP TBILISI                    '     TO              
120200                     RAD4-DELIVERY-TERMS                                  
120300             WHEN DIST76-TUNISIA                                          
120400               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
120500                     RAD4-DELIVERY-TERMS                                  
120600             WHEN OTHER                                                   
120700               MOVE 'CIP            (INCOTERMS 2010)'      TO             
120800                     RAD4-DELIVERY-TERMS                                  
120900             END-EVALUATE                                                 
121000           END-IF                                                         
121100         END-IF                                                           
121200       END-IF                                                             
121300     END-IF                                                               
121400                                                                          
121500     IF CDC-SE OR DDC-SE                                                  
121600                                                                          
121700*OBS --> FOR THE ENGLISH VERSION CHECK W4063700 AS THE CODE BELOW         
121800*OBS     MUST BE THE SAME AS IN W4063700!                                 
121900                                                                          
122000* FÖR BÅT TRANSPORTER DISTR. 7050-BRASILIEN, SKALL MAN HA EN ANNAN        
122100* LEV.VILLKORSTEXT, ENLIGT BJÖRN JENSEN, 29/5 '12.                        
122201* ANNAN TEXT GÄLLER ÄVEN FLYGTRANSPORTER, 01/11 '25 ENLIGT LINN A.        
122301                                                                          
122400       IF DIST76-BRASIL                                                   
122501         IF SHIP-IDTRPTNR < +100                                          
122600           MOVE 'CIP                            '          TO             
122701                       RAD4-DELIVERY-TERMS                                
122800         END-IF                                                           
122900                                                                          
123000         IF SKOLLI-KDFRAKT = BOAT-TRP                                     
123100           MOVE 'FOB GÖTEBORG  (INCOTERMS 2010) '          TO             
123200                       RAD4-DELIVERY-TERMS                                
123300         END-IF                                                           
123400       END-IF                                                             
123500                                                                          
123600* FÖR BÅT TRANSPORTER DISTR. COLUMBIA,DIST 7481 7482,                     
123700* SKALL MAN HA EN ANNAN LEV.VILLKORSTEXT,                                 
123800* ENLIGT BJÖRN JENSEN, 20/8 '15.                                          
123900                                                                          
124000       IF DIST76-COLUMBIA                                                 
124100         IF SHIP-IDTRPTNR > +99                                           
124200           MOVE 'CPT BUENAVENTURA - COLUMBIA '             TO             
124300                       RAD4-DELIVERY-TERMS                                
124400         END-IF                                                           
124500       END-IF                                                             
124600                                                                          
124700* FÖR FLYG TRANSPORTER DISTR. 4850-SAUDIARABIEN SKALL MAN HA EN           
124800* ANNAN LEV.VILLKORSTEXT, ENLIGT BJÖRN JENSEN, 26/11 '12.                 
124900       IF DIST76-SAUDI                                                    
125000         IF SHIP-IDTRPTNR < +100                                          
125100           MOVE 'FCA GLA GOTHENBURG (INCOTERMS 2010)'      TO             
125200                       RAD4-DELIVERY-TERMS                                
125300         END-IF                                                           
125400       END-IF                                                             
125500                                                                          
125600* SPECIELL TEXT PÅ FAKTURN FÖR OMAN, DIST 6233                            
125700* GÄLLER BARA FÖR BÅTTRANSPORTER                                          
125800* 09/1 '20 ENLIGT BJÖRN JENSEN.                                           
125900                                                                          
126000       IF DIST76-OMAN                                                     
126100         IF SHIP-IDTRPTNR > +99                                           
126200           MOVE 'CPT SOHAR               '                 TO             
126300                       RAD4-DELIVERY-TERMS                                
126400         END-IF                                                           
126500       END-IF                                                             
126600                                                                          
126700* SPECIELL TEXT PÅ FAKTURN FÖR UKRAINA DIST 2615                          
126800* GÄLLER BARA FÖR BIL O BÅTTRANSPORTER                                    
126900* 17/3 '20 ENLIGT BJÖRN JENSEN.                                           
127000                                                                          
127100       IF DIST76-UKRAINE                                                  
127200         IF SHIP-IDTRPTNR > +99                                           
127300           MOVE 'CIP COLOGNE             '                 TO             
127400                       RAD4-DELIVERY-TERMS                                
127500         END-IF                                                           
127600       END-IF                                                             
127700                                                                          
128800                                                                          
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200 CE-EQUAL-TO SECTION.                                                     
129300                                                                          
129400*  KINA REFILL SKA HA TRP-DOK I LOKAL VALUTA                              
129500                                                                          
129600     IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN) OR                      
129700       DIST35-NDCUS-NDCCN-REFILL                                          
129800       MOVE 'CNY'     TO CURR-KDVALISO-ROW                                
129900                         WS-KDVALISO-EXCH                                 
130000                                                                          
130100*      HÄMTA VALUTA FRÅN VALUTAREGISTRET                                  
130200       MOVE TODAYS-DATE-YEAR      TO W-DATE-AAMM(1:2)                     
130300       MOVE TODAYS-DATE-MONTH     TO W-DATE-AAMM(3:2)                     
130400       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
130500       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
130600       MOVE 'M'                   TO CURR-KDVALTYP                        
130700                                                                          
130800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
130900       IF CURR-KDSVAR = ' '                                               
131000          CONTINUE                                                        
131100       ELSE                                                               
131200          MOVE 1                  TO CURR-PRKURS-NEW                      
131300       END-IF                                                             
131400       COMPUTE WS-PRKURS     ROUNDED = 1 / CURR-PRKURS-NEW                
131500       COMPUTE WS-TOTAL-VALUE-PRKURS ROUNDED =                            
131600               WS-TOTAL-VALUE * WS-PRKURS                                 
131700     ELSE                                                                 
131800       IF ((CDC-SE OR DDC-SE) AND                                         
131810          (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                  
131900                                                                          
132000         MOVE 'ZAR'   TO CURR-KDVALISO-ROW                                
132100                           WS-KDVALISO-EXCH                               
132200                                                                          
132300*        HÄMTA VALUTA FRÅN VALUTAREGISTRET                                
132400         MOVE TODAYS-DATE-YEAR    TO W-DATE-AAMM(1:2)                     
132500         MOVE TODAYS-DATE-MONTH   TO W-DATE-AAMM(3:2)                     
132600         MOVE W-DATE-AAMM         TO CURR-TIAAMM                          
132700         MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV                    
132800         MOVE 'M'                 TO CURR-KDVALTYP                        
132900                                                                          
133000         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
133100         IF CURR-KDSVAR = ' '                                             
133200            CONTINUE                                                      
133300         ELSE                                                             
133400            MOVE 1                TO CURR-PRKURS-NEW                      
133500         END-IF                                                           
133600         COMPUTE WS-PRKURS   ROUNDED = 1 / CURR-PRKURS-NEW                
133700         COMPUTE WS-TOTAL-VALUE-PRKURS ROUNDED =                          
133800                 WS-TOTAL-VALUE * WS-PRKURS                               
133900       ELSE                                                               
134000                                                                          
134100         IF DIST79-DEALER-PRICE OR                                        
134200            DIST79-ECOM-PRICE                                             
134300           MOVE SGMT-PRKURS TO WS-PRKURS                                  
134400           MOVE SGMT-KDVALISO TO WS-KDVALISO-2                            
134500           COMPUTE WS-TOTAL-VALUE-PRKURS ROUNDED =                        
134600                   WS-TOTAL-VALUE * WS-PRKURS                             
134700         ELSE                                                             
134800           MOVE 1                TO WS-PRKURS                             
134900           MOVE 'SEK'            TO WS-KDVALISO-2                         
135000           MOVE WS-TOTAL-VALUE   TO WS-TOTAL-VALUE-PRKURS                 
135100         END-IF                                                           
135200       END-IF                                                             
135300     END-IF                                                               
135400     .                                                                    
135500     EJECT                                                                
135600 D-COMPUTE-TOTALS SECTION.                                                
135700                                                                          
135800     COMPUTE WS-VKORDNTO-TOTAL = SPARE-VKVIKT    +                        
135900                                 CHARIOT-VKVIKT  +                        
136000                                 ENGINE-B-VKVIKT +                        
136100                                 ENGINE-D-VKVIKT +                        
136200                                 TOOLS-VKVIKT    +                        
136300                                 PAINT-VKVIKT                             
136400                                                                          
136500     IF DIST79-DEALER-PRICE OR                                            
136600        DIST79-ECOM-PRICE   OR                                            
136700       ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN)                          
136800                            OR                                            
136900       ((CDC-SE OR DDC-SE) AND                                            
136910       (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                     
137000                                                                          
137100       COMPUTE SPARE-PRARTNTO ROUNDED    = SPARE-PRARTNTO    *            
137200                                           WS-PRKURS                      
137300       COMPUTE CHARIOT-PRARTNTO ROUNDED  = CHARIOT-PRARTNTO  *            
137400                                           WS-PRKURS                      
137500       COMPUTE ENGINE-B-PRARTNTO ROUNDED = ENGINE-B-PRARTNTO *            
137600                                           WS-PRKURS                      
137700       COMPUTE ENGINE-D-PRARTNTO ROUNDED = ENGINE-D-PRARTNTO *            
137800                                           WS-PRKURS                      
137900       COMPUTE TOOLS-PRARTNTO ROUNDED    = TOOLS-PRARTNTO    *            
138000                                           WS-PRKURS                      
138100       COMPUTE PAINT-PRARTNTO ROUNDED    = PAINT-PRARTNTO    *            
138200                                           WS-PRKURS                      
138300     END-IF                                                               
138400                                                                          
138500*BILL-IT, LOCAL CURRENCY OCH DNI OCH DDI SKALL SUMMERAS.                  
138600     COMPUTE WS-SUORDV-TOTAL = SPARE-PRARTNTO    +                        
138700                               CHARIOT-PRARTNTO  +                        
138800                               ENGINE-B-PRARTNTO +                        
138900                               ENGINE-D-PRARTNTO +                        
139000                               TOOLS-PRARTNTO    +                        
139100                               PAINT-PRARTNTO                             
139200     .                                                                    
139300     EJECT                                                                
139400 E-PRINT-DOCUMENT SECTION.                                                
139500                                                                          
139600     PERFORM S02-PRINT-META                                               
139700                                                                          
139800     PERFORM EA-PRINT-HEAD                                                
139900                                                                          
140000     PERFORM EB-PRINT-ROW1                                                
140100                                                                          
140200     PERFORM EC-PRINT-ROW2                                                
140300                                                                          
140400     PERFORM ED-PRINT-ROW3                                                
140500                                                                          
140600     PERFORM EE-PRINT-ROW4                                                
140700                                                                          
140800     PERFORM EF-PRINT-ROW5                                                
140900                                                                          
141000     PERFORM EG-PRINT-ROW6                                                
141100                                                                          
141200     PERFORM EH-PRINT-ROW7                                                
141300                                                                          
141400     PERFORM EI-PRINT-ROW8                                                
141500                                                                          
141600     PERFORM EJ-PRINT-ROW9                                                
141700                                                                          
141800     PERFORM EK-PRINT-ROW10                                               
141900                                                                          
142000     PERFORM EL-PRINT-ROW11                                               
142100                                                                          
142200     PERFORM EM-PRINT-ROW12                                               
142300                                                                          
142400     PERFORM EN-PRINT-ROW13                                               
142500                                                                          
142600     PERFORM EO-PRINT-ROW14                                               
142700                                                                          
142800     PERFORM EP-PRINT-ROW15                                               
142900     .                                                                    
143000     EJECT                                                                
143100 EA-PRINT-HEAD SECTION.                                                   
143200                                                                          
143300     IF WS-IX = ZERO                                                      
143400       PERFORM S30-IMPORTER                                               
143500     END-IF                                                               
143600     ADD 1                           TO WS-PAGE-NO                        
143700                                                                          
143800     MOVE SPACE                      TO SEND-RAD                          
143900     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
144000     PERFORM S90-PUT-DOC-LINE                                             
144100     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
144200     PERFORM S90-PUT-DOC-LINE                                             
144300     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
144400     PERFORM S90-PUT-DOC-LINE                                             
144500                                                                          
144600     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
144700     MOVE WS-TYP-IDSHIP              TO RAD1-TYP-IDSHIP                   
144800     MOVE RAD1-HEAD                  TO ARB-RAD                           
144900                                        SEND-RAD                          
145000     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
145100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
145200     MOVE 7                          TO W-LINE-COUNT                      
145300     PERFORM S01-PRINT-LINE                                               
145400                                                                          
145500     MOVE SPACE                      TO SEND-RAD                          
145600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
145700                                                                          
145800     MOVE RAD2X-HEAD                 TO ARB-RAD                           
145900                                        SEND-RAD                          
146000     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
146100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
146200     ADD 1                           TO W-LINE-COUNT                      
146300     PERFORM S01-PRINT-LINE                                               
146400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
146500                                                                          
146600     MOVE RAD3-HEAD                  TO ARB-RAD                           
146700                                        SEND-RAD                          
146800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
146900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
147000     ADD 1                           TO W-LINE-COUNT                      
147100     PERFORM S01-PRINT-LINE                                               
147200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
147300                                                                          
147400     MOVE SPACES                     TO RAD2-HEAD                         
147500     MOVE BET-ADBETRAD-2             TO RAD4H-IMPORTER                    
147600     MOVE SHIP-TISKEPPN              TO WS-YYMMDD                         
147700     MOVE WS-YYMMDD                  TO RAD2H-TIYYMMDD                    
147800     MOVE SGMT-IDDISTR               TO RAD2H-IDDISTR                     
147900     MOVE SHIP-IDSHIPM               TO RAD2H-IDSHIPM                     
148000     MOVE SHIP-IDTRPTNR              TO RAD2H-IDTRPTNR                    
148100     MOVE SHIP-IDLBBET               TO RAD2H-IDLBBET                     
148200     MOVE WS-PAGE-NO                 TO RAD2H-PAGE-NO                     
148300     MOVE RAD2-HEAD                  TO ARB-RAD                           
148400                                        SEND-RAD                          
148500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
148600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
148700     ADD 1                           TO W-LINE-COUNT                      
148800     PERFORM S01-PRINT-LINE                                               
148900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
149000                                                                          
149100     MOVE RAD5-HEAD                  TO ARB-RAD                           
149200                                        SEND-RAD                          
149300     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
149400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
149500     ADD 1                           TO W-LINE-COUNT                      
149600     PERFORM S01-PRINT-LINE                                               
149700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
149800     .                                                                    
149900     EJECT                                                                
150000 EB-PRINT-ROW1 SECTION.                                                   
150100                                                                          
150200     MOVE BEFRAKT-LEDTEXT (W-KDSPRAK) TO RAD1-FREIGHT-MODE-TEXT           
150300     IF WS-IDLC > SPACE                                                   
150400       MOVE IDLC-LEDTEXT (W-KDSPRAK)  TO RAD1-IDLC-TEXT                   
150500     ELSE                                                                 
150600       MOVE SPACE                     TO RAD1-IDLC-TEXT                   
150700     END-IF                                                               
150800                                                                          
150900     IF WS-BESLULEV > SPACE                                               
151000       MOVE 'VGM (KG)      '          TO RAD1-BESLULEV-TEXT               
151100     ELSE                                                                 
151200       MOVE SPACE                     TO RAD1-BESLULEV-TEXT               
151300     END-IF                                                               
151400     MOVE RAD1                        TO ARB-RAD                          
151500                                         SEND-RAD                         
151600     MOVE PRT-AFTER-2                 TO PRT-RADSKIP                      
151700     MOVE WS-SKIP2                    TO STYRTECKEN-RAD                   
151800     ADD 2                            TO W-LINE-COUNT                     
151900     PERFORM S01-PRINT-LINE                                               
152000     .                                                                    
152100     EJECT                                                                
152200 EC-PRINT-ROW2 SECTION.                                                   
152300                                                                          
152400     MOVE WS-KDFRAKT                 TO RAD2-IDTRANSP-NAMN                
152500     IF WS-IDLC > SPACE                                                   
152600       MOVE WS-IDLC                  TO RAD2-IDLC                         
152700     ELSE                                                                 
152800       MOVE SPACE                    TO RAD2-IDLC                         
152900     END-IF                                                               
153000                                                                          
153100     IF WS-BESLULEV > SPACE                                               
153200       MOVE WS-BESLULEV              TO RAD2-BESLULEV                     
153300     ELSE                                                                 
153400       MOVE SPACE                    TO RAD2-BESLULEV                     
153500     END-IF                                                               
153600     MOVE RAD2                       TO ARB-RAD                           
153700                                        SEND-RAD                          
153800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
153900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
154000     ADD 1                           TO W-LINE-COUNT                      
154100     PERFORM S01-PRINT-LINE                                               
154200     .                                                                    
154300     EJECT                                                                
154400 ED-PRINT-ROW3 SECTION.                                                   
154500                                                                          
154600     MOVE BELEVVIL-LEDTEXT (W-KDSPRAK)                                    
154700                                     TO RAD3-DELIVERY-TERMS-TEXT          
154800     MOVE RAD3                       TO ARB-RAD                           
154900                                        SEND-RAD                          
155000     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
155100     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
155200     ADD 2                           TO W-LINE-COUNT                      
155300     PERFORM S01-PRINT-LINE                                               
155400     .                                                                    
155500     EJECT                                                                
155600 EE-PRINT-ROW4 SECTION.                                                   
155700                                                                          
155800     MOVE RAD4                       TO ARB-RAD                           
155900                                        SEND-RAD                          
156000     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
156100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
156200     ADD 1                           TO W-LINE-COUNT                      
156300     PERFORM S01-PRINT-LINE                                               
156400     .                                                                    
156500     EJECT                                                                
156600 EF-PRINT-ROW5 SECTION.                                                   
156700                                                                          
156800     IF WS-IDBOKN  > SPACE OR WS-IDLICENS > SPACE OR                      
156900        WS-IDVCERT > SPACE                                                
157000       MOVE IDBOKN-LEDTEXT (W-KDSPRAK)   TO RAD5-IDBOKN-TEXT              
157100       MOVE IDLICENS-LEDTEXT (W-KDSPRAK) TO RAD5-IDLICENS-TEXT            
157200       MOVE 'GOODS CERTIFICATE NO'       TO RAD5-IDVCERT-TEXT             
157300       MOVE RAD5                         TO ARB-RAD                       
157400                                            SEND-RAD                      
157500       MOVE PRT-AFTER-2                  TO PRT-RADSKIP                   
157600       MOVE WS-SKIP2                     TO STYRTECKEN-RAD                
157700       ADD 2                             TO W-LINE-COUNT                  
157800       PERFORM S01-PRINT-LINE                                             
157900     END-IF                                                               
158000     .                                                                    
158100     EJECT                                                                
158200 EG-PRINT-ROW6 SECTION.                                                   
158300                                                                          
158400     IF WS-IDBOKN  > SPACE OR WS-IDLICENS > SPACE OR                      
158500        WS-IDVCERT > SPACE                                                
158600       MOVE WS-IDBOKN                    TO RAD6-IDBOKN-VALUE             
158700       MOVE WS-IDLICENS                  TO RAD6-IDLICENS-VALUE           
158800       MOVE WS-IDVCERT                   TO RAD6-IDVCERT-VALUE            
158900       MOVE RAD6                         TO ARB-RAD                       
159000                                            SEND-RAD                      
159100       MOVE PRT-AFTER-1                  TO PRT-RADSKIP                   
159200       MOVE WS-SKIP1                     TO STYRTECKEN-RAD                
159300       ADD 1                             TO W-LINE-COUNT                  
159400       PERFORM S01-PRINT-LINE                                             
159500     END-IF                                                               
159600     .                                                                    
159700     EJECT                                                                
159800 EH-PRINT-ROW7 SECTION.                                                   
159900                                                                          
160000     MOVE 'INVOICED VALUE'           TO RAD7-TOTAL-VALUE-TEXT             
160100     MOVE WS-TOTAL-VALUE-PRKURS      TO RAD7-TOTAL-VALUE                  
160200                                                                          
160300     IF ((CDC-SE OR DDC-SE) AND DIST35-REFILL-CN) OR                      
160400       DIST35-NDCUS-NDCCN-REFILL                                          
160500       MOVE WS-KDVALISO-EXCH        TO RAD7-KDVALISO                      
160600     ELSE                                                                 
160700       IF ((CDC-SE OR DDC-SE) AND                                         
160710           (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                 
160800                                                                          
160900         MOVE WS-KDVALISO-EXCH      TO RAD7-KDVALISO                      
161000       ELSE                                                               
161100         IF DIST79-DEALER-PRICE OR                                        
161200            DIST79-ECOM-PRICE                                             
161300           MOVE 'SEK'                TO RAD7-KDVALISO                     
161400         ELSE                                                             
161500            MOVE WS-KDVALISO-2       TO RAD7-KDVALISO                     
161600         END-IF                                                           
161700       END-IF                                                             
161800     END-IF                                                               
161900     MOVE RAD7                       TO ARB-RAD                           
162000                                        SEND-RAD                          
162100     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
162200     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
162300     ADD 2                           TO W-LINE-COUNT                      
162400     PERFORM S01-PRINT-LINE                                               
162500     .                                                                    
162600     EJECT                                                                
162700 EI-PRINT-ROW8 SECTION.                                                   
162800                                                                          
162900     IF WS-BEROUTE > SPACE                                                
163000       MOVE 'SEAL NO'                TO RAD8-BEROUTE-TEXT                 
163100       MOVE RAD8                     TO ARB-RAD                           
163200                                        SEND-RAD                          
163300       MOVE PRT-AFTER-2              TO PRT-RADSKIP                       
163400       MOVE WS-SKIP2                 TO STYRTECKEN-RAD                    
163500       ADD 2                         TO W-LINE-COUNT                      
163600       PERFORM S01-PRINT-LINE                                             
163700     END-IF                                                               
163800     .                                                                    
163900     EJECT                                                                
164000 EJ-PRINT-ROW9 SECTION.                                                   
164100                                                                          
164200     IF WS-BEROUTE > SPACE                                                
164300       MOVE WS-BEROUTE               TO RAD9-BEROUTE-VALUE                
164400       MOVE RAD9                     TO ARB-RAD                           
164500                                        SEND-RAD                          
164600       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
164700       MOVE WS-SKIP1                 TO STYRTECKEN-RAD                    
164800       ADD 1                         TO W-LINE-COUNT                      
164900       PERFORM S01-PRINT-LINE                                             
165000     END-IF                                                               
165100     .                                                                    
165200     EJECT                                                                
165300 EK-PRINT-ROW10 SECTION.                                                  
165400                                                                          
165500     MOVE IDSKEPPN-LEDTEXT (W-KDSPRAK) TO RAD10-SHIPPING-NO-TEXT          
165600     MOVE 'SHIPPING DATE  '          TO RAD10-IDSKEPPN-TEXT               
165700     MOVE RAD10                      TO ARB-RAD                           
165800                                        SEND-RAD                          
165900     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
166000     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
166100     ADD 2                           TO W-LINE-COUNT                      
166200     PERFORM S01-PRINT-LINE                                               
166300     .                                                                    
166400     EJECT                                                                
166500 EL-PRINT-ROW11 SECTION.                                                  
166600                                                                          
166700     MOVE WS-IDSHIPM                 TO RAD11-IDSHIPM-TEXT                
166800     MOVE WS-TISKEPPN                TO RAD11-TISKEPPN                    
166900     MOVE RAD11                      TO ARB-RAD                           
167000                                        SEND-RAD                          
167100     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
167200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
167300     ADD 1                           TO W-LINE-COUNT                      
167400     PERFORM S01-PRINT-LINE                                               
167500     .                                                                    
167600     EJECT                                                                
167700 EM-PRINT-ROW12 SECTION.                                                  
167800                                                                          
167900     MOVE KVKOLLI-LEDTEXT (W-KDSPRAK)  TO RAD12-QUANTITY-TEXT             
168000     MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD12-PACKING-TYPE-TEXT         
168100     MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD12-NET-WEIGHT-TEXT           
168200     MOVE SUORDV-LEDTEXT (W-KDSPRAK)   TO RAD12-VALUE-TEXT                
168300     MOVE RAD12                        TO ARB-RAD                         
168400                                          SEND-RAD                        
168500     MOVE PRT-AFTER-2                  TO PRT-RADSKIP                     
168600     MOVE WS-SKIP2                     TO STYRTECKEN-RAD                  
168700     ADD 2                             TO W-LINE-COUNT                    
168800     PERFORM S01-PRINT-LINE                                               
168900     .                                                                    
169000     EJECT                                                                
169100 EN-PRINT-ROW13 SECTION.                                                  
169200                                                                          
169300     MOVE SPACE                      TO ARB-RAD                           
169400                                        SEND-RAD                          
169500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
169600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
169700     ADD 1                           TO W-LINE-COUNT                      
169800     PERFORM S01-PRINT-LINE                                               
169900                                                                          
170000     MOVE NOO  TO EMBTYP-FOUND-SW                                         
170100     MOVE +1   TO INDX3                                                   
170200     MOVE +1   TO INDX4                                                   
170300     PERFORM UNTIL INDX4 > KDEMBTYP-MAX AND INDX5 > KDEMBTYP-MAX          
170400       IF W-LINE-COUNT > W-LINE-MAX - 1                                   
170500         PERFORM EA-PRINT-HEAD                                            
170600         PERFORM EM-PRINT-ROW12                                           
170700       END-IF                                                             
170800                                                                          
170900       PERFORM UNTIL EMBTYP-FOUND OR INDX3 > KDEMBTYP-MAX                 
171000         IF WS-KDEMBTYP (INDX3) > ZERO                                    
171100           MOVE WS-QUANTITY (INDX3)  TO RAD13-QUANTITY-VALUE              
171200           PERFORM ENA-GET-EMB-TEXT                                       
171300           ADD +1                    TO INDX3                             
171400           MOVE YES                  TO EMBTYP-FOUND-SW                   
171500         ELSE                                                             
171600           ADD +1                    TO INDX3                             
171700         END-IF                                                           
171800       END-PERFORM                                                        
171900       MOVE NOO                      TO EMBTYP-FOUND-SW                   
172000                                                                          
172100       IF INDX3 > KDEMBTYP-MAX                                            
172200         MOVE ZERO                   TO RAD13-QUANTITY-VALUE              
172300         MOVE SPACE                  TO RAD13-PACKING-TYPE                
172400         MOVE KDEMBTYP-MAX           TO INDX5                             
172500       END-IF                                                             
172600                                                                          
172700       IF INDX4 = 1                                                       
172800         MOVE 'SPARE PARTS'          TO RAD13-KVKOLLI-TEXT                
172900         IF SPARE-VKVIKT > ZERO                                           
173000           MOVE SPARE-VKVIKT         TO RAD13-NET-WEIGHT-VALUE            
173100         ELSE                                                             
173200           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
173300         END-IF                                                           
173400         IF SPARE-PRARTNTO > ZERO                                         
173500           MOVE SPARE-PRARTNTO       TO RAD13-VALUE-VALUE                 
173600         ELSE                                                             
173700           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
173800         END-IF                                                           
173900       END-IF                                                             
174000                                                                          
174100       IF INDX4 = 2                                                       
174200         MOVE 'BODIES'               TO RAD13-KVKOLLI-TEXT                
174300         IF CHARIOT-VKVIKT > ZERO                                         
174400           MOVE CHARIOT-VKVIKT       TO RAD13-NET-WEIGHT-VALUE            
174500         ELSE                                                             
174600           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
174700         END-IF                                                           
174800         IF CHARIOT-PRARTNTO > ZERO                                       
174900           MOVE CHARIOT-PRARTNTO     TO RAD13-VALUE-VALUE                 
175000         ELSE                                                             
175100           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
175200         END-IF                                                           
175300       END-IF                                                             
175400                                                                          
175500       IF INDX4 = 3                                                       
175600         MOVE 'ENGINE B'             TO RAD13-KVKOLLI-TEXT                
175700         IF ENGINE-B-VKVIKT > ZERO                                        
175800           MOVE ENGINE-B-VKVIKT      TO RAD13-NET-WEIGHT-VALUE            
175900         ELSE                                                             
176000           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
176100         END-IF                                                           
176200         IF ENGINE-B-PRARTNTO > ZERO                                      
176300           MOVE ENGINE-B-PRARTNTO    TO RAD13-VALUE-VALUE                 
176400         ELSE                                                             
176500           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
176600         END-IF                                                           
176700       END-IF                                                             
176800                                                                          
176900       IF INDX4 = 4                                                       
177000         MOVE 'ENGINE D'             TO RAD13-KVKOLLI-TEXT                
177100         IF ENGINE-D-VKVIKT > ZERO                                        
177200           MOVE ENGINE-D-VKVIKT      TO RAD13-NET-WEIGHT-VALUE            
177300         ELSE                                                             
177400           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
177500         END-IF                                                           
177600         IF ENGINE-D-PRARTNTO > ZERO                                      
177700           MOVE ENGINE-D-PRARTNTO    TO RAD13-VALUE-VALUE                 
177800         ELSE                                                             
177900           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
178000         END-IF                                                           
178100       END-IF                                                             
178200                                                                          
178300       IF INDX4 = 5                                                       
178400         MOVE 'TOOLS'                TO RAD13-KVKOLLI-TEXT                
178500         IF TOOLS-VKVIKT > ZERO                                           
178600           MOVE TOOLS-VKVIKT         TO RAD13-NET-WEIGHT-VALUE            
178700         ELSE                                                             
178800           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
178900         END-IF                                                           
179000         IF TOOLS-PRARTNTO > ZERO                                         
179100           MOVE TOOLS-PRARTNTO       TO RAD13-VALUE-VALUE                 
179200         ELSE                                                             
179300           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
179400         END-IF                                                           
179500       END-IF                                                             
179600                                                                          
179700       IF INDX4 = 6                                                       
179800         MOVE 'PAINT'                TO RAD13-KVKOLLI-TEXT                
179900         IF PAINT-VKVIKT > ZERO                                           
180000           MOVE PAINT-VKVIKT         TO RAD13-NET-WEIGHT-VALUE            
180100         ELSE                                                             
180200           MOVE ZERO                 TO RAD13-NET-WEIGHT-VALUE            
180300         END-IF                                                           
180400         IF PAINT-PRARTNTO > ZERO                                         
180500           MOVE PAINT-PRARTNTO       TO RAD13-VALUE-VALUE                 
180600         ELSE                                                             
180700           MOVE ZERO                 TO RAD13-VALUE-VALUE                 
180800         END-IF                                                           
180900       END-IF                                                             
181000                                                                          
181100       IF INDX4 > 6                                                       
181200         MOVE YES                    TO KOLLI-SW                          
181300         MOVE KDEMBTYP-MAX           TO INDX4                             
181400       END-IF                                                             
181500                                                                          
181600       IF KOLLI-END                                                       
181700         MOVE SPACE                  TO RAD13-KVKOLLI-TEXT                
181800         MOVE ZERO                   TO RAD13-NET-WEIGHT-VALUE            
181900         MOVE ZERO                   TO RAD13-VALUE-VALUE                 
182000       END-IF                                                             
182100                                                                          
182200       IF INDX4 = KDEMBTYP-MAX AND INDX5 = KDEMBTYP-MAX                   
182300         MOVE SPACE                  TO RAD13                             
182400       END-IF                                                             
182500                                                                          
182600       MOVE RAD13                    TO ARB-RAD                           
182700                                        SEND-RAD                          
182800       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
182900       MOVE WS-SKIP1                 TO STYRTECKEN-RAD                    
183000       ADD 1                         TO W-LINE-COUNT                      
183100       PERFORM S01-PRINT-LINE                                             
183200       ADD +1 TO INDX4                                                    
183300       ADD +1 TO INDX5                                                    
183400     END-PERFORM                                                          
183500     .                                                                    
183600     EJECT                                                                
183700 ENA-GET-EMB-TEXT SECTION.                                                
183800                                                                          
183900     MOVE '4738'                   TO IDHTYP                              
184000     MOVE LOW-VALUE                TO NYCKEL-VALFRI                       
184100     MOVE WS-KDEMBTYP (INDX3)      TO KDEMBTYP-4738                       
184200                                                                          
184300     PERFORM IMS-4738-GET-ROOT                                            
184400     IF SEGMENT-FOUND                                                     
184500       PERFORM IMS-4738-GET-SEGMENT                                       
184600       IF SEGMENT-FOUND                                                   
184700         MOVE EMBTYP-BEEMBTYP (2)  TO RAD13-PACKING-TYPE                  
184800       ELSE                                                               
184900         MOVE 'PARCEL'             TO RAD13-PACKING-TYPE                  
185000       END-IF                                                             
185100     END-IF                                                               
185200     .                                                                    
185300     EJECT                                                                
185400 EO-PRINT-ROW14 SECTION.                                                  
185500                                                                          
185600     IF W-LINE-COUNT > W-LINE-MAX - 1                                     
185700       PERFORM EA-PRINT-HEAD                                              
185800       PERFORM EM-PRINT-ROW12                                             
185900     END-IF                                                               
186000     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD14-GROSS-WEIGHT-TEXT         
186100     MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD14-VOLUME-TEXT               
186200     MOVE RAD14                      TO ARB-RAD                           
186300                                        SEND-RAD                          
186400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
186500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
186600     ADD 1                           TO W-LINE-COUNT                      
186700     PERFORM S01-PRINT-LINE                                               
186800     .                                                                    
186900     EJECT                                                                
187000 EP-PRINT-ROW15 SECTION.                                                  
187100                                                                          
187200     IF W-LINE-COUNT > W-LINE-MAX - 1                                     
187300       PERFORM EA-PRINT-HEAD                                              
187400       PERFORM EM-PRINT-ROW12                                             
187500     END-IF                                                               
187600     MOVE 'SUMMARY OF SPARE PARTS'   TO RAD15-BETOTAL-TEXT                
187700     MOVE WS-VKORDNTO-TOTAL          TO RAD15-VKORDNTO-TOTAL              
187800     MOVE WS-SUORDV-TOTAL            TO RAD15-SUORDV-TOTAL                
187900     MOVE WS-VKORDBTO-TOTAL          TO RAD15-VKORDBTO-TOTAL              
188000     MOVE WS-VLORDBTO-TOTAL          TO RAD15-VLORDBTO-TOTAL              
188100     MOVE RAD15                      TO ARB-RAD                           
188200                                        SEND-RAD                          
188300     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
188400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
188500     ADD 1                           TO W-LINE-COUNT                      
188600     PERFORM S01-PRINT-LINE                                               
188700     .                                                                    
188800     EJECT                                                                
188900 S01-PRINT-LINE SECTION.                                                  
189000                                                                          
189100*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
189200     PERFORM S90-PUT-DOC-LINE                                             
189300                                                                          
189400*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
189500*    -- OR ALWAYS IF WE COME FROM 4622                                    
189600*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
189700*    -- CANNOT BE SET TO J)                                               
189800     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
189900     OR TRPD-FLLDCKND = YES                                               
190000       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
190100                           PRT-WRITE                                      
190200                           W-IDPRTLST                                     
190300                           ALT-PCB                                        
190400                           PRT-RADSKIP                                    
190500                           ARB-RAD                                        
190600     END-IF                                                               
190700     .                                                                    
190800     EJECT                                                                
190900                                                                          
191000 S02-PRINT-META SECTION.                                                  
191100                                                                          
191200     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
191300     MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
191400     STRING WS-META                                                       
191500            'SHIPMENT_NUMBER='                                            
191600            WS-IDSHIPM-Z                                                  
191700            DELIMITED BY SIZE INTO SEND-RAD                               
191800                                                                          
191900     PERFORM S90-PUT-DOC-LINE                                             
192000                                                                          
192100     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
192200     STRING WS-META                                                       
192300            'DOCUMENT_TYPE='                                              
192400            WS-TYP-IDSHIP                                                 
192500            DELIMITED BY SIZE INTO SEND-RAD                               
192600                                                                          
192700     PERFORM S90-PUT-DOC-LINE                                             
192800                                                                          
192900     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
193000     MOVE SHIP-TISKEPPN      TO WS-YYMMDD                                 
193100     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
193200                                                                          
193300     STRING WS-META                                                       
193400            'SHIPPING_DATE='                                              
193500            WS-YEAR(1:2)                                                  
193600            WS-YYMMDD                                                     
193700            DELIMITED BY SIZE INTO SEND-RAD                               
193800                                                                          
193900     PERFORM S90-PUT-DOC-LINE                                             
194000                                                                          
194100     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
194200     MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
194300     STRING WS-META                                                       
194400            'DISTRICT_NUMBER='                                            
194500            WS-IDDISTR                                                    
194600            DELIMITED BY SIZE INTO SEND-RAD                               
194700                                                                          
194800     PERFORM S90-PUT-DOC-LINE                                             
194900                                                                          
195000     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
195100                                                                          
195200     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
195300     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
195400     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
195500     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
195600     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
195700     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
195800                                                                          
195900     STRING WS-META                                                       
196000            'FILE_NAME='                                                  
196100            DELIMITED BY SIZE                                             
196200            'SHIPDOC_BD'                                                  
196300            DELIMITED BY SIZE                                             
196400            '_'                                                           
196500            DELIMITED BY SIZE                                             
196600            FUNCTION TRIM (WS-IDSHIPM-Z)                                  
196700            DELIMITED BY SIZE                                             
196800            '_'                                                           
196900            FUNCTION TRIM (WS-IDDISTR)                                    
197000            DELIMITED BY SIZE                                             
197100            '_'                                                           
197200            DELIMITED BY SIZE                                             
197300            WS-TIMESTAMP                                                  
197400            DELIMITED BY SIZE INTO SEND-RAD                               
197500                                                                          
197600     PERFORM S90-PUT-DOC-LINE                                             
197700     .                                                                    
197800                                                                          
197900 S20-HAMTA-WDB2  SECTION.                                                 
198000                                                                          
198100     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
198200     PERFORM IMS-GU-WDB201                                                
198300     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
198400       MOVE +2 TO W-KDSPRAK                                               
198500     ELSE                                                                 
198600       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
198700     END-IF                                                               
198800     .                                                                    
198900     EJECT                                                                
199000 S30-IMPORTER SECTION.                                                    
199100                                                                          
199200     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
199300                            W-WDB201-IDKUNDNR                             
199400     PERFORM IMS-GU-WDB201                                                
199500     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
199600     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
199700     PERFORM IMS-GU-WDB101                                                
199800     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
199900     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
200000     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
200100     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
200200     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
200300     ADD +1 TO WS-IX                                                      
200400     .                                                                    
200500     EJECT                                                                
200600 S90-PUT-DOC-LINE SECTION.                                                
200700     IF (TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1')                 
200800                                 OR                                       
200900        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1')                 
201000                                                                          
201100       IF TRPD-FLSKRIV-ONDEM = YES                                        
201200         MOVE +1                          TO SEND-IDCOM                   
201300         MOVE 'PUT'                       TO SEND-KDFUNC                  
201400         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
201500         CALL WZ01SEND USING SEND-CONTROL-AREA                            
201600                             SEND-KVDLEN                                  
201700                             SEND-RAD-STYRTECKEN                          
201800         IF SEND-KDRC > ZERO                                              
201900           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
202000           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
202100           DELIMITED BY SIZE INTO ERRTEXT-STR                             
202200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
202300         END-IF                                                           
202400       END-IF                                                             
202500     END-IF                                                               
202600     .                                                                    
202700     EJECT                                                                
202800 IMS-GU-WDE101 SECTION.                                                   
202900                                                                          
203000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
203100          DELIMITED BY SIZE INTO SSA1                                     
203200     MOVE '  GE' TO GOOD-STATUSCODES                                      
203300     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
203400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
203500     PERFORM IMS-STATUSCHECK                                              
203600     .                                                                    
203700     EJECT                                                                
203800 IMS-GNP-WDE111 SECTION.                                                  
203900                                                                          
204000     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
204100                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
204200          DELIMITED BY SIZE INTO SSA1                                     
204300     MOVE '  GE' TO GOOD-STATUSCODES                                      
204400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
204500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
204600     PERFORM IMS-STATUSCHECK                                              
204700     .                                                                    
204800     EJECT                                                                
204900 IMS-GNP-WDE121 SECTION.                                                  
205000                                                                          
205100     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
205200          DELIMITED BY SIZE INTO SSA1                                     
205300     MOVE 'WDE121  '          TO SSA2                                     
205400     MOVE '  GE' TO GOOD-STATUSCODES                                      
205500     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
205600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
205700     PERFORM IMS-STATUSCHECK                                              
205800     .                                                                    
205900     EJECT                                                                
206000 IMS-GNP-WDE122 SECTION.                                                  
206100                                                                          
206200     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
206300          DELIMITED BY SIZE INTO SSA1                                     
206400     MOVE 'WDE122  '          TO SSA2                                     
206500     MOVE '  GE' TO GOOD-STATUSCODES                                      
206600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2              
206700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
206800     PERFORM IMS-STATUSCHECK                                              
206900     .                                                                    
207000     EJECT                                                                
207100 IMS-GNP-WDE131 SECTION.                                                  
207200                                                                          
207300     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
207400          DELIMITED BY SIZE INTO SSA1                                     
207500     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
207600          DELIMITED BY SIZE INTO SSA2                                     
207700     MOVE 'WDE131  '          TO SSA3                                     
207800     MOVE '  GE' TO GOOD-STATUSCODES                                      
207900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2 SSA3         
208000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
208100     PERFORM IMS-STATUSCHECK                                              
208200     .                                                                    
208300     EJECT                                                                
208400 IMS-4732-GET-ROOT SECTION.                                               
208500                                                                          
208600     STRING 'WDR101  (WDGXKEY  =' WDGX01 ')'                              
208700          DELIMITED BY SIZE INTO SSA1                                     
208800     MOVE '  GE' TO GOOD-STATUSCODES                                      
208900     CALL CBLTDLI USING GU 4732-PCB DLI-IO-4732 SSA1                      
209000     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
209100     PERFORM IMS-STATUSCHECK                                              
209200     .                                                                    
209300     EJECT                                                                
209400 IMS-4732-GET-SEGMENT SECTION.                                            
209500                                                                          
209600     MOVE 'WDGX4732' TO SSA1                                              
209700     MOVE '  GE' TO GOOD-STATUSCODES                                      
209800     CALL CBLTDLI USING GNP 4732-PCB DLI-IO-4732 SSA1                     
209900     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
210000     PERFORM IMS-STATUSCHECK                                              
210100     .                                                                    
210200     EJECT                                                                
210300 IMS-4735-GET-ROOT SECTION.                                               
210400                                                                          
210500     STRING 'WDR101  (WDGXKEY  =' WDGX01 ')'                              
210600          DELIMITED BY SIZE INTO SSA1                                     
210700     MOVE '  GE' TO GOOD-STATUSCODES                                      
210800     CALL CBLTDLI USING GU 4735-PCB DLI-IO-4735 SSA1                      
210900     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
211000     PERFORM IMS-STATUSCHECK                                              
211100     .                                                                    
211200     EJECT                                                                
211300 IMS-4735-GET-SEGMENT SECTION.                                            
211400                                                                          
211500     MOVE 'WDGX4735' TO SSA1                                              
211600     MOVE '  GE' TO GOOD-STATUSCODES                                      
211700     CALL CBLTDLI USING GNP 4735-PCB DLI-IO-4735 SSA1                     
211800     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
211900     PERFORM IMS-STATUSCHECK                                              
212000     .                                                                    
212100     EJECT                                                                
212200 IMS-4738-GET-ROOT SECTION.                                               
212300                                                                          
212400     STRING 'WDR101  (WDGXKEY  =' WDGX01 ')'                              
212500            DELIMITED BY SIZE INTO SSA1                                   
212600     MOVE '  GE' TO GOOD-STATUSCODES                                      
212700     CALL  CBLTDLI  USING GU   4738-PCB DLI-IO-4738 SSA1                  
212800     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
212900     PERFORM IMS-STATUSCHECK                                              
213000     .                                                                    
213100     EJECT                                                                
213200 IMS-4738-GET-SEGMENT SECTION.                                            
213300                                                                          
213400     MOVE 'WDGX4738' TO SSA1                                              
213500     MOVE '    ' TO GOOD-STATUSCODES                                      
213600     CALL  CBLTDLI  USING GNP  4738-PCB DLI-IO-4738 SSA1                  
213700     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
213800     PERFORM IMS-STATUSCHECK                                              
213900     .                                                                    
214000     EJECT                                                                
214100 IMS-GU-WDB101 SECTION.                                                   
214200                                                                          
214300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
214400          DELIMITED BY SIZE INTO SSA1                                     
214500     MOVE '  GE' TO GOOD-STATUSCODES                                      
214600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
214700     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
214800     PERFORM IMS-STATUSCHECK                                              
214900     .                                                                    
215000     EJECT                                                                
215100 IMS-GU-WDB201 SECTION.                                                   
215200                                                                          
215300     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
215400          DELIMITED BY SIZE INTO SSA1                                     
215500     MOVE '  GE' TO GOOD-STATUSCODES                                      
215600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
215700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSCHECK                                              
215900     .                                                                    
216000     EJECT                                                                
216100                                                                          
216200 IMS-GU-WDE711-ASEQ  SECTION.                                             
216300                                                                          
216400     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
216500          DELIMITED BY SIZE INTO SSA1                                     
216600                                                                          
216700     MOVE '  ' TO GOOD-STATUSCODES                                        
216800     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE711 SSA1                    
216900     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
217000                                                                          
217100     PERFORM IMS-STATUSCHECK                                              
217200     .                                                                    
217300     EJECT                                                                
217400 IMS-STATUSCHECK SECTION.                                                 
217500                                                                          
217600     SET STATUS-IX TO 1                                                   
217700     SEARCH GOOD-STATUS                                                   
217800       AT END                                                             
217900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
218000           DELIMITED BY SIZE INTO ERRTEXT                                 
218100         DISPLAY ERRTEXT                                                  
218200         CALL FELLOG                                                      
218300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
218400         CONTINUE                                                         
218500     END-SEARCH                                                           
219000     .                                                                    
220000                                                                          
