000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1029300.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   SEPTEMBER 1990.                                          
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION.                                                            
001000*        MPP-PRINTPROGRAM FÖR W10213, PROG-TO-PROG-SWITCH.                
001100*        LISTA STRUKTURRADER OCH LAGERPLATS.                              
001200*                                                                         
001300*    SUBPROGRAM:                                                          
001400*        W006PRS1 - SKÖTER ALL SKRIVNING MOT IMS-PRINTER.                 
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W1T293U                                             
001800*        MID:         W1I21301                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601*    -COPY WY2000W1                                                       
002610     SKIP3                                                                
002700 77  IDPGM                   PIC X(8)    VALUE 'W1029300'.                
002800 01  DYNAMISKA-SUBPROGRAM.                                                
002900    03 WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
003000    03 W006PRS1              PIC X(8)    VALUE 'W006PRS1'.                
003100    03 CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003200    03 FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
003300 77  JA                      PIC X       VALUE 'J'.                       
003400 77  NEJ                     PIC X       VALUE 'N'.                       
003500 77  RAETT-TEXT              PIC X       VALUE 'N'.                       
003600 77  GAELLANDE-STRUKTURRAD   PIC X       VALUE 'J'.                       
003700 77  RUB-IX                  PIC S9(9)   VALUE +1   COMP SYNC.            
003800 77  RADRAEKNARE             PIC S9(5)   VALUE ZERO.                      
003900     88 SIDBRYTNING                      VALUE +36 THRU +42.              
004000 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004100 77  STRUKTURNUMMER-SPAR     PIC S9(9)   VALUE ZERO COMP-3.               
004200 77  SATB-KDHOM-WS           PIC S9      VALUE ZERO COMP-3.               
004300 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
004400 77  BEART-WS                PIC X(25)   VALUE SPACE.                     
004500 77  DUMMY-RAD               PIC X(132)  VALUE SPACE.                     
004600 77  SATS-STR-CARP           PIC X(8)    VALUE '221     '.                
004700 77  SATS-STR-RA             PIC X(8)    VALUE '222     '.                
004800 77  SATS-STR-RB             PIC X(8)    VALUE '223     '.                
005100 77  SATS-STR-BERPV          PIC X(8)    VALUE '226     '.                
005300                                                                          
005400 01  SPAR-AAVV               PIC 9(4).                                    
005500 01  FILLER REDEFINES SPAR-AAVV.                                          
005600   03 SPAR-AA                PIC 99.                                      
005700   03 SPAR-VV                PIC 99.                                      
005800                                                                          
005900 01  FILLER                  PIC X(16)   VALUE 'DAGENS-DATUM'.            
006000 01  DAGENS-DATUM            PIC 9(6).                                    
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200   03  DAGENS-DATUM-AR       PIC 99.                                      
006300   03  DAGENS-DATUM-MANAD    PIC 99.                                      
006400   03  DAGENS-DATUM-DAG      PIC 99.                                      
006500 01  FILLER                  PIC X(16)   VALUE 'DAGENS-TID'.              
006600 01  DAGENS-TID              PIC 9(8).                                    
006700 01  FILLER REDEFINES DAGENS-TID.                                         
006800   03  DAGENS-TID-HH         PIC 99.                                      
006900   03  DAGENS-TID-MM         PIC 99.                                      
007000   03  FILLER                PIC 9(4).                                    
007100                                                                          
007200 77  INDATA-SW               PIC X       VALUE 'J'.                       
007300   88  INDATA-OK                         VALUE 'J'.                       
007400   88  INDATA-FEL                        VALUE 'N'.                       
007500                                                                          
007600 77  W-KDTRTYP               PIC X       VALUE SPACE.                     
007700   88  PRINT-OK                          VALUE 'X'.                       
007800                                                                          
007900 77  W-KDMFSFOR              PIC X       VALUE SPACE.                     
008000   88  SWEDISH-TEXT                      VALUE '1'.                       
008100   88  ENGLISH-TEXT                      VALUE '2'.                       
008200                                                                          
008300 77  W-IDPRTLST              PIC X(8)    VALUE SPACE.                     
008400                                                                          
008500 77  WS-KDSORT               PIC XX      VALUE SPACE.                     
008600                                                                          
008700 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
008800   88  GODK-MID                          VALUE '1213'.                    
008900     EJECT                                                                
009000******************************************************************        
009100*        LISTPOSTENS RUBRIKTEXTER, PÅ VARJE SPRÅK                *        
009200******************************************************************        
009300 01  RUBRIKTEXTER.                                                        
009400   03 RUB-1-1.                                                            
009500      05 FILLER           PIC X(18)  VALUE 'VISA STRUKTURRADER'.          
009600      05 FILLER           PIC X(18)  VALUE 'STRUCTURE LINES   '.          
009700   03 FILLER REDEFINES RUB-1-1.                                           
009800      05 RUB1-1 OCCURS 2  PIC X(18).                                      
009900   03 RUB-1-2.                                                            
010000      05 FILLER           PIC X(07)  VALUE 'DATUM'.                       
010100      05 FILLER           PIC X(07)  VALUE ' DATE'.                       
010200   03 FILLER REDEFINES RUB-1-2.                                           
010300      05 RUB1-2 OCCURS 2  PIC X(07).                                      
010400   03 RUB-1-3.                                                            
010500      05 FILLER           PIC X(06)  VALUE ' TID'.                        
010600      05 FILLER           PIC X(06)  VALUE 'TIME'.                        
010700   03 FILLER REDEFINES RUB-1-3.                                           
010800      05 RUB1-3 OCCURS 2  PIC X(06).                                      
010900*                                                                         
011000   03 RUB-2-1.                                                            
011100      05 FILLER           PIC X(16)  VALUE 'STRUKTURNUMMER'.              
011200      05 FILLER           PIC X(16)  VALUE 'PART NUMBER   '.              
011300   03 FILLER REDEFINES RUB-2-1.                                           
011400      05 RUB2-1 OCCURS 2  PIC X(16).                                      
011500   03 RUB-2-2.                                                            
011600      05 FILLER           PIC X(10)  VALUE '   SPRÅK'.                    
011700      05 FILLER           PIC X(10)  VALUE 'LANGUAGE'.                    
011800   03 FILLER REDEFINES RUB-2-2.                                           
011900      05 RUB2-2 OCCURS 2  PIC X(10).                                      
012000*                                                                         
012100   03 RUB-3-1.                                                            
012200      05 FILLER           PIC X(12)  VALUE 'BENÄMNING  '.                 
012300      05 FILLER           PIC X(12)  VALUE 'DESCRIPTION'.                 
012400   03 FILLER REDEFINES RUB-3-1.                                           
012500      05 RUB3-1 OCCURS 2  PIC X(12).                                      
012600   03 RUB-3-2.                                                            
012700      05 FILLER           PIC X(08)  VALUE 'PRODSL'.                      
012800      05 FILLER           PIC X(08)  VALUE 'PRODGR'.                      
012900   03 FILLER REDEFINES RUB-3-2.                                           
013000      05 RUB3-2 OCCURS 2  PIC X(08).                                      
013100   03 RUB-3-3.                                                            
013200      05 FILLER           PIC X(08)  VALUE 'FUNKGRP'.                     
013300      05 FILLER           PIC X(08)  VALUE 'FUNCGR '.                     
013400   03 FILLER REDEFINES RUB-3-3.                                           
013500      05 RUB3-3 OCCURS 2  PIC X(08).                                      
013600   03 RUB-3-4.                                                            
013700      05 FILLER           PIC X(16)  VALUE '   STRUKTURTYP'.              
013800      05 FILLER           PIC X(16)  VALUE 'TYPE OF STRUCT'.              
013900   03 FILLER REDEFINES RUB-3-4.                                           
014000      05 RUB3-4 OCCURS 2  PIC X(16).                                      
014100   03 RUB-3-5.                                                            
014200      05 FILLER           PIC X(11)  VALUE '   CLAGER'.                   
014300      05 FILLER           PIC X(11)  VALUE 'WAREHOUSE'.                   
014400   03 FILLER REDEFINES RUB-3-5.                                           
014500      05 RUB3-5 OCCURS 2  PIC X(11).                                      
014600*                                                                         
014700   03 RUB-4-1.                                                            
014800      05 FILLER           PIC X(11)   VALUE 'STRUKTURNOT'.                
014900      05 FILLER           PIC X(11)   VALUE 'STRUCT NOTE'.                
015000   03 FILLER REDEFINES RUB-4-1.                                           
015100      05 RUB4-1 OCCURS 2  PIC X(11).                                      
015200*                                                                         
015300   03 RUB-5-1.                                                            
015400      05 FILLER           PIC X(08)   VALUE ' RAD'.                       
015500      05 FILLER           PIC X(08)   VALUE 'LINE'.                       
015600   03 FILLER REDEFINES RUB-5-1.                                           
015700      05 RUB5-1 OCCURS 2  PIC X(08).                                      
015800   03 RUB-5-2.                                                            
015900      05 FILLER           PIC X(14)  VALUE 'ANTAL  LEVNR'.                
016000      05 FILLER           PIC X(14)  VALUE 'QUANT  SUPPL'.                
016100   03 FILLER REDEFINES RUB-5-2.                                           
016200      05 RUB5-2 OCCURS 2  PIC X(14).                                      
016300   03 RUB-5-3.                                                            
016400      05 FILLER           PIC X(30)  VALUE 'ARTNR/LEVBET     '.           
016500      05 FILLER           PIC X(30)  VALUE 'PART NO/REFERENCE'.           
016600   03 FILLER REDEFINES RUB-5-3.                                           
016700      05 RUB5-3 OCCURS 2  PIC X(30).                                      
016800   03 RUB-5-4.                                                            
016900      05 FILLER           PIC X(16)  VALUE 'BENÄMNING  '.                 
017000      05 FILLER           PIC X(16)  VALUE 'DESCRIPTION'.                 
017100   03 FILLER REDEFINES RUB-5-4.                                           
017200      05 RUB5-4 OCCURS 2  PIC X(16).                                      
017300   03 RUB-5-5.                                                            
017400      05 FILLER           PIC X(15)  VALUE 'ST  STATUS  FG'.              
017500      05 FILLER           PIC X(15)  VALUE 'TS  STATUS  DG'.              
017600   03 FILLER REDEFINES RUB-5-5.                                           
017700      05 RUB5-5 OCCURS 2  PIC X(15).                                      
017800   03 RUB-5-6.                                                            
017900      05 FILLER           PIC X(17)  VALUE 'ERS RES  OMR GNG'.            
018000      05 FILLER           PIC X(17)  VALUE 'SUP  SP AREA AIS'.            
018100   03 FILLER REDEFINES RUB-5-6.                                           
018200      05 RUB5-6 OCCURS 2  PIC X(17).                                      
018300   03 RUB-5-7.                                                            
018400      05 FILLER           PIC X(10)  VALUE 'PLATS     '.                  
018500      05 FILLER           PIC X(10)  VALUE '  LOC     '.                  
018600   03 FILLER REDEFINES RUB-5-7.                                           
018700      05 RUB5-7 OCCURS 2  PIC X(10).                                      
018800   03 RUB-5-8.                                                            
018900      05 FILLER           PIC X(05)  VALUE 'SORT '.                       
019000      05 FILLER           PIC X(05)  VALUE 'SORT '.                       
019100   03 FILLER REDEFINES RUB-5-8.                                           
019200      05 RUB5-8 OCCURS 2  PIC X(05).                                      
019300*                                                                         
019400   03 INF-1.                                                              
019500      05 FILLER           PIC X(12)  VALUE 'FORTS...    '.                
019600      05 FILLER           PIC X(12)  VALUE 'CONTINUED...'.                
019700   03 FILLER REDEFINES INF-1.                                             
019800      05 INF1   OCCURS 2  PIC X(12).                                      
019900   03 INF-2.                                                              
020000      05 FILLER           PIC X(08)  VALUE '...SLUT.'.                    
020100      05 FILLER           PIC X(08)  VALUE '...END. '.                    
020200   03 FILLER REDEFINES INF-2.                                             
020300      05 INF2   OCCURS 2  PIC X(08).                                      
020400     EJECT                                                                
020500******************************************************************        
020600*        LISTPOST AREA, RUBRIKER                                 *        
020700******************************************************************        
020800 01  FILLER               PIC X(16)  VALUE '        RUB1-RAD'.            
020900 01  RUB1-RAD.                                                            
021000   03 FILLER              PIC X(2)   VALUE  SPACE.                        
021100   03         FILLER      PIC X(9)   VALUE '1 2 1 3'.                     
021200   03 RUB1-T1             PIC X(18).                                      
021300   03         FILLER      PIC X(57)  VALUE  SPACE.                        
021400   03 RUB1-T2             PIC X(07).                                      
021500   03 RUB1-AA             PIC 99.                                         
021600   03         FILLER      PIC X      VALUE '/'.                           
021700   03 RUB1-MM             PIC 99.                                         
021800   03         FILLER      PIC X      VALUE '/'.                           
021900   03 RUB1-DD             PIC 99.                                         
022000   03         FILLER      PIC X(04)  VALUE  SPACE.                        
022100   03 RUB1-T3             PIC X(06).                                      
022200   03 RUB1-HH             PIC 99.                                         
022300   03         FILLER      PIC X      VALUE ':'.                           
022400   03 RUB1-MIN            PIC 99.                                         
022410   03         FILLER      PIC X(16)  VALUE  SPACE.                        
022500     EJECT                                                                
022600****************************************************************          
022700 01  FILLER               PIC X(16)  VALUE '        RUB2-RAD'.            
022800 01  RUB2-RAD.                                                            
022900   03 FILLER              PIC X(2)   VALUE  SPACE.                        
023000   03 RUB2-T1             PIC X(16).                                      
023100   03 RUB2-IDARTNR        PIC Z(8)9  VALUE  ZERO.                         
023200   03         FILLER      PIC X(12)  VALUE  SPACE.                        
023300   03 RUB2-T2             PIC X(10).                                      
023400   03 RUB2-IDSKYLT        PIC X(03).                                      
023500   03         FILLER      PIC X(64)  VALUE  SPACE.                        
023510   03         FILLER      PIC X(16)  VALUE  SPACE.                        
023600                                                                          
023700****************************************************************          
023800 01  FILLER               PIC X(16)  VALUE '        RUB3-RAD'.            
023900 01  RUB3-RAD.                                                            
024000   03 RUB3-T1             PIC X(12).                                      
024100   03 RUB3-BEART          PIC X(25).                                      
024200   03         FILLER      PIC X(03)  VALUE  SPACE.                        
024300   03 RUB3-T2             PIC X(08).                                      
024400   03 RUB3-KDPRODSL       PIC Z9     VALUE  ZERO.                         
024500   03         FILLER      PIC X(06)  VALUE  SPACE.                        
024600   03 RUB3-T3             PIC X(08).                                      
024700   03 RUB3-IDFKNGRP       PIC Z(4)9  VALUE  ZERO.                         
024800   03         FILLER      PIC X(04)  VALUE  SPACE.                        
024900   03 RUB3-T4             PIC X(16).                                      
025000   03 RUB3-IDSTRTYP       PIC X(4).                                       
025100   03 RUB3-T5             PIC X(11).                                      
025200   03 RUB3-KDCLAGER       PIC 9      VALUE  1.                            
025300   03         FILLER      PIC X(03)  VALUE  SPACE.                        
025400   03 RUB3-T6             PIC X(05).                                      
025410   03         FILLER      PIC X(16)  VALUE  SPACE.                        
025500                                                                          
025600****************************************************************          
025700 01  FILLER               PIC X(16)  VALUE '       RUB4A-RAD'.            
025800 01  RUB4A-RAD.                                                           
025900   03 FILLER              PIC X(2)   VALUE  SPACE.                        
026000   03 RUB4-T1             PIC X(11).                                      
026100   03         FILLER      PIC X(01)  VALUE  SPACE.                        
026200   03 RUB4A-TESTRNOT      PIC X(70)  VALUE  SPACE.                        
026300   03         FILLER      PIC X(32)  VALUE  SPACE.                        
026310   03         FILLER      PIC X(16)  VALUE  SPACE.                        
026400                                                                          
026500****************************************************************          
026600 01  FILLER               PIC X(16)  VALUE '       RUB4B-RAD'.            
026700 01  RUB4B-RAD.                                                           
026800   03         FILLER      PIC X(14)  VALUE  SPACE.                        
026900   03 RUB4B-TESTRNOT      PIC X(70)  VALUE  SPACE.                        
027000   03         FILLER      PIC X(32)  VALUE  SPACE.                        
027010   03         FILLER      PIC X(16)  VALUE  SPACE.                        
027100                                                                          
027200****************************************************************          
027300 01  FILLER               PIC X(16)  VALUE '        RUB5-RAD'.            
027400 01  RUB5-RAD.                                                            
027500   03 FILLER              PIC X(2)   VALUE  SPACE.                        
027600   03 RUB5-T1             PIC X(8).                                       
027700   03 RUB5-T2             PIC X(14).                                      
027800   03 RUB5-T3             PIC X(32).                                      
027900   03 RUB5-T4             PIC X(16).                                      
028000   03 RUB5-T5             PIC X(15).                                      
028100   03 RUB5-T6             PIC X(17).                                      
028200   03 RUB5-T7             PIC X(07).                                      
028300   03 RUB5-T8             PIC X(5).                                       
028310   03         FILLER      PIC X(16)  VALUE  SPACE.                        
028400     EJECT                                                                
028500******************************************************************        
028600*        LISTPOST AREA, UTRADER                                  *        
028700******************************************************************        
028800 01  FILLER               PIC X(16)  VALUE '         UTRADER'.            
028900 01  UTRAD                PIC X(132)  VALUE SPACE.                        
029000 01  FILLER REDEFINES UTRAD.                                              
029100   03         FILLER      PIC X.                                          
029200   03 L1-IDRADNR          PIC Z(4)9.                                      
029300   03         FILLER      PIC X(3).                                       
029400   03 L1-REANTPSA         PIC Z9.9(3).                                    
029500   03         FILLER      PIC X(2).                                       
029600   03 L1-IDLEVNR          PIC X(5).                                       
029700   03         FILLER      PIC X(2).                                       
029800   03 L1-BELEVART         PIC X(30).                                      
029900   03         FILLER      PIC X(2).                                       
030000   03 L1-BEART            PIC X(15).                                      
030100   03         FILLER      PIC X(2).                                       
030200   03 L1-IDSTRTYP         PIC X.                                          
030300   03         FILLER      PIC X(2).                                       
030400   03 L1-KDISATS          PIC X.                                          
030500   03         FILLER      PIC X.                                          
030600   03 L1-AAVV             PIC Z(4).                                       
030700   03         FILLER      PIC X(30).                                      
030800   03 L1-KDSORT           PIC X(2).                                       
030810   03         FILLER      PIC X(16).                                      
030900 01  FILLER REDEFINES UTRAD.                                              
031000   03         FILLER      PIC X.                                          
031100   03 L2-IDRADNR          PIC Z(4)9.                                      
031200   03         FILLER      PIC X(3).                                       
031300   03 L2-REANTPSA         PIC Z9.9(3).                                    
031400   03         FILLER      PIC X(9).                                       
031500   03 L2-IDARTNR          PIC Z(8)9.                                      
031600   03         FILLER      PIC X(23).                                      
031700   03 L2-BEART            PIC X(15).                                      
031800   03         FILLER      PIC X(2).                                       
031900   03 L2-IDSTRTYP         PIC X.                                          
032000   03         FILLER      PIC X(2).                                       
032100   03 L2-KDISATS          PIC X.                                          
032200   03         FILLER      PIC X.                                          
032300   03 L2-AAVV             PIC Z(4).                                       
032400   03         FILLER      PIC X(2).                                       
032500   03 L2-KDFARLIG         PIC X.                                          
032600   03         FILLER      PIC X(2).                                       
032700   03 L2-KDERS            PIC ZZ9.                                        
032800   03         FILLER      PIC X(3).                                       
032900   03 L2-FLLSRDEL         PIC X(1).                                       
033000   03         FILLER      PIC X(2).                                       
033100   03 L2-ADLAGOMR         PIC ZZ9.                                        
033200   03         FILLER      PIC X(1).                                       
033300   03 L2-ADGANG           PIC ZZ9.                                        
033400   03         FILLER      PIC X(1).                                       
033500   03 L2-ADPLATS          PIC Z(4)9.                                      
033600   03         FILLER      PIC X(3).                                       
033700   03 L2-KDSORT           PIC X(2).                                       
033710   03         FILLER      PIC X(16).                                      
033800 01  FILLER REDEFINES UTRAD.                                              
033900   03         FILLER      PIC X(12).                                      
034000   03 L3-INFORAD          PIC X(70).                                      
034100   03         FILLER      PIC X(34).                                      
034110   03         FILLER      PIC X(16).                                      
034200     EJECT                                                                
034300******************************************************************        
034400*        PARAMETERAREA FÖR DATUMKONTROLL                         *        
034500******************************************************************        
034600 01  FILLER                  PIC X(16)   VALUE 'WDATAREA'.                
034700*01  -COPY WDATAREA                                                       
034800     EJECT                                                                
034900******************************************************************        
035000*        PRINTER STYRKODER                                       *        
035100******************************************************************        
035200 01  FILLER                  PIC X(16)   VALUE 'PRT-W006PRAR'.            
035300*01        -COPY W006PRAR                                                 
035400     EJECT                                                                
035500******************************************************************        
035600*        PROGRAM-TO-PROGRAM                                      *        
035700******************************************************************        
035800 01  FILLER                  PIC X(16)   VALUE 'MID-MID-AREA'.            
035900*01  AREA  -COPY W1I21301 -PRE MID-.                                      
036000 01  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.                
036100*01  -COPY WMSGAREA                                                       
036200     EJECT                                                                
036300******************************************************************        
036400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                       *        
036500******************************************************************        
036600 01  IMS-WS.                                                              
036700   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
036800     SKIP3                                                                
036900 01  NYCKLAR-TILL-DLI.                                                    
037000   03  W-IDARTNR-X.                                                       
037100     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
037400   03  W-IDSKYLT-X.                                                       
037500     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
037600   03  W-KDSTRRAD-X.                                                      
037700     05  W-KDSTRRAD          PIC  X      VALUE '0'.                       
037800   03  W-IDRADNR-X.                                                       
037900     05  W-IDRADNR           PIC  S9(5)  VALUE +10   COMP-3.              
038200   03  W-BEART-X.                                                         
038300     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
038400     SKIP3                                                                
038500*                        **** STATUS-KOD FRÅN IMS                         
038600   03  STATUS-WS             PIC XX.                                      
038700     88  SEGMENT-FINNS                   VALUE '  '.                      
038800     88  SEGMENT-SAKNAS                  VALUE 'GE' 'GB'.                 
038900     SKIP3                                                                
039000   03  GODK-STATUSKODER.                                                  
039100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039200     SKIP3                                                                
039300 01  FILLER                  PIC X(8)   VALUE 'SSA-AREA'.                 
039400 01  SSA1        PIC X(64).                                               
039500 01  SSA2        PIC X(64).                                               
039600 01  SSA3        PIC X(64).                                               
039700     EJECT                                                                
039800*                            IMS FUNKTIONSKODER                           
039900*01    -COPY W0003                                                        
040000     EJECT                                                                
040100*                            DLI INPUT-OUTPUT AREA                        
040200 01  DLI-IO-AREA.                                                         
040300   03  IO-AREA-1             PIC X(250)  VALUE SPACE.                     
040400     SKIP3                                                                
040500*  03  WLSATB01  -COPY WDJ101  -PRE SATB-   -RED IO-AREA-1.               
040600     EJECT                                                                
040700*  03  WLSATB11  -COPY WDJ111  -PRE SATB-   -RED IO-AREA-1.               
040800     EJECT                                                                
040900*  03  WLSATB22  -COPY WDJ122  -PRE SATB-   -RED IO-AREA-1.               
041000     EJECT                                                                
041100   03  IO-AREA-2             PIC X(900)  VALUE SPACE.                     
041200     SKIP3                                                                
041500*  03  WLARTC01  -COPY WDK601               -RED IO-AREA-2.               
041600     EJECT                                                                
041700*  03  WLARTC11  -COPY WDK611               -RED IO-AREA-2.               
041800     EJECT                                                                
042100*  03  WLBENA01  -COPY WDD301  -PRE BEN-    -RED IO-AREA-2.               
042200     EJECT                                                                
042300*  03  WLBENA11  -COPY WDD311  -PRE BEN-    -RED IO-AREA-2.               
042400     EJECT                                                                
042500                                                                          
042600 LINKAGE SECTION.                                                         
042700*01  -COPY W0009     -PRE MSG-                                            
042800     EJECT                                                                
042900*01  -COPY W0009     -PRE ALT-                                            
043000                                                                          
043400*01  -COPY W0008     -PRE ARTC-                                           
043500     05  FILLER              PIC X.                                       
043600     EJECT                                                                
043700*01  -COPY W0008     -PRE SATB-                                           
043800     05  FILLER              PIC X.                                       
043900                                                                          
044000*01  -COPY W0008     -PRE BENAA-                                          
044100     05  FILLER              PIC X.                                       
044200     EJECT                                                                
044300*01  -COPY W0008     -PRE BENAB-                                          
044400     05  FILLER              PIC X.                                       
044500     EJECT                                                                
044600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ARTC-PCB                       
044700                                 SATB-PCB BENAA-PCB BENAB-PCB.            
044710 MAIN SECTION.                                                            
044800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ARTC-PCB                       
044900                                 SATB-PCB BENAA-PCB BENAB-PCB.            
045000     PERFORM IMS-GET-MSG                                                  
045100     IF SEGMENT-FINNS                                                     
045200        PERFORM A-INITIERA                                                
045300        PERFORM B-BEHANDLA-INDATA                                         
045400        IF INDATA-OK AND PRINT-OK                                         
045500           PERFORM IMS-GU-SATB-ART                                        
045600           IF SEGMENT-FINNS                                               
045700              CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN                  
045800                                  W-IDPRTLST ALT-PCB                      
045900                                  DUMMY-RAD DUMMY-RAD                     
046000              PERFORM C-REDIGERA-SKRIV-RUBRIKER                           
046100              PERFORM IMS-GNP-SATB-RAD                                    
046200              PERFORM UNTIL SEGMENT-SAKNAS                                
046300                 PERFORM D-SKRIV-RADER                                    
046400                 IF SEGMENT-SAKNAS                                        
046500                    MOVE '9' TO W-KDSTRRAD                                
046600                    PERFORM IMS-GNP-SATB-RAD                              
046700                 END-IF                                                   
046800                 IF SEGMENT-SAKNAS                                        
046900                    MOVE INF2(RUB-IX)  TO UTRAD                           
047000                    CALL W006PRS1 USING PRT-SPOOL-OVR                     
047100                                        PRT-WRITE                         
047200                                        W-IDPRTLST ALT-PCB                
047300                                        PRT-AFTER-1 UTRAD                 
047400                 END-IF                                                   
047500              END-PERFORM                                                 
047600              CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE                 
047700                                  W-IDPRTLST ALT-PCB                      
047800                                  DUMMY-RAD DUMMY-RAD                     
047900           END-IF                                                         
048000        END-IF                                                            
048100     END-IF                                                               
048200                                                                          
048300     MOVE ZERO TO RETURN-CODE                                             
048400     GOBACK                                                               
048500     .                                                                    
048600     EJECT                                                                
048700 A-INITIERA SECTION.                                                      
048800     IF MSG-DUBBLA-TRANSKODER                                             
048900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-MID-W1I21301            
049000        MOVE MSG-IDTRANS-2                 TO W-IDTRANS                   
049100        MOVE MSG-KDMFSFOR-2                TO W-KDMFSFOR                  
049200     ELSE                                                                 
049300        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-MID-W1I21301            
049400        MOVE MSG-IDTRANS-1                 TO W-IDTRANS                   
049500        MOVE MSG-KDMFSFOR-1                TO W-KDMFSFOR                  
049600     END-IF                                                               
049700     MOVE MSG-KDTRTYP                      TO W-KDTRTYP                   
049800                                                                          
049900     MOVE LOW-VALUE  TO MSG-AREA                                          
050000                                                                          
050100     IF NOT GODK-MID                                                      
050200       MOVE SPACE TO W-KDTRTYP                                            
050300     END-IF                                                               
050400                                                                          
050500     IF ENGLISH-TEXT                                                      
050600        MOVE +2 TO  RUB-IX                                                
050700     ELSE                                                                 
050800        MOVE +1 TO  RUB-IX                                                
050900     END-IF                                                               
051000                                                                          
051100     ACCEPT DAGENS-DATUM FROM DATE                                        
051200     ACCEPT DAGENS-TID   FROM TIME                                        
051300                                                                          
051400     PERFORM AA-INIT-RUBRIKTEXTER                                         
051500     .                                                                    
051600     EJECT                                                                
051700 AA-INIT-RUBRIKTEXTER  SECTION.                                           
051800     SKIP2                                                                
051900     IF MID-MID-IDSKYLT-UT = 'S  '                                        
052000        CONTINUE                                                          
052100     ELSE                                                                 
052200        MOVE +2  TO RUB-IX                                                
052300     END-IF                                                               
052400     MOVE RUB1-1(RUB-IX) TO RUB1-T1                                       
052500     MOVE RUB1-2(RUB-IX) TO RUB1-T2                                       
052600     MOVE RUB1-3(RUB-IX) TO RUB1-T3                                       
052700*                                                                         
052800     MOVE RUB2-1(RUB-IX) TO RUB2-T1                                       
052900     MOVE RUB2-2(RUB-IX) TO RUB2-T2                                       
053000*                                                                         
053100     MOVE RUB3-1(RUB-IX) TO RUB3-T1                                       
053200     MOVE RUB3-2(RUB-IX) TO RUB3-T2                                       
053300     MOVE RUB3-3(RUB-IX) TO RUB3-T3                                       
053400     MOVE RUB3-4(RUB-IX) TO RUB3-T4                                       
053500     MOVE RUB3-5(RUB-IX) TO RUB3-T5                                       
053600*                                                                         
053700     MOVE RUB4-1(RUB-IX) TO RUB4-T1                                       
053800*                                                                         
053900     MOVE RUB5-1(RUB-IX) TO RUB5-T1                                       
054000     MOVE RUB5-2(RUB-IX) TO RUB5-T2                                       
054100     MOVE RUB5-3(RUB-IX) TO RUB5-T3                                       
054200     MOVE RUB5-4(RUB-IX) TO RUB5-T4                                       
054300     MOVE RUB5-5(RUB-IX) TO RUB5-T5                                       
054400     MOVE RUB5-6(RUB-IX) TO RUB5-T6                                       
054500     MOVE RUB5-7(RUB-IX) TO RUB5-T7                                       
054600     MOVE RUB5-8(RUB-IX) TO RUB5-T8                                       
054700     .                                                                    
054800     EJECT                                                                
054900 B-BEHANDLA-INDATA SECTION.                                               
055000     SKIP2                                                                
055100     MOVE JA TO  INDATA-SW                                                
055200     IF MID-MID-IDARTNR-IN = ALL '+'                                      
055300        MOVE MID-MID-IDARTNR-UT TO IDARTNR-WS                             
055400        INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                
055500        MOVE IDARTNR-WS TO W-IDARTNR STRUKTURNUMMER-SPAR                  
055600     ELSE                                                                 
055700        MOVE ZERO TO IDARTNR-WS  W-IDARTNR                                
055800        MOVE NEJ  TO INDATA-SW                                            
055900     END-IF                                                               
056000                                                                          
056100     MOVE MID-MID-IDSKYLT-UT TO IDSKYLT-WS  W-IDSKYLT                     
056200                                                                          
056300**** BESTÄM UTSKRIFTSADRESSEN *******************************             
056400     EVALUATE MID-MID-KDPRTVAL                                            
056700        WHEN 'A' MOVE SATS-STR-RA     TO W-IDPRTLST                       
056800*                                     PACKNINGEN                          
056900        WHEN 'B' MOVE SATS-STR-RB     TO W-IDPRTLST                       
057000*                                     FÖRPACKNINGEN                       
057500        WHEN 'C' MOVE SATS-STR-BERPV  TO W-IDPRTLST                       
057600*                                     BEREDNINGEN PV (PVV)                
057700        WHEN 'D' MOVE SATS-STR-CARP   TO W-IDPRTLST                       
057800*                                     CARPAC                              
057900        WHEN OTHER                                                        
058000           MOVE NEJ TO INDATA-SW                                          
058100     END-EVALUATE                                                         
058200     .                                                                    
058300     EJECT                                                                
058400 C-REDIGERA-SKRIV-RUBRIKER SECTION.                                       
058500     SKIP2                                                                
058600     MOVE DAGENS-DATUM-AR          TO RUB1-AA                             
058700     MOVE DAGENS-DATUM-MANAD       TO RUB1-MM                             
058800     MOVE DAGENS-DATUM-DAG         TO RUB1-DD                             
058900     MOVE DAGENS-TID-HH            TO RUB1-HH                             
059000     MOVE DAGENS-TID-MM            TO RUB1-MIN                            
059100     MOVE IDARTNR-WS               TO RUB2-IDARTNR                        
059200     MOVE IDSKYLT-WS               TO RUB2-IDSKYLT                        
059300     IF (IDSKYLT-WS NOT = 'S  ')                                          
059400     OR (SATB-STR-BEART-SVE = SPACE)                                      
059500        IF  (IDSKYLT-WS NOT = 'S  ')                                      
059600*       AND (SATB-STR-KDBENHOM > 0)                                       
059700           MOVE SATB-STR-BEART-SVE TO W-BEART                             
059800           MOVE SATB-STR-KDBENHOM  TO SATB-KDHOM-WS                       
059900           PERFORM CA-HAEMTA-RAETT-HOMONYM                                
060000           MOVE BEART-WS           TO RUB3-BEART                          
060100        ELSE                                                              
060200           PERFORM IMS-GU-BENA-TEXT-BSEQ                                  
060300           IF SEGMENT-FINNS                                               
060400              MOVE BEN-TEXT-BEART  TO RUB3-BEART                          
060500           ELSE                                                           
060600              MOVE ALL '*'         TO RUB3-BEART                          
060700           END-IF                                                         
060800        END-IF                                                            
060900     ELSE                                                                 
061000        MOVE SATB-STR-BEART-SVE    TO RUB3-BEART                          
061100     END-IF                                                               
061200     IF  (SATB-STR-KDPRODSL NOT = ZERO)                                   
061300     AND (SATB-STR-IDFKNGRP NOT = ZERO)                                   
061400        MOVE SATB-STR-KDPRODSL     TO RUB3-KDPRODSL                       
061500        MOVE SATB-STR-IDFKNGRP     TO RUB3-IDFKNGRP                       
061600     ELSE                                                                 
061700        PERFORM IMS-GU-ARTC01                                             
061800        IF SEGMENT-FINNS                                                  
061900           MOVE ART-KDPRODSL       TO RUB3-KDPRODSL                       
062000           MOVE ART-IDFKNGRP       TO RUB3-IDFKNGRP                       
062100        END-IF                                                            
062200     END-IF                                                               
062300     MOVE SATB-STR-IDSTRTYP        TO RUB3-IDSTRTYP                       
062400     MOVE SATB-STR-TESTRNOT(1)     TO RUB4A-TESTRNOT                      
062500     MOVE SATB-STR-TESTRNOT(2)     TO RUB4B-TESTRNOT                      
062600     PERFORM S90-SKRIV-HUVUDRUBRIKER                                      
062700     .                                                                    
062800     EJECT                                                                
062900 CA-HAEMTA-RAETT-HOMONYM SECTION.                                         
063000     SKIP2                                                                
063100     MOVE SPACE               TO BEART-WS                                 
063200     MOVE 'S  '               TO W-IDSKYLT                                
063300     PERFORM IMS-GU-BENA-ASEQ                                             
063400     IF SEGMENT-FINNS                                                     
063500        IF BEN-BEN-KDHOMONYM = SATB-KDHOM-WS                              
063600           MOVE IDSKYLT-WS    TO W-IDSKYLT                                
063700           PERFORM  IMS-GNP-BEN-TEXT-ASEQ                                 
063800           IF SEGMENT-FINNS                                               
063900              MOVE BEN-TEXT-BEART TO BEART-WS                             
064000           END-IF                                                         
064100        ELSE                                                              
064200           PERFORM IMS-GNP-BENA-HOM                                       
064300           IF SEGMENT-FINNS                                               
064400              PERFORM CAA-HITTA-RAETT-TEXT                                
064500           ELSE                                                           
064600              PERFORM IMS-GU-BENA-ASEQ                                    
064700              IF SEGMENT-FINNS                                            
064800* - - - - - - - - - - - - - - - - - - - - - - - - LÄS TEXT                
064900                 MOVE IDSKYLT-WS        TO W-IDSKYLT                      
065000                 PERFORM IMS-GNP-BEN-TEXT-ASEQ                            
065100                 IF SEGMENT-FINNS                                         
065200                    MOVE BEN-TEXT-BEART TO BEART-WS                       
065300                 END-IF                                                   
065400              END-IF                                                      
065500           END-IF                                                         
065600        END-IF                                                            
065700     END-IF                                                               
065800     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
065900     .                                                                    
066000     EJECT                                                                
066100 CAA-HITTA-RAETT-TEXT SECTION.                                            
066200     SKIP2                                                                
066300     MOVE NEJ TO RAETT-TEXT                                               
066400     PERFORM IMS-GN-BENA-ASEQ                                             
066500     PERFORM UNTIL  SEGMENT-SAKNAS OR RAETT-TEXT = JA                     
066600        IF BEN-BEN-KDHOMONYM = SATB-KDHOM-WS                              
066700           MOVE IDSKYLT-WS TO W-IDSKYLT                                   
066800                                                                          
066900           PERFORM IMS-GNP-BEN-TEXT-ASEQ                                  
067000           IF SEGMENT-FINNS                                               
067100              MOVE BEN-TEXT-BEART TO BEART-WS                             
067200           END-IF                                                         
067300           MOVE JA TO RAETT-TEXT                                          
067400        ELSE                                                              
067500           PERFORM IMS-GN-BENA-ASEQ                                       
067600        END-IF                                                            
067700     END-PERFORM                                                          
067800     .                                                                    
067900     EJECT                                                                
068000 D-SKRIV-RADER  SECTION.                                                  
068100     SKIP2                                                                
068200     IF SEGMENT-FINNS                                                     
068300        MOVE JA                TO GAELLANDE-STRUKTURRAD                   
068400        MOVE SATB-RAD-KDSTRRAD TO W-KDSTRRAD                              
068500        MOVE SATB-RAD-IDRADNR  TO W-IDRADNR                               
068600        MOVE SATB-RAD-IDARTNR  TO W-IDARTNR                               
068610        MOVE SATB-RAD-TISTODAT TO TMP1-YYMMDD                             
068620        MOVE DAGENS-DATUM      TO TMP2-YYMMDD                             
068630        PERFORM WY2000P1                                                  
068700        EVALUATE TRUE                                                     
068800           WHEN  SATB-RAD-KDISATS = 'N' OR 'T'                            
068900                 MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                  
069000           WHEN  SATB-RAD-KDISATS = 'E'                                   
069100                 MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                  
069200           WHEN   (SATB-RAD-KDISATS  = 'U')                               
069300              AND (TMP1-YYMMDD > TMP2-YYMMDD)                             
069400                 MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                  
069500           WHEN   (SATB-RAD-KDISATS  = SPACE)                             
069600              AND (TMP1-YYMMDD > TMP2-YYMMDD)                             
069700                 MOVE ZERO              TO DAT-I-TIDATUM                  
069800                                           SPAR-AAVV                      
069900           WHEN OTHER                                                     
070000                 MOVE ZERO TO DAT-I-TIDATUM                               
070100                 MOVE NEJ  TO GAELLANDE-STRUKTURRAD                       
070200        END-EVALUATE                                                      
070300                                                                          
070400        IF  DAT-I-TIDATUM > ZERO                                          
070500           MOVE 'AAMMDD'       TO DAT-KDDATFORM                           
070600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
070700                               DAT-O-TIDATUM DAT-KDSVAR                   
070800           MOVE DAT-TIAA-VECKA TO SPAR-AA                                 
070900           MOVE DAT-TIVV       TO SPAR-VV                                 
071000        END-IF                                                            
071100                                                                          
071200        IF GAELLANDE-STRUKTURRAD = JA                                     
071300           PERFORM DA-REDIGERA-SKRIV-RAD                                  
071400           PERFORM IMS-GNP-SATB-NOT-OKVAL                                 
071500                                                                          
071600           IF SEGMENT-FINNS                                               
071700              MOVE SATB-NOT-TESTRNOT(1) TO L3-INFORAD                     
071800              PERFORM S93-SKRIV-UTRAD-NOTERING                            
071900                                                                          
072000              IF SATB-NOT-TESTRNOT(2) NOT = SPACE                         
072100                 MOVE SATB-NOT-TESTRNOT(2) TO L3-INFORAD                  
072200                 PERFORM S93-SKRIV-UTRAD-NOTERING                         
072300              END-IF                                                      
072400           END-IF                                                         
072500        END-IF                                                            
072600        IF SIDBRYTNING                                                    
072700           MOVE INF1(RUB-IX)     TO UTRAD                                 
072800           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                    
072900                               W-IDPRTLST ALT-PCB                         
073000                               PRT-AFTER-1 UTRAD                          
073100           MOVE SPACE TO UTRAD                                            
073200        END-IF                                                            
073300        MOVE STRUKTURNUMMER-SPAR TO W-IDARTNR                             
073400        PERFORM IMS-GNP-SATB-RAD                                          
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 DA-REDIGERA-SKRIV-RAD  SECTION.                                          
073900     SKIP2                                                                
074000     IF SATB-RAD-IDARTNR NOT = ZERO                                       
074100        MOVE SATB-RAD-IDRADNR      TO L2-IDRADNR                          
074200        MOVE SATB-RAD-REANTPSA     TO L2-REANTPSA                         
074300        MOVE SATB-RAD-IDARTNR      TO L2-IDARTNR                          
074400        MOVE SATB-RAD-KDISATS      TO L2-KDISATS                          
074500        MOVE SATB-RAD-IDSTRTYP     TO L2-IDSTRTYP                         
074600        MOVE SPAR-AAVV             TO L2-AAVV                             
074700        MOVE SATB-RAD-KDSORT       TO WS-KDSORT                           
074800        IF WS-KDSORT = SPACE                                              
074900           PERFORM LAES-KDSORT                                            
075000        END-IF                                                            
075100        MOVE WS-KDSORT       TO L2-KDSORT                                 
075200        IF SATB-RAD-BEART-SVE = SPACE                                     
075300           PERFORM IMS-GU-ARTC01                                          
075400           IF SEGMENT-FINNS                                               
076100              PERFORM IMS-GU-ARTC11                                       
076200              IF SEGMENT-FINNS                                            
076300                 MOVE CLAG-FLLSRDEL TO L2-FLLSRDEL                        
076310                 MOVE CLAG-KDFARLIG TO L2-KDFARLIG                        
076600                 MOVE CLAG-KDERS    TO L2-KDERS                           
076610                 MOVE CLAG-ADLAGOMR TO L2-ADLAGOMR                        
076620                 MOVE CLAG-ADGANG   TO L2-ADGANG                          
076630                 MOVE CLAG-ADPLATS  TO L2-ADPLATS                         
076900              ELSE                                                        
077000                 MOVE ZEROES        TO L2-ADLAGOMR                        
077100                                       L2-ADGANG                          
077200                                       L2-ADPLATS                         
077300              END-IF                                                      
077400              PERFORM IMS-GU-BENA-TEXT-BSEQ                               
077500              IF SEGMENT-FINNS                                            
077600                 MOVE BEN-TEXT-BEART  TO L2-BEART                         
077700              END-IF                                                      
077800           END-IF                                                         
077900        ELSE                                                              
078000           IF IDSKYLT-WS = 'S  '                                          
078100              MOVE SATB-RAD-BEART-SVE    TO L2-BEART                      
078200           ELSE                                                           
078300              MOVE SATB-RAD-BEART-SVE    TO W-BEART                       
078400              IF SATB-RAD-KDBENHOM > 0                                    
078500                 MOVE SATB-RAD-KDBENHOM  TO SATB-KDHOM-WS                 
078600                 PERFORM CA-HAEMTA-RAETT-HOMONYM                          
078700                 MOVE BEART-WS           TO L2-BEART                      
078800              ELSE                                                        
078900                 MOVE 'S  '              TO W-IDSKYLT                     
079000                 PERFORM IMS-GU-BENA-ASEQ                                 
079100                 IF SEGMENT-FINNS                                         
079200                    MOVE IDSKYLT-WS      TO W-IDSKYLT                     
079300                    PERFORM IMS-GNP-BEN-TEXT-ASEQ                         
079400                    IF SEGMENT-FINNS                                      
079500                       MOVE BEN-TEXT-BEART TO L2-BEART                    
079600                    ELSE                                                  
079700                       MOVE SATB-RAD-BEART-SVE TO L2-BEART                
079800                    END-IF                                                
079900                 ELSE                                                     
080000                    MOVE SATB-RAD-BEART-SVE TO L2-BEART                   
080100                 END-IF                                                   
080200                 MOVE IDSKYLT-WS         TO W-IDSKYLT                     
080300              END-IF                                                      
080400           END-IF                                                         
080500        END-IF                                                            
080600     ELSE                                                                 
080700        MOVE SATB-RAD-IDRADNR      TO L1-IDRADNR                          
080800        MOVE SATB-RAD-REANTPSA     TO L1-REANTPSA                         
080900        MOVE SATB-RAD-IDLEVNR      TO L1-IDLEVNR                          
081000        MOVE SATB-RAD-BELEVART     TO L1-BELEVART                         
081100        IF IDSKYLT-WS = 'S  '                                             
081200          MOVE SATB-RAD-BEART-SVE    TO L1-BEART                          
081300        ELSE                                                              
081400          MOVE SATB-RAD-BEART-SVE    TO W-BEART                           
081500          PERFORM CA-HAEMTA-RAETT-HOMONYM                                 
081600          MOVE BEART-WS              TO L1-BEART                          
081700        END-IF                                                            
081800        MOVE SATB-RAD-KDISATS      TO L1-KDISATS                          
081900        MOVE SATB-RAD-IDSTRTYP     TO L1-IDSTRTYP                         
082000        MOVE SPAR-AAVV             TO L1-AAVV                             
082100        MOVE SATB-RAD-KDSORT       TO WS-KDSORT                           
082200        MOVE WS-KDSORT             TO L1-KDSORT                           
082300     END-IF                                                               
082400     PERFORM S92-SKRIV-UTRAD-ARTIKEL                                      
082500     MOVE STRUKTURNUMMER-SPAR TO W-IDARTNR                                
082600     .                                                                    
082700     EJECT                                                                
082800 LAES-KDSORT SECTION.                                                     
082900     SKIP2                                                                
083000     MOVE SATB-RAD-IDARTNR    TO W-IDARTNR                                
083100     PERFORM IMS-GU-ARTC01                                                
083200     IF SEGMENT-FINNS                                                     
083300        MOVE ART-KDSORT       TO WS-KDSORT                                
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 S90-SKRIV-HUVUDRUBRIKER SECTION.                                         
083800     SKIP2                                                                
083900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
084000                         W-IDPRTLST ALT-PCB   PRT-NYSIDA-RAD4             
084100                                              RUB1-RAD                    
084200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
084300                         W-IDPRTLST ALT-PCB   PRT-AFTER-2                 
084400                                              RUB2-RAD                    
084500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
084600                         W-IDPRTLST ALT-PCB   PRT-AFTER-2                 
084700                                              RUB3-RAD                    
084800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
084900                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
085000                                              RUB4A-RAD                   
085100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
085200                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
085300                                              RUB4B-RAD                   
085400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
085500                         W-IDPRTLST ALT-PCB   PRT-AFTER-2                 
085600                                              RUB5-RAD                    
085700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
085800                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
085900                                              DUMMY-RAD                   
086000     MOVE +10 TO RADRAEKNARE                                              
086100     .                                                                    
086200     EJECT                                                                
086300 S91-SKRIV-RADRUBRIK SECTION.                                             
086400                                                                          
086500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
086600                         W-IDPRTLST ALT-PCB   PRT-NYSIDA-RAD4             
086700                                              RUB5-RAD                    
086800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
086900                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
087000                                              DUMMY-RAD                   
087100     ADD +2 TO RADRAEKNARE                                                
087200     .                                                                    
087300 S92-SKRIV-UTRAD-ARTIKEL SECTION.                                         
087400                                                                          
087500     IF SIDBRYTNING                                                       
087600        MOVE ZERO        TO RADRAEKNARE                                   
087700        PERFORM S91-SKRIV-RADRUBRIK                                       
087800     END-IF                                                               
087900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
088000                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
088100                                              UTRAD                       
088200     MOVE SPACE TO UTRAD                                                  
088300     MOVE ZERO  TO SPAR-AAVV                                              
088400     ADD +1     TO RADRAEKNARE                                            
088500     .                                                                    
088600     SKIP2                                                                
088700 S93-SKRIV-UTRAD-NOTERING SECTION.                                        
088800                                                                          
088900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
089000                         W-IDPRTLST ALT-PCB   PRT-AFTER-1                 
089100                                              UTRAD                       
089200     MOVE SPACE TO UTRAD                                                  
089300     ADD +1     TO RADRAEKNARE                                            
089400     .                                                                    
089500     EJECT                                                                
089600******************************************************                    
089700*                  IMS SEKTIONER                     *                    
089800******************************************************                    
089900 IMS-GET-MSG SECTION.                                                     
090000                                                                          
090100     MOVE '  QC' TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
090300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600                                                                          
090700 IMS-GU-ARTC01 SECTION.                                                   
090800                                                                          
090900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
091000          DELIMITED BY SIZE INTO SSA1                                     
091100     MOVE '  GE' TO GODK-STATUSKODER                                      
091200     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-2 SSA1                        
091300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
092700     EJECT                                                                
092800 IMS-GU-ARTC11 SECTION.                                                   
092900                                                                          
093200     MOVE 'WLARTC11 ' TO SSA1                                             
093300     MOVE '  GE' TO GODK-STATUSKODER                                      
093400     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA-2 SSA1                       
093500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
093600     PERFORM IMS-STATUSKONTROLL                                           
093700     .                                                                    
094800     EJECT                                                                
094900 IMS-GU-SATB-ART        SECTION.                                          
095000                                                                          
095100     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
095200          DELIMITED BY SIZE INTO SSA1                                     
095300     MOVE '  GE' TO GODK-STATUSKODER                                      
095400     CALL CBLTDLI USING GU SATB-PCB IO-AREA-1 SSA1                        
095500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800                                                                          
095900 IMS-GNP-SATB-RAD       SECTION.                                          
096000                                                                          
096100     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
096200          DELIMITED BY SIZE INTO SSA1                                     
096300     STRING 'WLSATB11(WDJ111KY>=' W-KDSTRRAD-X                            
096400                                  W-IDRADNR-X ')'                         
096500          DELIMITED BY SIZE INTO SSA2                                     
096600     MOVE '  GE' TO GODK-STATUSKODER                                      
096700     CALL CBLTDLI USING GNP SATB-PCB IO-AREA-1 SSA1 SSA2                  
096800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-GNP-SATB-NOT-OKVAL SECTION.                                          
097300                                                                          
097400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
097500          DELIMITED BY SIZE INTO SSA1                                     
097600     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
097700                                  W-IDRADNR-X ')'                         
097800          DELIMITED BY SIZE INTO SSA2                                     
097900     MOVE 'WLSATB22 '         TO SSA3                                     
098000     MOVE '  GE' TO GODK-STATUSKODER                                      
098100     CALL CBLTDLI USING GNP SATB-PCB IO-AREA-1 SSA1 SSA2 SSA3             
098200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
098300     PERFORM IMS-STATUSKONTROLL                                           
098400     .                                                                    
098500     EJECT                                                                
098600 IMS-GU-BENA-ASEQ SECTION.                                                
098700                                                                          
098800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
098900                                  W-BEART-X ')'                           
099000          DELIMITED BY SIZE INTO SSA1                                     
099100     MOVE '  GE' TO GODK-STATUSKODER                                      
099200     CALL CBLTDLI USING GU BENAA-PCB IO-AREA-2 SSA1                       
099300     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
099400     PERFORM IMS-STATUSKONTROLL                                           
099500     .                                                                    
099600 IMS-GN-BENA-ASEQ  SECTION.                                               
099700                                                                          
099800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
099900                                  W-BEART-X ')'                           
100000          DELIMITED BY SIZE INTO SSA1                                     
100100     MOVE '  GE' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GN BENAA-PCB IO-AREA-2 SSA1                       
100300     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600 IMS-GNP-BEN-TEXT-ASEQ SECTION.                                           
100700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
100800            DELIMITED BY SIZE INTO SSA1                                   
100900     MOVE '  GE' TO GODK-STATUSKODER                                      
101000     CALL CBLTDLI USING GNP BENAA-PCB IO-AREA-2 SSA1                      
101100     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     SKIP2                                                                
101500 IMS-GNP-BENA-HOM SECTION.                                                
101600     MOVE 'WLBENA13 ' TO SSA1                                             
101700     MOVE '  GE' TO GODK-STATUSKODER                                      
101800     CALL CBLTDLI USING GNP BENAA-PCB IO-AREA-2 SSA1                      
101900     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
102000     PERFORM IMS-STATUSKONTROLL                                           
102100     .                                                                    
102200     EJECT                                                                
102300 IMS-GU-BENA-TEXT-BSEQ  SECTION.                                          
102400                                                                          
102500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
102600          DELIMITED BY SIZE INTO SSA1                                     
102700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
102800          DELIMITED BY SIZE INTO SSA2                                     
102900     MOVE '  GE' TO GODK-STATUSKODER                                      
103000     CALL CBLTDLI USING GU BENAB-PCB IO-AREA-2 SSA1 SSA2                  
103100     MOVE BENAB-STATUS-CODE TO STATUS-WS                                  
103200     PERFORM IMS-STATUSKONTROLL                                           
103300     .                                                                    
103400                                                                          
103500 IMS-STATUSKONTROLL     SECTION.                                          
103600                                                                          
103700     SET STATUS-IX TO 1                                                   
103800     SEARCH GODK-STATUS                                                   
103900       AT END CALL FELLOG                                                 
104000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
104100     END-SEARCH                                                           
104200     .                                                                    
104210     EJECT                                                                
104300*    -COPY WY2000P1                                                       
