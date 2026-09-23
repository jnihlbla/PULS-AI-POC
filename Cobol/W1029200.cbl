000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1029200.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   OKT 1990.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION.                                                            
001000*        MPP-PRINTPROGRAM FÖR W10214, PROG-TO-PROG-SWITCH.                
001100*        LISTA STRUKTURHISTORIK.                                          
001200*                                                                         
001300*    SUBPROGRAM:                                                          
001400*        W006PRS1 - SKÖTER ALL SKRIVNING MOT IMS-PRINTER.                 
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W1T292U                                             
001800*        MID:         W1I21401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002701*    -COPY WY2000W1                                                       
002702     SKIP3                                                                
002703*    -COPY WY2000W3                                                       
002710     SKIP3                                                                
002800 77  IDPGM                   PIC X(8)    VALUE 'W1029200'.                
002900                                                                          
003000 01  DYNAMISKA-SUBPROGRAM.                                                
003100    03 WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
003200    03 W006PRS1              PIC X(8)    VALUE 'W006PRS1'.                
003300    03 CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003400    03 FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
003500                                                                          
003600 77  JA                      PIC X       VALUE 'J'.                       
003700 77  NEJ                     PIC X       VALUE 'N'.                       
003800 77  RAETT-TEXT              PIC X       VALUE 'N'.                       
003900 77  GAELLANDE-STRUKTURRAD   PIC X       VALUE 'J'.                       
004000 77  RUB-IX                  PIC S9(9)   VALUE +1   COMP SYNC.            
004100 77  MAX-RADER               PIC S9(3)   VALUE +37.                       
004200 77  RADRAEKNARE             PIC S9(5)   VALUE ZERO.                      
004300 77  SIDRAEKNARE             PIC S9(3)   VALUE +1.                        
004400 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004500 77  STRUKTURNUMMER-SPAR     PIC S9(9)   VALUE ZERO COMP-3.               
004600 77  SATB-KDHOM-WS           PIC S9      VALUE ZERO COMP-3.               
004700 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
004800 77  BEART-WS                PIC X(25)   VALUE SPACE.                     
004900 77  KDBENHOM-WS             PIC 9       VALUE ZERO.                      
005000 77  DUMMY-RAD               PIC X(132)  VALUE SPACE.                     
005200 77  SATS-HIST-RA            PIC X(3)    VALUE '232'.                     
005300 77  SATS-HIST-RB            PIC X(3)    VALUE '233'.                     
005600 77  SATS-HIST-BERPV         PIC X(3)    VALUE '236'.                     
005700 77  SATS-HIST-CARP          PIC X(3)    VALUE '231'.                     
005800                                                                          
005900                                                                          
006000                                                                          
006100 77  INDATA-SW               PIC X       VALUE 'J'.                       
006200   88  INDATA-OK                         VALUE 'J'.                       
006300   88  INDATA-FEL                        VALUE 'N'.                       
006400                                                                          
006500 77 SKRIV-SIDRUBR-SW         PIC X       VALUE 'J'.                       
006600     88 SKRIV-SIDRUBRIK                  VALUE 'J'.                       
006700                                                                          
006800 77  W-KDTRTYP               PIC X       VALUE SPACE.                     
006900   88  PRINT-OK                          VALUE 'X'.                       
007000                                                                          
007100 77  W-KDMFSFOR              PIC X       VALUE SPACE.                     
007200   88  SWEDISH-TEXT                      VALUE '1'.                       
007300   88  ENGLISH-TEXT                      VALUE '2'.                       
007400                                                                          
007500 77  W-IDPRTLST              PIC X(8)    VALUE SPACE.                     
007600                                                                          
007700 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
007800   88  GODK-MID                          VALUE '1214'.                    
007900                                                                          
008000 77  STOPP-DATUM             PIC 9(5)    VALUE ZERO.                      
008100                                                                          
008200 01  SPAR-FAELT              PIC X(30)   VALUE SPACE.                     
008300*                                                                         
008400 01  FILLER REDEFINES SPAR-FAELT.                                         
008500   03 SPAR-ARTIKELNR         PIC X(9).                                    
008600   03 FILLER                 PIC X(21).                                   
008700                                                                          
008800 01  VECKA-TOM               PIC X(4)    VALUE SPACE.                     
008900 01  VECKA-FOM               PIC X(4)    VALUE SPACE.                     
009000                                                                          
009100 01  SPAR-START-DATUM        PIC 9(4).                                    
009200*                                                                         
009300 01  FILLER REDEFINES SPAR-START-DATUM.                                   
009400   03 SPAR-START-AA          PIC 9(2).                                    
009500   03 SPAR-START-VV          PIC 9(2).                                    
009600                                                                          
009700 01  SPAR-STOPP-DATUM        PIC 9(4).                                    
009800*                                                                         
009900 01  FILLER REDEFINES SPAR-STOPP-DATUM.                                   
010000   03 SPAR-STOPP-AA          PIC 9(2).                                    
010100   03 SPAR-STOPP-VV          PIC 9(2).                                    
010200                                                                          
010300 01  VECKA-START             PIC 9(4).                                    
010400*                                                                         
010500 01  FILLER REDEFINES VECKA-START.                                        
010600   03 VECKA-START-AA         PIC 9(2).                                    
010700   03 VECKA-START-VV         PIC 9(2).                                    
010800                                                                          
010900 01  SPAR-AAVV               PIC 9(4).                                    
011000 01  FILLER REDEFINES SPAR-AAVV.                                          
011100   03 SPAR-AA                PIC 99.                                      
011200   03 SPAR-VV                PIC 99.                                      
011300                                                                          
011400 01  FILLER                  PIC X(16)   VALUE 'DAGENS-DATUM'.            
011500 01  DAGENS-DATUM            PIC 9(6).                                    
011600 01  FILLER REDEFINES DAGENS-DATUM.                                       
011700   03  DAGENS-DATUM-AR       PIC 99.                                      
011800   03  DAGENS-DATUM-MANAD    PIC 99.                                      
011900   03  DAGENS-DATUM-DAG      PIC 99.                                      
012000                                                                          
012100 01  FILLER                  PIC X(16)   VALUE 'DAGENS-TID'.              
012200 01  DAGENS-TID              PIC 9(8).                                    
012300 01  FILLER REDEFINES DAGENS-TID.                                         
012400   03  DAGENS-TID-HH         PIC 99.                                      
012500   03  DAGENS-TID-MM         PIC 99.                                      
012600   03  FILLER                PIC 9(4).                                    
012700                                                                          
012800 77  STOPP-DATUM-SW          PIC X       VALUE 'J'.                       
012900   88  STOPP-DATUM-OK                    VALUE 'J'.                       
013000   88  STOPP-DATUM-FEL                   VALUE 'N'.                       
013100                                                                          
013200 77  START-DATUM-SW          PIC X       VALUE 'J'.                       
013300   88  START-DATUM-OK                    VALUE 'J'.                       
013400   88  START-DATUM-FEL                   VALUE 'N'.                       
013500                                                                          
013600 77  BEHANDLING-SW           PIC X       VALUE 'J'.                       
013700   88  BEHANDLING-OK                     VALUE 'J'.                       
013800                                                                          
013900 77  STATUS-SW               PIC X       VALUE 'J'.                       
014000   88  STATUS-OK                         VALUE 'J'.                       
014100                                                                          
014200     EJECT                                                                
014300******************************************************************        
014400*        LISTPOSTENS RUBRIKTEXTER, PÅ VARJE SPRÅK                *        
014500******************************************************************        
014600 01  RUBRIKTEXTER.                                                        
014700   03 RUB-1-1.                                                            
014800      05 FILLER           PIC X(27)  VALUE                                
014900                                   'STRUKTURHISTORIK           '.         
015000      05 FILLER           PIC X(27)  VALUE                                
015100                                   'NEW AND OLD STRUCTURE LINES'.         
015200   03 FILLER REDEFINES RUB-1-1.                                           
015300      05 RUB1-1 OCCURS 2  PIC X(27).                                      
015400   03 RUB-1-2.                                                            
015500      05 FILLER           PIC X(07)  VALUE 'DATUM'.                       
015600      05 FILLER           PIC X(07)  VALUE ' DATE'.                       
015700   03 FILLER REDEFINES RUB-1-2.                                           
015800      05 RUB1-2 OCCURS 2  PIC X(07).                                      
015900   03 RUB-1-3.                                                            
016000      05 FILLER           PIC X(06)  VALUE ' TID'.                        
016100      05 FILLER           PIC X(06)  VALUE 'TIME'.                        
016200   03 FILLER REDEFINES RUB-1-3.                                           
016300      05 RUB1-3 OCCURS 2  PIC X(06).                                      
016400*                                                                         
016500   03 RUB-2-1.                                                            
016600      05 FILLER           PIC X(16)  VALUE 'STRUKTURNUMMER'.              
016700      05 FILLER           PIC X(16)  VALUE 'PART NUMBER   '.              
016800   03 FILLER REDEFINES RUB-2-1.                                           
016900      05 RUB2-1 OCCURS 2  PIC X(16).                                      
017000   03 RUB-2-2.                                                            
017100      05 FILLER           PIC X(10)  VALUE '  SPRÅK '.                    
017200      05 FILLER           PIC X(10)  VALUE 'LANGUAGE'.                    
017300   03 FILLER REDEFINES RUB-2-2.                                           
017400      05 RUB2-2 OCCURS 2  PIC X(10).                                      
017500   03 RUB-2-3.                                                            
017600      05 FILLER           PIC X(12)  VALUE 'VECKA FOM'.                   
017700      05 FILLER           PIC X(12)  VALUE 'WEEK FROM'.                   
017800   03 FILLER REDEFINES RUB-2-3.                                           
017900      05 RUB2-3 OCCURS 2  PIC X(12).                                      
018000   03 RUB-2-4.                                                            
018100      05 FILLER           PIC X(05)  VALUE '  TOM'.                       
018200      05 FILLER           PIC X(05)  VALUE 'UP TO'.                       
018300   03 FILLER REDEFINES RUB-2-4.                                           
018400      05 RUB2-4 OCCURS 2  PIC X(05).                                      
018500*                                                                         
018600   03 RUB-3-1.                                                            
018700      05 FILLER           PIC X(12)  VALUE 'BENÄMNING  '.                 
018800      05 FILLER           PIC X(12)  VALUE 'DESCRIPTION'.                 
018900   03 FILLER REDEFINES RUB-3-1.                                           
019000      05 RUB3-1 OCCURS 2  PIC X(12).                                      
019100   03 RUB-3-2.                                                            
019200      05 FILLER           PIC X(08)  VALUE 'PRODSL'.                      
019300      05 FILLER           PIC X(08)  VALUE 'PRODGR'.                      
019400   03 FILLER REDEFINES RUB-3-2.                                           
019500      05 RUB3-2 OCCURS 2  PIC X(08).                                      
019600   03 RUB-3-3.                                                            
019700      05 FILLER           PIC X(08)  VALUE 'FUNKGRP'.                     
019800      05 FILLER           PIC X(08)  VALUE 'FUNCGR '.                     
019900   03 FILLER REDEFINES RUB-3-3.                                           
020000      05 RUB3-3 OCCURS 2  PIC X(08).                                      
020100   03 RUB-3-4.                                                            
020200      05 FILLER           PIC X(05)  VALUE 'LEVNR'.                       
020300      05 FILLER           PIC X(05)  VALUE 'SUPPL'.                       
020400   03 FILLER REDEFINES RUB-3-4.                                           
020500      05 RUB3-4 OCCURS 2  PIC X(05).                                      
020600   03 RUB-3-5.                                                            
020700      05 FILLER           PIC X(9)   VALUE ' ERS.KOD'.                    
020800      05 FILLER           PIC X(9)   VALUE 'SUP.CODE'.                    
020900   03 FILLER REDEFINES RUB-3-5.                                           
021000      05 RUB3-5 OCCURS 2  PIC X(9).                                       
021100   03 RUB-3-6.                                                            
021200      05 FILLER           PIC X(15)  VALUE '   STRUKTURTYP'.              
021300      05 FILLER           PIC X(15)  VALUE 'TYPE OF STRUCT'.              
021400   03 FILLER REDEFINES RUB-3-6.                                           
021500      05 RUB3-6 OCCURS 2  PIC X(15).                                      
021600*                                                                         
021700   03 RUB-4-1.                                                            
021800      05 FILLER           PIC X(11)   VALUE 'STRUKTURNOT'.                
021900      05 FILLER           PIC X(11)   VALUE 'STRUCT NOTE'.                
022000   03 FILLER REDEFINES RUB-4-1.                                           
022100      05 RUB4-1 OCCURS 2  PIC X(11).                                      
022200*                                                                         
022300   03 RUB-5-1.                                                            
022400      05 FILLER           PIC X(06)   VALUE ' RAD'.                       
022500      05 FILLER           PIC X(06)   VALUE 'LINE'.                       
022600   03 FILLER REDEFINES RUB-5-1.                                           
022700      05 RUB5-1 OCCURS 2  PIC X(06).                                      
022800   03 RUB-5-2.                                                            
022900      05 FILLER           PIC X(14)  VALUE 'ANTAL  LEVNR'.                
023000      05 FILLER           PIC X(14)  VALUE 'QUANT  SUPPL'.                
023100   03 FILLER REDEFINES RUB-5-2.                                           
023200      05 RUB5-2 OCCURS 2  PIC X(14).                                      
023300   03 RUB-5-3.                                                            
023400      05 FILLER           PIC X(31)  VALUE 'ARTNR/LEVBET     '.           
023500      05 FILLER           PIC X(31)  VALUE 'PART NO/REFERENCE'.           
023600   03 FILLER REDEFINES RUB-5-3.                                           
023700      05 RUB5-3 OCCURS 2  PIC X(31).                                      
023800   03 RUB-5-4.                                                            
023900      05 FILLER           PIC X(16)  VALUE 'BENÄMNING  '.                 
024000      05 FILLER           PIC X(16)  VALUE 'DESCRIPTION'.                 
024100   03 FILLER REDEFINES RUB-5-4.                                           
024200      05 RUB5-4 OCCURS 2  PIC X(16).                                      
024300   03 RUB-5-5.                                                            
024400      05 FILLER           PIC X(18)  VALUE 'ÄO-TILLK   VECKA'.            
024500      05 FILLER           PIC X(18)  VALUE 'ADD. DCN    WEEK'.            
024600   03 FILLER REDEFINES RUB-5-5.                                           
024700      05 RUB5-5 OCCURS 2  PIC X(18).                                      
024800   03 RUB-5-6.                                                            
024900      05 FILLER           PIC X(18)  VALUE 'ÄO-UTGÅR   VECKA'.            
025000      05 FILLER           PIC X(18)  VALUE 'OUTG. DCN   WEEK'.            
025100   03 FILLER REDEFINES RUB-5-6.                                           
025200      05 RUB5-6 OCCURS 2  PIC X(18).                                      
025300   03 RUB-5-7.                                                            
025400      05 FILLER           PIC X(03)  VALUE 'ST'.                          
025500      05 FILLER           PIC X(03)  VALUE 'TS'.                          
025600   03 FILLER REDEFINES RUB-5-7.                                           
025700      05 RUB5-7 OCCURS 2  PIC X(03).                                      
025800   03 RUB-5-8.                                                            
025900      05 FILLER           PIC X(04)  VALUE 'SIDA'.                        
026000      05 FILLER           PIC X(04)  VALUE 'PAGE'.                        
026100   03 FILLER REDEFINES RUB-5-8.                                           
026200      05 RUB5-8 OCCURS 2  PIC X(04).                                      
026300     EJECT                                                                
026400******************************************************************        
026500*        LISTPOST AREA, RUBRIKER                                 *        
026600******************************************************************        
026700 01  FILLER               PIC X(16)  VALUE '        RUB1-RAD'.            
026800 01  RUB1-RAD.                                                            
026900   03 FILLER              PIC X(2)   VALUE  SPACE.                        
027000   03         FILLER      PIC X(9)   VALUE '1 2 1 4'.                     
027100   03 RUB1-T1             PIC X(27).                                      
027200   03         FILLER      PIC X(48)  VALUE  SPACE.                        
027300   03 RUB1-T2             PIC X(07).                                      
027400   03 RUB1-AA             PIC 99.                                         
027500   03         FILLER      PIC X      VALUE '/'.                           
027600   03 RUB1-MM             PIC 99.                                         
027700   03         FILLER      PIC X      VALUE '/'.                           
027800   03 RUB1-DD             PIC 99.                                         
027900   03         FILLER      PIC X(04)  VALUE  SPACE.                        
028000   03 RUB1-T3             PIC X(06).                                      
028100   03 RUB1-HH             PIC 99.                                         
028200   03         FILLER      PIC X      VALUE ':'.                           
028300   03 RUB1-MIN            PIC 99.                                         
028310   03         FILLER      PIC X(16)  VALUE  SPACE.                        
028400     EJECT                                                                
028500****************************************************************          
028600 01  FILLER               PIC X(16)  VALUE '        RUB2-RAD'.            
028700 01  RUB2-RAD.                                                            
028800   03 FILLER              PIC X(2)   VALUE  SPACE.                        
028900   03 RUB2-T1             PIC X(16).                                      
029000   03 RUB2-IDARTNR        PIC Z(8)9  VALUE  ZERO.                         
029100   03         FILLER      PIC X(12)  VALUE  SPACE.                        
029200   03 RUB2-T2             PIC X(10).                                      
029300   03 RUB2-IDSKYLT        PIC X(03).                                      
029400   03         FILLER      PIC X(22)  VALUE  SPACE.                        
029500   03 RUB2-T3             PIC X(11).                                      
029600   03 RUB2-VECKA-FOM      PIC Z(04).                                      
029700   03         FILLER      PIC X(04)  VALUE  SPACE.                        
029800   03 RUB2-T4             PIC X(05).                                      
029900   03         FILLER      PIC X(02)  VALUE  SPACE.                        
030000   03 RUB2-VECKA-TOM      PIC Z(04).                                      
030100   03         FILLER      PIC X(12)  VALUE  SPACE.                        
030110   03         FILLER      PIC X(16)  VALUE  SPACE.                        
030200                                                                          
030300****************************************************************          
030400 01  FILLER               PIC X(16)  VALUE '        RUB3-RAD'.            
030500 01  RUB3-RAD.                                                            
030600   03 FILLER              PIC X(2)   VALUE  SPACE.                        
030700   03 RUB3-T1             PIC X(12).                                      
030800   03 RUB3-BEART          PIC X(25).                                      
030900   03         FILLER      PIC X(02)  VALUE  SPACE.                        
031000   03 RUB3-T2             PIC X(08).                                      
031100   03 RUB3-KDPRODSL       PIC Z9     VALUE  ZERO.                         
031200   03         FILLER      PIC X(03)  VALUE  SPACE.                        
031300   03 RUB3-T3             PIC X(08).                                      
031400   03 RUB3-IDFKNGRP       PIC Z(4)9  VALUE  ZERO.                         
031500   03         FILLER      PIC X(03)  VALUE  SPACE.                        
031600   03 RUB3-T4             PIC X(07).                                      
031610   03         FILLER      PIC X(01)  VALUE  SPACE.                        
031700   03 RUB3-IDLEVNR        PIC X(5).                                       
031800   03         FILLER      PIC X(02)  VALUE  SPACE.                        
031900   03 RUB3-T5             PIC X(9).                                       
032000   03 RUB3-KDERS          PIC 9      VALUE  ZERO.                         
032100   03         FILLER      PIC X(02)  VALUE  SPACE.                        
032200   03 RUB3-T6             PIC X(15).                                      
032300   03 RUB3-IDSTRTYP       PIC X(4).                                       
032310   03         FILLER      PIC X(16)  VALUE  SPACE.                        
032400                                                                          
032500****************************************************************          
032600 01  FILLER               PIC X(16)  VALUE '       RUB4A-RAD'.            
032700 01  RUB4A-RAD.                                                           
032800   03 FILLER              PIC X(2)   VALUE  SPACE.                        
032900   03 RUB4-T1             PIC X(11).                                      
033000   03         FILLER      PIC X(01)  VALUE  SPACE.                        
033100   03 RUB4A-TESTRNOT      PIC X(70)  VALUE  SPACE.                        
033200   03         FILLER      PIC X(32)  VALUE  SPACE.                        
033210   03         FILLER      PIC X(16)  VALUE  SPACE.                        
033300                                                                          
033400****************************************************************          
033500 01  FILLER               PIC X(16)  VALUE '       RUB4B-RAD'.            
033600 01  RUB4B-RAD.                                                           
033700   03 FILLER              PIC X(2)   VALUE  SPACE.                        
033800   03         FILLER      PIC X(12)  VALUE  SPACE.                        
033900   03 RUB4B-TESTRNOT      PIC X(70)  VALUE  SPACE.                        
034000   03         FILLER      PIC X(32)  VALUE  SPACE.                        
034010   03         FILLER      PIC X(16)  VALUE  SPACE.                        
034100                                                                          
034200****************************************************************          
034300 01  FILLER               PIC X(16)  VALUE '        RUB5-RAD'.            
034400 01  RUB5-RAD.                                                            
034500   03 FILLER              PIC X(4)   VALUE  SPACE.                        
034600   03 RUB5-T1             PIC X(6).                                       
034700   03 RUB5-T2             PIC X(14).                                      
034800   03 RUB5-T3             PIC X(31).                                      
034900   03 RUB5-T4             PIC X(16).                                      
035000   03 RUB5-T5             PIC X(18).                                      
035100   03 RUB5-T6             PIC X(18).                                      
035200   03 RUB5-T7             PIC X(03).                                      
035300   03 RUB5-T8             PIC X(04).                                      
035400   03 RUB5-SIDA           PIC Z9     VALUE  ZERO.                         
035410   03 FILLER              PIC X(16)  VALUE  SPACE.                        
035500     EJECT                                                                
035600******************************************************************        
035700*        LISTPOST AREA, UTRADER                                  *        
035800******************************************************************        
035900 01  FILLER               PIC X(16)  VALUE '         UTRADER'.            
036000 01  UTRAD                PIC X(132)  VALUE SPACE.                        
036100 01  FILLER REDEFINES UTRAD.                                              
036200   03         FILLER      PIC X(3).                                       
036300   03 L1-IDRADNR          PIC Z(4)9.                                      
036400   03         FILLER      PIC X(1).                                       
036500   03 L1-REANTPSA         PIC Z9.9(3).                                    
036600   03         FILLER      PIC X(2).                                       
036700   03 L1-IDLEVNR          PIC X(5).                                       
036800   03         FILLER      PIC X(2).                                       
036900   03 L1-IDARTNR-BELEVART PIC X(30).                                      
037000   03         FILLER      PIC X(1).                                       
037100   03 L1-BEART            PIC X(15).                                      
037200   03         FILLER      PIC X(2).                                       
037300   03 L1-IDAO-STA         PIC X(10).                                      
037400   03         FILLER      PIC X(2).                                       
037500   03 L1-TISTADAT-VECKA   PIC Z(4).                                       
037600   03         FILLER      PIC X(2).                                       
037700   03 L1-IDAO-STO         PIC X(10).                                      
037800   03         FILLER      PIC X(2).                                       
037900   03 L1-TISTODAT-VECKA   PIC Z(4).                                       
038000   03         FILLER      PIC X(2).                                       
038100   03 L1-IDSTRTYP         PIC X.                                          
038200   03         FILLER      PIC X(7).                                       
038210   03         FILLER      PIC X(16).                                      
038300     EJECT                                                                
038400******************************************************************        
038500*        PARAMETERAREA FÖR DATUMKONTROLL                         *        
038600******************************************************************        
038700 01  FILLER                  PIC X(16)   VALUE 'WDATAREA'.                
038800*01  -COPY WDATAREA                                                       
038900     EJECT                                                                
039000******************************************************************        
039100*        PRINTER STYRTECKEN                                      *        
039200******************************************************************        
039300 01  FILLER                  PIC X(16)   VALUE 'PRT-W006PRAR'.            
039400*01        -COPY W006PRAR                                                 
039500     EJECT                                                                
039600******************************************************************        
039700*        PROGRAM-TO-PROGRAM                                      *        
039800******************************************************************        
039900 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
040000*01  MID   -COPY W1I21401                                                 
040100 01  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.                
040200*01  -COPY WMSGAREA                                                       
040300     EJECT                                                                
040400******************************************************************        
040500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                       *        
040600******************************************************************        
040700 01  IMS-WS.                                                              
040800   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
040900     SKIP3                                                                
041000 01  NYCKLAR-TILL-DLI.                                                    
041100   03  W-IDARTNR-X.                                                       
041200     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
041300                                                                          
041400   03  W-IDARTNR2-X.                                                      
041500     05  W-IDARTNR2          PIC S9(9)   VALUE ZERO  COMP-3.              
041600                                                                          
041700   03  W-IDSKYLT-X.                                                       
041800     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
041900                                                                          
042000   03  W-BEART-X.                                                         
042100       05 W-BEART            PIC X(25)   VALUE SPACE.                     
042500     SKIP3                                                                
042600*                        **** STATUS-KOD FRÅN IMS                         
042700   03  STATUS-WS             PIC XX.                                      
042800     88  SEGMENT-FINNS                   VALUE '  '.                      
042900     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
043000     SKIP3                                                                
043100   03  GODK-STATUSKODER.                                                  
043200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
043300     SKIP3                                                                
043400 01    SSA1                  PIC X(64).                                   
043500 01    SSA2                  PIC X(64).                                   
043600 01    SSA3                  PIC X(64).                                   
043700     EJECT                                                                
043800*                            IMS FUNKTIONSKODER                           
043900*01    -COPY W0003                                                        
044000     EJECT                                                                
044100*                            DLI INPUT-OUTPUT AREA                        
044200 01  FILLER                  PIC X(16)   VALUE 'IO-AREA'.                 
044300 01  DLI-IO-AREA.                                                         
044400   03  IO-AREA               PIC X(928)  VALUE SPACE.                     
044500     SKIP3                                                                
044800*  03  WLARTC01  -COPY WDK601                -RED IO-AREA.                
044900     EJECT                                                                
045000*  03  WLARTC11  -COPY WDK611                -RED IO-AREA.                
045100     EJECT                                                                
045200*  03  WLBENA01  -COPY WDD301  -PRE BENA01-  -RED IO-AREA.                
045300     EJECT                                                                
045400*  03  WLBENA11  -COPY WDD311  -PRE BENA11-  -RED IO-AREA.                
045500     EJECT                                                                
045600                                                                          
045700*                            DLI INPUT-OUTPUT AREA 2                      
045800 01  FILLER                  PIC X(16)   VALUE 'IO-AREA2'.                
045900 01  DLI-IO-AREA2.                                                        
046000   03  IO-AREA2              PIC X(250)  VALUE SPACE.                     
046100*  03  WLSATB01  -COPY WDJ101  -PRE SATB01-  -RED IO-AREA2.               
046200     EJECT                                                                
046300*  03  WLSATB11  -COPY WDJ111  -PRE SATB11-  -RED IO-AREA2.               
046400     EJECT                                                                
046500 LINKAGE SECTION.                                                         
046600*01  -COPY W0009     -PRE MSG-                                            
046700                                                                          
046800*01  -COPY W0009     -PRE ALT-                                            
046900     EJECT                                                                
047300*01  -COPY W0008     -PRE ARTC-                                           
047400     05  FILLER              PIC X.                                       
047500                                                                          
047600*01  -COPY W0008     -PRE SATB-                                           
047700     05  FILLER              PIC X.                                       
047800     EJECT                                                                
047900*01  -COPY W0008     -PRE BENA-A-                                         
048000     05  FILLER              PIC X.                                       
048100                                                                          
048200*01  -COPY W0008     -PRE BENA-B-                                         
048300     05  FILLER              PIC X.                                       
048400     EJECT                                                                
048500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ARTC-PCB                       
048600                             SATB-PCB BENA-A-PCB BENA-B-PCB.              
048610 MAIN SECTION.                                                            
048700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ARTC-PCB                       
048800                             SATB-PCB BENA-A-PCB BENA-B-PCB.              
048900     PERFORM IMS-GET-MSG                                                  
049000     IF SEGMENT-FINNS                                                     
049100       PERFORM A-INITIERA                                                 
049200       PERFORM B-BEHANDLA-INDATA                                          
049300       IF INDATA-OK AND PRINT-OK                                          
049400         PERFORM C-SKRIV-LISTA                                            
049500       END-IF                                                             
049600     END-IF                                                               
049700                                                                          
049800     MOVE ZERO TO RETURN-CODE                                             
049900     GOBACK                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 A-INITIERA SECTION.                                                      
050300                                                                          
050400     IF MSG-DUBBLA-TRANSKODER                                             
050500        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I21401                
050600        MOVE MSG-IDTRANS-2                 TO W-IDTRANS                   
050700        MOVE MSG-KDMFSFOR-2                TO W-KDMFSFOR                  
050800     ELSE                                                                 
050900        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I21401                
051000        MOVE MSG-IDTRANS-1                 TO W-IDTRANS                   
051100        MOVE MSG-KDMFSFOR-1                TO W-KDMFSFOR                  
051200     END-IF                                                               
051300     MOVE MSG-KDTRTYP                      TO W-KDTRTYP                   
051400                                                                          
051500     MOVE LOW-VALUE  TO MSG-AREA                                          
051600                                                                          
051700     IF NOT GODK-MID                                                      
051800       MOVE SPACE TO W-KDTRTYP                                            
051900     END-IF                                                               
052000                                                                          
052100     IF ENGLISH-TEXT                                                      
052200        MOVE +2 TO  RUB-IX                                                
052300     ELSE                                                                 
052400        MOVE +1 TO  RUB-IX                                                
052500     END-IF                                                               
052600                                                                          
052700     ACCEPT DAGENS-DATUM FROM DATE                                        
052800     ACCEPT DAGENS-TID   FROM TIME                                        
052900                                                                          
053000     PERFORM AA-INIT-RUBRIKTEXTER                                         
053100     .                                                                    
053200     EJECT                                                                
053300 AA-INIT-RUBRIKTEXTER  SECTION.                                           
053400     SKIP2                                                                
053500     IF MID-IDSKYLT-UT = 'S  '                                            
053600        CONTINUE                                                          
053700     ELSE                                                                 
053800        MOVE +2  TO RUB-IX                                                
053900     END-IF                                                               
054000     MOVE RUB1-1(RUB-IX) TO RUB1-T1                                       
054100     MOVE RUB1-2(RUB-IX) TO RUB1-T2                                       
054200     MOVE RUB1-3(RUB-IX) TO RUB1-T3                                       
054300*                                                                         
054400     MOVE RUB2-1(RUB-IX) TO RUB2-T1                                       
054500     MOVE RUB2-2(RUB-IX) TO RUB2-T2                                       
054600     MOVE RUB2-3(RUB-IX) TO RUB2-T3                                       
054700     MOVE RUB2-4(RUB-IX) TO RUB2-T4                                       
054800*                                                                         
054900     MOVE RUB3-1(RUB-IX) TO RUB3-T1                                       
055000     MOVE RUB3-2(RUB-IX) TO RUB3-T2                                       
055100     MOVE RUB3-3(RUB-IX) TO RUB3-T3                                       
055200     MOVE RUB3-4(RUB-IX) TO RUB3-T4                                       
055300     MOVE RUB3-5(RUB-IX) TO RUB3-T5                                       
055400     MOVE RUB3-6(RUB-IX) TO RUB3-T6                                       
055500*                                                                         
055600     MOVE RUB4-1(RUB-IX) TO RUB4-T1                                       
055700*                                                                         
055800     MOVE RUB5-1(RUB-IX) TO RUB5-T1                                       
055900     MOVE RUB5-2(RUB-IX) TO RUB5-T2                                       
056000     MOVE RUB5-3(RUB-IX) TO RUB5-T3                                       
056100     MOVE RUB5-4(RUB-IX) TO RUB5-T4                                       
056200     MOVE RUB5-5(RUB-IX) TO RUB5-T5                                       
056300     MOVE RUB5-6(RUB-IX) TO RUB5-T6                                       
056400     MOVE RUB5-7(RUB-IX) TO RUB5-T7                                       
056500     MOVE RUB5-8(RUB-IX) TO RUB5-T8                                       
056600     .                                                                    
056700     EJECT                                                                
056800 B-BEHANDLA-INDATA SECTION.                                               
056900                                                                          
057000     MOVE JA TO INDATA-SW                                                 
057100                                                                          
057200     MOVE MID-IDARTNR-UT TO IDARTNR-WS                                    
057300     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
057400                                                                          
057500     MOVE MID-IDSKYLT-UT TO IDSKYLT-WS                                    
057600                                                                          
057700     MOVE MID-VECKA-FOM-UT TO VECKA-FOM                                   
057800     INSPECT VECKA-FOM REPLACING LEADING SPACE BY ZERO                    
057900                                                                          
058000     MOVE MID-VECKA-TOM-UT TO VECKA-TOM                                   
058100     INSPECT VECKA-TOM REPLACING LEADING SPACE BY ZERO                    
058200                                                                          
058300     IF VECKA-FOM = ZERO                                                  
058400       MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                 
058500       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
058600       CALL WDATKONV USING DAT-KDDATFORM                                  
058700                           DAT-I-TIDATUM                                  
058800                           DAT-O-TIDATUM                                  
058900                           DAT-KDSVAR                                     
059000       IF DAT-KDSVAR-OK                                                   
059100         MOVE DAT-TIAA-VECKA TO VECKA-START-AA                            
059200         MOVE DAT-TIVV       TO VECKA-START-VV                            
059300       END-IF                                                             
059400       MOVE VECKA-START TO VECKA-FOM                                      
059500     END-IF                                                               
059600                                                                          
059700     IF VECKA-TOM = ZERO                                                  
059800       MOVE 9999  TO VECKA-TOM                                            
059900     END-IF                                                               
060000                                                                          
060100     IF (IDARTNR-WS NUMERIC) AND                                          
060200        (VECKA-FOM NUMERIC)  AND                                          
060300        (VECKA-TOM NUMERIC)                                               
060400       CONTINUE                                                           
060500     ELSE                                                                 
060600       MOVE NEJ TO INDATA-SW                                              
060700     END-IF                                                               
060800                                                                          
060900**** BESTÄM UTSKRIFTSADRESSEN *******************************             
061000     EVALUATE MID-KDPRTVAL                                                
061300        WHEN 'A' MOVE SATS-HIST-RA     TO W-IDPRTLST                      
061400*                                      PACKNINGEN                         
061500        WHEN 'B' MOVE SATS-HIST-RB     TO W-IDPRTLST                      
061600*                                      FÖRPACKNINGEN                      
062100        WHEN 'C' MOVE SATS-HIST-BERPV  TO W-IDPRTLST                      
062200*                                      BEREDNING PV (PVV)                 
062410        WHEN 'D' MOVE SATS-HIST-CARP   TO W-IDPRTLST                      
062420*                                      CARPAC                             
062500        WHEN OTHER                                                        
062600           MOVE NEJ TO INDATA-SW                                          
062700     END-EVALUATE                                                         
062800     .                                                                    
062900     EJECT                                                                
063000 C-SKRIV-LISTA SECTION.                                                   
063100                                                                          
063200     MOVE IDARTNR-WS TO W-IDARTNR2                                        
063300     PERFORM IMS-GET-SATB01                                               
063400     IF SEGMENT-FINNS                                                     
063500       PERFORM S03-OPEN-PRT                                               
063600       PERFORM CA-REDIGERA-SKRIV-LISTRUBRIKER                             
063700                                                                          
063800       PERFORM IMS-GET-SATB11                                             
063900       PERFORM UNTIL SEGMENT-SAKNAS                                       
064000         MOVE JA   TO SKRIV-SIDRUBR-SW                                    
064100         MOVE ZERO TO RADRAEKNARE                                         
064200         PERFORM UNTIL (RADRAEKNARE > MAX-RADER) OR                       
064300                        (SEGMENT-SAKNAS)                                  
064400           PERFORM CC-KOLLA-DATUM                                         
064500           IF STATUS-OK                                                   
064600             IF SKRIV-SIDRUBRIK                                           
064700               PERFORM CB-REDIGERA-SKRIV-SIDRUBRIK                        
064800               MOVE NEJ TO SKRIV-SIDRUBR-SW                               
064900             END-IF                                                       
065000             PERFORM CD-SKRIV-RAD                                         
065100           END-IF                                                         
065200           PERFORM IMS-GET-SATB11                                         
065300         END-PERFORM                                                      
065400       END-PERFORM                                                        
065500       PERFORM S04-CLOSE-PRT                                              
065600     END-IF                                                               
065700     .                                                                    
065800     EJECT                                                                
065900 CA-REDIGERA-SKRIV-LISTRUBRIKER SECTION.                                  
066000                                                                          
066100     MOVE ZERO TO SIDRAEKNARE                                             
066200                                                                          
066300     MOVE DAGENS-DATUM-AR          TO RUB1-AA                             
066400     MOVE DAGENS-DATUM-MANAD       TO RUB1-MM                             
066500     MOVE DAGENS-DATUM-DAG         TO RUB1-DD                             
066600     MOVE DAGENS-TID-HH            TO RUB1-HH                             
066700     MOVE DAGENS-TID-MM            TO RUB1-MIN                            
066800     MOVE IDARTNR-WS               TO RUB2-IDARTNR                        
066900     MOVE IDSKYLT-WS               TO RUB2-IDSKYLT                        
067000     MOVE VECKA-FOM                TO RUB2-VECKA-FOM                      
067100     MOVE VECKA-TOM                TO RUB2-VECKA-TOM                      
067200                                                                          
067300     MOVE IDARTNR-WS TO W-IDARTNR                                         
067400     PERFORM IMS-GET-ARTC01                                               
067500     IF SEGMENT-FINNS                                                     
067600       MOVE ART-IDLEVNR         TO RUB3-IDLEVNR                           
067800       PERFORM IMS-GET-ARTC11                                             
067900       IF SEGMENT-FINNS                                                   
068000         MOVE CLAG-KDERS        TO RUB3-KDERS                             
068100       END-IF                                                             
068200                                                                          
068300       PERFORM S01-HAEMTA-BEART-BSEQ                                      
068400       MOVE BEART-WS TO RUB3-BEART                                        
068500     ELSE                                                                 
068600       MOVE SPACE                  TO RUB3-IDLEVNR                        
068700       IF IDSKYLT-WS = 'S  '                                              
068800         MOVE SATB01-STR-BEART-SVE TO RUB3-BEART                          
068900       ELSE                                                               
069000         MOVE SATB01-STR-BEART-SVE TO W-BEART                             
069100         MOVE SATB01-STR-KDBENHOM  TO KDBENHOM-WS                         
069200         PERFORM S02-HAEMTA-BEART-ASEQ                                    
069300         MOVE BEART-WS TO RUB3-BEART                                      
069400       END-IF                                                             
069500     END-IF                                                               
069600                                                                          
069700     IF  (SATB01-STR-KDPRODSL NOT = ZERO)                                 
069800     AND (SATB01-STR-IDFKNGRP NOT = ZERO)                                 
069900        MOVE SATB01-STR-KDPRODSL   TO RUB3-KDPRODSL                       
070000        MOVE SATB01-STR-IDFKNGRP   TO RUB3-IDFKNGRP                       
070100     ELSE                                                                 
070200        PERFORM IMS-GET-ARTC01                                            
070300        IF SEGMENT-FINNS                                                  
070400           MOVE ART-KDPRODSL       TO RUB3-KDPRODSL                       
070500           MOVE ART-IDFKNGRP       TO RUB3-IDFKNGRP                       
070600        END-IF                                                            
070700     END-IF                                                               
070800     MOVE SATB01-STR-IDSTRTYP      TO RUB3-IDSTRTYP                       
070900     MOVE SATB01-STR-TESTRNOT(1)   TO RUB4A-TESTRNOT                      
071000     MOVE SATB01-STR-TESTRNOT(2)   TO RUB4B-TESTRNOT                      
071100     PERFORM S90-SKRIV-HUVUDRUBRIKER                                      
071200     .                                                                    
071300     EJECT                                                                
071400 CB-REDIGERA-SKRIV-SIDRUBRIK SECTION.                                     
071500                                                                          
071600     ADD +1 TO SIDRAEKNARE                                                
071700     MOVE SIDRAEKNARE TO RUB5-SIDA                                        
071800                                                                          
071900     IF SIDRAEKNARE = +1                                                  
072000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                        
072100                           W-IDPRTLST ALT-PCB                             
072200                           PRT-AFTER-1 RUB5-RAD                           
072300       MOVE +10 TO RADRAEKNARE                                            
072400     ELSE                                                                 
072500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                        
072600                           W-IDPRTLST ALT-PCB                             
072700                           PRT-NYSIDA-RAD4 RUB5-RAD                       
072800       MOVE +2  TO RADRAEKNARE                                            
072900     END-IF                                                               
073000                                                                          
073100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
073200                         W-IDPRTLST ALT-PCB                               
073300                         PRT-AFTER-1 DUMMY-RAD                            
073400     .                                                                    
073500     EJECT                                                                
073600 CC-KOLLA-DATUM SECTION.                                                  
073700                                                                          
073800     MOVE SATB11-RAD-TISTODAT TO STOPP-DATUM                              
073900                                                                          
074000     IF STOPP-DATUM = 99999                                               
074100       MOVE JA TO STOPP-DATUM-SW                                          
074200       MOVE SATB11-RAD-TISTADAT TO DAT-I-TIDATUM                          
074300       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
074400       CALL WDATKONV USING DAT-KDDATFORM                                  
074500                           DAT-I-TIDATUM                                  
074600                           DAT-O-TIDATUM                                  
074700                           DAT-KDSVAR                                     
074800                                                                          
074900       MOVE DAT-TIAA-VECKA TO SPAR-START-AA                               
075000       MOVE DAT-TIVV       TO SPAR-START-VV                               
075100                                                                          
075101       MOVE SPAR-START-DATUM    TO TMP1-YYWW                              
075102       MOVE VECKA-TOM           TO TMP2-YYWW                              
075110       PERFORM WY2000P3                                                   
075200       IF TMP1-YYWW <= TMP2-YYWW                                          
075300         MOVE JA TO STATUS-SW                                             
075400       ELSE                                                               
075500         MOVE NEJ TO STATUS-SW                                            
075600       END-IF                                                             
075700                                                                          
075701       MOVE SPAR-START-DATUM   TO TMP1-YYWW                               
075702       MOVE VECKA-FOM          TO TMP2-YYWW                               
075710       PERFORM WY2000P3                                                   
075800       IF TMP1-YYWW < TMP2-YYWW                                           
075900         MOVE JA TO START-DATUM-SW                                        
076000       ELSE                                                               
076100         MOVE NEJ TO START-DATUM-SW                                       
076200       END-IF                                                             
076300     ELSE                                                                 
076400       MOVE NEJ TO STOPP-DATUM-SW                                         
076500       IF SATB11-RAD-KDISATS = 'U' OR 'E'                                 
076501         MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                        
076502         MOVE DAGENS-DATUM          TO TMP2-YYMMDD                        
076510         PERFORM WY2000P1                                                 
076600         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
076700           MOVE SATB11-RAD-TISTADAT TO DAT-I-TIDATUM                      
076800           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
076900           CALL WDATKONV USING DAT-KDDATFORM                              
077000                               DAT-I-TIDATUM                              
077100                               DAT-O-TIDATUM                              
077200                               DAT-KDSVAR                                 
077300                                                                          
077400           MOVE DAT-TIAA-VECKA TO SPAR-START-AA                           
077500           MOVE DAT-TIVV       TO SPAR-START-VV                           
077600                                                                          
077700           MOVE SATB11-RAD-TISTODAT TO DAT-I-TIDATUM                      
077800           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
077900           CALL WDATKONV USING DAT-KDDATFORM                              
078000                               DAT-I-TIDATUM                              
078100                               DAT-O-TIDATUM                              
078200                               DAT-KDSVAR                                 
078300                                                                          
078400           MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                           
078500           MOVE DAT-TIVV TO SPAR-STOPP-VV                                 
078600                                                                          
078601           MOVE SPAR-STOPP-DATUM    TO TMP1-YYWW                          
078602           MOVE VECKA-FOM           TO TMP2-YYWW                          
078604           MOVE VECKA-TOM           TO TMP3-YYWW                          
078610           PERFORM WY2000Q3                                               
078700           IF (TMP1-YYWW >= TMP2-YYWW)  AND                               
078800              (TMP1-YYWW <= TMP3-YYWW)                                    
078900             MOVE JA TO BEHANDLING-SW                                     
079000             MOVE NEJ TO STOPP-DATUM-SW                                   
079100           ELSE                                                           
079200             MOVE NEJ TO BEHANDLING-SW                                    
079300             MOVE JA TO STOPP-DATUM-SW                                    
079400           END-IF                                                         
079500                                                                          
079501           MOVE SPAR-START-DATUM    TO TMP1-YYWW                          
079502           MOVE VECKA-TOM           TO TMP2-YYWW                          
079510           PERFORM WY2000P3                                               
079600           IF TMP1-YYWW <= TMP2-YYWW                                      
079700             MOVE JA TO BEHANDLING-SW                                     
079800           ELSE                                                           
079900             MOVE NEJ TO BEHANDLING-SW                                    
080000           END-IF                                                         
080100                                                                          
080101           MOVE SPAR-START-DATUM   TO TMP1-YYWW                           
080102           MOVE VECKA-FOM          TO TMP2-YYWW                           
080110           PERFORM WY2000P3                                               
080200           IF TMP1-YYWW < TMP2-YYWW                                       
080300             MOVE JA TO START-DATUM-SW                                    
080400           ELSE                                                           
080500             MOVE NEJ TO START-DATUM-SW                                   
080600           END-IF                                                         
080700                                                                          
080800         ELSE                                                             
080900           MOVE SATB11-RAD-TISTADAT TO DAT-I-TIDATUM                      
081000           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
081100           CALL WDATKONV USING DAT-KDDATFORM                              
081200                               DAT-I-TIDATUM                              
081300                               DAT-O-TIDATUM                              
081400                               DAT-KDSVAR                                 
081500                                                                          
081600           MOVE DAT-TIAA-VECKA TO SPAR-START-AA                           
081700           MOVE DAT-TIVV       TO SPAR-START-VV                           
081800                                                                          
081900           MOVE SATB11-RAD-TISTODAT TO DAT-I-TIDATUM                      
082000           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
082100           CALL WDATKONV USING DAT-KDDATFORM                              
082200                               DAT-I-TIDATUM                              
082300                               DAT-O-TIDATUM                              
082400                               DAT-KDSVAR                                 
082500                                                                          
082600           MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                           
082700           MOVE DAT-TIVV       TO SPAR-STOPP-VV                           
082800                                                                          
082801           MOVE SPAR-START-DATUM   TO TMP1-YYWW                           
082802           MOVE VECKA-FOM          TO TMP2-YYWW                           
082810           PERFORM WY2000P3                                               
082900           IF TMP1-YYWW < TMP2-YYWW                                       
083000             MOVE JA TO START-DATUM-SW                                    
083100           ELSE                                                           
083200             MOVE NEJ TO START-DATUM-SW                                   
083300           END-IF                                                         
083400                                                                          
083401           MOVE SPAR-STOPP-DATUM    TO TMP1-YYWW                          
083402           MOVE VECKA-FOM           TO TMP2-YYWW                          
083403           MOVE VECKA-TOM           TO TMP3-YYWW                          
083410           PERFORM WY2000Q3                                               
083500           IF (TMP1-YYWW >= TMP2-YYWW)  AND                               
083600              (TMP1-YYWW <= TMP3-YYWW)                                    
083700             MOVE JA TO BEHANDLING-SW                                     
083800           ELSE                                                           
083900             MOVE NEJ TO BEHANDLING-SW                                    
084000           END-IF                                                         
084100         END-IF                                                           
084200       ELSE                                                               
084300         MOVE JA TO BEHANDLING-SW                                         
084400         MOVE SATB11-RAD-TISTADAT TO DAT-I-TIDATUM                        
084500         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
084600         CALL WDATKONV USING DAT-KDDATFORM                                
084700                             DAT-I-TIDATUM                                
084800                             DAT-O-TIDATUM                                
084900                             DAT-KDSVAR                                   
085000                                                                          
085100         MOVE DAT-TIAA-VECKA TO SPAR-START-AA                             
085200         MOVE DAT-TIVV       TO SPAR-START-VV                             
085300                                                                          
085400         MOVE SATB11-RAD-TISTODAT TO DAT-I-TIDATUM                        
085500         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
085600         CALL WDATKONV USING DAT-KDDATFORM                                
085700                             DAT-I-TIDATUM                                
085800                             DAT-O-TIDATUM                                
085900                             DAT-KDSVAR                                   
086000                                                                          
086100         MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                             
086200         MOVE DAT-TIVV       TO SPAR-STOPP-VV                             
086300                                                                          
086301         MOVE SPAR-START-DATUM   TO TMP1-YYWW                             
086302         MOVE VECKA-FOM          TO TMP2-YYWW                             
086310         PERFORM WY2000P3                                                 
086400         IF TMP1-YYWW >= TMP2-YYWW                                        
086500           MOVE JA TO BEHANDLING-SW                                       
086600         ELSE                                                             
086700           MOVE NEJ TO BEHANDLING-SW                                      
086800         END-IF                                                           
086900       END-IF                                                             
087000                                                                          
087100                                                                          
087200       IF BEHANDLING-OK                                                   
087201         MOVE SPAR-START-DATUM   TO TMP1-YYWW                             
087202         MOVE VECKA-TOM          TO TMP2-YYWW                             
087203         MOVE SPAR-STOPP-DATUM   TO TMP3-YYWW                             
087204         MOVE VECKA-FOM          TO TMP4-YYWW                             
087210         PERFORM WY2000Q3                                                 
087300         IF (TMP1-YYWW <= TMP2-YYWW)  AND                                 
087400            (TMP3-YYWW >= TMP4-YYWW)                                      
087500           MOVE JA TO STATUS-SW                                           
087600         ELSE                                                             
087700           MOVE NEJ TO STATUS-SW                                          
087800         END-IF                                                           
087900       ELSE                                                               
088000         MOVE NEJ TO STATUS-SW                                            
088100       END-IF                                                             
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600 CD-SKRIV-RAD SECTION.                                                    
088700                                                                          
088800     MOVE SATB11-RAD-IDRADNR   TO L1-IDRADNR                              
088900     MOVE SATB11-RAD-REANTPSA  TO L1-REANTPSA                             
089000     MOVE SATB11-RAD-IDSTRTYP  TO L1-IDSTRTYP                             
089100                                                                          
089200     IF START-DATUM-OK                                                    
089300       MOVE ZERO               TO L1-TISTADAT-VECKA                       
089400       MOVE SPACE              TO L1-IDAO-STA                             
089500     ELSE                                                                 
089600       MOVE SPAR-START-DATUM   TO L1-TISTADAT-VECKA                       
089700       MOVE SATB11-RAD-IDAO-STA TO L1-IDAO-STA                            
089800     END-IF                                                               
089900                                                                          
090000     IF SATB11-RAD-IDLEVNR NOT = SPACE                                    
090100       MOVE SATB11-RAD-IDLEVNR TO L1-IDLEVNR                              
090200       MOVE SATB11-RAD-BELEVART TO L1-IDARTNR-BELEVART                    
090300       IF IDSKYLT-WS = 'S  '                                              
090400         MOVE SATB11-RAD-BEART-SVE TO L1-BEART                            
090500       ELSE                                                               
090600         MOVE SATB11-RAD-BEART-SVE TO W-BEART                             
090700         MOVE SATB11-RAD-KDBENHOM  TO KDBENHOM-WS                         
090800         PERFORM S02-HAEMTA-BEART-ASEQ                                    
090900         MOVE BEART-WS TO L1-BEART                                        
091000       END-IF                                                             
091100     ELSE                                                                 
091200       IF SATB11-RAD-IDARTNR = ZERO                                       
091300         MOVE SPACE TO L1-IDLEVNR                                         
091400         IF IDSKYLT-WS = 'S  '                                            
091500           MOVE SATB11-RAD-BEART-SVE TO L1-BEART                          
091600         ELSE                                                             
091700           MOVE SATB11-RAD-BEART-SVE TO W-BEART                           
091800           MOVE SATB11-RAD-KDBENHOM TO KDBENHOM-WS                        
091900           PERFORM S02-HAEMTA-BEART-ASEQ                                  
092000           MOVE BEART-WS TO L1-BEART                                      
092100         END-IF                                                           
092200       ELSE                                                               
092300         MOVE SATB11-RAD-IDARTNR TO W-IDARTNR                             
092400                                    L1-IDARTNR-BELEVART                   
092500         INSPECT L1-IDARTNR-BELEVART REPLACING LEADING                    
092600                 ZERO BY SPACE                                            
092700         MOVE SPACE              TO L1-IDLEVNR                            
092800         PERFORM S01-HAEMTA-BEART-BSEQ                                    
092900         MOVE BEART-WS TO L1-BEART                                        
093000       END-IF                                                             
093100     END-IF                                                               
093200                                                                          
093300     IF STOPP-DATUM-OK                                                    
093400       MOVE ZERO                TO L1-TISTODAT-VECKA                      
093500       MOVE SPACE               TO L1-IDAO-STO                            
093600     ELSE                                                                 
093700       MOVE SPAR-STOPP-DATUM    TO L1-TISTODAT-VECKA                      
093800       MOVE SATB11-RAD-IDAO-STO TO L1-IDAO-STO                            
093900     END-IF                                                               
094000                                                                          
094100     PERFORM S92-SKRIV-UTRAD                                              
094200     .                                                                    
094300     EJECT                                                                
094400 S01-HAEMTA-BEART-BSEQ SECTION.                                           
094500                                                                          
094600     PERFORM IMS-GET-BENA01-BSEQ                                          
094700     IF SEGMENT-FINNS                                                     
094800       MOVE IDSKYLT-WS TO W-IDSKYLT                                       
094900       PERFORM IMS-GET-BENA11-BSEQ                                        
095000       IF SEGMENT-FINNS                                                   
095100         MOVE BENA11-TEXT-BEART TO BEART-WS                               
095200       ELSE                                                               
095300         MOVE SPACE             TO BEART-WS                               
095400       END-IF                                                             
095500     ELSE                                                                 
095600       MOVE SPACE TO BEART-WS                                             
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000 S02-HAEMTA-BEART-ASEQ SECTION.                                           
096100                                                                          
096200     MOVE 'S' TO W-IDSKYLT                                                
096300     PERFORM IMS-GET-BENA01-ASEQ                                          
096400     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
096500                    KDBENHOM-WS = BENA01-BEN-KDHOMONYM                    
096600       IF SEGMENT-FINNS                                                   
096700         IF KDBENHOM-WS = BENA01-BEN-KDHOMONYM                            
096800           CONTINUE                                                       
096900         ELSE                                                             
097000           PERFORM IMS-GET-BENA01-ASEQ-NEXT                               
097100         END-IF                                                           
097200       END-IF                                                             
097300     END-PERFORM                                                          
097400                                                                          
097500     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
097600     IF SEGMENT-FINNS                                                     
097700       PERFORM IMS-GET-BENA11-ASEQ                                        
097800       IF SEGMENT-FINNS                                                   
097900         MOVE BENA11-TEXT-BEART TO BEART-WS                               
098000       ELSE                                                               
098100         MOVE SPACE             TO BEART-WS                               
098200       END-IF                                                             
098300     ELSE                                                                 
098400       MOVE SPACE TO BEART-WS                                             
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800 S03-OPEN-PRT SECTION.                                                    
098900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN                           
099000                         W-IDPRTLST ALT-PCB                               
099100                         DUMMY-RAD DUMMY-RAD                              
099200     .                                                                    
099300     EJECT                                                                
099400 S04-CLOSE-PRT SECTION.                                                   
099500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE                          
099600                         W-IDPRTLST ALT-PCB                               
099700                         DUMMY-RAD DUMMY-RAD                              
099800     .                                                                    
099900     EJECT                                                                
100000 S90-SKRIV-HUVUDRUBRIKER SECTION.                                         
100100     SKIP2                                                                
100200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
100300                         W-IDPRTLST ALT-PCB                               
100400                         PRT-NYSIDA-RAD4 RUB1-RAD                         
100500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
100600                         W-IDPRTLST ALT-PCB                               
100700                         PRT-AFTER-2 RUB2-RAD                             
100800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
100900                         W-IDPRTLST ALT-PCB                               
101000                         PRT-AFTER-2 RUB3-RAD                             
101100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
101200                         W-IDPRTLST ALT-PCB                               
101300                         PRT-AFTER-1 RUB4A-RAD                            
101400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
101500                         W-IDPRTLST ALT-PCB                               
101600                         PRT-AFTER-1 RUB4B-RAD                            
101700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
101800                         W-IDPRTLST ALT-PCB                               
101900                         PRT-AFTER-1 DUMMY-RAD                            
102000     .                                                                    
102100     EJECT                                                                
102200 S92-SKRIV-UTRAD SECTION.                                                 
102300                                                                          
102400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                          
102500                         W-IDPRTLST ALT-PCB                               
102600                         PRT-AFTER-1 UTRAD                                
102700                                                                          
102800     ADD +1     TO RADRAEKNARE                                            
102900     MOVE SPACE TO UTRAD                                                  
103000     .                                                                    
103100     EJECT                                                                
103200******************************************************                    
103300*                  IMS SEKTIONER                     *                    
103400******************************************************                    
103500 IMS-GET-MSG SECTION.                                                     
103600                                                                          
103700     MOVE '  QC' TO GODK-STATUSKODER                                      
103800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
103900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     EJECT                                                                
105200 IMS-GET-ARTC01 SECTION.                                                  
105300                                                                          
105400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
105500          DELIMITED BY SIZE INTO SSA1                                     
105600     MOVE '  GE' TO GODK-STATUSKODER                                      
105700     CALL CBLTDLI USING GU ARTC-PCB IO-AREA SSA1                          
105800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105900     PERFORM IMS-STATUSKONTROLL                                           
106000     .                                                                    
106100 IMS-GET-ARTC11 SECTION.                                                  
106200                                                                          
106500     MOVE 'WLARTC11 ' TO SSA1                                             
106600     MOVE '  GE' TO GODK-STATUSKODER                                      
106700     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA SSA1                         
106800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     .                                                                    
107300 IMS-GET-SATB01 SECTION.                                                  
107400                                                                          
107500     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR2-X ')'                        
107600          DELIMITED BY SIZE INTO SSA1                                     
107700     MOVE '  GE' TO GODK-STATUSKODER                                      
107800     CALL CBLTDLI USING GU SATB-PCB IO-AREA2 SSA1                         
107900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200 IMS-GET-SATB11 SECTION.                                                  
108300                                                                          
108400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR2-X ')'                        
108500          DELIMITED BY SIZE INTO SSA1                                     
108600     MOVE 'WLSATB11 ' TO SSA2                                             
108700     MOVE '  GE' TO GODK-STATUSKODER                                      
108800     CALL CBLTDLI USING GNP SATB-PCB IO-AREA2 SSA1 SSA2                   
108900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300 IMS-GET-BENA01-ASEQ SECTION.                                             
109400                                                                          
109500     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
109600                                  W-BEART-X ')'                           
109700          DELIMITED BY SIZE INTO SSA1                                     
109800     MOVE '  GE' TO GODK-STATUSKODER                                      
109900     CALL CBLTDLI USING GU BENA-A-PCB IO-AREA SSA1                        
110000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     .                                                                    
110300 IMS-GET-BENA01-ASEQ-NEXT SECTION.                                        
110400                                                                          
110500     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
110600                                  W-BEART-X ')'                           
110700          DELIMITED BY SIZE INTO SSA1                                     
110800     MOVE '  GE' TO GODK-STATUSKODER                                      
110900     CALL CBLTDLI USING GN BENA-A-PCB IO-AREA SSA1                        
111000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
111100     PERFORM IMS-STATUSKONTROLL                                           
111200     .                                                                    
111300 IMS-GET-BENA11-ASEQ SECTION.                                             
111400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
111500            DELIMITED BY SIZE INTO SSA1                                   
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GNP BENA-A-PCB IO-AREA SSA1                       
111800     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP2                                                                
112200 IMS-GET-BENA01-BSEQ SECTION.                                             
112300                                                                          
112400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GU BENA-B-PCB IO-AREA SSA1                        
112800     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100 IMS-GET-BENA11-BSEQ SECTION.                                             
113200                                                                          
113300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
113600          DELIMITED BY SIZE INTO SSA2                                     
113700     MOVE '  GE' TO GODK-STATUSKODER                                      
113800     CALL CBLTDLI USING GNP BENA-B-PCB IO-AREA SSA1 SSA2                  
113900     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
114000     PERFORM IMS-STATUSKONTROLL                                           
114100     .                                                                    
114200                                                                          
114300 IMS-STATUSKONTROLL SECTION.                                              
114400                                                                          
114500     SET STATUS-IX TO 1                                                   
114600     SEARCH GODK-STATUS                                                   
114700       AT END CALL FELLOG                                                 
114800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
114900     END-SEARCH                                                           
115000     .                                                                    
115010     EJECT                                                                
115100*    -COPY WY2000P3                                                       
115110     EJECT                                                                
115120*    -COPY WY2000Q3                                                       
115130     EJECT                                                                
115200*    -COPY WY2000P1                                                       
