000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL012000.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   04/05/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700* WL012000 PROGRAM IS A REPLICA OF W4031200 PROGRAM                       
000800* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
000900*                                                                         
001000*    NAMN:       CARPARTS.LDC.SPLITOFORDER                                
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        SPLIT OF ORDER                                                   
001400*    FUNKTION.                                                            
001500*        ANNULLERA/DELA ORDER FRÅGA/UPPDATERA.                            
001600*                                                                         
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WDE4                                       
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0120T                                             
002200*        REQUEST:     WL0120I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        RESPONSE:    WL0120O1                                            
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'WL012000'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600                                                                          
003700 77  PGM-POS                     PIC X(16)   VALUE SPACE.                 
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  FL-SAMTL-PLKLST-HAR-USER-NOLL PIC X.                                 
004100 77  BORTTAG                     PIC X       VALUE 'B'.                   
004200                                                                          
004300 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
004400 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  SPRAK-INDX                  PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  REQU-RAD-IND                PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  RESP-RAD-IND                PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  WS-RESP-AREA                PIC S9(5)   VALUE ZERO COMP-3.           
004900 77  WS-RAD-IND                  PIC  9(9)   VALUE  0.                    
005000 77  MAX-LINE                    PIC S9(9)   VALUE +13  COMP SYNC.        
005100 77  MAX-RESP-LAENGD             PIC S9(4)   VALUE +920 COMP SYNC.        
005200                                                                          
005300 77  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
005400 77  DAGENS-DATUM                PIC 9(6).                                
005500 77  WS-JFR-IDPRODNR             PIC S9(7)              COMP-3.           
005600 77  WS-4487-IDDC                PIC X(2).                                
005700 77  ANTAL-LAS                   PIC 9(3).                                
005800*                                                                         
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400*                                                                         
006500 77  INDATA-SW                   PIC  X(1).                               
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-FEL                          VALUE 'N'.                   
006800                                                                          
006900 77  WS-IDELMT-ERROR             PIC X(16).                               
007000 77  WS-IDMSG-ERROR              PIC X(03).                               
007100 77  WS-IDMSG-INFO               PIC X(03).                               
007200                                                                          
007300*                                                                         
007400 77  WS-NY-RADNR-FROM            PIC 9(5)    VALUE ZERO COMP-3.           
007500 77  WS-NY-RADNR-TOM             PIC 9(5)    VALUE ZERO COMP-3.           
007600 77  WS-KDFEL                    PIC 9(2)    VALUE ZERO COMP-3.           
007700                                                                          
007800 01  TESTMED.                                                             
007900     03  FILLER                  PIC X(5)    VALUE 'USER='.               
008000     03  TEST-1                  PIC X(5).                                
008100     03  FILLER                  PIC X(5)    VALUE '4488='.               
008200     03  TEST-2                  PIC 9(11).                               
008300     03  TEST-3                  PIC 9(07).                               
008400     03  TEST-4                  PIC 9(03).                               
008500 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
008600 01  WS-IDUSER                   PIC 9(5).                                
008700 01  WS-IDUSER-NUM               PIC 9(5).                                
008800                                                                          
008900 01  WS-IDPRCPLK                 PIC X(4).                                
009000 01  WS-IDLOTNR-PLK              PIC 9(3).                                
009100 01  WS-IDANSTNR                 PIC 9(5).                                
009200 01  WS-IDANSTNR-MIN             PIC 9(5).                                
009300 01  WS-IDANSTNR-MAX             PIC 9(5).                                
009400                                                                          
009500 01  WS-IDDISTR                            PIC X(4).                      
009600 01  IDDISTR-WS REDEFINES WS-IDDISTR       PIC 9(4).                      
009700 01  WS-IDKUNDNR                           PIC X(6).                      
009800 01  IDKUNDNR-WS REDEFINES WS-IDKUNDNR     PIC 9(6).                      
009900 01  WS-IDORDNR                            PIC X(5).                      
010000 01  IDORDNR-WS REDEFINES WS-IDORDNR       PIC 9(5).                      
010100 01  WS-IDPRODNR                           PIC X(7).                      
010200 01  IDPRODNR-WS REDEFINES WS-IDPRODNR     PIC 9(7).                      
010300 01  WS-IDKOLLI                            PIC 9(5).                      
010400 01  WS-IDPRODNR-X7                        PIC X(7).                      
010500                                                                          
010600 77  FILLER                      PIC X(8)    VALUE 'DDDDDDDD'.            
010700 77  WS-SPARA-IDDISTR            PIC 9(4)    VALUE ZERO COMP-3.           
010800 77  WS-SPARA-IDKUNDNR           PIC 9(6)    VALUE ZERO COMP-3.           
010900 77  WS-SPARA-IDORDER            PIC 9(7)    VALUE ZERO COMP-3.           
011000 77  WS-SPARA-IDORDNR            PIC 9(5)    VALUE ZERO COMP-3.           
011100 77  WS-SPARA-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
011200 77  WS-SPARA-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
011300 77  WS-SPARA-IDPLKLST-2POS      PIC 9(2).                                
011400 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   VALUE ZERO COMP-3.           
011500                                                                          
011600 77  FILLER                      PIC X(8)    VALUE 'EEEEEEEE'.            
011700 01  WS-NYCKELFALT.                                                       
011800     03  WS-DARFS-NYCKEL         PIC 9(12).                               
011900     03  WS-MID-KDPRCGRP-NEXT    PIC X(5).                                
012000     03  WS-MID-IDPRODNR-NEXT    PIC S9(7)   COMP-3.                      
012100     03  WS-MID-IDPLKLST-NEXT    PIC S9(3)   COMP-3.                      
012200     03  WS-MID-KDPRCGRP-ENTER   PIC X(5).                                
012300     03  WS-MID-IDPRODNR-ENTER   PIC S9(7)   COMP-3.                      
012400     03  WS-MID-IDPLKLST-ENTER   PIC S9(3)   COMP-3.                      
012500                                                                          
012600 01  WS-SPARA-IDKUNDRF.                                                   
012700     03  WS-SPARA-IDORDNR-ALFA   PIC X(5).                                
012800     03  FILLER                  PIC X(5)    VALUE SPACE.                 
012900                                                                          
013000 01  WS-START-STOPP-IDPURAD.                                              
013100     03  WS-START-IDPURAD        PIC S9(5)   VALUE ZERO COMP-3.           
013200     03  WS-STOPP-IDPURAD        PIC S9(5)   VALUE ZERO COMP-3.           
013300                                                                          
013400 01  WS-JFR-IDANSTNR.                                                     
013500     03  FILLER-IDANST           PIC X(3)    VALUE '000'.                 
013600     03  WS-JFR-IDANSTNR-5       PIC X(5).                                
013700                                                                          
013800                                                                          
013900 77  FILLER                      PIC X(8)    VALUE 'FFFFFFFF'.            
014000 77  WS-KVRADER                  PIC 9(5)    VALUE ZERO.                  
014100 77  WS-SUM-KVRADER              PIC 9(7)    VALUE ZERO COMP-3.           
014200 77  WS-KVORDRAD-KVAR            PIC 9(7)    VALUE ZERO COMP-3.           
014300 77  WS-DATUM                    PIC 9(6)    VALUE ZERO COMP-3.           
014400 77  WS-TID                      PIC 9(6)    VALUE ZERO COMP-3.           
014500 77  WS-ANST-KVORDRAD-KVAR       PIC 9(5)    VALUE ZERO COMP-3.           
014600                                                                          
014700*      - - - - - - - - - - - - - *****    SWITCHAR                        
014800 77  FILLER                      PIC X(8)    VALUE 'GGGGGGGG'.            
014900 77  SW-AENDRA                   PIC X       VALUE 'N'.                   
015000 77  SW-ORDERID                  PIC X       VALUE 'N'.                   
015100 77  SW-IDPRODNR                 PIC X       VALUE 'N'.                   
015200 77  KOLLI-FINNS-I-TAB-SW        PIC X       VALUE 'N'.                   
015300                                                                          
015400* --- STORE CHANGED CASES FOR CHECK                                       
015500 77  TAB-INX                     PIC S9(9)   VALUE +0   COMP SYNC.        
015600 77  MAX-TAB-INX                 PIC S9(9)   VALUE +100 COMP SYNC.        
015700                                                                          
015800                                                                          
015900 01  SAVE-IDKOLLI-TAB.                                                    
016000     03  TAB-IDKOLLI  OCCURS 100 PIC 9(5)    VALUE ZERO.                  
016100                                                                          
016200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016300 01  GENERELLA-SUBPROGRAM.                                                
016400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
016700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016800                                                                          
016900*    --- PARAMETRAR TILL ABEND                                            
017000                                                                          
017100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017400                                                                          
017500 01  MESSAGE-CODES.                                                       
017600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
017700                                                                          
017800*                                                                         
017900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
018000                                                                          
018100*01  -COPY WZ01SUB                                                        
018200                                                                          
018300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018400                                                                          
018500 01  REQU-AREA.                                                           
018600*    03  -COPY WZ01REQ2                                                   
018700*    03  -COPY WL0120I1                                                   
018800                                                                          
018900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019000                                                                          
019100 01  RESP-AREA.                                                           
019200*    03  -COPY WZ01RES2                                                   
019300*    03  -COPY WL0120O1                                                   
019400                                                                          
019500 01  NYCKEL-KOMBINATION          PIC S9(2)   VALUE ZERO  COMP-3.          
019600     88  ENDAST-ANSTNR                       VALUE +1.                    
019700     88  ANSTNR-ORDERID                      VALUE +2.                    
019800     88  ENDAST-ORDERID                      VALUE +3.                    
019900                                                                          
020000 01  WS-TRAEFF-PACKARE           PIC X(1).                                
020100     88  TRAEFF-PACKARE                      VALUE 'J'.                   
020200                                                                          
020300 01  NYCKEL-TYP                  PIC S9(2)   COMP-3.                      
020400     88  GAMLA-NYCKLAR                       VALUE +1.                    
020500     88  NYA-NYCKLAR                         VALUE +2.                    
020600                                                                          
020700 01  WS-ORDERDEL-KLAR            PIC X(1).                                
020800     88  ORDERDEL-KLAR                       VALUE 'J'.                   
020900                                                                          
021000 01  WS-ORDERDEL-STARTAD         PIC X(1).                                
021100     88  ORDERDEL-STARTAD                    VALUE 'J'.                   
021200                                                                          
021300 01  WS-ORDERDEL-STATUS          PIC X(1).                                
021400     88  ORDERDEL-STATUS-PACKAD              VALUE 'J'.                   
021500                                                                          
021600 01  WS-VISA-RAD                 PIC X(1).                                
021700     88  VISA-EJ-RAD-PA-BILD                 VALUE 'N'.                   
021800     88  VISA-RAD-PA-BILD                    VALUE 'J'.                   
021900                                                                          
022000 01  WS-SLINGA-KLAR              PIC X(1).                                
022100     88  SLINGA-KLAR                         VALUE 'J'.                   
022200                                                                          
022300 01  FILLER                      PIC X(8)    VALUE 'HHHHHHHH'.            
022400 01  NYCKLAR-TILL-DLI.                                                    
022500     03  W-WDE4A1-KUNDORDER-X.                                            
022600         05  W-4A1-IDDISTR       PIC S9(5)               COMP-3.          
022700         05  W-4A1-IDKUNDNR      PIC S9(7)               COMP-3.          
022800         05  W-4A1-IDKUNDRF.                                              
022900             07  W-4A1-IDORDNR   PIC X(5).                                
023000             07  FILLER          PIC X(5)    VALUE SPACE.                 
023100                                                                          
023200     03  W-WDE401-KUNDORDER-X.                                            
023300         05  W-401-IDDISTR       PIC S9(5)               COMP-3.          
023400         05  W-401-IDKUNDNR      PIC S9(7)               COMP-3.          
023500         05  W-401-IDKUNDRF.                                              
023600             07  W-401-IDORDNR   PIC X(5).                                
023700             07  FILLER          PIC X(5)    VALUE SPACE.                 
023800         05  W-401-IDPRODNR      PIC S9(7)               COMP-3.          
023900         05  W-401-IDPLKLST      PIC S9(3)               COMP-3.          
024000                                                                          
024100     03  W-WDE411-KEYSEQ-MIN-X.                                           
024200         05  W-411-IDPRODNR-MIN  PIC S9(7)               COMP-3.          
024300         05  W-411-IDPURAD-MIN   PIC S9(5)               COMP-3.          
024400                                                                          
024500     03  W-WDE411-KEYSEQ-MAX-X.                                           
024600         05  W-411-IDPRODNR-MAX  PIC S9(7)               COMP-3.          
024700         05  W-411-IDPURAD-MAX   PIC S9(5)               COMP-3.          
024800                                                                          
024900*                                                                         
025000     03  W-WDE4F1KY-MAX-X.                                                
025100         05  W-IDPRODNR-WDE4F-MAX                                         
025200                                 PIC S9(7)   VALUE ZERO  COMP-3.          
025300         05  FILLER              PIC X(25)   VALUE HIGH-VALUE.            
025400                                                                          
025500     03  W-WDE4F1KY-MIN-X.                                                
025600         05  W-IDPRODNR-WDE4F-MIN                                         
025700                                 PIC S9(7)   VALUE ZERO  COMP-3.          
025800         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
025900*                                                                         
026000     03  W-WDE4F-IDPLKST-X.                                               
026100         05  W-IDPLKST-WDE4F     PIC S9(3)   VALUE ZERO  COMP-3.          
026200*                                                                         
026300     03  W-WDE601-VOLVOORDER-X.                                           
026400         05    W-601-IDPRODNR    PIC S9(7)               COMP-3.          
026500*                                                                         
026600     03  W-WDE611-IDKOLLI-X.                                              
026700         05  W-611-IDKOLLI       PIC S9(5)   VALUE ZERO  COMP-3.          
026800*                                                                         
026900                                                                          
027000     03  W-4447-X.                                                        
027100         05  W-4447-IDHTYP       PIC X(4)    VALUE '4447'.                
027200         05  W-4447-IDDC         PIC X(2).                                
027300         05  W-4447-LOW-VALUE    PIC X(24)   VALUE LOW-VALUE.             
027400                                                                          
027500     03  W-4448-X.                                                        
027600         05  W-4448-IDPRC        PIC X(4).                                
027700         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
027800                                                                          
027900     03  W-4487-X.                                                        
028000         05  W-4487-IDHTYP       PIC X(4)    VALUE '4487'.                
028100         05  W-4487-IDDC         PIC X(2).                                
028200         05  W-4487-LOW-VALUE    PIC X(24)   VALUE LOW-VALUE.             
028300                                                                          
028400     03  W-4488-X.                                                        
028500         05  W-4488-KDPRCGRP     PIC X(5).                                
028600                                                                          
028700     03  W-4490-X.                                                        
028800         05 W-4490-DARFS         PIC 9(12).                               
028900         05 W-4490-IDPRODNR      PIC S9(7)               COMP-3.          
029000         05 W-4490-IDPLKLST      PIC S9(3)               COMP-3.          
029100                                                                          
029200     03 W-WDQ301KY-X.                                                     
029300         05  W-IDORDER-WDQ3      PIC S9(7)   VALUE ZERO  COMP-3.          
029400         05  W-IDDC-WDQ3         PIC X(2).                                
029500         05  W-IDPRODNR-WDQ3     PIC S9(7)   VALUE ZERO  COMP-3.          
029600         05  W-IDPLKLST-WDQ3     PIC S9(3)   VALUE ZERO  COMP-3.          
029700                                                                          
029800     03 W-WDQ301KY-MIN-X.                                                 
029900         05  W-IDORDER-WDQ3-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
030000         05  W-IDDC-WDQ3-MIN     PIC X(2).                                
030100         05  W-IDPRODNR-WDQ3-MIN PIC S9(7)   VALUE ZERO  COMP-3.          
030200         05  FILLER              PIC  X(2)   VALUE LOW-VALUE.             
030300                                                                          
030400     03 W-WDQ301KY-MAX-X.                                                 
030500         05  W-IDORDER-WDQ3-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
030600         05  W-IDDC-WDQ3-MAX     PIC X(2).                                
030700         05  W-IDPRODNR-WDQ3-MAX PIC S9(7)   VALUE ZERO  COMP-3.          
030800         05  FILLER              PIC  X(2)   VALUE HIGH-VALUE.            
030900                                                                          
031000     03 W-IDDC-B6-X.                                                      
031100         05  W-IDDC-B6           PIC X(2).                                
031200                                                                          
031300 01  MESSAGE-CODES.                                                       
031400     03  NO-DATA-ENTERED         PIC X(3)    VALUE '014'.                 
031500     03  INVALID-KEY-FIELDS      PIC X(3)    VALUE '022'.                 
031600     03  IS-INVALID              PIC X(3)    VALUE '023'.                 
031700     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
031800     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
031900     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '041'.                 
032000     03  LINES-NOT-FOUND         PIC X(3)    VALUE '027'.                 
032100     03  PRINTING-REQUESTED      PIC X(3)    VALUE '042'.                 
032200     03  TRAILER-RECEIVED        PIC X(3)    VALUE '104'.                 
032300     03  DATA-ENTER-BUT-NO-PRESSED                                        
032400                                 PIC X(3)    VALUE '013'.                 
032500     03  NOT-NUMERIC             PIC X(3)    VALUE '158'.                 
032600                                                                          
032700 01    MEDDELANDE.                                                        
032800   03    FEL1.                                                            
032900     05    FILLER                PIC X(40)   VALUE                        
033000             '708. ORDERN SAKNAS ELLER KLAR          '.                   
033100     05    FILLER                PIC X(40)   VALUE                        
033200             '708 ORDER MISSING OR READY             '.                   
033300   03    FILLER REDEFINES FEL1.                                           
033400     05    FEL-1 OCCURS 2        PIC X(40).                               
033500                                                                          
033600   03    FEL2.                                                            
033700     05    FILLER                PIC X(40)   VALUE                        
033800             '702 ORDERVIS PACKNING PÅGÅR.           '.                   
033900     05    FILLER                PIC X(40)   VALUE                        
034000             '702 REPORTING PER ORDER IN PROGRESS    '.                   
034100   03    FILLER REDEFINES FEL2.                                           
034200     05    FEL-2 OCCURS 2        PIC X(40).                               
034300                                                                          
034400   03    FEL3.                                                            
034500     05    FILLER                PIC X(40)   VALUE                        
034600             'SOFTWARE ORDER                         '.                   
034700     05    FILLER                PIC X(40)   VALUE                        
034800             'SOFTWARE ORDER                         '.                   
034900   03    FILLER REDEFINES FEL3.                                           
035000     05    FEL-3 OCCURS 2        PIC X(40).                               
035100                                                                          
035200   03    FEL4.                                                            
035300     05    FILLER                PIC X(40)   VALUE                        
035400             '711. ANGIVEN PACKARE SAKNAS PÅ ORDERN  '.                   
035500     05    FILLER                PIC X(40)   VALUE                        
035600             '711 PACKER AND ORDER DO NOT MATCH      '.                   
035700   03    FILLER REDEFINES FEL4.                                           
035800     05    FEL-4 OCCURS 2        PIC X(40).                               
035900                                                                          
036000   03    FEL5.                                                            
036100     05    FILLER                PIC X(40)   VALUE                        
036200             '712. INGA UTEST. ORDERDELAR FÖR PACKARE'.                   
036300     05    FILLER                PIC X(40)   VALUE                        
036400             'NO REMAIN. ORDER PARTS FOR THIS PACKER '.                   
036500   03    FILLER REDEFINES FEL5.                                           
036600     05    FEL-5 OCCURS 2        PIC X(40).                               
036700                                                                          
036800   03    FEL8.                                                            
036900     05    FILLER                PIC X(40)   VALUE                        
037000             '714. RAPPORTERING PÅBÖRJAD ELLER KLAR  '.                   
037100     05    FILLER                PIC X(40)   VALUE                        
037200             '714  REPORTING IN PROGRESS OR READY    '.                   
037300   03    FILLER REDEFINES FEL8.                                           
037400     05    FEL-8 OCCURS 2        PIC X(40).                               
037500                                                                          
037600   03    FEL9.                                                            
037700     05    FILLER                PIC X(40)   VALUE                        
037800             '719. PACKARE SAKNAS                    '.                   
037900     05    FILLER                PIC X(40)   VALUE                        
038000             '719 PACKER MISSING                     '.                   
038100   03    FILLER REDEFINES FEL9.                                           
038200     05    FEL-9 OCCURS 2        PIC X(40).                               
038300                                                                          
038400   03    FEL10.                                                           
038500     05    FILLER                PIC X(40)   VALUE                        
038600             '720. ANG. PACKARES ORDERDEL REDAN KLAR '.                   
038700     05    FILLER                PIC X(40)   VALUE                        
038800             '720 ORDER PART OF PACKER READY         '.                   
038900   03    FILLER REDEFINES FEL10.                                          
039000     05    FEL-10 OCCURS 2       PIC X(40).                               
039100                                                                          
039200   03    FEL11.                                                           
039300     05    FILLER                PIC X(40)   VALUE                        
039400             '777. UPPDATERING EJ TILLÅTEN           '.                   
039500     05    FILLER                PIC X(40)   VALUE                        
039600             '777. UPDATING NOT ALLOWED              '.                   
039700   03    FILLER REDEFINES FEL11.                                          
039800     05    FEL-11 OCCURS 2       PIC X(40).                               
039900                                                                          
040000   03    FEL12.                                                           
040100     05    FILLER                PIC X(40)   VALUE                        
040200             '748. UPPLYSTA FÄLT FEL                 '.                   
040300     05    FILLER                PIC X(40)   VALUE                        
040400             '748. HIGHLIT FIELDS WRONG              '.                   
040500   03    FILLER REDEFINES FEL12.                                          
040600     05    FEL-12 OCCURS 2       PIC X(40).                               
040700                                                                          
040800   03    FEL13.                                                           
040900     05    FILLER                PIC X(40)   VALUE                        
041000             '760. UPPGIFTER SAKNAS                  '.                   
041100     05    FILLER                PIC X(40)   VALUE                        
041200             '760. INFORMATION MISSING               '.                   
041300   03    FILLER REDEFINES FEL13.                                          
041400     05    FEL-13 OCCURS 2       PIC X(40).                               
041500                                                                          
041600   03    FEL14.                                                           
041700     05    FILLER                PIC X(40)   VALUE                        
041800             '749 FEL NYCKEL                         '.                   
041900     05    FILLER                PIC X(40)   VALUE                        
042000             '749 WRONG KEY                          '.                   
042100   03    FILLER REDEFINES FEL14.                                          
042200     05    FEL-14 OCCURS 2       PIC X(40).                               
042300                                                                          
042400   03    FEL16.                                                           
042500     05    FILLER                PIC X(40)   VALUE                        
042600             '799. KONTROLL FÖR PACKAREN PÅGÅR       '.                   
042700     05    FILLER                PIC X(40)   VALUE                        
042800             '799 CONTROL FOR PACKER IN PROGRESS     '.                   
042900   03    FILLER REDEFINES FEL16.                                          
043000     05    FEL-16 OCCURS 2       PIC X(40).                               
043100                                                                          
043200   03    FEL17.                                                           
043300     05    FILLER                PIC X(40)   VALUE                        
043400             '800 AVVIKELSE UPPDAT FÖR PACKAREN PÅGÅR'.                   
043500     05    FILLER                PIC X(40)   VALUE                        
043600             'DEVIATION UPDAT. FOR PACKER IN PROGRESS'.                   
043700   03    FILLER REDEFINES FEL17.                                          
043800     05    FEL-17 OCCURS 2       PIC X(40).                               
043900                                                                          
044000   03    FEL18.                                                           
044100     05    FILLER                PIC X(40)   VALUE                        
044200             '801. MANUELL ORDER EJ KOLLIVIS         '.                   
044300     05    FILLER                PIC X(40)   VALUE                        
044400             '801 MANUAL ORDER - NOT PER CASE        '.                   
044500   03    FILLER REDEFINES FEL18.                                          
044600     05    FEL-18 OCCURS 2       PIC X(40).                               
044700                                                                          
044800   03    FEL19.                                                           
044900     05    FILLER                PIC X(40)   VALUE                        
045000             '003 TRYCK PF11 VID UPPDATERING         '.                   
045100     05    FILLER                PIC X(40)   VALUE                        
045200             '003 PRESS PF11 TO UPDATE               '.                   
045300   03    FILLER REDEFINES FEL19.                                          
045400     05    FEL-19 OCCURS 2       PIC X(40).                               
045500                                                                          
045600   03    MED1.                                                            
045700     05    FILLER                PIC X(40)   VALUE                        
045800             '778. FLER RADER FINNS                  '.                   
045900     05    FILLER                PIC X(40)   VALUE                        
046000             '778. MORE LINES                        '.                   
046100   03    FILLER REDEFINES MED1.                                           
046200     05    MED-1 OCCURS 2        PIC X(40).                               
046300                                                                          
046400   03    MED2.                                                            
046500     05    FILLER                PIC X(40)   VALUE                        
046600             '779. ÄNDRING KLAR                      '.                   
046700     05    FILLER                PIC X(40)   VALUE                        
046800             '779. CHANGE READY                      '.                   
046900   03    FILLER REDEFINES MED2.                                           
047000     05    MED-2 OCCURS 2        PIC X(40).                               
047100                                                                          
047200   03    MED4.                                                            
047300     05    FILLER                PIC X(40)   VALUE                        
047400             '006 DETTA ÄR FÖRSTA SIDAN              '.                   
047500     05    FILLER                PIC X(40)   VALUE                        
047600             '006 THIS IS THE FIRST PAGE             '.                   
047700   03    FILLER REDEFINES MED4.                                           
047800     05    MED-4 OCCURS 2        PIC X(40).                               
047900                                                                          
048000   03    MED5.                                                            
048100     05    FILLER                PIC X(40)   VALUE                        
048200             '106 DETTA ÄR SISTA SIDAN               '.                   
048300     05    FILLER                PIC X(40)   VALUE                        
048400             '106 THIS IS THE LAST PAGE              '.                   
048500   03    FILLER REDEFINES MED5.                                           
048600     05    MED-5 OCCURS 2        PIC X(40).                               
048700                                                                          
048800*                                                                         
048900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
049000*                                                                         
049100 01    IMS-WS.                                                            
049200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
049300                                                                          
049400*                        **** STATUS-KOD FRÅN IMS                         
049500   03    STATUS-4488-WS          PIC XX.                                  
049600     88    SEG-4488-FINNS                   VALUE '  '.                   
049700     88    SEG-4488-SAKNAS                  VALUE 'GE' 'GB'.              
049800   03    STATUS-4490-WS          PIC XX.                                  
049900     88    SEG-4490-FINNS                   VALUE '  '.                   
050000     88    SEG-4490-SAKNAS                  VALUE 'GE'.                   
050100   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
050200     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
050300     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
050400   03    STATUS-ORAD-WS          PIC XX.                                  
050500     88    ORAD-FINNS                       VALUE '  '.                   
050600     88    ORAD-SAKNAS                      VALUE 'GE' 'GP'.              
050700   03    STATUS-WS               PIC XX.                                  
050800     88    SEGMENT-FINNS                    VALUE '  '.                   
050900     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
051000     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
051100     88    END-OF-DB                        VALUE 'GB'.                   
051200                                                                          
051300   03    GODK-STATUSKODER.                                                
051400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
051500                                                                          
051600                                                                          
051700 01    SSA1                      PIC X(128).                              
051800 01    SSA2                      PIC X(64).                               
051900 01    SSA3                      PIC X(64).                               
052000 01    SSA4                      PIC X(64).                               
052100                                                                          
052200*                            IMS FUNKTIONSKODER                           
052300*01    -COPY W0003                                                        
052400                                                                          
052500*                            DLI INPUT-OUTPUT AREA                        
052600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA1'.          
052700 01    DLI-IO-AREA.                                                       
052800   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
052900                                                                          
053000*  03    WLXXDJ01 -COPY WDGX4305            -RED IO-AREA.                 
053100                                                                          
053200*  03    WLXXDJ12 -COPY WDGX4306              -RED IO-AREA.               
053300                                                                          
053400*                            DLI INPUT-OUTPUT AREA -3                     
053500 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA3'.          
053600 01    DLI-IO-AREA3.                                                      
053700*  03    WDGX4487 -COPY WDGX4487                                          
053800                                                                          
053900*  03    WDGX4488 -COPY WDGX4488                                          
054000                                                                          
054100*                            DLI INPUT-OUTPUT AREA WDGX4490               
054200 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4490'.           
054300 01    DLI-IO-WDGX4490.                                                   
054400*  03    -COPY WDGX4490                                                   
054500                                                                          
054600 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDQ301'.             
054700 01    DLI-IO-WDQ301.                                                     
054800*  03    -COPY WDQ301                                                     
054900                                                                          
055000 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4447'.           
055100 01    DLI-IO-WDGX4447.                                                   
055200*  03    -COPY WDGX4447                                                   
055300                                                                          
055400 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4448'.           
055500 01    DLI-IO-WDGX4448.                                                   
055600*  03    -COPY WDGX4448                                                   
055700                                                                          
055800 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E601'.           
055900 01    DLI-IO-E601.                                                       
056000*  03    -COPY WDE601                                                     
056100                                                                          
056200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E611'.           
056300 01    DLI-IO-E611.                                                       
056400*  03    -COPY WDE611                                                     
056500                                                                          
056600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E401'.           
056700 01    DLI-IO-E401.                                                       
056800*  03    -COPY WDE401                                                     
056900                                                                          
057000 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E411'.           
057100 01    DLI-IO-E411.                                                       
057200*  03    -COPY WDE411                                                     
057300                                                                          
057400 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E4F1'.           
057500 01    DLI-IO-E4F1.                                                       
057600*  03    -COPY WDE4F1                                                     
057700                                                                          
057800                                                                          
057900***********************************************************               
058000                                                                          
058100 LINKAGE SECTION.                                                         
058200*01    -COPY W0009     -PRE MSG-                                          
058300                                                                          
058400*01    -COPY W0008     -PRE DETTAPGM-                                     
058500     05  FILLER                  PIC X.                                   
058600                                                                          
058700*01    -COPY W0008     -PRE WDE4-                                         
058800     05  FILLER                  PIC X.                                   
058900                                                                          
059000*01    -COPY W0008     -PRE WDE41-                                        
059100     05  FILLER                  PIC X.                                   
059200                                                                          
059300*01    -COPY W0008     -PRE WDE42-                                        
059400     05  FILLER                  PIC X.                                   
059500                                                                          
059600*01    -COPY W0008     -PRE WDE43-                                        
059700     05  FILLER                  PIC X.                                   
059800                                                                          
059900*01    -COPY W0008     -PRE WDE4F-                                        
060000     05  FILLER                  PIC X.                                   
060100                                                                          
060200*01    -COPY W0008     -PRE WDE6-                                         
060300     05  FILLER                  PIC X.                                   
060400                                                                          
060500*01    -COPY W0008     -PRE XXDJ-                                         
060600     05  FILLER                  PIC X.                                   
060700                                                                          
060800*01    -COPY W0008     -PRE 4487-                                         
060900     05  FILLER                  PIC X.                                   
061000                                                                          
061100*01    -COPY W0008     -PRE ORQA-                                         
061200     05  FILLER                  PIC X.                                   
061300                                                                          
061400*01    -COPY W0008     -PRE XXKH-                                         
061500     05  FILLER                  PIC X.                                   
061600                                                                          
061700 PROCEDURE DIVISION USING MSG-PCB DETTAPGM-PCB                            
061800                          WDE4-PCB WDE41-PCB WDE42-PCB WDE43-PCB          
061900                          WDE4F-PCB                                       
062000                          WDE6-PCB XXDJ-PCB  4487-PCB                     
062100                          ORQA-PCB XXKH-PCB.                              
062200 MAIN SECTION.                                                            
062300                                                                          
062400     PERFORM S01-HAEMTA-ANROPSDATA                                        
062500     IF SUB-KDRC = 0                                                      
062600       PERFORM A-INIT                                                     
062700       PERFORM B-KOLLA-NYCKLAR                                            
062800       IF NYCKLAR-OK                                                      
062900         MOVE 'EFTER B-KOL'      TO PGM-POS                               
063000         IF SW-ORDERID = JA AND SW-IDPRODNR = NEJ                         
063100           PERFORM C-LAS-IDPRODNR                                         
063200         END-IF                                                           
063300                                                                          
063400         IF WS-KDFEL = ZERO                                               
063500           IF REQU-KDPGMACT = 'E'                                         
063600             PERFORM D-UPPDATERA                                          
063700             IF WS-KDFEL = ZERO                                           
063800               MOVE 21           TO WS-KDFEL                              
063900             END-IF                                                       
064000           ELSE                                                           
064100             PERFORM H-KOLLA-OM-FELTRYCK                                  
064200             IF INDATA-OK                                                 
064300               IF ENDAST-ANSTNR                                           
064400                 PERFORM E-BEHANDLA-ENDAST-ANSTNR                         
064500               ELSE                                                       
064600                 IF ANSTNR-ORDERID                                        
064700                   PERFORM F-BEHANDLA-ANSTNR-ORDERID                      
064800                 ELSE                                                     
064900                   IF ENDAST-ORDERID                                      
065000                     PERFORM G-BEHANDLA-ENDAST-ORDERID                    
065100                   ELSE                                                   
065200                     MOVE 13     TO WS-KDFEL                              
065300                   END-IF                                                 
065400                 END-IF                                                   
065500               END-IF                                                     
065600             END-IF                                                       
065700           END-IF                                                         
065800         END-IF                                                           
065900                                                                          
066000         IF WS-KDFEL > ZERO                                               
066100           PERFORM J-HAMTA-MEDDELANDE                                     
066200         END-IF                                                           
066300       END-IF                                                             
066400                                                                          
066500       MOVE RESP-IDMSG-INFO      TO WS-IDMSG-INFO                         
066600       MOVE RESP-IDMSG-ERROR     TO WS-IDMSG-ERROR                        
066700       MOVE RESP-IDELMT-ERROR    TO WS-IDELMT-ERROR                       
066800       IF WS-IDMSG-ERROR NOT = SPACE                                      
066900         MOVE ALL '+'            TO RESP-WL0120O1(1:34)                   
067000         MOVE WS-IDMSG-ERROR     TO RESP-IDMSG-ERROR                      
067100         MOVE WS-IDELMT-ERROR    TO RESP-IDELMT-ERROR                     
067200         MOVE WS-IDMSG-INFO      TO RESP-IDMSG-INFO                       
067300         MOVE 001                TO RESP-IDRESVER                         
067400         IF REQU-KDPGMACT = 'S'                                           
067500           MOVE ZERO             TO RESP-KVRADER                          
067600         ELSE                                                             
067700           IF REQU-KVRADER NUMERIC                                        
067800             MOVE REQU-KVRADER   TO RESP-KVRADER                          
067900           ELSE                                                           
068000             MOVE ZERO           TO RESP-KVRADER                          
068100           END-IF                                                         
068200         END-IF                                                           
068300       END-IF                                                             
068400       PERFORM S02-RETURNERA-SVAR                                         
068500     END-IF                                                               
068600                                                                          
068700                                                                          
068800     MOVE ZERO                   TO RETURN-CODE                           
068900     GOBACK                                                               
069000     .                                                                    
069100                                                                          
069200 A-INIT SECTION.                                                          
069300     MOVE 'STA A-INI'            TO PGM-POS                               
069400     MOVE ALL '+'                TO RESP-AREA                             
069500     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
069600                                    RESP-IDMSG-INFO                       
069700                                    RESP-IDELMT-ERROR                     
069800     MOVE ZERO                   TO RESP-KVRADER                          
069900     MOVE '001'                  TO RESP-IDRESVER                         
070000     ACCEPT DAGENS-DATUM       FROM DATE                                  
070100     MOVE REQU-IDDC-KEY          TO RESP-IDDC-KEY                         
070200                                                                          
070300     MOVE ZERO                   TO WS-DARFS-NYCKEL                       
070400     MOVE 20                     TO WS-DARFS-NYCKEL (1:2)                 
070500     .                                                                    
070600                                                                          
070700 B-KOLLA-NYCKLAR SECTION.                                                 
070800     MOVE 'STA B-KOL'            TO PGM-POS                               
070900                                                                          
071000     MOVE JA                     TO NYCKLAR-SW                            
071100                                                                          
071200     PERFORM BA-CHECK-IDANSTNR-OR-IDQUEUENR                               
071300                                                                          
071400     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
071500       IF REQU-IDDISTR-KEY NUMERIC                                        
071600         MOVE JA                 TO SW-ORDERID                            
071700         MOVE +2                 TO NYCKEL-TYP                            
071800         MOVE REQU-IDDISTR-KEY   TO RESP-IDDISTR-KEY                      
071900         MOVE REQU-IDDISTR-KEY   TO WS-IDDISTR                            
072000       ELSE                                                               
072100         MOVE NEJ                TO NYCKLAR-SW                            
072200         MOVE ZERO               TO WS-IDDISTR                            
072300         MOVE 'IDDISTR'          TO RESP-IDELMT-ERROR                     
072400       END-IF                                                             
072500     END-IF                                                               
072600                                                                          
072700     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
072800       IF REQU-IDKUNDNR-KEY NUMERIC                                       
072900         MOVE JA                 TO SW-ORDERID                            
073000         MOVE +2                 TO NYCKEL-TYP                            
073100         MOVE REQU-IDKUNDNR-KEY  TO RESP-IDKUNDNR-KEY                     
073200         MOVE REQU-IDKUNDNR-KEY  TO WS-IDKUNDNR                           
073300       ELSE                                                               
073400         MOVE NEJ                TO NYCKLAR-SW                            
073500         MOVE ZERO               TO WS-IDKUNDNR                           
073600         MOVE 'IDKUNDNR'         TO RESP-IDELMT-ERROR                     
073700       END-IF                                                             
073800     END-IF                                                               
073900                                                                          
074000     IF REQU-IDORDNR-KEY NOT = ALL '+'                                    
074100       IF REQU-IDORDNR-KEY NUMERIC                                        
074200         MOVE JA                 TO SW-ORDERID                            
074300         MOVE +2                 TO NYCKEL-TYP                            
074400         MOVE REQU-IDORDNR-KEY   TO RESP-IDORDNR-KEY                      
074500         MOVE REQU-IDORDNR-KEY   TO WS-IDORDNR                            
074600       ELSE                                                               
074700         MOVE NEJ                TO NYCKLAR-SW                            
074800         MOVE 'IDORDNR'          TO RESP-IDELMT-ERROR                     
074900         MOVE ZERO               TO WS-IDORDNR                            
075000       END-IF                                                             
075100     END-IF                                                               
075200                                                                          
075300     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
075400       IF REQU-IDPRODNR-KEY NUMERIC                                       
075500         MOVE JA                 TO SW-IDPRODNR                           
075600         MOVE +2                 TO NYCKEL-TYP                            
075700         MOVE REQU-IDPRODNR-KEY  TO RESP-IDPRODNR-KEY                     
075800         MOVE REQU-IDPRODNR-KEY  TO WS-IDPRODNR                           
075900       ELSE                                                               
076000         MOVE NEJ                TO NYCKLAR-SW                            
076100         MOVE ZERO               TO WS-IDPRODNR                           
076200         MOVE 'IDPRODNR'         TO RESP-IDELMT-ERROR                     
076300       END-IF                                                             
076400     END-IF                                                               
076500                                                                          
076600     IF REQU-IDKOLLI-KEY NOT = ALL '+'                                    
076700       IF REQU-IDKOLLI-KEY NUMERIC                                        
076800         MOVE REQU-IDKOLLI-KEY   TO RESP-IDKOLLI-KEY                      
076900       ELSE                                                               
077000         MOVE NEJ                TO NYCKLAR-SW                            
077100         MOVE 'IDKOLLI'          TO RESP-IDELMT-ERROR                     
077200       END-IF                                                             
077300     END-IF                                                               
077400                                                                          
077500     IF WS-IDANSTNR-MIN NOT = ZERO AND                                    
077600        WS-IDANSTNR-MAX NOT = ZERO                                        
077700       IF SW-IDPRODNR = JA OR SW-ORDERID = JA                             
077800         MOVE +2                 TO NYCKEL-KOMBINATION                    
077900       ELSE                                                               
078000         MOVE +1                 TO NYCKEL-KOMBINATION                    
078100       END-IF                                                             
078200     ELSE                                                                 
078300       IF SW-IDPRODNR = JA OR SW-ORDERID = JA                             
078400         MOVE +3                 TO NYCKEL-KOMBINATION                    
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     IF ENDAST-ANSTNR                                                     
078900       MOVE ZERO                 TO WS-IDDISTR                            
079000                                    WS-IDKUNDNR                           
079100                                    WS-IDORDNR                            
079200                                    WS-IDPRODNR                           
079300     ELSE                                                                 
079400       IF ANSTNR-ORDERID AND SW-IDPRODNR = JA                             
079500         MOVE ZERO               TO WS-IDDISTR                            
079600                                    WS-IDKUNDNR                           
079700                                    WS-IDORDNR                            
079800       ELSE                                                               
079900         IF ANSTNR-ORDERID AND SW-ORDERID  = JA                           
080000           MOVE ZERO             TO WS-IDPRODNR                           
080100         ELSE                                                             
080200           IF ENDAST-ORDERID AND SW-IDPRODNR = JA                         
080300             MOVE ZERO           TO WS-IDUSER                             
080400             MOVE ZERO           TO WS-IDDISTR                            
080500                                    WS-IDKUNDNR                           
080600                                    WS-IDORDNR                            
080700           ELSE                                                           
080800             IF ENDAST-ORDERID AND SW-ORDERID = JA                        
080900               MOVE ZERO         TO WS-IDUSER                             
081000               MOVE ZERO         TO WS-IDPRODNR                           
081100             END-IF                                                       
081200           END-IF                                                         
081300         END-IF                                                           
081400       END-IF                                                             
081500     END-IF                                                               
081600                                                                          
081700     IF REQU-KDPGMACT = 'E'                                               
081800       IF REQU-KVRADER = ALL '+'                                          
081900         MOVE ZERO               TO WS-KVRADER                            
082000       ELSE                                                               
082100         MOVE REQU-KVRADER       TO WS-KVRADER                            
082200       END-IF                                                             
082300                                                                          
082400       MOVE NEJ                  TO SW-AENDRA                             
082500       MOVE +1                   TO REQU-RAD-IND                          
082600       PERFORM UNTIL REQU-RAD-IND > WS-KVRADER OR                         
082700                     REQU-RAD-IND > 500                                   
082800         IF REQU-IDANSTNR-NEW (REQU-RAD-IND) NOT = ALL '+'                
082900           IF REQU-IDANSTNR-NEW (REQU-RAD-IND) NUMERIC                    
083000             MOVE JA             TO SW-AENDRA                             
083100             MOVE JA             TO NYCKLAR-SW                            
083200           ELSE                                                           
083300             MOVE 99             TO WS-KDFEL                              
083400             MOVE 'IDANSTNR'                                              
083500                                 TO RESP-IDELMT-ERROR                     
083600             MOVE '024'          TO RESP-IDMSG-ERROR                      
083700                                    RESP-IDMSG-ERROR-LINE                 
083800                                      (REQU-RAD-IND)                      
083900             MOVE REQU-IDANSTNR-NEW (REQU-RAD-IND)                        
084000                                 TO RESP-IDANSTNR-NEW                     
084100                                      (REQU-RAD-IND) (1:)                 
084200             INSPECT RESP-IDANSTNR-NEW (REQU-RAD-IND)                     
084300               REPLACING LEADING ZERO BY SPACE                            
084400           END-IF                                                         
084500         END-IF                                                           
084600         ADD +1                  TO REQU-RAD-IND                          
084700       END-PERFORM                                                        
084800       IF WS-KDFEL = ZERO AND                                             
084900          SW-AENDRA = NEJ                                                 
085000         MOVE 13                 TO WS-KDFEL                              
085100       END-IF                                                             
085200     END-IF                                                               
085300     .                                                                    
085400                                                                          
085500 BA-CHECK-IDANSTNR-OR-IDQUEUENR SECTION.                                  
085600                                                                          
085700     IF REQU-FLPREPRINT-KEY = 'Y' OR 'J'                                  
085800       IF REQU-IDQUEUENR-KEY = ALL '+' OR                                 
085900          REQU-IDQUEUENR-KEY IS NUMERIC                                   
086000         IF REQU-IDQUEUENR-KEY = ZERO OR ALL '+'                          
086100           MOVE 99000            TO WS-IDANSTNR-MIN                       
086200           MOVE 99999            TO WS-IDANSTNR-MAX                       
086300         ELSE                                                             
086400           COMPUTE WS-IDANSTNR-MIN = 99000 + REQU-IDQUEUENR-KEY           
086500           COMPUTE WS-IDANSTNR-MAX = 99000 + REQU-IDQUEUENR-KEY           
086600         END-IF                                                           
086700       ELSE                                                               
086800         MOVE NEJ                TO NYCKLAR-SW                            
086900         MOVE 'IDQUEUENR'        TO RESP-IDELMT-ERROR                     
087000       END-IF                                                             
087100     ELSE                                                                 
087200       IF REQU-IDANSTNR-KEY = ALL '+'                                     
087300         MOVE ZERO               TO WS-IDANSTNR-MIN                       
087400                                    WS-IDANSTNR-MAX                       
087500       ELSE                                                               
087600         IF REQU-IDANSTNR-KEY IS NUMERIC                                  
087700           MOVE REQU-IDANSTNR-KEY                                         
087800                                 TO WS-IDANSTNR-MIN                       
087900                                    WS-IDANSTNR-MAX                       
088000           MOVE JA               TO NYCKLAR-SW                            
088100           MOVE +2               TO NYCKEL-TYP                            
088200         ELSE                                                             
088300           MOVE ZERO             TO WS-IDANSTNR-MIN                       
088400                                    WS-IDANSTNR-MAX                       
088500           MOVE NEJ              TO NYCKLAR-SW                            
088600           MOVE 'IDANSTNR'       TO RESP-IDELMT-ERROR                     
088700         END-IF                                                           
088800       END-IF                                                             
088900     END-IF                                                               
089000     .                                                                    
089100                                                                          
089200 C-LAS-IDPRODNR SECTION.                                                  
089300                                                                          
089400     MOVE 'STA C-LAS'            TO PGM-POS                               
089500     MOVE NEJ                    TO WS-SLINGA-KLAR                        
089600     MOVE IDDISTR-WS             TO W-4A1-IDDISTR                         
089700     MOVE IDKUNDNR-WS            TO W-4A1-IDKUNDNR                        
089800     MOVE IDORDNR-WS             TO W-4A1-IDORDNR                         
089900                                                                          
090000     PERFORM IMS-GU-WDE4ASEQ                                              
090100                                                                          
090200     PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                                
090300                   SLINGA-KLAR                                            
090400       IF KORD-IDDC = REQU-IDDC-KEY AND                                   
090500          KORD-KVORDRAD-LEVPL   = 0                                       
090600         MOVE KORD-IDPRODNR      TO IDPRODNR-WS                           
090700         MOVE JA                 TO WS-SLINGA-KLAR                        
090800       ELSE                                                               
090900         PERFORM IMS-GN-WDE4ASEQ                                          
091000       END-IF                                                             
091100     END-PERFORM                                                          
091200                                                                          
091300     IF NOT SLINGA-KLAR                                                   
091400       MOVE 1                    TO WS-KDFEL                              
091500     ELSE                                                                 
091600       MOVE KORD-IDORDER         TO W-IDORDER-WDQ3                        
091700       MOVE KORD-IDDC            TO W-IDDC-WDQ3                           
091800       MOVE KORD-IDPRODNR        TO W-IDPRODNR-WDQ3                       
091900       MOVE KORD-IDPLKLST        TO W-IDPLKLST-WDQ3                       
092000       PERFORM IMS-GHU-ORQA-WDQ301                                        
092100       IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW') AND                         
092200          ODEL-IDPRC = '9998'                                             
092300         MOVE 3                  TO WS-KDFEL                              
092400       END-IF                                                             
092500     END-IF                                                               
092600     .                                                                    
092700                                                                          
092800 D-UPPDATERA SECTION.                                                     
092900                                                                          
093000     MOVE 'STA D-UPP'            TO PGM-POS                               
093100     IF SW-AENDRA = JA                                                    
093200       IF WS-KDFEL = ZERO                                                 
093300         PERFORM DA-UPPDATERA-PACKARE                                     
093400         IF ENDAST-ANSTNR                                                 
093500           PERFORM DB-VISA-BILD                                           
093600         ELSE                                                             
093700           IF ANSTNR-ORDERID                                              
093800             PERFORM DC-VISA-BILD                                         
093900           ELSE                                                           
094000             IF ENDAST-ORDERID                                            
094100               PERFORM DD-VISA-BILD                                       
094200             END-IF                                                       
094300           END-IF                                                         
094400         END-IF                                                           
094500       END-IF                                                             
094600     END-IF                                                               
094700     .                                                                    
094800                                                                          
094900 DA-UPPDATERA-PACKARE SECTION.                                            
095000                                                                          
095100     MOVE 'STA DA-UPP'           TO PGM-POS                               
095200     MOVE 1                      TO WS-RAD-IND                            
095300     PERFORM UNTIL WS-RAD-IND > REQU-KVRADER  OR                          
095400                   WS-RAD-IND > 500                                       
095500       MOVE REQU-IDANSTNR-NEW (WS-RAD-IND)                                
095600                                 TO WS-JFR-IDANSTNR-5                     
095700                                                                          
095800       IF WS-JFR-IDANSTNR-5 NOT = ALL '+'                                 
095900         IF WS-JFR-IDANSTNR-5  NUMERIC                                    
096000           CONTINUE                                                       
096100         ELSE                                                             
096200           MOVE    99            TO WS-KDFEL                              
096300           MOVE 'IDANSTNR'       TO RESP-IDELMT-ERROR                     
096400           MOVE '024'            TO RESP-IDMSG-ERROR                      
096500                                    RESP-IDMSG-ERROR-LINE                 
096600                                      (WS-RAD-IND)                        
096700           MOVE REQU-IDANSTNR-NEW (REQU-RAD-IND)                          
096800                                 TO RESP-IDANSTNR-NEW                     
096900                                      (REQU-RAD-IND) (1:)                 
097000           INSPECT RESP-IDANSTNR-NEW (REQU-RAD-IND)                       
097100             REPLACING LEADING ZERO BY SPACE                              
097200         END-IF                                                           
097300                                                                          
097400         MOVE REQU-IDPRODNR (WS-RAD-IND)                                  
097500                                 TO WS-IDPRODNR-X7                        
097600         IF WS-IDPRODNR-X7 = ALL '+'                                      
097700           MOVE ZERO             TO W-411-IDPRODNR-MIN                    
097800                                    W-411-IDPRODNR-MAX                    
097900         ELSE                                                             
098000           INSPECT WS-IDPRODNR-X7 REPLACING ALL '+' BY ZERO               
098100           IF WS-IDPRODNR-X7 NUMERIC                                      
098200             MOVE REQU-IDPRODNR (WS-RAD-IND)                              
098300                                 TO W-411-IDPRODNR-MIN                    
098400                                    W-411-IDPRODNR-MAX                    
098500           ELSE                                                           
098600             MOVE  98            TO WS-KDFEL                              
098700             MOVE 'IDPRODNR'     TO RESP-IDELMT-ERROR                     
098800             MOVE '024'          TO RESP-IDMSG-ERROR                      
098900                                    RESP-IDMSG-ERROR-LINE                 
099000                                      (WS-RAD-IND)                        
099100*WS-KDFEL = 98  FEL-X = 024. NOT NUMERIC                                  
099200           END-IF                                                         
099300         END-IF                                                           
099400                                                                          
099500         IF REQU-IDRADNR-ORD-TOM (WS-RAD-IND) = ALL '+'                   
099600           MOVE ZERO             TO W-411-IDPURAD-MIN                     
099700           MOVE 9999             TO W-411-IDPURAD-MAX                     
099800         ELSE                                                             
099900           MOVE REQU-IDRADNR-ORD-TOM (WS-RAD-IND)                         
100000                                 TO W-411-IDPURAD-MIN                     
100100                                    W-411-IDPURAD-MAX                     
100200         END-IF                                                           
100300         PERFORM IMS-GU-WDE411-01-BSEQ                                    
100400         IF SEGMENT-FINNS                                                 
100500           MOVE KORD-IDDISTR     TO W-401-IDDISTR                         
100600                                    W-4A1-IDDISTR                         
100700                                    WS-SPARA-IDDISTR                      
100800           MOVE KORD-IDKUNDNR    TO W-401-IDKUNDNR                        
100900                                    W-4A1-IDKUNDNR                        
101000                                    WS-SPARA-IDKUNDNR                     
101100           MOVE KORD-IDKUNDRF    TO W-401-IDKUNDRF                        
101200                                    W-4A1-IDKUNDRF                        
101300                                    WS-SPARA-IDKUNDRF                     
101400           MOVE WS-SPARA-IDORDNR-ALFA                                     
101500                                 TO WS-SPARA-IDORDNR                      
101600           MOVE KORD-IDORDER     TO WS-SPARA-IDORDER                      
101700           MOVE KORD-IDPRODNR    TO W-401-IDPRODNR                        
101800                                    WS-SPARA-IDPRODNR                     
101900           MOVE KORD-IDPLKLST    TO W-401-IDPLKLST                        
102000                                    WS-SPARA-IDPLKLST                     
102100           PERFORM DAA-KONTROLL-RAPPORTERING                              
102200         ELSE                                                             
102300           MOVE LINES-NOT-FOUND  TO WS-KDFEL                              
102400         END-IF                                                           
102500                                                                          
102600         IF WS-KDFEL = ZERO                                               
102700           PERFORM IMS-GHU-WDE401                                         
102800           MOVE 000              TO FILLER-IDANST                         
102900           MOVE WS-JFR-IDANSTNR  TO KORD-IDUSER                           
103000           PERFORM IMS-REPL-WDE401                                        
103100                                                                          
103200           PERFORM DAE-UPPDAT-ODEL                                        
103300                                                                          
103400           PERFORM DAC-UPPDAT-PRODTAB                                     
103500                                                                          
103600           PERFORM DAD-UPPDAT-KOLLIREG                                    
103700         END-IF                                                           
103800       END-IF                                                             
103900                                                                          
104000       ADD 1                     TO WS-RAD-IND                            
104100     END-PERFORM                                                          
104200     .                                                                    
104300                                                                          
104400 DAA-KONTROLL-RAPPORTERING SECTION.                                       
104500     MOVE 'STA DAA-KON'          TO PGM-POS                               
104600                                                                          
104700     PERFORM IMS-GU-WDE401                                                
104800                                                                          
104900     IF KORD-KVORDRAD-PACK = KORD-KVORDRAD + KORD-KVORDRAD-LEVPL          
105000       MOVE 8                    TO WS-KDFEL                              
105100     END-IF                                                               
105200                                                                          
105300     MOVE KORD-IDORDER           TO W-IDORDER-WDQ3                        
105400     MOVE KORD-IDDC              TO W-IDDC-WDQ3                           
105500     MOVE KORD-IDPRODNR          TO W-IDPRODNR-WDQ3                       
105600     MOVE KORD-IDPLKLST          TO W-IDPLKLST-WDQ3                       
105700     PERFORM IMS-GHU-ORQA-WDQ301                                          
105800     IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW')                               
105900     AND ODEL-IDPRC = '9998'                                              
106000       MOVE 3                    TO WS-KDFEL                              
106100     END-IF                                                               
106200                                                                          
106300     IF WS-KDFEL = 0                                                      
106400       IF KORD-IDUSER > ZERO                                              
106500         MOVE 'N'                TO WS-SLINGA-KLAR                        
106600         PERFORM IMS-GNP-ORAD                                             
106700         PERFORM UNTIL ORAD-SAKNAS OR                                     
106800                       SLINGA-KLAR                                        
106900           IF ORAD-KDRADSTA > 3                                           
107000             MOVE JA             TO WS-SLINGA-KLAR                        
107100           ELSE                                                           
107200             PERFORM IMS-GNP-ORAD                                         
107300           END-IF                                                         
107400         END-PERFORM                                                      
107500         IF SLINGA-KLAR AND                                               
107600            WS-JFR-IDANSTNR = ZERO AND                                    
107700            ORAD-IDLEVNR NOT = '10987' AND                                
107800            ORAD-IDLEVNR NOT = 'BQ8VA'                                    
107900           MOVE 8                TO WS-KDFEL                              
108000         END-IF                                                           
108100       END-IF                                                             
108200     END-IF                                                               
108300     .                                                                    
108400                                                                          
108500 DAC-UPPDAT-PRODTAB SECTION.                                              
108600                                                                          
108700     MOVE 'STA DAC-UPP'          TO PGM-POS                               
108800     MOVE REQU-IDDC-KEY          TO W-4447-IDDC                           
108900     MOVE ODEL-IDPRC             TO W-4448-IDPRC                          
109000     PERFORM IMS-GU-4448                                                  
109100                                                                          
109200     MOVE REQU-IDDC-KEY          TO W-4487-IDDC                           
109300     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
109400     PERFORM IMS-GU-4488                                                  
109500                                                                          
109600     IF SEGMENT-FINNS                                                     
109700       MOVE ODEL-DARFS           TO W-4490-DARFS                          
109800       MOVE ODEL-IDPRODNR        TO W-4490-IDPRODNR                       
109900       MOVE ODEL-IDPLKLST        TO W-4490-IDPLKLST                       
110000       PERFORM IMS-GHU-4490                                               
110100                                                                          
110200       IF SEGMENT-FINNS                                                   
110300         MOVE WS-JFR-IDANSTNR    TO 4490-IDUSER                           
110400         PERFORM IMS-REPL-4490                                            
110500       END-IF                                                             
110600     END-IF                                                               
110700     .                                                                    
110800                                                                          
110900 DAE-UPPDAT-ODEL SECTION.                                                 
111000     MOVE 'STA DAE-UPP'          TO PGM-POS                               
111100                                                                          
111200     MOVE WS-SPARA-IDORDER       TO W-IDORDER-WDQ3                        
111300     MOVE REQU-IDDC-KEY          TO W-IDDC-WDQ3                           
111400     MOVE WS-SPARA-IDPRODNR      TO W-IDPRODNR-WDQ3                       
111500     MOVE WS-SPARA-IDPLKLST      TO W-IDPLKLST-WDQ3                       
111600                                                                          
111700     PERFORM IMS-GHU-ORQA-WDQ301                                          
111800                                                                          
111900     MOVE WS-JFR-IDANSTNR        TO ODEL-IDUSER                           
112000                                                                          
112100     PERFORM IMS-REPL-ORQA-WDQ301                                         
112200     .                                                                    
112300                                                                          
112400 DAD-UPPDAT-KOLLIREG SECTION.                                             
112500     MOVE 'DAD-UPPDAT-KOLLIREG ' TO PGM-POS                               
112600                                                                          
112700     MOVE 1                      TO TAB-INX                               
112800     PERFORM UNTIL TAB-INX > MAX-TAB-INX                                  
112900       MOVE ZERO                 TO TAB-IDKOLLI(TAB-INX)                  
113000       ADD 1                     TO TAB-INX                               
113100     END-PERFORM                                                          
113200*                                                                         
113300* MOVE WDE4F- KEYS                                                        
113400     MOVE REQU-IDPRODNR(WS-RAD-IND)                                       
113500                                 TO W-IDPRODNR-WDE4F-MAX                  
113600                                    W-IDPRODNR-WDE4F-MIN                  
113700     MOVE WS-SPARA-IDPLKLST      TO W-IDPLKST-WDE4F                       
113800*                                                                         
113900     PERFORM IMS-GU-WDE4F                                                 
114000     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DB                            
114100       MOVE NEJ                  TO KOLLI-FINNS-I-TAB-SW                  
114200       MOVE 1                    TO TAB-INX                               
114300       PERFORM UNTIL TAB-INX > MAX-TAB-INX                                
114400         IF TAB-IDKOLLI (TAB-INX) = ZERO                                  
114500           MOVE SEQF-IDKOLLI     TO TAB-IDKOLLI(TAB-INX)                  
114600           MOVE MAX-TAB-INX      TO TAB-INX                               
114700         ELSE                                                             
114800           IF SEQF-IDKOLLI = TAB-IDKOLLI (TAB-INX)                        
114900             MOVE JA             TO KOLLI-FINNS-I-TAB-SW                  
115000             MOVE MAX-TAB-INX    TO TAB-INX                               
115100           END-IF                                                         
115200         END-IF                                                           
115300         ADD 1 TO TAB-INX                                                 
115400       END-PERFORM                                                        
115500                                                                          
115600       IF KOLLI-FINNS-I-TAB-SW = NEJ                                      
115700         MOVE REQU-IDPRODNR(WS-RAD-IND)                                   
115800                                 TO W-601-IDPRODNR                        
115900         MOVE SEQF-IDKOLLI       TO W-611-IDKOLLI                         
116000                                                                          
116100         PERFORM IMS-GHU-WDE611                                           
116200                                                                          
116300         MOVE WS-JFR-IDANSTNR-5  TO KOLLI-IDPLOCK                         
116400         PERFORM IMS-REPL-WDE611                                          
116500       END-IF                                                             
116600                                                                          
116700       PERFORM IMS-GN-WDE4F                                               
116800     END-PERFORM                                                          
116900     .                                                                    
117000                                                                          
117100                                                                          
117200 DB-VISA-BILD SECTION.                                                    
117300     MOVE 'STA DB-VISA'          TO PGM-POS                               
117400                                                                          
117500     MOVE 'N'                    TO WS-TRAEFF-PACKARE                     
117600     MOVE 1                      TO WS-RAD-IND                            
117700                                                                          
117800     MOVE REQU-IDDC-KEY          TO W-4487-IDDC                           
117900                                                                          
118000     MOVE LOW-VALUE              TO W-4488-KDPRCGRP                       
118100     PERFORM IMS-GU-4488-FIRST                                            
118200                                                                          
118300     IF SEG-4488-FINNS                                                    
118400       PERFORM IMS-GNP-4490                                               
118500     END-IF                                                               
118600                                                                          
118700     PERFORM UNTIL SEG-4488-SAKNAS OR                                     
118800                   WS-RAD-IND > 500                                       
118900       IF 4487-IDDC = REQU-IDDC-KEY                                       
119000                                                                          
119100         PERFORM UNTIL SEG-4490-SAKNAS OR                                 
119200                       WS-RAD-IND > 500                                   
119300           MOVE 4490-IDPRODNR    TO WS-SPARA-IDPRODNR                     
119400           MOVE 4490-IDPLKLST    TO WS-SPARA-IDPLKLST                     
119500           MOVE 4490-IDUSER      TO WS-JFR-IDANSTNR                       
119600                                                                          
119700           IF WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                    
119800              WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                        
119900             MOVE 'J'            TO WS-TRAEFF-PACKARE                     
120000             MOVE 4490-IDPRODNR  TO W-601-IDPRODNR                        
120100             PERFORM IMS-GU-WDE401-ESEQ                                   
120200             IF SEGMENT-FINNS                                             
120300               MOVE KORD-IDDISTR TO W-401-IDDISTR                         
120400                                    WS-SPARA-IDDISTR                      
120500               MOVE KORD-IDKUNDNR                                         
120600                                 TO W-401-IDKUNDNR                        
120700                                    WS-SPARA-IDKUNDNR                     
120800               MOVE KORD-IDKUNDRF                                         
120900                                 TO W-401-IDKUNDRF                        
121000                                    WS-SPARA-IDKUNDRF                     
121100               MOVE WS-SPARA-IDORDNR-ALFA                                 
121200                                 TO WS-SPARA-IDORDNR                      
121300               MOVE KORD-IDPRODNR                                         
121400                                 TO W-401-IDPRODNR                        
121500               MOVE KORD-IDPLKLST                                         
121600                                 TO W-401-IDPLKLST                        
121700               PERFORM IMS-GU-WDE401                                      
121800                                                                          
121900               MOVE KORD-IDPRODNR                                         
122000                                 TO WS-JFR-IDPRODNR                       
122100               MOVE KORD-IDORDER TO WS-SPARA-IDORDER                      
122200               MOVE 'J'          TO WS-ORDERDEL-KLAR                      
122300               MOVE 'N'          TO WS-ORDERDEL-STARTAD                   
122400               PERFORM IMS-GNP-ORAD                                       
122500                                                                          
122600               IF ORAD-FINNS                                              
122700                 MOVE ORAD-IDPURAD                                        
122800                                 TO WS-START-IDPURAD                      
122900                                    WS-STOPP-IDPURAD                      
123000                 IF ORAD-KDRADSTA < 4                                     
123100                   MOVE 'N'      TO WS-ORDERDEL-KLAR                      
123200                 END-IF                                                   
123300                                                                          
123400                 IF ORAD-KDRADSTA > 3                                     
123500                   MOVE 'J'      TO WS-ORDERDEL-STARTAD                   
123600                 END-IF                                                   
123700               END-IF                                                     
123800                                                                          
123900               PERFORM UNTIL ORAD-SAKNAS                                  
124000                 PERFORM IMS-GNP-ORAD                                     
124100                                                                          
124200                 IF ORAD-FINNS                                            
124300                   MOVE ORAD-IDPURAD                                      
124400                                 TO WS-STOPP-IDPURAD                      
124500                                                                          
124600                   IF ORAD-KDRADSTA < 4                                   
124700                     MOVE 'N'    TO WS-ORDERDEL-KLAR                      
124800                   END-IF                                                 
124900                                                                          
125000                   IF ORAD-KDRADSTA > 3                                   
125100                      MOVE 'J'   TO WS-ORDERDEL-STARTAD                   
125200                   END-IF                                                 
125300                 END-IF                                                   
125400               END-PERFORM                                                
125500                                                                          
125600               PERFORM S10-FYLL-I-RAD                                     
125700             END-IF                                                       
125800           END-IF                                                         
125900                                                                          
126000           PERFORM IMS-GNP-4490                                           
126100           MOVE 4490-IDUSER      TO WS-JFR-IDANSTNR                       
126200         END-PERFORM                                                      
126300       END-IF                                                             
126400                                                                          
126500       IF SEG-4490-SAKNAS OR WS-RAD-IND < 500                             
126600         PERFORM IMS-GN-4488                                              
126700         PERFORM UNTIL SEG-4488-SAKNAS                                    
126800                       OR                                                 
126900                       4487-IDDC = REQU-IDDC-KEY                          
127000           PERFORM IMS-GN-4488                                            
127100         END-PERFORM                                                      
127200         IF SEG-4488-FINNS                                                
127300           PERFORM IMS-GNP-4490                                           
127400         END-IF                                                           
127500       END-IF                                                             
127600     END-PERFORM                                                          
127700                                                                          
127800     IF NOT TRAEFF-PACKARE                                                
127900       MOVE 4                    TO WS-KDFEL                              
128000*WS-KDFEL = 4 = FEL-4 = 711. ANGIVEN PACKARE SAKNAS PÅ ORDERN             
128100     ELSE                                                                 
128200       IF WS-RAD-IND = 501 AND SEG-4490-FINNS                             
128300         PERFORM UNTIL SEG-4488-SAKNAS OR                                 
128400                       WS-KDFEL        = 20                               
128500           PERFORM UNTIL SEG-4490-SAKNAS  OR                              
128600                         WS-KDFEL     = 20                                
128700             MOVE 4490-IDUSER    TO WS-JFR-IDANSTNR                       
128800             IF 4487-IDDC = REQU-IDDC-KEY AND                             
128900                WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                  
129000                WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                      
129100               MOVE 20           TO WS-KDFEL                              
129200*WS-KDFEL = 20 = MED-1 = 778. FLER RADER FINNS                            
129300             ELSE                                                         
129400               PERFORM IMS-GNP-4490                                       
129500             END-IF                                                       
129600           END-PERFORM                                                    
129700           IF WS-KDFEL = 20                                               
129800             CONTINUE                                                     
129900           ELSE                                                           
130000             PERFORM IMS-GN-4488                                          
130100             IF SEG-4488-FINNS                                            
130200               MOVE 4487-IDDC    TO WS-4487-IDDC                          
130300               PERFORM IMS-GNP-4490                                       
130400             END-IF                                                       
130500           END-IF                                                         
130600         END-PERFORM                                                      
130700       END-IF                                                             
130800     END-IF                                                               
130900     .                                                                    
131000                                                                          
131100 DC-VISA-BILD SECTION.                                                    
131200     MOVE 'STA DC-VISA'          TO PGM-POS                               
131300                                                                          
131400     MOVE 1                      TO WS-RAD-IND                            
131500     MOVE IDPRODNR-WS            TO W-411-IDPRODNR-MIN                    
131600                                    W-411-IDPRODNR-MAX                    
131700     MOVE 1                      TO W-411-IDPURAD-MIN                     
131800     MOVE 99999                  TO W-411-IDPURAD-MAX                     
131900     PERFORM IMS-GU-WDE411-01-BSEQ                                        
132000                                                                          
132100     IF SEGMENT-FINNS                                                     
132200       MOVE KORD-IDDISTR         TO W-4A1-IDDISTR                         
132300       MOVE KORD-IDKUNDNR        TO W-4A1-IDKUNDNR                        
132400       MOVE KORD-IDKUNDRF        TO W-4A1-IDKUNDRF                        
132500       PERFORM IMS-GU-WDE4ASEQ                                            
132600                                                                          
132700       IF KUNDORDER-SEK-FINNS                                             
132800         PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                            
132900                       WS-RAD-IND > 500                                   
133000           MOVE KORD-IDDISTR     TO W-401-IDDISTR                         
133100                                    WS-SPARA-IDDISTR                      
133200           MOVE KORD-IDKUNDNR    TO W-401-IDKUNDNR                        
133300                                    WS-SPARA-IDKUNDNR                     
133400           MOVE KORD-IDKUNDRF    TO W-401-IDKUNDRF                        
133500                                    WS-SPARA-IDKUNDRF                     
133600           MOVE WS-SPARA-IDORDNR-ALFA                                     
133700                                 TO WS-SPARA-IDORDNR                      
133800           MOVE KORD-IDPRODNR    TO W-401-IDPRODNR                        
133900                                    WS-SPARA-IDPRODNR                     
134000           MOVE KORD-IDPLKLST    TO W-401-IDPLKLST                        
134100                                    WS-SPARA-IDPLKLST                     
134200           MOVE KORD-IDORDER     TO WS-SPARA-IDORDER                      
134300           PERFORM IMS-GU-WDE401                                          
134400           MOVE KORD-IDPRODNR    TO WS-JFR-IDPRODNR                       
134500           MOVE KORD-IDUSER      TO WS-JFR-IDANSTNR                       
134600                                                                          
134700           IF WS-JFR-IDPRODNR   = IDPRODNR-WS AND                         
134800              WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                    
134900              WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                        
135000             MOVE 'J'            TO WS-ORDERDEL-KLAR                      
135100             MOVE 'N'            TO WS-ORDERDEL-STARTAD                   
135200             PERFORM IMS-GNP-ORAD                                         
135300                                                                          
135400             IF ORAD-FINNS                                                
135500               MOVE ORAD-IDPURAD TO WS-START-IDPURAD                      
135600                                    WS-STOPP-IDPURAD                      
135700               IF ORAD-KDRADSTA < 4                                       
135800                 MOVE 'N'        TO WS-ORDERDEL-KLAR                      
135900               END-IF                                                     
136000                                                                          
136100               IF ORAD-KDRADSTA > 3                                       
136200                 MOVE 'J'        TO WS-ORDERDEL-STARTAD                   
136300               END-IF                                                     
136400             END-IF                                                       
136500                                                                          
136600             PERFORM UNTIL ORAD-SAKNAS                                    
136700               PERFORM IMS-GNP-ORAD                                       
136800                                                                          
136900               IF ORAD-FINNS                                              
137000                 MOVE ORAD-IDPURAD                                        
137100                                 TO WS-STOPP-IDPURAD                      
137200                                                                          
137300                 IF ORAD-KDRADSTA < 4                                     
137400                   MOVE 'N'      TO WS-ORDERDEL-KLAR                      
137500                 END-IF                                                   
137600                                                                          
137700                 IF ORAD-KDRADSTA > 3                                     
137800                   MOVE 'J'      TO WS-ORDERDEL-STARTAD                   
137900                 END-IF                                                   
138000               END-IF                                                     
138100             END-PERFORM                                                  
138200                                                                          
138300             PERFORM S10-FYLL-I-RAD                                       
138400           END-IF                                                         
138500                                                                          
138600           PERFORM IMS-GN-WDE4ASEQ                                        
138700                                                                          
138800         END-PERFORM                                                      
138900                                                                          
139000         IF WS-RAD-IND = 501 AND SEGMENT-FINNS                            
139100           MOVE 20               TO WS-KDFEL                              
139200         END-IF                                                           
139300       ELSE                                                               
139400         MOVE 1                  TO WS-KDFEL                              
139500       END-IF                                                             
139600     END-IF                                                               
139700     .                                                                    
139800                                                                          
139900 DD-VISA-BILD SECTION.                                                    
140000     MOVE 'STA DD-VISA'          TO PGM-POS                               
140100                                                                          
140200     MOVE 1                      TO WS-RAD-IND                            
140300     MOVE IDPRODNR-WS            TO W-411-IDPRODNR-MIN                    
140400                                    W-411-IDPRODNR-MAX                    
140500     MOVE 1                      TO W-411-IDPURAD-MIN                     
140600     MOVE 99999                  TO W-411-IDPURAD-MAX                     
140700     PERFORM IMS-GU-WDE411-01-BSEQ                                        
140800                                                                          
140900     IF SEGMENT-FINNS                                                     
141000       MOVE KORD-IDDISTR         TO W-4A1-IDDISTR                         
141100       MOVE KORD-IDKUNDNR        TO W-4A1-IDKUNDNR                        
141200       MOVE KORD-IDKUNDRF        TO W-4A1-IDKUNDRF                        
141300       PERFORM IMS-GU-WDE4ASEQ                                            
141400                                                                          
141500       IF KUNDORDER-SEK-FINNS                                             
141600         PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                            
141700                       WS-RAD-IND > 500                                   
141800           MOVE KORD-IDDISTR     TO W-401-IDDISTR                         
141900                                    WS-SPARA-IDDISTR                      
142000           MOVE KORD-IDKUNDNR    TO W-401-IDKUNDNR                        
142100                                    WS-SPARA-IDKUNDNR                     
142200           MOVE KORD-IDKUNDRF    TO W-401-IDKUNDRF                        
142300                                    WS-SPARA-IDKUNDRF                     
142400           MOVE WS-SPARA-IDORDNR-ALFA                                     
142500                                 TO WS-SPARA-IDORDNR                      
142600           MOVE KORD-IDPRODNR    TO W-401-IDPRODNR                        
142700                                    WS-SPARA-IDPRODNR                     
142800           MOVE KORD-IDPLKLST    TO W-401-IDPLKLST                        
142900                                    WS-SPARA-IDPLKLST                     
143000           MOVE KORD-IDORDER     TO WS-SPARA-IDORDER                      
143100           PERFORM IMS-GU-WDE401                                          
143200           MOVE KORD-IDPRODNR    TO WS-JFR-IDPRODNR                       
143300                                                                          
143400           IF WS-JFR-IDPRODNR = IDPRODNR-WS                               
143500             MOVE 'J'            TO WS-ORDERDEL-KLAR                      
143600             MOVE 'N'            TO WS-ORDERDEL-STARTAD                   
143700             PERFORM IMS-GNP-ORAD                                         
143800                                                                          
143900             IF ORAD-FINNS                                                
144000               MOVE ORAD-IDPURAD TO WS-START-IDPURAD                      
144100                                    WS-STOPP-IDPURAD                      
144200               IF ORAD-KDRADSTA < 4                                       
144300                 MOVE 'N'        TO WS-ORDERDEL-KLAR                      
144400               END-IF                                                     
144500                                                                          
144600               IF ORAD-KDRADSTA > 3                                       
144700                 MOVE 'J'        TO WS-ORDERDEL-STARTAD                   
144800               END-IF                                                     
144900             END-IF                                                       
145000                                                                          
145100             PERFORM UNTIL ORAD-SAKNAS                                    
145200               PERFORM IMS-GNP-ORAD                                       
145300                                                                          
145400               IF ORAD-FINNS                                              
145500                 MOVE ORAD-IDPURAD                                        
145600                                 TO WS-STOPP-IDPURAD                      
145700                 IF ORAD-KDRADSTA < 4                                     
145800                   MOVE 'N'      TO WS-ORDERDEL-KLAR                      
145900                 END-IF                                                   
146000                                                                          
146100                 IF ORAD-KDRADSTA > 3                                     
146200                   MOVE 'J'      TO WS-ORDERDEL-STARTAD                   
146300                 END-IF                                                   
146400               END-IF                                                     
146500             END-PERFORM                                                  
146600                                                                          
146700             PERFORM S10-FYLL-I-RAD                                       
146800           END-IF                                                         
146900                                                                          
147000           PERFORM IMS-GN-WDE4ASEQ                                        
147100                                                                          
147200         END-PERFORM                                                      
147300                                                                          
147400         IF WS-RAD-IND = 501 AND SEGMENT-FINNS                            
147500           MOVE 20               TO WS-KDFEL                              
147600         END-IF                                                           
147700       ELSE                                                               
147800         MOVE 1                  TO WS-KDFEL                              
147900       END-IF                                                             
148000     END-IF                                                               
148100     .                                                                    
148200                                                                          
148300 E-BEHANDLA-ENDAST-ANSTNR SECTION.                                        
148400     MOVE 'STA E-BEHA'           TO PGM-POS                               
148500                                                                          
148600     MOVE 'N'                    TO WS-TRAEFF-PACKARE                     
148700     MOVE 1                      TO WS-RAD-IND                            
148800                                                                          
148900     MOVE REQU-IDDC-KEY          TO W-4487-IDDC                           
149000     MOVE LOW-VALUE              TO W-4488-KDPRCGRP                       
149100                                                                          
149200     PERFORM IMS-GU-4488-FIRST                                            
149300     MOVE +1                     TO ANTAL-LAS                             
149400     IF SEG-4488-FINNS                                                    
149500       IF REQU-KDPGMACT = 'S'                                             
149600         PERFORM IMS-GNP-4490                                             
149700         MOVE +1                 TO ANTAL-LAS                             
149800       END-IF                                                             
149900     END-IF                                                               
150000     MOVE 'STA E-PERFORM'        TO PGM-POS                               
150100     PERFORM UNTIL SEG-4488-SAKNAS OR                                     
150200                   WS-RAD-IND > 500                                       
150300       IF 4487-IDDC = REQU-IDDC-KEY                                       
150400                                                                          
150500         PERFORM UNTIL SEG-4490-SAKNAS OR                                 
150600                       WS-RAD-IND > 500                                   
150700           MOVE 4490-IDPRODNR    TO WS-SPARA-IDPRODNR                     
150800           MOVE 4490-IDPLKLST    TO WS-SPARA-IDPLKLST                     
150900           MOVE 4490-IDUSER      TO WS-JFR-IDANSTNR                       
151000                                                                          
151100           IF WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                    
151200              WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                        
151300             MOVE 'J'            TO WS-TRAEFF-PACKARE                     
151400             MOVE 4490-IDPRODNR  TO W-601-IDPRODNR                        
151500             PERFORM IMS-GU-WDE401-ESEQ                                   
151600             IF SEGMENT-FINNS                                             
151700               MOVE KORD-IDDISTR TO W-401-IDDISTR                         
151800                                    WS-SPARA-IDDISTR                      
151900               MOVE KORD-IDKUNDNR                                         
152000                                 TO W-401-IDKUNDNR                        
152100                                    WS-SPARA-IDKUNDNR                     
152200               MOVE KORD-IDKUNDRF                                         
152300                                 TO W-401-IDKUNDRF                        
152400                                    WS-SPARA-IDKUNDRF                     
152500                                                                          
152600               MOVE WS-SPARA-IDORDNR-ALFA                                 
152700                                 TO WS-SPARA-IDORDNR                      
152800               MOVE WS-SPARA-IDPRODNR                                     
152900                                 TO W-401-IDPRODNR                        
153000               MOVE WS-SPARA-IDPLKLST                                     
153100                                 TO W-401-IDPLKLST                        
153200               MOVE KORD-IDPRODNR                                         
153300                                 TO W-401-IDPRODNR                        
153400               MOVE KORD-IDPLKLST                                         
153500                                 TO W-401-IDPLKLST                        
153600               PERFORM IMS-GU-WDE401                                      
153700               MOVE +1           TO ANTAL-LAS                             
153800               MOVE KORD-IDPRODNR                                         
153900                                 TO WS-JFR-IDPRODNR                       
154000               MOVE KORD-IDORDER TO WS-SPARA-IDORDER                      
154100               MOVE 'J'          TO WS-ORDERDEL-KLAR                      
154200               MOVE 'N'          TO WS-ORDERDEL-STARTAD                   
154300               PERFORM IMS-GNP-ORAD                                       
154400               MOVE +1           TO ANTAL-LAS                             
154500               IF ORAD-FINNS                                              
154600                 MOVE ORAD-IDPURAD                                        
154700                                 TO WS-START-IDPURAD                      
154800                                    WS-STOPP-IDPURAD                      
154900                 IF ORAD-KDRADSTA < 4                                     
155000                   MOVE 'N'      TO WS-ORDERDEL-KLAR                      
155100                 END-IF                                                   
155200                                                                          
155300                 IF ORAD-KDRADSTA > 3                                     
155400                   MOVE 'J'      TO WS-ORDERDEL-STARTAD                   
155500                 END-IF                                                   
155600               END-IF                                                     
155700                                                                          
155800               PERFORM UNTIL ORAD-SAKNAS                                  
155900                 PERFORM IMS-GNP-ORAD                                     
156000                 MOVE +1         TO ANTAL-LAS                             
156100                 IF ORAD-FINNS                                            
156200                   MOVE ORAD-IDPURAD                                      
156300                                 TO WS-STOPP-IDPURAD                      
156400                   IF ORAD-KDRADSTA < 4                                   
156500                     MOVE 'N'    TO WS-ORDERDEL-KLAR                      
156600                   END-IF                                                 
156700                                                                          
156800                   IF ORAD-KDRADSTA > 3                                   
156900                     MOVE 'J'    TO WS-ORDERDEL-STARTAD                   
157000                   END-IF                                                 
157100                 END-IF                                                   
157200               END-PERFORM                                                
157300               MOVE 'INNAN S10'  TO PGM-POS                               
157400               PERFORM S10-FYLL-I-RAD                                     
157500             END-IF                                                       
157600           END-IF                                                         
157700                                                                          
157800           PERFORM IMS-GNP-4490                                           
157900           MOVE +1               TO ANTAL-LAS                             
158000           MOVE 4490-IDUSER      TO WS-JFR-IDANSTNR                       
158100         END-PERFORM                                                      
158200       END-IF                                                             
158300                                                                          
158400       IF SEG-4490-SAKNAS OR WS-RAD-IND < 500                             
158500         PERFORM IMS-GN-4488                                              
158600         MOVE +1                 TO ANTAL-LAS                             
158700         PERFORM UNTIL SEG-4488-SAKNAS                                    
158800                       OR                                                 
158900                       4487-IDDC = REQU-IDDC-KEY                          
159000           PERFORM IMS-GN-4488                                            
159100           MOVE +1               TO ANTAL-LAS                             
159200         END-PERFORM                                                      
159300         IF SEG-4488-FINNS                                                
159400           PERFORM IMS-GNP-4490                                           
159500           MOVE +1               TO ANTAL-LAS                             
159600         END-IF                                                           
159700       END-IF                                                             
159800     END-PERFORM                                                          
159900                                                                          
160000     IF NOT TRAEFF-PACKARE                                                
160100       MOVE 5                    TO WS-KDFEL                              
160200       MOVE 'IDUSER'             TO RESP-IDELMT-ERROR                     
160300*WS-KDFEL = 5 = FEL-5 = 712. INGA UTEST. ORDERDELAR FÖR PACKARE           
160400     ELSE                                                                 
160500       IF WS-RAD-IND = 501 AND SEG-4490-FINNS                             
160600         PERFORM UNTIL SEG-4488-SAKNAS OR                                 
160700                       WS-KDFEL        = 20                               
160800           PERFORM UNTIL  SEG-4490-SAKNAS  OR                             
160900                          WS-KDFEL     = 20                               
161000             MOVE 4490-IDUSER    TO WS-JFR-IDANSTNR                       
161100             IF 4487-IDDC = REQU-IDDC-KEY AND                             
161200                WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                  
161300                WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                      
161400               MOVE 20           TO WS-KDFEL                              
161500*WS-KDFEL = 20 = MED-1 = 778. FLER RADER FINNS                            
161600             ELSE                                                         
161700               PERFORM IMS-GNP-4490                                       
161800             END-IF                                                       
161900           END-PERFORM                                                    
162000           IF WS-KDFEL             = 20                                   
162100             CONTINUE                                                     
162200           ELSE                                                           
162300             PERFORM IMS-GN-4488                                          
162400             IF SEG-4488-FINNS                                            
162500               MOVE 4487-IDDC    TO WS-4487-IDDC                          
162600               PERFORM IMS-GNP-4490                                       
162700             END-IF                                                       
162800           END-IF                                                         
162900         END-PERFORM                                                      
163000                                                                          
163100       END-IF                                                             
163200     END-IF                                                               
163300     .                                                                    
163400                                                                          
163500 F-BEHANDLA-ANSTNR-ORDERID SECTION.                                       
163600     MOVE 'STA F-BEHA'           TO PGM-POS                               
163700                                                                          
163800     MOVE IDPRODNR-WS            TO W-411-IDPRODNR-MIN                    
163900                                    W-411-IDPRODNR-MAX                    
164000     MOVE 1                      TO W-411-IDPURAD-MIN                     
164100     MOVE 99999                  TO W-411-IDPURAD-MAX                     
164200     PERFORM IMS-GU-WDE411-01-BSEQ                                        
164300                                                                          
164400     IF SEGMENT-FINNS                                                     
164500       MOVE 1                    TO WS-RAD-IND                            
164600       MOVE KORD-IDDISTR         TO W-4A1-IDDISTR                         
164700       MOVE KORD-IDKUNDNR        TO W-4A1-IDKUNDNR                        
164800       MOVE KORD-IDKUNDRF        TO W-4A1-IDKUNDRF                        
164900       PERFORM IMS-GU-WDE4ASEQ                                            
165000                                                                          
165100       PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                              
165200                     WS-RAD-IND > 500                                     
165300         MOVE KORD-IDDISTR       TO W-401-IDDISTR                         
165400                                    WS-SPARA-IDDISTR                      
165500         MOVE KORD-IDKUNDNR      TO W-401-IDKUNDNR                        
165600                                    WS-SPARA-IDKUNDNR                     
165700         MOVE KORD-IDKUNDRF      TO W-401-IDKUNDRF                        
165800                                    WS-SPARA-IDKUNDRF                     
165900         MOVE WS-SPARA-IDORDNR-ALFA                                       
166000                                 TO WS-SPARA-IDORDNR                      
166100         MOVE KORD-IDPRODNR      TO W-401-IDPRODNR                        
166200                                    WS-SPARA-IDPRODNR                     
166300         MOVE KORD-IDPLKLST      TO W-401-IDPLKLST                        
166400                                    WS-SPARA-IDPLKLST                     
166500         MOVE KORD-IDORDER       TO WS-SPARA-IDORDER                      
166600         PERFORM IMS-GU-WDE401                                            
166700                                                                          
166800         IF SEGMENT-FINNS                                                 
166900           MOVE KORD-IDPRODNR    TO WS-JFR-IDPRODNR                       
167000           MOVE KORD-IDUSER      TO WS-JFR-IDANSTNR                       
167100         ELSE                                                             
167200           MOVE ZERO             TO WS-JFR-IDPRODNR                       
167300           MOVE ZERO             TO WS-JFR-IDANSTNR                       
167400         END-IF                                                           
167500                                                                          
167600         IF SEGMENT-FINNS AND                                             
167700            WS-JFR-IDPRODNR   = IDPRODNR-WS AND                           
167800            WS-JFR-IDANSTNR-5 >= WS-IDANSTNR-MIN AND                      
167900            WS-JFR-IDANSTNR-5 <= WS-IDANSTNR-MAX                          
168000           MOVE 'J'              TO WS-ORDERDEL-KLAR                      
168100           MOVE 'N'              TO WS-ORDERDEL-STARTAD                   
168200           PERFORM IMS-GNP-ORAD                                           
168300                                                                          
168400           IF ORAD-FINNS                                                  
168500             MOVE ORAD-IDPURAD   TO WS-START-IDPURAD                      
168600                                    WS-STOPP-IDPURAD                      
168700             IF ORAD-KDRADSTA < 4                                         
168800               MOVE 'N'          TO WS-ORDERDEL-KLAR                      
168900             END-IF                                                       
169000                                                                          
169100             IF ORAD-KDRADSTA > 3                                         
169200               MOVE 'J'          TO WS-ORDERDEL-STARTAD                   
169300             END-IF                                                       
169400           END-IF                                                         
169500                                                                          
169600           PERFORM UNTIL ORAD-SAKNAS                                      
169700             PERFORM IMS-GNP-ORAD                                         
169800                                                                          
169900             IF ORAD-FINNS                                                
170000               MOVE ORAD-IDPURAD TO WS-STOPP-IDPURAD                      
170100               IF ORAD-KDRADSTA < 4                                       
170200                 MOVE 'N'        TO WS-ORDERDEL-KLAR                      
170300               END-IF                                                     
170400                                                                          
170500               IF ORAD-KDRADSTA > 3                                       
170600                 MOVE 'J'        TO WS-ORDERDEL-STARTAD                   
170700               END-IF                                                     
170800             END-IF                                                       
170900           END-PERFORM                                                    
171000                                                                          
171100           PERFORM S10-FYLL-I-RAD                                         
171200         END-IF                                                           
171300                                                                          
171400         PERFORM IMS-GN-WDE4ASEQ                                          
171500                                                                          
171600       END-PERFORM                                                        
171700                                                                          
171800       IF WS-RAD-IND = 501 AND SEGMENT-FINNS                              
171900         MOVE 20                 TO WS-KDFEL                              
172000       END-IF                                                             
172100     ELSE                                                                 
172200       MOVE 1                    TO WS-KDFEL                              
172300     END-IF                                                               
172400     .                                                                    
172500                                                                          
172600 G-BEHANDLA-ENDAST-ORDERID SECTION.                                       
172700     MOVE 'STA G-BEHA'           TO PGM-POS                               
172800                                                                          
172900     MOVE IDPRODNR-WS            TO W-411-IDPRODNR-MIN                    
173000                                    W-411-IDPRODNR-MAX                    
173100     MOVE 1                      TO W-411-IDPURAD-MIN                     
173200     MOVE 99999                  TO W-411-IDPURAD-MAX                     
173300     PERFORM IMS-GU-WDE411-01-BSEQ                                        
173400                                                                          
173500     IF SEGMENT-FINNS                                                     
173600       MOVE 1                    TO WS-RAD-IND                            
173700       MOVE KORD-IDDISTR         TO W-4A1-IDDISTR                         
173800       MOVE KORD-IDKUNDNR        TO W-4A1-IDKUNDNR                        
173900       MOVE KORD-IDKUNDRF        TO W-4A1-IDKUNDRF                        
174000       PERFORM IMS-GU-WDE4ASEQ                                            
174100                                                                          
174200                                                                          
174300       PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                              
174400                     WS-RAD-IND > 500                                     
174500         MOVE KORD-IDDISTR       TO W-401-IDDISTR                         
174600                                    WS-SPARA-IDDISTR                      
174700         MOVE KORD-IDKUNDNR      TO W-401-IDKUNDNR                        
174800                                    WS-SPARA-IDKUNDNR                     
174900         MOVE KORD-IDKUNDRF      TO W-401-IDKUNDRF                        
175000                                    WS-SPARA-IDKUNDRF                     
175100         MOVE WS-SPARA-IDORDNR-ALFA                                       
175200                                 TO WS-SPARA-IDORDNR                      
175300         MOVE KORD-IDPRODNR      TO W-401-IDPRODNR                        
175400                                    WS-SPARA-IDPRODNR                     
175500         MOVE KORD-IDPLKLST      TO W-401-IDPLKLST                        
175600                                    WS-SPARA-IDPLKLST                     
175700         MOVE KORD-IDORDER       TO WS-SPARA-IDORDER                      
175800         PERFORM IMS-GU-WDE401                                            
175900                                                                          
176000         IF SEGMENT-FINNS                                                 
176100           MOVE KORD-IDPRODNR    TO WS-JFR-IDPRODNR                       
176200         ELSE                                                             
176300           MOVE ZERO             TO WS-JFR-IDPRODNR                       
176400         END-IF                                                           
176500                                                                          
176600         IF SEGMENT-FINNS AND                                             
176700            WS-JFR-IDPRODNR = IDPRODNR-WS                                 
176800           MOVE 'N'              TO WS-ORDERDEL-STARTAD                   
176900           MOVE 'J'              TO WS-ORDERDEL-KLAR                      
177000           PERFORM IMS-GNP-ORAD                                           
177100                                                                          
177200           IF ORAD-FINNS                                                  
177300             MOVE ORAD-IDPURAD   TO WS-START-IDPURAD                      
177400                                    WS-STOPP-IDPURAD                      
177500             IF ORAD-KDRADSTA < 4                                         
177600               MOVE 'N'          TO WS-ORDERDEL-KLAR                      
177700             END-IF                                                       
177800                                                                          
177900             IF ORAD-KDRADSTA > 3                                         
178000               MOVE 'J'          TO WS-ORDERDEL-STARTAD                   
178100             END-IF                                                       
178200           END-IF                                                         
178300                                                                          
178400           PERFORM UNTIL ORAD-SAKNAS                                      
178500             PERFORM IMS-GNP-ORAD                                         
178600                                                                          
178700             IF ORAD-FINNS                                                
178800               MOVE ORAD-IDPURAD TO WS-STOPP-IDPURAD                      
178900                                                                          
179000               IF ORAD-KDRADSTA < 4                                       
179100                 MOVE 'N'        TO WS-ORDERDEL-KLAR                      
179200               END-IF                                                     
179300                                                                          
179400               IF ORAD-KDRADSTA > 3                                       
179500                 MOVE 'J'        TO WS-ORDERDEL-STARTAD                   
179600               END-IF                                                     
179700             END-IF                                                       
179800           END-PERFORM                                                    
179900                                                                          
180000           PERFORM S10-FYLL-I-RAD                                         
180100         END-IF                                                           
180200                                                                          
180300         PERFORM IMS-GN-WDE4ASEQ                                          
180400                                                                          
180500       END-PERFORM                                                        
180600                                                                          
180700       IF WS-RAD-IND = 501 AND SEGMENT-FINNS                              
180800         MOVE 20                 TO WS-KDFEL                              
180900       END-IF                                                             
181000     ELSE                                                                 
181100       MOVE 1                    TO WS-KDFEL                              
181200     END-IF                                                               
181300     .                                                                    
181400                                                                          
181500 H-KOLLA-OM-FELTRYCK SECTION.                                             
181600     MOVE 'STA H-KOLL'           TO PGM-POS                               
181700     MOVE JA                     TO INDATA-SW                             
181800     IF REQU-KDPGMACT = 'E'                                               
181900       IF REQU-KVRADER = ALL '+'                                          
182000         MOVE 500                TO WS-KVRADER                            
182100       ELSE                                                               
182200         MOVE REQU-KVRADER       TO WS-KVRADER                            
182300       END-IF                                                             
182400                                                                          
182500       MOVE 1                    TO INDX                                  
182600       PERFORM UNTIL INDX > WS-KVRADER OR INDX > 500                      
182700         IF REQU-IDANSTNR-NEW (INDX) NOT = ALL '+'                        
182800           MOVE NEJ              TO INDATA-SW                             
182900           MOVE 'IDANSTNR'       TO RESP-IDELMT-ERROR                     
183000           MOVE '013'            TO RESP-IDMSG-ERROR                      
183100                                    RESP-IDMSG-ERROR-LINE(INDX)           
183200         END-IF                                                           
183300         ADD 1                   TO INDX                                  
183400       END-PERFORM                                                        
183500     END-IF                                                               
183600     IF INDATA-FEL                                                        
183700       MOVE 19                   TO WS-KDFEL                              
183800     END-IF                                                               
183900     .                                                                    
184000                                                                          
184100 J-HAMTA-MEDDELANDE SECTION.                                              
184200                                                                          
184300     MOVE 'STA J-HAMTA'          TO PGM-POS                               
184400     EVALUATE WS-KDFEL                                                    
184500       WHEN 1                                                             
184600         MOVE '027'              TO RESP-IDMSG-ERROR                      
184700         MOVE 'IDORDNR'          TO RESP-IDELMT-ERROR                     
184800       WHEN 2                                                             
184900         MOVE '109'              TO RESP-IDMSG-ERROR                      
185000       WHEN 3                                                             
185100         MOVE '110'              TO RESP-IDMSG-ERROR                      
185200       WHEN 4                                                             
185300         MOVE '111'              TO RESP-IDMSG-ERROR                      
185400       WHEN 5                                                             
185500         MOVE '112'              TO RESP-IDMSG-ERROR                      
185600       WHEN 8                                                             
185700         MOVE '113'              TO RESP-IDMSG-ERROR                      
185800       WHEN 12                                                            
185900         MOVE '023'              TO RESP-IDMSG-ERROR                      
186000       WHEN 13                                                            
186100         MOVE '043'              TO RESP-IDMSG-ERROR                      
186200       WHEN 14                                                            
186300         MOVE '023'              TO RESP-IDMSG-ERROR                      
186400       WHEN 19                                                            
186500         MOVE '013'              TO RESP-IDMSG-ERROR                      
186600       WHEN 20                                                            
186700         MOVE '028'              TO RESP-IDMSG-ERROR                      
186800       WHEN 21                                                            
186900         MOVE '001'              TO RESP-IDMSG-INFO                       
187000       WHEN 27                                                            
187100         MOVE '027'              TO RESP-IDMSG-INFO                       
187200       WHEN 98                                                            
187300         MOVE  'IDPRODNR'        TO RESP-IDELMT-ERROR                     
187400         MOVE '024'              TO RESP-IDMSG-ERROR                      
187500       WHEN 99                                                            
187600         MOVE 'IDANSTNR'         TO RESP-IDELMT-ERROR                     
187700         MOVE '024'              TO RESP-IDMSG-ERROR                      
187800     END-EVALUATE                                                         
187900     .                                                                    
188000                                                                          
188100*    --- DISPATCHER-SEKTIONER                                             
188200 S01-HAEMTA-ANROPSDATA SECTION.                                           
188300     MOVE 'STA S01-HA'           TO PGM-POS                               
188400     MOVE 'GETARG'               TO SUB-KDFUNC                            
188500     MOVE 'CARPARTS.LDC.SPLITOFORDER'                                     
188600                                 TO SUB-ADDISPABS                         
188700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
188800                                                                          
188900     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
189000                                    SUB-KVDLEN                            
189100                                    REQU-AREA                             
189200                                                                          
189300     IF SUB-KDRC > 0                                                      
189400       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
189500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
189600         DELIMITED BY SIZE     INTO FELTEXT                               
189700       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
189800     END-IF                                                               
189900     .                                                                    
190000                                                                          
190100 S02-RETURNERA-SVAR SECTION.                                              
190200     MOVE 'STA S02-RET'          TO PGM-POS                               
190300     MOVE 'RETURN'               TO SUB-KDFUNC                            
190400     MOVE LENGTH OF RESP-AREA    TO SUB-KVDLEN                            
190500     COMPUTE WS-RESP-AREA         = LENGTH OF RESP-AREA                   
190600                                  - LENGTH OF RESP-RAD                    
190700                                  * (500 - WS-KVRADER)                    
190800     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
190900                                    SUB-KVDLEN                            
191000                                    RESP-AREA                             
191100                                                                          
191200     IF SUB-KDRC > 0                                                      
191300       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
191400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
191500         DELIMITED BY SIZE     INTO FELTEXT                               
191600       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
191700     END-IF                                                               
191800     .                                                                    
191900                                                                          
192000 S10-FYLL-I-RAD SECTION.                                                  
192100     MOVE 'STA S10-FYLL'         TO PGM-POS                               
192200                                                                          
192300     MOVE WS-SPARA-IDPRODNR      TO W-601-IDPRODNR                        
192400     PERFORM IMS-GU-WDE601                                                
192500                                                                          
192600     PERFORM S10A-KOLLA-OM-ORDER-KLAR                                     
192700                                                                          
192800     IF VISA-EJ-RAD-PA-BILD                                               
192900        OR                                                                
193000        VORD-IDDC NOT = REQU-IDDC-KEY                                     
193100       CONTINUE                                                           
193200     ELSE                                                                 
193300       MOVE WS-SPARA-IDDISTR     TO RESP-IDDISTR (WS-RAD-IND)             
193400       MOVE WS-SPARA-IDKUNDNR    TO RESP-IDKUNDNR (WS-RAD-IND)            
193500       MOVE WS-SPARA-IDORDNR     TO RESP-IDORDNR (WS-RAD-IND)             
193600       MOVE WS-SPARA-IDPRODNR    TO RESP-IDPRODNR (WS-RAD-IND)            
193700       MOVE WS-SPARA-IDPLKLST    TO RESP-IDPLKLST (WS-RAD-IND)            
193800       MOVE WS-IDPRCPLK          TO RESP-IDPRCPLK (WS-RAD-IND)            
193900       MOVE WS-IDLOTNR-PLK       TO RESP-IDLOTNR-PLK (WS-RAD-IND)         
194000       IF ENDAST-ANSTNR                                                   
194100         CONTINUE                                                         
194200       ELSE                                                               
194300         MOVE KORD-IDUSER        TO WS-JFR-IDANSTNR                       
194400       END-IF                                                             
194500       IF REQU-FLPREPRINT-KEY = 'Y' OR 'J'                                
194600         MOVE WS-JFR-IDANSTNR-5  TO WS-IDANSTNR                           
194700         COMPUTE RESP-IDQUEUENR (WS-RAD-IND) = WS-IDANSTNR                
194800                                             - 99000                      
194900         MOVE ZERO               TO RESP-IDANSTNR-OLD (WS-RAD-IND)        
195000       ELSE                                                               
195100         MOVE WS-JFR-IDANSTNR-5  TO RESP-IDANSTNR-OLD (WS-RAD-IND)        
195200         MOVE ZERO               TO RESP-IDQUEUENR (WS-RAD-IND)           
195300       END-IF                                                             
195400       INSPECT RESP-IDANSTNR-OLD(WS-RAD-IND)                              
195500         REPLACING LEADING ZERO  BY SPACE                                 
195600                                                                          
195700                                                                          
195800       IF ORDERDEL-KLAR                                                   
195900         IF ORDERDEL-STATUS-PACKAD                                        
196000           MOVE '**'             TO RESP-KDASTERISK (WS-RAD-IND)          
196100         ELSE                                                             
196200           MOVE '*'              TO RESP-KDASTERISK (WS-RAD-IND)          
196300         END-IF                                                           
196400       ELSE                                                               
196500         IF ORDERDEL-STARTAD                                              
196600           MOVE '* '             TO RESP-KDASTERISK (WS-RAD-IND)          
196700         ELSE                                                             
196800           MOVE SPACE            TO RESP-KDASTERISK (WS-RAD-IND)          
196900         END-IF                                                           
197000       END-IF                                                             
197100                                                                          
197200       MOVE WS-RAD-IND           TO RESP-KVRADER                          
197300                                    WS-KVRADER                            
197400       MOVE WS-START-IDPURAD     TO RESP-IDRADNR-ORD-FROM                 
197500                                      (WS-RAD-IND)                        
197600       MOVE WS-STOPP-IDPURAD     TO RESP-IDRADNR-ORD-TOM                  
197700                                      (WS-RAD-IND)                        
197800       ADD 1                     TO WS-RAD-IND                            
197900     END-IF                                                               
198000     .                                                                    
198100                                                                          
198200 S10A-KOLLA-OM-ORDER-KLAR SECTION.                                        
198300                                                                          
198400     MOVE 'STA S10A-KOLL'        TO PGM-POS                               
198500     MOVE NEJ                    TO WS-VISA-RAD                           
198600                                    WS-ORDERDEL-STATUS                    
198700     MOVE WS-SPARA-IDORDER       TO W-IDORDER-WDQ3-MIN                    
198800                                    W-IDORDER-WDQ3-MAX                    
198900     MOVE REQU-IDDC-KEY          TO W-IDDC-WDQ3-MIN                       
199000                                    W-IDDC-WDQ3-MAX                       
199100     MOVE WS-SPARA-IDPRODNR      TO W-IDPRODNR-WDQ3-MIN                   
199200                                    W-IDPRODNR-WDQ3-MAX                   
199300                                                                          
199400     PERFORM IMS-GU-ORQA-WDQ301                                           
199500                                                                          
199600     PERFORM                                                              
199700       UNTIL SEGMENT-SAKNAS OR                                            
199800             (VISA-RAD-PA-BILD AND                                        
199900              ODEL-IDPLKLST > WS-SPARA-IDPLKLST)                          
200000       IF SEGMENT-FINNS                                                   
200100         IF ODEL-KDODELSTA NOT = 'P'                                      
200200           IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW') AND                     
200300              ODEL-IDPRC = '9998'                                         
200400**SOFTWARE KONTROLL                                                       
200500             CONTINUE                                                     
200600           ELSE                                                           
200700             MOVE JA             TO WS-VISA-RAD                           
200800           END-IF                                                         
200900         END-IF                                                           
201000         IF ODEL-IDPLKLST = WS-SPARA-IDPLKLST                             
201100           MOVE ODEL-IDPRCPLK    TO WS-IDPRCPLK                           
201200           MOVE ODEL-IDLOTNR-PLK TO WS-IDLOTNR-PLK                        
201300         END-IF                                                           
201400                                                                          
201500         IF ODEL-KDODELSTA = 'P' AND                                      
201600            ODEL-IDPLKLST = WS-SPARA-IDPLKLST                             
201700           MOVE JA               TO WS-ORDERDEL-STATUS                    
201800         END-IF                                                           
201900         PERFORM IMS-GN-ORQA-WDQ301                                       
202000       END-IF                                                             
202100     END-PERFORM                                                          
202200     .                                                                    
202300                                                                          
202400* IMS SEKTIONER                                                           
202500                                                                          
202600 IMS-GU-WDE401 SECTION.                                                   
202700                                                                          
202800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
202900       DELIMITED BY SIZE       INTO SSA1                                  
203000     MOVE '  GE'                 TO GODK-STATUSKODER                      
203100     CALL CBLTDLI             USING GU                                    
203200                                    WDE4-PCB                              
203300                                    DLI-IO-E401                           
203400                                    SSA1                                  
203500     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800                                                                          
203900 IMS-GHU-WDE401 SECTION.                                                  
204000                                                                          
204100     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
204200       DELIMITED BY SIZE       INTO SSA1                                  
204300     MOVE '    '                 TO GODK-STATUSKODER                      
204400     CALL CBLTDLI             USING GHU                                   
204500                                    WDE4-PCB                              
204600                                    DLI-IO-E401                           
204700                                    SSA1                                  
204800     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     .                                                                    
205100                                                                          
205200 IMS-REPL-WDE401 SECTION.                                                 
205300                                                                          
205400     MOVE '  '                   TO GODK-STATUSKODER                      
205500     CALL CBLTDLI             USING REPL                                  
205600                                    WDE4-PCB                              
205700                                    DLI-IO-E401                           
205800     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
205900     PERFORM IMS-STATUSKONTROLL                                           
206000     .                                                                    
206100                                                                          
206200 IMS-GNP-ORAD SECTION.                                                    
206300                                                                          
206400     MOVE 'WDE411 '              TO SSA1                                  
206500     MOVE '  GE'                 TO GODK-STATUSKODER                      
206600     CALL CBLTDLI             USING GNP                                   
206700                                    WDE4-PCB                              
206800                                    DLI-IO-E411                           
206900                                    SSA1                                  
207000     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
207100                                    STATUS-ORAD-WS                        
207200     PERFORM IMS-STATUSKONTROLL                                           
207300     .                                                                    
207400                                                                          
207500 IMS-GU-WDE4ASEQ SECTION.                                                 
207600                                                                          
207700     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
207800       DELIMITED BY SIZE       INTO SSA1                                  
207900     MOVE '  GE'                 TO GODK-STATUSKODER                      
208000     CALL CBLTDLI             USING GU                                    
208100                                    WDE41-PCB                             
208200                                    DLI-IO-E401                           
208300                                    SSA1                                  
208400     MOVE WDE41-STATUS-CODE      TO STATUS-WS                             
208500                                    STATUS-KUNDORDER-SEK-WS               
208600     PERFORM IMS-STATUSKONTROLL                                           
208700     .                                                                    
208800                                                                          
208900 IMS-GN-WDE4ASEQ SECTION.                                                 
209000                                                                          
209100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
209200       DELIMITED BY SIZE       INTO SSA1                                  
209300     MOVE '  GE'                 TO GODK-STATUSKODER                      
209400     CALL CBLTDLI             USING GN                                    
209500                                    WDE41-PCB                             
209600                                    DLI-IO-E401                           
209700                                    SSA1                                  
209800     MOVE WDE41-STATUS-CODE      TO STATUS-WS                             
209900                                    STATUS-KUNDORDER-SEK-WS               
210000     PERFORM IMS-STATUSKONTROLL                                           
210100     .                                                                    
210200                                                                          
210300 IMS-GU-WDE411-01-BSEQ SECTION.                                           
210400                                                                          
210500     STRING 'WDE411  (WDE4BSEQ>=' W-WDE411-KEYSEQ-MIN-X                   
210600                    '&WDE4BSEQ<=' W-WDE411-KEYSEQ-MAX-X ')'               
210700       DELIMITED BY SIZE       INTO SSA1                                  
210800     MOVE 'WDE401 '              TO SSA2                                  
210900     MOVE '  GE'                 TO GODK-STATUSKODER                      
211000     CALL CBLTDLI             USING GU                                    
211100                                    WDE42-PCB                             
211200                                    DLI-IO-E401                           
211300                                    SSA1 SSA2                             
211400     MOVE WDE42-STATUS-CODE      TO STATUS-WS                             
211500     PERFORM IMS-STATUSKONTROLL                                           
211600     .                                                                    
211700                                                                          
211800 IMS-GU-WDE401-ESEQ SECTION.                                              
211900                                                                          
212000     STRING 'WDE401  (WDE4ESEQ =' W-WDE601-VOLVOORDER-X ')'               
212100       DELIMITED BY SIZE       INTO SSA1                                  
212200     MOVE '  GE'                 TO GODK-STATUSKODER                      
212300     CALL CBLTDLI             USING GU                                    
212400                                    WDE43-PCB                             
212500                                    DLI-IO-E401                           
212600                                    SSA1                                  
212700     MOVE WDE43-STATUS-CODE      TO STATUS-WS                             
212800     PERFORM IMS-STATUSKONTROLL                                           
212900     .                                                                    
213000                                                                          
213100 IMS-GU-WDE4F SECTION.                                                    
213200     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
213300                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
213400                    '&IDPLKLST =' W-WDE4F-IDPLKST-X ')'                   
213500       DELIMITED BY SIZE       INTO SSA1                                  
213600     MOVE '  GE'                 TO GODK-STATUSKODER                      
213700     CALL CBLTDLI             USING GU                                    
213800                                    WDE4F-PCB                             
213900                                    DLI-IO-E4F1                           
214000                                    SSA1                                  
214100     MOVE WDE4F-STATUS-CODE      TO STATUS-WS                             
214200     PERFORM IMS-STATUSKONTROLL                                           
214300     .                                                                    
214400                                                                          
214500 IMS-GN-WDE4F SECTION.                                                    
214600     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
214700                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
214800                    '&IDPLKLST =' W-WDE4F-IDPLKST-X ')'                   
214900       DELIMITED BY SIZE       INTO SSA1                                  
215000     MOVE '  GEGB'               TO GODK-STATUSKODER                      
215100     CALL CBLTDLI             USING GN                                    
215200                                    WDE4F-PCB                             
215300                                    DLI-IO-E4F1                           
215400                                    SSA1                                  
215500     MOVE WDE4F-STATUS-CODE      TO STATUS-WS                             
215600     PERFORM IMS-STATUSKONTROLL                                           
215700     .                                                                    
215800                                                                          
215900 IMS-GU-WDE601 SECTION.                                                   
216000                                                                          
216100     STRING 'WDE601  (IDPRODNR =' W-WDE601-VOLVOORDER-X ')'               
216200       DELIMITED BY SIZE       INTO SSA1                                  
216300     MOVE '  '                   TO GODK-STATUSKODER                      
216400     CALL CBLTDLI             USING GU                                    
216500                                    WDE6-PCB                              
216600                                    DLI-IO-E601                           
216700                                    SSA1                                  
216800     MOVE WDE6-STATUS-CODE       TO STATUS-WS                             
216900     PERFORM IMS-STATUSKONTROLL                                           
217000     .                                                                    
217100                                                                          
217200 IMS-GU-4448 SECTION.                                                     
217300                                                                          
217400     MOVE 'IMS-GU-4888 '         TO PGM-POS                               
217500     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X ')'                            
217600       DELIMITED BY SIZE       INTO SSA1                                  
217700     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X ')'                            
217800       DELIMITED BY SIZE       INTO SSA2                                  
217900     MOVE '    '                 TO GODK-STATUSKODER                      
218000     CALL CBLTDLI             USING GU                                    
218100                                    XXKH-PCB                              
218200                                    DLI-IO-WDGX4448                       
218300                                    SSA1 SSA2                             
218400     MOVE XXKH-STATUS-CODE       TO STATUS-WS                             
218500     PERFORM IMS-STATUSKONTROLL                                           
218600     .                                                                    
218700                                                                          
218800 IMS-GU-4488-FIRST SECTION.                                               
218900     MOVE 'IMS-488FIRST'         TO PGM-POS                               
219000                                                                          
219100     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
219200       DELIMITED BY SIZE       INTO SSA1                                  
219300     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
219400       DELIMITED BY SIZE       INTO SSA2                                  
219500     MOVE '  GEGB'               TO GODK-STATUSKODER                      
219600     CALL CBLTDLI             USING GU                                    
219700                                    4487-PCB                              
219800                                    DLI-IO-AREA3                          
219900                                    SSA1                                  
220000                                    SSA2                                  
220100     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
220200                                    STATUS-4488-WS                        
220300     PERFORM IMS-STATUSKONTROLL                                           
220400     .                                                                    
220500                                                                          
220600 IMS-GU-4488 SECTION.                                                     
220700                                                                          
220800     MOVE 'IMS-GU-4888 '         TO PGM-POS                               
220900     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
221000       DELIMITED BY SIZE       INTO SSA1                                  
221100     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
221200       DELIMITED BY SIZE       INTO SSA2                                  
221300     MOVE '  GE'                 TO GODK-STATUSKODER                      
221400     CALL CBLTDLI             USING GU                                    
221500                                    4487-PCB                              
221600                                    DLI-IO-AREA3                          
221700                                    SSA1 SSA2                             
221800     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
221900                                    STATUS-4488-WS                        
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200                                                                          
222300 IMS-GHU-4490 SECTION.                                                    
222400                                                                          
222500     MOVE 'IMS-GHU-4490'         TO PGM-POS                               
222600     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
222700       DELIMITED BY SIZE       INTO SSA1                                  
222800     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
222900       DELIMITED BY SIZE       INTO SSA2                                  
223000     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
223100       DELIMITED BY SIZE       INTO SSA3                                  
223200     MOVE '  GE'                 TO GODK-STATUSKODER                      
223300     CALL CBLTDLI             USING GHU                                   
223400                                    4487-PCB                              
223500                                    DLI-IO-WDGX4490                       
223600                                    SSA1 SSA2 SSA3                        
223700     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
223800                                    STATUS-4490-WS                        
223900     PERFORM IMS-STATUSKONTROLL                                           
224000     .                                                                    
224100                                                                          
224200 IMS-GN-4488 SECTION.                                                     
224300                                                                          
224400     MOVE 'IMS-GN-4488 '         TO PGM-POS                               
224500     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
224600       DELIMITED BY SIZE       INTO SSA1                                  
224700     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
224800       DELIMITED BY SIZE       INTO SSA2                                  
224900     MOVE '  GEGB'               TO GODK-STATUSKODER                      
225000     CALL CBLTDLI             USING GN                                    
225100                                    4487-PCB                              
225200                                    DLI-IO-AREA3                          
225300                                    SSA1 SSA2                             
225400     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
225500                                    STATUS-4488-WS                        
225600     PERFORM IMS-STATUSKONTROLL                                           
225700     .                                                                    
225800                                                                          
225900 IMS-GNP-4490 SECTION.                                                    
226000                                                                          
226100     MOVE 'IMS-GNP4490 '         TO PGM-POS                               
226200     MOVE 'WDGX4490 '            TO SSA1                                  
226300     MOVE '  GE'                 TO GODK-STATUSKODER                      
226400     CALL CBLTDLI             USING GNP                                   
226500                                    4487-PCB                              
226600                                    DLI-IO-WDGX4490                       
226700                                    SSA1                                  
226800     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
226900                                    STATUS-4490-WS                        
227000     PERFORM IMS-STATUSKONTROLL                                           
227100     .                                                                    
227200                                                                          
227300 IMS-REPL-4490 SECTION.                                                   
227400                                                                          
227500     MOVE '  '                   TO GODK-STATUSKODER                      
227600     CALL CBLTDLI             USING REPL                                  
227700                                    4487-PCB                              
227800                                    DLI-IO-WDGX4490                       
227900     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
228000                                    STATUS-4490-WS                        
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300                                                                          
228400 IMS-GU-ORQA-WDQ301 SECTION.                                              
228500                                                                          
228600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
228700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
228800       DELIMITED BY SIZE       INTO SSA1                                  
228900     MOVE '  GE'                 TO GODK-STATUSKODER                      
229000     CALL CBLTDLI             USING GU                                    
229100                                    ORQA-PCB                              
229200                                    DLI-IO-WDQ301                         
229300                                    SSA1                                  
229400     MOVE ORQA-STATUS-CODE       TO STATUS-WS                             
229500     PERFORM IMS-STATUSKONTROLL                                           
229600     .                                                                    
229700                                                                          
229800                                                                          
229900 IMS-GN-ORQA-WDQ301 SECTION.                                              
230000                                                                          
230100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
230200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
230300       DELIMITED BY SIZE       INTO SSA1                                  
230400     MOVE '  GE'                 TO GODK-STATUSKODER                      
230500     CALL CBLTDLI             USING GN                                    
230600                                    ORQA-PCB                              
230700                                    DLI-IO-WDQ301                         
230800                                    SSA1                                  
230900     MOVE ORQA-STATUS-CODE       TO STATUS-WS                             
231000     PERFORM IMS-STATUSKONTROLL                                           
231100     .                                                                    
231200                                                                          
231300                                                                          
231400 IMS-GHU-ORQA-WDQ301 SECTION.                                             
231500                                                                          
231600     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
231700       DELIMITED BY SIZE       INTO SSA1                                  
231800     MOVE '  '                   TO GODK-STATUSKODER                      
231900     CALL CBLTDLI             USING GHU                                   
232000                                    ORQA-PCB                              
232100                                    DLI-IO-WDQ301                         
232200                                    SSA1                                  
232300     MOVE ORQA-STATUS-CODE       TO STATUS-WS                             
232400     PERFORM IMS-STATUSKONTROLL                                           
232500     .                                                                    
232600                                                                          
232700                                                                          
232800 IMS-REPL-ORQA-WDQ301  SECTION.                                           
232900                                                                          
233000     MOVE '  '                   TO GODK-STATUSKODER                      
233100     CALL CBLTDLI             USING REPL                                  
233200                                    ORQA-PCB                              
233300                                    DLI-IO-WDQ301                         
233400     MOVE ORQA-STATUS-CODE       TO STATUS-WS                             
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700                                                                          
233800                                                                          
233900 IMS-GHU-WDE611   SECTION.                                                
234000     STRING 'WDE601  (IDPRODNR =' W-WDE601-VOLVOORDER-X ')'               
234100       DELIMITED BY SIZE       INTO SSA1                                  
234200     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
234300       DELIMITED BY SIZE       INTO SSA2                                  
234400     MOVE '  '                   TO GODK-STATUSKODER                      
234500     CALL CBLTDLI             USING GHU                                   
234600                                    WDE6-PCB                              
234700                                    DLI-IO-E611                           
234800                                    SSA1 SSA2                             
234900     MOVE WDE6-STATUS-CODE       TO STATUS-WS                             
235000     PERFORM IMS-STATUSKONTROLL                                           
235100                                                                          
235200     .                                                                    
235300                                                                          
235400 IMS-REPL-WDE611 SECTION.                                                 
235500     MOVE '    '                 TO GODK-STATUSKODER                      
235600     CALL CBLTDLI             USING REPL                                  
235700                                    WDE6-PCB                              
235800                                    DLI-IO-E611                           
235900     MOVE WDE6-STATUS-CODE       TO STATUS-WS                             
236000     PERFORM IMS-STATUSKONTROLL                                           
236100                                                                          
236200     .                                                                    
236300                                                                          
236400 IMS-STATUSKONTROLL SECTION.                                              
236500                                                                          
236600     SET STATUS-IX               TO 1                                     
236700     SEARCH GODK-STATUS                                                   
236800       AT END                                                             
236900         CALL FELLOG                                                      
237000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
237100         CONTINUE                                                         
237200     END-SEARCH                                                           
237300     .                                                                    
