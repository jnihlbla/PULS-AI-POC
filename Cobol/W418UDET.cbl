000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             W411UDET.                                        
000500 AUTHOR.                 LARS THELL CAP PROGRAMATOR                       
000600     DATE-WRITTEN.       JULI 1995.                                       
000700*                                                                         
000800     REMARKS.                                                             
000900*    FUNKTION:                                                            
001000*        UTSKRIFT AV DETALJINFORMATION                                    
001100*                                                                         
001110*                                                                         
001120******************************************************************        
001130*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL016110    *        
001140******************************************************************        
001150*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 DATA DIVISION.                                                           
001600                                                                          
001700 WORKING-STORAGE SECTION.                                                 
001800     SKIP2                                                                
001801                                                                          
001810*    -- CHECKED BY WY2000                                                 
001900 77  PROGRAM-NAMN            PIC X(8) VALUE 'W418UDET'.                   
002000     SKIP2                                                                
002100*    ---- KONSTANTER                                                      
002200                                                                          
002300 77  JA                      PIC X       VALUE 'J'.                       
002400 77  NEJ                     PIC X       VALUE 'N'.                       
002600 77  TEXT-INDX               PIC S9(4)   VALUE ZERO COMP SYNC.            
002700     EJECT                                                                
002800*  UTSKRIFTRADER                                                          
002900                                                                          
003000 01  LIST-HRAD1.                                                          
003100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003200     03   FILLER                  PIC X(12) VALUE                         
003300                                        'W418UDET-001'.                   
003400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
003500     03   FILLER                  PIC X(23) VALUE                         
003600                                  'DISCREPANCY REPORT LINE'.              
003700                                                                          
003800 01  LIST-HRAD2.                                                          
003810     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003820     03   FILLER                  PIC X(9)  VALUE 'DISTRICT '.            
003830     03   FILLER                  PIC X(2)  VALUE SPACE.                  
003840     03   HRAD2-IDDISTR           PIC Z(4)  VALUE ZERO.                   
003850     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003860     03   FILLER                  PIC X(5)  VALUE 'RET. '.                
003870     03   HRAD2-IDKUNDNR          PIC Z(6)  VALUE ZERO.                   
003880     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003890     03   FILLER                  PIC X(9)  VALUE 'D/R NO.  '.            
003891     03   HRAD2-IDRAPPNR          PIC Z(7)  VALUE ZERO.                   
004800                                                                          
004900 01  LIST-HRAD3.                                                          
005000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
005100     03   FILLER                  PIC X(6)  VALUE 'PART  '.               
005200     03   HRAD3-IDARTNR           PIC Z(7)  VALUE ZERO.                   
005300     03   FILLER                  PIC X(1)  VALUE '-'.                    
005400     03   HRAD3-REKSIFFR          PIC 9.                                  
005500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
005510     03   FILLER                  PIC X(6)  VALUE 'LINE  '.               
005520     03   HRAD3-IDRADNR           PIC Z(5)  VALUE ZERO.                   
005600                                                                          
005700 01  LIST-HRAD4.                                                          
005800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
005900     03   FILLER                  PIC X(19) VALUE                         
006000                                  'DISCREPANCY REPORT:'.                  
006100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
006200     03   FILLER                  PIC X(8)  VALUE                         
006300                                  'INVOICE:'.                             
006400     03   FILLER                  PIC X(11) VALUE SPACE.                  
006500     03   FILLER                  PIC X(16)  VALUE                        
006600                                  'PARTS MASTER:   '.                     
006700                                                                          
006800 01  LIST-HRAD5.                                                          
006900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
007000     03   FILLER                  PIC X(21) VALUE                         
007100                                  '====================='.                
007200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
007300     03   FILLER                  PIC X(18) VALUE                         
007400                                  '=================='.                   
007500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
007600     03   FILLER                  PIC X(19) VALUE                         
007700                                  '==================='.                  
007800                                                                          
007900 01  LIST-LRAD1.                                                          
008000     03   FILLER                  PIC X(1).                               
008100     03   FILLER                  PIC X(9)  VALUE 'CODE     '.            
008200     03   FILLER                  PIC X(10).                              
008300     03   LRAD1-KDANMORS          PIC 9(2).                               
008400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
008500     03   FILLER                  PIC X(9)  VALUE 'ORD. QTY '.            
008600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
008700     03   LRAD1-KVBEART-Q         PIC Z(6)9.                              
008800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
008900     03   FILLER                  PIC X(5)  VALUE 'LOC. '.                
009000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
009100     03   LRAD1-ADLAGOMR          PIC Z(1)9.                              
009200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
009300     03   LRAD1-ADGANG            PIC Z(1)9.                              
009400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
009500     03   LRAD1-ADPLATS           PIC Z(4)9.                              
009600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
009700                                                                          
009800 01  LIST-LRAD2.                                                          
009801     03   FILLER                  PIC X(1).                               
009802     03   FILLER                  PIC X(7)  VALUE 'ORDER  '.              
009803     03   FILLER                  PIC X(7).                               
009804     03   LRAD2-IDORDNR7          PIC Z(7).                               
009805     03   FILLER                  PIC X(1).                               
009806     03   FILLER                  PIC X(8)  VALUE 'DEL. QTY'.             
009807     03   FILLER                  PIC X(3).                               
009808     03   LRAD2-KVLEVART          PIC Z(6)9.                              
009809     03   FILLER                  PIC X(1)  VALUE SPACE.                  
009810     03   FILLER                  PIC X(2)  VALUE 'SB'.                   
009811     03   FILLER                  PIC X(9)  VALUE SPACE.                  
009812     03   LRAD2-KVLS              PIC -(7)9.                              
011300                                                                          
011400 01  LIST-LRAD3.                                                          
011500     03   FILLER                  PIC X(1).                               
011600     03   FILLER                  PIC X(5)  VALUE 'QTY  '.                
011700     03   FILLER                  PIC X(11).                              
011800     03   LRAD3-KVLEVANM-BEKR     PIC Z(4)9.                              
011900     03   FILLER                  PIC X(1).                               
012000     03   FILLER                  PIC X(9)  VALUE 'GR.WEIGHT'.            
012100     03   FILLER                  PIC X(2).                               
012200     03   LRAD3-VKORDBTO-KOLLI    PIC Z(4)9.9.                            
012300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
012400     03   FILLER                  PIC X(6)  VALUE 'PB    '.               
012500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
012600     03   LRAD3-KVPB-TOT          PIC Z(5)9.9.                            
012700                                                                          
012800 01  LIST-LRAD4.                                                          
012900     03   FILLER                  PIC X(1).                               
013000     03   FILLER                  PIC X(5)  VALUE 'PRICE'.                
013100     03   FILLER                  PIC X(6).                               
013200     03   LRAD4-PRARTBTO          PIC Z(6)9.99.                           
013300     03   FILLER                  PIC X(1).                               
013400     03   FILLER                  PIC X(8)  VALUE 'NET WGHT'.             
013500     03   FILLER                  PIC X(3).                               
013600     03   LRAD4-VKORDNTO-KOLLI    PIC Z(4)9.9.                            
013700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
013800     03   FILLER                  PIC X(8)  VALUE 'SUP.CODE'.             
013900     03   FILLER                  PIC X(9)  VALUE SPACE.                  
014000     03   LRAD4-KDERS             PIC 99.                                 
014100                                                                          
014200 01  LIST-LRAD5.                                                          
014300     03   FILLER                  PIC X(1).                               
014400     03   FILLER                  PIC X(5)  VALUE 'CASE '.                
014500     03   FILLER                  PIC X(11).                              
014600     03   LRAD5-IDKOLLI           PIC Z(5).                               
014700     03   FILLER                  PIC X(1).                               
014800     03   FILLER                  PIC X(4)  VALUE 'TARE'.                 
014900     03   FILLER                  PIC X(7).                               
015000     03   LRAD5-VKTARA            PIC Z(4)9.9.                            
015100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
015200     03   FILLER                  PIC X(8)  VALUE 'NET WGHT'.             
015300     03   FILLER                  PIC X(4)  VALUE SPACE.                  
015400     03   LRAD5-VKART             PIC Z(6)9.                              
015500                                                                          
015600 01  LIST-LRAD6.                                                          
015700     03   FILLER                  PIC X(1).                               
015800     03   FILLER                  PIC X(7)  VALUE 'INVOICE'.              
015900     03   FILLER                  PIC X(5).                               
016000     03   LRAD6-KDFAKTYP          PIC X(1).                               
016100     03   FILLER                  PIC X(1)  VALUE '-'.                    
016200     03   LRAD6-IDFAKT            PIC Z(7).                               
016300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
016400     03   FILLER                  PIC X(10) VALUE 'TOT.WEIGHT'.           
016500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
016600     03   LRAD6-VKORDNTO-TOT      PIC Z(4)9.9.                            
016700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
016800     03   FILLER                  PIC X(8)  VALUE 'GR PRICE'.             
016900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
017000     03   LRAD6-PRARTBTO-EXP      PIC Z(6)9.99.                           
017100                                                                          
017200 01  LIST-LRAD7.                                                          
017300     03   FILLER                  PIC X(1).                               
017400     03   FILLER                  PIC X(12) VALUE 'INVOICE DATE'.         
017500     03   FILLER                  PIC X(3).                               
017600     03   LRAD7-TIFAKT            PIC Z(6).                               
017700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
017800     03   FILLER                  PIC X(5)  VALUE 'CLASS'.                
017900     03   FILLER                  PIC X(12).                              
018000     03   LRAD7-KDORDKL           PIC 9.                                  
018100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
018200     03   FILLER                  PIC X(8)  VALUE 'COST PR.'.             
018300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
018400     03   LRAD7-PRINK             PIC Z(6)9.99.                           
018500                                                                          
018600 01  LIST-LRAD8.                                                          
018700     03   FILLER                  PIC X(23) VALUE SPACE.                  
018800     03   FILLER                  PIC X(9) VALUE 'DIR. SUP.'.             
018900     03   FILLER                  PIC X(8).                               
019000     03   LRAD8-FLDIRLEV          PIC X(1).                               
019100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
019200     03   FILLER                  PIC X(8)  VALUE 'PROD.GR.'.             
019300     03   FILLER                  PIC X(8).                               
019400     03   LRAD8-KDPRODSL          PIC Z(2)9.                              
019500                                                                          
019600 01  LIST-LRAD9.                                                          
019700     03   FILLER                  PIC X(23) VALUE SPACE.                  
019800     03   FILLER                  PIC X(7) VALUE 'PACKER '.               
019900     03   FILLER                  PIC X(3).                               
020000     03   LRAD9-IDUSER-PACK       PIC X(8).                               
020100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
020200     03   FILLER                  PIC X(8)  VALUE 'FUNC.GRP'.             
020300     03   FILLER                  PIC X(6).                               
020400     03   LRAD9-IDFKNGRP          PIC Z(4)9.                              
020500                                                                          
020600 01  LIST-LRAD10.                                                         
020700     03   FILLER                  PIC X(23) VALUE SPACE.                  
020800     03   FILLER                  PIC X(9) VALUE 'PROD. NO.'.             
020900     03   FILLER                  PIC X(2).                               
021000     03   LRAD10-IDPRODNR         PIC Z(6)9.                              
021100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
021200     03   FILLER                  PIC X(8)  VALUE 'INV.DATE'.             
021300     03   FILLER                  PIC X(6).                               
021400     03   LRAD10-TIINVDAT         PIC Z(5).                               
021500                                                                          
021600 01  LIST-LRAD11.                                                         
021700     03   FILLER                  PIC X(23) VALUE SPACE.                  
021800     03   FILLER                  PIC X(10) VALUE 'QTY LINES '.           
021900     03   FILLER                  PIC X(3).                               
022000     03   LRAD11-KVORDRAD         PIC Z(4)9.                              
022100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022200     03   FILLER                  PIC X(7)  VALUE 'INV.QTY'.              
022300     03   FILLER                  PIC X(4).                               
022400     03   LRAD11-KVINVS           PIC -(7)9.                              
022500                                                                          
022600 01  LIST-LRAD12.                                                         
022700     03   FILLER                  PIC X(23) VALUE SPACE.                  
022800     03   FILLER                  PIC X(9)  VALUE 'REG. RESP'.            
022900     03   FILLER                  PIC X(1).                               
023000     03   LRAD12-IDUSER-OREG      PIC X(8).                               
023100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023200     03   FILLER                  PIC X(5)  VALUE 'PROC.'.                
023300     03   FILLER                  PIC X(11).                              
023400     03   LRAD12-IDANSK           PIC Z(2)9.                              
023500                                                                          
023600 01  LIST-LRAD13.                                                         
023700     03   FILLER                  PIC X(23) VALUE SPACE.                  
023800     03   FILLER                  PIC X(9)  VALUE 'REG. DATE'.            
023900     03   FILLER                  PIC X(3).                               
024000     03   LRAD13-TIREGDAT         PIC 9(6).                               
024100                                                                          
024200 01  LIST-LRAD14.                                                         
024300     03   FILLER                  PIC X(1) VALUE SPACE.                   
024400     03   FILLER                  PIC X(10) VALUE 'DESCRIPT.:'.           
024500     03   FILLER                  PIC X(1).                               
024600     03   LRAD14-BEART            PIC X(25).                              
024700                                                                          
024800 01  LIST-LRAD15.                                                         
024900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
025000     03   FILLER                  PIC X(26) VALUE                         
025100                           'MESSAGE FROM VIPS/PULS:  '.                   
025200                                                                          
025300 01  LIST-LRAD16.                                                         
025400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
025500     03   FILLER                  PIC X(20) VALUE                         
025600                           'MESSAGE FROM ADM.:  '.                        
025700                                                                          
025800 01  LIST-LRAD17.                                                         
025900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
026000     03   FILLER                  PIC X(23) VALUE                         
026100                           'MESSAGE FROM REM.:     '.                     
026200                                                                          
026300 01  LIST-LRAD18.                                                         
026400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
026500     03   FILLER                  PIC X(23) VALUE                         
026600                           'MESSAGE FROM RET.DEPT.'.                      
026700                                                                          
026800 01  LIST-LRAD-TEAMNOT.                                                   
026900     03   FILLER                  PIC X(1) VALUE SPACE.                   
027000     03   LRAD-TEANMNOT           PIC X(70).                              
027100                                                                          
027200 01  LIST-BLANKRAD.                                                       
027300     03   FILLER                  PIC X(80) VALUE SPACE.                  
027400                                                                          
027500 01  LIST-STRECKRAD.                                                      
027600     03   FILLER                  PIC X(1) VALUE SPACE.                   
027700     03   FILLER                  PIC X(70) VALUE ALL '='.                
027800                                                                          
027900 01  LIST-ASTERRAD.                                                       
028000     03   FILLER                  PIC X(1) VALUE SPACE.                   
028100     03   FILLER                  PIC X(54) VALUE ALL '*'.                
028200                                                                          
028300 01  DYNAMISKA-SUBPROGRAM.                                                
028400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
028500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
028600   03  W006PRS1              PIC X(8)    VALUE 'W006PRS1'.                
028700                                                                          
028800* VARIABLER TILL SUBPROGRAM W006PRR1                                      
028900*01  -COPY W006PRAR                                                       
029000     SKIP2                                                                
029100     EJECT                                                                
029200 01  WS-RAPP-AREA.                                                        
029300     03  WS-RAPP-PRINTER         PIC X(8).                                
029400     03  WS-RAPP-LISTRAD.                                                 
029500         05  FILLER              PIC X(1)    VALUE SPACE.                 
029600         05  WS-RAPP-RAD         PIC X(120).                              
029700     03  WS-DUMMY                PIC X(1).                                
029800     EJECT                                                                
029900*    --- STATUS-KOD FRÅN IMS                                              
030000 01  STATUS-WS                   PIC XX.                                  
030100     88  STATUS-OK                           VALUE '  '.                  
030200*    --- IMS FUNKTIONSKODER                                               
030300*01  -COPY W0003                                                          
030400     EJECT                                                                
030500*01  -COPY WMSGAREA                                                       
030600     EJECT                                                                
030700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
030800                                                                          
030900 01  GODK-STATUSKODER.                                                    
031000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
031100     SKIP2                                                                
031200 01  SSA1                    PIC X(64).                                   
031300 01  SSA2                    PIC X(64).                                   
031400     SKIP2                                                                
031500 LINKAGE SECTION.                                                         
031600     SKIP2                                                                
031700*01  -COPY W418UDET                                                       
031800     EJECT                                                                
031900*01  -COPY W0009      -PRE  ALT-                                          
032000     EJECT                                                                
032100 PROCEDURE DIVISION  USING  UDET-W418UDET                                 
032200                            ALT-PCB.                                      
032300                                                                          
032400 STYR SECTION.                                                            
032500                                                                          
032600     PERFORM A-INIT                                                       
032700                                                                          
032800     PERFORM B-SKAPA-LISTA                                                
032900                                                                          
032910     PERFORM Z-FINIT                                                      
032920                                                                          
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400 A-INIT        SECTION.                                                   
033500                                                                          
033600     MOVE UDET-IDPRTLST        TO WS-RAPP-PRINTER                         
033700                                                                          
033800     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
033900                         PRT-OPEN                                         
034000                         WS-RAPP-PRINTER                                  
034100                         ALT-PCB                                          
034200                         WS-DUMMY                                         
034300                         WS-DUMMY                                         
034400     .                                                                    
034500     EJECT                                                                
034600 B-SKAPA-LISTA  SECTION.                                                  
034700                                                                          
034800     PERFORM BA-REDIGERA-HUVUD                                            
034900     PERFORM BB-REDIGERA-RADER                                            
035000     .                                                                    
035100     EJECT                                                                
035200                                                                          
035300 BA-REDIGERA-HUVUD  SECTION.                                              
035400                                                                          
035500     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
035600     MOVE LIST-HRAD1            TO WS-RAPP-RAD                            
035700     PERFORM S01-SKRIV-RAD                                                
035800                                                                          
035900     MOVE UDET-IDDISTR          TO HRAD2-IDDISTR                          
036000     MOVE UDET-IDKUNDNR         TO HRAD2-IDKUNDNR                         
036100     MOVE UDET-IDRAPPNR         TO HRAD2-IDRAPPNR                         
036200     MOVE LIST-HRAD2            TO WS-RAPP-RAD                            
036300     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
036400     PERFORM S01-SKRIV-RAD                                                
036500                                                                          
036600     MOVE UDET-IDARTNR          TO HRAD3-IDARTNR                          
036610     MOVE UDET-REKSIFFR         TO HRAD3-REKSIFFR                         
036700     MOVE UDET-IDRADNR          TO HRAD3-IDRADNR                          
036800     MOVE LIST-HRAD3            TO WS-RAPP-RAD                            
036900     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
037000     PERFORM S01-SKRIV-RAD                                                
037100                                                                          
037200     MOVE LIST-HRAD4            TO WS-RAPP-RAD                            
037300     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
037400     PERFORM S01-SKRIV-RAD                                                
037500                                                                          
037600     MOVE LIST-HRAD5            TO WS-RAPP-RAD                            
037700     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
037800     PERFORM S01-SKRIV-RAD                                                
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200 BB-REDIGERA-RADER  SECTION.                                              
038300                                                                          
038400     MOVE UDET-KDANMORS         TO LRAD1-KDANMORS                         
038500     MOVE UDET-KVBEART-Q        TO LRAD1-KVBEART-Q                        
038600     MOVE UDET-ADLAGOMR         TO LRAD1-ADLAGOMR                         
038700     MOVE UDET-ADGANG           TO LRAD1-ADGANG                           
038800     MOVE UDET-ADPLATS          TO LRAD1-ADPLATS                          
038900     MOVE LIST-LRAD1            TO WS-RAPP-RAD                            
039000     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
039100     PERFORM S01-SKRIV-RAD                                                
039200                                                                          
039400     MOVE UDET-IDORDNR5         TO LRAD2-IDORDNR7                         
039500     MOVE UDET-KVLEVART         TO LRAD2-KVLEVART                         
039600     MOVE UDET-KVLS             TO LRAD2-KVLS                             
039700     MOVE LIST-LRAD2            TO WS-RAPP-RAD                            
039800     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
039900     PERFORM S01-SKRIV-RAD                                                
040000                                                                          
040100     MOVE UDET-KVLEVANM-BEKR    TO LRAD3-KVLEVANM-BEKR                    
040200     MOVE UDET-VKORDBTO-KOLLI   TO LRAD3-VKORDBTO-KOLLI                   
040300     MOVE UDET-KVPB-TOT         TO LRAD3-KVPB-TOT                         
040400     MOVE LIST-LRAD3            TO WS-RAPP-RAD                            
040500     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
040600     PERFORM S01-SKRIV-RAD                                                
040700                                                                          
040800     MOVE UDET-PRARTBTO         TO LRAD4-PRARTBTO                         
040900     MOVE UDET-VKORDNTO-KOLLI   TO LRAD4-VKORDNTO-KOLLI                   
041000     MOVE UDET-KDERS            TO LRAD4-KDERS                            
041100     MOVE LIST-LRAD4            TO WS-RAPP-RAD                            
041200     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
041300     PERFORM S01-SKRIV-RAD                                                
041400                                                                          
041500     MOVE UDET-IDKOLLI          TO LRAD5-IDKOLLI                          
041600     MOVE UDET-VKTARA           TO LRAD5-VKTARA                           
041700     MOVE UDET-VKART            TO LRAD5-VKART                            
041800     MOVE LIST-LRAD5            TO WS-RAPP-RAD                            
041900     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
042000     PERFORM S01-SKRIV-RAD                                                
042100                                                                          
042200     MOVE UDET-KDFAKTYP         TO LRAD6-KDFAKTYP                         
042300     MOVE UDET-IDFAKT           TO LRAD6-IDFAKT                           
042400     MOVE UDET-VKORDNTO-TOT     TO LRAD6-VKORDNTO-TOT                     
042500     MOVE UDET-PRARTBTO-EXP     TO LRAD6-PRARTBTO-EXP                     
042600     MOVE LIST-LRAD6            TO WS-RAPP-RAD                            
042700     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
042800     PERFORM S01-SKRIV-RAD                                                
042900                                                                          
043000     MOVE UDET-TIFAKT           TO LRAD7-TIFAKT                           
043100     MOVE UDET-KDORDKL          TO LRAD7-KDORDKL                          
043200     MOVE UDET-PRINK            TO LRAD7-PRINK                            
043300     MOVE LIST-LRAD7            TO WS-RAPP-RAD                            
043400     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
043500     PERFORM S01-SKRIV-RAD                                                
043600                                                                          
043700     MOVE UDET-FLDIRLEV         TO LRAD8-FLDIRLEV                         
043800     MOVE UDET-KDPRODSL         TO LRAD8-KDPRODSL                         
043900     MOVE LIST-LRAD8            TO WS-RAPP-RAD                            
044000     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
044100     PERFORM S01-SKRIV-RAD                                                
044200                                                                          
044300     MOVE UDET-IDUSER-PACK      TO LRAD9-IDUSER-PACK                      
044310     INSPECT LRAD9-IDUSER-PACK REPLACING LEADING ZERO BY SPACE            
044400     MOVE UDET-IDFKNGRP         TO LRAD9-IDFKNGRP                         
044500     MOVE LIST-LRAD9            TO WS-RAPP-RAD                            
044600     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
044700     PERFORM S01-SKRIV-RAD                                                
044800                                                                          
044900     MOVE UDET-IDPRODNR         TO LRAD10-IDPRODNR                        
045000     MOVE UDET-TIINVDAT         TO LRAD10-TIINVDAT                        
045100     MOVE LIST-LRAD10           TO WS-RAPP-RAD                            
045200     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
045300     PERFORM S01-SKRIV-RAD                                                
045400                                                                          
045500     MOVE UDET-KVORDRAD         TO LRAD11-KVORDRAD                        
045600     MOVE UDET-KVINVS           TO LRAD11-KVINVS                          
045700     MOVE LIST-LRAD11           TO WS-RAPP-RAD                            
045800     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
045900     PERFORM S01-SKRIV-RAD                                                
046000                                                                          
046100     MOVE UDET-IDUSER-OREG      TO LRAD12-IDUSER-OREG                     
046200     MOVE UDET-IDANSK           TO LRAD12-IDANSK                          
046300     MOVE LIST-LRAD12           TO WS-RAPP-RAD                            
046400     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
046500     PERFORM S01-SKRIV-RAD                                                
046600                                                                          
046700     MOVE UDET-TIREGDAT         TO LRAD13-TIREGDAT                        
046800     MOVE LIST-LRAD13           TO WS-RAPP-RAD                            
046900     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
047000     PERFORM S01-SKRIV-RAD                                                
047100                                                                          
047200     MOVE UDET-BEART            TO LRAD14-BEART                           
047300     MOVE LIST-LRAD14           TO WS-RAPP-RAD                            
047400     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
047500     PERFORM S01-SKRIV-RAD                                                
047600                                                                          
047900                                                                          
048000     PERFORM BBA-FIXA-TEXT-INFO                                           
048100                                                                          
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 BBA-FIXA-TEXT-INFO  SECTION.                                             
050100                                                                          
050200     PERFORM BBAA-TEANMNOT-REG                                            
050300     PERFORM BBAB-TEANMNOT-ADM                                            
050400     PERFORM BBAC-TEANMNOT-REM                                            
050500     PERFORM BBAD-TEANMNOT-RET                                            
050600                                                                          
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 BBAA-TEANMNOT-REG SECTION.                                               
051100                                                                          
051200     MOVE LIST-LRAD15           TO WS-RAPP-RAD                            
051300     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
051400     PERFORM S01-SKRIV-RAD                                                
051500                                                                          
051600     MOVE LIST-STRECKRAD        TO WS-RAPP-RAD                            
051700     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
051800     PERFORM S01-SKRIV-RAD                                                
051900                                                                          
052000     MOVE +1                    TO TEXT-INDX                              
052100     PERFORM UNTIL TEXT-INDX    >  3                                      
052200        MOVE UDET-TEANMNOT-REG (TEXT-INDX)                                
052300                                TO LRAD-TEANMNOT                          
052400        MOVE LIST-LRAD-TEAMNOT  TO WS-RAPP-RAD                            
052500        MOVE PRT-AFTER-1        TO PRT-RADSKIP                            
052600        PERFORM S01-SKRIV-RAD                                             
052700        ADD +1                  TO TEXT-INDX                              
052800     END-PERFORM                                                          
052900                                                                          
053000     .                                                                    
053100     EJECT                                                                
053200                                                                          
053300 BBAB-TEANMNOT-ADM SECTION.                                               
053400                                                                          
053500     MOVE LIST-LRAD16           TO WS-RAPP-RAD                            
053600     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
053700     PERFORM S01-SKRIV-RAD                                                
053800                                                                          
053900     MOVE LIST-STRECKRAD        TO WS-RAPP-RAD                            
054000     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
054100     PERFORM S01-SKRIV-RAD                                                
054200                                                                          
054300     MOVE +1                    TO TEXT-INDX                              
054400     PERFORM UNTIL TEXT-INDX    >  3                                      
054500        MOVE UDET-TEANMNOT-ADM (TEXT-INDX)                                
054600                                TO LRAD-TEANMNOT                          
054700        MOVE LIST-LRAD-TEAMNOT  TO WS-RAPP-RAD                            
054800        MOVE PRT-AFTER-1        TO PRT-RADSKIP                            
054900        PERFORM S01-SKRIV-RAD                                             
055000        ADD +1                  TO TEXT-INDX                              
055100     END-PERFORM                                                          
055200     .                                                                    
055300     EJECT                                                                
055400                                                                          
055500 BBAC-TEANMNOT-REM SECTION.                                               
055600                                                                          
055700     MOVE LIST-LRAD17           TO WS-RAPP-RAD                            
055800     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
055900     PERFORM S01-SKRIV-RAD                                                
056000                                                                          
056100     MOVE LIST-STRECKRAD        TO WS-RAPP-RAD                            
056200     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
056300     PERFORM S01-SKRIV-RAD                                                
056400                                                                          
056500     MOVE +1                    TO TEXT-INDX                              
056600     PERFORM UNTIL TEXT-INDX    >  3                                      
056700        MOVE UDET-TEANMNOT-REM (TEXT-INDX)                                
056800                                TO LRAD-TEANMNOT                          
056900        MOVE LIST-LRAD-TEAMNOT  TO WS-RAPP-RAD                            
057000        MOVE PRT-AFTER-1        TO PRT-RADSKIP                            
057100        PERFORM S01-SKRIV-RAD                                             
057200        ADD +1                  TO TEXT-INDX                              
057300     END-PERFORM                                                          
057400                                                                          
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800 BBAD-TEANMNOT-RET SECTION.                                               
057900                                                                          
058000     MOVE LIST-LRAD18           TO WS-RAPP-RAD                            
058100     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
058200     PERFORM S01-SKRIV-RAD                                                
058300                                                                          
058400     MOVE LIST-STRECKRAD        TO WS-RAPP-RAD                            
058500     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
058600     PERFORM S01-SKRIV-RAD                                                
058700                                                                          
058800     MOVE +1                    TO TEXT-INDX                              
058900     PERFORM UNTIL TEXT-INDX    >  3                                      
059000        MOVE UDET-TEANMNOT-RET (TEXT-INDX)                                
059100                                TO LRAD-TEANMNOT                          
059200        MOVE LIST-LRAD-TEAMNOT  TO WS-RAPP-RAD                            
059300        MOVE PRT-AFTER-1        TO PRT-RADSKIP                            
059400        PERFORM S01-SKRIV-RAD                                             
059500        ADD +1                  TO TEXT-INDX                              
059600     END-PERFORM                                                          
059700                                                                          
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 Z-FINIT                   SECTION.                                       
060200                                                                          
060300     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
060400                         PRT-CLOSE                                        
060500                         WS-RAPP-PRINTER                                  
060600                         ALT-PCB                                          
060700                         WS-DUMMY                                         
060800                         WS-DUMMY                                         
060900     .                                                                    
061000     EJECT                                                                
061100 S01-SKRIV-RAD SECTION.                                                   
061200                                                                          
061300     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
061400                         PRT-WRITE                                        
061500                         WS-RAPP-PRINTER                                  
061600                         ALT-PCB                                          
061700                         PRT-RADSKIP                                      
061800                         WS-RAPP-LISTRAD                                  
061900                                                                          
062000     MOVE SPACE                TO WS-RAPP-LISTRAD                         
062100     .                                                                    
062200     EJECT                                                                
