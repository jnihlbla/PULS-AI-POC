000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3715600.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   95/01/04.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER NER FÄRDIGA FAKTUROR TILL TULLEN EKONOMI                   
000900*        OCH FÖR PRINTNING.                                               
001000*        OBS !!! NÄR MAN SKICKAR GODS TILL MAASTRICHT                     
001100*        DISTRIKTET ALLTID 9927, SKICKAS NÅGOT FRÅN MAASTRICHT            
001200*        DÅ ÄR DISTRIKTET ALLTID ???????                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER OCH UPPD      WL3171 (WDR4)                     
001500*        PROGRAMMET LÄSER               WLARTC (WDK6)                     
001600*        PROGRAMMET LÄSER               WLBENA (WDD3)                     
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200*  ETRACKER 5594753/071029/EÖ                                             
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000                                                                          
003100*          --- TULLDATA PTYP RZQ O RZR                                    
003200     SELECT W37157                     ASSIGN TO W37156D3.                
003300*          --- PULS ECONOMICAL DATA LOG                                   
003400     SELECT W37160                     ASSIGN TO W37156D5.                
003500*          --- IDFAKT PARAMETER FRÅN W3018200                             
003600     SELECT INDATA                     ASSIGN TO W37156D6.                
003700*          --- LISTA TILL D&P DC 91                                       
003800     SELECT DAPLISTA21                 ASSIGN TO W37156D7.                
003900*          --- LISTA TILL D&P DC61                                        
004000     SELECT DAPLISTA61                 ASSIGN TO W37156D8.                
004100*          --- LISTA TILL D&P DC62                                        
004200     SELECT DAPLISTA62                 ASSIGN TO W37156D9.                
004300     EJECT                                                                
004400*          --- LISTA TILL DOC. RETRIEVAL DC62                             
004500     SELECT DOCRLISTA62                ASSIGN TO W37156DA.                
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800     SKIP3                                                                
004900 FILE SECTION.                                                            
005000                                                                          
005100 FD  W37157                                                               
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY WDGZRZR -PRE  UT57RZR-  -L.                               
005600                                                                          
005700*01  POST -COPY WDGZRZQ -PRE  UT57RZQ-  -L.                               
005800     SKIP3                                                                
005900 FD  W37160                                                               
006000     LABEL RECORD STANDARD                                                
006100     RECORDING F                                                          
006200     BLOCK CONTAINS 0.                                                    
006300     SKIP2                                                                
006400*01  POST -COPY  W51060 -PRE UT51-   -L.                                  
006500     EJECT                                                                
006600 FD  INDATA                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900     SKIP2                                                                
007000 01  INPOST              PIC X(80).                                       
007100     SKIP3                                                                
007200 FD  DAPLISTA21                                                           
007300     RECORDING       V                                                    
007400     BLOCK CONTAINS  0.                                                   
007500     SKIP2                                                                
007600 01  DAPPOST21           PIC X(134).                                      
007700     SKIP3                                                                
007800 FD  DAPLISTA61                                                           
007900     RECORDING       V                                                    
008000     BLOCK CONTAINS  0.                                                   
008100     SKIP2                                                                
008200 01  DAPPOST61           PIC X(134).                                      
008300     SKIP3                                                                
008400 FD  DAPLISTA62                                                           
008500     RECORDING       V                                                    
008600     BLOCK CONTAINS  0.                                                   
008700     SKIP2                                                                
008800 01  DAPPOST62           PIC X(134).                                      
008900     SKIP3                                                                
009000 FD  DOCRLISTA62                                                          
009100     RECORDING       V                                                    
009200     BLOCK CONTAINS  0.                                                   
009300     SKIP2                                                                
009400 01  DOCRPOST62          PIC X(134).                                      
009500     SKIP3                                                                
009600 WORKING-STORAGE SECTION.                                                 
009700     SKIP2                                                                
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                   PIC X(8)  VALUE 'W3715600'.                  
010100 77  JA                      PIC X     VALUE 'J'.                         
010200 77  NEJ                     PIC X     VALUE 'N'.                         
010300                                                                          
010400 77  WS-IDDC                 PIC X(2)  VALUE SPACE.                       
010500 77  WS-SDC-91               PIC X(2)  VALUE '91'.                        
010600                                                                          
010700 77  FAKTURA-KLAR            PIC X     VALUE 'N'.                         
010800 77  DUMMY-AREA              PIC X(50) VALUE SPACE.                       
010900 77  LIST-MAASTRI-AD         PIC X(15) VALUE 'GRAANMOLEN 9-11'.           
011100 77  LIST-MAASTRI-CITY     PIC X(18) VALUE '6229 PA MAASTRICHT'.          
011200 77  LIST-MAASTRI-COUNTRY    PIC X(11) VALUE 'NETHERLANDS'.               
011300 77  LIST-MAASTRI-IDDISTR    PIC X(4)  VALUE '9927'.                      
011400 77  LIST-NAGOYA-AD          PIC X(16) VALUE 'CENTRE NDC     '.           
011500 77  LIST-MINTO-AD           PIC X(16) VALUE 'CENTRE NDC     '.           
011600 77  LIST-MINTO-CITY         PIC X(16) VALUE 'MINTO          '.           
011700 77  LIST-NAGOYA-CITY        PIC X(16) VALUE 'NAGOYA         '.           
011800 77  RAD-INDX                PIC S9(4) VALUE +0    COMP SYNC.             
011900 77  INDX                    PIC S9(4) VALUE +0    COMP SYNC.             
012000 77  MAX-INDX                PIC S9(4) VALUE +11   COMP SYNC.             
012100 77  WS-IDDISTR              PIC S9(5) VALUE ZERO.                        
012200 77  WS-IDFAKT               PIC S9(7) VALUE ZERO COMP-3.                 
012300 77  WS-RED-VOLYM            PIC Z(3)9.9(3) VALUE ZERO.                   
012400 77  WS-RED-ANT-KOLLI        PIC Z(7)9 VALUE ZERO.                        
012500 77  WS-IDKOLLI              PIC S9(5) VALUE ZERO COMP-3.                 
012600 77  WS-VKORDBTO-FAKT        PIC S9(6)V9(1) VALUE ZERO COMP-3.            
012700 77  WS-VLORDBTO-FAKT        PIC S9(4)V9(3) VALUE ZERO COMP-3.            
012800 77  SPAR-3171-IDDC          PIC X(2)  VALUE SPACE.                       
012900 77  SPAR-3171-IDFAKT        PIC S9(7) VALUE ZERO COMP-3.                 
013000 77  SPAR-3171-IDKOLLI       PIC S9(5) VALUE ZERO COMP-3.                 
013100 77  SPAR-3171-TIFAKT        PIC S9(7) VALUE ZERO COMP-3.                 
013200 77  SPAR-3171-DAFAKT        PIC  9(8) VALUE ZERO.                        
013300 77  WS-ANT-KOLLI            PIC S9(5) VALUE ZERO COMP-3.                 
013400 77  W-3172-IDDC-SEND        PIC X(2)  VALUE SPACE.                       
013500 77  W-3172-IDFAKT           PIC S9(7) VALUE ZERO COMP-3.                 
013600 77  W-3172-SUFKTNTO         PIC S9(11)V9(2) VALUE ZERO COMP-3.           
013700 77  W-3172-DASNDDAT         PIC 9(8)  VALUE ZERO.                        
013800 77  W-3172-VKORDBTO-FAKT    PIC S9(6)V9(1) VALUE ZERO COMP-3.            
013900 77  W-3174-VLORDBTO-KOLLI   PIC S9(4)V9(3) VALUE ZERO COMP-3.            
014000 77  W-3174-VKORDBTO-KOLLI   PIC S9(6)V9(1) VALUE ZERO COMP-3.            
014100                                                                          
014200                                                                          
014300 01  FILLER                  PIC X(16) VALUE 'WS-SEKTION'.                
014400 01  WS-SEKTION              PIC X(30) VALUE SPACE.                       
014500 01  FILLER                  PIC X(16) VALUE 'WS-IMS-SEKTION'.            
014600 01  WS-IMS-SEKTION          PIC X(30) VALUE SPACE.                       
014700 01  FILLER                  PIC X(16) VALUE 'WS-FIL-SEKTION'.            
014800 01  WS-FIL-SEKTION          PIC X(30) VALUE SPACE.                       
014900                                                                          
015000     EJECT                                                                
015100 01  WS-TIFAKT               PIC 9(6)  VALUE ZERO.                        
015200 01  FILLER REDEFINES WS-TIFAKT.                                          
015300     03  TIFAKT-AAR          PIC 9(2).                                    
015400     03  TIFAKT-MAANAD       PIC 9(4).                                    
015500                                                                          
015600 01  WS-RED-TIFAKT           PIC 9(8)  VALUE ZERO.                        
015700 01  FILLER REDEFINES WS-RED-TIFAKT.                                      
015800     03  TIFAKT-SEKEL        PIC 9(2).                                    
015900     03  TIFAKT-REST         PIC 9(6).                                    
016000                                                                          
016100 77  WS-SUARTSTD-TOT         PIC S9(5)V9(2) VALUE ZERO COMP-3.            
016200     SKIP2                                                                
016300*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
016400 77  WS-AAAAMMDD             PIC 9(8)  VALUE ZERO.                        
016500 77  WS-TTMMSSTH             PIC 9(8)  VALUE ZERO.                        
016600 01  FILLER                  PIC X(11) VALUE 'IDPRTLST ? '.               
016700 01  WS-PRT-IDPRTLST2        PIC X(8)  VALUE '37156B  '.                  
016800 01  WS-PRT-IDPRTLST3        PIC X(8)  VALUE 'W3715661'.                  
016900 01  WS-PRT-IDPRTLST4        PIC X(8)  VALUE 'W3715662'.                  
017000 01  PRT-AREA.                                                            
017100     03 WS-PRT-IDPRTLST      PIC X(8)  VALUE '37156A  '.                  
017200     03 WS-PRT-IDLIST.                                                    
017300        05 WS-IDLIST         PIC X(3)  VALUE 'BYT'.                       
017400        05 WS-PRT-IDFAKT     PIC 9(7).                                    
017500     03 WS-PRT-LISTRAD.                                                   
017600        05 FILLER            PIC X(2)  VALUE SPACE.                       
017700        05 WS-RAD            PIC X(130).                                  
017800     03 WS-PRT-DUMMY         PIC X(132) VALUE SPACE.                      
017900 01  PRT-DAP-AREA.                                                        
018000     03 WS-DAP-LINE          PIC X(130)  VALUE SPACE.                     
018100 01  FELTEXT.                                                             
018200     03  FILLER              PIC X(8)  VALUE 'FELTEXT'.                   
018300     03  FELTEXT-STR         PIC X(72) VALUE SPACE.                       
018400     EJECT                                                                
018500 01  WS-KDARTURS             PIC X(2)  VALUE SPACE.                       
018600                                                                          
018700 01  FILLER                  PIC X(12) VALUE 'DAGENS-DATUM'.              
018800 01  DAGENS-DATUM            PIC 9(6)  VALUE ZERO.                        
018900 01  FILLER REDEFINES DAGENS-DATUM.                                       
019000     03  DAGENS-DATUM-AAR    PIC 9(2).                                    
019100     03  DAGENS-DATUM-MAANAD PIC 9(2).                                    
019200     03  DAGENS-DATUM-DAG    PIC 9(2).                                    
019300     EJECT                                                                
019400 01  DYNAMISKA-SUBPROGRAM.                                                
019500*                                                                         
019600     03  CBLTDLI             PIC X(8)  VALUE 'CBLTDLI '.                  
019700     03  FELLOG              PIC X(8)  VALUE 'FELLOG  '.                  
019800     03  POSTSUM             PIC X(8)  VALUE 'POSTSUM'.                   
019900     03  W006PRR2            PIC X(8)  VALUE 'W006PRR2'.                  
020000     03  W009CIA             PIC X(8)  VALUE 'W009CIA'.                   
020100     EJECT                                                                
020200*    --- PARAMETRAR TILL POSTSUM                                          
020300*                                                                         
020400*01  -COPY W0005   -PRE  POSTSUM-                                         
020500     EJECT                                                                
020600 01  W006-AREA-START         PIC X(24) VALUE 'W006-AREA-START'.           
020700                                                                          
020800* VARIABLER TILL SUBPROGRAM W006PRAR                                      
020900*01  -COPY W006PRAR                                                       
021000     SKIP2                                                                
021100*    --- PARAMETRAR TILL W009CIA                                          
021200*01  -COPY W009CIA                                                        
021300     EJECT                                                                
021400*    --- LISTAN                                                           
021500 01  FILLER                  PIC X(10) VALUE 'RUBRIK-RAD'.                
021600 01  RUBRIK-RAD.                                                          
021700     03  FILLER              PIC X(4)  VALUE SPACE.                       
021800     03  FILLER              PIC X(26) VALUE                              
021900                                     'VOLVO CAR CUSTOMER SERVICE'.        
022000     03  FILLER              PIC X(14) VALUE SPACE.                       
022100     03  FILLER              PIC X(16) VALUE 'K-INVOICE       '.          
022200     03  FILLER              PIC X(8)  VALUE 'W37156-0'.                  
022300     03  LIST-DC             PIC X(2)  VALUE SPACE.                       
022400     03  FILLER              PIC X(21) VALUE SPACE.                       
022500     03  FILLER              PIC X(7)  VALUE 'INVOICE'.                   
022600     03  FILLER              PIC X(29) VALUE SPACE.                       
022700                                                                          
022800 01  FILLER                  PIC X(14) VALUE 'U-RUBRIK-RAD1A'.            
022900 01  U-RUBRIK-RAD1A.                                                      
023000     03  FILLER              PIC X(4)  VALUE SPACE.                       
023100     03  FILLER              PIC X(51) VALUE SPACE.                       
023200     03  FILLER              PIC X(22) VALUE SPACE.                       
023300     03  FILLER              PIC X(7)  VALUE 'VAT NO.'.                   
023400     03  FILLER              PIC X(22) VALUE SPACE.                       
023500     03  FILLER              PIC X(24) VALUE SPACE.                       
023600                                                                          
023700 01  FILLER                  PIC X(13) VALUE 'U-RUBRIK-RAD1'.             
023800 01  U-RUBRIK-RAD1.                                                       
023900     03  FILLER              PIC X(4)  VALUE SPACE.                       
024000     03  FILLER              PIC X(10) VALUE 'DISPATCHER'.                
024100     03  FILLER              PIC X(33) VALUE SPACE.                       
024200     03  FILLER              PIC X(8)  VALUE 'RECEIVER'.                  
024300     03  FILLER              PIC X(22) VALUE SPACE.                       
024400     03  LIST-MAASTRI-VAT    PIC X(14) VALUE SPACE.                       
024500     03  FILLER              PIC X(15) VALUE SPACE.                       
024600     03  FILLER              PIC X(10) VALUE 'PAGE      '.                
024700     03  FILLER              PIC X(14) VALUE SPACE.                       
024800                                                                          
024900*MAASTRICHT                                                               
025000 01  FILLER                  PIC X(13) VALUE 'U-RUBRIK-RAD2'.             
025100 01  U-RUBRIK-RAD2.                                                       
025200     03  FILLER              PIC X(4)  VALUE SPACE.                       
025300     03  FILLER              PIC X(26) VALUE                              
025400                                     'VOLVO CAR CUSTOMER SERVICE'.        
025500     03  FILLER              PIC X(15) VALUE SPACE.                       
025600     03  FILLER              PIC X(26) VALUE                              
025700                                     'VOLVO CAR CUSTOMER SERVICE'.        
025800     03  FILLER              PIC X(6)  VALUE SPACE.                       
025900     03  LIST-VAT            PIC X(14) VALUE SPACE.                       
026000     03  FILLER              PIC X(15) VALUE SPACE.                       
026100     03  LIST-SIDA           PIC 9(4)  VALUE ZERO.                        
026200     03  FILLER              PIC X(6)  VALUE SPACE.                       
026300                                                                          
026400                                                                          
026500 01  FILLER                  PIC X(13) VALUE 'U-RUBRIK-RAD2'.             
026600 01  U-RUBRIK-RAD3.                                                       
026700     03  FILLER              PIC X(4)  VALUE SPACE.                       
026800     03  LIST-SENDER-AD      PIC X(16) VALUE SPACE.                       
026900     03  FILLER              PIC X(11) VALUE SPACE.                       
027000     03  FILLER              PIC X(16) VALUE SPACE.                       
027100     03  LIST-RECIVER-AD     PIC X(15) VALUE SPACE.                       
027100     03  FILLER              PIC X(15) VALUE SPACE.                       
027200     03  FILLER              PIC X(4)  VALUE 'DATE'.                      
027300     03  FILLER              PIC X(3)  VALUE SPACE.                       
027400     03  FILLER              PIC X(10) VALUE 'DISTR.NO. '.                
027500     03  FILLER              PIC X(11) VALUE 'INVOICE NO.'.               
027600     03  FILLER              PIC X(25) VALUE SPACE.                       
027700                                                                          
027800                                                                          
027900 01  FILLER                  PIC X(13) VALUE 'U-RUBRIK-RAD4'.             
028000 01  U-RUBRIK-RAD4.                                                       
028100     03  FILLER              PIC X(4)  VALUE SPACE.                       
028200     03  LIST-SENDER-CITY    PIC X(16) VALUE SPACE.                       
028300     03  FILLER              PIC X(27) VALUE SPACE.                       
028400     03  LIST-RECIVER-CITY   PIC X(18) VALUE SPACE.                       
028500     03  FILLER              PIC X(12) VALUE SPACE.                       
028600     03  LIST-AAR            PIC X(2)  VALUE SPACE.                       
028700     03  LIST-MAN            PIC X(2)  VALUE SPACE.                       
028800     03  LIST-DAG            PIC X(2)  VALUE SPACE.                       
028900     03  FILLER              PIC X(1)  VALUE SPACE.                       
029000     03  LIST-IDDISTR        PIC X(4)  VALUE SPACE.                       
029100     03  FILLER              PIC X(6)  VALUE SPACE.                       
029200     03  LIST-IDFAKT         PIC X(7)  VALUE SPACE.                       
029300     03  FILLER              PIC X(29) VALUE SPACE.                       
029400                                                                          
029500 01  FILLER                  PIC X(11) VALUE 'SISTA-RADEN'.               
029600 01  SISTA-RADEN.                                                         
029700     03  FILLER              PIC X(4)  VALUE SPACE.                       
029800     03  FILLER              PIC X(14) VALUE 'GROSS WEIGHT  '.            
029900     03  LIST-GROSS-WEIGHT   PIC Z(5)9.9(1) VALUE ZERO.                   
030000     03  FILLER              PIC X(4)  VALUE SPACE.                       
030100     03  FILLER              PIC X(14) VALUE 'TOTAL VALUE   '.            
030200     03  LIST-SUARTSTD-TOT   PIC Z(5).9(2) VALUE ZERO.                    
030300     03  FILLER              PIC X(4)  VALUE SPACE.                       
030400     03  WS-TOT-VOLUME-RUB   PIC X(14) VALUE 'TOTAL VOLUME  '.            
030500     03  WS-TOT-VOLUME       PIC X(08) VALUE SPACE.                       
030600     03  FILLER              PIC X(4)  VALUE SPACE.                       
030700     03  WS-TOT-CASE-RUB     PIC X(14) VALUE 'NO OF CASES   '.            
030800     03  WS-TOT-KOLLI        PIC X(08) VALUE SPACE.                       
030900     03  FILLER              PIC X(26) VALUE SPACE.                       
031000                                                                          
031100                                                                          
031200 01  FILLER                  PIC X(13) VALUE 'U-RUBRIK-RAD6'.             
031300 01  U-RUBRIK-RAD6.                                                       
031400     03  FILLER              PIC X(4)  VALUE SPACE.                       
031500     03  FILLER              PIC X(5)  VALUE 'CASE '.                     
031600     03  FILLER              PIC X(2)  VALUE SPACE.                       
031700     03  FILLER              PIC X(9)  VALUE '  PART NO'.                 
031800     03  FILLER              PIC X(2)  VALUE SPACE.                       
031900     03  FILLER              PIC X(10) VALUE 'PART NAME ' .               
032000     03  FILLER              PIC X(13) VALUE SPACE.                       
032100     03  FILLER              PIC X(1)  VALUE SPACE.                       
032200     03  FILLER              PIC X(5)  VALUE 'Q.DEL' .                    
032300     03  FILLER              PIC X(2)  VALUE SPACE.                       
032400     03  FILLER              PIC X(7)  VALUE 'U-PRICE' .                  
032500     03  FILLER              PIC X(2)  VALUE SPACE.                       
032600     03  FILLER              PIC X(7)  VALUE 'T-PRICE' .                  
032700     03  FILLER              PIC X(1)  VALUE SPACE.                       
032800     03  FILLER              PIC X(3)  VALUE 'ORG' .                      
032900     03  FILLER              PIC X(1)  VALUE SPACE.                       
033000     03  FILLER              PIC X(7)  VALUE 'NET WGT'.                   
033100     03  FILLER              PIC X(2)  VALUE SPACE.                       
033200     03  FILLER              PIC X(9)  VALUE 'STAT.NO. ' .                
033300     03  FILLER              PIC X(2)  VALUE SPACE.                       
033400     03  WS-VOLUME-RUB       PIC X(6)  VALUE 'VOLUME'.                    
033500     03  FILLER              PIC X(2)  VALUE SPACE.                       
033600     03  WS-VIKT-RUB         PIC X(9)  VALUE 'GROSS WGT'.                 
033700     03  FILLER              PIC X(17) VALUE SPACE.                       
033800                                                                          
033900                                                                          
034000 01  FILLER                  PIC X(05) VALUE 'U-RAD'.                     
034100 01  U-RAD.                                                               
034200     03  FILLER              PIC X(4)  VALUE SPACE.                       
034300     03  LIST-IDKOLLI        PIC Z(5).                                    
034400     03  FILLER              PIC X(2)  VALUE SPACE.                       
034500     03  LIST-IDARTNR        PIC Z(9).                                    
034600     03  FILLER              PIC X(2)  VALUE SPACE.                       
034700     03  LIST-BEART          PIC X(25) VALUE SPACE.                       
034800     03  LIST-KVCLEAR        PIC Z(4).                                    
034900     03  FILLER              PIC X(1)  VALUE SPACE.                       
035000     03  LIST-PRARTSTD       PIC Z(5).9(2) VALUE ZERO.                    
035100     03  FILLER              PIC X(1)  VALUE SPACE.                       
035200     03  LIST-PRARTSTD-RAD   PIC Z(5).9(2) VALUE ZERO.                    
035300     03  FILLER              PIC X(1)  VALUE SPACE.                       
035400     03  LIST-KDARTURS       PIC X(2)  VALUE SPACE.                       
035500     03  FILLER              PIC X(1)  VALUE SPACE.                       
035600     03  FILLER              PIC X(1)  VALUE SPACE.                       
035700     03  LIST-VKART          PIC Z(7)  VALUE ZERO.                        
035800     03  FILLER              PIC X(1)  VALUE SPACE.                       
035900     03  LIST-IDSTAT         PIC Z(9)  VALUE ZERO.                        
036000     03  FILLER              PIC X(2)  VALUE SPACE.                       
036100     03  FILLER              PIC X(28) VALUE SPACE.                       
036200     03  FILLER              PIC X(7)  VALUE SPACE.                       
036300     EJECT                                                                
036400                                                                          
036500 01  FILLER                  PIC X(06) VALUE 'U-RAD2'.                    
036600 01  U-RAD2.                                                              
036700     03  FILLER              PIC X(4)  VALUE SPACE.                       
036800     03  FILLER              PIC X(88) VALUE SPACE.                       
036900     03  LIST-VOLYM          PIC Z(3)9.9(3) VALUE ZERO.                   
037000     03  FILLER              PIC X(3)  VALUE SPACE.                       
037100     03  LIST-VIKT           PIC Z(5)9.9(1) VALUE ZERO.                   
037200     03  FILLER              PIC X(12) VALUE SPACE.                       
037300     03  FILLER              PIC X(3)  VALUE SPACE.                       
037400     EJECT                                                                
037500 01  UT57-AREA-START         PIC X(24) VALUE 'UT57-AREA-START'.           
037600 01  UT57-AREA               PIC X(200).                                  
037700 01  FILLER -COPY WDGZRZR  -PRE UT57RZR-  -RED  UT57-AREA                 
037800 01  FILLER -COPY WDGZRZQ  -PRE UT57RZQ-  -RED  UT57-AREA                 
037900     EJECT                                                                
038000 01  UT51-AREA-START         PIC X(24) VALUE 'UT51-AREA-START'.           
038100*01  AREA   -COPY  W51060  -PRE UT51-                                     
038200                                                                          
038300 01  IN-AREA-START           PIC X(16) VALUE 'IN-AREA-START'.             
038400    SKIP2                                                                 
038500 01  IN-AREA.                                                             
038600        03  IN-IDFAKT        PIC X(7).                                    
038700        03  FILLER           PIC X(73).                                   
038800     EJECT                                                                
038900 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
039000     SKIP3                                                                
039100 01  NYCKLAR-TILL-DLI.                                                    
039200     03  W-WDGXKEY-X.                                                     
039300         05  FILLER          PIC X(4)  VALUE '3171'.                      
039400         05  FILLER          PIC X(26) VALUE LOW-VALUE.                   
039500     03  W-IDFAKT-X.                                                      
039600         05  W-IDFAKT        PIC S9(7) VALUE ZERO COMP-3.                 
039700     03  W-IDKOLLI-X.                                                     
039800         05  W-IDKOLLI       PIC S9(5) VALUE ZERO COMP-3.                 
039900     03  W-IDARTNR-X.                                                     
040000         05  W-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.                 
040100     03  W-IDSKYLT-X.                                                     
040200         05  W-IDSKYLT       PIC X(3)  VALUE 'GB '.                       
040300     03  W-KDSEGKEY-X.                                                    
040400         05  W-KDSEGKEY      PIC X(1)  VALUE SPACE.                       
040500                                                                          
040600     03  W-IDDC-B6-X.                                                     
040700         05 W-IDDC-B6                  PIC X(2).                          
040800                                                                          
040900     SKIP2                                                                
041000*    --- STATUS-KOD FRÅN IMS                                              
041100 01  STATUS-WS               PIC XX.                                      
041200     88  SEGMENT-FINNS                 VALUE '  '.                        
041300     88  SEGMENT-FINNS-REDAN           VALUE 'II'.                        
041400     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
041500     88  SEGMENT-DATA                  VALUE 'GA'.                        
041600     88  BAS-SLUT                      VALUE 'GB'.                        
041700     88  IMS-EJ-OK                     VALUE 'XD'.                        
041800     SKIP2                                                                
041900 01  GODK-STATUSKODER.                                                    
042000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042100     SKIP3                                                                
042200 01  SSA1                    PIC X(64).                                   
042300 01  SSA2                    PIC X(64).                                   
042400     EJECT                                                                
042500*    --- IMS FUNKTIONSKODER                                               
042600*01  -COPY W0003                                                          
042700     EJECT                                                                
042800*    ---  DLI INPUT-OUTPUT AREA                                           
042900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
043000     SKIP3                                                                
043100 01  DLI-IO-AREA.                                                         
043200     03  IO-AREA             PIC X(900) VALUE SPACE.                      
043300     SKIP3                                                                
043400     03  WLARTC01 REDEFINES IO-AREA.                                      
043500*        05  -COPY WDK601  -PRE ARTC01-                                   
043600     SKIP3                                                                
043700     03  WLARTC11 REDEFINES IO-AREA.                                      
043800*        05  -COPY WDK611  -PRE ARTC11-                                   
043900     EJECT                                                                
044000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
044100 01  DLI-IO-AREA2.                                                        
044200     03  IO-AREA2            PIC X(200) VALUE SPACE.                      
044300     SKIP3                                                                
044400     03  WLBENA11 REDEFINES IO-AREA2.                                     
044500         05  -COPY WDD311  -PRE BENA-                                     
044600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA1'.              
044700 01  DLI-IO-AREA1.                                                        
044800     03  IO-AREA1            PIC X(600) VALUE SPACE.                      
044900     SKIP3                                                                
045000     03  WL317111 REDEFINES IO-AREA1.                                     
045100*        05  -COPY WDGX3172  -PRE 3171-                                   
045200     03  WL317121 REDEFINES IO-AREA1.                                     
045300*        05  -COPY WDGX3174  -PRE 3171-                                   
045400     03  WL317131 REDEFINES IO-AREA1.                                     
045500*        05  -COPY WDGX3176  -PRE 3171-                                   
045600                                                                          
045700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045800 01   DLI-IO-AREA-B601.                                                   
045900*     03  -COPY WDB601                                                    
046000                                                                          
046100     EJECT                                                                
046200 LINKAGE SECTION.                                                         
046300                                                                          
046400*01  -COPY W0009   -PRE MSG-                                              
046500     EJECT                                                                
046600*01  -COPY W0009   -PRE ALT-                                              
046700     EJECT                                                                
046800*01  -COPY W0009   -PRE ALT2-                                             
046900     EJECT                                                                
047000*01  -COPY W0008  -PRE LISB-                                              
047100     05  FILLER              PIC X.                                       
047200     EJECT                                                                
047300*01  -COPY W0008  -PRE 3171-                                              
047400     05  FILLER              PIC X.                                       
047500     EJECT                                                                
047600*01  -COPY W0008  -PRE ARTC-                                              
047700     05  FILLER              PIC X.                                       
047800     EJECT                                                                
047900*01  -COPY W0008  -PRE BENA-                                              
048000     05  FILLER              PIC X.                                       
048100     EJECT                                                                
048200*01  -COPY W0008  -PRE WDB6-                                              
048300     05  FILLER              PIC X.                                       
048400     EJECT                                                                
048500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALT2-PCB LISB-PCB              
048600                           3171-PCB ARTC-PCB BENA-PCB WDB6-PCB.           
048700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT2-PCB LISB-PCB              
048800                           3171-PCB ARTC-PCB BENA-PCB WDB6-PCB.           
048900                                                                          
049000     SKIP2                                                                
049100     PERFORM A-INIT                                                       
049200     PERFORM IMS-GU-INVOICE                                               
049300     PERFORM B-INITIERA-LISTAN                                            
049400     PERFORM IMS-GNP-KOLLI-RADER                                          
049500                                                                          
049600     PERFORM UNTIL SEGMENT-SAKNAS                                         
049700             OR BAS-SLUT                                                  
049800             OR 3171-SEG-NAME-FB = 'WL317101'                             
049900             OR 3171-SEG-NAME-FB = 'WL317111'                             
050000                                                                          
050100             IF 3171-SEG-NAME-FB = 'WL317121'                             
050200                PERFORM C-SKAPA-KOLLI-RAD                                 
050300             END-IF                                                       
050400                                                                          
050500             IF 3171-SEG-NAME-FB = 'WL317131'                             
050600                PERFORM F-BEH-FAKT-HUVUD                                  
050700                PERFORM E-UPDATE-ARTNR-FIELDS                             
050800                PERFORM G-BEH-FAKT-LINE                                   
050900             END-IF                                                       
051000                                                                          
051100             PERFORM IMS-GNP-KOLLI-RADER                                  
051200     END-PERFORM                                                          
051300     PERFORM D-SKRIV-SISTA-POSTEN                                         
051400     PERFORM H-SISTA-RADEN                                                
051500     PERFORM IMS-GHU-INVOICE                                              
051600     MOVE '2' TO 3171-3172-KDTRSTAT                                       
051700     PERFORM IMS-REPL-INVOICE                                             
051800                                                                          
051900     PERFORM Z-FINIT                                                      
052000                                                                          
052100     MOVE ZERO TO RETURN-CODE                                             
052200     GOBACK                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 A-INIT SECTION.                                                          
052600     MOVE 'A-INIT'   TO WS-SEKTION                                        
052700*    DISPLAY WS-SEKTION                                                   
052800     SKIP2                                                                
052900                                                                          
053000     OPEN INPUT INDATA                                                    
053100     READ INDATA INTO IN-AREA                                             
053200     END-READ                                                             
053300     CLOSE INDATA                                                         
053400     MOVE IN-IDFAKT TO W-IDFAKT                                           
053500     DISPLAY ' FAKTURA ' IN-IDFAKT                                        
053600                                                                          
053700     OPEN OUTPUT W37157                                                   
053800                 W37160                                                   
053900                 DAPLISTA21                                               
054000                 DAPLISTA61                                               
054100                 DAPLISTA62                                               
054200                 DOCRLISTA62                                              
054300                                                                          
054400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
054500     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
054600     MOVE  999 TO RAD-INDX                                                
054700     MOVE ZERO       TO WS-ANT-KOLLI                                      
054800                                                                          
054900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
055000     SKIP2                                                                
055100     .                                                                    
055200     EJECT                                                                
055300 B-INITIERA-LISTAN SECTION.                                               
055400     MOVE 'B-INITIERA-LISTAN' TO WS-SEKTION                               
055500*    DISPLAY WS-SEKTION                                                   
055600     SKIP2                                                                
055700*MAASTRICHT                                                               
055800     MOVE 3171-3172-IDDC-SEND        TO WS-IDDC                           
055900                                                                          
056000     IF DCS-IDDC NOT = WS-IDDC                                            
056100        MOVE WS-IDDC TO W-IDDC-B6                                         
056200        PERFORM IMS-GU-WDB601                                             
056300     END-IF                                                               
056400     IF DCS-SDC AND DCS-HOLLAND                                           
056500*MAASTRICHT                                                               
056600                                                                          
056700        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
056800                         PRT-OPEN                                         
056900                         WS-PRT-IDPRTLST2                                 
057000                         ALT2-PCB                                         
057100                         LISB-PCB                                         
057200                         WS-PRT-IDLIST                                    
057300                         WS-PRT-DUMMY                                     
057400                         WS-PRT-DUMMY                                     
057500     END-IF                                                               
057600     IF DCS-NDC-PF AND DCS-JAPAN                                          
057700*NAGOYA                                                                   
057800                                                                          
057900        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
058000                         PRT-OPEN                                         
058100                         WS-PRT-IDPRTLST3                                 
058200                         ALT2-PCB                                         
058300                         LISB-PCB                                         
058400                         WS-PRT-IDLIST                                    
058500                         WS-PRT-DUMMY                                     
058600                         WS-PRT-DUMMY                                     
058700     END-IF                                                               
058800                                                                          
058900     MOVE 3171-3172-VKORDBTO-FAKT TO W-3172-VKORDBTO-FAKT                 
059000     MOVE 3171-3172-IDDC-SEND    TO WS-IDDC                               
059100                                    W-3172-IDDC-SEND                      
059200     MOVE 3171-3172-IDFAKT       TO W-3172-IDFAKT                         
059300     MOVE 3171-3172-SUFKTNTO     TO W-3172-SUFKTNTO                       
059400     MOVE 3171-3172-DASNDDAT     TO W-3172-DASNDDAT                       
059500                                                                          
059600     IF DCS-SDC AND DCS-HOLLAND                                           
059700        MOVE +9927 TO WS-IDDISTR                                          
059800     END-IF                                                               
059900**********************************************************                
060000*** PGA ATT MAASTRICHT ALLTID FINNS MED SOM MOTTAGARE  ***                
060100*** ELLER SOM SÄNDARE                                  ***                
060200*** DVS OM XXXXXXXX ÄR SÄNDARE SÅ ÄR MAASTRICHT        ***                
060300*** MOTTAGARE ELLER TVÄRTOM                            ***                
060400**********************************************************                
060500     IF DCS-IDDC NOT = WS-SDC-91                                          
060600        MOVE WS-SDC-91 TO W-IDDC-B6                                       
060700        PERFORM IMS-GU-WDB601                                             
060800     END-IF                                                               
060900     MOVE DCS-IDVAT TO LIST-MAASTRI-VAT                                   
061000                                                                          
061100     IF DCS-IDDC NOT = WS-IDDC                                            
061200        MOVE WS-IDDC   TO W-IDDC-B6                                       
061300        PERFORM IMS-GU-WDB601                                             
061400     END-IF                                                               
061500     IF DCS-NDC-PF AND DCS-JAPAN                                          
061600        MOVE LIST-NAGOYA-AD TO LIST-SENDER-AD                             
061800        MOVE LIST-MAASTRI-AD TO LIST-RECIVER-AD                           
061700        MOVE LIST-NAGOYA-CITY TO LIST-SENDER-CITY                         
061800        MOVE LIST-MAASTRI-CITY TO LIST-RECIVER-CITY                       
061900        MOVE LIST-MAASTRI-IDDISTR TO LIST-IDDISTR                         
062000        MOVE DCS-IDVAT TO LIST-VAT                                        
062100     END-IF                                                               
062200     IF DCS-NDC-PF AND DCS-AUSTRALIA                                      
062300        MOVE LIST-MINTO-AD     TO LIST-SENDER-AD                          
061800        MOVE LIST-MAASTRI-AD TO LIST-RECIVER-AD                           
062400        MOVE LIST-MINTO-CITY   TO LIST-SENDER-CITY                        
062500        MOVE LIST-MAASTRI-CITY TO LIST-RECIVER-CITY                       
062600        MOVE LIST-MAASTRI-IDDISTR TO LIST-IDDISTR                         
062700        MOVE DCS-IDVAT  TO LIST-VAT                                       
062800     END-IF                                                               
062900     MOVE 3171-3172-IDFAKT TO WS-PRT-IDFAKT                               
063000     MOVE 3171-3172-IDFAKT    TO LIST-IDFAKT                              
063100     MOVE 3171-3172-VKORDBTO-FAKT  TO LIST-GROSS-WEIGHT                   
063200     MOVE 3171-3172-IDFAKT    TO WS-IDFAKT                                
063300     ACCEPT DAGENS-DATUM FROM DATE                                        
063400     MOVE DAGENS-DATUM-AAR     TO LIST-AAR                                
063500     MOVE DAGENS-DATUM-MAANAD  TO LIST-MAN                                
063600     MOVE DAGENS-DATUM-DAG     TO LIST-DAG                                
063700     .                                                                    
063800     EJECT                                                                
063900 C-SKAPA-KOLLI-RAD SECTION.                                               
064000     MOVE 'C-SKAPA-KOLLI-RAD' TO WS-SEKTION                               
064100*    DISPLAY WS-SEKTION                                                   
064200     ADD +1                 TO WS-ANT-KOLLI                               
064300     MOVE 3171-3174-IDKOLLI TO LIST-IDKOLLI                               
064400     IF DCS-IDDC NOT = WS-IDDC                                            
064500        MOVE WS-IDDC TO W-IDDC-B6                                         
064600        PERFORM IMS-GU-WDB601                                             
064700     END-IF                                                               
064800     IF DCS-NDC-PF AND DCS-JAPAN                                          
064900        IF SPAR-3171-IDKOLLI = ZERO                                       
065000           MOVE 3171-3174-IDKOLLI TO                                      
065100                                SPAR-3171-IDKOLLI                         
065200           MOVE 3171-3174-VLORDBTO-KOLLI                                  
065300                               TO LIST-VOLYM                              
065400                                  W-3174-VLORDBTO-KOLLI                   
065500           MOVE 3171-3174-VKORDBTO-KOLLI                                  
065600                               TO LIST-VIKT                               
065700                                  W-3174-VKORDBTO-KOLLI                   
065800        ELSE                                                              
065900           IF W-3174-VKORDBTO-KOLLI > ZERO                                
066000           AND W-3174-VLORDBTO-KOLLI > ZERO                               
066100              MOVE U-RAD2 TO WS-RAD                                       
066200              MOVE U-RAD2 TO WS-DAP-LINE                                  
066300              MOVE PRT-AFTER-1  TO PRT-RADSKIP                            
066400              PERFORM S01-WRITE-LINE                                      
066500              PERFORM S20-SKRIV-DAP                                       
066600              MOVE SPACE TO WS-RAD                                        
066700              MOVE SPACE TO WS-DAP-LINE                                   
066800              MOVE +1 TO PRT-RADSKIP                                      
066900              ADD 1 TO RAD-INDX GIVING RAD-INDX                           
067000           END-IF                                                         
067100           MOVE 3171-3174-VLORDBTO-KOLLI                                  
067200                               TO LIST-VOLYM                              
067300           MOVE 3171-3174-VKORDBTO-KOLLI                                  
067400                               TO LIST-VIKT                               
067500                                  W-3174-VLORDBTO-KOLLI                   
067600           MOVE 3171-3174-IDKOLLI TO                                      
067700                                SPAR-3171-IDKOLLI                         
067800                                  W-3174-VKORDBTO-KOLLI                   
067900        END-IF                                                            
068000                                                                          
068100        IF 3171-3174-VKORDBTO-KOLLI > ZERO                                
068200        AND 3171-3174-VLORDBTO-KOLLI > ZERO                               
068300           ADD 3171-3174-VKORDBTO-KOLLI                                   
068400           TO WS-VKORDBTO-FAKT                                            
068500           ADD 3171-3174-VLORDBTO-KOLLI                                   
068600           TO WS-VLORDBTO-FAKT                                            
068700        END-IF                                                            
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 D-SKRIV-SISTA-POSTEN SECTION.                                            
069200     MOVE 'D-SKRIV-SISTA-POSTEN' TO WS-SEKTION                            
069300*    DISPLAY WS-SEKTION                                                   
069400     IF DCS-IDDC NOT = WS-IDDC                                            
069500        MOVE WS-IDDC TO W-IDDC-B6                                         
069600        PERFORM IMS-GU-WDB601                                             
069700     END-IF                                                               
069800     IF DCS-NDC-PF AND DCS-JAPAN                                          
069900        IF W-3174-VKORDBTO-KOLLI > ZERO                                   
070000        AND W-3174-VLORDBTO-KOLLI > ZERO                                  
070100            MOVE U-RAD2 TO WS-RAD                                         
070200            MOVE U-RAD2 TO WS-DAP-LINE                                    
070300            MOVE PRT-AFTER-1  TO PRT-RADSKIP                              
070400            PERFORM S01-WRITE-LINE                                        
070500            PERFORM S20-SKRIV-DAP                                         
070600            MOVE SPACE TO WS-RAD                                          
070700            MOVE SPACE TO WS-DAP-LINE                                     
070800            MOVE +1 TO PRT-RADSKIP                                        
070900            ADD 1 TO RAD-INDX GIVING RAD-INDX                             
071000        END-IF                                                            
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 E-UPDATE-ARTNR-FIELDS SECTION.                                           
071500     MOVE 'E-UPDATE-ARTNR-FIELDS' TO WS-SEKTION                           
071600*    DISPLAY WS-SEKTION                                                   
071700     MOVE 3171-3176-IDARTNR-OBJ      TO W-IDARTNR                         
071800                                   LIST-IDARTNR                           
071900     PERFORM IMS-LAES-BESKRIVNING                                         
072000     IF SEGMENT-FINNS                                                     
072100       MOVE BENA-TEXT-BEART TO LIST-BEART                                 
072200     ELSE                                                                 
072300       MOVE SPACE           TO LIST-BEART                                 
072400     END-IF                                                               
072500     PERFORM IMS-GET-ARTC-01-11                                           
072600     IF SEGMENT-FINNS                                                     
072700* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
072800       MOVE ARTC11-CLAG-KDARTURS TO WS-KDARTURS                           
072900       MOVE ARTC11-CLAG-PRARTSTD TO LIST-PRARTSTD                         
073000       COMPUTE LIST-PRARTSTD-RAD = ARTC11-CLAG-PRARTSTD *                 
073100                                   3171-3176-KVANTAL-DEB                  
073200       COMPUTE WS-SUARTSTD-TOT =                                          
073300                 WS-SUARTSTD-TOT + ARTC11-CLAG-PRARTSTD *                 
073400                                   3171-3176-KVANTAL-DEB                  
073500       MOVE 3171-3176-KVANTAL-DEB        TO LIST-KVCLEAR                  
073600       MOVE WS-KDARTURS             TO LIST-KDARTURS                      
073700       MOVE ARTC11-CLAG-VKART       TO LIST-VKART                         
073800       MOVE ARTC11-CLAG-IDSTATNR(3) TO LIST-IDSTAT                        
073900     ELSE                                                                 
074000       MOVE ZERO                    TO LIST-PRARTSTD                      
074100                                       LIST-PRARTSTD-RAD                  
074200                                       LIST-VKART                         
074300                                       LIST-IDSTAT                        
074400       MOVE SPACE                   TO LIST-KDARTURS                      
074500     END-IF                                                               
074600     MOVE U-RAD TO WS-RAD                                                 
074700     MOVE U-RAD TO WS-DAP-LINE                                            
074800     .                                                                    
074900     EJECT                                                                
075000 F-BEH-FAKT-HUVUD SECTION.                                                
075100     MOVE 'F-BEH-FAKT-HUVUD' TO WS-SEKTION                                
075200*    DISPLAY WS-SEKTION                                                   
075300     SKIP2                                                                
075400     IF RAD-INDX > 30                                                     
075500        PERFORM FA-SKRIV-FAKTURA                                          
075600        MOVE 1 TO RAD-INDX                                                
075700     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000 FA-SKRIV-FAKTURA SECTION.                                                
076100     MOVE 'FA-SKRIV-FAKTURA' TO WS-SEKTION                                
076200*    DISPLAY WS-SEKTION                                                   
076300     SKIP2                                                                
076400     ADD +1 TO  LIST-SIDA                                                 
076500                                                                          
076600     MOVE '01'       TO LIST-DC                                           
076700     MOVE RUBRIK-RAD TO WS-RAD                                            
076800     MOVE WS-IDDC    TO LIST-DC                                           
076900     MOVE RUBRIK-RAD TO WS-DAP-LINE                                       
077000     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
077100     PERFORM S01-WRITE-LINE                                               
077200     PERFORM S20-SKRIV-DAP                                                
077300                                                                          
077400     MOVE SPACE TO WS-RAD                                                 
077500     MOVE SPACE TO WS-DAP-LINE                                            
077600     MOVE U-RUBRIK-RAD1A TO WS-RAD                                        
077700     MOVE U-RUBRIK-RAD1A TO WS-DAP-LINE                                   
077800     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
077900     PERFORM S01-WRITE-LINE                                               
078000     PERFORM S20-SKRIV-DAP                                                
078100                                                                          
078200*MAASTRICHT OCH ANDRA LÄNDER                                              
078400     MOVE SPACE TO WS-RAD                                                 
078500     MOVE SPACE TO WS-DAP-LINE                                            
078600     MOVE U-RUBRIK-RAD1 TO WS-RAD                                         
078700     MOVE U-RUBRIK-RAD1 TO WS-DAP-LINE                                    
078800     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
078900     PERFORM S01-WRITE-LINE                                               
079000     PERFORM S20-SKRIV-DAP                                                
079100                                                                          
079200     MOVE SPACE TO WS-RAD                                                 
079300     MOVE SPACE TO WS-DAP-LINE                                            
079400     MOVE U-RUBRIK-RAD2 TO WS-RAD                                         
079500     MOVE U-RUBRIK-RAD2 TO WS-DAP-LINE                                    
079600     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
079700     PERFORM S01-WRITE-LINE                                               
079800     PERFORM S20-SKRIV-DAP                                                
079900                                                                          
080000     MOVE SPACE TO WS-RAD                                                 
080100     MOVE SPACE TO WS-DAP-LINE                                            
080200     MOVE U-RUBRIK-RAD3 TO WS-RAD                                         
080300     MOVE U-RUBRIK-RAD3 TO WS-DAP-LINE                                    
080400     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
080500     PERFORM S01-WRITE-LINE                                               
080600     PERFORM S20-SKRIV-DAP                                                
080700                                                                          
080800     MOVE SPACE TO WS-RAD                                                 
080900     MOVE SPACE TO WS-DAP-LINE                                            
081000     MOVE U-RUBRIK-RAD4 TO WS-RAD                                         
081100     MOVE U-RUBRIK-RAD4 TO WS-DAP-LINE                                    
081200     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
081300     PERFORM S01-WRITE-LINE                                               
081400     PERFORM S20-SKRIV-DAP                                                
081500                                                                          
081600     MOVE SPACE TO WS-RAD                                                 
081700     MOVE SPACE TO WS-DAP-LINE                                            
081800*    MOVE SISTA-RADEN TO WS-RAD                                           
081900*    MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
082000*    PERFORM S01-WRITE-LINE                                               
082100                                                                          
082200     IF DCS-IDDC NOT = WS-IDDC                                            
082300        MOVE WS-IDDC TO W-IDDC-B6                                         
082400        PERFORM IMS-GU-WDB601                                             
082500     END-IF                                                               
082600     IF DCS-NDC-PF AND DCS-JAPAN                                          
082700        MOVE 'VOLUME'     TO WS-VOLUME-RUB                                
082800        MOVE 'GROSS WGT'  TO WS-VIKT-RUB                                  
082900     ELSE                                                                 
083000        MOVE SPACE        TO WS-VOLUME-RUB                                
083100                             WS-VIKT-RUB                                  
083200     END-IF                                                               
083300                                                                          
083400     MOVE U-RUBRIK-RAD6 TO WS-RAD                                         
083500     MOVE U-RUBRIK-RAD6 TO WS-DAP-LINE                                    
083600     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
083700     PERFORM S01-WRITE-LINE                                               
083800     PERFORM S20-SKRIV-DAP                                                
083900     MOVE SPACE TO WS-RAD                                                 
084000     MOVE SPACE TO WS-DAP-LINE                                            
084100     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
084200     .                                                                    
084300     EJECT                                                                
084400 G-BEH-FAKT-LINE SECTION.                                                 
084500      MOVE 'G-BEH-FAKT-LINE' TO WS-SEKTION                                
084600*    DISPLAY WS-SEKTION                                                   
084700     SKIP2                                                                
084800     PERFORM GA-SKRIV-RAD-FAKTURA                                         
084900     PERFORM S15B-EKONOMITRANS-WDR901                                     
085000     PERFORM GD-SKRIV-RAD-FIL-TILL-TULL                                   
085100     .                                                                    
085200     EJECT                                                                
085300 GA-SKRIV-RAD-FAKTURA  SECTION.                                           
085400      MOVE 'GA-SKRIV-RAD-FAKTURA' TO WS-SEKTION                           
085500*    DISPLAY WS-SEKTION                                                   
085600     SKIP2                                                                
085700     PERFORM S01-WRITE-LINE                                               
085800     PERFORM S20-SKRIV-DAP                                                
085900     MOVE SPACE TO WS-RAD                                                 
086000     MOVE SPACE TO WS-DAP-LINE                                            
086100     MOVE +1 TO PRT-RADSKIP                                               
086200     ADD 1 TO RAD-INDX GIVING RAD-INDX                                    
086300     .                                                                    
086400     EJECT                                                                
086500 GD-SKRIV-RAD-FIL-TILL-TULL SECTION.                                      
086600     MOVE 'GD-SKRIV-RAD-FIL-TILL-TUL1' TO WS-SEKTION                      
086700*    DISPLAY WS-SEKTION                                                   
086800     SKIP2                                                                
086900     MOVE '001'                   TO UT57RZQ-IDPTYP                       
087000     MOVE WS-IDFAKT               TO UT57RZQ-IDFAKT                       
087100     MOVE DAGENS-DATUM            TO UT57RZQ-TIFAKT                       
087200     MOVE 'SEK'                   TO UT57RZQ-KDVALUTA                     
087300     PERFORM S13B-SKRIV-W37157-RZQ                                        
087400*-----------------                                                        
087500     MOVE '002'                   TO UT57RZR-IDPTYP                       
087600     MOVE WS-IDFAKT               TO UT57RZR-IDFAKT                       
087700     MOVE 3171-3176-IDARTNR-OBJ        TO UT57RZR-IDARTNR                 
087800                                                                          
087900     MOVE 3171-3176-KVANTAL-DEB        TO UT57RZR-KVANTAL-002             
088000     MOVE ARTC11-CLAG-KDARTURS    TO UT57RZR-KDARTURS                     
088100     MOVE ARTC11-CLAG-KDSRA       TO UT57RZR-KDSRA-002                    
088200     COMPUTE UT57RZR-SUARTSJK =                                           
088300             ARTC11-CLAG-PRARTSJK * 3171-3176-KVANTAL-DEB                 
088400     MOVE ARTC11-CLAG-VKART   TO UT57RZR-VKART                            
088500     MOVE BENA-TEXT-BEART   TO UT57RZR-BEART-SVE                          
088600     MOVE 'N'   TO UT57RZR-FLSATS                                         
088700     PERFORM S13A-SKRIV-W37157-RZR                                        
088800     .                                                                    
088900     EJECT                                                                
089000 H-SISTA-RADEN SECTION.                                                   
089100     MOVE 'H-SISTA-RADEN' TO WS-SEKTION                                   
089200*    DISPLAY WS-SEKTION                                                   
089300     SKIP2                                                                
089400                                                                          
089500     IF DCS-IDDC NOT = WS-IDDC                                            
089600        MOVE WS-IDDC TO W-IDDC-B6                                         
089700        PERFORM IMS-GU-WDB601                                             
089800     END-IF                                                               
089900     IF DCS-NDC-PF AND DCS-JAPAN                                          
090000        MOVE 'TOTAL VOLUME  '                                             
090100                           TO WS-TOT-VOLUME-RUB                           
090200        MOVE 'NO OF CASES   '                                             
090300                          TO WS-TOT-CASE-RUB                              
090400****** BERÄKNA TOTALA VIKTEN PER KOLLI FÖR JAPAN **                       
090500******************************************************************        
090600*** REDIGERA VOLYMEN BEROEND PÅ HUR MÅNGA DECIMALER           ****        
090700******************************************************************        
090800        MOVE WS-VKORDBTO-FAKT   TO LIST-GROSS-WEIGHT                      
090900        MOVE WS-VLORDBTO-FAKT   TO WS-RED-VOLYM                           
091000        MOVE WS-ANT-KOLLI       TO WS-RED-ANT-KOLLI                       
091100        MOVE WS-RED-ANT-KOLLI   TO WS-TOT-KOLLI                           
091200        MOVE WS-RED-VOLYM       TO WS-TOT-VOLUME                          
091300     ELSE                                                                 
091400        MOVE SPACE                                                        
091500                           TO WS-TOT-VOLUME-RUB                           
091600        MOVE SPACE                                                        
091700                           TO WS-TOT-CASE-RUB                             
091800        MOVE W-3172-VKORDBTO-FAKT TO LIST-GROSS-WEIGHT                    
091900     END-IF                                                               
092000     MOVE WS-SUARTSTD-TOT TO LIST-SUARTSTD-TOT                            
092100     PERFORM S15A-EKONOMITRANS-WDR901                                     
092200     MOVE SISTA-RADEN TO WS-RAD                                           
092300     MOVE SISTA-RADEN TO WS-DAP-LINE                                      
092400     MOVE PRT-AFTER-3  TO PRT-RADSKIP                                     
092500     PERFORM S01-WRITE-LINE                                               
092600     PERFORM S20-SKRIV-DAP                                                
092700     .                                                                    
092800     EJECT                                                                
092900 Z-FINIT SECTION.                                                         
093000     MOVE 'Z-FINIT' TO WS-SEKTION                                         
093100*    DISPLAY WS-SEKTION                                                   
093200     SKIP2                                                                
093300     CLOSE W37157                                                         
093400           W37160                                                         
093500           DAPLISTA21                                                     
093600           DAPLISTA61                                                     
093700           DAPLISTA62                                                     
093800           DOCRLISTA62                                                    
093900     SKIP2                                                                
094000     MOVE 'S' TO POSTSUM-OPKOD                                            
094100     CALL POSTSUM USING POSTSUM-PARM                                      
094200     SKIP2                                                                
094300*MAASTRICHT                                                               
094500     MOVE W-3172-IDDC-SEND  TO WS-IDDC                                    
094600     IF DCS-IDDC NOT = WS-IDDC                                            
094700        MOVE WS-IDDC TO W-IDDC-B6                                         
094800        PERFORM IMS-GU-WDB601                                             
094900     END-IF                                                               
095000     IF DCS-SDC AND DCS-HOLLAND                                           
095100        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
095200                         PRT-CLOSE                                        
095300                         WS-PRT-IDPRTLST2                                 
095400                         ALT2-PCB                                         
095500                         LISB-PCB                                         
095600                         WS-PRT-IDLIST                                    
095700                         WS-PRT-DUMMY                                     
095800                         WS-PRT-DUMMY                                     
095900     END-IF                                                               
096000     IF DCS-NDC-PF AND DCS-JAPAN                                          
096100        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
096200                         PRT-CLOSE                                        
096300                         WS-PRT-IDPRTLST3                                 
096400                         ALT2-PCB                                         
096500                         LISB-PCB                                         
096600                         WS-PRT-IDLIST                                    
096700                         WS-PRT-DUMMY                                     
096800                         WS-PRT-DUMMY                                     
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200 S01-WRITE-LINE   SECTION.                                                
097300     MOVE 'S01-WRITE-LINE' TO WS-FIL-SEKTION                              
097400     SKIP2                                                                
097500*MAASTRICHT                                                               
097700     MOVE W-3172-IDDC-SEND   TO WS-IDDC                                   
097800     IF DCS-IDDC NOT = WS-IDDC                                            
097900        MOVE WS-IDDC TO W-IDDC-B6                                         
098000        PERFORM IMS-GU-WDB601                                             
098100     END-IF                                                               
098200     IF DCS-SDC AND DCS-HOLLAND                                           
098300        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
098400                         PRT-WRITE                                        
098500                         WS-PRT-IDPRTLST2                                 
098600                         ALT2-PCB                                         
098700                         LISB-PCB                                         
098800                         WS-PRT-IDLIST                                    
098900                         PRT-RADSKIP                                      
099000                         WS-PRT-LISTRAD                                   
099100                         WS-PRT-DUMMY                                     
099200                         WS-PRT-DUMMY                                     
099300     END-IF                                                               
099400     IF DCS-NDC-PF AND DCS-JAPAN                                          
099500        CALL W006PRR2 USING PRT-SPOOL-OVR                                 
099600                      PRT-WRITE                                           
099700                      WS-PRT-IDPRTLST3                                    
099800                      ALT2-PCB                                            
099900                      LISB-PCB                                            
100000                      WS-PRT-IDLIST                                       
100100                      PRT-RADSKIP                                         
100200                      WS-PRT-LISTRAD                                      
100300                      WS-PRT-DUMMY                                        
100400                      WS-PRT-DUMMY                                        
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 S13A-SKRIV-W37157-RZR SECTION.                                           
100900     MOVE 'S13A-SKRIV-W37157-RZR' TO WS-FIL-SEKTION                       
101000*    DISPLAY WS-FIL-SEKTION                                               
101100     SKIP2                                                                
101200     WRITE UT57RZR-POST FROM UT57-AREA                                    
101300                                                                          
101400     MOVE UT57RZR-IDPTYP TO POSTSUM-TRANSTYP                              
101500     MOVE 'W37157R' TO POSTSUM-FDNAMN                                     
101600     MOVE 'W37156D3' TO POSTSUM-DDNAMN2                                   
101700     CALL POSTSUM USING POSTSUM-PARM                                      
101800     .                                                                    
101900     EJECT                                                                
102000 S13B-SKRIV-W37157-RZQ SECTION.                                           
102100     MOVE 'S13B-SKRIV-W37157-RZQ' TO WS-FIL-SEKTION                       
102200*    DISPLAY WS-FIL-SEKTION                                               
102300     SKIP2                                                                
102400     WRITE UT57RZQ-POST FROM UT57-AREA                                    
102500                                                                          
102600     MOVE UT57RZQ-IDPTYP TO POSTSUM-TRANSTYP                              
102700     MOVE 'W37157Q' TO POSTSUM-FDNAMN                                     
102800     MOVE 'W37156D3' TO POSTSUM-DDNAMN2                                   
102900     CALL POSTSUM USING POSTSUM-PARM                                      
103000     .                                                                    
103100     EJECT                                                                
103200 S15A-EKONOMITRANS-WDR901 SECTION.                                        
103300     MOVE 'S15A-EKONOMITRANS-WDR901' TO WS-FIL-SEKTION                    
103400*    DISPLAY WS-FIL-SEKTION                                               
103500                                                                          
103600     MOVE '202'                  TO UT51-EKHT-KDEKHHT                     
103700     MOVE '202'                  TO UT51-EKHT-KDEKSHT                     
103800     MOVE 'SUM'                  TO UT51-EKHT-KDEKNIVA                    
103900     MOVE WS-AAAAMMDD            TO UT51-EKHT-DAREGDAT                    
104000     MOVE WS-TTMMSSTH            TO UT51-EKHT-TIKLOCK                     
104100     MOVE 0                      TO UT51-EKHT-IDSEKVNR                    
104200     MOVE W-3172-IDDC-SEND                                                
104300                                 TO UT51-EKHT-IDDC-SEND                   
104400                                    WS-IDDC                               
104500     IF DCS-IDDC NOT = WS-IDDC                                            
104600        MOVE WS-IDDC TO W-IDDC-B6                                         
104700        PERFORM IMS-GU-WDB601                                             
104800     END-IF                                                               
104900*LN  IF CDC-SE                                                            
105000*       MOVE WC-SDC-NL-ET        TO UT51-EKHT-IDDC-REC                    
105100*       MOVE  8014               TO UT51-EKHT-IDDISTR                     
105200*    ELSE                                                                 
105300        IF DCS-SDC AND DCS-HOLLAND                                        
105400*          MOVE WC-CDC-SE        TO UT51-EKHT-IDDC-REC                    
105500           MOVE WS-SDC-91        TO UT51-EKHT-IDDC-REC                    
105600           MOVE  9927            TO UT51-EKHT-IDDISTR                     
105700        ELSE                                                              
105800           IF DCS-NDC-PF AND DCS-JAPAN                                    
105900*             MOVE WC-CDC-SE     TO UT51-EKHT-IDDC-REC                    
106000              MOVE WS-SDC-91     TO UT51-EKHT-IDDC-REC                    
106100              MOVE  9927         TO UT51-EKHT-IDDISTR                     
106200           ELSE                                                           
106300              IF DCS-NDC-PF AND DCS-AUSTRALIA                             
106400*                MOVE WC-CDC-SE  TO UT51-EKHT-IDDC-REC                    
106500                 MOVE WS-SDC-91  TO UT51-EKHT-IDDC-REC                    
106600                MOVE  9927       TO UT51-EKHT-IDDISTR                     
106700              END-IF                                                      
106800           END-IF                                                         
106900        END-IF                                                            
107000*    END-IF                                                               
107100     MOVE +0                     TO UT51-EKHT-IDKUNDNR                    
107200                                                                          
107300     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
107400     MOVE W-3172-IDFAKT        TO CIA-IDARTBET-IN                         
107500     CALL W009CIA USING           CIA-W009CIA                             
107600     MOVE CIA-IDARTBET-UT      TO UT51-EKHT-IDVERGL                       
107700                                                                          
107800     MOVE    W-3172-DASNDDAT   TO WS-TIFAKT                               
107900     MOVE WS-TIFAKT            TO TIFAKT-REST                             
108000                                                                          
108100     IF TIFAKT-AAR  > 50                                                  
108200        MOVE 19  TO TIFAKT-SEKEL                                          
108300     ELSE                                                                 
108400        MOVE 20  TO TIFAKT-SEKEL                                          
108500     END-IF                                                               
108600                                                                          
108700     MOVE WS-RED-TIFAKT          TO UT51-EKHT-DAVERDAT                    
108800     MOVE ZERO                   TO UT51-EKHT-KDPRODSL                    
108900     MOVE ZERO                   TO UT51-EKHT-KDPSLLOC                    
109000     MOVE ZERO                   TO UT51-EKHT-IDARTNR                     
109100     MOVE ' '                    TO UT51-EKHT-FLLSBOK                     
109200     MOVE 'SEK'                  TO UT51-EKHT-KDVALISO                    
109300     MOVE  1.00                  TO UT51-EKHT-PRKURS                      
109400     MOVE ZERO                   TO UT51-EKHT-PRARTNTO                    
109500     MOVE ZERO                   TO UT51-EKHT-PRARTSJK                    
109600     MOVE ZERO                   TO UT51-EKHT-PRHEMTAG                    
109700     MOVE ZERO                   TO UT51-EKHT-PRARTSTD                    
109800     MOVE ZERO                   TO UT51-EKHT-PRLANDCO                    
109900     MOVE ZERO                   TO UT51-EKHT-PRINK                       
110000     MOVE ZERO                   TO UT51-EKHT-PRDIRLON                    
110100     MOVE ZERO                   TO UT51-EKHT-PRDMTRL                     
110200     MOVE ZERO                   TO UT51-EKHT-PROVRPAL                    
110300     MOVE ZERO                   TO UT51-EKHT-KVANTAL                     
110400     MOVE WS-SUARTSTD-TOT        TO UT51-EKHT-SUBEL                       
110500     MOVE 'W3715600'             TO UT51-EKHT-IDPGM                       
110600     MOVE '    '                 TO UT51-EKHT-IDTRANS                     
110700     MOVE 'W510EKHA'             TO UT51-EKHT-IDCPYTXT                    
110800     MOVE ZERO                   TO UT51-EKHT-BEVAT                       
110900                                    UT51-EKHT-IDANALYS                    
111000                                    UT51-EKHT-IDKONTO                     
111200                                    UT51-EKHT-KDANMORS                    
111300                                    UT51-EKHT-KDFRAKT                     
111400                                    UT51-EKHT-SUVAT                       
111500     MOVE ZERO                   TO UT51-EKHT-DAAVIDAT                    
111600                                    UT51-EKHT-IDAVINR                     
111700                                    UT51-EKHT-KDAVVTYP                    
111800                                    UT51-EKHT-KDRT                        
111900                                    UT51-EKHT-KVANTMOT                    
112000                                    UT51-EKHT-KVAVIS                      
112100     MOVE SPACE                  TO UT51-EKHT-KDSORT                      
111100                                    UT51-EKHT-IDKST                       
112200                                    UT51-EKHT-KDTRADP                     
112300                                    UT51-EKHT-IDLEVNR                     
112400                                    UT51-EKHT-FLDCET                      
112410                                    UT51-EKHT-IDKUNDRF                    
112410                                    UT51-EKHT-IDFAKT-EXP                  
112500********************************************************                  
112600     PERFORM S16-SKRIV-W5160                                              
112700                                                                          
112800     .                                                                    
112900     EJECT                                                                
113000 S15B-EKONOMITRANS-WDR901 SECTION.                                        
113100     MOVE 'S15B-EKONOMITRANS-WDR901' TO WS-FIL-SEKTION                    
113200*    DISPLAY WS-FIL-SEKTION                                               
113300                                                                          
113400     ADD +1                      TO WS-TTMMSSTH                           
113500     MOVE '202'                  TO UT51-EKHT-KDEKHHT                     
113600     MOVE '202'                  TO UT51-EKHT-KDEKSHT                     
113700     MOVE 'DET'                  TO UT51-EKHT-KDEKNIVA                    
113800     MOVE WS-AAAAMMDD            TO UT51-EKHT-DAREGDAT                    
113900     MOVE WS-TTMMSSTH            TO UT51-EKHT-TIKLOCK                     
114000     MOVE 0                      TO UT51-EKHT-IDSEKVNR                    
114100     MOVE W-3172-IDDC-SEND       TO UT51-EKHT-IDDC-SEND                   
114200                                    WS-IDDC                               
114300     IF DCS-IDDC NOT = WS-IDDC                                            
114400        MOVE WS-IDDC TO W-IDDC-B6                                         
114500        PERFORM IMS-GU-WDB601                                             
114600     END-IF                                                               
114700*LN  IF CDC-SE                                                            
114800*       MOVE WC-SDC-NL-ET        TO UT51-EKHT-IDDC-REC                    
114900*       MOVE  8014               TO UT51-EKHT-IDDISTR                     
115000*    ELSE                                                                 
115100        IF DCS-SDC AND DCS-HOLLAND                                        
115200*          MOVE WC-CDC-SE        TO UT51-EKHT-IDDC-REC                    
115300           MOVE WS-SDC-91        TO UT51-EKHT-IDDC-REC                    
115400           MOVE  9927            TO UT51-EKHT-IDDISTR                     
115500        ELSE                                                              
115600           IF DCS-NDC-PF AND DCS-JAPAN                                    
115700*             MOVE WC-CDC-SE     TO UT51-EKHT-IDDC-REC                    
115800              MOVE WS-SDC-91     TO UT51-EKHT-IDDC-REC                    
115900              MOVE  9927         TO UT51-EKHT-IDDISTR                     
116000           ELSE                                                           
116100              IF DCS-NDC-PF AND DCS-AUSTRALIA                             
116200*                MOVE WC-CDC-SE  TO UT51-EKHT-IDDC-REC                    
116300                 MOVE WS-SDC-91  TO UT51-EKHT-IDDC-REC                    
116400                 MOVE  9927      TO UT51-EKHT-IDDISTR                     
116500              END-IF                                                      
116600           END-IF                                                         
116700        END-IF                                                            
116800*    END-IF                                                               
116900     MOVE +0                     TO UT51-EKHT-IDKUNDNR                    
117000                                                                          
117100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
117200     MOVE W-3172-IDFAKT     TO CIA-IDARTBET-IN                            
117300     CALL W009CIA USING           CIA-W009CIA                             
117400     MOVE CIA-IDARTBET-UT      TO UT51-EKHT-IDVERGL                       
117500                                                                          
117600     MOVE    W-3172-DASNDDAT   TO WS-TIFAKT                               
117700     MOVE WS-TIFAKT            TO TIFAKT-REST                             
117800                                                                          
117900     IF TIFAKT-AAR  > 50                                                  
118000        MOVE 19  TO TIFAKT-SEKEL                                          
118100     ELSE                                                                 
118200        MOVE 20  TO TIFAKT-SEKEL                                          
118300     END-IF                                                               
118400                                                                          
118500     MOVE WS-RED-TIFAKT          TO UT51-EKHT-DAVERDAT                    
118600     MOVE ZERO                   TO UT51-EKHT-KDPRODSL                    
118700     MOVE ZERO                   TO UT51-EKHT-KDPSLLOC                    
118800     MOVE 3171-3176-IDARTNR-OBJ  TO UT51-EKHT-IDARTNR                     
118900     MOVE ' '                    TO UT51-EKHT-FLLSBOK                     
119000     MOVE 'SEK'                  TO UT51-EKHT-KDVALISO                    
119100     MOVE  1.00                  TO UT51-EKHT-PRKURS                      
119200     MOVE ZERO                   TO UT51-EKHT-PRARTNTO                    
119300     MOVE ZERO                   TO UT51-EKHT-PRARTSJK                    
119400     MOVE ZERO                   TO UT51-EKHT-PRHEMTAG                    
119500     MOVE ARTC11-CLAG-PRARTSTD   TO UT51-EKHT-PRARTSTD                    
119600     MOVE ZERO                   TO UT51-EKHT-PRLANDCO                    
119700     MOVE ZERO                   TO UT51-EKHT-PRINK                       
119800     MOVE ZERO                   TO UT51-EKHT-PRDIRLON                    
119900     MOVE ZERO                   TO UT51-EKHT-PRDMTRL                     
120000     MOVE ZERO                   TO UT51-EKHT-PROVRPAL                    
120100     MOVE 3171-3176-KVANTAL-DEB  TO UT51-EKHT-KVANTAL                     
120200     MOVE ZERO                   TO UT51-EKHT-SUBEL                       
120300     MOVE 'W3715600'             TO UT51-EKHT-IDPGM                       
120400     MOVE '    '                 TO UT51-EKHT-IDTRANS                     
120500     MOVE 'W510EKHA'             TO UT51-EKHT-IDCPYTXT                    
120600     MOVE ZERO                   TO UT51-EKHT-BEVAT                       
120700                                    UT51-EKHT-IDANALYS                    
120800                                    UT51-EKHT-IDKONTO                     
121000                                    UT51-EKHT-KDANMORS                    
121100                                    UT51-EKHT-KDFRAKT                     
121200                                    UT51-EKHT-SUVAT                       
121300     MOVE ZERO                   TO UT51-EKHT-DAAVIDAT                    
121400                                    UT51-EKHT-IDAVINR                     
121500                                    UT51-EKHT-KDAVVTYP                    
121600                                    UT51-EKHT-KDRT                        
121700                                    UT51-EKHT-KVANTMOT                    
121800                                    UT51-EKHT-KVAVIS                      
121900     MOVE SPACE                  TO UT51-EKHT-KDSORT                      
120900                                    UT51-EKHT-IDKST                       
122000                                    UT51-EKHT-KDTRADP                     
122100                                    UT51-EKHT-IDLEVNR                     
122110                                    UT51-EKHT-FLDCET                      
122120                                    UT51-EKHT-IDKUNDRF                    
122120                                    UT51-EKHT-IDFAKT-EXP                  
122200********************************************************                  
122300     PERFORM S16-SKRIV-W5160                                              
122400     .                                                                    
122500     EJECT                                                                
122600 S16-SKRIV-W5160 SECTION.                                                 
122700     MOVE 'S16-SKRIV-W5160' TO WS-FIL-SEKTION                             
122800*    DISPLAY WS-FIL-SEKTION                                               
122900     SKIP2                                                                
123000     WRITE UT51-POST FROM UT51-AREA                                       
123100                                                                          
123200     MOVE 'UT51'   TO POSTSUM-TRANSTYP                                    
123300     MOVE 'W37160' TO POSTSUM-FDNAMN                                      
123400     MOVE 'W37156D5' TO POSTSUM-DDNAMN2                                   
123500     CALL POSTSUM USING POSTSUM-PARM                                      
123600     .                                                                    
123700     EJECT                                                                
123800 S20-SKRIV-DAP   SECTION.                                                 
123900     MOVE 'S20-SKRIV-DAP  ' TO WS-FIL-SEKTION                             
124000*    DISPLAY WS-FIL-SEKTION                                               
124100     SKIP2                                                                
124200     IF DCS-SDC AND DCS-HOLLAND                                           
124300       WRITE DAPPOST21 FROM WS-DAP-LINE                                   
124400                                                                          
124500       MOVE 'DC21'   TO POSTSUM-TRANSTYP                                  
124600       MOVE 'W37156' TO POSTSUM-FDNAMN                                    
124700       MOVE 'W37156D7' TO POSTSUM-DDNAMN2                                 
124800       CALL POSTSUM USING POSTSUM-PARM                                    
124900     END-IF                                                               
125000     IF DCS-NDC-PF AND DCS-JAPAN                                          
125100       WRITE DAPPOST61 FROM WS-DAP-LINE                                   
125200                                                                          
125300       MOVE 'DC61'   TO POSTSUM-TRANSTYP                                  
125400       MOVE 'W37156' TO POSTSUM-FDNAMN                                    
125500       MOVE 'W37156D8' TO POSTSUM-DDNAMN2                                 
125600       CALL POSTSUM USING POSTSUM-PARM                                    
125700     END-IF                                                               
125800     IF DCS-NDC-PF AND DCS-AUSTRALIA                                      
125900       WRITE DAPPOST62 FROM WS-DAP-LINE                                   
126000                                                                          
126100       MOVE 'DC62'   TO POSTSUM-TRANSTYP                                  
126200       MOVE 'W37156' TO POSTSUM-FDNAMN                                    
126300       MOVE 'W37156D9' TO POSTSUM-DDNAMN2                                 
126400       CALL POSTSUM USING POSTSUM-PARM                                    
126500     END-IF                                                               
126600*                                                                         
126600***  START TEMP FIX                                                       
126600***  TAS BORT NÄR DC21 SKALL HA FAKTURAN SOM WEB DOKUMENT.                
126700***  IF DCS-FLWEBDC = 'J'                                                 
126600***  SLUT TEMP FIX                                                        
126700     IF DCS-NDC-PF                                                        
126800       WRITE DOCRPOST62 FROM WS-DAP-LINE                                  
126900                                                                          
127000       MOVE 'DC6X'   TO POSTSUM-TRANSTYP                                  
127100       MOVE 'W37156' TO POSTSUM-FDNAMN                                    
127200       MOVE 'W37156DA' TO POSTSUM-DDNAMN2                                 
127300       CALL POSTSUM USING POSTSUM-PARM                                    
127400     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
127700                                                                          
127800* --- IMS SEKTIONER ---                                                   
127900     SKIP3                                                                
128000     SKIP2                                                                
128100 IMS-GET-ARTC-01-11 SECTION.                                              
128200     MOVE 'IMS-GET-ARTC-01-11'  TO WS-IMS-SEKTION                         
128300*    DISPLAY WS-IMS-SEKTION                                               
128400                                                                          
128500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
128600          DELIMITED BY SIZE INTO SSA1                                     
128700     MOVE 'WLARTC11 ' TO SSA2                                             
128800     MOVE '  GE' TO GODK-STATUSKODER                                      
128900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
129000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
129100     PERFORM IMS-STATUSKONTROLL                                           
129200     .                                                                    
129300     EJECT                                                                
129400 IMS-REPL-INVOICE   SECTION.                                              
129500     MOVE 'IMS-REPL-INVOICE  '  TO WS-IMS-SEKTION                         
129600*    DISPLAY WS-IMS-SEKTION                                               
129700                                                                          
129800     MOVE '  ' TO GODK-STATUSKODER                                        
129900     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-AREA1                        
130000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300     SKIP3                                                                
130400 IMS-GHU-INVOICE SECTION.                                                 
130500     MOVE 'IMS-GHU-INVOICE  '  TO WS-IMS-SEKTION                          
130600*    DISPLAY WS-IMS-SEKTION                                               
130700                                                                          
130800     STRING 'WL317101(WDGXKEY  =' W-WDGXKEY-X ')'                         
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
131100          DELIMITED BY SIZE INTO SSA2                                     
131200     MOVE '    ' TO GODK-STATUSKODER                                      
131300     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-AREA1 SSA1 SSA2               
131400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     SKIP3                                                                
131800                                                                          
131900 IMS-GU-INVOICE SECTION.                                                  
132000     MOVE 'IMS-GU-INVOICE  '  TO WS-IMS-SEKTION                           
132100*    DISPLAY WS-IMS-SEKTION                                               
132200                                                                          
132300     STRING 'WL317101(WDGXKEY  =' W-WDGXKEY-X ')'                         
132400          DELIMITED BY SIZE INTO SSA1                                     
132500     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
132600          DELIMITED BY SIZE INTO SSA2                                     
132700     MOVE '    ' TO GODK-STATUSKODER                                      
132800     CALL CBLTDLI USING GU 3171-PCB DLI-IO-AREA1 SSA1 SSA2                
132900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
133000     PERFORM IMS-STATUSKONTROLL                                           
133100     .                                                                    
133200     SKIP3                                                                
133300                                                                          
133400 IMS-GNP-KOLLI-RADER SECTION.                                             
133500     MOVE 'IMS-GNP-KOLLI-RADER' TO WS-IMS-SEKTION                         
133600*    DISPLAY WS-IMS-SEKTION                                               
133700                                                                          
133800     MOVE '  GEGAGB' TO GODK-STATUSKODER                                  
133900     CALL CBLTDLI USING GN 3171-PCB DLI-IO-AREA1                          
134000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
134100     PERFORM IMS-STATUSKONTROLL                                           
134200     .                                                                    
134300     EJECT                                                                
134400                                                                          
134500 IMS-LAES-BESKRIVNING  SECTION.                                           
134600     MOVE 'IMS-LAES-BESKRIVN'  TO WS-IMS-SEKTION                          
134700*    DISPLAY WS-IMS-SEKTION                                               
134800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
134900          DELIMITED BY SIZE INTO SSA1                                     
135000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
135100          DELIMITED BY SIZE INTO SSA2                                     
135200     MOVE '  GE' TO GODK-STATUSKODER                                      
135300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
135400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
135500     PERFORM IMS-STATUSKONTROLL                                           
135600     .                                                                    
135700     SKIP2                                                                
135800 IMS-GU-WDB601    SECTION.                                                
135900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     MOVE '  GE' TO GODK-STATUSKODER                                      
136200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
136300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     IF SEGMENT-SAKNAS                                                    
136600         MOVE SPACE TO DCS-KDDC                                           
136700     END-IF                                                               
136800     .                                                                    
136900     SKIP2                                                                
137000 IMS-STATUSKONTROLL SECTION.                                              
137100     SKIP2                                                                
137200     SET STATUS-IX TO 1                                                   
137300     SEARCH GODK-STATUS                                                   
137400       AT END                                                             
137500         MOVE 'WS-IMS-SEKTION' TO FELTEXT-STR                             
137600         DISPLAY FELTEXT                                                  
137700         CALL FELLOG                                                      
137800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137900         CONTINUE                                                         
138000     END-SEARCH                                                           
138100     .                                                                    
