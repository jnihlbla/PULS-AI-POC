000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3725700.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   97/09/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BERÄKNAR ANTAL ARBETSDAGAR I SNITT DET TAR ATT        
001100* GODKÄNNA EN RAPPORT UNDER EN PERIOD                                     
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- UPPFÖLJNINGS MATERIAL TILL DE OLIKA PERIODLISTORNA         
002900     SELECT W3724E                     ASSIGN TO W37257D1.                
003000     SKIP2                                                                
003100*          --- PERIOD LISTA DC11                                          
003200     SELECT LISTA                      ASSIGN TO W37257D2.                
003300     SKIP2                                                                
003400*          --- PERIOD LISTA DC91                                          
003500     SELECT LISTB                      ASSIGN TO W37257D3.                
003600*          --- PERIOD LISTA DC61                                          
003700     SELECT LISTG                      ASSIGN TO W37257D4.                
003800*          --- PERIOD LISTA DC62                                          
003900     SELECT LISTH                      ASSIGN TO W37257D5.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W3724E                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W3714E      -L.                                                
005000     SKIP3                                                                
005100 FD  LISTA                                                                
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400     SKIP2                                                                
005500 01  LISTAS                      PIC X(121).                              
005600     SKIP3                                                                
005700 FD  LISTB                                                                
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100 01  LISTBS                      PIC X(121).                              
006200     EJECT                                                                
006300 FD  LISTG                                                                
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600     SKIP2                                                                
006700 01  LISTGS                      PIC X(121).                              
006800     EJECT                                                                
006900 FD  LISTH                                                                
007000     RECORDING       F                                                    
007100     BLOCK CONTAINS  0.                                                   
007200     SKIP2                                                                
007300 01  LISTHS                      PIC X(121).                              
007400     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600                                                                          
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900 77  IDPGM                       PIC X(8)    VALUE 'W3725700'.            
008000 77   PROGRAM-NAMN           VALUE 'W3725700'                             
008100                                 PIC X(8).                                
008200 77  JA                          PIC X       VALUE 'N'.                   
008300 77  NEJ                         PIC X       VALUE 'J'.                   
008400                                                                          
008500 77  W3724E-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W3724E                       VALUE 'J'.                   
008700                                                                          
008800 01  SISTA-POST-SKRIVEN-DC11     PIC X       VALUE 'N'.                   
008900                                                                          
009000 01  SISTA-POST-SKRIVEN-DC91     PIC X       VALUE 'N'.                   
009100                                                                          
009200 01  SISTA-POST-SKRIVEN-DC61     PIC X       VALUE 'N'.                   
009300                                                                          
009400 01  SISTA-POST-SKRIVEN-DC62     PIC X       VALUE 'N'.                   
009500                                                                          
009600 77  FORSTA-POST-DC11-SW         PIC X       VALUE 'J'.                   
009700     88  FORSTA-POSTEN-DC11                  VALUE 'J'.                   
009800     88  ANDRA-POSTEN-DC11                  VALUE 'N'.                    
009900                                                                          
010000 77  FORSTA-POST-DC91-SW         PIC X       VALUE 'J'.                   
010100     88  FORSTA-POSTEN-DC91                  VALUE 'J'.                   
010200     88  ANDRA-POSTEN-DC91                  VALUE 'N'.                    
010300                                                                          
010400 77  FORSTA-POST-DC61-SW         PIC X       VALUE 'J'.                   
010500     88  FORSTA-POSTEN-DC61                  VALUE 'J'.                   
010600     88  ANDRA-POSTEN-DC61                  VALUE 'N'.                    
010700                                                                          
010800 77  FORSTA-POST-DC62-SW         PIC X       VALUE 'J'.                   
010900     88  FORSTA-POSTEN-DC62                  VALUE 'J'.                   
011000     88  ANDRA-POSTEN-DC62                  VALUE 'N'.                    
011100                                                                          
011200 77  SPAR-IDDISTR-DC11           PIC S9(5) VALUE +0 COMP-3.               
011300 77  SPAR-IDDISTR-DC91           PIC S9(5) VALUE +0 COMP-3.               
011400 77  SPAR-IDDISTR-DC61           PIC S9(5) VALUE +0 COMP-3.               
011500 77  SPAR-IDDISTR-DC62           PIC S9(5) VALUE +0 COMP-3.               
011600                                                                          
011700 77  SPAR-IDKUNDNR-DC11          PIC S9(7) VALUE +0 COMP-3.               
011800 77  SPAR-IDKUNDNR-DC91          PIC S9(7) VALUE +0 COMP-3.               
011900 77  SPAR-IDKUNDNR-DC61          PIC S9(7) VALUE +0 COMP-3.               
012000 77  SPAR-IDKUNDNR-DC62          PIC S9(7) VALUE +0 COMP-3.               
012100                                                                          
012200 77  W-ANTALPOSTER-TOT-DC11      PIC S9(7)   VALUE +0 COMP-3.             
012300 77  W-ANTALPOSTER-TOT-DC91      PIC S9(7)   VALUE +0 COMP-3.             
012400 77  W-ANTALPOSTER-TOT-DC61      PIC S9(7)   VALUE +0 COMP-3.             
012500 77  W-ANTALPOSTER-TOT-DC62      PIC S9(7)   VALUE +0 COMP-3.             
012600 77  W-ANTALPOSTER-DC11          PIC S9(4)   VALUE +0 COMP-3.             
012700 77  W-ANTALPOSTER-DC91          PIC S9(4)   VALUE +0 COMP-3.             
012800 77  W-ANTALPOSTER-DC61          PIC S9(4)   VALUE +0 COMP-3.             
012900 77  W-ANTALPOSTER-DC62          PIC S9(4)   VALUE +0 COMP-3.             
013000 77  W-ANTALSUMMA-DC11           PIC S9(7)V99 VALUE +0 COMP-3.            
013100 77  W-ANTALSUMMA-DC91           PIC S9(7)V99 VALUE +0 COMP-3.            
013200 77  W-ANTALSUMMA-DC61           PIC S9(7)V99 VALUE +0 COMP-3.            
013300 77  W-ANTALSUMMA-DC62           PIC S9(7)V99 VALUE +0 COMP-3.            
013400 77  W-KVARBDAG-DC11             PIC S9(4)   VALUE +0 COMP-3.             
013500 77  W-KVARBDAG-DC91             PIC S9(4)   VALUE +0 COMP-3.             
013600 77  W-KVARBDAG-DC61             PIC S9(4)   VALUE +0 COMP-3.             
013700 77  W-KVARBDAG-DC62             PIC S9(4)   VALUE +0 COMP-3.             
013800     EJECT                                                                
013900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014000 01  FILLER REDEFINES DAGENS-DATUM.                                       
014100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014400                                                                          
014500 01  WS-DATUM.                                                            
014600     03  DATUM-SEKEL             PIC X(2).                                
014700     03  DATUM-AAR               PIC X(2).                                
014800     03  FILLER                  PIC X(1)    VALUE '-'.                   
014900     03  DATUM-MAANAD            PIC X(2).                                
015000     03  FILLER                  PIC X(1)    VALUE '-'.                   
015100     03  DATUM-DAG               PIC X(2).                                
015200                                                                          
015300                                                                          
015400 01  WS-DATUM-GB.                                                         
015500     03  DATUM-GB-DAG            PIC X(2).                                
015600     03  FILLER                  PIC X(1)    VALUE '-'.                   
015700     03  DATUM-GB-MAANAD         PIC X(2).                                
015800     03  FILLER                  PIC X(1)    VALUE '-'.                   
015900     03  DATUM-GB-SEKEL          PIC X(2).                                
016000     03  DATUM-GB-AAR            PIC X(2).                                
016100                                                                          
016200                                                                          
016300                                                                          
016400 01  WS-DATUM-US.                                                         
016500     03  DATUM-US-MAANAD         PIC X(2).                                
016600     03  FILLER                  PIC X(1)    VALUE '-'.                   
016700     03  DATUM-US-DAG            PIC X(2).                                
016800     03  FILLER                  PIC X(1)    VALUE '-'.                   
016900     03  DATUM-US-SEKEL          PIC X(2).                                
017000     03  DATUM-US-AAR            PIC X(2).                                
017100                                                                          
017200                                                                          
017300     EJECT                                                                
017400 01  DYNAMISKA-SUBPROGRAM.                                                
017500*                                                                         
017600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
018000     SKIP2                                                                
018100*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
018200 01  FILLER                      PIC X(16)   VALUE 'DATKORT'.             
018300 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
018400     SKIP2                                                                
018500*01  -COPY WDATKORT                                                       
018600*    --- PARAMETRAR TILL ABEND                                            
018700                                                                          
018800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019100     SKIP2                                                                
019200 01  FELTEXT.                                                             
019300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
019500     EJECT                                                                
019600*    --- PARAMETRAR TILL POSTSUM                                          
019700*                                                                         
019800*01  -COPY W0005   -PRE  POSTSUM-                                         
019900     EJECT                                                                
020000*    --- VALID IDDC CODES                                                 
020100*                                                                         
020200*01  -COPY WWDC99                                                         
020300     EJECT                                                                
020400*01  -COPY WDATAREA                                                       
020500     EJECT                                                                
020600 01  IN-AREA-START               PIC X(24)   VALUE                        
020700                                 'IN-AREA-START  '.                       
020800     SKIP2                                                                
020900                                                                          
021000*01  AREA -COPY W3714E     -PRE IN-                                       
021100     EJECT                                                                
021200 01  WUT-AREA-START             PIC X(24)   VALUE                         
021300                                 'UT-RAD-AREA-START'.                     
021400 01  UT-RAD-DC11                 PIC X(121)  VALUE SPACE.                 
021500 01  UT-RAD-DC91                 PIC X(121)  VALUE SPACE.                 
021600 01  UT-RAD-DC61                 PIC X(121)  VALUE SPACE.                 
021700 01  UT-RAD-DC62                 PIC X(121)  VALUE SPACE.                 
021800                                                                          
021900 01  W-HJLP-AREA-START          PIC X(24)   VALUE                         
022000                                 'W-HJALP-AREA-START  '.                  
022100     SKIP2                                                                
022200 01  W-HJALPAREOR.                                                        
022300*                                                                         
022400     03  W-SKIP                  PIC 9(3) COMP-3  VALUE 1.                
022500     03  W-ANTAL-RADER-DC11                                               
022600                                 PIC 9(3)    VALUE 999.                   
022700     03  W-ANTAL-RADER-DC91                                               
022800                                 PIC 9(3)    VALUE 999.                   
022900     03  W-ANTAL-RADER-DC61                                               
023000                                 PIC 9(3)    VALUE 999.                   
023100     03  W-ANTAL-RADER-DC62                                               
023200                                 PIC 9(3)    VALUE 999.                   
023300     03  W-MAX-RADER-PER-SIDA                                             
023400                                 PIC 9(3)    VALUE 42.                    
023500     03  W-MAX-POSITIONER-PER-RAD                                         
023600                                 PIC 9(3)    VALUE 120.                   
023700     03  W-SIDRAKNARE-DC11       PIC S9(5)   COMP-3 VALUE ZERO.           
023800                                                                          
023900     03  W-SIDRAKNARE-DC91       PIC S9(5)   COMP-3 VALUE ZERO.           
024000                                                                          
024100     03  W-SIDRAKNARE-DC61       PIC S9(5)   COMP-3 VALUE ZERO.           
024200                                                                          
024300     03  W-SIDRAKNARE-DC62       PIC S9(5)   COMP-3 VALUE ZERO.           
024400     EJECT                                                                
024500*                                                                         
024600     EJECT                                                                
024700 01  W002-AREA-START             PIC X(24)   VALUE                        
024800                                 'W002-AREA-START  '.                     
024900     SKIP2                                                                
025000 01  W-RUBRIK1-DC11.                                                      
025100     03  FILLER                PIC X(1)   VALUE '1'.                      
025200     03  FILLER                PIC X(2)   VALUE SPACE.                    
025300     03  FILLER                PIC X(15)  VALUE 'W37257-011'.             
025400     03  FILLER                PIC X(33)                                  
025500                  VALUE 'PERIOD RAPPORT BYTESTUPPFÖLJNING '.              
025600     03  FILLER                PIC X(34)                                  
025700                  VALUE 'TIDEN DET TAR FRÅN STATUS 2 TILL 3'.             
025800     03  FILLER                PIC X(7)   VALUE ' DATUM '.                
025900     03  W-DAGENS-DATUM-DC11   PIC X(10)  VALUE SPACE.                    
026000     03  FILLER                PIC X(5)   VALUE SPACE.                    
026100     03  FILLER                PIC X(5)  VALUE 'SIDA '.                   
026200     03  W-SID-DC11            PIC Z(4)9.                                 
026300     03  FILLER                PIC X(3)  VALUE SPACE.                     
026400     EJECT                                                                
026500*                                                                         
026600 01  W-RUBRIK1-DC91.                                                      
026700     03  FILLER                PIC X(1)   VALUE '1'.                      
026800     03  FILLER                PIC X(2)   VALUE SPACE.                    
026900     03  FILLER                PIC X(15)  VALUE 'W37257-091'.             
027000     03  FILLER                PIC X(33)                                  
027100                  VALUE 'AVERAGE TIME IN DAYS FROM STATUS '.              
027200     03  FILLER                PIC X(34)                                  
027300                  VALUE '2 TO STATUS 3 PERIOD LIST         '.             
027400     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
027500     03  W-DAGENS-DATUM-DC91   PIC X(10)  VALUE SPACE.                    
027600     03  FILLER                PIC X(5)   VALUE SPACE.                    
027700     03  FILLER                PIC X(5)  VALUE 'SIDA '.                   
027800     03  W-SID-DC91            PIC Z(4)9.                                 
027900     03  FILLER                PIC X(3)  VALUE SPACE.                     
028000     EJECT                                                                
028100*                                                                         
028200 01  W-RUBRIK1-DC61.                                                      
028300     03  FILLER                PIC X(1)   VALUE '1'.                      
028400     03  FILLER                PIC X(2)   VALUE SPACE.                    
028500     03  FILLER                PIC X(15)  VALUE 'W37257-061'.             
028600     03  FILLER                PIC X(33)                                  
028700                  VALUE 'AVERAGE TIME IN DAYS FROM STATUS '.              
028800     03  FILLER                PIC X(34)                                  
028900                  VALUE '2 TO STATUS 3 PERIOD LIST         '.             
029000     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
029100     03  W-DAGENS-DATUM-DC61   PIC X(10)  VALUE SPACE.                    
029200     03  FILLER                PIC X(5)   VALUE SPACE.                    
029300     03  FILLER                PIC X(5)  VALUE 'SIDA '.                   
029400     03  W-SID-DC61            PIC Z(4)9.                                 
029500     03  FILLER                PIC X(3)  VALUE SPACE.                     
029600     EJECT                                                                
029700*                                                                         
029800 01  W-RUBRIK1-DC62.                                                      
029900     03  FILLER                PIC X(1)   VALUE '1'.                      
030000     03  FILLER                PIC X(2)   VALUE SPACE.                    
030100     03  FILLER                PIC X(15)  VALUE 'W37257-062'.             
030200     03  FILLER                PIC X(33)                                  
030300                  VALUE 'AVERAGE TIME IN DAYS FROM STATUS '.              
030400     03  FILLER                PIC X(34)                                  
030500                  VALUE '2 TO STATUS 3 PERIOD LIST         '.             
030600     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
030700     03  W-DAGENS-DATUM-DC62   PIC X(10)  VALUE SPACE.                    
030800     03  FILLER                PIC X(5)   VALUE SPACE.                    
030900     03  FILLER                PIC X(5)  VALUE 'SIDA '.                   
031000     03  W-SID-DC62            PIC Z(4)9.                                 
031100     03  FILLER                PIC X(3)  VALUE SPACE.                     
031200     EJECT                                                                
031300*                                                                         
031400     SKIP2                                                                
031500 01  W-RUBRIK2-DC11.                                                      
031600     03  FILLER                PIC X(1)   VALUE '0'.                      
031700     03  FILLER                PIC X(2)   VALUE SPACE.                    
031800     03  FILLER                PIC X(15)  VALUE 'DISTRIKT  '.             
031900     03  FILLER                PIC X(15)  VALUE 'KUND '.                  
032000     03  FILLER                PIC X(15)  VALUE 'ANTAL '.                 
032100     03  FILLER                PIC X(5)   VALUE SPACE.                    
032200     03  FILLER                PIC X(35)  VALUE SPACE.                    
032300     03  FILLER                PIC X(32)  VALUE SPACE.                    
032400*                                                                         
032500 01  W-RUBRIK2-DC91.                                                      
032600     03  FILLER                PIC X(1)   VALUE '0'.                      
032700     03  FILLER                PIC X(2)   VALUE SPACE.                    
032800     03  FILLER                PIC X(15)  VALUE 'DISTRICT  '.             
032900     03  FILLER                PIC X(15)  VALUE 'CUSTOMER'.               
033000     03  FILLER                PIC X(04)  VALUE SPACE.                    
033100     03  FILLER                PIC X(38)  VALUE                           
033200                  'DAYS (WORKING DAY S EXCL. WEEKENDDAYS)'.               
033300     03  FILLER                PIC X(29)  VALUE SPACE.                    
033400*                                                                         
033500 01  W-RUBRIK2-DC61.                                                      
033600     03  FILLER                PIC X(1)   VALUE '0'.                      
033700     03  FILLER                PIC X(2)   VALUE SPACE.                    
033800     03  FILLER                PIC X(15)  VALUE 'DISTRICT  '.             
033900     03  FILLER                PIC X(15)  VALUE 'CUSTOMER'.               
034000     03  FILLER                PIC X(04)  VALUE SPACE.                    
034100     03  FILLER                PIC X(38)  VALUE                           
034200               'DAYS (WORKING DAY S EXCL. WEEKENDDAYS)'.                  
034300     03  FILLER                PIC X(29)  VALUE SPACE.                    
034400*                                                                         
034500 01  W-RUBRIK2-DC62.                                                      
034600     03  FILLER                PIC X(1)   VALUE '0'.                      
034700     03  FILLER                PIC X(2)   VALUE SPACE.                    
034800     03  FILLER                PIC X(15)  VALUE 'DISTRICT  '.             
034900     03  FILLER                PIC X(15)  VALUE 'CUSTOMER'.               
035000     03  FILLER                PIC X(04)  VALUE SPACE.                    
035100     03  FILLER                PIC X(38)  VALUE                           
035200               'DAYS (WORKING DAY S EXCL. WEEKENDDAYS)'.                  
035300     03  FILLER                PIC X(29)  VALUE SPACE.                    
035400*                                                                         
035500     EJECT                                                                
035600 01  W-DETALJ1-DC11.                                                      
035700     03  W-STYRTECKEN-DC11     PIC X(1)   VALUE '0'.                      
035800     03  FILLER                PIC X(2)   VALUE SPACE.                    
035900     03  FILLER                PIC X(3)   VALUE SPACE.                    
036000     03  W-IDDISTR-DC11        PIC Z(4)9  VALUE ZERO.                     
036100     03  FILLER                PIC X(4)   VALUE SPACE.                    
036200     03  W-IDKUNDNR-DC11       PIC Z(6)9  VALUE ZERO.                     
036300     03  FILLER                PIC X(6)   VALUE SPACE.                    
036400     03  WS-KVARBDAG-DC11      PIC Z(6)9.99 VALUE ZERO.                   
036500                                                                          
036600 01  W-DETALJ1-DC91.                                                      
036700     03  W-STYRTECKEN-DC91     PIC X(1)   VALUE '0'.                      
036800     03  FILLER                PIC X(2)   VALUE SPACE.                    
036900     03  FILLER                PIC X(3)   VALUE SPACE.                    
037000     03  W-IDDISTR-DC91        PIC Z(4)9  VALUE ZERO.                     
037100     03  FILLER                PIC X(8)   VALUE SPACE.                    
037200     03  W-IDKUNDNR-DC91       PIC Z(6)9  VALUE ZERO.                     
037300     03  FILLER                PIC X(5)   VALUE SPACE.                    
037400     03  WS-KVARBDAG-DC91      PIC Z(6)9.99 VALUE ZERO.                   
037500                                                                          
037600                                                                          
037700                                                                          
037800                                                                          
037900 01  W-DETALJ1-DC61.                                                      
038000     03  W-STYRTECKEN-DC61     PIC X(1)   VALUE '0'.                      
038100     03  FILLER                PIC X(2)   VALUE SPACE.                    
038200     03  FILLER                PIC X(3)   VALUE SPACE.                    
038300     03  W-IDDISTR-DC61        PIC Z(4)9  VALUE ZERO.                     
038400     03  FILLER                PIC X(8)   VALUE SPACE.                    
038500     03  W-IDKUNDNR-DC61       PIC Z(6)9  VALUE ZERO.                     
038600     03  FILLER                PIC X(5)   VALUE SPACE.                    
038700     03  WS-KVARBDAG-DC61      PIC Z(6)9.99 VALUE ZERO.                   
038800                                                                          
038900                                                                          
039000 01  W-DETALJ1-DC62.                                                      
039100     03  W-STYRTECKEN-DC62     PIC X(1)   VALUE '0'.                      
039200     03  FILLER                PIC X(2)   VALUE SPACE.                    
039300     03  FILLER                PIC X(3)   VALUE SPACE.                    
039400     03  W-IDDISTR-DC62        PIC Z(4)9  VALUE ZERO.                     
039500     03  FILLER                PIC X(8)   VALUE SPACE.                    
039600     03  W-IDKUNDNR-DC62       PIC Z(6)9  VALUE ZERO.                     
039700     03  FILLER                PIC X(5)   VALUE SPACE.                    
039800     03  WS-KVARBDAG-DC62      PIC Z(6)9.99 VALUE ZERO.                   
039900                                                                          
040000 01  W-BLANKRAD.                                                          
040100     03  W-STYR                PIC X(1)   VALUE ' '.                      
040200     03  FILLER                PIC X(120) VALUE SPACE.                    
040300                                                                          
040400     EJECT                                                                
040500 PROCEDURE DIVISION.                                                      
040600 MAIN SECTION.                                                            
040700     SKIP2                                                                
040800                                                                          
040900     PERFORM A-INIT                                                       
041000     PERFORM S01-LAES-W3724E                                              
041100     PERFORM UNTIL END-OF-W3724E                                          
041200       PERFORM B-BEARBETA                                                 
041300       PERFORM S01-LAES-W3724E                                            
041400     END-PERFORM                                                          
041500                                                                          
041600     PERFORM C-KOLL-OM-SIST-POST                                          
041700                                                                          
041800     PERFORM Z-FINIT                                                      
041900                                                                          
042000     MOVE ZERO TO RETURN-CODE                                             
042100     GOBACK                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 A-INIT SECTION.                                                          
042500                                                                          
042600     OPEN INPUT  W3724E                                                   
042700                                                                          
042800     OPEN OUTPUT LISTA                                                    
042900                 LISTB                                                    
043000                 LISTG                                                    
043100                 LISTH                                                    
043200     SKIP2                                                                
043300     ACCEPT DAGENS-DATUM  FROM DATE                                       
043400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043500*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
043600                                                                          
043700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
043800                                                                          
043900     MOVE   D-AAR            TO  DAGENS-DATUM-AAR                         
044000     MOVE   D-MAANAD         TO  DAGENS-DATUM-MAANAD                      
044100     MOVE   D-DAG            TO  DAGENS-DATUM-DAG                         
044200                                                                          
044300     IF DAGENS-DATUM-AAR > 50                                             
044400        MOVE 19              TO DATUM-SEKEL                               
044500                                DATUM-GB-SEKEL                            
044600                                DATUM-US-SEKEL                            
044700     ELSE                                                                 
044800        MOVE 20              TO DATUM-SEKEL                               
044900                                DATUM-GB-SEKEL                            
045000                                DATUM-US-SEKEL                            
045100     END-IF                                                               
045200                                                                          
045300     MOVE DAGENS-DATUM-AAR    TO DATUM-AAR                                
045400                                 DATUM-GB-AAR                             
045500                                 DATUM-US-AAR                             
045600     MOVE DAGENS-DATUM-MAANAD TO DATUM-MAANAD                             
045700                                 DATUM-GB-MAANAD                          
045800                                 DATUM-US-MAANAD                          
045900     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
046000                                 DATUM-GB-DAG                             
046100                                 DATUM-US-DAG                             
046200                                                                          
046300     MOVE WS-DATUM            TO W-DAGENS-DATUM-DC11                      
046400     MOVE WS-DATUM-GB         TO W-DAGENS-DATUM-DC91                      
046500     MOVE WS-DATUM-US         TO W-DAGENS-DATUM-DC61                      
046600                                 W-DAGENS-DATUM-DC62                      
046700     MOVE +1                  TO W-SIDRAKNARE-DC11                        
046800                                 W-SIDRAKNARE-DC91                        
046900                                 W-SIDRAKNARE-DC61                        
047000                                 W-SIDRAKNARE-DC62                        
047100     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC11                  
047200     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC91                  
047300     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC61                  
047400     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC62                  
047500     .                                                                    
047600     EJECT                                                                
047700 B-BEARBETA SECTION.                                                      
047800                                                                          
047900     MOVE IN-IDDC        TO WS-IDDC                                       
048000     PERFORM BH-KONTR-OM-FIRST-POST                                       
048100                                                                          
048200     EVALUATE TRUE                                                        
048300         WHEN CDC-SE                                                      
048400                                                                          
048500           IF SPAR-IDDISTR-DC11 NOT = IN-IDDISTR                          
048600              PERFORM BB-BERAKNA-ARBETDAGAR-DC11                          
048700              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
048800              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC11                       
048900              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC11                      
049000              PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                         
049100           ELSE                                                           
049200             IF SPAR-IDKUNDNR-DC11 NOT = IN-IDKUNDNR                      
049300                PERFORM BB-BERAKNA-ARBETDAGAR-DC11                        
049400                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
049500                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC11                     
049600                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC11                    
049700                PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                       
049800             ELSE                                                         
049900                PERFORM BBA-SUMMERA-ARBETDAGAR-DC11                       
050000             END-IF                                                       
050100           END-IF                                                         
050200                                                                          
050300         WHEN SDC-NL-ET                                                   
050400           IF SPAR-IDDISTR-DC91 NOT = IN-IDDISTR                          
050500              PERFORM BC-BERAKNA-ARBETDAGAR-DC91                          
050600              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
050700              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC91                       
050800              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC91                      
050900              PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                         
051000           ELSE                                                           
051100             IF SPAR-IDKUNDNR-DC91 NOT = IN-IDKUNDNR                      
051200                PERFORM BC-BERAKNA-ARBETDAGAR-DC91                        
051300                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
051400                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC91                     
051500                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC91                    
051600                PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                       
051700             ELSE                                                         
051800                PERFORM BCA-SUMMERA-ARBETDAGAR-DC91                       
051900             END-IF                                                       
052000           END-IF                                                         
052100         WHEN NDC-JP                                                      
052200           IF SPAR-IDDISTR-DC61 NOT = IN-IDDISTR                          
052300              PERFORM BI-BERAKNA-ARBETDAGAR-DC61                          
052400              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
052500              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC61                       
052600              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC61                      
052700              PERFORM BIA-SUMMERA-ARBETDAGAR-DC61                         
052800           ELSE                                                           
052900             IF SPAR-IDKUNDNR-DC61 NOT = IN-IDKUNDNR                      
053000                PERFORM BI-BERAKNA-ARBETDAGAR-DC61                        
053100                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
053200                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC61                     
053300                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC61                    
053400                PERFORM BIA-SUMMERA-ARBETDAGAR-DC61                       
053500             ELSE                                                         
053600                PERFORM BIA-SUMMERA-ARBETDAGAR-DC61                       
053700             END-IF                                                       
053800           END-IF                                                         
053900         WHEN NDC-AU                                                      
054000           IF SPAR-IDDISTR-DC62 NOT = IN-IDDISTR                          
054100              PERFORM BJ-BERAKNA-ARBETDAGAR-DC62                          
054200              PERFORM BA-KONTROLLERA-SIDBRYTNING                          
054300              MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC62                       
054400              MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC62                      
054500              PERFORM BJA-SUMMERA-ARBETDAGAR-DC62                         
054600           ELSE                                                           
054700             IF SPAR-IDKUNDNR-DC62 NOT = IN-IDKUNDNR                      
054800                PERFORM BJ-BERAKNA-ARBETDAGAR-DC62                        
054900                PERFORM BA-KONTROLLERA-SIDBRYTNING                        
055000                MOVE IN-IDDISTR  TO SPAR-IDDISTR-DC62                     
055100                MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR-DC62                    
055200                PERFORM BJA-SUMMERA-ARBETDAGAR-DC62                       
055300             ELSE                                                         
055400                PERFORM BJA-SUMMERA-ARBETDAGAR-DC62                       
055500             END-IF                                                       
055600           END-IF                                                         
055700     END-EVALUATE                                                         
055800                                                                          
055900     .                                                                    
056000     EJECT                                                                
056100 BA-KONTROLLERA-SIDBRYTNING SECTION.                                      
056200                                                                          
056300     EVALUATE TRUE                                                        
056400         WHEN CDC-SE                                                      
056500            PERFORM BAA-KONTROLLERA-SIDBRYTNING                           
056600         WHEN SDC-NL-ET                                                   
056700            PERFORM BAB-KONTROLLERA-SIDBRYTNING                           
056800         WHEN NDC-JP                                                      
056900            PERFORM BAG-KONTROLLERA-SIDBRYTNING                           
057000         WHEN NDC-AU                                                      
057100            PERFORM BAH-KONTROLLERA-SIDBRYTNING                           
057200     END-EVALUATE                                                         
057300                                                                          
057400     .                                                                    
057500     EJECT                                                                
057600 BAA-KONTROLLERA-SIDBRYTNING SECTION.                                     
057700                                                                          
057800     IF W-ANTAL-RADER-DC11 > W-MAX-RADER-PER-SIDA                         
057900        ADD +1 TO W-SIDRAKNARE-DC11                                       
058000        MOVE W-SIDRAKNARE-DC11 TO W-SID-DC11                              
058100        MOVE W-RUBRIK1-DC11  TO UT-RAD-DC11                               
058200        PERFORM S02-SKRIV-LISTA                                           
058300        MOVE W-RUBRIK2-DC11  TO UT-RAD-DC11                               
058400        PERFORM S02-SKRIV-LISTA                                           
058500        MOVE +4 TO W-ANTAL-RADER-DC11                                     
058600        MOVE '0'          TO W-STYRTECKEN-DC11                            
058700     ELSE                                                                 
058800        MOVE ' '          TO W-STYRTECKEN-DC11                            
058900     END-IF                                                               
059000     SKIP2                                                                
059100**** SKRIVER DEN ORDINARIE RADEN ****                                     
059200     MOVE SPAR-IDDISTR-DC11   TO W-IDDISTR-DC11                           
059300     MOVE SPAR-IDKUNDNR-DC11  TO W-IDKUNDNR-DC11                          
059400     MOVE W-DETALJ1-DC11      TO UT-RAD-DC11                              
059500     PERFORM S02-SKRIV-LISTA                                              
059600     SKIP2                                                                
059700     MOVE SPACE TO UT-RAD-DC11                                            
059800     ADD  +1 TO W-ANTAL-RADER-DC11                                        
059900     .                                                                    
060000     EJECT                                                                
060100 BAB-KONTROLLERA-SIDBRYTNING SECTION.                                     
060200                                                                          
060300     IF W-ANTAL-RADER-DC91 > W-MAX-RADER-PER-SIDA                         
060400        ADD +1 TO W-SIDRAKNARE-DC91                                       
060500        MOVE W-SIDRAKNARE-DC91 TO W-SID-DC91                              
060600        MOVE W-RUBRIK1-DC91  TO UT-RAD-DC91                               
060700        PERFORM S03-SKRIV-LISTB                                           
060800        MOVE W-RUBRIK2-DC91  TO UT-RAD-DC91                               
060900        PERFORM S03-SKRIV-LISTB                                           
061000        MOVE +4 TO W-ANTAL-RADER-DC91                                     
061100        MOVE '0'          TO W-STYRTECKEN-DC91                            
061200     ELSE                                                                 
061300        MOVE ' '          TO W-STYRTECKEN-DC91                            
061400     END-IF                                                               
061500                                                                          
061600**** SKRIVER DEN ORDINARIE RADEN ****                                     
061700     MOVE SPAR-IDDISTR-DC91   TO W-IDDISTR-DC91                           
061800     MOVE SPAR-IDKUNDNR-DC91  TO W-IDKUNDNR-DC91                          
061900     MOVE W-DETALJ1-DC91      TO UT-RAD-DC91                              
062000     PERFORM S03-SKRIV-LISTB                                              
062100     SKIP2                                                                
062200     MOVE SPACE TO UT-RAD-DC91                                            
062300     ADD  +1 TO W-ANTAL-RADER-DC91                                        
062400     .                                                                    
062500     EJECT                                                                
062600 BAG-KONTROLLERA-SIDBRYTNING SECTION.                                     
062700                                                                          
062800     IF W-ANTAL-RADER-DC61 > W-MAX-RADER-PER-SIDA                         
062900        ADD +1 TO W-SIDRAKNARE-DC61                                       
063000        MOVE W-SIDRAKNARE-DC61 TO W-SID-DC61                              
063100        MOVE W-RUBRIK1-DC61  TO UT-RAD-DC61                               
063200        PERFORM S08-SKRIV-LISTG                                           
063300        MOVE W-RUBRIK2-DC61  TO UT-RAD-DC61                               
063400        PERFORM S08-SKRIV-LISTG                                           
063500        MOVE +4 TO W-ANTAL-RADER-DC61                                     
063600        MOVE '0'          TO W-STYRTECKEN-DC61                            
063700     ELSE                                                                 
063800        MOVE ' '          TO W-STYRTECKEN-DC61                            
063900     END-IF                                                               
064000     SKIP2                                                                
064100**** SKRIVER DEN ORDINARIE RADEN ****                                     
064200     MOVE SPAR-IDDISTR-DC61   TO W-IDDISTR-DC61                           
064300     MOVE SPAR-IDKUNDNR-DC61  TO W-IDKUNDNR-DC61                          
064400     MOVE W-DETALJ1-DC61      TO UT-RAD-DC61                              
064500     PERFORM S08-SKRIV-LISTG                                              
064600     SKIP2                                                                
064700     MOVE SPACE TO UT-RAD-DC61                                            
064800     ADD  +1 TO W-ANTAL-RADER-DC61                                        
064900     .                                                                    
065000     EJECT                                                                
065100 BAH-KONTROLLERA-SIDBRYTNING SECTION.                                     
065200                                                                          
065300     IF W-ANTAL-RADER-DC62 > W-MAX-RADER-PER-SIDA                         
065400        ADD +1 TO W-SIDRAKNARE-DC62                                       
065500        MOVE W-SIDRAKNARE-DC62 TO W-SID-DC62                              
065600        MOVE W-RUBRIK1-DC62  TO UT-RAD-DC62                               
065700        PERFORM S09-SKRIV-LISTH                                           
065800        MOVE W-RUBRIK2-DC62  TO UT-RAD-DC62                               
065900        PERFORM S09-SKRIV-LISTH                                           
066000        MOVE +4 TO W-ANTAL-RADER-DC62                                     
066100        MOVE '0'          TO W-STYRTECKEN-DC62                            
066200     ELSE                                                                 
066300        MOVE ' '          TO W-STYRTECKEN-DC62                            
066400     END-IF                                                               
066500     SKIP2                                                                
066600**** SKRIVER DEN ORDINARIE RADEN ****                                     
066700     MOVE SPAR-IDDISTR-DC62   TO W-IDDISTR-DC62                           
066800     MOVE SPAR-IDKUNDNR-DC62  TO W-IDKUNDNR-DC62                          
066900     MOVE W-DETALJ1-DC62      TO UT-RAD-DC62                              
067000     PERFORM S09-SKRIV-LISTH                                              
067100     SKIP2                                                                
067200     MOVE SPACE TO UT-RAD-DC62                                            
067300     ADD  +1 TO W-ANTAL-RADER-DC62                                        
067400     .                                                                    
067500     EJECT                                                                
067600 BB-BERAKNA-ARBETDAGAR-DC11 SECTION.                                      
067700                                                                          
067800     IF W-ANTALPOSTER-DC11 > ZERO                                         
067900        COMPUTE W-ANTALSUMMA-DC11 =                                       
068000                W-KVARBDAG-DC11 / W-ANTALPOSTER-DC11                      
068100     END-IF                                                               
068200     MOVE W-ANTALSUMMA-DC11   TO WS-KVARBDAG-DC11                         
068300***  NOLLSTÄLL RÄKNARE *****                                              
068400                                                                          
068500     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC11                  
068600     MOVE ZERO                TO W-ANTALSUMMA-DC11                        
068700     MOVE ZERO                TO  W-ANTALPOSTER-DC11                      
068800     MOVE ZERO                TO  W-KVARBDAG-DC11                         
068900                                                                          
069000     .                                                                    
069100     EJECT                                                                
069200 BBA-SUMMERA-ARBETDAGAR-DC11 SECTION.                                     
069300                                                                          
069400     ADD +1            TO W-ANTALPOSTER-DC11                              
069500     ADD +1            TO W-ANTALPOSTER-TOT-DC11                          
069600     ADD IN-KVARBDAG   TO W-KVARBDAG-DC11                                 
069700     MOVE 'N'                 TO SISTA-POST-SKRIVEN-DC11                  
069800                                                                          
069900                                                                          
070000     .                                                                    
070100     EJECT                                                                
070200 BC-BERAKNA-ARBETDAGAR-DC91 SECTION.                                      
070300                                                                          
070400***  BERÄKNAR SNITT ARBETSTIDEN EN BYTESRAPPORT TAR FRÅN ****             
070500***  STATUS 2  TILL 3 INOM EN PERIOD                     ****             
070600                                                                          
070700     IF W-ANTALPOSTER-DC91 > ZERO                                         
070800        COMPUTE W-ANTALSUMMA-DC91 =                                       
070900                W-KVARBDAG-DC91 / W-ANTALPOSTER-DC91                      
071000     END-IF                                                               
071100     MOVE W-ANTALSUMMA-DC91   TO WS-KVARBDAG-DC91                         
071200     MOVE 'J'                 TO SISTA-POST-SKRIVEN-DC91                  
071300                                                                          
071400***  NOLLSTÄLL RÄKNARE *****                                              
071500                                                                          
071600     MOVE ZERO                TO W-ANTALSUMMA-DC91                        
071700     MOVE ZERO                TO  W-ANTALPOSTER-DC91                      
071800     MOVE ZERO                TO  W-KVARBDAG-DC91                         
071900                                                                          
072000     .                                                                    
072100     EJECT                                                                
072200                                                                          
072300 BCA-SUMMERA-ARBETDAGAR-DC91 SECTION.                                     
072400                                                                          
072500     ADD +1            TO W-ANTALPOSTER-DC91                              
072600     ADD +1            TO W-ANTALPOSTER-TOT-DC91                          
072700     ADD IN-KVARBDAG   TO W-KVARBDAG-DC91                                 
072800     MOVE 'N'          TO SISTA-POST-SKRIVEN-DC91                         
072900                                                                          
073000                                                                          
073100     .                                                                    
073200     EJECT                                                                
073300 BH-KONTR-OM-FIRST-POST SECTION.                                          
073400                                                                          
073500     EVALUATE TRUE                                                        
073600         WHEN CDC-SE                                                      
073700           IF FORSTA-POSTEN-DC11                                          
073800              MOVE 'N'           TO FORSTA-POST-DC11-SW                   
073900              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC11                     
074000              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC11                    
074100              MOVE W-SIDRAKNARE-DC11 TO W-SID-DC11                        
074200              MOVE W-RUBRIK1-DC11  TO UT-RAD-DC11                         
074300              PERFORM S02-SKRIV-LISTA                                     
074400              MOVE W-RUBRIK2-DC11  TO UT-RAD-DC11                         
074500              PERFORM S02-SKRIV-LISTA                                     
074600              MOVE +4 TO W-ANTAL-RADER-DC11                               
074700              MOVE W-BLANKRAD    TO UT-RAD-DC11                           
074800              PERFORM S02-SKRIV-LISTA                                     
074900           END-IF                                                         
075000         WHEN SDC-NL-ET                                                   
075100           IF FORSTA-POSTEN-DC91                                          
075200              MOVE 'N'           TO FORSTA-POST-DC91-SW                   
075300              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC91                     
075400              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC91                    
075500              MOVE W-SIDRAKNARE-DC91 TO W-SID-DC91                        
075600              MOVE W-RUBRIK1-DC91  TO UT-RAD-DC91                         
075700              PERFORM S03-SKRIV-LISTB                                     
075800              MOVE W-RUBRIK2-DC91  TO UT-RAD-DC91                         
075900              PERFORM S03-SKRIV-LISTB                                     
076000              MOVE +4 TO W-ANTAL-RADER-DC91                               
076100              MOVE W-BLANKRAD    TO UT-RAD-DC91                           
076200              PERFORM S03-SKRIV-LISTB                                     
076300           END-IF                                                         
076400         WHEN NDC-JP                                                      
076500           IF FORSTA-POSTEN-DC61                                          
076600              MOVE 'N'           TO FORSTA-POST-DC61-SW                   
076700              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC61                     
076800              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC61                    
076900              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC61                    
077000              MOVE W-SIDRAKNARE-DC61 TO W-SID-DC61                        
077100              MOVE W-RUBRIK1-DC61  TO UT-RAD-DC61                         
077200              PERFORM S08-SKRIV-LISTG                                     
077300              MOVE W-RUBRIK2-DC61  TO UT-RAD-DC61                         
077400              PERFORM S08-SKRIV-LISTG                                     
077500              MOVE +4 TO W-ANTAL-RADER-DC61                               
077600              MOVE W-BLANKRAD    TO UT-RAD-DC61                           
077700              PERFORM S08-SKRIV-LISTG                                     
077800           END-IF                                                         
077900         WHEN NDC-AU                                                      
078000           IF FORSTA-POSTEN-DC62                                          
078100              MOVE 'N'           TO FORSTA-POST-DC62-SW                   
078200              MOVE IN-IDDISTR    TO SPAR-IDDISTR-DC62                     
078300              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC62                    
078400              MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR-DC62                    
078500              MOVE W-SIDRAKNARE-DC62 TO W-SID-DC62                        
078600              MOVE W-RUBRIK1-DC62  TO UT-RAD-DC62                         
078700              PERFORM S09-SKRIV-LISTH                                     
078800              MOVE W-RUBRIK2-DC62  TO UT-RAD-DC62                         
078900              PERFORM S09-SKRIV-LISTH                                     
079000              MOVE +4 TO W-ANTAL-RADER-DC62                               
079100              MOVE W-BLANKRAD    TO UT-RAD-DC62                           
079200              PERFORM S09-SKRIV-LISTH                                     
079300           END-IF                                                         
079400     END-EVALUATE                                                         
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800 BI-BERAKNA-ARBETDAGAR-DC61 SECTION.                                      
079900                                                                          
080000     IF W-ANTALPOSTER-DC61 > ZERO                                         
080100        COMPUTE W-ANTALSUMMA-DC61 =                                       
080200                W-KVARBDAG-DC61 / W-ANTALPOSTER-DC61                      
080300     END-IF                                                               
080400     MOVE W-ANTALSUMMA-DC61   TO WS-KVARBDAG-DC61                         
080500                                                                          
080600     MOVE 'J'          TO SISTA-POST-SKRIVEN-DC61                         
080700***  NOLLSTÄLL RÄKNARE *****                                              
080800                                                                          
080900     MOVE ZERO                TO W-ANTALSUMMA-DC61                        
081000     MOVE ZERO                TO  W-ANTALPOSTER-DC61                      
081100     MOVE ZERO                TO  W-KVARBDAG-DC61                         
081200     .                                                                    
081300     EJECT                                                                
081400 BIA-SUMMERA-ARBETDAGAR-DC61 SECTION.                                     
081500                                                                          
081600     ADD +1            TO W-ANTALPOSTER-DC61                              
081700     ADD +1            TO W-ANTALPOSTER-TOT-DC61                          
081800     ADD IN-KVARBDAG   TO W-KVARBDAG-DC61                                 
081900     MOVE 'N'          TO SISTA-POST-SKRIVEN-DC61                         
082000                                                                          
082100                                                                          
082200     .                                                                    
082300     EJECT                                                                
082400 BJ-BERAKNA-ARBETDAGAR-DC62 SECTION.                                      
082500                                                                          
082600     IF W-ANTALPOSTER-DC62 > ZERO                                         
082700        COMPUTE W-ANTALSUMMA-DC62 =                                       
082800                W-KVARBDAG-DC62 / W-ANTALPOSTER-DC62                      
082900     END-IF                                                               
083000     MOVE W-ANTALSUMMA-DC62   TO WS-KVARBDAG-DC62                         
083100                                                                          
083200     MOVE 'J'          TO SISTA-POST-SKRIVEN-DC62                         
083300***  NOLLSTÄLL RÄKNARE *****                                              
083400                                                                          
083500     MOVE ZERO                TO W-ANTALSUMMA-DC62                        
083600     MOVE ZERO                TO  W-ANTALPOSTER-DC62                      
083700     MOVE ZERO                TO  W-KVARBDAG-DC62                         
083800     .                                                                    
083900     EJECT                                                                
084000 BJA-SUMMERA-ARBETDAGAR-DC62 SECTION.                                     
084100                                                                          
084200     ADD +1            TO W-ANTALPOSTER-DC62                              
084300     ADD +1            TO W-ANTALPOSTER-TOT-DC62                          
084400     ADD IN-KVARBDAG   TO W-KVARBDAG-DC62                                 
084500     MOVE 'N'          TO SISTA-POST-SKRIVEN-DC62                         
084600                                                                          
084700                                                                          
084800     .                                                                    
084900     EJECT                                                                
085000 C-KOLL-OM-SIST-POST SECTION.                                             
085100                                                                          
085200     IF SISTA-POST-SKRIVEN-DC11 = 'N'                                     
085300        PERFORM BB-BERAKNA-ARBETDAGAR-DC11                                
085400        PERFORM BAA-KONTROLLERA-SIDBRYTNING                               
085500     END-IF                                                               
085600                                                                          
085700     IF SISTA-POST-SKRIVEN-DC91 = 'N'                                     
085800        PERFORM BC-BERAKNA-ARBETDAGAR-DC91                                
085900        PERFORM BAB-KONTROLLERA-SIDBRYTNING                               
086000     END-IF                                                               
086100                                                                          
086200     IF SISTA-POST-SKRIVEN-DC61 = 'N'                                     
086300        PERFORM BI-BERAKNA-ARBETDAGAR-DC61                                
086400        PERFORM BAG-KONTROLLERA-SIDBRYTNING                               
086500     END-IF                                                               
086600                                                                          
086700     IF SISTA-POST-SKRIVEN-DC62 = 'N'                                     
086800        PERFORM BJ-BERAKNA-ARBETDAGAR-DC62                                
086900        PERFORM BAH-KONTROLLERA-SIDBRYTNING                               
087000     END-IF                                                               
087100                                                                          
087200     .                                                                    
087300     EJECT                                                                
087400 Z-FINIT SECTION.                                                         
087500     CLOSE W3724E                                                         
087600           LISTA                                                          
087700           LISTB                                                          
087800           LISTG                                                          
087810           LISTH                                                          
087900     SKIP2                                                                
088000     MOVE 'S' TO POSTSUM-OPKOD                                            
088100     CALL POSTSUM USING POSTSUM-PARM                                      
088200     .                                                                    
088300     EJECT                                                                
088400                                                                          
088500 S01-LAES-W3724E  SECTION.                                                
088600     READ W3724E INTO IN-AREA                                             
088700     AT END                                                               
088800        MOVE HIGH-VALUE TO IN-AREA                                        
088900        SET END-OF-W3724E TO TRUE                                         
089000                                                                          
089100     NOT AT END                                                           
089200        MOVE 'W3724E' TO POSTSUM-FDNAMN                                   
089300        MOVE 'W37257D1' TO POSTSUM-DDNAMN2                                
089400        MOVE 'IN  '    TO POSTSUM-TRANSTYP                                
089500        CALL POSTSUM USING POSTSUM-PARM                                   
089600     END-READ                                                             
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000 S02-SKRIV-LISTA  SECTION.                                                
090100                                                                          
090200     WRITE LISTAS FROM UT-RAD-DC11                                        
090300                                                                          
090400     .                                                                    
090500     EJECT                                                                
090600 S03-SKRIV-LISTB  SECTION.                                                
090700                                                                          
090800     WRITE LISTBS FROM UT-RAD-DC91                                        
090900                                                                          
091000     .                                                                    
091100     EJECT                                                                
091200                                                                          
091300 S08-SKRIV-LISTG  SECTION.                                                
091400                                                                          
091500     WRITE LISTGS FROM UT-RAD-DC61                                        
091600                                                                          
091700     .                                                                    
091800     EJECT                                                                
091900                                                                          
092000 S09-SKRIV-LISTH  SECTION.                                                
092100                                                                          
092200     WRITE LISTHS FROM UT-RAD-DC62                                        
092300                                                                          
092400     .                                                                    
092500     EJECT                                                                
092600                                                                          
092700 S99-ABEND SECTION.                                                       
092800                                                                          
092900     SKIP2                                                                
093000     MOVE 'S' TO POSTSUM-OPKOD                                            
093100     CALL POSTSUM USING POSTSUM-PARM                                      
093200     CALL ABEND USING RKOD-ABEND                                          
093300     .                                                                    
